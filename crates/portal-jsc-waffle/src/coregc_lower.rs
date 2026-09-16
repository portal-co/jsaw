//! Source `Module` -> core `Module` transform: value plan, CFG/value
//! rewrite, liveness-derived root-frame insertion, and checkpoint-safe
//! direct-call/function-reference lowering.
//!
//! Implements `docs/plan-coregc-atomic-collector-and-lowering.md` §6. This
//! module owns every decision about how a source Waffle value/block/call
//! maps to core Wasm; it never encodes a header byte offset directly (it
//! only calls the `Func`s exposed by `coregc_runtime`/`coregc_roots`) and it
//! never decides runtime heap policy.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use portal_pc_waffle::{
    Block, BlockTarget, EntityRef, Func, FuncDecl, FunctionBody, Global, HeapType, Module,
    Operator, Signature, SignatureData, Table, TableData, Terminator, Type, Value, ValueDef,
    WithNullable,
};

use crate::{
    coregc::{CoreGcError, CoreGcInventory, CoreGcStorage},
    coregc_layout::CoreGcDescriptorTable,
    coregc_runtime::{trap_code, CoreGcRuntime},
};

/// How one source SSA value (or function parameter/return/block parameter)
/// is represented in the lowered core module (`docs/...` §6.2).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LowerValue {
    /// An unchanged core scalar.
    Scalar(Type),
    /// A concrete managed reference, always carried as two adjacent `i32`s.
    FatRef {
        #[allow(dead_code)]
        nullable: bool,
        concrete: crate::coregc::CoreGcTypeId,
    },
    /// A dynamic value (`anyref`/`eqref`/`i31ref`/abstract
    /// `structref`/`arrayref`): the same two-`i32` pair as a `FatRef`, but
    /// without one fixed concrete type id — the type word may name any heap
    /// type or the i31-immediate sentinel (`docs/plan-coregc-atomic-
    /// collector-and-lowering.md` §13.2). It can still hold a heap pointer,
    /// so liveness must root it exactly like a `FatRef`.
    Dynamic,
}

impl LowerValue {
    fn flat_len(self) -> usize {
        match self {
            LowerValue::Scalar(_) => 1,
            LowerValue::FatRef { .. } | LowerValue::Dynamic => 2,
        }
    }

    fn flat_types(self) -> Vec<Type> {
        match self {
            LowerValue::Scalar(ty) => vec![ty],
            LowerValue::FatRef { .. } | LowerValue::Dynamic => vec![Type::I32, Type::I32],
        }
    }

    /// Whether values of this plan can hold a heap pointer and therefore
    /// need a shadow-frame slot when live across a checkpoint.
    fn needs_root(self) -> bool {
        matches!(self, LowerValue::FatRef { .. } | LowerValue::Dynamic)
    }
}

/// A lowered value: either one scalar core value or the `(address,
/// type_id)` pair of a fat reference.
#[derive(Clone, Copy, Debug)]
enum Lowered {
    Scalar(Value),
    Fat(Value, Value),
}

impl Lowered {
    fn flatten_into(self, out: &mut Vec<Value>) {
        match self {
            Lowered::Scalar(value) => out.push(value),
            Lowered::Fat(address, type_id) => {
                out.push(address);
                out.push(type_id);
            }
        }
    }
}

/// Classify a source value type. `anyref`/`i31ref`/`externref`/`eqref`/
/// `structref`/`arrayref`/multi-value are rejected here by construction: a
/// concrete `HeapType::Sig` into a struct/array signature is `FatRef`, a
/// concrete `HeapType::Sig` into a func signature is a plain scalar table
/// index (`docs/...` §6.7), and everything else is an error.
fn classify_type(
    source: &Module<'_>,
    inventory: &CoreGcInventory,
    ty: Type,
) -> Result<LowerValue, CoreGcError> {
    match ty {
        Type::I32 | Type::I64 | Type::F32 | Type::F64 => Ok(LowerValue::Scalar(ty)),
        Type::Heap(reference) => match reference.value {
            HeapType::Any | HeapType::Eq | HeapType::I31 | HeapType::Struct | HeapType::Array => {
                Ok(LowerValue::Dynamic)
            }
            HeapType::Sig { sig_index } => match &source.signatures[sig_index] {
                SignatureData::Struct { .. } | SignatureData::Array { .. } => {
                    let concrete = inventory.id_for(sig_index).ok_or_else(|| CoreGcError {
                        message: format!(
                            "coregc lowering found a managed reference to signature {} \
                             absent from the inventory",
                            sig_index.index()
                        ),
                    })?;
                    Ok(LowerValue::FatRef {
                        nullable: reference.nullable,
                        concrete,
                    })
                }
                SignatureData::Func { .. } | SignatureData::Import { .. } => {
                    Ok(LowerValue::Scalar(Type::I32))
                }
                unsupported => Err(CoreGcError {
                    message: format!(
                        "coregc lowering does not support a reference to signature kind {unsupported:?}"
                    ),
                }),
            },
            unsupported => Err(CoreGcError {
                message: format!(
                    "coregc lowering does not support heap type {unsupported:?}; only concrete \
                     struct/array/func references are accepted in v1"
                ),
            }),
        },
        unsupported => Err(CoreGcError {
            message: format!("coregc lowering does not support value type {unsupported:?}"),
        }),
    }
}

fn is_checkpoint_operator(op: &Operator) -> bool {
    matches!(
        op,
        Operator::StructNew { .. }
            | Operator::StructNewDefault { .. }
            | Operator::ArrayNewDefault { .. }
            | Operator::ArrayNewFixed { .. }
            | Operator::Call { .. }
            | Operator::CallRef { .. }
            | Operator::CallIndirect { .. }
    )
}

/// Liveness and checkpoint-spill analysis (`docs/...` §6.4), independently
/// testable against a value-plan closure and a hand-built `FunctionBody`.
mod liveness {
    use super::*;

    #[derive(Debug, Default)]
    pub(super) struct Liveness {
        /// For each checkpoint instruction's own `Value`, the sorted set of
        /// fat-ref values that must already be rooted before it executes.
        pub(super) checkpoint_live: BTreeMap<Value, Vec<Value>>,
        /// For each block whose terminator is a tail call, the fat-ref values
        /// that must be rooted at the tail call's own checkpoint (before the
        /// caller's frame is popped and the callee takes ownership of the
        /// arguments).
        pub(super) tail_call_live: BTreeMap<Block, Vec<Value>>,
    }

    fn block_defs_uses(
        body: &FunctionBody,
        block: Block,
        is_fatref: &impl Fn(Value) -> bool,
    ) -> (HashSet<Value>, HashSet<Value>) {
        let mut defs = HashSet::new();
        for &(_, param) in &body.blocks[block].params {
            if is_fatref(param) {
                defs.insert(param);
            }
        }
        for record in &body.blocks[block].insts {
            if is_fatref(record.value) {
                defs.insert(record.value);
            }
        }
        let mut raw_uses = HashSet::new();
        for record in &body.blocks[block].insts {
            body.values[record.value].visit_uses(&body.arg_pool, |used| {
                if is_fatref(used) {
                    raw_uses.insert(used);
                }
            });
        }
        body.blocks[block]
            .terminator
            .terminator
            .visit_uses(|used| {
                if is_fatref(used) {
                    raw_uses.insert(used);
                }
            });
        let uses: HashSet<Value> = raw_uses.difference(&defs).copied().collect();
        (defs, uses)
    }

    /// Compute liveness and per-checkpoint spill sets over `body`, treating
    /// only values for which `is_fatref` is true as significant, and only
    /// operators matched by `is_checkpoint` as checkpoint instructions.
    pub(super) fn compute(
        body: &FunctionBody,
        is_fatref: impl Fn(Value) -> bool,
        is_checkpoint: impl Fn(&Operator) -> bool,
    ) -> Result<Liveness, CoreGcError> {
        let blocks: Vec<Block> = body.blocks.entries().map(|(block, _)| block).collect();
        let mut defs: HashMap<Block, HashSet<Value>> = HashMap::new();
        let mut uses: HashMap<Block, HashSet<Value>> = HashMap::new();
        for &block in &blocks {
            let (d, u) = block_defs_uses(body, block, &is_fatref);
            defs.insert(block, d);
            uses.insert(block, u);
        }
        let mut live_in: HashMap<Block, HashSet<Value>> =
            blocks.iter().map(|&b| (b, HashSet::new())).collect();
        let mut live_out: HashMap<Block, HashSet<Value>> =
            blocks.iter().map(|&b| (b, HashSet::new())).collect();

        let cap = blocks.len().saturating_mul(4) + 16;
        let mut converged = false;
        for _ in 0..cap {
            let mut changed = false;
            for &block in &blocks {
                let mut new_out: HashSet<Value> = HashSet::new();
                for &succ in &body.blocks[block].succs {
                    new_out.extend(live_in[&succ].iter().copied());
                }
                let mut new_in: HashSet<Value> = uses[&block].clone();
                for value in new_out.difference(&defs[&block]) {
                    new_in.insert(*value);
                }
                if new_in != live_in[&block] {
                    live_in.insert(block, new_in);
                    changed = true;
                }
                if new_out != live_out[&block] {
                    live_out.insert(block, new_out);
                    changed = true;
                }
            }
            if !changed {
                converged = true;
                break;
            }
        }
        if !converged {
            return Err(CoreGcError {
                message: "coregc liveness dataflow did not converge; the source CFG is malformed"
                    .to_owned(),
            });
        }

        let mut checkpoint_live = BTreeMap::new();
        let mut tail_call_live = BTreeMap::new();
        for &block in &blocks {
            let mut live = live_out[&block].clone();
            let mut terminator_uses = Vec::new();
            body.blocks[block]
                .terminator
                .terminator
                .visit_uses(|used| {
                    if is_fatref(used) {
                        terminator_uses.push(used);
                    }
                });
            live.extend(terminator_uses);

            if matches!(
                &body.blocks[block].terminator.terminator,
                Terminator::ReturnCall { .. }
                    | Terminator::ReturnCallIndirect { .. }
                    | Terminator::ReturnCallRef { .. }
            ) {
                let mut required: Vec<Value> = live.iter().copied().collect();
                required.sort_by_key(|v| v.index());
                tail_call_live.insert(block, required);
            }

            for record in body.blocks[block].insts.iter().rev() {
                let value = record.value;
                let def = &body.values[value];
                let mut fatref_args = Vec::new();
                if let ValueDef::Operator(_, args, _) = def {
                    for &arg in &body.arg_pool[*args] {
                        if is_fatref(arg) {
                            fatref_args.push(arg);
                        }
                    }
                }
                def.visit_uses(&body.arg_pool, |used| {
                    if is_fatref(used) && !fatref_args.contains(&used) {
                        fatref_args.push(used);
                    }
                });
                let checkpoint = matches!(def, ValueDef::Operator(op, ..) if is_checkpoint(op));
                // Remove this instruction's own result *before* capturing
                // the checkpoint's requirements: the checkpoint (e.g. the
                // internal alloc call inside struct.new) runs before this
                // instruction's result exists, so it can never itself be
                // something the checkpoint needs already rooted, even if a
                // later instruction's use had provisionally added it to
                // `live` while walking backward past that later use.
                if is_fatref(value) {
                    live.remove(&value);
                }
                for &arg in &fatref_args {
                    live.insert(arg);
                }
                if checkpoint {
                    let mut required: Vec<Value> = live.iter().copied().collect();
                    required.sort_by_key(|v| v.index());
                    checkpoint_live.insert(value, required);
                }
            }

            // Block params are defined at block entry, before any
            // instruction; the reverse walk above only removes
            // instruction-result definitions, so params must be removed here
            // to reach the same live-in set the block-level dataflow computed
            // (which already folds params into `defs(block)`).
            for &(_, param) in &body.blocks[block].params {
                if is_fatref(param) {
                    live.remove(&param);
                }
            }

            debug_assert_eq!(
                live, live_in[&block],
                "per-instruction backward sweep must reproduce the block-level live-in set"
            );
        }

        Ok(Liveness {
            checkpoint_live,
            tail_call_live,
        })
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use portal_pc_waffle::{BlockTarget, SignatureData};

        // `Operator::Nop` stands in for "a checkpoint instruction" in these
        // fixtures so liveness can be tested without any struct/array/call
        // plumbing; real checkpoint detection is `is_checkpoint_operator`,
        // exercised end to end by `coregc_lower::tests`.
        fn is_checkpoint(op: &Operator) -> bool {
            matches!(op, Operator::Nop)
        }

        fn new_body() -> (Module<'static>, FunctionBody) {
            let mut module = Module::empty();
            let sig = module.signatures.push(SignatureData::Func {
                params: vec![],
                returns: vec![Type::I32],
                shared: false,
            });
            let body = FunctionBody::new(&module, sig);
            (module, body)
        }

        fn fatref(body: &mut FunctionBody, block: Block) -> Value {
            // A distinguishable "fat ref" producer with two I32 results,
            // matching the real address/type-id shape; liveness only cares
            // about the value's identity, not this shape.
            body.add_op(block, Operator::I32Const { value: 0 }, &[], &[Type::I32])
        }

        #[test]
        fn straight_line_checkpoint_requires_a_later_used_value_to_be_live_across_it() {
            let (_module, mut body) = new_body();
            let entry = body.entry;
            let a = fatref(&mut body, entry);
            let checkpoint = body.add_op(entry, Operator::Nop, &[], &[]);
            let ret = body.add_op(entry, Operator::I32Const { value: 1 }, &[a], &[Type::I32]);
            body.set_terminator(entry, Terminator::Return { values: vec![ret] });

            body.recompute_edges();
            let fatrefs: HashSet<Value> = [a].into_iter().collect();
            let result = compute(&body, |v| fatrefs.contains(&v), is_checkpoint).expect("liveness");
            assert_eq!(result.checkpoint_live[&checkpoint], vec![a]);
        }

        #[test]
        fn a_value_defined_after_the_checkpoint_is_not_required_live() {
            let (_module, mut body) = new_body();
            let entry = body.entry;
            let checkpoint = body.add_op(entry, Operator::Nop, &[], &[]);
            let a = fatref(&mut body, entry);
            let ret = body.add_op(entry, Operator::I32Const { value: 1 }, &[a], &[Type::I32]);
            body.set_terminator(entry, Terminator::Return { values: vec![ret] });

            body.recompute_edges();
            let fatrefs: HashSet<Value> = [a].into_iter().collect();
            let result = compute(&body, |v| fatrefs.contains(&v), is_checkpoint).expect("liveness");
            assert!(result.checkpoint_live[&checkpoint].is_empty());
        }

        #[test]
        fn a_value_used_only_as_the_checkpoint_operators_own_argument_is_still_required_live() {
            // Mirrors struct.new(a): `a` must survive the allocating call
            // that happens *before* it is stored into the new payload, even
            // though nothing uses `a` afterward at the source level.
            let (_module, mut body) = new_body();
            let entry = body.entry;
            let a = fatref(&mut body, entry);
            let checkpoint = body.add_op(entry, Operator::Nop, &[a], &[]);
            let ret = body.add_op(entry, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
            body.set_terminator(entry, Terminator::Return { values: vec![ret] });

            body.recompute_edges();
            let fatrefs: HashSet<Value> = [a].into_iter().collect();
            let result = compute(&body, |v| fatrefs.contains(&v), is_checkpoint).expect("liveness");
            assert_eq!(result.checkpoint_live[&checkpoint], vec![a]);
        }

        #[test]
        fn a_value_live_only_across_a_loop_back_edge_is_required_at_an_interior_checkpoint() {
            // header(x): if cond { checkpoint; br header(x) } else { return x }
            // `x` must be live across the checkpoint even though the only
            // *textual* use is the loop-carried branch argument, proving the
            // fixed-point dataflow (not just a single backward pass) is
            // exercised by a genuine back edge.
            let (_module, mut body) = new_body();
            let entry = body.entry;
            let header = body.add_block();
            let x_entry = fatref(&mut body, entry);
            body.set_terminator(
                entry,
                Terminator::Br {
                    target: BlockTarget {
                        block: header,
                        args: vec![x_entry],
                    },
                },
            );
            let x = body.add_blockparam(header, Type::I32);
            let cond = body.add_op(header, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
            let checkpoint_block = body.add_block();
            let exit_block = body.add_block();
            body.set_terminator(
                header,
                Terminator::CondBr {
                    cond,
                    if_true: BlockTarget {
                        block: checkpoint_block,
                        args: vec![],
                    },
                    if_false: BlockTarget {
                        block: exit_block,
                        args: vec![],
                    },
                },
            );
            let checkpoint = body.add_op(checkpoint_block, Operator::Nop, &[], &[]);
            body.set_terminator(
                checkpoint_block,
                Terminator::Br {
                    target: BlockTarget {
                        block: header,
                        args: vec![x],
                    },
                },
            );
            body.set_terminator(exit_block, Terminator::Return { values: vec![x] });

            body.recompute_edges();
            let fatrefs: HashSet<Value> = [x_entry, x].into_iter().collect();
            let result = compute(&body, |v| fatrefs.contains(&v), is_checkpoint).expect("liveness");
            assert_eq!(result.checkpoint_live[&checkpoint], vec![x]);
        }
    }
}

use liveness::Liveness;

/// Reachable-block reverse postorder, so translating blocks in this order
/// guarantees every dominating definition is already translated (SSA
/// requires def-dominates-use; entry dominates everything and back-edge
/// targets are always loop headers whose params — not instruction results —
/// are what a back edge feeds). Unreachable blocks are appended afterward in
/// index order so every block still gets a terminator.
fn translation_order(body: &FunctionBody) -> Vec<Block> {
    let mut visited = HashSet::new();
    let mut postorder = Vec::new();
    let mut stack: Vec<(Block, usize)> = vec![(body.entry, 0)];
    visited.insert(body.entry);
    while let Some(&mut (block, ref mut index)) = stack.last_mut() {
        let succs = &body.blocks[block].succs;
        if *index < succs.len() {
            let next = succs[*index];
            *index += 1;
            if visited.insert(next) {
                stack.push((next, 0));
            }
        } else {
            postorder.push(block);
            stack.pop();
        }
    }
    postorder.reverse();
    for (block, _) in body.blocks.entries() {
        if visited.insert(block) {
            postorder.push(block);
        }
    }
    postorder
}

fn i32_const(body: &mut FunctionBody, block: Block, value: u32) -> Value {
    body.add_op(block, Operator::I32Const { value }, &[], &[Type::I32])
}

/// Per-function ABI plan, computed for every accepted source function before
/// any body is translated so (mutually) recursive/forward calls resolve.
struct FunctionPlan {
    lowered_func: Func,
    lowered_signature: Signature,
    param_plan: Vec<LowerValue>,
    return_plan: Vec<LowerValue>,
}

/// Whole-module lowering plan built by preflight.
pub(crate) struct ModulePlan {
    functions: BTreeMap<Func, FunctionPlan>,
    /// 1-based function-reference table slot for every `Func` ever named by
    /// a `RefFunc` operator (`docs/...` §6.7); slot `0` is reserved null.
    func_ref_slots: BTreeMap<Func, u32>,
    func_ref_table: Option<Table>,
    /// Source tables addressed by `CallIndirect`, copied into the output
    /// module with their `func_elements` remapped to lowered `Func`s.
    copied_tables: BTreeMap<Table, Table>,
    /// One flattened `(params..., i32 selector) -> returns` signature per
    /// distinct source signature ever named by a `CallRef`/`CallIndirect`
    /// site, interned once here (during preflight, which already holds
    /// `&mut Module`) so codegen never needs mutable module access.
    call_site_signatures: BTreeMap<Signature, Signature>,
}

/// Result of lowering: every accepted source function's lowered `Func`, and
/// the export list to publish (mirrors source exports whose function
/// accepted lowering).
pub(crate) struct LoweredModule {
    pub(crate) exports: Vec<(String, Func)>,
}

fn function_signature<'m>(
    source: &'m Module<'_>,
    signature: Signature,
) -> Result<(&'m Vec<Type>, &'m Vec<Type>), CoreGcError> {
    match &source.signatures[signature] {
        SignatureData::Func {
            params,
            returns,
            shared,
        } => {
            if *shared {
                return Err(CoreGcError {
                    message: format!(
                        "coregc lowering does not support shared function signature {}",
                        signature.index()
                    ),
                });
            }
            if returns.len() > 1 {
                return Err(CoreGcError {
                    message: format!(
                        "coregc lowering does not support multi-value returns (signature {})",
                        signature.index()
                    ),
                });
            }
            Ok((params, returns))
        }
        other => Err(CoreGcError {
            message: format!(
                "coregc lowering expected a function signature at {}, found {other:?}",
                signature.index()
            ),
        }),
    }
}

fn classify_all(
    source: &Module<'_>,
    inventory: &CoreGcInventory,
    types: &[Type],
) -> Result<Vec<LowerValue>, CoreGcError> {
    types
        .iter()
        .map(|&ty| classify_type(source, inventory, ty))
        .collect()
}

fn flatten_plan(plan: &[LowerValue]) -> Vec<Type> {
    plan.iter().flat_map(|value| value.flat_types()).collect()
}

fn intern_call_site_signature(
    source: &Module<'_>,
    inventory: &CoreGcInventory,
    out: &mut Module<'static>,
    sig_index: Signature,
    interned: &mut BTreeMap<Signature, Signature>,
) -> Result<(), CoreGcError> {
    if interned.contains_key(&sig_index) {
        return Ok(());
    }
    let (params, returns) = function_signature(source, sig_index)?;
    // The signature used by `CallIndirect` is the callee's own flattened
    // signature; the selector is a separate operand popped by
    // `call_indirect` itself, NOT a parameter in this signature (getting
    // this wrong makes the validator pop one more i32 than the call site
    // provides).
    let flat_params = flatten_plan(&classify_all(source, inventory, params)?);
    let flat_returns = flatten_plan(&classify_all(source, inventory, returns)?);
    let lowered_signature = out.signatures.push(SignatureData::Func {
        params: flat_params,
        returns: flat_returns,
        shared: false,
    });
    interned.insert(sig_index, lowered_signature);
    Ok(())
}

fn copy_source_table(
    source: &Module<'_>,
    out: &mut Module<'static>,
    table_index: Table,
    functions: &BTreeMap<Func, FunctionPlan>,
    copied_tables: &mut BTreeMap<Table, Table>,
) -> Result<(), CoreGcError> {
    if copied_tables.contains_key(&table_index) {
        return Ok(());
    }
    let source_table = &source.tables[table_index];
    if !matches!(
        source_table.ty,
        Type::Heap(WithNullable {
            value: HeapType::FuncRef,
            ..
        })
    ) {
        return Err(CoreGcError {
            message: format!(
                "coregc lowering only supports call_indirect against a funcref table (table {})",
                table_index.index()
            ),
        });
    }
    let remapped_elements = match &source_table.func_elements {
        Some(elements) => {
            let mut remapped = Vec::with_capacity(elements.len());
            for &element in elements {
                if element.is_invalid() {
                    remapped.push(Func::invalid());
                    continue;
                }
                let lowered = functions.get(&element).ok_or_else(|| CoreGcError {
                    message: format!(
                        "coregc lowering: table {} element names unsupported function {}",
                        table_index.index(),
                        element.index()
                    ),
                })?;
                remapped.push(lowered.lowered_func);
            }
            Some(remapped)
        }
        None => None,
    };
    let lowered_table = out.tables.push(TableData {
        ty: source_table.ty.clone(),
        initial: source_table.initial,
        max: source_table.max,
        func_elements: remapped_elements,
        table64: source_table.table64,
    });
    copied_tables.insert(table_index, lowered_table);
    Ok(())
}

/// Preflight: build `CoreGcInventory`/`CoreGcDescriptorTable` are the
/// caller's job (they already exist by the time this runs); this function
/// classifies every function's flattened ABI, validates every function body
/// against the total accepted-operator matcher, and inventories the
/// function-reference table (`docs/...` §6.1).
pub(crate) fn preflight(
    source: &Module<'_>,
    inventory: &CoreGcInventory,
    out: &mut Module<'static>,
) -> Result<ModulePlan, CoreGcError> {
    let mut functions = BTreeMap::new();
    for (func, decl) in source.funcs.entries() {
        let FuncDecl::Body(signature, _, _) = decl else {
            return Err(CoreGcError {
                message: format!(
                    "coregc lowering does not support function imports (function {})",
                    func.index()
                ),
            });
        };
        let (params, returns) = function_signature(source, *signature)?;
        let param_plan = classify_all(source, inventory, params)?;
        let return_plan = classify_all(source, inventory, returns)?;
        let lowered_params = flatten_plan(&param_plan);
        let lowered_returns = flatten_plan(&return_plan);
        let lowered_signature = out.signatures.push(SignatureData::Func {
            params: lowered_params,
            returns: lowered_returns,
            shared: false,
        });
        let lowered_func = out.funcs.push(FuncDecl::None(std::marker::PhantomData));
        functions.insert(
            func,
            FunctionPlan {
                lowered_func,
                lowered_signature,
                param_plan,
                return_plan,
            },
        );
    }

    let mut func_ref_slots: BTreeMap<Func, u32> = BTreeMap::new();
    let mut copied_tables: BTreeMap<Table, Table> = BTreeMap::new();
    let mut call_site_signatures: BTreeMap<Signature, Signature> = BTreeMap::new();
    let mut needs_func_ref_table = false;

    for (func, decl) in source.funcs.entries() {
        let FuncDecl::Body(_, _, body) = decl else {
            unreachable!("checked above");
        };
        if body.shared {
            return Err(CoreGcError {
                message: format!("coregc lowering does not support shared function {}", func.index()),
            });
        }
        validate_function_body(source, inventory, &functions, body, func)?;
        for (_, block_def) in body.blocks.entries() {
            match &block_def.terminator.terminator {
                Terminator::ReturnCallRef { sig, .. } => {
                    intern_call_site_signature(
                        source,
                        inventory,
                        out,
                        *sig,
                        &mut call_site_signatures,
                    )?;
                    needs_func_ref_table = true;
                }
                Terminator::ReturnCallIndirect { sig, table, .. } => {
                    intern_call_site_signature(
                        source,
                        inventory,
                        out,
                        *sig,
                        &mut call_site_signatures,
                    )?;
                    copy_source_table(source, out, *table, &functions, &mut copied_tables)?;
                }
                _ => {}
            }
        }
        for (_, def) in body.values.entries() {
            if let ValueDef::Operator(Operator::RefFunc { func_index }, _, _) = def {
                if !functions.contains_key(func_index) {
                    return Err(CoreGcError {
                        message: format!(
                            "coregc lowering: ref.func target {} was not accepted",
                            func_index.index()
                        ),
                    });
                }
                let next_slot = u32::try_from(func_ref_slots.len() + 1).map_err(|_| CoreGcError {
                    message: "coregc function-reference table exceeds u32 slots".to_owned(),
                })?;
                func_ref_slots.entry(*func_index).or_insert(next_slot);
            }
            if let ValueDef::Operator(
                Operator::CallRef { sig_index } | Operator::CallIndirect { sig_index, .. },
                _,
                _,
            ) = def
            {
                intern_call_site_signature(
                    source,
                    inventory,
                    out,
                    *sig_index,
                    &mut call_site_signatures,
                )?;
                needs_func_ref_table |= matches!(
                    def,
                    ValueDef::Operator(Operator::CallRef { .. }, _, _)
                );
            }
            if let ValueDef::Operator(Operator::CallIndirect { table_index, .. }, _, _) = def {
                copy_source_table(source, out, *table_index, &functions, &mut copied_tables)?;
            }
        }
    }

    let func_ref_table = if func_ref_slots.is_empty() && !needs_func_ref_table {
        None
    } else {
        let mut elements = vec![Func::invalid(); func_ref_slots.len() + 1];
        for (&func, &slot) in &func_ref_slots {
            elements[slot as usize] = functions[&func].lowered_func;
        }
        let table = out.tables.push(TableData {
            ty: Type::Heap(WithNullable {
                value: HeapType::FuncRef,
                nullable: true,
            }),
            initial: elements.len() as u64,
            max: Some(elements.len() as u64),
            func_elements: Some(elements),
            table64: false,
        });
        Some(table)
    };

    Ok(ModulePlan {
        functions,
        func_ref_slots,
        func_ref_table,
        copied_tables,
        call_site_signatures,
    })
}

/// The total accepted-operator/terminator matcher (`docs/...` §6.1 step 3).
/// Rejects with the operator name, function index, and value index the
/// moment something outside the v1 surface is found, before any output is
/// generated for *any* function.
fn validate_function_body(
    source: &Module<'_>,
    inventory: &CoreGcInventory,
    functions: &BTreeMap<Func, FunctionPlan>,
    body: &FunctionBody,
    func: Func,
) -> Result<(), CoreGcError> {
    for (value, def) in body.values.entries() {
        match def {
            ValueDef::BlockParam(..) | ValueDef::Alias(_) => {}
            ValueDef::Operator(op, _, tys) => {
                let types = &body.type_pool[*tys];
                if types.len() > 1 {
                    return Err(reject(func, value, "a multi-value result"));
                }
                for &ty in types {
                    classify_type(source, inventory, ty).map_err(|inner| CoreGcError {
                        message: format!(
                            "coregc lowering rejects function {} value {}: {}",
                            func.index(),
                            value.index(),
                            inner.message
                        ),
                    })?;
                }
                validate_operator(source, inventory, functions, op, func, value)?;
            }
            ValueDef::PickOutput(..) => {
                return Err(reject(func, value, "PickOutput (multi-value results are unsupported)"));
            }
            ValueDef::Placeholder(_) | ValueDef::None => {
                return Err(reject(func, value, "an unresolved value definition"));
            }
        }
    }
    for (block, block_def) in body.blocks.entries() {
        for &(ty, _) in &block_def.params {
            classify_type(source, inventory, ty).map_err(|inner| CoreGcError {
                message: format!(
                    "coregc lowering rejects function {} block {}: {}",
                    func.index(),
                    block.index(),
                    inner.message
                ),
            })?;
        }
        match &block_def.terminator.terminator {
            Terminator::Br { .. }
            | Terminator::CondBr { .. }
            | Terminator::Return { .. }
            | Terminator::ReturnCall { .. }
            | Terminator::ReturnCallIndirect { .. }
            | Terminator::ReturnCallRef { .. }
            | Terminator::Unreachable => {}
            other => {
                return Err(CoreGcError {
                    message: format!(
                        "coregc lowering rejects function {} block {}: unsupported terminator {other:?}",
                        func.index(),
                        block.index()
                    ),
                });
            }
        }
    }
    Ok(())
}

/// Pure scalar numeric/comparison/conversion operators that lower to
/// themselves unchanged (their operands and results are all core scalars).
/// This is deliberately the *comprehensive* non-SIMD scalar set from
/// waffle's `Operator` enum, not just the subset jsaw's `conv.rs` currently
/// emits, so a future jsaw change that starts emitting e.g. `I32Rotl` does
/// not silently become a lowering rejection.
fn is_scalar_passthrough(op: &Operator) -> bool {
    use Operator as O;
    matches!(
        op,
        // Constants
        O::I32Const { .. } | O::I64Const { .. } | O::F32Const { .. } | O::F64Const { .. }
        // i32 comparisons
        | O::I32Eqz | O::I32Eq | O::I32Ne | O::I32LtS | O::I32LtU | O::I32GtS | O::I32GtU
        | O::I32LeS | O::I32LeU | O::I32GeS | O::I32GeU
        // i64 comparisons
        | O::I64Eqz | O::I64Eq | O::I64Ne | O::I64LtS | O::I64LtU | O::I64GtS | O::I64GtU
        | O::I64LeS | O::I64LeU | O::I64GeS | O::I64GeU
        // f32/f64 comparisons
        | O::F32Eq | O::F32Ne | O::F32Lt | O::F32Gt | O::F32Le | O::F32Ge
        | O::F64Eq | O::F64Ne | O::F64Lt | O::F64Gt | O::F64Le | O::F64Ge
        // i32 arithmetic/bitwise
        | O::I32Clz | O::I32Ctz | O::I32Popcnt | O::I32Add | O::I32Sub | O::I32Mul
        | O::I32DivS | O::I32DivU | O::I32RemS | O::I32RemU | O::I32And | O::I32Or
        | O::I32Xor | O::I32Shl | O::I32ShrS | O::I32ShrU | O::I32Rotl | O::I32Rotr
        // i64 arithmetic/bitwise
        | O::I64Clz | O::I64Ctz | O::I64Popcnt | O::I64Add | O::I64Sub | O::I64Mul
        | O::I64DivS | O::I64DivU | O::I64RemS | O::I64RemU | O::I64And | O::I64Or
        | O::I64Xor | O::I64Shl | O::I64ShrS | O::I64ShrU | O::I64Rotl | O::I64Rotr
        // f32 arithmetic
        | O::F32Abs | O::F32Neg | O::F32Ceil | O::F32Floor | O::F32Trunc | O::F32Nearest
        | O::F32Sqrt | O::F32Add | O::F32Sub | O::F32Mul | O::F32Div | O::F32Min
        | O::F32Max | O::F32Copysign
        // f64 arithmetic
        | O::F64Abs | O::F64Neg | O::F64Ceil | O::F64Floor | O::F64Trunc | O::F64Nearest
        | O::F64Sqrt | O::F64Add | O::F64Sub | O::F64Mul | O::F64Div | O::F64Min
        | O::F64Max | O::F64Copysign
        // Conversions
        | O::I32WrapI64 | O::I32TruncF32S | O::I32TruncF32U | O::I32TruncF64S | O::I32TruncF64U
        | O::I64ExtendI32S | O::I64ExtendI32U | O::I64TruncF32S | O::I64TruncF32U
        | O::I64TruncF64S | O::I64TruncF64U | O::F32ConvertI32S | O::F32ConvertI32U
        | O::F32ConvertI64S | O::F32ConvertI64U | O::F32DemoteF64 | O::F64ConvertI32S
        | O::F64ConvertI32U | O::F64ConvertI64S | O::F64ConvertI64U | O::F64PromoteF32
        | O::I32Extend8S | O::I32Extend16S | O::I64Extend8S | O::I64Extend16S | O::I64Extend32S
        | O::I32TruncSatF32S | O::I32TruncSatF32U | O::I32TruncSatF64S | O::I32TruncSatF64U
        | O::I64TruncSatF32S | O::I64TruncSatF32U | O::I64TruncSatF64S | O::I64TruncSatF64U
        // Reinterpretations
        | O::F32ReinterpretI32 | O::F64ReinterpretI64 | O::I32ReinterpretF32 | O::I64ReinterpretF64
    )
}

fn reject(func: Func, value: Value, what: &str) -> CoreGcError {
    CoreGcError {
        message: format!(
            "coregc lowering rejects function {} value {}: {what}",
            func.index(),
            value.index()
        ),
    }
}

fn validate_operator(
    source: &Module<'_>,
    inventory: &CoreGcInventory,
    functions: &BTreeMap<Func, FunctionPlan>,
    op: &Operator,
    func: Func,
    value: Value,
) -> Result<(), CoreGcError> {
    let accepted = match op {
        Operator::StructNew { .. }
        | Operator::StructGet { .. }
        | Operator::StructSet { .. }
        | Operator::StructNewDefault { .. }
        | Operator::StructGetS { .. }
        | Operator::StructGetU { .. }
        | Operator::ArrayNewDefault { .. }
        | Operator::ArrayNewFixed { .. }
        | Operator::ArrayGet { .. }
        | Operator::ArrayGetS { .. }
        | Operator::ArrayGetU { .. }
        | Operator::ArraySet { .. }
        | Operator::ArrayFill { .. }
        | Operator::ArrayCopy { .. }
        | Operator::ArrayLen
        | Operator::RefNull { .. }
        | Operator::RefIsNull
        | Operator::RefFunc { .. }
        | Operator::RefI31
        | Operator::RefTest { .. }
        | Operator::RefCast { .. }
        | Operator::RefEq
        | Operator::Select
        | Operator::TypedSelect { .. } => true,
        Operator::Call { function_index } => {
            if !functions.contains_key(function_index) {
                return Err(reject(func, value, "a call to an unsupported/unclassified function"));
            }
            true
        }
        Operator::CallRef { sig_index } | Operator::CallIndirect { sig_index, .. } => {
            function_signature(source, *sig_index)?;
            true
        }
        // Scalar operators lower to themselves; the comprehensive set lives
        // in `is_scalar_passthrough` so this accept list cannot drift from it
        // (jsaw's compiler output exercises most of these).
        _ => is_scalar_passthrough(op),
    };
    if accepted {
        Ok(())
    } else {
        Err(reject(func, value, &format!("unsupported operator {op:?}")))
    }
}

// ---------------------------------------------------------------------
// Codegen
// ---------------------------------------------------------------------

use crate::coregc_layout::CoreGcSlotLayout;
use portal_pc_waffle::MemoryArg;

/// A small cursor over "the block we are currently appending to" within one
/// source block's translation. A checked runtime trap (e.g. an array-length
/// overflow check) must branch to a fresh block; every instruction lowered
/// after that point continues in the new block, while branch targets from
/// *other* source blocks always resolve through `block_map` to the first
/// piece, so mid-translation splits are invisible to the rest of the CFG.
struct Emit<'b> {
    body: &'b mut FunctionBody,
    current: Block,
}

impl<'b> Emit<'b> {
    fn op(&mut self, op: Operator, args: &[Value], tys: &[Type]) -> Value {
        self.body.add_op(self.current, op, args, tys)
    }

    fn const_i32(&mut self, value: u32) -> Value {
        i32_const(self.body, self.current, value)
    }

    fn const_i64(&mut self, value: u64) -> Value {
        self.body
            .add_op(self.current, Operator::I64Const { value }, &[], &[Type::I64])
    }

    /// Branch to an `Unreachable` block if `cond` holds; every later
    /// instruction in this source block continues in a fresh continuation
    /// block.
    /// Extract one element of a multi-result value (Waffle requires
    /// `PickOutput` to project a tuple-typed operator result, §6.3).
    fn pick(&mut self, from: Value, index: u32, ty: Type) -> Value {
        let value = self.body.add_value(ValueDef::PickOutput(from, index, ty));
        self.body.append_to_block(self.current, value);
        value
    }

    /// The trap identity is recorded in the runtime's `trap_code` global
    /// first (see `coregc_runtime::trap_code`).
    fn trap_if(&mut self, cond: Value, trap_global: Global, code: u32) {
        let trap_block = self.body.add_block();
        let continue_block = self.body.add_block();
        self.body.set_terminator(
            self.current,
            Terminator::CondBr {
                cond,
                if_true: BlockTarget {
                    block: trap_block,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: continue_block,
                    args: vec![],
                },
            },
        );
        let code_value = i32_const(self.body, trap_block, code);
        self.body.add_op(
            trap_block,
            Operator::GlobalSet {
                global_index: trap_global,
            },
            &[code_value],
            &[],
        );
        self.body.set_terminator(trap_block, Terminator::Unreachable);
        self.current = continue_block;
    }
}

fn alignment_exponent(storage: &CoreGcStorage) -> u32 {
    match storage {
        CoreGcStorage::I8 => 0,
        CoreGcStorage::I16 => 1,
        CoreGcStorage::I32 | CoreGcStorage::F32 | CoreGcStorage::FuncRef { .. } => 2,
        CoreGcStorage::I64 | CoreGcStorage::F64 | CoreGcStorage::ManagedRef { .. } => 3,
        CoreGcStorage::DynamicRef { .. } => unreachable!("rejected by classify_type"),
    }
}

fn scalar_result_type(storage: &CoreGcStorage) -> Type {
    match storage {
        CoreGcStorage::I8 | CoreGcStorage::I16 | CoreGcStorage::I32 | CoreGcStorage::FuncRef { .. } => {
            Type::I32
        }
        CoreGcStorage::I64 => Type::I64,
        CoreGcStorage::F32 => Type::F32,
        CoreGcStorage::F64 => Type::F64,
        CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. } => {
            unreachable!("handled as a fat pair, not a scalar")
        }
    }
}

fn scalar_load_operator(storage: &CoreGcStorage, memory: MemoryArg) -> Operator {
    match storage {
        CoreGcStorage::I8 => Operator::I32Load8U { memory },
        CoreGcStorage::I16 => Operator::I32Load16U { memory },
        CoreGcStorage::I32 | CoreGcStorage::FuncRef { .. } => Operator::I32Load { memory },
        CoreGcStorage::I64 => Operator::I64Load { memory },
        CoreGcStorage::F32 => Operator::F32Load { memory },
        CoreGcStorage::F64 => Operator::F64Load { memory },
        CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. } => {
            unreachable!("handled as a fat pair, not a scalar")
        }
    }
}

fn scalar_store_operator(storage: &CoreGcStorage, memory: MemoryArg) -> Operator {
    match storage {
        CoreGcStorage::I8 => Operator::I32Store8 { memory },
        CoreGcStorage::I16 => Operator::I32Store16 { memory },
        CoreGcStorage::I32 | CoreGcStorage::FuncRef { .. } => Operator::I32Store { memory },
        CoreGcStorage::I64 => Operator::I64Store { memory },
        CoreGcStorage::F32 => Operator::F32Store { memory },
        CoreGcStorage::F64 => Operator::F64Store { memory },
        CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. } => {
            unreachable!("handled as a fat pair, not a scalar")
        }
    }
}

/// Like [`load_field`], but for the packed sign/zero-extending reads
/// (`struct.get_s/u`, `array.get_s/u`). Only 1/2-byte scalar storage is
/// packed; everything else (and managed refs, which are never packed) goes
/// through the ordinary path.
fn load_field_extended(
    emit: &mut Emit<'_>,
    memory: portal_pc_waffle::Memory,
    base_addr: Value,
    slot: &CoreGcSlotLayout,
    signed: bool,
) -> Result<Lowered, CoreGcError> {
    let packed = matches!(slot.storage, CoreGcStorage::I8 | CoreGcStorage::I16);
    if !packed {
        return Ok(load_field(emit, memory, base_addr, slot));
    }
    let mem = MemoryArg {
        align: alignment_exponent(&slot.storage),
        offset: u64::from(slot.offset),
        memory,
    };
    let op = match (&slot.storage, signed) {
        (CoreGcStorage::I8, true) => Operator::I32Load8S { memory: mem },
        (CoreGcStorage::I8, false) => Operator::I32Load8U { memory: mem },
        (CoreGcStorage::I16, true) => Operator::I32Load16S { memory: mem },
        (CoreGcStorage::I16, false) => Operator::I32Load16U { memory: mem },
        _ => unreachable!("packed checked above"),
    };
    Ok(Lowered::Scalar(emit.op(op, &[base_addr], &[Type::I32])))
}

fn store_field(
    emit: &mut Emit<'_>,
    memory: portal_pc_waffle::Memory,
    base_addr: Value,
    slot: &CoreGcSlotLayout,
    value: Lowered,
) -> Result<(), CoreGcError> {
    match (&slot.storage, value) {
        (CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. }, Lowered::Fat(addr, type_id)) => {
            // Each word of the fat pair is an ordinary i32 access; the
            // slot's own (8-byte) alignment governs its *offset*, not the
            // natural alignment of each individual i32 load/store.
            let mem0 = MemoryArg {
                align: 2,
                offset: u64::from(slot.offset),
                memory,
            };
            let mem1 = MemoryArg {
                offset: u64::from(slot.offset) + 4,
                ..mem0
            };
            emit.op(Operator::I32Store { memory: mem0 }, &[base_addr, addr], &[]);
            emit.op(Operator::I32Store { memory: mem1 }, &[base_addr, type_id], &[]);
            Ok(())
        }
        (storage, Lowered::Scalar(v)) if !matches!(storage, CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. }) => {
            let mem = MemoryArg {
                align: alignment_exponent(storage),
                offset: u64::from(slot.offset),
                memory,
            };
            emit.op(scalar_store_operator(storage, mem), &[base_addr, v], &[]);
            Ok(())
        }
        _ => Err(CoreGcError {
            message: "coregc lowering: field storage kind does not match the value being stored"
                .to_owned(),
        }),
    }
}

fn load_field(
    emit: &mut Emit<'_>,
    memory: portal_pc_waffle::Memory,
    base_addr: Value,
    slot: &CoreGcSlotLayout,
) -> Lowered {
    match &slot.storage {
        CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. } => {
            let mem0 = MemoryArg {
                align: 2,
                offset: u64::from(slot.offset),
                memory,
            };
            let mem1 = MemoryArg {
                offset: u64::from(slot.offset) + 4,
                ..mem0
            };
            let addr = emit.op(Operator::I32Load { memory: mem0 }, &[base_addr], &[Type::I32]);
            let type_id = emit.op(Operator::I32Load { memory: mem1 }, &[base_addr], &[Type::I32]);
            Lowered::Fat(addr, type_id)
        }
        storage => {
            let mem = MemoryArg {
                align: alignment_exponent(storage),
                offset: u64::from(slot.offset),
                memory,
            };
            let ty = scalar_result_type(storage);
            let value = emit.op(scalar_load_operator(storage, mem), &[base_addr], &[ty]);
            Lowered::Scalar(value)
        }
    }
}

struct FunctionLowering<'a> {
    source: &'a Module<'a>,
    inventory: &'a CoreGcInventory,
    descriptors: &'a CoreGcDescriptorTable,
    runtime: &'a CoreGcRuntime,
    plan: &'a ModulePlan,
    source_body: &'a FunctionBody,
    value_plan: HashMap<Value, LowerValue>,
    value_map: HashMap<Value, Lowered>,
    block_map: HashMap<Block, Block>,
    slot_of: BTreeMap<Value, u32>,
    frame: Option<Value>,
}

impl<'a> FunctionLowering<'a> {
    fn resolve(&self, value: Value) -> Value {
        self.source_body.resolve_alias(value)
    }

    fn lowered(&self, value: Value) -> Lowered {
        let resolved = self.resolve(value);
        *self.value_map.get(&resolved).unwrap_or_else(|| {
            panic!(
                "coregc lowering internal error: value {} used before definition",
                resolved.index()
            )
        })
    }

    fn scalar(&self, value: Value) -> Value {
        match self.lowered(value) {
            Lowered::Scalar(v) => v,
            Lowered::Fat(..) => panic!("coregc lowering internal error: expected a scalar value"),
        }
    }

    fn fat(&self, value: Value) -> (Value, Value) {
        match self.lowered(value) {
            Lowered::Fat(addr, ty) => (addr, ty),
            Lowered::Scalar(_) => panic!("coregc lowering internal error: expected a fat reference"),
        }
    }

    fn descriptor(&self, sig: Signature) -> Result<(crate::coregc::CoreGcTypeId, &crate::coregc_layout::CoreGcPayloadLayout), CoreGcError> {
        let type_id = self.inventory.id_for(sig).ok_or_else(|| CoreGcError {
            message: format!(
                "coregc lowering: signature {} is not a managed type",
                sig.index()
            ),
        })?;
        let layout = self
            .descriptors
            .layouts
            .iter()
            .find(|layout| layout.id == type_id)
            .ok_or_else(|| CoreGcError {
                message: format!("coregc lowering: no descriptor for type {}", type_id.get()),
            })?;
        Ok((type_id, layout))
    }

    fn zero_frame(&self, emit: &mut Emit<'_>) -> Value {
        match self.frame {
            Some(frame) => frame,
            None => emit.const_i32(0),
        }
    }

    fn checkpoint(&self, emit: &mut Emit<'_>) {
        let frame = self.zero_frame(emit);
        emit.op(
            Operator::Call {
                function_index: self.runtime.checkpoint,
            },
            &[frame],
            &[],
        );
    }

    fn root_store(&self, emit: &mut Emit<'_>, slot: u32, addr: Value, type_id: Value) {
        let frame = self
            .frame
            .expect("coregc lowering internal error: root_store requires a pushed frame");
        let slot_c = emit.const_i32(slot);
        emit.op(
            Operator::Call {
                function_index: self.runtime.roots.store,
            },
            &[frame, slot_c, addr, type_id],
            &[],
        );
    }

    /// After lowering any instruction whose *source* value is spilled,
    /// immediately root-store it (`docs/...` §6.4 item 6).
    fn maybe_spill(&self, emit: &mut Emit<'_>, source_value: Value, lowered: Lowered) {
        if let Some(&slot) = self.slot_of.get(&source_value) {
            let (addr, type_id) = match lowered {
                Lowered::Fat(addr, type_id) => (addr, type_id),
                Lowered::Scalar(_) => {
                    panic!("coregc lowering internal error: only fat-ref values are spilled")
                }
            };
            self.root_store(emit, slot, addr, type_id);
        }
    }

    /// `ref.test ty` on the pair `(addr, type_id)` (docs plan §13.2).
    /// Returns an i32 predicate value.
    fn lower_ref_test(
        &self,
        emit: &mut Emit<'_>,
        addr: Value,
        type_id: Value,
        ty: Type,
    ) -> Result<Value, CoreGcError> {
        let Type::Heap(reference) = ty else {
            return Err(CoreGcError {
                message: format!("coregc lowering: ref.test against non-reference type {ty:?}"),
            });
        };
        // Validate the pair once (header coherence, immediate rule).
        emit.op(
            Operator::Call {
                function_index: self.runtime.validate_ref,
            },
            &[addr, type_id],
            &[Type::I32],
        );
        let zero = emit.const_i32(0);
        let addr_zero = emit.op(Operator::I32Eq, &[addr, zero], &[Type::I32]);
        let type_zero = emit.op(Operator::I32Eq, &[type_id, zero], &[Type::I32]);
        let is_null = emit.op(Operator::I32And, &[addr_zero, type_zero], &[Type::I32]);
        let not_null = emit.op(Operator::I32Eqz, &[is_null], &[Type::I32]);
        let type_matches = match reference.value {
            HeapType::Sig { sig_index } => {
                let id = self.inventory.id_for(sig_index).ok_or_else(|| CoreGcError {
                    message: format!(
                        "coregc lowering: ref.test against unmanaged signature {}",
                        sig_index.index()
                    ),
                })?;
                let id_c = emit.const_i32(id.get());
                emit.op(Operator::I32Eq, &[type_id, id_c], &[Type::I32])
            }
            HeapType::I31 => {
                let i31_c = emit.const_i32(crate::coregc_runtime::I31_TYPE_ID);
                emit.op(Operator::I32Eq, &[type_id, i31_c], &[Type::I32])
            }
            HeapType::Any | HeapType::Eq => {
                // Everything this backend produces (struct, array, i31) is in
                // the eq hierarchy — the test is purely about null.
                not_null
            }
            unsupported => {
                return Err(CoreGcError {
                    message: format!(
                        "coregc lowering does not support ref.test against {unsupported:?}"
                    ),
                });
            }
        };
        if reference.nullable {
            // ref.test (ref null T) also matches null.
            Ok(emit.op(Operator::I32Or, &[is_null, type_matches], &[Type::I32]))
        } else {
            Ok(type_matches)
        }
    }

    fn lower_operator(
        &self,
        emit: &mut Emit<'_>,
        op: &Operator,
        args: &[Value],
        result_ty: Option<Type>,
    ) -> Result<Option<Lowered>, CoreGcError> {
        let memory = self.runtime.memory;
        match op {
            Operator::StructNew { sig } => {
                let (type_id, layout) = self.descriptor(*sig)?;
                let payload_bytes = layout
                    .fixed_payload_bytes
                    .expect("struct descriptors always have a fixed payload size");
                self.checkpoint(emit);
                let type_id_c = emit.const_i32(type_id.get());
                let bytes_c = emit.const_i32(payload_bytes);
                let addr = emit.op(
                    Operator::Call {
                        function_index: self.runtime.alloc,
                    },
                    &[type_id_c, bytes_c],
                    &[Type::I32],
                );
                for (arg, slot) in args.iter().zip(&layout.slots) {
                    let value = self.lowered(*arg);
                    store_field(emit, memory, addr, slot, value)?;
                }
                Ok(Some(Lowered::Fat(addr, type_id_c)))
            }
            Operator::StructGet { sig, idx } => {
                let (type_id, layout) = self.descriptor(*sig)?;
                let slot = layout.slots.get(*idx as usize).ok_or_else(|| CoreGcError {
                    message: format!(
                        "coregc lowering: struct.get field {idx} is outside descriptor type {}",
                        type_id.get()
                    ),
                })?;
                let (addr, recv_type) = self.fat(args[0]);
                let type_id_c = emit.const_i32(type_id.get());
                let _ = recv_type;
                let checked = emit.op(
                    Operator::Call {
                        function_index: self.runtime.validate_ref,
                    },
                    &[addr, type_id_c],
                    &[Type::I32],
                );
                Ok(Some(load_field(emit, memory, checked, slot)))
            }
            Operator::StructSet { sig, idx } => {
                let (type_id, layout) = self.descriptor(*sig)?;
                let slot = layout.slots.get(*idx as usize).ok_or_else(|| CoreGcError {
                    message: format!(
                        "coregc lowering: struct.set field {idx} is outside descriptor type {}",
                        type_id.get()
                    ),
                })?;
                let (addr, _) = self.fat(args[0]);
                let type_id_c = emit.const_i32(type_id.get());
                let checked = emit.op(
                    Operator::Call {
                        function_index: self.runtime.validate_ref,
                    },
                    &[addr, type_id_c],
                    &[Type::I32],
                );
                let value = self.lowered(args[1]);
                store_field(emit, memory, checked, slot, value)?;
                Ok(None)
            }
            Operator::ArrayNewDefault { sig } => {
                let (type_id, layout) = self.descriptor(*sig)?;
                let stride = layout
                    .array_stride
                    .expect("array descriptors always have a stride");
                self.checkpoint(emit);
                let len = self.scalar(args[0]);
                let len64 = emit.op(Operator::I64ExtendI32U, &[len], &[Type::I64]);
                let stride64 = emit.const_i64(u64::from(stride));
                let mul64 = emit.op(Operator::I64Mul, &[len64, stride64], &[Type::I64]);
                let four64 = emit.const_i64(4);
                let total64 = emit.op(Operator::I64Add, &[mul64, four64], &[Type::I64]);
                let max64 = emit.const_i64(u64::from(u32::MAX));
                let overflow = emit.op(Operator::I64GtU, &[total64, max64], &[Type::I32]);
                emit.trap_if(overflow, self.runtime.trap_code, trap_code::OUT_OF_BOUNDS);
                let payload_bytes = emit.op(Operator::I32WrapI64, &[total64], &[Type::I32]);
                let type_id_c = emit.const_i32(type_id.get());
                let addr = emit.op(
                    Operator::Call {
                        function_index: self.runtime.alloc,
                    },
                    &[type_id_c, payload_bytes],
                    &[Type::I32],
                );
                emit.op(
                    Operator::I32Store {
                        memory: MemoryArg {
                            align: 2,
                            offset: 0,
                            memory,
                        },
                    },
                    &[addr, len],
                    &[],
                );
                let four = emit.const_i32(4);
                let element_base = emit.op(Operator::I32Add, &[addr, four], &[Type::I32]);
                let element_bytes = emit.op(Operator::I32Sub, &[payload_bytes, four], &[Type::I32]);
                emit.op(
                    Operator::Call {
                        function_index: self.runtime.zero_bytes,
                    },
                    &[element_base, element_bytes],
                    &[],
                );
                Ok(Some(Lowered::Fat(addr, type_id_c)))
            }
            Operator::ArrayLen => {
                let (addr, _) = self.fat(args[0]);
                let type_id = self.array_type_id(args[0])?;
                let type_id_c = emit.const_i32(type_id.get());
                let checked = emit.op(
                    Operator::Call {
                        function_index: self.runtime.validate_ref,
                    },
                    &[addr, type_id_c],
                    &[Type::I32],
                );
                let length = emit.op(
                    Operator::I32Load {
                        memory: MemoryArg {
                            align: 2,
                            offset: 0,
                            memory,
                        },
                    },
                    &[checked],
                    &[Type::I32],
                );
                Ok(Some(Lowered::Scalar(length)))
            }
            Operator::StructNewDefault { sig } => {
                let (type_id, layout) = self.descriptor(*sig)?;
                let payload_bytes = layout
                    .fixed_payload_bytes
                    .expect("struct descriptors always have a fixed payload size");
                self.checkpoint(emit);
                let type_id_c = emit.const_i32(type_id.get());
                let bytes_c = emit.const_i32(payload_bytes);
                let addr = emit.op(
                    Operator::Call {
                        function_index: self.runtime.alloc,
                    },
                    &[type_id_c, bytes_c],
                    &[Type::I32],
                );
                emit.op(
                    Operator::Call {
                        function_index: self.runtime.zero_bytes,
                    },
                    &[addr, bytes_c],
                    &[],
                );
                Ok(Some(Lowered::Fat(addr, type_id_c)))
            }
            Operator::StructGetS { sig, idx } | Operator::StructGetU { sig, idx } => {
                let (type_id, layout) = self.descriptor(*sig)?;
                let slot = layout.slots.get(*idx as usize).ok_or_else(|| CoreGcError {
                    message: format!(
                        "coregc lowering: struct.get_s/u field {idx} is outside descriptor type {}",
                        type_id.get()
                    ),
                })?;
                let (addr, _) = self.fat(args[0]);
                let type_id_c = emit.const_i32(type_id.get());
                let checked = emit.op(
                    Operator::Call {
                        function_index: self.runtime.validate_ref,
                    },
                    &[addr, type_id_c],
                    &[Type::I32],
                );
                let signed = matches!(op, Operator::StructGetS { .. });
                Ok(Some(load_field_extended(
                    emit,
                    memory,
                    checked,
                    slot,
                    signed,
                )?))
            }
            Operator::ArrayNewFixed { sig, num } => {
                let (type_id, layout) = self.descriptor(*sig)?;
                let stride = layout.array_stride.expect("array descriptor has a stride");
                let element_slot = CoreGcSlotLayout {
                    offset: 0,
                    storage: layout.slots[0].storage.clone(),
                };
                if args.len() != *num {
                    return Err(CoreGcError {
                        message: format!(
                            "coregc lowering: array.new_fixed with {} elements for a {}-element constructor",
                            args.len(),
                            num
                        ),
                    });
                }
                self.checkpoint(emit);
                // payload_bytes = 4 + num*stride is a compile-time constant;
                // it still goes through the allocator's own overflow checks.
                let payload_bytes = 4u32
                    .checked_add((*num as u32).checked_mul(stride).ok_or_else(|| CoreGcError {
                        message: "coregc lowering: array.new_fixed payload size overflows u32"
                            .to_owned(),
                    })?)
                    .ok_or_else(|| CoreGcError {
                        message: "coregc lowering: array.new_fixed payload size overflows u32"
                            .to_owned(),
                    })?;
                let type_id_c = emit.const_i32(type_id.get());
                let bytes_c = emit.const_i32(payload_bytes);
                let addr = emit.op(
                    Operator::Call {
                        function_index: self.runtime.alloc,
                    },
                    &[type_id_c, bytes_c],
                    &[Type::I32],
                );
                let len_c = emit.const_i32(*num as u32);
                emit.op(
                    Operator::I32Store {
                        memory: MemoryArg {
                            align: 2,
                            offset: 0,
                            memory,
                        },
                    },
                    &[addr, len_c],
                    &[],
                );
                for (index, arg) in args.iter().enumerate() {
                    let offset = 4 + (index as u32) * stride;
                    let offset_c = emit.const_i32(offset);
                    let element_addr = emit.op(Operator::I32Add, &[addr, offset_c], &[Type::I32]);
                    let value = self.lowered(*arg);
                    store_field(emit, memory, element_addr, &element_slot, value)?;
                }
                Ok(Some(Lowered::Fat(addr, type_id_c)))
            }
            Operator::ArrayGetS { sig } | Operator::ArrayGetU { sig } => {
                let (type_id, layout) = self.descriptor(*sig)?;
                let element_slot = CoreGcSlotLayout {
                    offset: 0,
                    storage: layout.slots[0].storage.clone(),
                };
                let stride = layout.array_stride.expect("array descriptor has a stride");
                let (addr, _) = self.fat(args[0]);
                let index = self.scalar(args[1]);
                let type_id_c = emit.const_i32(type_id.get());
                let checked_index = emit.op(
                    Operator::Call {
                        function_index: self.runtime.array_bounds,
                    },
                    &[addr, type_id_c, index],
                    &[Type::I32],
                );
                let stride_c = emit.const_i32(stride);
                let offset = emit.op(Operator::I32Mul, &[checked_index, stride_c], &[Type::I32]);
                let four = emit.const_i32(4);
                let offset = emit.op(Operator::I32Add, &[offset, four], &[Type::I32]);
                let element_addr = emit.op(Operator::I32Add, &[addr, offset], &[Type::I32]);
                let signed = matches!(op, Operator::ArrayGetS { .. });
                Ok(Some(load_field_extended(
                    emit,
                    memory,
                    element_addr,
                    &element_slot,
                    signed,
                )?))
            }
            Operator::ArrayFill { sig } => {
                let (type_id, layout) = self.descriptor(*sig)?;
                let stride = layout.array_stride.expect("array descriptor has a stride");
                let element_slot = CoreGcSlotLayout {
                    offset: 0,
                    storage: layout.slots[0].storage.clone(),
                };
                let (addr, _) = self.fat(args[0]);
                let start = self.scalar(args[1]);
                let value = self.lowered(args[2]);
                let len = self.scalar(args[3]);
                let type_id_c = emit.const_i32(type_id.get());
                let checked_addr = emit.op(
                    Operator::Call {
                        function_index: self.runtime.validate_ref,
                    },
                    &[addr, type_id_c],
                    &[Type::I32],
                );
                // Trap unless start + len <= array.len (computed in i64 to
                // avoid u32 wrap).
                let array_len = emit.op(
                    Operator::I32Load {
                        memory: MemoryArg {
                            align: 2,
                            offset: 0,
                            memory,
                        },
                    },
                    &[checked_addr],
                    &[Type::I32],
                );
                let start64 = emit.op(Operator::I64ExtendI32U, &[start], &[Type::I64]);
                let len64 = emit.op(Operator::I64ExtendI32U, &[len], &[Type::I64]);
                let end64 = emit.op(Operator::I64Add, &[start64, len64], &[Type::I64]);
                let array_len64 = emit.op(Operator::I64ExtendI32U, &[array_len], &[Type::I64]);
                let out_of_range = emit.op(Operator::I64GtU, &[end64, array_len64], &[Type::I32]);
                emit.trap_if(out_of_range, self.runtime.trap_code, trap_code::OUT_OF_BOUNDS);
                // Loop: for k in 0..len, store element at start + k.
                let loop_head = emit.body.add_block();
                let loop_body = emit.body.add_block();
                let zero_k = emit.const_i32(0);
                emit.body.set_terminator(
                    emit.current,
                    Terminator::Br {
                        target: BlockTarget {
                            block: loop_head,
                            args: vec![zero_k],
                        },
                    },
                );
                let k = emit.body.add_blockparam(loop_head, Type::I32);
                let done = emit.body.add_op(loop_head, Operator::I32GeU, &[k, len], &[Type::I32]);
                let after = emit.body.add_block();
                emit.body.set_terminator(
                    loop_head,
                    Terminator::CondBr {
                        cond: done,
                        if_true: BlockTarget {
                            block: after,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: loop_body,
                            args: vec![k],
                        },
                    },
                );
                let bk = emit.body.add_blockparam(loop_body, Type::I32);
                let idx = emit.body.add_op(loop_body, Operator::I32Add, &[start, bk], &[Type::I32]);
                let stride_c = emit.body.add_op(
                    loop_body,
                    Operator::I32Const { value: stride },
                    &[],
                    &[Type::I32],
                );
                let offset = emit.body.add_op(loop_body, Operator::I32Mul, &[idx, stride_c], &[Type::I32]);
                let four = emit.body.add_op(loop_body, Operator::I32Const { value: 4 }, &[], &[Type::I32]);
                let offset = emit.body.add_op(loop_body, Operator::I32Add, &[offset, four], &[Type::I32]);
                let element_addr =
                    emit.body.add_op(loop_body, Operator::I32Add, &[checked_addr, offset], &[Type::I32]);
                let mut fill_emit = Emit {
                    body: emit.body,
                    current: loop_body,
                };
                store_field(&mut fill_emit, memory, element_addr, &element_slot, value)?;
                let one = fill_emit.const_i32(1);
                let next_k = fill_emit.body.add_op(loop_body, Operator::I32Add, &[bk, one], &[Type::I32]);
                fill_emit.body.set_terminator(
                    loop_body,
                    Terminator::Br {
                        target: BlockTarget {
                            block: loop_head,
                            args: vec![next_k],
                        },
                    },
                );
                emit.current = after;
                Ok(None)
            }
            Operator::ArrayCopy { dest, src } => {
                let (dst_type, dst_layout) = self.descriptor(*dest)?;
                let (src_type, src_layout) = self.descriptor(*src)?;
                let dst_stride = dst_layout.array_stride.expect("array descriptor has a stride");
                let src_stride = src_layout.array_stride.expect("array descriptor has a stride");
                let (dst_addr, _) = self.fat(args[0]);
                let di = self.scalar(args[1]);
                let (src_addr, _) = self.fat(args[2]);
                let si = self.scalar(args[3]);
                let len = self.scalar(args[4]);
                let dst_type_c = emit.const_i32(dst_type.get());
                let checked_dst = emit.op(
                    Operator::Call {
                        function_index: self.runtime.validate_ref,
                    },
                    &[dst_addr, dst_type_c],
                    &[Type::I32],
                );
                let src_type_c = emit.const_i32(src_type.get());
                let checked_src = emit.op(
                    Operator::Call {
                        function_index: self.runtime.validate_ref,
                    },
                    &[src_addr, src_type_c],
                    &[Type::I32],
                );
                let mem0 = MemoryArg {
                    align: 2,
                    offset: 0,
                    memory,
                };
                let dst_len = emit.op(Operator::I32Load { memory: mem0 }, &[checked_dst], &[Type::I32]);
                let src_len = emit.op(Operator::I32Load { memory: mem0 }, &[checked_src], &[Type::I32]);
                for (index_arg, array_len) in [(di, dst_len), (si, src_len)] {
                    let index64 = emit.op(Operator::I64ExtendI32U, &[index_arg], &[Type::I64]);
                    let len64 = emit.op(Operator::I64ExtendI32U, &[len], &[Type::I64]);
                    let end64 = emit.op(Operator::I64Add, &[index64, len64], &[Type::I64]);
                    let array_len64 = emit.op(Operator::I64ExtendI32U, &[array_len], &[Type::I64]);
                    let out_of_range = emit.op(Operator::I64GtU, &[end64, array_len64], &[Type::I32]);
                    emit.trap_if(out_of_range, self.runtime.trap_code, trap_code::OUT_OF_BOUNDS);
                }
                // Byte-wise memmove over the element region: copy backward
                // when dst > src (same-array overlap), forward otherwise.
                let di_bytes = {
                    let stride_c = emit.const_i32(dst_stride);
                    let offset = emit.op(Operator::I32Mul, &[di, stride_c], &[Type::I32]);
                    let four = emit.const_i32(4);
                    let offset = emit.op(Operator::I32Add, &[offset, four], &[Type::I32]);
                    emit.op(Operator::I32Add, &[checked_dst, offset], &[Type::I32])
                };
                let si_bytes = {
                    let stride_c = emit.const_i32(src_stride);
                    let offset = emit.op(Operator::I32Mul, &[si, stride_c], &[Type::I32]);
                    let four = emit.const_i32(4);
                    let offset = emit.op(Operator::I32Add, &[offset, four], &[Type::I32]);
                    emit.op(Operator::I32Add, &[checked_src, offset], &[Type::I32])
                };
                let byte_len = {
                    let stride_c = emit.const_i32(dst_stride);
                    emit.op(Operator::I32Mul, &[len, stride_c], &[Type::I32])
                };
                let backward = emit.op(Operator::I32GtU, &[di_bytes, si_bytes], &[Type::I32]);
                let down_loop = emit.body.add_block();
                let up_loop = emit.body.add_block();
                let after = emit.body.add_block();
                emit.body.set_terminator(
                    emit.current,
                    Terminator::CondBr {
                        cond: backward,
                        // dst > src: copy high addresses first (count down)
                        // so overlapping source bytes are read before being
                        // overwritten.
                        if_true: BlockTarget {
                            block: down_loop,
                            args: vec![byte_len],
                        },
                        if_false: BlockTarget {
                            block: up_loop,
                            args: vec![],
                        },
                    },
                );
                // Down: i counts down from byte_len to 0, copying i-1.
                let forward_loop = down_loop;
                let fi = emit.body.add_blockparam(forward_loop, Type::I32);
                let f_done = emit.body.add_op(forward_loop, Operator::I32Eqz, &[fi], &[Type::I32]);
                let f_body = emit.body.add_block();
                emit.body.set_terminator(
                    forward_loop,
                    Terminator::CondBr {
                        cond: f_done,
                        if_true: BlockTarget {
                            block: after,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: f_body,
                            args: vec![fi],
                        },
                    },
                );
                let fbi = emit.body.add_blockparam(f_body, Type::I32);
                let one_f = emit.body.add_op(f_body, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
                let prev = emit.body.add_op(f_body, Operator::I32Sub, &[fbi, one_f], &[Type::I32]);
                let src_p = emit.body.add_op(f_body, Operator::I32Add, &[si_bytes, prev], &[Type::I32]);
                let dst_p = emit.body.add_op(f_body, Operator::I32Add, &[di_bytes, prev], &[Type::I32]);
                let byte_v = emit.body.add_op(
                    f_body,
                    Operator::I32Load8U {
                        memory: MemoryArg {
                            align: 0,
                            offset: 0,
                            memory,
                        },
                    },
                    &[src_p],
                    &[Type::I32],
                );
                emit.body.add_op(
                    f_body,
                    Operator::I32Store8 {
                        memory: MemoryArg {
                            align: 0,
                            offset: 0,
                            memory,
                        },
                    },
                    &[dst_p, byte_v],
                    &[],
                );
                emit.body.set_terminator(
                    f_body,
                    Terminator::Br {
                        target: BlockTarget {
                            block: forward_loop,
                            args: vec![prev],
                        },
                    },
                );
                // Up: i counts up from 0 to byte_len (dst <= src, so reading
                // low addresses first cannot clobber unread source bytes).
                let backward_loop = up_loop;
                let zero_b = emit.body.add_op(
                    backward_loop,
                    Operator::I32Const { value: 0 },
                    &[],
                    &[Type::I32],
                );
                let b_loop_head = emit.body.add_block();
                emit.body.set_terminator(
                    backward_loop,
                    Terminator::Br {
                        target: BlockTarget {
                            block: b_loop_head,
                            args: vec![zero_b],
                        },
                    },
                );
                let bi = emit.body.add_blockparam(b_loop_head, Type::I32);
                let b_done = emit.body.add_op(b_loop_head, Operator::I32GeU, &[bi, byte_len], &[Type::I32]);
                let b_body = emit.body.add_block();
                emit.body.set_terminator(
                    b_loop_head,
                    Terminator::CondBr {
                        cond: b_done,
                        if_true: BlockTarget {
                            block: after,
                            args: vec![],
                        },
                        if_false: BlockTarget {
                            block: b_body,
                            args: vec![bi],
                        },
                    },
                );
                let bbi = emit.body.add_blockparam(b_body, Type::I32);
                let src_q = emit.body.add_op(b_body, Operator::I32Add, &[si_bytes, bbi], &[Type::I32]);
                let dst_q = emit.body.add_op(b_body, Operator::I32Add, &[di_bytes, bbi], &[Type::I32]);
                let byte_w = emit.body.add_op(
                    b_body,
                    Operator::I32Load8U {
                        memory: MemoryArg {
                            align: 0,
                            offset: 0,
                            memory,
                        },
                    },
                    &[src_q],
                    &[Type::I32],
                );
                emit.body.add_op(
                    b_body,
                    Operator::I32Store8 {
                        memory: MemoryArg {
                            align: 0,
                            offset: 0,
                            memory,
                        },
                    },
                    &[dst_q, byte_w],
                    &[],
                );
                let one_b = emit.body.add_op(b_body, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
                let next_b = emit.body.add_op(b_body, Operator::I32Add, &[bbi, one_b], &[Type::I32]);
                emit.body.set_terminator(
                    b_body,
                    Terminator::Br {
                        target: BlockTarget {
                            block: b_loop_head,
                            args: vec![next_b],
                        },
                    },
                );
                emit.current = after;
                Ok(None)
            }
            Operator::ArrayGet { sig } | Operator::ArraySet { sig } => {
                let (type_id, layout) = self.descriptor(*sig)?;
                // The descriptor's element "slot" already carries offset `4`
                // (the payload's length-word prefix, `docs/...` §3.3) for use
                // when reading it directly off payload offset zero. Here we
                // instead compute the exact per-element address ourselves
                // (`addr + 4 + index*stride`), so the field helpers below must
                // address *from* that already-complete pointer at offset
                // zero — reusing `slot.offset` again would double-count the
                // `+4` prefix and corrupt every element after the first.
                let element_slot = CoreGcSlotLayout {
                    offset: 0,
                    storage: layout.slots[0].storage.clone(),
                };
                let stride = layout.array_stride.expect("array descriptor has a stride");
                let (addr, _) = self.fat(args[0]);
                let index = self.scalar(args[1]);
                let type_id_c = emit.const_i32(type_id.get());
                let checked_index = emit.op(
                    Operator::Call {
                        function_index: self.runtime.array_bounds,
                    },
                    &[addr, type_id_c, index],
                    &[Type::I32],
                );
                let stride_c = emit.const_i32(stride);
                let offset = emit.op(Operator::I32Mul, &[checked_index, stride_c], &[Type::I32]);
                let four = emit.const_i32(4);
                let offset = emit.op(Operator::I32Add, &[offset, four], &[Type::I32]);
                let element_addr = emit.op(Operator::I32Add, &[addr, offset], &[Type::I32]);
                if matches!(op, Operator::ArrayGet { .. }) {
                    Ok(Some(load_field(emit, memory, element_addr, &element_slot)))
                } else {
                    let value = self.lowered(args[2]);
                    store_field(emit, memory, element_addr, &element_slot, value)?;
                    Ok(None)
                }
            }
            Operator::RefNull { ty } => {
                let plan = classify_type(self.source, self.inventory, *ty)?;
                match plan {
                    LowerValue::Scalar(_) => Ok(Some(Lowered::Scalar(emit.const_i32(0)))),
                    LowerValue::FatRef { .. } | LowerValue::Dynamic => {
                        let addr = emit.const_i32(0);
                        let type_id = emit.const_i32(0);
                        Ok(Some(Lowered::Fat(addr, type_id)))
                    }
                }
            }
            Operator::RefIsNull => {
                let zero = emit.const_i32(0);
                let result = match self.lowered(args[0]) {
                    Lowered::Scalar(v) => emit.op(Operator::I32Eq, &[v, zero], &[Type::I32]),
                    // Null is exactly the pair (0, 0): an i31 immediate with
                    // payload zero must NOT read as null (its type word is
                    // the sentinel, not 0).
                    Lowered::Fat(addr, type_id) => {
                        let addr_zero = emit.op(Operator::I32Eq, &[addr, zero], &[Type::I32]);
                        let type_zero = emit.op(Operator::I32Eq, &[type_id, zero], &[Type::I32]);
                        emit.op(Operator::I32And, &[addr_zero, type_zero], &[Type::I32])
                    }
                };
                Ok(Some(Lowered::Scalar(result)))
            }
            Operator::RefI31 => {
                let payload = self.scalar(args[0]);
                let i31_type = emit.const_i32(crate::coregc_runtime::I31_TYPE_ID);
                Ok(Some(Lowered::Fat(payload, i31_type)))
            }
            Operator::RefTest { ty } => {
                let (addr, type_id) = self.fat(args[0]);
                let result = self.lower_ref_test(emit, addr, type_id, *ty)?;
                Ok(Some(Lowered::Scalar(result)))
            }
            Operator::RefCast { ty } => {
                let (addr, type_id) = self.fat(args[0]);
                let tested = self.lower_ref_test(emit, addr, type_id, *ty)?;
                let not_tested = emit.op(Operator::I32Eqz, &[tested], &[Type::I32]);
                emit.trap_if(not_tested, self.runtime.trap_code, trap_code::BAD_CAST);
                // A cast never rewrites the pair: the actual type id is
                // preserved (the next test/cast/dereference re-checks it).
                Ok(Some(Lowered::Fat(addr, type_id)))
            }
            Operator::RefEq => {
                let (a_addr, a_type) = self.fat(args[0]);
                let (b_addr, b_type) = self.fat(args[1]);
                // Both pairs get validated (header coherence, immediate
                // rule), then compared per docs plan §13.2.
                for (addr, ty) in [(a_addr, a_type), (b_addr, b_type)] {
                    emit.op(
                        Operator::Call {
                            function_index: self.runtime.validate_ref,
                        },
                        &[addr, ty],
                        &[Type::I32],
                    );
                }
                let zero = emit.const_i32(0);
                let i31_type = emit.const_i32(crate::coregc_runtime::I31_TYPE_ID);
                let a_addr_zero = emit.op(Operator::I32Eq, &[a_addr, zero], &[Type::I32]);
                let a_type_zero = emit.op(Operator::I32Eq, &[a_type, zero], &[Type::I32]);
                let a_null = emit.op(Operator::I32And, &[a_addr_zero, a_type_zero], &[Type::I32]);
                let b_addr_zero = emit.op(Operator::I32Eq, &[b_addr, zero], &[Type::I32]);
                let b_type_zero = emit.op(Operator::I32Eq, &[b_type, zero], &[Type::I32]);
                let b_null = emit.op(Operator::I32And, &[b_addr_zero, b_type_zero], &[Type::I32]);
                let a_i31 = emit.op(Operator::I32Eq, &[a_type, i31_type], &[Type::I32]);
                let b_i31 = emit.op(Operator::I32Eq, &[b_type, i31_type], &[Type::I32]);
                let a_not_null = emit.op(Operator::I32Eqz, &[a_null], &[Type::I32]);
                let b_not_null = emit.op(Operator::I32Eqz, &[b_null], &[Type::I32]);
                let a_not_i31 = emit.op(Operator::I32Eqz, &[a_i31], &[Type::I32]);
                let a_heap = emit.op(Operator::I32And, &[a_not_null, a_not_i31], &[Type::I32]);
                let b_not_i31 = emit.op(Operator::I32Eqz, &[b_i31], &[Type::I32]);
                let b_heap = emit.op(Operator::I32And, &[b_not_null, b_not_i31], &[Type::I32]);
                let addr_eq = emit.op(Operator::I32Eq, &[a_addr, b_addr], &[Type::I32]);
                let type_ne = emit.op(Operator::I32Ne, &[a_type, b_type], &[Type::I32]);
                // Corruption: one address with two different heap types.
                let both_heap = emit.op(Operator::I32And, &[a_heap, b_heap], &[Type::I32]);
                let same_addr = emit.op(Operator::I32And, &[both_heap, addr_eq], &[Type::I32]);
                let corrupt = emit.op(Operator::I32And, &[same_addr, type_ne], &[Type::I32]);
                emit.trap_if(corrupt, self.runtime.trap_code, trap_code::ALLOCATOR_CORRUPTION);
                let both_null = emit.op(Operator::I32And, &[a_null, b_null], &[Type::I32]);
                let both_i31 = emit.op(Operator::I32And, &[a_i31, b_i31], &[Type::I32]);
                let i31_eq = emit.op(Operator::I32And, &[both_i31, addr_eq], &[Type::I32]);
                let heap_eq = emit.op(Operator::I32And, &[both_heap, addr_eq], &[Type::I32]);
                let eq = emit.op(Operator::I32Or, &[both_null, i31_eq], &[Type::I32]);
                let eq = emit.op(Operator::I32Or, &[eq, heap_eq], &[Type::I32]);
                Ok(Some(Lowered::Scalar(eq)))
            }
            Operator::Select => {
                // Untyped select only validates for numeric operand types, so
                // this is always scalar here.
                let a = self.scalar(args[0]);
                let b = self.scalar(args[1]);
                let cond = self.scalar(args[2]);
                let value = emit.op(Operator::Select, &[a, b, cond], &result_ty.into_iter().collect::<Vec<_>>());
                Ok(Some(Lowered::Scalar(value)))
            }
            Operator::TypedSelect { ty } => {
                let cond = self.scalar(args[2]);
                match (self.lowered(args[0]), self.lowered(args[1])) {
                    (Lowered::Scalar(a), Lowered::Scalar(b)) => {
                        let value = emit.op(Operator::TypedSelect { ty: *ty }, &[a, b, cond], &[*ty]);
                        Ok(Some(Lowered::Scalar(value)))
                    }
                    (Lowered::Fat(a_addr, a_ty), Lowered::Fat(b_addr, b_ty)) => {
                        // A select over a fat reference is two scalar selects.
                        let addr = emit.op(
                            Operator::TypedSelect { ty: Type::I32 },
                            &[a_addr, b_addr, cond],
                            &[Type::I32],
                        );
                        let type_id = emit.op(
                            Operator::TypedSelect { ty: Type::I32 },
                            &[a_ty, b_ty, cond],
                            &[Type::I32],
                        );
                        Ok(Some(Lowered::Fat(addr, type_id)))
                    }
                    _ => Err(CoreGcError {
                        message: "coregc lowering: select over mismatched value shapes".to_owned(),
                    }),
                }
            }
            Operator::RefFunc { func_index } => {
                let slot = *self
                    .plan
                    .func_ref_slots
                    .get(func_index)
                    .expect("preflight registered every ref.func target");
                Ok(Some(Lowered::Scalar(emit.const_i32(slot))))
            }
            Operator::Call { function_index } => {
                let callee = &self.plan.functions[function_index];
                self.checkpoint(emit);
                let mut flat_args = Vec::new();
                for &arg in args {
                    self.lowered(arg).flatten_into(&mut flat_args);
                }
                let flat_returns = flatten_plan(&callee.return_plan);
                let result = emit.op(
                    Operator::Call {
                        function_index: callee.lowered_func,
                    },
                    &flat_args,
                    &flat_returns,
                );
                let return_plan = callee.return_plan.clone();
                Ok(self.repair_call_result(emit, &return_plan, result))
            }
            Operator::CallRef { sig_index } | Operator::CallIndirect { sig_index, .. } => {
                let (_, returns) = function_signature(self.source, *sig_index)?;
                let return_plan = classify_all(self.source, self.inventory, returns)?;
                let flat_returns = flatten_plan(&return_plan);
                let lowered_signature = self.plan.call_site_signatures[sig_index];
                let call_args = &args[..args.len() - 1];
                let selector = args[args.len() - 1];
                self.checkpoint(emit);
                let mut flat_args = Vec::new();
                for &arg in call_args {
                    self.lowered(arg).flatten_into(&mut flat_args);
                }
                let selector_value = self.scalar(selector);
                flat_args.push(selector_value);
                let table = match op {
                    Operator::CallRef { .. } => self
                        .plan
                        .func_ref_table
                        .expect("preflight built a function-reference table for every RefFunc target"),
                    Operator::CallIndirect { table_index, .. } => self.plan.copied_tables[table_index],
                    _ => unreachable!(),
                };
                let result = emit.op(
                    Operator::CallIndirect {
                        sig_index: lowered_signature,
                        table_index: table,
                    },
                    &flat_args,
                    &flat_returns,
                );
                Ok(self.repair_call_result(emit, &return_plan, result))
            }
            _ => {
                // Ordinary scalar arithmetic/comparison/conversion operator:
                // arguments and result stay scalar, operator unchanged.
                let scalar_args: Vec<Value> = args.iter().map(|&a| self.scalar(a)).collect();
                let tys: Vec<Type> = match result_ty {
                    Some(ty) => vec![ty],
                    None => vec![],
                };
                if tys.is_empty() {
                    emit.op(op.clone(), &scalar_args, &[]);
                    Ok(None)
                } else {
                    let value = emit.op(op.clone(), &scalar_args, &tys);
                    Ok(Some(Lowered::Scalar(value)))
                }
            }
        }
    }

    fn array_type_id(&self, array_value: Value) -> Result<crate::coregc::CoreGcTypeId, CoreGcError> {
        let resolved = self.resolve(array_value);
        match self.value_plan.get(&resolved) {
            Some(LowerValue::FatRef { concrete, .. }) => Ok(*concrete),
            _ => Err(CoreGcError {
                message: "coregc lowering internal error: array.len receiver is not a fat reference"
                    .to_owned(),
            }),
        }
    }

    /// `Call`/`CallRef`/`CallIndirect` all return either zero flattened
    /// values, one scalar core value, or two (a fat pair, extracted with
    /// `PickOutput` since Waffle represents a multi-value operator result as
    /// one tuple-typed value, §6.3); repackage it back into a `Lowered`.
    fn repair_call_result(
        &self,
        emit: &mut Emit<'_>,
        return_plan: &[LowerValue],
        result: Value,
    ) -> Option<Lowered> {
        match return_plan.first() {
            None => None,
            Some(LowerValue::Scalar(_)) => Some(Lowered::Scalar(result)),
            Some(LowerValue::FatRef { .. }) | Some(LowerValue::Dynamic) => {
                let addr = emit.pick(result, 0, Type::I32);
                let type_id = emit.pick(result, 1, Type::I32);
                Some(Lowered::Fat(addr, type_id))
            }
        }
    }
}

/// Debug root-discipline verifier (`docs/plan-coregc-atomic-collector-and-lowering.md`
/// §6.4 item 7).
///
/// Checks that every value required to be rooted at a checkpoint has an
/// assigned shadow-frame slot, and that every slot-holding value is actually
/// defined somewhere in the source body (as a block parameter or an
/// instruction result — those are exactly the two sites where codegen emits
/// the corresponding `root_store`). A dominance argument is deliberately
/// unnecessary: the source body is valid SSA (Waffle validates
/// def-dominates-use), so the root_store emitted at each spilled value's
/// definition site necessarily dominates every checkpoint that needs it.
///
/// A violation is an internal lowering bug, not a source-level diagnostic.
/// Collect the flattened results of a call/call_indirect op result into
/// individual `Value`s (zero results → empty, one → itself, more →
/// `PickOutput` projections), for use by tail-call lowering which must
/// forward them to a `Return` terminator.
fn collect_flat_result(
    body: &mut FunctionBody,
    block: Block,
    result: Value,
    flat_returns: &[Type],
) -> Vec<Value> {
    match flat_returns.len() {
        0 => vec![],
        1 => vec![result],
        n => (0..n)
            .map(|index| {
                let value = body.add_value(ValueDef::PickOutput(result, index as u32, flat_returns[index]));
                body.append_to_block(block, value);
                value
            })
            .collect(),
    }
}

fn verify_root_discipline(
    body: &FunctionBody,
    liveness: &Liveness,
    slot_of: &BTreeMap<Value, u32>,
    func: Func,
) -> Result<(), CoreGcError> {
    for (checkpoint, required) in &liveness.checkpoint_live {
        for &value in required {
            if !slot_of.contains_key(&value) {
                return Err(CoreGcError {
                    message: format!(
                        "coregc root verifier: function {} checkpoint at value {} requires \
                         value {} to be rooted, but no shadow-frame slot was assigned to it",
                        func.index(),
                        checkpoint.index(),
                        value.index()
                    ),
                });
            }
        }
    }
    for (block, required) in &liveness.tail_call_live {
        for &value in required {
            if !slot_of.contains_key(&value) {
                return Err(CoreGcError {
                    message: format!(
                        "coregc root verifier: function {} tail call in block {} requires \
                         value {} to be rooted, but no shadow-frame slot was assigned to it",
                        func.index(),
                        block.index(),
                        value.index()
                    ),
                });
            }
        }
    }
    for &value in slot_of.keys() {
        let defined = match &body.values[value] {
            ValueDef::BlockParam(..) | ValueDef::Operator(..) => true,
            // Aliases resolve to their target before use, so a spilled value
            // is always stored at its resolved definition site; the alias
            // itself is not a definition site.
            ValueDef::Alias(_) => true,
            ValueDef::PickOutput(..) | ValueDef::Placeholder(_) | ValueDef::None => false,
        };
        if !defined {
            return Err(CoreGcError {
                message: format!(
                    "coregc root verifier: function {} assigns a root slot to value {} which has \
                     no definition site in the body",
                    func.index(),
                    value.index()
                ),
            });
        }
    }
    Ok(())
}

fn classify_function_body(
    source: &Module<'_>,
    inventory: &CoreGcInventory,
    source_body: &FunctionBody,
) -> Result<HashMap<Value, LowerValue>, CoreGcError> {
    let mut value_plan = HashMap::new();
    for (_, block_def) in source_body.blocks.entries() {
        for &(ty, param) in &block_def.params {
            value_plan.insert(param, classify_type(source, inventory, ty)?);
        }
    }
    for (value, def) in source_body.values.entries() {
        if let ValueDef::Operator(_, _, tys) = def {
            let types = &source_body.type_pool[*tys];
            if types.len() == 1 {
                value_plan.insert(value, classify_type(source, inventory, types[0])?);
            }
        }
    }
    Ok(value_plan)
}

fn lower_function(
    source: &Module<'_>,
    inventory: &CoreGcInventory,
    descriptors: &CoreGcDescriptorTable,
    runtime: &CoreGcRuntime,
    plan: &ModulePlan,
    out: &mut Module<'static>,
    source_func: Func,
    source_body: &FunctionBody,
    name: String,
) -> Result<(), CoreGcError> {
    let function_plan = &plan.functions[&source_func];
    let mut out_body = FunctionBody::new(out, function_plan.lowered_signature);

    let value_plan = classify_function_body(source, inventory, source_body)?;

    let is_fatref = |v: Value| value_plan.get(&v).is_some_and(|p| p.needs_root());
    let computed = liveness::compute(source_body, is_fatref, is_checkpoint_operator)?;
    let spilled: BTreeSet<Value> = computed
        .checkpoint_live
        .values()
        .chain(computed.tail_call_live.values())
        .flatten()
        .copied()
        .collect();
    let slot_of: BTreeMap<Value, u32> = spilled
        .iter()
        .enumerate()
        .map(|(index, &value)| (value, index as u32))
        .collect();
    verify_root_discipline(source_body, &computed, &slot_of, source_func)?;

    // Create every block (and its flattened params) up front so any
    // instruction later can resolve a branch-target/use regardless of
    // translation order; only *instruction* translation needs the
    // dominance-respecting order from `translation_order`.
    let mut block_map: HashMap<Block, Block> = HashMap::new();
    let mut value_map: HashMap<Value, Lowered> = HashMap::new();
    block_map.insert(source_body.entry, out_body.entry);
    {
        let mut cursor = 0usize;
        let entry_params = source_body.blocks[source_body.entry].params.clone();
        for (index, &(_, source_param)) in entry_params.iter().enumerate() {
            let lowered = match function_plan.param_plan[index] {
                LowerValue::Scalar(_) => {
                    let value = out_body.blocks[out_body.entry].params[cursor].1;
                    cursor += 1;
                    Lowered::Scalar(value)
                }
                LowerValue::FatRef { .. } | LowerValue::Dynamic => {
                    let addr = out_body.blocks[out_body.entry].params[cursor].1;
                    let type_id = out_body.blocks[out_body.entry].params[cursor + 1].1;
                    cursor += 2;
                    Lowered::Fat(addr, type_id)
                }
            };
            value_map.insert(source_param, lowered);
        }
    }
    let source_blocks: Vec<(Block, Vec<(Type, Value)>)> = source_body
        .blocks
        .entries()
        .map(|(block, def)| (block, def.params.clone()))
        .collect();
    for (block, params) in &source_blocks {
        if *block == source_body.entry {
            continue;
        }
        let out_block = out_body.add_block();
        block_map.insert(*block, out_block);
        for &(_, source_param) in params {
            let lowered = match value_plan[&source_param] {
                LowerValue::Scalar(ty) => Lowered::Scalar(out_body.add_blockparam(out_block, ty)),
                LowerValue::FatRef { .. } | LowerValue::Dynamic => {
                    let addr = out_body.add_blockparam(out_block, Type::I32);
                    let type_id = out_body.add_blockparam(out_block, Type::I32);
                    Lowered::Fat(addr, type_id)
                }
            };
            value_map.insert(source_param, lowered);
        }
    }

    let frame = if spilled.is_empty() {
        None
    } else {
        let slot_count_value = spilled.len() as u32;
        let entry_block = out_body.entry;
        let slot_count = i32_const(&mut out_body, entry_block, slot_count_value);
        let frame = out_body.add_op(
            out_body.entry,
            Operator::Call {
                function_index: runtime.roots.push,
            },
            &[slot_count],
            &[Type::I32],
        );
        Some(frame)
    };

    let mut lowering = FunctionLowering {
        source,
        inventory,
        descriptors,
        runtime,
        plan,
        source_body,
        value_plan,
        value_map,
        block_map,
        slot_of,
        frame,
    };

    for block in translation_order(source_body) {
        let out_block = lowering.block_map[&block];
        let mut emit = Emit {
            body: &mut out_body,
            current: out_block,
        };
        for &(_, source_param) in &source_body.blocks[block].params {
            if let Some(&slot) = lowering.slot_of.get(&source_param) {
                let (addr, type_id) = lowering.fat(source_param);
                lowering.root_store(&mut emit, slot, addr, type_id);
            }
        }
        for record in &source_body.blocks[block].insts {
            let value = record.value;
            let def = &source_body.values[value];
            match def {
                ValueDef::Alias(_) => {
                    // Transparent redirect; any later use resolves through
                    // `resolve_alias` before looking up `value_map`.
                }
                ValueDef::Operator(op, args_ref, tys_ref) => {
                    let args: Vec<Value> = source_body.arg_pool[*args_ref].to_vec();
                    let types = &source_body.type_pool[*tys_ref];
                    let result_ty = if types.len() == 1 { Some(types[0]) } else { None };
                    let lowered = lowering.lower_operator(&mut emit, op, &args, result_ty)?;
                    if let Some(lowered) = lowered {
                        lowering.value_map.insert(value, lowered);
                        lowering.maybe_spill(&mut emit, value, lowered);
                    }
                }
                other => {
                    return Err(CoreGcError {
                        message: format!(
                            "coregc lowering internal error: unexpected value definition {other:?} \
                             in function {} (should have been rejected by preflight)",
                            source_func.index()
                        ),
                    });
                }
            }
        }

        let current = emit.current;
        match &source_body.blocks[block].terminator.terminator {
            Terminator::Br { target } => {
                let resolved_target = lowering.lower_target(target);
                out_body.set_terminator(current, Terminator::Br { target: resolved_target });
            }
            Terminator::CondBr {
                cond,
                if_true,
                if_false,
            } => {
                let cond = lowering.scalar(*cond);
                let if_true = lowering.lower_target(if_true);
                let if_false = lowering.lower_target(if_false);
                out_body.set_terminator(
                    current,
                    Terminator::CondBr {
                        cond,
                        if_true,
                        if_false,
                    },
                );
            }
            Terminator::Return { values } => {
                let mut flat_values = Vec::new();
                for &value in values {
                    lowering.lowered(value).flatten_into(&mut flat_values);
                }
                if let Some(frame) = lowering.frame {
                    out_body.add_op(
                        current,
                        Operator::Call {
                            function_index: runtime.roots.pop,
                        },
                        &[frame],
                        &[],
                    );
                }
                out_body.set_terminator(
                    current,
                    Terminator::Return {
                        values: flat_values,
                    },
                );
            }
            Terminator::ReturnCall { func: callee_func, args } => {
                // Tail-call discipline (docs plan §13.3): checkpoint while
                // the caller's frame still roots the live arguments, then
                // pop it, then a normal call whose results are returned
                // immediately (no allocation can occur between pop and the
                // callee's own push_frame).
                let callee = &plan.functions[callee_func];
                lowering.checkpoint(&mut Emit {
                    body: &mut out_body,
                    current,
                });
                let mut flat_args = Vec::new();
                for &arg in args {
                    lowering.lowered(arg).flatten_into(&mut flat_args);
                }
                if let Some(frame) = lowering.frame {
                    out_body.add_op(
                        current,
                        Operator::Call {
                            function_index: runtime.roots.pop,
                        },
                        &[frame],
                        &[],
                    );
                }
                let flat_returns = flatten_plan(&callee.return_plan);
                let result = out_body.add_op(
                    current,
                    Operator::Call {
                        function_index: callee.lowered_func,
                    },
                    &flat_args,
                    &flat_returns,
                );
                let return_values = collect_flat_result(
                    &mut out_body,
                    current,
                    result,
                    &flat_returns,
                );
                out_body.set_terminator(current, Terminator::Return { values: return_values });
            }
            Terminator::ReturnCallRef { sig, args } => {
                let (params, returns) = function_signature(source, *sig)?;
                let param_plan = classify_all(source, inventory, params)?;
                let return_plan = classify_all(source, inventory, returns)?;
                let flat_returns = flatten_plan(&return_plan);
                let lowered_signature = plan.call_site_signatures[sig];
                let call_args = &args[..args.len() - 1];
                let selector = args[args.len() - 1];
                let _ = param_plan;
                lowering.checkpoint(&mut Emit {
                    body: &mut out_body,
                    current,
                });
                let mut flat_args = Vec::new();
                for &arg in call_args {
                    lowering.lowered(arg).flatten_into(&mut flat_args);
                }
                let selector_value = lowering.scalar(selector);
                flat_args.push(selector_value);
                if let Some(frame) = lowering.frame {
                    out_body.add_op(
                        current,
                        Operator::Call {
                            function_index: runtime.roots.pop,
                        },
                        &[frame],
                        &[],
                    );
                }
                let table = plan
                    .func_ref_table
                    .expect("preflight built a function-reference table for every RefFunc target");
                let result = out_body.add_op(
                    current,
                    Operator::CallIndirect {
                        sig_index: lowered_signature,
                        table_index: table,
                    },
                    &flat_args,
                    &flat_returns,
                );
                let return_values = collect_flat_result(
                    &mut out_body,
                    current,
                    result,
                    &flat_returns,
                );
                out_body.set_terminator(current, Terminator::Return { values: return_values });
            }
            Terminator::ReturnCallIndirect { sig, table, args } => {
                let (params, returns) = function_signature(source, *sig)?;
                let param_plan = classify_all(source, inventory, params)?;
                let return_plan = classify_all(source, inventory, returns)?;
                let flat_returns = flatten_plan(&return_plan);
                let lowered_signature = plan.call_site_signatures[sig];
                let call_args = &args[..args.len() - 1];
                let selector = args[args.len() - 1];
                let _ = param_plan;
                lowering.checkpoint(&mut Emit {
                    body: &mut out_body,
                    current,
                });
                let mut flat_args = Vec::new();
                for &arg in call_args {
                    lowering.lowered(arg).flatten_into(&mut flat_args);
                }
                let selector_value = lowering.scalar(selector);
                flat_args.push(selector_value);
                if let Some(frame) = lowering.frame {
                    out_body.add_op(
                        current,
                        Operator::Call {
                            function_index: runtime.roots.pop,
                        },
                        &[frame],
                        &[],
                    );
                }
                let lowered_table = plan.copied_tables[table];
                let result = out_body.add_op(
                    current,
                    Operator::CallIndirect {
                        sig_index: lowered_signature,
                        table_index: lowered_table,
                    },
                    &flat_args,
                    &flat_returns,
                );
                let return_values = collect_flat_result(
                    &mut out_body,
                    current,
                    result,
                    &flat_returns,
                );
                out_body.set_terminator(current, Terminator::Return { values: return_values });
            }
            Terminator::Unreachable => {
                out_body.set_terminator(current, Terminator::Unreachable);
            }
            other => {
                return Err(CoreGcError {
                    message: format!(
                        "coregc lowering internal error: unexpected terminator {other:?} in function {} \
                         (should have been rejected by preflight)",
                        source_func.index()
                    ),
                });
            }
        }
    }

    out_body.recompute_edges();
    out_body.validate().map_err(|error| CoreGcError {
        message: format!(
            "coregc lowering produced an invalid body for function {}: {error}",
            source_func.index()
        ),
    })?;
    out.funcs[function_plan.lowered_func] = FuncDecl::Body(function_plan.lowered_signature, name, out_body);
    Ok(())
}

impl<'a> FunctionLowering<'a> {
    fn lower_target(&self, target: &BlockTarget) -> BlockTarget {
        let mut args = Vec::new();
        for &value in &target.args {
            self.lowered(value).flatten_into(&mut args);
        }
        BlockTarget {
            block: self.block_map[&target.block],
            args,
        }
    }
}

/// Lower every accepted function in `source` into `out` (which must already
/// contain the runtime built by `coregc_runtime::build`), returning the
/// export list to publish.
pub(crate) fn lower(
    source: &Module<'_>,
    inventory: &CoreGcInventory,
    descriptors: &CoreGcDescriptorTable,
    runtime: &CoreGcRuntime,
    out: &mut Module<'static>,
) -> Result<LoweredModule, CoreGcError> {
    let plan = preflight(source, inventory, out)?;
    for (func, decl) in source.funcs.entries() {
        let FuncDecl::Body(_, name, body) = decl else {
            unreachable!("preflight already rejected imports");
        };
        lower_function(
            source,
            inventory,
            descriptors,
            runtime,
            &plan,
            out,
            func,
            body,
            format!("coregc_{name}"),
        )?;
    }
    let mut exports = Vec::new();
    for export in &source.exports {
        if let portal_pc_waffle::ExportKind::Func(func) = export.kind {
            let lowered = plan.functions.get(&func).ok_or_else(|| CoreGcError {
                message: format!(
                    "coregc lowering: export '{}' names an unsupported function",
                    export.name
                ),
            })?;
            exports.push((export.name.clone(), lowered.lowered_func));
        }
    }
    Ok(LoweredModule { exports })
}

#[cfg(test)]
pub(crate) mod tests_fixtures {
    use super::*;
    use crate::coregc::CoreGcInventory;
    use crate::coregc_runtime::{self, CoreGcOptions};
    use portal_pc_waffle::{
        Export, ExportKind, SignatureData, StorageType, WithMutablility, WithNullable,
    };
    use wasmtime::{Engine, Instance, Module as WasmtimeModule, Store};

    pub(crate) fn field(value: StorageType) -> WithMutablility<StorageType> {
        WithMutablility {
            mutable: true,
            value,
        }
    }

    /// `Node { value: i32, next: ref null Node }` plus a function `build(n)`
    /// that allocates an `n`-node list one node at a time (forcing a
    /// collection at every allocation, per the test's `collect_threshold_bytes
    /// = 0`) and then traverses it, summing `value` fields. Every previously
    /// allocated node must survive every later collection for this to return
    /// the correct sum, exercising retention across a loop-carried fat-ref
    /// block parameter through many collections, not just one.
    pub(crate) fn list_sum_source() -> (Module<'static>, Signature) {
        let mut module = Module::empty();
        let node = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        module.signatures[node] = SignatureData::Struct {
            fields: vec![
                field(StorageType::Val(Type::I32)),
                field(StorageType::Val(Type::Heap(WithNullable {
                    nullable: true,
                    value: HeapType::Sig { sig_index: node },
                }))),
            ],
            shared: false,
        };
        let node_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig { sig_index: node },
        });
        let signature = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(&module, signature);
        let entry = body.entry;
        let n = body.blocks[entry].params[0].1;

        let loop1 = body.add_block();
        let build_step = body.add_block();
        let loop2 = body.add_block();
        let traverse_step = body.add_block();
        let done = body.add_block();

        let null_addr = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let null_type = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let zero = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let null_ref = body.add_op(
            entry,
            Operator::RefNull { ty: node_ref },
            &[],
            &[node_ref],
        );
        let _ = (null_addr, null_type);
        body.set_terminator(
            entry,
            Terminator::Br {
                target: BlockTarget {
                    block: loop1,
                    args: vec![null_ref, zero],
                },
            },
        );

        let cur1 = body.add_blockparam(loop1, node_ref);
        let i = body.add_blockparam(loop1, Type::I32);
        let reached_n = body.add_op(loop1, Operator::I32GeU, &[i, n], &[Type::I32]);
        let sum_zero = body.add_op(loop1, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        body.set_terminator(
            loop1,
            Terminator::CondBr {
                cond: reached_n,
                if_true: BlockTarget {
                    block: loop2,
                    args: vec![cur1, sum_zero],
                },
                if_false: BlockTarget {
                    block: build_step,
                    args: vec![cur1, i],
                },
            },
        );

        let bs_cur = body.add_blockparam(build_step, node_ref);
        let bs_i = body.add_blockparam(build_step, Type::I32);
        let node_value = body.add_op(
            build_step,
            Operator::StructNew { sig: node },
            &[bs_i, bs_cur],
            &[node_ref],
        );
        let one = body.add_op(build_step, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let next_i = body.add_op(build_step, Operator::I32Add, &[bs_i, one], &[Type::I32]);
        body.set_terminator(
            build_step,
            Terminator::Br {
                target: BlockTarget {
                    block: loop1,
                    args: vec![node_value, next_i],
                },
            },
        );

        let cur2 = body.add_blockparam(loop2, node_ref);
        let sum = body.add_blockparam(loop2, Type::I32);
        let is_null = body.add_op(loop2, Operator::RefIsNull, &[cur2], &[Type::I32]);
        body.set_terminator(
            loop2,
            Terminator::CondBr {
                cond: is_null,
                if_true: BlockTarget {
                    block: done,
                    args: vec![sum],
                },
                if_false: BlockTarget {
                    block: traverse_step,
                    args: vec![cur2, sum],
                },
            },
        );

        let ts_cur = body.add_blockparam(traverse_step, node_ref);
        let ts_sum = body.add_blockparam(traverse_step, Type::I32);
        let value = body.add_op(
            traverse_step,
            Operator::StructGet { sig: node, idx: 0 },
            &[ts_cur],
            &[Type::I32],
        );
        let next = body.add_op(
            traverse_step,
            Operator::StructGet { sig: node, idx: 1 },
            &[ts_cur],
            &[node_ref],
        );
        let new_sum = body.add_op(traverse_step, Operator::I32Add, &[ts_sum, value], &[Type::I32]);
        body.set_terminator(
            traverse_step,
            Terminator::Br {
                target: BlockTarget {
                    block: loop2,
                    args: vec![next, new_sum],
                },
            },
        );

        let done_sum = body.add_blockparam(done, Type::I32);
        body.set_terminator(done, Terminator::Return { values: vec![done_sum] });

        body.recompute_edges();
        body.validate().expect("hand-built source body validates");
        let func = module
            .funcs
            .push(FuncDecl::Body(signature, "build".to_owned(), body));
        module.exports.push(Export {
            name: "build".to_owned(),
            kind: ExportKind::Func(func),
        });
        (module, signature)
    }

    /// `Node { value: i32, next: ref null Node }` plus `NodeArray = array of
    /// ref null Node`, and a function `build_array(n)` that: allocates a
    /// `NodeArray` of length `n` (`array.new_default`, exercising the
    /// zero-fill path), fills each slot with a freshly allocated `Node`
    /// (`array.set` storing a fat pair), then re-reads and sums every
    /// element (`array.get`/`array.len`/`struct.get`). The array itself and
    /// every already-stored node must survive the *other* nodes' allocating
    /// checkpoints.
    pub(crate) fn array_sum_source() -> Module<'static> {
        let mut module = Module::empty();
        let node = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        module.signatures[node] = SignatureData::Struct {
            fields: vec![
                field(StorageType::Val(Type::I32)),
                field(StorageType::Val(Type::Heap(WithNullable {
                    nullable: true,
                    value: HeapType::Sig { sig_index: node },
                }))),
            ],
            shared: false,
        };
        let node_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig { sig_index: node },
        });
        let array = module.signatures.push(SignatureData::Array {
            ty: field(StorageType::Val(node_ref)),
            shared: false,
        });
        let array_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig { sig_index: array },
        });
        let signature = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(&module, signature);
        let entry = body.entry;
        let n = body.blocks[entry].params[0].1;

        let fill_loop = body.add_block();
        let fill_step = body.add_block();
        let sum_loop = body.add_block();
        let sum_step = body.add_block();
        let done = body.add_block();

        let arr = body.add_op(
            entry,
            Operator::ArrayNewDefault { sig: array },
            &[n],
            &[array_ref],
        );
        let zero = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        body.set_terminator(
            entry,
            Terminator::Br {
                target: BlockTarget {
                    block: fill_loop,
                    args: vec![zero],
                },
            },
        );

        let fi = body.add_blockparam(fill_loop, Type::I32);
        let fill_done = body.add_op(fill_loop, Operator::I32GeU, &[fi, n], &[Type::I32]);
        body.set_terminator(
            fill_loop,
            Terminator::CondBr {
                cond: fill_done,
                if_true: BlockTarget {
                    block: sum_loop,
                    args: vec![zero, zero],
                },
                if_false: BlockTarget {
                    block: fill_step,
                    args: vec![fi],
                },
            },
        );

        let fs_i = body.add_blockparam(fill_step, Type::I32);
        let null_ref = body.add_op(fill_step, Operator::RefNull { ty: node_ref }, &[], &[node_ref]);
        let node_value = body.add_op(
            fill_step,
            Operator::StructNew { sig: node },
            &[fs_i, null_ref],
            &[node_ref],
        );
        body.add_op(
            fill_step,
            Operator::ArraySet { sig: array },
            &[arr, fs_i, node_value],
            &[],
        );
        let one = body.add_op(fill_step, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let fs_next = body.add_op(fill_step, Operator::I32Add, &[fs_i, one], &[Type::I32]);
        body.set_terminator(
            fill_step,
            Terminator::Br {
                target: BlockTarget {
                    block: fill_loop,
                    args: vec![fs_next],
                },
            },
        );

        let si = body.add_blockparam(sum_loop, Type::I32);
        let sum = body.add_blockparam(sum_loop, Type::I32);
        let length = body.add_op(sum_loop, Operator::ArrayLen, &[arr], &[Type::I32]);
        let sum_done = body.add_op(sum_loop, Operator::I32GeU, &[si, length], &[Type::I32]);
        body.set_terminator(
            sum_loop,
            Terminator::CondBr {
                cond: sum_done,
                if_true: BlockTarget {
                    block: done,
                    args: vec![sum],
                },
                if_false: BlockTarget {
                    block: sum_step,
                    args: vec![si, sum],
                },
            },
        );

        let ss_i = body.add_blockparam(sum_step, Type::I32);
        let ss_sum = body.add_blockparam(sum_step, Type::I32);
        let element = body.add_op(
            sum_step,
            Operator::ArrayGet { sig: array },
            &[arr, ss_i],
            &[node_ref],
        );
        let value = body.add_op(
            sum_step,
            Operator::StructGet { sig: node, idx: 0 },
            &[element],
            &[Type::I32],
        );
        let new_sum = body.add_op(sum_step, Operator::I32Add, &[ss_sum, value], &[Type::I32]);
        let one_s = body.add_op(sum_step, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let ss_next = body.add_op(sum_step, Operator::I32Add, &[ss_i, one_s], &[Type::I32]);
        body.set_terminator(
            sum_step,
            Terminator::Br {
                target: BlockTarget {
                    block: sum_loop,
                    args: vec![ss_next, new_sum],
                },
            },
        );

        let done_sum = body.add_blockparam(done, Type::I32);
        body.set_terminator(done, Terminator::Return { values: vec![done_sum] });

        body.recompute_edges();
        body.validate().expect("hand-built source body validates");
        let func = module
            .funcs
            .push(FuncDecl::Body(signature, "build_array".to_owned(), body));
        module.exports.push(Export {
            name: "build_array".to_owned(),
            kind: ExportKind::Func(func),
        });
        module
    }

    /// `Node { value: i32, next: ref null Node }`, a helper `make_node(v) ->
    /// ref null Node` (`struct.new(v, null)`), and `caller(a, b) -> i32`
    /// which allocates one node directly, **then** calls `make_node` (a real
    /// `Operator::Call`) to allocate a second, and finally reads both
    /// `.value` fields. The directly-allocated node is only used *after*
    /// the call, so it must survive the call's own checkpoint.
    pub(crate) fn direct_call_source() -> Module<'static> {
        let mut module = Module::empty();
        let node = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        module.signatures[node] = SignatureData::Struct {
            fields: vec![
                field(StorageType::Val(Type::I32)),
                field(StorageType::Val(Type::Heap(WithNullable {
                    nullable: true,
                    value: HeapType::Sig { sig_index: node },
                }))),
            ],
            shared: false,
        };
        let node_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig { sig_index: node },
        });

        let make_node_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![node_ref],
            shared: false,
        });
        let mut make_node_body = FunctionBody::new(&module, make_node_sig);
        let mn_entry = make_node_body.entry;
        let v = make_node_body.blocks[mn_entry].params[0].1;
        let null_ref = make_node_body.add_op(mn_entry, Operator::RefNull { ty: node_ref }, &[], &[node_ref]);
        let result = make_node_body.add_op(
            mn_entry,
            Operator::StructNew { sig: node },
            &[v, null_ref],
            &[node_ref],
        );
        make_node_body.set_terminator(mn_entry, Terminator::Return { values: vec![result] });
        make_node_body.recompute_edges();
        make_node_body.validate().expect("make_node body validates");
        let make_node = module
            .funcs
            .push(FuncDecl::Body(make_node_sig, "make_node".to_owned(), make_node_body));

        let caller_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32, Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut caller_body = FunctionBody::new(&module, caller_sig);
        let entry = caller_body.entry;
        let a = caller_body.blocks[entry].params[0].1;
        let b = caller_body.blocks[entry].params[1].1;
        let null_ref2 = caller_body.add_op(entry, Operator::RefNull { ty: node_ref }, &[], &[node_ref]);
        let n1 = caller_body.add_op(
            entry,
            Operator::StructNew { sig: node },
            &[a, null_ref2],
            &[node_ref],
        );
        let n2 = caller_body.add_op(
            entry,
            Operator::Call {
                function_index: make_node,
            },
            &[b],
            &[node_ref],
        );
        let v1 = caller_body.add_op(
            entry,
            Operator::StructGet { sig: node, idx: 0 },
            &[n1],
            &[Type::I32],
        );
        let v2 = caller_body.add_op(
            entry,
            Operator::StructGet { sig: node, idx: 0 },
            &[n2],
            &[Type::I32],
        );
        let sum = caller_body.add_op(entry, Operator::I32Add, &[v1, v2], &[Type::I32]);
        caller_body.set_terminator(entry, Terminator::Return { values: vec![sum] });
        caller_body.recompute_edges();
        caller_body.validate().expect("caller body validates");
        let caller = module
            .funcs
            .push(FuncDecl::Body(caller_sig, "caller".to_owned(), caller_body));
        module.exports.push(Export {
            name: "caller".to_owned(),
            kind: ExportKind::Func(caller),
        });
        module
    }

    /// A concrete-signature closure-table pattern: `add_one(x) -> x+1` and
    /// `double(x) -> x*2` are both taken by `ref.func` and invoked through
    /// `call_ref`. `apply(which, x)` picks one and calls through it; the
    /// `else` arm calls through a deliberate null function reference to
    /// prove that traps deterministically with no coregc-specific runtime
    /// support (core Wasm's own `call_indirect` semantics).
    pub(crate) fn call_ref_source() -> Module<'static> {
        let mut module = Module::empty();
        let unary_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let unary_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig { sig_index: unary_sig },
        });

        let mut add_one_body = FunctionBody::new(&module, unary_sig);
        let add_one_entry = add_one_body.entry;
        let x0 = add_one_body.blocks[add_one_entry].params[0].1;
        let one = add_one_body.add_op(add_one_entry, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let result0 = add_one_body.add_op(add_one_entry, Operator::I32Add, &[x0, one], &[Type::I32]);
        add_one_body.set_terminator(add_one_entry, Terminator::Return { values: vec![result0] });
        add_one_body.recompute_edges();
        add_one_body.validate().unwrap();
        let add_one = module
            .funcs
            .push(FuncDecl::Body(unary_sig, "add_one".to_owned(), add_one_body));

        let mut double_body = FunctionBody::new(&module, unary_sig);
        let double_entry = double_body.entry;
        let x1 = double_body.blocks[double_entry].params[0].1;
        let two = double_body.add_op(double_entry, Operator::I32Const { value: 2 }, &[], &[Type::I32]);
        let result1 = double_body.add_op(double_entry, Operator::I32Mul, &[x1, two], &[Type::I32]);
        double_body.set_terminator(double_entry, Terminator::Return { values: vec![result1] });
        double_body.recompute_edges();
        double_body.validate().unwrap();
        let double = module
            .funcs
            .push(FuncDecl::Body(unary_sig, "double".to_owned(), double_body));

        // apply(which, x): which==0 -> add_one(x); which==1 -> double(x); else -> call through null (traps)
        let apply_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32, Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(&module, apply_sig);
        let entry = body.entry;
        let which = body.blocks[entry].params[0].1;
        let x = body.blocks[entry].params[1].1;

        let zero_c = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let is_add_one = body.add_op(entry, Operator::I32Eq, &[which, zero_c], &[Type::I32]);
        let add_one_block = body.add_block();
        let check_double = body.add_block();
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: is_add_one,
                if_true: BlockTarget { block: add_one_block, args: vec![] },
                if_false: BlockTarget { block: check_double, args: vec![] },
            },
        );
        let add_one_ref = body.add_op(add_one_block, Operator::RefFunc { func_index: add_one }, &[], &[unary_ref]);
        let add_one_result = body.add_op(
            add_one_block,
            Operator::CallRef { sig_index: unary_sig },
            &[x, add_one_ref],
            &[Type::I32],
        );
        body.set_terminator(add_one_block, Terminator::Return { values: vec![add_one_result] });

        let one_c = body.add_op(check_double, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let is_double = body.add_op(check_double, Operator::I32Eq, &[which, one_c], &[Type::I32]);
        let double_block = body.add_block();
        let null_block = body.add_block();
        body.set_terminator(
            check_double,
            Terminator::CondBr {
                cond: is_double,
                if_true: BlockTarget { block: double_block, args: vec![] },
                if_false: BlockTarget { block: null_block, args: vec![] },
            },
        );
        let double_ref = body.add_op(double_block, Operator::RefFunc { func_index: double }, &[], &[unary_ref]);
        let double_result = body.add_op(
            double_block,
            Operator::CallRef { sig_index: unary_sig },
            &[x, double_ref],
            &[Type::I32],
        );
        body.set_terminator(double_block, Terminator::Return { values: vec![double_result] });

        let null_ref = body.add_op(null_block, Operator::RefNull { ty: unary_ref }, &[], &[unary_ref]);
        let null_result = body.add_op(
            null_block,
            Operator::CallRef { sig_index: unary_sig },
            &[x, null_ref],
            &[Type::I32],
        );
        body.set_terminator(null_block, Terminator::Return { values: vec![null_result] });

        body.recompute_edges();
        body.validate().expect("apply body validates");
        let apply = module
            .funcs
            .push(FuncDecl::Body(apply_sig, "apply".to_owned(), body));
        module.exports.push(Export {
            name: "apply".to_owned(),
            kind: ExportKind::Func(apply),
        });
        module
    }


    /// A fixture whose only unusual feature is a `Terminator::Select`
    /// (br_table) — still rejected with a named diagnostic.
    /// A dynamic-value scenario mirroring jsaw's repr: `Box { value:
    /// anyref }` holds either an i31 sentinel (jsaw's JS-null idiom) or a
    /// boxed `Number`, and exported functions exercise `RefI31`, `RefTest`
    /// (i31/concrete/nullable-any/non-null-any), `RefCast` (success and a
    /// trapping failure), and `RefEq` — all under forced collection so the
    /// `anyref` fields must keep their referents alive (descriptor tag 8
    /// scanning).
    pub(crate) fn dynamic_box_source() -> Module<'static> {
        let mut module = Module::empty();
        let anyref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Any,
        });
        let i31ref = Type::Heap(WithNullable {
            nullable: false,
            value: HeapType::I31,
        });
        let eqref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Eq,
        });
        let number = module.signatures.push(SignatureData::Struct {
            fields: vec![field(StorageType::Val(Type::I32))],
            shared: false,
        });
        let number_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig { sig_index: number },
        });
        let boxed = module.signatures.push(SignatureData::Struct {
            fields: vec![field(StorageType::Val(anyref))],
            shared: false,
        });
        let box_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig { sig_index: boxed },
        });

        let mut builders = DynamicBoxBuilders {
            number,
            number_ref,
            boxed,
            box_ref,
            anyref,
        };

        // is_null_sentinel(): Box(ref.i31(0)); ref.test (ref i31) -> 1
        builders.push_fn(&mut module, "is_null_sentinel", |body, entry, m| {
            let zero = m.c(body, entry, 0);
            let sentinel = body.add_op(entry, Operator::RefI31, &[zero], &[i31ref]);
            let b = body.add_op(entry, Operator::StructNew { sig: boxed }, &[sentinel], &[box_ref]);
            let v = body.add_op(entry, Operator::StructGet { sig: boxed, idx: 0 }, &[b], &[anyref]);
            let t = body.add_op(
                entry,
                Operator::RefTest {
                    ty: Type::Heap(WithNullable {
                        nullable: false,
                        value: HeapType::I31,
                    }),
                },
                &[v],
                &[Type::I32],
            );
            body.set_terminator(entry, Terminator::Return { values: vec![t] });
        });

        // unbox_number(): Box(Number(42)); get; test i31 (0); test $Number
        // (1); cast $Number; read .v -> 42 + t1*1000 + t2*100 = 142
        builders.push_fn(&mut module, "unbox_number", |body, entry, m| {
            let forty_two = m.c(body, entry, 42);
            let n = body.add_op(entry, Operator::StructNew { sig: number }, &[forty_two], &[number_ref]);
            let b = body.add_op(entry, Operator::StructNew { sig: boxed }, &[n], &[box_ref]);
            let v = body.add_op(entry, Operator::StructGet { sig: boxed, idx: 0 }, &[b], &[anyref]);
            let t1 = body.add_op(
                entry,
                Operator::RefTest {
                    ty: Type::Heap(WithNullable {
                        nullable: false,
                        value: HeapType::I31,
                    }),
                },
                &[v],
                &[Type::I32],
            );
            let t2 = body.add_op(
                entry,
                Operator::RefTest { ty: number_ref },
                &[v],
                &[Type::I32],
            );
            let casted = body.add_op(entry, Operator::RefCast { ty: number_ref }, &[v], &[number_ref]);
            let num_v = body.add_op(entry, Operator::StructGet { sig: number, idx: 0 }, &[casted], &[Type::I32]);
            let k1000 = m.c(body, entry, 1000);
            let t1_scaled = body.add_op(entry, Operator::I32Mul, &[t1, k1000], &[Type::I32]);
            let k100 = m.c(body, entry, 100);
            let t2_scaled = body.add_op(entry, Operator::I32Mul, &[t2, k100], &[Type::I32]);
            let s1 = body.add_op(entry, Operator::I32Add, &[num_v, t1_scaled], &[Type::I32]);
            let s2 = body.add_op(entry, Operator::I32Add, &[s1, t2_scaled], &[Type::I32]);
            body.set_terminator(entry, Terminator::Return { values: vec![s2] });
        });

        // bad_cast(): Box(ref.i31(7)); get; ref.cast $Number -> traps BAD_CAST
        builders.push_fn(&mut module, "bad_cast", |body, entry, m| {
            let seven = m.c(body, entry, 7);
            let sentinel = body.add_op(entry, Operator::RefI31, &[seven], &[i31ref]);
            let b = body.add_op(entry, Operator::StructNew { sig: boxed }, &[sentinel], &[box_ref]);
            let v = body.add_op(entry, Operator::StructGet { sig: boxed, idx: 0 }, &[b], &[anyref]);
            let casted = body.add_op(entry, Operator::RefCast { ty: number_ref }, &[v], &[number_ref]);
            let num_v = body.add_op(entry, Operator::StructGet { sig: number, idx: 0 }, &[casted], &[Type::I32]);
            body.set_terminator(entry, Terminator::Return { values: vec![num_v] });
        });

        // eq_checks(): 1 (same heap ref) + 1 (i31 same payload) + 0 (i31 vs
        // heap) + 1 (null vs null) + 0 (heap vs null) = 3
        builders.push_fn(&mut module, "eq_checks", |body, entry, m| {
            let five = m.c(body, entry, 5);
            let n = body.add_op(entry, Operator::StructNew { sig: number }, &[five], &[number_ref]);
            let same_heap = body.add_op(entry, Operator::RefEq, &[n, n], &[Type::I32]);
            let nine_a = m.c(body, entry, 9);
            let i31_a = body.add_op(entry, Operator::RefI31, &[nine_a], &[i31ref]);
            let nine_b = m.c(body, entry, 9);
            let i31_b = body.add_op(entry, Operator::RefI31, &[nine_b], &[i31ref]);
            let same_i31 = body.add_op(entry, Operator::RefEq, &[i31_a, i31_b], &[Type::I32]);
            let i31_vs_heap = body.add_op(entry, Operator::RefEq, &[i31_a, n], &[Type::I32]);
            let null_a = body.add_op(entry, Operator::RefNull { ty: eqref }, &[], &[eqref]);
            let null_b = body.add_op(entry, Operator::RefNull { ty: eqref }, &[], &[eqref]);
            let null_eq = body.add_op(entry, Operator::RefEq, &[null_a, null_b], &[Type::I32]);
            let heap_vs_null = body.add_op(entry, Operator::RefEq, &[n, null_a], &[Type::I32]);
            let s1 = body.add_op(entry, Operator::I32Add, &[same_heap, same_i31], &[Type::I32]);
            let s2 = body.add_op(entry, Operator::I32Add, &[s1, i31_vs_heap], &[Type::I32]);
            let s3 = body.add_op(entry, Operator::I32Add, &[s2, null_eq], &[Type::I32]);
            let s4 = body.add_op(entry, Operator::I32Add, &[s3, heap_vs_null], &[Type::I32]);
            body.set_terminator(entry, Terminator::Return { values: vec![s4] });
        });

        // null_tests(): test (ref null any) null (1) + 2*test (ref any)
        // null (0) + 4*test (ref any) Number (4) = 5
        builders.push_fn(&mut module, "null_tests", |body, entry, m| {
            let any_nullable = Type::Heap(WithNullable {
                nullable: true,
                value: HeapType::Any,
            });
            let any_non_null = Type::Heap(WithNullable {
                nullable: false,
                value: HeapType::Any,
            });
            let null_v = body.add_op(entry, Operator::RefNull { ty: anyref }, &[], &[anyref]);
            let t1 = body.add_op(entry, Operator::RefTest { ty: any_nullable }, &[null_v], &[Type::I32]);
            let t2 = body.add_op(entry, Operator::RefTest { ty: any_non_null }, &[null_v], &[Type::I32]);
            let eight = m.c(body, entry, 8);
            let n = body.add_op(entry, Operator::StructNew { sig: number }, &[eight], &[number_ref]);
            let t3 = body.add_op(entry, Operator::RefTest { ty: any_non_null }, &[n], &[Type::I32]);
            let two = m.c(body, entry, 2);
            let t2_scaled = body.add_op(entry, Operator::I32Mul, &[t2, two], &[Type::I32]);
            let four = m.c(body, entry, 4);
            let t3_scaled = body.add_op(entry, Operator::I32Mul, &[t3, four], &[Type::I32]);
            let s1 = body.add_op(entry, Operator::I32Add, &[t1, t2_scaled], &[Type::I32]);
            let s2 = body.add_op(entry, Operator::I32Add, &[s1, t3_scaled], &[Type::I32]);
            body.set_terminator(entry, Terminator::Return { values: vec![s2] });
        });

        module
    }

    struct DynamicBoxBuilders {
        number: Signature,
        number_ref: Type,
        boxed: Signature,
        box_ref: Type,
        anyref: Type,
    }

    impl DynamicBoxBuilders {
        fn c(&self, body: &mut FunctionBody, block: Block, value: u32) -> Value {
            body.add_op(block, Operator::I32Const { value }, &[], &[Type::I32])
        }

        fn push_fn(
            &mut self,
            module: &mut Module<'static>,
            name: &str,
            build: impl FnOnce(&mut FunctionBody, Block, &mut Self),
        ) {
            let signature = module.signatures.push(SignatureData::Func {
                params: vec![],
                returns: vec![Type::I32],
                shared: false,
            });
            let mut body = FunctionBody::new(module, signature);
            let entry = body.entry;
            build(&mut body, entry, self);
            body.recompute_edges();
            body.validate().expect("dynamic-box body validates");
            let func = module
                .funcs
                .push(FuncDecl::Body(signature, name.to_owned(), body));
            module.exports.push(Export {
                name: name.to_owned(),
                kind: ExportKind::Func(func),
            });
        }
    }

    pub(crate) fn select_terminator_source() -> Module<'static> {
        let mut module = Module::empty();
        let unary_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(&module, unary_sig);
        let entry = body.entry;
        let x = body.blocks[entry].params[0].1;
        let arm = body.add_block();
        body.set_terminator(
            entry,
            Terminator::Select {
                value: x,
                targets: vec![BlockTarget {
                    block: arm,
                    args: vec![],
                }],
                default: BlockTarget {
                    block: arm,
                    args: vec![],
                },
            },
        );
        body.set_terminator(arm, Terminator::Return { values: vec![x] });
        body.recompute_edges();
        body.validate().unwrap();
        let f = module
            .funcs
            .push(FuncDecl::Body(unary_sig, "branchy".to_owned(), body));
        module.exports.push(Export {
            name: "branchy".to_owned(),
            kind: ExportKind::Func(f),
        });
        module
    }

    /// An `i8` array scenario exercising `ArrayNewFixed`, `ArrayGetU`,
    /// `ArrayFill`, and `ArrayCopy` (including an overlapping same-array
    /// copy), plus a second i32-element array to prove element widths do not
    /// cross-contaminate. `byte_ops()` returns the combined sum.
    pub(crate) fn byte_array_source() -> Module<'static> {
        let mut module = Module::empty();
        let i8_array = module.signatures.push(SignatureData::Array {
            ty: field(StorageType::I8),
            shared: false,
        });
        let i8_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig {
                sig_index: i8_array,
            },
        });
        let signature = module.signatures.push(SignatureData::Func {
            params: vec![],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(&module, signature);
        let entry = body.entry;
        let c = |body: &mut FunctionBody, value: u32| {
            body.add_op(entry, Operator::I32Const { value }, &[], &[Type::I32])
        };
        let e0 = c(&mut body, 10);
        let e1 = c(&mut body, 20);
        let e2 = c(&mut body, 30);
        let arr1 = body.add_op(
            entry,
            Operator::ArrayNewFixed {
                sig: i8_array,
                num: 3,
            },
            &[e0, e1, e2],
            &[i8_ref],
        );
        let len3 = c(&mut body, 3);
        let arr2 = body.add_op(
            entry,
            Operator::ArrayNewDefault { sig: i8_array },
            &[len3],
            &[i8_ref],
        );
        let zero = c(&mut body, 0);
        let forty = c(&mut body, 40);
        let two = c(&mut body, 2);
        body.add_op(
            entry,
            Operator::ArrayFill { sig: i8_array },
            &[arr2, zero, forty, two],
            &[],
        );
        let one = c(&mut body, 1);
        // arr2 = [40, 40, 0]; copy arr1[0..2] into arr2[1..3] -> [40, 10, 20]
        body.add_op(
            entry,
            Operator::ArrayCopy {
                dest: i8_array,
                src: i8_array,
            },
            &[arr2, one, arr1, zero, two],
            &[],
        );
        // Overlapping same-array copy within arr1 (dst > src):
        // arr1 = [10, 20, 30] -> copy arr1[0..2] into arr1[1..3] -> [10, 10, 20]
        body.add_op(
            entry,
            Operator::ArrayCopy {
                dest: i8_array,
                src: i8_array,
            },
            &[arr1, one, arr1, zero, two],
            &[],
        );
        let reads2: Vec<Value> = (0..3)
            .map(|i| {
                let index = c(&mut body, i);
                body.add_op(
                    entry,
                    Operator::ArrayGetU { sig: i8_array },
                    &[arr2, index],
                    &[Type::I32],
                )
            })
            .collect();
        let s2a = body.add_op(entry, Operator::I32Add, &[reads2[0], reads2[1]], &[Type::I32]);
        let sum2 = body.add_op(entry, Operator::I32Add, &[s2a, reads2[2]], &[Type::I32]);
        let reads1: Vec<Value> = (0..3)
            .map(|i| {
                let index = c(&mut body, i);
                body.add_op(
                    entry,
                    Operator::ArrayGetU { sig: i8_array },
                    &[arr1, index],
                    &[Type::I32],
                )
            })
            .collect();
        let s1a = body.add_op(entry, Operator::I32Add, &[reads1[0], reads1[1]], &[Type::I32]);
        let sum1 = body.add_op(entry, Operator::I32Add, &[s1a, reads1[2]], &[Type::I32]);
        let total = body.add_op(entry, Operator::I32Add, &[sum2, sum1], &[Type::I32]);
        body.set_terminator(entry, Terminator::Return { values: vec![total] });
        body.recompute_edges();
        body.validate().expect("byte_ops body validates");
        let func = module
            .funcs
            .push(FuncDecl::Body(signature, "byte_ops".to_owned(), body));
        module.exports.push(Export {
            name: "byte_ops".to_owned(),
            kind: ExportKind::Func(func),
        });
        module
    }

    /// A `count(n, cur) -> i32` function that recurses *to itself* with a
    /// tail call: `if n == 0 { cur.value } else { count(n-1, struct.new(n,
    /// cur)) }`. The freshly allocated node and the incoming `cur` must both
    /// survive the tail call's checkpoint under forced collection, and the
    /// final `.value` read returns the very first `n`.
    pub(crate) fn tail_call_count_source() -> Module<'static> {
        let mut module = Module::empty();
        let node = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        module.signatures[node] = SignatureData::Struct {
            fields: vec![
                field(StorageType::Val(Type::I32)),
                field(StorageType::Val(Type::Heap(WithNullable {
                    nullable: true,
                    value: HeapType::Sig { sig_index: node },
                }))),
            ],
            shared: false,
        };
        let node_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig { sig_index: node },
        });
        let count_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32, node_ref],
            returns: vec![Type::I32],
            shared: false,
        });

        let mut body = FunctionBody::new(&module, count_sig);
        let entry = body.entry;
        let n = body.blocks[entry].params[0].1;
        let cur = body.blocks[entry].params[1].1;
        let zero = body.add_op(entry, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let done = body.add_op(entry, Operator::I32Eq, &[n, zero], &[Type::I32]);
        let base = body.add_block();
        let step = body.add_block();
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: done,
                if_true: BlockTarget {
                    block: base,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: step,
                    args: vec![],
                },
            },
        );
        let value = body.add_op(
            base,
            Operator::StructGet { sig: node, idx: 0 },
            &[cur],
            &[Type::I32],
        );
        body.set_terminator(base, Terminator::Return { values: vec![value] });
        let next_node = body.add_op(
            step,
            Operator::StructNew { sig: node },
            &[n, cur],
            &[node_ref],
        );
        let one = body.add_op(step, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let next_n = body.add_op(step, Operator::I32Sub, &[n, one], &[Type::I32]);
        let placeholder = module.funcs.push(FuncDecl::None(std::marker::PhantomData));
        body.set_terminator(
            step,
            Terminator::ReturnCall {
                func: placeholder,
                args: vec![next_n, next_node],
            },
        );
        body.recompute_edges();
        body.validate().expect("count body validates");
        module.funcs[placeholder] = FuncDecl::Body(count_sig, "count".to_owned(), body);

        // start(n): seed the recursion with a leaf node holding value 999
        // (never observed unless the chain is broken).
        let start_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut start_body = FunctionBody::new(&module, start_sig);
        let s_entry = start_body.entry;
        let s_n = start_body.blocks[s_entry].params[0].1;
        let sentinel = start_body.add_op(s_entry, Operator::I32Const { value: 999 }, &[], &[Type::I32]);
        let null_ref = start_body.add_op(s_entry, Operator::RefNull { ty: node_ref }, &[], &[node_ref]);
        let leaf = start_body.add_op(
            s_entry,
            Operator::StructNew { sig: node },
            &[sentinel, null_ref],
            &[node_ref],
        );
        let result = start_body.add_op(
            s_entry,
            Operator::Call {
                function_index: placeholder,
            },
            &[s_n, leaf],
            &[Type::I32],
        );
        start_body.set_terminator(s_entry, Terminator::Return { values: vec![result] });
        start_body.recompute_edges();
        start_body.validate().expect("start body validates");
        let start = module
            .funcs
            .push(FuncDecl::Body(start_sig, "start".to_owned(), start_body));
        module.exports.push(Export {
            name: "start".to_owned(),
            kind: ExportKind::Func(start),
        });
        module
    }

    /// Pick one of two freshly allocated nodes with `TypedSelect` over the
    /// reference type, then read the chosen `.value`. Both candidates must
    /// survive the second allocation's checkpoint.
    pub(crate) fn select_ref_source() -> Module<'static> {
        let mut module = Module::empty();
        let node = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        module.signatures[node] = SignatureData::Struct {
            fields: vec![
                field(StorageType::Val(Type::I32)),
                field(StorageType::Val(Type::Heap(WithNullable {
                    nullable: true,
                    value: HeapType::Sig { sig_index: node },
                }))),
            ],
            shared: false,
        };
        let node_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig { sig_index: node },
        });
        let signature = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(&module, signature);
        let entry = body.entry;
        let cond = body.blocks[entry].params[0].1;
        let null_ref = body.add_op(entry, Operator::RefNull { ty: node_ref }, &[], &[node_ref]);
        let forty = body.add_op(entry, Operator::I32Const { value: 40 }, &[], &[Type::I32]);
        let a = body.add_op(entry, Operator::StructNew { sig: node }, &[forty, null_ref], &[node_ref]);
        let null_ref2 = body.add_op(entry, Operator::RefNull { ty: node_ref }, &[], &[node_ref]);
        let two = body.add_op(entry, Operator::I32Const { value: 2 }, &[], &[Type::I32]);
        let b = body.add_op(entry, Operator::StructNew { sig: node }, &[two, null_ref2], &[node_ref]);
        let chosen = body.add_op(
            entry,
            Operator::TypedSelect { ty: node_ref },
            &[a, b, cond],
            &[node_ref],
        );
        let value = body.add_op(
            entry,
            Operator::StructGet { sig: node, idx: 0 },
            &[chosen],
            &[Type::I32],
        );
        body.set_terminator(entry, Terminator::Return { values: vec![value] });
        body.recompute_edges();
        body.validate().expect("select body validates");
        let func = module
            .funcs
            .push(FuncDecl::Body(signature, "pick".to_owned(), body));
        module.exports.push(Export {
            name: "pick".to_owned(),
            kind: ExportKind::Func(func),
        });
        module
    }

    /// A fixture exercising `ReturnCallRef` through a deliberately-null
    /// function reference: accepted since stage 2 (tail-call support), and
    /// traps at runtime on the null call.
    pub(crate) fn tail_call_source() -> Module<'static> {
        let mut module = Module::empty();
        let unary_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let unary_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig {
                sig_index: unary_sig,
            },
        });
        let mut callee_body = FunctionBody::new(&module, unary_sig);
        let callee_entry = callee_body.entry;
        let x = callee_body.blocks[callee_entry].params[0].1;
        callee_body.set_terminator(callee_entry, Terminator::Return { values: vec![x] });
        callee_body.recompute_edges();
        callee_body.validate().unwrap();
        module
            .funcs
            .push(FuncDecl::Body(unary_sig, "callee".to_owned(), callee_body));

        let mut body = FunctionBody::new(&module, unary_sig);
        let entry = body.entry;
        let x = body.blocks[entry].params[0].1;
        let null_ref = body.add_op(entry, Operator::RefNull { ty: unary_ref }, &[], &[unary_ref]);
        body.set_terminator(
            entry,
            Terminator::ReturnCallRef {
                sig: unary_sig,
                args: vec![x, null_ref],
            },
        );
        body.recompute_edges();
        body.validate().unwrap();
        let f = module
            .funcs
            .push(FuncDecl::Body(unary_sig, "tail_caller".to_owned(), body));
        module.exports.push(Export {
            name: "tail_caller".to_owned(),
            kind: ExportKind::Func(f),
        });
        module
    }

    pub(crate) fn lower_and_instantiate(source: &Module<'_>, options: CoreGcOptions) -> (Store<()>, Instance) {
        let inventory = CoreGcInventory::build(source).expect("inventory");
        let descriptors = CoreGcDescriptorTable::build(&inventory).expect("descriptors");
        let mut out = Module::empty();
        let runtime = coregc_runtime::build(&mut out, &options, &descriptors).expect("runtime");
        let lowered = lower(source, &inventory, &descriptors, &runtime, &mut out).expect("lowering");
        for (name, func) in &lowered.exports {
            out.exports.push(Export {
                name: name.clone(),
                kind: ExportKind::Func(*func),
            });
        }
        out.exports.push(Export {
            name: "memory".to_owned(),
            kind: ExportKind::Memory(runtime.memory),
        });
        out.exports.push(Export {
            name: "collect".to_owned(),
            kind: ExportKind::Func(runtime.collect),
        });
        for (_, decl) in out.funcs.entries() {
            if let FuncDecl::Body(_, name, body) = decl {
                body.validate()
                    .unwrap_or_else(|error| panic!("lowered function '{name}' is invalid: {error}"));
            }
        }
        let bytes = portal_pc_waffle::to_wasm_bytes(&out).expect("core wasm encodes");
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("lowered artifact validates with default core features");
        let engine = Engine::default();
        let module = WasmtimeModule::new(&engine, bytes).expect("engine compiles lowered module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("lowered module instantiates");
        (store, instance)
    }

}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::coregc::CoreGcInventory;
    use crate::coregc_runtime::{self, CoreGcOptions};
    use portal_pc_waffle::{
        Export, ExportKind, SignatureData, StorageType, WithMutablility, WithNullable,
    };
    use wasmtime::{Engine, Instance, Module as WasmtimeModule, Store};

    use super::tests_fixtures::*;
    #[test]
    fn list_built_and_summed_survives_a_forced_collection_at_every_allocation() {
        let (source, _) = list_sum_source();
        let (mut store, instance) = lower_and_instantiate(
            &source,
            CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        );
        let build = instance
            .get_typed_func::<i32, i32>(&mut store, "build")
            .expect("build export");
        // sum(0..n) with a forced collection at every single-node allocation
        // and every traversal read: nothing before the current node in the
        // loop-carried `cur1`/`cur2` chain may be reclaimed early.
        assert_eq!(build.call(&mut store, 20).expect("build(20)"), (0..20).sum::<i32>());
    }

    #[test]
    fn root_verifier_rejects_a_checkpoint_value_without_a_slot() {
        // Simulates a broken lowering that failed to assign a shadow-frame
        // slot to a value the collector needs: the verifier must catch the
        // omission, proving it is not a no-op.
        let (source, _) = list_sum_source();
        let FuncDecl::Body(_, _, body) = &source.funcs.values().next().unwrap() else {
            unreachable!()
        };
        let inventory = CoreGcInventory::build(&source).expect("inventory");
        let value_plan = classify_function_body(&source, &inventory, body).expect("classification");
        let is_fatref = |v: Value| value_plan.get(&v).is_some_and(|p| p.needs_root());
        let mut liveness = super::liveness::compute(body, is_fatref, is_checkpoint_operator)
            .expect("liveness");
        let slot_of: BTreeMap<Value, u32> = BTreeMap::new();
        assert!(verify_root_discipline(body, &liveness, &slot_of, Func::new(0)).is_err());
        // Sanity check: with honest slot assignment, verification passes.
        let spilled: BTreeSet<Value> = liveness
            .checkpoint_live
            .values()
            .flatten()
            .copied()
            .collect();
        let honest: BTreeMap<Value, u32> = spilled
            .iter()
            .enumerate()
            .map(|(index, &value)| (value, index as u32))
            .collect();
        verify_root_discipline(body, &liveness, &honest, Func::new(0)).expect("honest passes");
        // And a doctored liveness map with a bogus extra requirement fails.
        let fake = Value::new(u32::MAX as usize - 1);
        let first_checkpoint = *liveness.checkpoint_live.keys().next().unwrap();
        liveness
            .checkpoint_live
            .entry(first_checkpoint)
            .or_default()
            .push(fake);
        assert!(verify_root_discipline(body, &liveness, &honest, Func::new(0)).is_err());
    }

    #[test]
    fn dynamic_values_support_i31_tests_casts_and_equality_under_forced_collection() {
        let source = dynamic_box_source();
        let (mut store, instance) = lower_and_instantiate(
            &source,
            CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        );
        let is_null_sentinel = instance
            .get_typed_func::<(), i32>(&mut store, "is_null_sentinel")
            .expect("is_null_sentinel export");
        assert_eq!(is_null_sentinel.call(&mut store, ()).expect("is_null_sentinel"), 1);
        let unbox_number = instance
            .get_typed_func::<(), i32>(&mut store, "unbox_number")
            .expect("unbox_number export");
        assert_eq!(unbox_number.call(&mut store, ()).expect("unbox_number"), 142);
        let bad_cast = instance
            .get_typed_func::<(), i32>(&mut store, "bad_cast")
            .expect("bad_cast export");
        assert!(bad_cast.call(&mut store, ()).is_err(), "BAD_CAST must trap");
        let eq_checks = instance
            .get_typed_func::<(), i32>(&mut store, "eq_checks")
            .expect("eq_checks export");
        assert_eq!(eq_checks.call(&mut store, ()).expect("eq_checks"), 3);
        let null_tests = instance
            .get_typed_func::<(), i32>(&mut store, "null_tests")
            .expect("null_tests export");
        assert_eq!(null_tests.call(&mut store, ()).expect("null_tests"), 5);
    }

    #[test]
    fn packed_byte_arrays_support_fixed_fill_copy_and_unsigned_reads() {
        let source = byte_array_source();
        let (mut store, instance) = lower_and_instantiate(
            &source,
            CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        );
        let byte_ops = instance
            .get_typed_func::<(), i32>(&mut store, "byte_ops")
            .expect("byte_ops export");
        // arr2 = [40, 10, 20] -> 70; arr1 = [10, 10, 20] -> 40.
        assert_eq!(byte_ops.call(&mut store, ()).expect("byte_ops()"), 110);
    }

    #[test]
    fn tail_call_recursion_survives_a_forced_collection_at_every_allocation() {
        let source = tail_call_count_source();
        let (mut store, instance) = lower_and_instantiate(
            &source,
            CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        );
        let start = instance
            .get_typed_func::<i32, i32>(&mut store, "start")
            .expect("start export");
        // count(3, leaf) -> count(2, node(3)) -> count(1, node(2)) ->
        // count(0, node(1)) -> node(1).value = 1
        assert_eq!(start.call(&mut store, 3).expect("start(3)"), 1);
        assert_eq!(start.call(&mut store, 7).expect("start(7)"), 1);
    }

    #[test]
    fn typed_select_over_fat_references_survives_forced_collection() {
        let source = select_ref_source();
        let (mut store, instance) = lower_and_instantiate(
            &source,
            CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        );
        let pick = instance
            .get_typed_func::<i32, i32>(&mut store, "pick")
            .expect("pick export");
        assert_eq!(pick.call(&mut store, 1).expect("pick(1)"), 40);
        assert_eq!(pick.call(&mut store, 0).expect("pick(0)"), 2);
    }

    #[test]
    fn array_built_and_summed_survives_a_forced_collection_at_every_allocation() {
        let source = array_sum_source();
        let (mut store, instance) = lower_and_instantiate(
            &source,
            CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        );
        let build_array = instance
            .get_typed_func::<i32, i32>(&mut store, "build_array")
            .expect("build_array export");
        assert_eq!(
            build_array.call(&mut store, 20).expect("build_array(20)"),
            (0..20).sum::<i32>()
        );
    }

    #[test]
    fn a_directly_allocated_value_survives_a_call_that_also_allocates() {
        let source = direct_call_source();
        let (mut store, instance) = lower_and_instantiate(
            &source,
            CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        );
        let caller = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "caller")
            .expect("caller export");
        assert_eq!(caller.call(&mut store, (10, 32)).expect("caller(10, 32)"), 42);
    }

    #[test]
    fn debug_minimal_call_ref() {
        let mut module = Module::empty();
        let unary_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let unary_ref = Type::Heap(WithNullable {
            nullable: true,
            value: HeapType::Sig { sig_index: unary_sig },
        });
        let mut add_one_body = FunctionBody::new(&module, unary_sig);
        let e = add_one_body.entry;
        let x0 = add_one_body.blocks[e].params[0].1;
        let one = add_one_body.add_op(e, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let r0 = add_one_body.add_op(e, Operator::I32Add, &[x0, one], &[Type::I32]);
        add_one_body.set_terminator(e, Terminator::Return { values: vec![r0] });
        add_one_body.recompute_edges();
        let add_one = module.funcs.push(FuncDecl::Body(unary_sig, "add_one".into(), add_one_body));

        let apply_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(&module, apply_sig);
        let entry = body.entry;
        let x = body.blocks[entry].params[0].1;
        let r = body.add_op(entry, Operator::RefFunc { func_index: add_one }, &[], &[unary_ref]);
        let result = body.add_op(entry, Operator::CallRef { sig_index: unary_sig }, &[x, r], &[Type::I32]);
        body.set_terminator(entry, Terminator::Return { values: vec![result] });
        body.recompute_edges();
        let apply = module.funcs.push(FuncDecl::Body(apply_sig, "apply".into(), body));
        module.exports.push(Export { name: "apply".into(), kind: ExportKind::Func(apply) });

        let (mut store, instance) = lower_and_instantiate(&module, CoreGcOptions::default());
        let apply = instance.get_typed_func::<i32, i32>(&mut store, "apply").unwrap();
        assert_eq!(apply.call(&mut store, 41).unwrap(), 42);
    }

    #[test]
    fn ref_func_and_call_ref_dispatch_over_concrete_signatures() {
        let source = call_ref_source();
        let (mut store, instance) = lower_and_instantiate(&source, CoreGcOptions::default());
        let apply = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "apply")
            .expect("apply export");
        assert_eq!(apply.call(&mut store, (0, 41)).expect("add_one via call_ref"), 42);
        assert_eq!(apply.call(&mut store, (1, 21)).expect("double via call_ref"), 42);
        assert!(
            apply.call(&mut store, (2, 0)).is_err(),
            "calling through a null function reference must trap, not silently succeed"
        );
    }

    #[test]
    fn list_built_and_summed_matches_with_a_normal_collection_threshold() {
        let (source, _) = list_sum_source();
        let (mut store, instance) = lower_and_instantiate(&source, CoreGcOptions::default());
        let build = instance
            .get_typed_func::<i32, i32>(&mut store, "build")
            .expect("build export");
        assert_eq!(build.call(&mut store, 20).expect("build(20)"), (0..20).sum::<i32>());
    }

    #[test]
    fn unreachable_nodes_are_reclaimed_after_the_list_is_dropped() {
        let (source, _) = list_sum_source();
        let (mut store, instance) = lower_and_instantiate(
            &source,
            CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        );
        let build = instance
            .get_typed_func::<i32, i32>(&mut store, "build")
            .expect("build export");
        build.call(&mut store, 5).expect("first build");
        // The function already popped its shadow frame on return, so the
        // entire list from the first call is unreachable garbage now. A
        // second call must still work correctly (proving the first call's
        // garbage doesn't corrupt anything the allocator relies on) and
        // an explicit forced collection afterward must not trap.
        assert_eq!(build.call(&mut store, 7).expect("second build"), (0..7).sum::<i32>());
        let collect = instance
            .get_typed_func::<(), ()>(&mut store, "collect")
            .expect("collect export");
        collect.call(&mut store, ()).expect("forced collection after both calls");
    }
}


#[cfg(test)]
mod call_indirect_regressions {
    use portal_pc_waffle::{
        EntityRef, Export, ExportKind, FuncDecl, FunctionBody, HeapType, Module, Operator,
        SignatureData, TableData, Terminator, Type, WithNullable,
    };

    /// Regression guard for the call-site-signature ABI: `call_indirect`'s
    /// signature is the *callee's* signature; the selector is a separate
    /// operand popped by the instruction itself, not a signature parameter.
    /// coregc once interned the signature with a spurious trailing i32,
    /// which made the validator underflow ("expected i32 but nothing on
    /// stack") at every indirect call.
    #[test]
    fn call_indirect_with_operands_validates() {
        let mut module = Module::empty();
        let unary_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut add_one_body = FunctionBody::new(&module, unary_sig);
        let entry = add_one_body.entry;
        let x = add_one_body.blocks[entry].params[0].1;
        let one = add_one_body.add_op(entry, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let sum = add_one_body.add_op(entry, Operator::I32Add, &[x, one], &[Type::I32]);
        add_one_body.set_terminator(entry, Terminator::Return { values: vec![sum] });
        add_one_body.recompute_edges();
        let add_one = module
            .funcs
            .push(FuncDecl::Body(unary_sig, "add_one".to_owned(), add_one_body));

        let table = module.tables.push(TableData {
            ty: Type::Heap(WithNullable {
                value: HeapType::FuncRef,
                nullable: true,
            }),
            initial: 2,
            max: Some(2),
            func_elements: Some(vec![EntityRef::invalid(), add_one]),
            table64: false,
        });

        let mut apply_body = FunctionBody::new(&module, unary_sig);
        let entry = apply_body.entry;
        let x = apply_body.blocks[entry].params[0].1;
        let slot = apply_body.add_op(entry, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let result = apply_body.add_op(
            entry,
            Operator::CallIndirect {
                sig_index: unary_sig,
                table_index: table,
            },
            &[x, slot],
            &[Type::I32],
        );
        apply_body.set_terminator(entry, Terminator::Return { values: vec![result] });
        apply_body.recompute_edges();
        let apply = module
            .funcs
            .push(FuncDecl::Body(unary_sig, "apply".to_owned(), apply_body));
        module.exports.push(Export {
            name: "apply".to_owned(),
            kind: ExportKind::Func(apply),
        });

        let bytes = portal_pc_waffle::to_wasm_bytes(&module).expect("compiles");
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("validates with default core features");

        // Both operands (the argument local and the selector constant) must
        // actually be emitted before `call_indirect`.
        let mut apply_ops = None;
        for payload in wasmparser::Parser::new(0).parse_all(&bytes) {
            if let Ok(wasmparser::Payload::CodeSectionEntry(body)) = payload {
                let ops = body
                    .get_operators_reader()
                    .expect("operators")
                    .into_iter()
                    .collect::<Result<Vec<_>, _>>()
                    .expect("decode");
                if ops
                    .iter()
                    .any(|op| matches!(op, wasmparser::Operator::CallIndirect { .. }))
                {
                    apply_ops = Some(ops);
                }
            }
        }
        let apply_ops = apply_ops.expect("a function with call_indirect exists");
        let call_pos = apply_ops
            .iter()
            .position(|op| matches!(op, wasmparser::Operator::CallIndirect { .. }))
            .unwrap();
        let has_arg = apply_ops[..call_pos]
            .iter()
            .any(|op| matches!(op, wasmparser::Operator::LocalGet { local_index: 0 }));
        let has_selector = apply_ops[..call_pos]
            .iter()
            .any(|op| matches!(op, wasmparser::Operator::I32Const { value: 1 }));
        assert!(
            has_arg && has_selector,
            "call_indirect operands must be emitted: {apply_ops:?}"
        );
    }
}
