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
    Block, BlockTarget, EntityRef, Func, FuncDecl, FunctionBody, HeapType, Module, Operator,
    Signature, SignatureData, Table, TableData, Terminator, Type, Value, ValueDef, WithNullable,
};

use crate::{
    coregc::{CoreGcError, CoreGcInventory, CoreGcStorage},
    coregc_layout::CoreGcDescriptorTable,
    coregc_runtime::CoreGcRuntime,
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
}

impl LowerValue {
    fn flat_len(self) -> usize {
        match self {
            LowerValue::Scalar(_) => 1,
            LowerValue::FatRef { .. } => 2,
        }
    }

    fn flat_types(self) -> Vec<Type> {
        match self {
            LowerValue::Scalar(ty) => vec![ty],
            LowerValue::FatRef { .. } => vec![Type::I32, Type::I32],
        }
    }

    fn is_fatref(self) -> bool {
        matches!(self, LowerValue::FatRef { .. })
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
            | Operator::ArrayNewDefault { .. }
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

        Ok(Liveness { checkpoint_live })
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
                if !call_site_signatures.contains_key(sig_index) {
                    let (params, returns) = function_signature(source, *sig_index)?;
                    // The signature used by `CallIndirect` is the callee's
                    // own flattened signature; the selector is a separate
                    // operand popped by `call_indirect` itself, NOT a
                    // parameter in this signature (getting this wrong makes
                    // the validator pop one more i32 than the call site
                    // provides).
                    let flat_params = flatten_plan(&classify_all(source, inventory, params)?);
                    let flat_returns = flatten_plan(&classify_all(source, inventory, returns)?);
                    let lowered_signature = out.signatures.push(SignatureData::Func {
                        params: flat_params,
                        returns: flat_returns,
                        shared: false,
                    });
                    call_site_signatures.insert(*sig_index, lowered_signature);
                }
            }
            if let ValueDef::Operator(Operator::CallIndirect { table_index, .. }, _, _) = def {
                if !copied_tables.contains_key(table_index) {
                    let source_table = &source.tables[*table_index];
                    if !matches!(source_table.ty, Type::Heap(WithNullable { value: HeapType::FuncRef, .. })) {
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
                    copied_tables.insert(*table_index, lowered_table);
                }
            }
        }
    }

    let func_ref_table = if func_ref_slots.is_empty() {
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
        | Operator::ArrayNewDefault { .. }
        | Operator::ArrayGet { .. }
        | Operator::ArraySet { .. }
        | Operator::ArrayLen
        | Operator::RefNull { .. }
        | Operator::RefIsNull
        | Operator::RefFunc { .. }
        | Operator::I32Const { .. }
        | Operator::I64Const { .. }
        | Operator::F32Const { .. }
        | Operator::F64Const { .. }
        | Operator::I32Add
        | Operator::I32Sub
        | Operator::I32Mul
        | Operator::I32And
        | Operator::I32Or
        | Operator::I32Xor
        | Operator::I32Shl
        | Operator::I32ShrS
        | Operator::I32ShrU
        | Operator::I32Eq
        | Operator::I32Ne
        | Operator::I32Eqz
        | Operator::I32LtS
        | Operator::I32LtU
        | Operator::I32LeS
        | Operator::I32LeU
        | Operator::I32GtS
        | Operator::I32GtU
        | Operator::I32GeS
        | Operator::I32GeU
        | Operator::I64Add
        | Operator::I64Sub
        | Operator::I64Mul
        | Operator::I64And
        | Operator::I64Or
        | Operator::I64Xor
        | Operator::I64Eq
        | Operator::I64Ne
        | Operator::I64Eqz
        | Operator::I64LtS
        | Operator::I64LtU
        | Operator::I64LeS
        | Operator::I64LeU
        | Operator::I64GtS
        | Operator::I64GtU
        | Operator::I64GeS
        | Operator::I64GeU
        | Operator::F32Add
        | Operator::F32Sub
        | Operator::F32Mul
        | Operator::F32Div
        | Operator::F64Add
        | Operator::F64Sub
        | Operator::F64Mul
        | Operator::F64Div
        | Operator::I32WrapI64
        | Operator::I64ExtendI32S
        | Operator::I64ExtendI32U => true,
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
        _ => false,
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

    fn trap_if(&mut self, cond: Value) {
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

fn store_field(
    emit: &mut Emit<'_>,
    memory: portal_pc_waffle::Memory,
    base_addr: Value,
    slot: &CoreGcSlotLayout,
    value: Lowered,
) -> Result<(), CoreGcError> {
    match (&slot.storage, value) {
        (CoreGcStorage::ManagedRef { .. }, Lowered::Fat(addr, type_id)) => {
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
        CoreGcStorage::ManagedRef { .. } => {
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
                emit.trap_if(overflow);
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
                    LowerValue::FatRef { .. } => {
                        let addr = emit.const_i32(0);
                        let type_id = emit.const_i32(0);
                        Ok(Some(Lowered::Fat(addr, type_id)))
                    }
                }
            }
            Operator::RefIsNull => {
                let value = self.lowered(args[0]);
                let addr = match value {
                    Lowered::Scalar(v) => v,
                    Lowered::Fat(addr, _) => addr,
                };
                let zero = emit.const_i32(0);
                let result = emit.op(Operator::I32Eq, &[addr, zero], &[Type::I32]);
                Ok(Some(Lowered::Scalar(result)))
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
            Some(LowerValue::FatRef { .. }) => {
                let addr = emit.pick(result, 0, Type::I32);
                let type_id = emit.pick(result, 1, Type::I32);
                Some(Lowered::Fat(addr, type_id))
            }
        }
    }
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

    let is_fatref = |v: Value| value_plan.get(&v).is_some_and(|p| p.is_fatref());
    let computed = liveness::compute(source_body, is_fatref, is_checkpoint_operator)?;
    let spilled: BTreeSet<Value> = computed
        .checkpoint_live
        .values()
        .flatten()
        .copied()
        .collect();
    let slot_of: BTreeMap<Value, u32> = spilled
        .iter()
        .enumerate()
        .map(|(index, &value)| (value, index as u32))
        .collect();

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
                LowerValue::FatRef { .. } => {
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
                LowerValue::FatRef { .. } => {
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
mod tests {
    use super::*;
    use crate::coregc::CoreGcInventory;
    use crate::coregc_runtime::{self, CoreGcOptions};
    use portal_pc_waffle::{
        Export, ExportKind, SignatureData, StorageType, WithMutablility, WithNullable,
    };
    use wasmtime::{Engine, Instance, Module as WasmtimeModule, Store};

    fn field(value: StorageType) -> WithMutablility<StorageType> {
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
    fn list_sum_source() -> (Module<'static>, Signature) {
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
    fn array_sum_source() -> Module<'static> {
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
    fn direct_call_source() -> Module<'static> {
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
    fn call_ref_source() -> Module<'static> {
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


    fn lower_and_instantiate(source: &Module<'_>, options: CoreGcOptions) -> (Store<()>, Instance) {
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
