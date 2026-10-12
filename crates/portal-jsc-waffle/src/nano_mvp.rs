//! The nano WASM-MVP emitter (DreamComp #30): `BackendPlan` → pure core Wasm.
//!
//! This is the first runnable nano backend. It consumes the stable
//! [`BackendPlan`] (`#25`), rebuilds a Waffle `Module` from the plan's flat
//! block-parameter CFG with every reference/aggregate operation lowered onto
//! the logical arena ABI in one linear memory, and encodes with the existing
//! (determinism-pinned) waffle backend — no separate opcode writer.
//!
//! Arena ABI (per #21): one instance-owned linear memory, a monotonic bump
//! allocator with a deterministic exhaustion trap, and an 8-byte header
//! (`type_id`, `len`) per allocation. References lower to i32 arena offsets;
//! offset 0 is the reserved null sentinel and is never allocated. Function
//! references lower to table indices; index 0 is a trapping stub.
//!
//! Mid-block trapping operations (null dereference, out-of-bounds, failed
//! cast, allocation exhaustion) are factored into generated helper functions,
//! so plan blocks never need internal control-flow splits.
//!
//! Fail-closed: any plan operator outside the supported surface produces a
//! structured [`NanoMvpError`] and the whole emission fails. Nothing is
//! silently substituted from CoreGC.

use std::collections::BTreeMap;
use std::fmt;

use portal_pc_waffle::{
    Block, BlockTarget, EntityRef, Export, ExportKind, Func, FuncDecl, FunctionBody, Global,
    GlobalData, HeapType, Import, ImportKind, Memory, MemoryArg, MemoryData, Module, Operator,
    Signature, SignatureData, Table, TableData, Terminator, Type, Value, ValueDef, WithNullable,
};

use crate::backend_plan::{
    BackendPlan, PlanCallKind, PlanConst, PlanFuncId, PlanFunction, PlanHeapType, PlanInst,
    PlanSigId, PlanSignature, PlanStorage, PlanTerminator, PlanType, PlanValueDef, PlanValueId,
};

/// The emitter schema this module implements.
pub const NANO_MVP_SCHEMA: &str = "jsaw.nano-mvp.v1";

const HEADER_SIZE: i32 = 8;
const NULL_OFFSET: i32 = 0;
/// The bump pointer starts above the null slot and reserved low bytes.
const ARENA_BASE: i32 = 16;
/// wasm32 memories cap at 65536 pages (4 GiB); growing past the cap fails,
/// and the allocator turns that into a deterministic trap.
const MAX_PAGES: u32 = 65536;

/// A fail-closed emission diagnostic.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NanoMvpError {
    /// The function being emitted, when known.
    pub function: Option<PlanFuncId>,
    /// Stable diagnostic kind (an operator name or a short snake_case tag).
    pub kind: &'static str,
    /// Human-readable detail.
    pub detail: String,
}

impl NanoMvpError {
    fn new(function: Option<PlanFuncId>, kind: &'static str, detail: impl Into<String>) -> Self {
        Self {
            function,
            kind,
            detail: detail.into(),
        }
    }
}

impl fmt::Display for NanoMvpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.function {
            Some(func) => write!(f, "f{}: {}: {}", func.index(), self.kind, self.detail),
            None => write!(f, "{}: {}", self.kind, self.detail),
        }
    }
}

impl std::error::Error for NanoMvpError {}

/// Emit one BackendPlan as a pure core-Wasm module.
pub fn emit_nano_mvp(plan: &BackendPlan) -> Result<Vec<u8>, Vec<NanoMvpError>> {
    let mut emitter = Emitter::new(plan);
    emitter.emit()
}

/// Deterministic emission fingerprint: SHA3-256 over a domain separator and
/// the emitted bytes, lowercase hex.
pub fn emit_nano_mvp_fingerprint(bytes: &[u8]) -> String {
    use sha3::Digest;
    let mut hasher = sha3::Sha3_256::new();
    hasher.update(b"portal-jsc-nano-mvp\0");
    hasher.update((bytes.len() as u64).to_be_bytes());
    hasher.update(bytes);
    hasher
        .finalize()
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}

// ---------------------------------------------------------------------------
// Layout
// ---------------------------------------------------------------------------

/// Physical layout of one aggregate signature in the arena.
#[derive(Clone, Debug)]
struct AggLayout {
    /// Runtime type id stored in the allocation header.
    type_id: i32,
    /// Array aggregate (element-wise layout) vs struct (field-wise).
    is_array: bool,
    /// Struct: byte offset of each field from the allocation base.
    field_offsets: Vec<u32>,
    /// Total allocation size including the header (arrays: the fixed prefix;
    /// the element area is computed per allocation from the length).
    fixed_size: u32,
    /// Allocation alignment, as log2.
    align_log2: u32,
    /// Array: element byte size.
    elem_size: u32,
}

/// Size/alignment of one plan type's physical storage (references are i32
/// arena offsets).
fn storage_of(plan: &BackendPlan, ty: usize) -> Option<(u32, u32)> {
    Some(match &plan.types[ty] {
        PlanType::I32 | PlanType::F32 => (4, 4),
        PlanType::I64 | PlanType::F64 => (8, 8),
        PlanType::Ref { .. } => (4, 4),
        PlanType::V128 => (16, 16),
    })
}

/// Size/alignment of one field storage class.
fn field_storage(plan: &BackendPlan, storage: &PlanStorage) -> Option<(u32, u32)> {
    Some(match storage {
        PlanStorage::I8 => (1, 1),
        PlanStorage::I16 => (2, 2),
        PlanStorage::Val(ty) => storage_of(plan, ty.index())?,
    })
}

// ---------------------------------------------------------------------------
// Emitter
// ---------------------------------------------------------------------------

struct Emitter<'p> {
    plan: &'p BackendPlan,
    errors: Vec<NanoMvpError>,
    /// PlanSigId → layout, for aggregate signatures.
    layouts: BTreeMap<u32, AggLayout>,
    /// PlanSigId → waffle Signature (function signatures only).
    func_sigs: BTreeMap<u32, Signature>,
    /// PlanFuncId → waffle Func.
    funcs: BTreeMap<u32, Func>,
    /// ref.func target → table index (0 is the trapping stub).
    table_index: BTreeMap<u32, u32>,
    /// The arena's bump-pointer global (appended after the plan's globals).
    bump_global: Global,
    /// The arena memory (appended after the plan's memories).
    arena_memory: Memory,
    /// The funcref table (the plan's own funcref table if it declares one,
    /// else a fresh one appended last).
    func_table: Table,
    /// The shared allocator.
    alloc_func: Func,
    /// The trapping null-dereference guard.
    deref_func: Func,
    /// The array bounds guard.
    bounds_func: Func,
    /// The ref.cast guard.
    cast_func: Func,
    /// The trapping null-funcref stub.
    trap_stub: Func,
    /// The allocator signature (reused as a placeholder swap target).
    alloc_signature: Signature,
    /// The boundary between struct and array type ids for generic tests.
    struct_type_max: i32,
}

impl<'p> Emitter<'p> {
    fn new(plan: &'p BackendPlan) -> Self {
        Self {
            plan,
            errors: Vec::new(),
            layouts: BTreeMap::new(),
            func_sigs: BTreeMap::new(),
            funcs: BTreeMap::new(),
            table_index: BTreeMap::new(),
            bump_global: Global::invalid(),
            arena_memory: Memory::invalid(),
            func_table: Table::invalid(),
            alloc_func: Func::invalid(),
            deref_func: Func::invalid(),
            bounds_func: Func::invalid(),
            cast_func: Func::invalid(),
            trap_stub: Func::invalid(),
            alloc_signature: Signature::invalid(),
            struct_type_max: 0,
        }
    }

    fn error(&mut self, function: Option<PlanFuncId>, kind: &'static str, detail: impl Into<String>) {
        self.errors.push(NanoMvpError::new(function, kind, detail));
    }

    /// Map a plan value type to the arena-lowered wasm type.
    fn lower_type(&mut self, ty: usize) -> Option<Type> {
        let lowered = match &self.plan.types[ty] {
            PlanType::I32 => Type::I32,
            PlanType::I64 => Type::I64,
            PlanType::F32 => Type::F32,
            PlanType::F64 => Type::F64,
            PlanType::Ref { .. } => Type::I32,
            PlanType::V128 => {
                self.error(None, "unsupported_type", "v128 is outside the MVP surface");
                return None;
            }
        };
        Some(lowered)
    }

    fn lower_types(&mut self, ids: &[crate::backend_plan::TypeId]) -> Option<Vec<Type>> {
        let mut out = Vec::with_capacity(ids.len());
        for id in ids {
            out.push(self.lower_type(id.index())?);
        }
        Some(out)
    }

    fn emit(&mut self) -> Result<Vec<u8>, Vec<NanoMvpError>> {
        self.compute_layouts();
        let mut module = Module::empty();

        self.emit_signatures(&mut module);
        self.emit_funcs_and_table(&mut module);
        self.emit_globals_and_memory(&mut module);
        self.emit_allocator(&mut module);
        self.emit_guards(&mut module);
        if !self.errors.is_empty() {
            return Err(std::mem::take(&mut self.errors));
        }
        for func in 0..self.plan.functions.len() {
            self.emit_body(&mut module, PlanFuncId(func as u32));
        }
        self.emit_boundaries(&mut module);
        module.start_func = self.plan.start.and_then(|f| self.funcs.get(&f.0).copied());
        if !self.errors.is_empty() {
            return Err(std::mem::take(&mut self.errors));
        }
        portal_pc_waffle::to_wasm_bytes(&module)
            .map_err(|e| vec![NanoMvpError::new(None, "encode", format!("{e:#}"))])
    }

    /// Assign every aggregate signature a deterministic layout and runtime
    /// type id. Structs get the low range, arrays follow; generic
    /// `ref.test <struct>`/`ref.test <array>` checks test the range boundary.
    fn compute_layouts(&mut self) {
        let mut struct_sigs = Vec::new();
        let mut array_sigs = Vec::new();
        for (index, sig) in self.plan.signatures.iter().enumerate() {
            match sig {
                PlanSignature::Struct { .. } => struct_sigs.push(index as u32),
                PlanSignature::Array { .. } => array_sigs.push(index as u32),
                _ => {}
            }
        }
        let mut next_id: i32 = 1;
        for sig_id in struct_sigs {
            let PlanSignature::Struct { fields, .. } = &self.plan.signatures[sig_id as usize]
            else {
                continue;
            };
            let mut offsets = Vec::with_capacity(fields.len());
            let mut cursor = HEADER_SIZE as u32;
            let mut max_align = 8u32;
            let mut ok = true;
            for field in fields {
                let Some((size, align)) = field_storage(self.plan, &field.storage) else {
                    self.error(
                        None,
                        "unsupported_layout",
                        format!("struct signature {sig_id} has an unsupported field storage"),
                    );
                    ok = false;
                    break;
                };
                max_align = max_align.max(align);
                cursor = (cursor + align - 1) & !(align - 1);
                offsets.push(cursor);
                cursor += size;
            }
            if !ok {
                continue;
            }
            let size = (cursor + max_align - 1) & !(max_align - 1);
            self.layouts.insert(
                sig_id,
                AggLayout {
                    type_id: next_id,
                    is_array: false,
                    field_offsets: offsets,
                    fixed_size: size,
                    align_log2: max_align.trailing_zeros(),
                    elem_size: 0,
                },
            );
            next_id += 1;
        }
        self.struct_type_max = next_id - 1;
        for sig_id in array_sigs {
            let PlanSignature::Array { element, .. } = &self.plan.signatures[sig_id as usize]
            else {
                continue;
            };
            let Some((elem_size, elem_align)) = field_storage(self.plan, &element.storage)
            else {
                self.error(
                    None,
                    "unsupported_layout",
                    format!("array signature {sig_id} has an unsupported element storage"),
                );
                continue;
            };
            let align = elem_align.max(8);
            self.layouts.insert(
                sig_id,
                AggLayout {
                    type_id: next_id,
                    is_array: true,
                    field_offsets: Vec::new(),
                    fixed_size: HEADER_SIZE as u32,
                    align_log2: align.trailing_zeros(),
                    elem_size,
                },
            );
            next_id += 1;
        }
    }

    fn emit_signatures(&mut self, module: &mut Module<'static>) {
        for (index, sig) in self.plan.signatures.iter().enumerate() {
            if let PlanSignature::Func {
                params,
                returns,
                shared,
            } = sig
            {
                let (Some(params), Some(returns)) =
                    (self.lower_types(params), self.lower_types(returns))
                else {
                    continue;
                };
                let waffle_sig = module.signatures.push(SignatureData::Func {
                    params: params.to_vec(),
                    returns: returns.to_vec(),
                    shared: *shared,
                });
                self.func_sigs.insert(index as u32, waffle_sig);
            }
        }
    }

    /// Register every function (imports first — core Wasm orders the function
    /// index space import-first) and build the funcref table contents.
    fn emit_funcs_and_table(&mut self, module: &mut Module<'static>) {
        // Core Wasm orders the function index space import-first; a defined
        // function preceding an import would produce an invalid module, so
        // fail closed on such plans instead of emitting one.
        let mut seen_defined = false;
        for (index, func) in self.plan.functions.iter().enumerate() {
            let Some(&sig) = self.func_sigs.get(&func.signature.0) else {
                self.error(
                    Some(PlanFuncId(index as u32)),
                    "unsupported_type",
                    format!("function {:?} has a non-function signature", func.signature),
                );
                continue;
            };
            match &func.kind {
                crate::backend_plan::PlanFunctionKind::Import {
                    module: import_module,
                    name,
                } => {
                    if seen_defined {
                        self.error(
                            Some(PlanFuncId(index as u32)),
                            "import_order",
                            format!("import {:?} follows a defined function", func.name),
                        );
                        continue;
                    }
                    let waffle_func = module
                        .funcs
                        .push(FuncDecl::Import(sig, format!("{import_module}.{name}")));
                    module.imports.push(Import {
                        module: import_module.clone(),
                        name: name.clone(),
                        kind: ImportKind::Func(waffle_func),
                    });
                    self.funcs.insert(index as u32, waffle_func);
                }
                crate::backend_plan::PlanFunctionKind::Defined => {
                    seen_defined = true;
                    let waffle_func = module.funcs.push(FuncDecl::Body(
                        sig,
                        func.name.clone(),
                        FunctionBody::new(module, sig),
                    ));
                    self.funcs.insert(index as u32, waffle_func);
                }
            }
        }

        // Collect every ref.func target in plan order, deduplicated. Table
        // index 0 is the trapping stub for null funcrefs; real targets start
        // at 1.
        let mut seen: BTreeMap<u32, u32> = BTreeMap::new();
        let mut order: Vec<u32> = Vec::new();
        for func in &self.plan.functions {
            for block in &func.blocks {
                for inst in &block.insts {
                    if let Some(target) = inst.ref_func
                        && !seen.contains_key(&target.0)
                    {
                        seen.insert(target.0, (order.len() + 1) as u32);
                        order.push(target.0);
                    }
                }
            }
        }
        self.table_index = seen;

        let stub_sig = module.signatures.push(SignatureData::Func {
            params: vec![],
            returns: vec![],
            shared: false,
        });
        let stub = module.funcs.push(FuncDecl::Body(
            stub_sig,
            "js_nano_trap_stub".into(),
            FunctionBody::new(module, stub_sig),
        ));
        self.trap_stub = stub;
        let mut table_elems = vec![stub];
        for target in order {
            let Some(&waffle_func) = self.funcs.get(&target) else {
                self.error(
                    None,
                    "undefined_function",
                    format!("ref.func target {target} is not a plan function"),
                );
                continue;
            };
            table_elems.push(waffle_func);
        }

        // The plan's funcref table becomes ours: its WasmGC element encoding
        // is not the MVP table's job; our contents are the stub + targets.
        let mut found = false;
        for table in &self.plan.tables {
            let is_funcref = matches!(
                self.plan.types.get(table.element.index()),
                Some(PlanType::Ref {
                    heap: PlanHeapType::FuncRef,
                    ..
                })
            );
            if is_funcref && !found {
                self.func_table = module.tables.push(TableData {
                    ty: Type::Heap(WithNullable {
                        nullable: true,
                        value: HeapType::FuncRef,
                    }),
                    initial: table_elems.len() as u64,
                    max: table.max,
                    func_elements: Some(table_elems.clone()),
                    table64: table.table64,
                });
                found = true;
            } else {
                let Some(ty) = self.lower_type(table.element.index()) else {
                    continue;
                };
                module.tables.push(TableData {
                    ty,
                    initial: table.initial,
                    max: table.max,
                    func_elements: None,
                    table64: table.table64,
                });
            }
        }
        if !found {
            self.func_table = module.tables.push(TableData {
                ty: Type::Heap(WithNullable {
                    nullable: true,
                    value: HeapType::FuncRef,
                }),
                initial: table_elems.len() as u64,
                max: None,
                func_elements: Some(table_elems),
                table64: false,
            });
        }
    }

    fn emit_globals_and_memory(&mut self, module: &mut Module<'static>) {
        for global in &self.plan.globals {
            let Some(ty) = self.lower_type(global.ty.index()) else {
                continue;
            };
            module.globals.push(GlobalData {
                ty,
                value: global.init,
                mutable: global.mutable,
            });
        }
        self.bump_global = module.globals.push(GlobalData {
            ty: Type::I32,
            value: Some(ARENA_BASE as u64),
            mutable: true,
        });
        for memory in &self.plan.memories {
            module.memories.push(MemoryData {
                initial_pages: memory.initial_pages as usize,
                maximum_pages: memory.maximum_pages.map(|m| m as usize),
                segments: Vec::new(),
                memory64: memory.memory64,
                shared: memory.shared,
                page_size_log2: None,
            });
        }
        self.arena_memory = module.memories.push(MemoryData {
            initial_pages: 1,
            maximum_pages: Some(MAX_PAGES as usize),
            segments: Vec::new(),
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
    }

    /// The monotonic bump allocator. Exhaustion past the memory cap traps
    /// deterministically (`memory.grow` yields -1 → `unreachable`).
    fn emit_allocator(&mut self, module: &mut Module<'static>) {
        let sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32, Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        self.alloc_signature = sig;
        let bump = self.bump_global;
        let mem = self.arena_memory;
        let mut body = FunctionBody::new(module, sig);
        let entry = body.entry;
        let size = body.blocks[entry].params[0].1;
        let align_log2 = body.blocks[entry].params[1].1;

        let bump32 = body.add_op(entry, Operator::GlobalGet { global_index: bump }, &[], &[Type::I32]);
        let bump64 = body.add_op(entry, Operator::I64ExtendI32U, &[bump32], &[Type::I64]);
        let one = body.add_op(entry, Operator::I64Const { value: 1 }, &[], &[Type::I64]);
        let align64 = body.add_op(entry, Operator::I64ExtendI32U, &[align_log2], &[Type::I64]);
        let shifted = body.add_op(entry, Operator::I64Shl, &[one, align64], &[Type::I64]);
        let mask = body.add_op(entry, Operator::I64Sub, &[shifted, one], &[Type::I64]);
        let bumped = body.add_op(entry, Operator::I64Add, &[bump64, mask], &[Type::I64]);
        let all_ones = body.add_op(entry, Operator::I64Const { value: u64::MAX }, &[], &[Type::I64]);
        let not_mask = body.add_op(entry, Operator::I64Xor, &[mask, all_ones], &[Type::I64]);
        let aligned = body.add_op(entry, Operator::I64And, &[bumped, not_mask], &[Type::I64]);
        let size64 = body.add_op(entry, Operator::I64ExtendI32U, &[size], &[Type::I64]);
        let end = body.add_op(entry, Operator::I64Add, &[aligned, size64], &[Type::I64]);
        let pages = body.add_op(entry, Operator::MemorySize { mem }, &[], &[Type::I32]);
        let pages64 = body.add_op(entry, Operator::I64ExtendI32U, &[pages], &[Type::I64]);
        let page_size = body.add_op(
            entry,
            Operator::I64Const { value: 65536 },
            &[],
            &[Type::I64],
        );
        let cap = body.add_op(entry, Operator::I64Mul, &[pages64, page_size], &[Type::I64]);
        let fits = body.add_op(entry, Operator::I64LeU, &[end, cap], &[Type::I32]);
        let ok = body.add_block();
        let grow = body.add_block();
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: fits,
                if_true: BlockTarget {
                    block: ok,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: grow,
                    args: vec![],
                },
            },
        );
        let deficit = body.add_op(grow, Operator::I64Sub, &[end, cap], &[Type::I64]);
        let round = body.add_op(grow, Operator::I64Const { value: 65535 }, &[], &[Type::I64]);
        let deficit = body.add_op(grow, Operator::I64Add, &[deficit, round], &[Type::I64]);
        let sixteen = body.add_op(grow, Operator::I64Const { value: 16 }, &[], &[Type::I64]);
        let needed64 = body.add_op(grow, Operator::I64ShrU, &[deficit, sixteen], &[Type::I64]);
        let needed = body.add_op(grow, Operator::I32WrapI64, &[needed64], &[Type::I32]);
        let grown = body.add_op(grow, Operator::MemoryGrow { mem }, &[needed], &[Type::I32]);
        let minus_one = body.add_op(grow, Operator::I32Const { value: -1i32 as u32 }, &[], &[Type::I32]);
        let failed = body.add_op(grow, Operator::I32Eq, &[grown, minus_one], &[Type::I32]);
        let ok2 = body.add_block();
        let trap = body.add_block();
        body.set_terminator(
            grow,
            Terminator::CondBr {
                cond: failed,
                if_true: BlockTarget {
                    block: trap,
                    args: vec![],
                },
                if_false: BlockTarget {
                    block: ok2,
                    args: vec![],
                },
            },
        );
        body.set_terminator(trap, Terminator::Unreachable);
        body.set_terminator(
            ok2,
            Terminator::Br {
                target: BlockTarget {
                    block: ok,
                    args: vec![],
                },
            },
        );
        let end32 = body.add_op(ok, Operator::I32WrapI64, &[end], &[Type::I32]);
        body.add_op(ok, Operator::GlobalSet { global_index: bump }, &[end32], &[]);
        let aligned32 = body.add_op(ok, Operator::I32WrapI64, &[aligned], &[Type::I32]);
        body.set_terminator(ok, Terminator::Return { values: vec![aligned32] });

        self.alloc_func = module.funcs.push(FuncDecl::Body(sig, "js_nano_alloc".into(), body));
    }

    /// The trapping guards, each a small generated function so plan blocks
    /// never split mid-block for a trap.
    fn emit_guards(&mut self, module: &mut Module<'static>) {
        // __nano_deref(addr) -> addr; traps on null.
        let deref_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(module, deref_sig);
        let entry = body.entry;
        let addr = body.blocks[entry].params[0].1;
        let is_null = body.add_op(entry, Operator::I32Eqz, &[addr], &[Type::I32]);
        let ok = body.add_block();
        let trap = body.add_block();
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: is_null,
                if_true: BlockTarget { block: trap, args: vec![] },
                if_false: BlockTarget { block: ok, args: vec![] },
            },
        );
        body.set_terminator(trap, Terminator::Unreachable);
        body.set_terminator(ok, Terminator::Return { values: vec![addr] });
        self.deref_func = module.funcs.push(FuncDecl::Body(deref_sig, "js_nano_deref".into(), body));

        // __nano_bounds(idx, len) -> idx; traps on idx >= len (unsigned).
        let bounds_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32, Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(module, bounds_sig);
        let entry = body.entry;
        let idx = body.blocks[entry].params[0].1;
        let len = body.blocks[entry].params[1].1;
        let oob = body.add_op(entry, Operator::I32GeU, &[idx, len], &[Type::I32]);
        let ok = body.add_block();
        let trap = body.add_block();
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: oob,
                if_true: BlockTarget { block: trap, args: vec![] },
                if_false: BlockTarget { block: ok, args: vec![] },
            },
        );
        body.set_terminator(trap, Terminator::Unreachable);
        body.set_terminator(ok, Terminator::Return { values: vec![idx] });
        self.bounds_func = module.funcs.push(FuncDecl::Body(bounds_sig, "js_nano_bounds".into(), body));

        // __nano_cast(addr, expected, mode, nullable) -> addr.
        // mode 0: header type id == expected; mode 1: 0 < id <= expected
        // (any struct); mode 2: id > expected (any array). Null passes a
        // nullable cast and traps otherwise; a type mismatch traps.
        let cast_sig = module.signatures.push(SignatureData::Func {
            params: vec![Type::I32, Type::I32, Type::I32, Type::I32],
            returns: vec![Type::I32],
            shared: false,
        });
        let struct_max = self.struct_type_max;
        let mem = self.arena_memory;
        let mut body = FunctionBody::new(module, cast_sig);
        let entry = body.entry;
        let addr = body.blocks[entry].params[0].1;
        let expected = body.blocks[entry].params[1].1;
        let mode = body.blocks[entry].params[2].1;
        let nullable = body.blocks[entry].params[3].1;
        let is_null = body.add_op(entry, Operator::I32Eqz, &[addr], &[Type::I32]);
        let null_arm = body.add_block();
        let non_null = body.add_block();
        let ok = body.add_block();
        let trap = body.add_block();
        // null → nullable ? ok : trap
        body.set_terminator(
            entry,
            Terminator::CondBr {
                cond: is_null,
                if_true: BlockTarget { block: null_arm, args: vec![] },
                if_false: BlockTarget { block: non_null, args: vec![] },
            },
        );
        body.set_terminator(
            null_arm,
            Terminator::CondBr {
                cond: nullable,
                if_true: BlockTarget { block: ok, args: vec![] },
                if_false: BlockTarget { block: trap, args: vec![] },
            },
        );
        let memarg = |offset: u64| MemoryArg { align: 0, offset, memory: mem };
        // mode < 3 reads the header; 3 (any) and 4 (i31 tag) must not load.
        let three = body.add_op(non_null, Operator::I32Const { value: 3 }, &[], &[Type::I32]);
        let needs_header = body.add_op(non_null, Operator::I32LtU, &[mode, three], &[Type::I32]);
        let header_path = body.add_block();
        let tag_path = body.add_block();
        let join = body.add_block();
        let join_param = body.add_blockparam(join, Type::I32);
        body.set_terminator(
            non_null,
            Terminator::CondBr {
                cond: needs_header,
                if_true: BlockTarget { block: header_path, args: vec![] },
                if_false: BlockTarget { block: tag_path, args: vec![] },
            },
        );
        // Header path: modes 0 (exact), 1 (struct range), 2 (array range).
        let header = body.add_op(header_path, Operator::I32Load { memory: memarg(0) }, &[addr], &[Type::I32]);
        let exact = body.add_op(header_path, Operator::I32Eq, &[header, expected], &[Type::I32]);
        let zero = body.add_op(header_path, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
        let above_zero = body.add_op(header_path, Operator::I32GtS, &[header, zero], &[Type::I32]);
        let at_most = body.add_op(header_path, Operator::I32LeS, &[header, expected], &[Type::I32]);
        let struct_ok = body.add_op(header_path, Operator::I32And, &[above_zero, at_most], &[Type::I32]);
        let beyond = body.add_op(header_path, Operator::I32GtS, &[header, expected], &[Type::I32]);
        let is_mode0 = body.add_op(header_path, Operator::I32Eqz, &[mode], &[Type::I32]);
        let one = body.add_op(header_path, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let is_mode1 = body.add_op(header_path, Operator::I32Eq, &[mode, one], &[Type::I32]);
        let ok_01 = body.add_op(header_path, Operator::TypedSelect { ty: Type::I32 }, &[struct_ok, exact, is_mode0], &[Type::I32]);
        let ok_header = body.add_op(header_path, Operator::TypedSelect { ty: Type::I32 }, &[beyond, ok_01, is_mode1], &[Type::I32]);
        body.set_terminator(
            header_path,
            Terminator::Br { target: BlockTarget { block: join, args: vec![ok_header] } },
        );
        // Tag path: mode 3 (any) is always ok; mode 4 checks the i31 tag bit.
        let four = body.add_op(tag_path, Operator::I32Const { value: 4 }, &[], &[Type::I32]);
        let is_tag = body.add_op(tag_path, Operator::I32Eq, &[mode, four], &[Type::I32]);
        let one2 = body.add_op(tag_path, Operator::I32Const { value: 1 }, &[], &[Type::I32]);
        let tagged = body.add_op(tag_path, Operator::I32And, &[addr, one2], &[Type::I32]);
        let ok_tag = body.add_op(tag_path, Operator::TypedSelect { ty: Type::I32 }, &[tagged, one2, is_tag], &[Type::I32]);
        body.set_terminator(
            tag_path,
            Terminator::Br { target: BlockTarget { block: join, args: vec![ok_tag] } },
        );
        let ok_val = join_param;
        body.set_terminator(
            join,
            Terminator::CondBr {
                cond: ok_val,
                if_true: BlockTarget { block: ok, args: vec![] },
                if_false: BlockTarget { block: trap, args: vec![] },
            },
        );
        body.set_terminator(trap, Terminator::Unreachable);
        body.set_terminator(ok, Terminator::Return { values: vec![addr] });
        self.cast_func = module.funcs.push(FuncDecl::Body(cast_sig, "js_nano_cast".into(), body));

        // The trap stub: `unreachable`, so a null-funcref call_indirect into
        // slot 0 traps deterministically.
        let mut decl = std::mem::replace(
            &mut module.funcs[self.trap_stub],
            FuncDecl::Import(self.alloc_signature, String::new()),
        );
        let stub_body = decl.body_mut().expect("stub was created as a body");
        stub_body.set_terminator(stub_body.entry, Terminator::Unreachable);
        module.funcs[self.trap_stub] = decl;
    }

    // -----------------------------------------------------------------------
    // Bodies
    // -----------------------------------------------------------------------

    fn emit_body(&mut self, module: &mut Module<'static>, plan_func: PlanFuncId) {
        let func = &self.plan.functions[plan_func.index()];
        let crate::backend_plan::PlanFunctionKind::Defined = func.kind else {
            return;
        };
        let Some(&waffle_func) = self.funcs.get(&plan_func.0) else {
            return;
        };
        let Some(entry_plan) = func.entry else {
            self.error(
                Some(plan_func),
                "missing_entry",
                format!("function {} has no entry block", func.name),
            );
            return;
        };

        let mut decl = std::mem::replace(
            &mut module.funcs[waffle_func],
            FuncDecl::Import(self.alloc_signature, String::new()),
        );
        let body = decl.body_mut().expect("defined function has a body");

        let mut blocks: Vec<Block> = Vec::with_capacity(func.blocks.len());
        for i in 0..func.blocks.len() {
            blocks.push(if i == entry_plan.index() {
                body.entry
            } else {
                body.add_block()
            });
        }

        let mut values: Vec<Option<Value>> = vec![None; func.values.len()];
        for (index, plan_block) in func.blocks.iter().enumerate() {
            let waffle_block = blocks[index];
            for (param_index, param) in plan_block.params.iter().enumerate() {
                let value = if index == entry_plan.index() {
                    match body.blocks[waffle_block].params.get(param_index) {
                        Some((_, value)) => *value,
                        None => {
                            self.error(
                                Some(plan_func),
                                "entry_arity",
                                format!(
                                    "entry block param {param_index} exceeds the signature arity"
                                ),
                            );
                            continue;
                        }
                    }
                } else {
                    let ty = func.values[param.index()].ty.map(|ty| ty.index());
                    let Some(ty) = ty.and_then(|ty| self.lower_type(ty)) else {
                        self.error(
                            Some(plan_func),
                            "unsupported_type",
                            format!("block {index} param {param_index} has an unsupported type"),
                        );
                        continue;
                    };
                    body.add_blockparam(waffle_block, ty)
                };
                values[param.index()] = Some(value);
            }
        }

        for (index, plan_block) in func.blocks.iter().enumerate() {
            let waffle_block = blocks[index];
            for inst in &plan_block.insts {
                self.emit_inst(body, waffle_block, plan_func, func, inst, &mut values);
            }
            self.emit_terminator(body, waffle_block, plan_func, func, plan_block, &blocks, &mut values);
        }
        module.funcs[waffle_func] = decl;
    }

    fn map_value(&mut self, plan_func: PlanFuncId, values: &[Option<Value>], id: PlanValueId) -> Value {
        match values.get(id.index()).copied().flatten() {
            Some(value) => value,
            None => {
                self.error(
                    Some(plan_func),
                    "undefined_value",
                    format!("value {id} has no emitted definition"),
                );
                Value::invalid()
            }
        }
    }

    fn map_args(&mut self, plan_func: PlanFuncId, values: &[Option<Value>], ids: &[PlanValueId]) -> Vec<Value> {
        ids.iter().map(|id| self.map_value(plan_func, values, *id)).collect()
    }

    fn record_results(&mut self, values: &mut Vec<Option<Value>>, results: &[PlanValueId], value: Value) {
        if let Some(result) = results.first() {
            values[result.index()] = Some(value);
        }
    }

    // -- arena helpers --------------------------------------------------------

    fn call_alloc(&mut self, body: &mut FunctionBody, block: Block, size: Value, align_log2: u32) -> Value {
        let align = body.add_op(
            block,
            Operator::I32Const { value: align_log2 },
            &[],
            &[Type::I32],
        );
        body.add_op(
            block,
            Operator::Call { function_index: self.alloc_func },
            &[size, align],
            &[Type::I32],
        )
    }

    fn const_i32(&mut self, body: &mut FunctionBody, block: Block, value: i32) -> Value {
        body.add_op(block, Operator::I32Const { value: value as u32 }, &[], &[Type::I32])
    }

    fn memarg(&self, offset: u64) -> MemoryArg {
        MemoryArg { align: 0, offset, memory: self.arena_memory }
    }

    fn load_at(&mut self, body: &mut FunctionBody, block: Block, addr: Value, offset: u32, ty: Type, signed: Option<bool>) -> Value {
        let memory = self.memarg(offset as u64);
        let (op, out) = match (ty, signed) {
            (Type::I32, None) => (Operator::I32Load { memory }, Type::I32),
            (Type::I64, None) => (Operator::I64Load { memory }, Type::I64),
            (Type::F32, None) => (Operator::F32Load { memory }, Type::F32),
            (Type::F64, None) => (Operator::F64Load { memory }, Type::F64),
            (Type::I32, Some(true)) => (Operator::I32Load8S { memory }, Type::I32),
            (Type::I32, Some(false)) => (Operator::I32Load8U { memory }, Type::I32),
            _ => unreachable!(),
        };
        body.add_op(block, op, &[addr], &[out])
    }

    fn store_at(&mut self, body: &mut FunctionBody, block: Block, addr: Value, offset: u32, value: Value, ty: Type, narrow: bool) {
        let memory = self.memarg(offset as u64);
        let op = match (ty, narrow) {
            (Type::I32, false) => Operator::I32Store { memory },
            (Type::I64, false) => Operator::I64Store { memory },
            (Type::F32, false) => Operator::F32Store { memory },
            (Type::F64, false) => Operator::F64Store { memory },
            (Type::I32, true) => Operator::I32Store8 { memory },
            _ => unreachable!(),
        };
        body.add_op(block, op, &[addr, value], &[]);
    }

    fn header(&mut self, body: &mut FunctionBody, block: Block, addr: Value, type_id: i32, len: Value) {
        let type_id = self.const_i32(body, block, type_id);
        self.store_at(body, block, addr, 0, type_id, Type::I32, false);
        self.store_at(body, block, addr, 4, len, Type::I32, false);
    }

    fn deref(&mut self, body: &mut FunctionBody, block: Block, value: Value) -> Value {
        body.add_op(
            block,
            Operator::Call { function_index: self.deref_func },
            &[value],
            &[Type::I32],
        )
    }

    fn bounds(&mut self, body: &mut FunctionBody, block: Block, idx: Value, len: Value) -> Value {
        body.add_op(
            block,
            Operator::Call { function_index: self.bounds_func },
            &[idx, len],
            &[Type::I32],
        )
    }

    // -- the operator surface -------------------------------------------------

    fn emit_inst(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        plan_func: PlanFuncId,
        func: &PlanFunction,
        inst: &PlanInst,
        values: &mut Vec<Option<Value>>,
    ) {
        let name = inst.operator.as_str();
        let args = self.map_args(plan_func, values, &inst.args);

        if let Some(constant) = inst.const_value {
            let (op, out) = match constant {
                PlanConst::I32(v) => (Operator::I32Const { value: v }, Type::I32),
                PlanConst::I64(v) => (Operator::I64Const { value: v }, Type::I64),
                PlanConst::F32(v) => (Operator::F32Const { value: v }, Type::F32),
                PlanConst::F64(v) => (Operator::F64Const { value: v }, Type::F64),
            };
            let result = body.add_op(block, op, &[], &[out]);
            self.record_results(values, &inst.results, result);
            return;
        }

        if name == "pick_output" {
            let Some(result) = inst.results.first() else {
                return;
            };
            let PlanValueDef::PickOutput { index, .. } = &func.values[result.index()].def else {
                self.error(
                    Some(plan_func),
                    "invalid_projection",
                    "pick_output result value is not a PickOutput def",
                );
                return;
            };
            let Some(ty) = func.values[result.index()].ty.and_then(|ty| self.lower_type(ty.index())) else {
                return;
            };
            let result_value = body.add_value(ValueDef::PickOutput(args[0], *index, ty));
            self.record_results(values, &inst.results, result_value);
            return;
        }

        if let Some(call) = &inst.call {
            self.emit_call(body, block, plan_func, func, inst, values, args, call);
            return;
        }

        if inst.arena.is_some()
            || matches!(name, "RefNull" | "RefIsNull" | "RefEq" | "RefI31" | "I31GetS" | "I31GetU")
        {
            self.emit_arena_op(body, block, plan_func, func, inst, values, args);
            return;
        }

        self.emit_scalar_op(body, block, plan_func, func, inst, values, args);
    }

    fn emit_call(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        plan_func: PlanFuncId,
        func: &PlanFunction,
        inst: &PlanInst,
        values: &mut Vec<Option<Value>>,
        args: Vec<Value>,
        call: &crate::backend_plan::PlanCall,
    ) {
        let Some(sig) = self.func_sigs.get(&call.signature.0).copied() else {
            self.error(
                Some(plan_func),
                "call_shape",
                format!("call has non-function signature {:?}", call.signature),
            );
            return;
        };
        let out: Vec<Type> = self.lower_types(&inst.result_types).unwrap_or_default();
        let (op, final_args) = match call.kind {
            PlanCallKind::Direct { callee } => {
                let Some(&target) = self.funcs.get(&callee.0) else {
                    self.error(
                        Some(plan_func),
                        "undefined_function",
                        format!("call target {callee} is not a plan function"),
                    );
                    return;
                };
                (Operator::Call { function_index: target }, args)
            }
            PlanCallKind::Indirect { table } => {
                let Some((callee, rest)) = args.split_last() else {
                    self.error(Some(plan_func), "call_shape", "indirect call has no callee operand");
                    return;
                };
                let mut full = rest.to_vec();
                full.push(*callee);
                (
                    Operator::CallIndirect {
                        sig_index: sig,
                        table_index: <Table as EntityRef>::new(table as usize),
                    },
                    full,
                )
            }
            PlanCallKind::Ref => {
                // call_ref: the callee is the last operand, and funcrefs are
                // table indices — emit call_indirect over our table.
                let Some((callee, rest)) = args.split_last() else {
                    self.error(Some(plan_func), "call_shape", "call_ref has no callee operand");
                    return;
                };
                let mut full = rest.to_vec();
                full.push(*callee);
                (
                    Operator::CallIndirect {
                        sig_index: sig,
                        table_index: self.func_table,
                    },
                    full,
                )
            }
        };
        let result = body.add_op(block, op, &final_args, &out);
        self.record_results(values, &inst.results, result);
    }

    /// The aggregate/reference surface, lowered onto the arena.
    fn emit_arena_op(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        plan_func: PlanFuncId,
        func: &PlanFunction,
        inst: &PlanInst,
        values: &mut Vec<Option<Value>>,
        args: Vec<Value>,
    ) {
        let name = inst.operator.as_str();
        match name {
            "StructNew" | "StructNewDefault" => {
                let Some(sig) = inst.payload.sig else {
                    self.error(Some(plan_func), "layout", format!("{name} has no signature payload"));
                    return;
                };
                let Some(layout) = self.layouts.get(&sig.0).cloned() else {
                    self.error(Some(plan_func), "layout", format!("{name} signature {sig} is not an aggregate"));
                    return;
                };
                let PlanSignature::Struct { fields, .. } = &self.plan.signatures[sig.index()]
                else {
                    unreachable!()
                };
                let size = self.const_i32(body, block, layout.fixed_size as i32);
                let addr = self.call_alloc(body, block, size, layout.align_log2);
                let zero_len = self.const_i32(body, block, 0);
                self.header(body, block, addr, layout.type_id, zero_len);
                for (i, field) in fields.iter().enumerate() {
                    let value = if name == "StructNew" {
                        args[i]
                    } else {
                        self.zero_of(body, block, field)
                    };
                    let (ty, narrow) = self.store_shape(&field.storage);
                    self.store_at(body, block, addr, layout.field_offsets[i], value, ty, narrow);
                }
                self.record_results(values, &inst.results, addr);
            }
            "StructGet" | "StructGetS" | "StructGetU" => {
                let (Some(sig), Some(field)) = (inst.payload.sig, inst.payload.field) else {
                    self.error(Some(plan_func), "layout", format!("{name} has no sig/field payload"));
                    return;
                };
                let Some(layout) = self.layouts.get(&sig.0).cloned() else {
                    self.error(Some(plan_func), "layout", format!("{name} signature {sig} is not an aggregate"));
                    return;
                };
                let PlanSignature::Struct { fields, .. } = &self.plan.signatures[sig.index()]
                else {
                    unreachable!()
                };
                let (ty, signed) = self.load_shape(&fields[field as usize].storage, name);
                let addr = self.deref(body, block, args[0]);
                let result = self.load_at(body, block, addr, layout.field_offsets[field as usize], ty, signed);
                self.record_results(values, &inst.results, result);
            }
            "StructSet" => {
                let (Some(sig), Some(field)) = (inst.payload.sig, inst.payload.field) else {
                    self.error(Some(plan_func), "layout", format!("{name} has no sig/field payload"));
                    return;
                };
                let Some(layout) = self.layouts.get(&sig.0).cloned() else {
                    self.error(Some(plan_func), "layout", format!("{name} signature {sig} is not an aggregate"));
                    return;
                };
                let PlanSignature::Struct { fields, .. } = &self.plan.signatures[sig.index()]
                else {
                    unreachable!()
                };
                let (ty, narrow) = self.store_shape(&fields[field as usize].storage);
                let addr = self.deref(body, block, args[0]);
                self.store_at(body, block, addr, layout.field_offsets[field as usize], args[1], ty, narrow);
            }
            "ArrayNewDefault" => {
                let Some(sig) = inst.payload.sig else {
                    self.error(Some(plan_func), "layout", format!("{name} has no signature payload"));
                    return;
                };
                let Some(layout) = self.layouts.get(&sig.0).cloned() else {
                    self.error(Some(plan_func), "layout", format!("{name} signature {sig} is not an aggregate"));
                    return;
                };
                let len = args[0];
                let size = self.array_size(body, block, layout.elem_size, len);
                let addr = self.call_alloc(body, block, size, layout.align_log2);
                self.header(body, block, addr, layout.type_id, len);
                let zero = self.const_i32(body, block, 0);
                let base = self.const_i32(body, block, HEADER_SIZE);
                let start = body.add_op(block, Operator::I32Add, &[addr, base], &[Type::I32]);
                let elem = self.const_i32(body, block, layout.elem_size as i32);
                let bytes = body.add_op(block, Operator::I32Mul, &[len, elem], &[Type::I32]);
                body.add_op(
                    block,
                    Operator::MemoryFill { mem: self.arena_memory },
                    &[start, zero, bytes],
                    &[],
                );
                self.record_results(values, &inst.results, addr);
            }
            "ArrayNewFixed" => {
                let Some(sig) = inst.payload.sig else {
                    self.error(Some(plan_func), "layout", format!("{name} has no signature payload"));
                    return;
                };
                let Some(layout) = self.layouts.get(&sig.0).cloned() else {
                    self.error(Some(plan_func), "layout", format!("{name} signature {sig} is not an aggregate"));
                    return;
                };
                let PlanSignature::Array { element, .. } = &self.plan.signatures[sig.index()]
                else {
                    unreachable!()
                };
                let (ty, narrow) = self.store_shape(&element.storage);
                let len = self.const_i32(body, block, args.len() as i32);
                let size = self.array_size(body, block, layout.elem_size, len);
                let addr = self.call_alloc(body, block, size, layout.align_log2);
                self.header(body, block, addr, layout.type_id, len);
                for (i, value) in args.iter().enumerate() {
                    let idx = self.const_i32(body, block, i as i32);
                    let slot = self.array_slot(body, block, addr, idx, layout.elem_size);
                    self.store_at(body, block, slot, 0, *value, ty, narrow);
                }
                self.record_results(values, &inst.results, addr);
            }
            "ArrayLen" => {
                let addr = self.deref(body, block, args[0]);
                let result = self.load_at(body, block, addr, 4, Type::I32, None);
                self.record_results(values, &inst.results, result);
            }
            "ArrayGet" | "ArrayGetS" | "ArrayGetU" => {
                let Some(sig) = inst.payload.sig else {
                    self.error(Some(plan_func), "layout", format!("{name} has no signature payload"));
                    return;
                };
                let Some(layout) = self.layouts.get(&sig.0).cloned() else {
                    self.error(Some(plan_func), "layout", format!("{name} signature {sig} is not an aggregate"));
                    return;
                };
                let PlanSignature::Array { element, .. } = &self.plan.signatures[sig.index()]
                else {
                    unreachable!()
                };
                let (ty, signed) = self.load_shape(&element.storage, name);
                let addr = self.deref(body, block, args[0]);
                let len = self.load_at(body, block, addr, 4, Type::I32, None);
                let idx = self.bounds(body, block, args[1], len);
                let slot = self.array_slot(body, block, addr, idx, layout.elem_size);
                let result = self.load_at(body, block, slot, 0, ty, signed);
                self.record_results(values, &inst.results, result);
            }
            "ArraySet" => {
                let Some(sig) = inst.payload.sig else {
                    self.error(Some(plan_func), "layout", format!("{name} has no signature payload"));
                    return;
                };
                let Some(layout) = self.layouts.get(&sig.0).cloned() else {
                    self.error(Some(plan_func), "layout", format!("{name} signature {sig} is not an aggregate"));
                    return;
                };
                let PlanSignature::Array { element, .. } = &self.plan.signatures[sig.index()]
                else {
                    unreachable!()
                };
                let (ty, narrow) = self.store_shape(&element.storage);
                let addr = self.deref(body, block, args[0]);
                let len = self.load_at(body, block, addr, 4, Type::I32, None);
                let idx = self.bounds(body, block, args[1], len);
                let slot = self.array_slot(body, block, addr, idx, layout.elem_size);
                self.store_at(body, block, slot, 0, args[2], ty, narrow);
            }
            "ArrayCopy" => {
                // args: [dest, dest_idx, src, src_idx, len]
                let (Some(dest_sig), Some(src_sig)) = (inst.payload.sig, inst.payload.field) else {
                    self.error(Some(plan_func), "layout", format!("{name} has no sig payload"));
                    return;
                };
                let (Some(dest), Some(src)) = (
                    self.layouts.get(&dest_sig.0).cloned(),
                    self.layouts.get(&src_sig).cloned(),
                ) else {
                    self.error(Some(plan_func), "layout", format!("{name} signature is not an aggregate"));
                    return;
                };
                if dest.elem_size != src.elem_size {
                    self.error(
                        Some(plan_func),
                        "layout",
                        "array.copy between different element sizes is unsupported",
                    );
                    return;
                }
                let dest_addr = self.deref(body, block, args[0]);
                let src_addr = self.deref(body, block, args[2]);
                let dest_slot = self.array_slot(body, block, dest_addr, args[1], dest.elem_size);
                let src_slot = self.array_slot(body, block, src_addr, args[3], src.elem_size);
                let elem = self.const_i32(body, block, dest.elem_size as i32);
                let bytes = body.add_op(block, Operator::I32Mul, &[args[4], elem], &[Type::I32]);
                body.add_op(
                    block,
                    Operator::MemoryCopy { dst_mem: self.arena_memory, src_mem: self.arena_memory },
                    &[dest_slot, src_slot, bytes],
                    &[],
                );
            }
            "RefNull" => {
                let result = self.const_i32(body, block, NULL_OFFSET);
                self.record_results(values, &inst.results, result);
            }
            "RefIsNull" => {
                let result = body.add_op(block, Operator::I32Eqz, &[args[0]], &[Type::I32]);
                self.record_results(values, &inst.results, result);
            }
            "RefEq" => {
                let result = body.add_op(block, Operator::I32Eq, &[args[0], args[1]], &[Type::I32]);
                self.record_results(values, &inst.results, result);
            }
            "RefI31" => {
                // i31 values are tagged odd: arena offsets are always
                // 8-aligned, so the low bit distinguishes them from
                // references without a range check. The 31-bit payload
                // shifts left by one; the sign bit of the tagged value is
                // the payload's own bit 30, so `>>s 1`/`>>u 1` recover it
                // with exactly i31 signed/unsigned semantics.
                let one = self.const_i32(body, block, 1);
                let shifted = body.add_op(block, Operator::I32Shl, &[args[0], one], &[Type::I32]);
                let result = body.add_op(block, Operator::I32Or, &[shifted, one], &[Type::I32]);
                self.record_results(values, &inst.results, result);
            }
            "I31GetS" => {
                let one = self.const_i32(body, block, 1);
                let result = body.add_op(block, Operator::I32ShrS, &[args[0], one], &[Type::I32]);
                self.record_results(values, &inst.results, result);
            }
            "I31GetU" => {
                let one = self.const_i32(body, block, 1);
                let result = body.add_op(block, Operator::I32ShrU, &[args[0], one], &[Type::I32]);
                self.record_results(values, &inst.results, result);
            }
            "RefTest" => {
                let result = self.ref_test(body, block, plan_func, inst, args[0]);
                self.record_results(values, &inst.results, result);
            }
            "RefCast" => {
                let Some(heap) = inst.payload.heap.clone() else {
                    self.error(Some(plan_func), "unsupported_operator", "ref.cast has no heap payload");
                    return;
                };
                let (expected, mode) = match &heap {
                    PlanHeapType::Sig(sig) => {
                        let Some(layout) = self.layouts.get(&sig.0) else {
                            self.error(Some(plan_func), "layout", format!("cast target {sig} is not an aggregate"));
                            return;
                        };
                        (layout.type_id, 0)
                    }
                    PlanHeapType::Struct => (self.struct_type_max, 1),
                    PlanHeapType::Array => (self.struct_type_max, 2),
                    PlanHeapType::Any | PlanHeapType::Eq => {
                        // Casting to the top of the hierarchy is the
                        // identity; no guard call needed.
                        self.record_results(values, &inst.results, args[0]);
                        return;
                    }
                    PlanHeapType::I31 => (0, 4),
                    other => {
                        self.error(
                            Some(plan_func),
                            "unsupported_operator",
                            format!("ref.cast target {other:?} is outside the nano MVP surface"),
                        );
                        return;
                    }
                };
                let expected = self.const_i32(body, block, expected);
                let mode = self.const_i32(body, block, mode);
                let nullable = self.const_i32(body, block, inst.payload.nullable.unwrap_or(false) as i32);
                let result = body.add_op(
                    block,
                    Operator::Call { function_index: self.cast_func },
                    &[args[0], expected, mode, nullable],
                    &[Type::I32],
                );
                self.record_results(values, &inst.results, result);
            }
            _ => {
                self.error(
                    Some(plan_func),
                    "unsupported_operator",
                    format!("arena operator {name} is outside the nano MVP surface"),
                );
            }
        }
    }

    /// `ref.test`: null passes iff the target is nullable; non-null values
    /// check the header type id.
    fn ref_test(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        plan_func: PlanFuncId,
        inst: &PlanInst,
        value: Value,
    ) -> Value {
        let nullable = inst.payload.nullable.unwrap_or(false);
        let Some(heap) = inst.payload.heap.clone() else {
            self.error(Some(plan_func), "unsupported_operator", "ref.test has no heap payload");
            return self.const_i32(body, block, 0);
        };
        let (expected, mode) = match &heap {
            PlanHeapType::Sig(sig) => {
                let Some(layout) = self.layouts.get(&sig.0).cloned() else {
                    self.error(Some(plan_func), "layout", format!("ref target {sig} is not an aggregate"));
                    return self.const_i32(body, block, 0);
                };
                (layout.type_id, 0)
            }
            PlanHeapType::Struct => (self.struct_type_max, 1),
            PlanHeapType::Array => (self.struct_type_max, 2),
            PlanHeapType::Any | PlanHeapType::Eq => (0, 3),
            PlanHeapType::I31 => (0, 4),
            other => {
                self.error(
                    Some(plan_func),
                    "unsupported_operator",
                    format!("ref.test target {other:?} is outside the nano MVP surface"),
                );
                return self.const_i32(body, block, 0);
            }
        };
        // Modes 0-2 read the header; 3 (any) and 4 (i31 tag) must not.
        let type_ok = if mode <= 2 {
            let header = self.load_at(body, block, value, 0, Type::I32, None);
            match mode {
                0 => {
                    let expected = self.const_i32(body, block, expected);
                    body.add_op(block, Operator::I32Eq, &[header, expected], &[Type::I32])
                }
                1 => {
                    let zero = self.const_i32(body, block, 0);
                    let max = self.const_i32(body, block, expected);
                    let above = body.add_op(block, Operator::I32GtS, &[header, zero], &[Type::I32]);
                    let at_most = body.add_op(block, Operator::I32LeS, &[header, max], &[Type::I32]);
                    body.add_op(block, Operator::I32And, &[above, at_most], &[Type::I32])
                }
                _ => {
                    let max = self.const_i32(body, block, expected);
                    body.add_op(block, Operator::I32GtS, &[header, max], &[Type::I32])
                }
            }
        } else if mode == 3 {
            self.const_i32(body, block, 1)
        } else {
            let one = self.const_i32(body, block, 1);
            body.add_op(block, Operator::I32And, &[value, one], &[Type::I32])
        };
        let is_null = body.add_op(block, Operator::I32Eqz, &[value], &[Type::I32]);
        if nullable {
            body.add_op(block, Operator::I32Or, &[is_null, type_ok], &[Type::I32])
        } else {
            let not_null = body.add_op(block, Operator::I32Eqz, &[is_null], &[Type::I32]);
            body.add_op(block, Operator::I32And, &[not_null, type_ok], &[Type::I32])
        }
    }

    // -- layout helpers -------------------------------------------------------

    /// The store shape (waffle type + sub-width flag) for a storage class.
    fn store_shape(&mut self, storage: &PlanStorage) -> (Type, bool) {
        match storage {
            PlanStorage::I8 => (Type::I32, true),
            PlanStorage::I16 => (Type::I32, false),
            PlanStorage::Val(ty) => (self.lower_type(ty.index()).unwrap_or(Type::I32), false),
        }
    }

    /// The load shape (type + signedness) for a storage class under an
    /// operator name (`StructGetS`/`ArrayGetU` select the sub-width load).
    fn load_shape(&mut self, storage: &PlanStorage, name: &str) -> (Type, Option<bool>) {
        match storage {
            PlanStorage::I8 => (Type::I32, Some(name.ends_with('S'))),
            PlanStorage::I16 => (Type::I32, Some(name.ends_with('S'))),
            PlanStorage::Val(ty) => (self.lower_type(ty.index()).unwrap_or(Type::I32), None),
        }
    }

    fn zero_of(&mut self, body: &mut FunctionBody, block: Block, field: &crate::backend_plan::PlanField) -> Value {
        match &field.storage {
            PlanStorage::I8 | PlanStorage::I16 => self.const_i32(body, block, 0),
            PlanStorage::Val(ty) => match &self.plan.types[ty.index()] {
                PlanType::I32 => self.const_i32(body, block, 0),
                PlanType::I64 => body.add_op(block, Operator::I64Const { value: 0 }, &[], &[Type::I64]),
                PlanType::F32 => body.add_op(block, Operator::F32Const { value: 0 }, &[], &[Type::F32]),
                PlanType::F64 => body.add_op(block, Operator::F64Const { value: 0 }, &[], &[Type::F64]),
                PlanType::Ref { .. } => self.const_i32(body, block, NULL_OFFSET),
                PlanType::V128 => self.const_i32(body, block, 0),
            },
        }
    }

    /// `HEADER + len * elem_size` as an allocation size.
    fn array_size(&mut self, body: &mut FunctionBody, block: Block, elem_size: u32, len: Value) -> Value {
        let elem = self.const_i32(body, block, elem_size as i32);
        let bytes = body.add_op(block, Operator::I32Mul, &[len, elem], &[Type::I32]);
        let header = self.const_i32(body, block, HEADER_SIZE);
        body.add_op(block, Operator::I32Add, &[bytes, header], &[Type::I32])
    }

    /// The element slot address for `array[index]` (no bounds/null checks —
    /// callers do those through the guards first).
    fn array_slot(&mut self, body: &mut FunctionBody, block: Block, array: Value, index: Value, elem_size: u32) -> Value {
        let elem = self.const_i32(body, block, elem_size as i32);
        let base = self.const_i32(body, block, HEADER_SIZE);
        let off = body.add_op(block, Operator::I32Mul, &[index, elem], &[Type::I32]);
        let at = body.add_op(block, Operator::I32Add, &[array, off], &[Type::I32]);
        body.add_op(block, Operator::I32Add, &[at, base], &[Type::I32])
    }

    // -- the scalar surface ---------------------------------------------------

    fn emit_scalar_op(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        plan_func: PlanFuncId,
        func: &PlanFunction,
        inst: &PlanInst,
        values: &mut Vec<Option<Value>>,
        args: Vec<Value>,
    ) {
        let name = inst.operator.as_str();
        let out: Vec<Type> = self.lower_types(&inst.result_types).unwrap_or_default();
        let op = match name {
            "Nop" => {
                return;
            }
            "Unreachable" => Operator::Unreachable,
            "I32Eqz" => Operator::I32Eqz,
            "I32Eq" => Operator::I32Eq,
            "I32Ne" => Operator::I32Ne,
            "I32LtS" => Operator::I32LtS,
            "I32LtU" => Operator::I32LtU,
            "I32GtS" => Operator::I32GtS,
            "I32GtU" => Operator::I32GtU,
            "I32LeS" => Operator::I32LeS,
            "I32LeU" => Operator::I32LeU,
            "I32GeS" => Operator::I32GeS,
            "I32GeU" => Operator::I32GeU,
            "I64Eqz" => Operator::I64Eqz,
            "I64Eq" => Operator::I64Eq,
            "I64Ne" => Operator::I64Ne,
            "I64LtS" => Operator::I64LtS,
            "I64LtU" => Operator::I64LtU,
            "I64GtS" => Operator::I64GtS,
            "I64GtU" => Operator::I64GtU,
            "I64LeS" => Operator::I64LeS,
            "I64LeU" => Operator::I64LeU,
            "I64GeS" => Operator::I64GeS,
            "I64GeU" => Operator::I64GeU,
            "F32Eq" => Operator::F32Eq,
            "F32Ne" => Operator::F32Ne,
            "F32Lt" => Operator::F32Lt,
            "F32Gt" => Operator::F32Gt,
            "F32Le" => Operator::F32Le,
            "F32Ge" => Operator::F32Ge,
            "F64Eq" => Operator::F64Eq,
            "F64Ne" => Operator::F64Ne,
            "F64Lt" => Operator::F64Lt,
            "F64Gt" => Operator::F64Gt,
            "F64Le" => Operator::F64Le,
            "F64Ge" => Operator::F64Ge,
            "I32Clz" => Operator::I32Clz,
            "I32Ctz" => Operator::I32Ctz,
            "I32Popcnt" => Operator::I32Popcnt,
            "I32Add" => Operator::I32Add,
            "I32Sub" => Operator::I32Sub,
            "I32Mul" => Operator::I32Mul,
            "I32DivS" => Operator::I32DivS,
            "I32DivU" => Operator::I32DivU,
            "I32RemS" => Operator::I32RemS,
            "I32RemU" => Operator::I32RemU,
            "I32And" => Operator::I32And,
            "I32Or" => Operator::I32Or,
            "I32Xor" => Operator::I32Xor,
            "I32Shl" => Operator::I32Shl,
            "I32ShrS" => Operator::I32ShrS,
            "I32ShrU" => Operator::I32ShrU,
            "I32Rotl" => Operator::I32Rotl,
            "I32Rotr" => Operator::I32Rotr,
            "I64Clz" => Operator::I64Clz,
            "I64Ctz" => Operator::I64Ctz,
            "I64Popcnt" => Operator::I64Popcnt,
            "I64Add" => Operator::I64Add,
            "I64Sub" => Operator::I64Sub,
            "I64Mul" => Operator::I64Mul,
            "I64DivS" => Operator::I64DivS,
            "I64DivU" => Operator::I64DivU,
            "I64RemS" => Operator::I64RemS,
            "I64RemU" => Operator::I64RemU,
            "I64And" => Operator::I64And,
            "I64Or" => Operator::I64Or,
            "I64Xor" => Operator::I64Xor,
            "I64Shl" => Operator::I64Shl,
            "I64ShrS" => Operator::I64ShrS,
            "I64ShrU" => Operator::I64ShrU,
            "I64Rotl" => Operator::I64Rotl,
            "I64Rotr" => Operator::I64Rotr,
            "F32Abs" => Operator::F32Abs,
            "F32Neg" => Operator::F32Neg,
            "F32Ceil" => Operator::F32Ceil,
            "F32Floor" => Operator::F32Floor,
            "F32Trunc" => Operator::F32Trunc,
            "F32Nearest" => Operator::F32Nearest,
            "F32Sqrt" => Operator::F32Sqrt,
            "F32Add" => Operator::F32Add,
            "F32Sub" => Operator::F32Sub,
            "F32Mul" => Operator::F32Mul,
            "F32Div" => Operator::F32Div,
            "F32Min" => Operator::F32Min,
            "F32Max" => Operator::F32Max,
            "F32Copysign" => Operator::F32Copysign,
            "F64Abs" => Operator::F64Abs,
            "F64Neg" => Operator::F64Neg,
            "F64Ceil" => Operator::F64Ceil,
            "F64Floor" => Operator::F64Floor,
            "F64Trunc" => Operator::F64Trunc,
            "F64Nearest" => Operator::F64Nearest,
            "F64Sqrt" => Operator::F64Sqrt,
            "F64Add" => Operator::F64Add,
            "F64Sub" => Operator::F64Sub,
            "F64Mul" => Operator::F64Mul,
            "F64Div" => Operator::F64Div,
            "F64Min" => Operator::F64Min,
            "F64Max" => Operator::F64Max,
            "F64Copysign" => Operator::F64Copysign,
            "I32WrapI64" => Operator::I32WrapI64,
            "I32TruncF32S" => Operator::I32TruncF32S,
            "I32TruncF32U" => Operator::I32TruncF32U,
            "I32TruncF64S" => Operator::I32TruncF64S,
            "I32TruncF64U" => Operator::I32TruncF64U,
            "I64ExtendI32S" => Operator::I64ExtendI32S,
            "I64ExtendI32U" => Operator::I64ExtendI32U,
            "I64TruncF32S" => Operator::I64TruncF32S,
            "I64TruncF32U" => Operator::I64TruncF32U,
            "I64TruncF64S" => Operator::I64TruncF64S,
            "I64TruncF64U" => Operator::I64TruncF64U,
            "F32ConvertI32S" => Operator::F32ConvertI32S,
            "F32ConvertI32U" => Operator::F32ConvertI32U,
            "F32ConvertI64S" => Operator::F32ConvertI64S,
            "F32ConvertI64U" => Operator::F32ConvertI64U,
            "F32DemoteF64" => Operator::F32DemoteF64,
            "F64ConvertI32S" => Operator::F64ConvertI32S,
            "F64ConvertI32U" => Operator::F64ConvertI32U,
            "F64ConvertI64S" => Operator::F64ConvertI64S,
            "F64ConvertI64U" => Operator::F64ConvertI64U,
            "F64PromoteF32" => Operator::F64PromoteF32,
            "I32TruncSatF32S" => Operator::I32TruncSatF32S,
            "I32TruncSatF32U" => Operator::I32TruncSatF32U,
            "I32TruncSatF64S" => Operator::I32TruncSatF64S,
            "I32TruncSatF64U" => Operator::I32TruncSatF64U,
            "I64TruncSatF32S" => Operator::I64TruncSatF32S,
            "I64TruncSatF32U" => Operator::I64TruncSatF32U,
            "I64TruncSatF64S" => Operator::I64TruncSatF64S,
            "I64TruncSatF64U" => Operator::I64TruncSatF64U,
            "I32Extend8S" => Operator::I32Extend8S,
            "I32Extend16S" => Operator::I32Extend16S,
            "I64Extend8S" => Operator::I64Extend8S,
            "I64Extend16S" => Operator::I64Extend16S,
            "I64Extend32S" => Operator::I64Extend32S,
            "F32ReinterpretI32" => Operator::F32ReinterpretI32,
            "F64ReinterpretI64" => Operator::F64ReinterpretI64,
            "I32ReinterpretF32" => Operator::I32ReinterpretF32,
            "I64ReinterpretF64" => Operator::I64ReinterpretF64,
            "Select" => Operator::Select,
            "TypedSelect" => {
                let Some(ty) = out.first().copied() else {
                    self.error(Some(plan_func), "call_shape", "typed_select has no result type");
                    return;
                };
                Operator::TypedSelect { ty }
            }
            "GlobalGet" => {
                let Some(global) = inst.payload.global else {
                    self.error(Some(plan_func), "boundary", "global.get has no global payload");
                    return;
                };
                Operator::GlobalGet { global_index: <Global as EntityRef>::new(global as usize) }
            }
            "GlobalSet" => {
                let Some(global) = inst.payload.global else {
                    self.error(Some(plan_func), "boundary", "global.set has no global payload");
                    return;
                };
                Operator::GlobalSet { global_index: <Global as EntityRef>::new(global as usize) }
            }
            "RefFunc" => {
                let Some(target) = inst.ref_func else {
                    self.error(Some(plan_func), "boundary", "ref.func has no target payload");
                    return;
                };
                let Some(&index) = self.table_index.get(&target.0) else {
                    self.error(
                        Some(plan_func),
                        "undefined_function",
                        format!("ref.func target {target} has no table slot"),
                    );
                    return;
                };
                Operator::I32Const { value: index }
            }
            other => {
                self.error(
                    Some(plan_func),
                    "unsupported_operator",
                    format!("operator {other} is outside the nano MVP surface"),
                );
                return;
            }
        };
        let result = body.add_op(block, op, &args, &out);
        self.record_results(values, &inst.results, result);
    }

    // -- terminators ----------------------------------------------------------

    fn emit_terminator(
        &mut self,
        body: &mut FunctionBody,
        block: Block,
        plan_func: PlanFuncId,
        func: &PlanFunction,
        plan_block: &crate::backend_plan::PlanBlock,
        blocks: &[Block],
        values: &mut Vec<Option<Value>>,
    ) {
        let terminator = match &plan_block.terminator {
            PlanTerminator::Br { target } => Terminator::Br {
                target: BlockTarget {
                    block: blocks[target.block.index()],
                    args: self.map_args(plan_func, values, &target.args),
                },
            },
            PlanTerminator::CondBr { cond, if_true, if_false } => Terminator::CondBr {
                cond: self.map_value(plan_func, values, *cond),
                if_true: BlockTarget {
                    block: blocks[if_true.block.index()],
                    args: self.map_args(plan_func, values, &if_true.args),
                },
                if_false: BlockTarget {
                    block: blocks[if_false.block.index()],
                    args: self.map_args(plan_func, values, &if_false.args),
                },
            },
            PlanTerminator::Select { value, targets, default } => Terminator::Select {
                value: self.map_value(plan_func, values, *value),
                targets: targets
                    .iter()
                    .map(|t| BlockTarget {
                        block: blocks[t.block.index()],
                        args: self.map_args(plan_func, values, &t.args),
                    })
                    .collect(),
                default: BlockTarget {
                    block: blocks[default.block.index()],
                    args: self.map_args(plan_func, values, &default.args),
                },
            },
            PlanTerminator::Return { values: returns } => Terminator::Return {
                values: self.map_args(plan_func, values, returns),
            },
            PlanTerminator::TailCall { call, signature, args } => {
                let args = self.map_args(plan_func, values, args);
                let Some(&sig) = self.func_sigs.get(&signature.0) else {
                    self.error(
                        Some(plan_func),
                        "call_shape",
                        format!("tail call has non-function signature {signature}"),
                    );
                    return;
                };
                match call {
                    PlanCallKind::Direct { callee } => {
                        let Some(&target) = self.funcs.get(&callee.0) else {
                            self.error(
                                Some(plan_func),
                                "undefined_function",
                                format!("tail-call target {callee} is not a plan function"),
                            );
                            return;
                        };
                        Terminator::ReturnCall { func: target, args }
                    }
                    PlanCallKind::Indirect { table } => {
                        let Some((callee, rest)) = args.split_last() else {
                            self.error(Some(plan_func), "call_shape", "indirect tail call has no callee operand");
                            return;
                        };
                        let mut full = rest.to_vec();
                        full.push(*callee);
                        Terminator::ReturnCallIndirect {
                            sig,
                            table: <Table as EntityRef>::new(*table as usize),
                            args: full,
                        }
                    }
                    PlanCallKind::Ref => {
                        let Some((callee, rest)) = args.split_last() else {
                            self.error(Some(plan_func), "call_shape", "tail call_ref has no callee operand");
                            return;
                        };
                        let mut full = rest.to_vec();
                        full.push(*callee);
                        Terminator::ReturnCallIndirect {
                            sig,
                            table: self.func_table,
                            args: full,
                        }
                    }
                }
            }
            PlanTerminator::Trap => Terminator::Unreachable,
            PlanTerminator::Ub => Terminator::UB,
        };
        let _ = func;
        body.set_terminator(block, terminator);
    }

    fn emit_boundaries(&mut self, module: &mut Module<'static>) {
        for boundary in &self.plan.boundaries {
            match boundary.direction {
                crate::backend_plan::BoundaryDirection::Export => match &boundary.target {
                    crate::backend_plan::BoundaryTarget::Func(func) => {
                        let Some(&waffle_func) = self.funcs.get(&func.0) else {
                            self.error(
                                None,
                                "boundary",
                                format!("export {:?} is not a plan function", boundary.name),
                            );
                            continue;
                        };
                        module.exports.push(Export {
                            name: boundary.name.clone(),
                            kind: ExportKind::Func(waffle_func),
                        });
                    }
                    crate::backend_plan::BoundaryTarget::Memory(index) => {
                        module.exports.push(Export {
                            name: boundary.name.clone(),
                            kind: ExportKind::Memory(<Memory as EntityRef>::new(*index as usize)),
                        });
                    }
                    crate::backend_plan::BoundaryTarget::Global(index) => {
                        module.exports.push(Export {
                            name: boundary.name.clone(),
                            kind: ExportKind::Global(<Global as EntityRef>::new(*index as usize)),
                        });
                    }
                    crate::backend_plan::BoundaryTarget::Table(index) => {
                        module.exports.push(Export {
                            name: boundary.name.clone(),
                            kind: ExportKind::Table(<Table as EntityRef>::new(*index as usize)),
                        });
                    }
                    other => {
                        self.error(
                            None,
                            "boundary",
                            format!("export of {other:?} is outside the nano MVP surface"),
                        );
                    }
                },
                crate::backend_plan::BoundaryDirection::Import => {
                    // Imports were registered with their functions.
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
