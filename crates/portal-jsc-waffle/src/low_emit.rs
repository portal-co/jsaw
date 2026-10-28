//! Low-level emitter skeleton and representation trait (DreamComp #36).
//!
//! This module is the shared pre-target machinery for jsaw's C, Rust, and
//! MiniBC backends. It consumes a stable [`BackendPlan`] (#25) — exactly as
//! `nano_mvp` does — and produces a target-neutral [`LeModule`]: typed
//! functions over a flat block-parameter CFG with every scalar operation,
//! call/return plumbing, tail-call trampoline, boundary adaptation, and
//! diagnostic already resolved. What it deliberately does NOT contain is any
//! representation decision: allocation, aggregate/reference operations,
//! function-reference storage, and lifetime/runtime operations all go through
//! the [`Representation`] trait, implemented once per lane
//! ([`nano::NanoRepresentation`], [`coregc::CoreGcRepresentation`]) and
//! shared by every target emitter.
//!
//! Design invariants (operator decisions, `docs/jsaw-low-level-backends-scout.md`
//! §8 in the DreamComp repo):
//!
//! - **No linear-memory emulation.** Targets render native values: C
//!   structs/pointers, Rust types, MiniBC slots. The only byte-addressed
//!   accessor in the IR is the representation-owned arena pair
//!   ([`LeExpr::ArenaLoad`]/[`LeStmt::ArenaStore`]), emitted only by the nano
//!   representation and rendered per target runtime.
//! - **References are one word.** The zero word is `null`; odd words are
//!   `i31` payloads (`((v & 0x7fff_ffff) << 1) | 1`, matching nano_mvp's
//!   tagging); all other words are representation-owned storage identities
//!   (nano: 8-aligned arena offset; CoreGC: pointer to a header'd
//!   allocation). `RefNull`, `RefIsNull`, `RefEq`, `RefI31`, and `I31Get*`
//!   are therefore *shared* lowering, not trait surface. Word arithmetic in
//!   the IR uses i64 operators; targets render word-width ops for
//!   reference-typed locals.
//! - **Traps are deterministic and total.** Mid-block trapping checks live in
//!   helper functions ([`LeBody::Structured`]), so plan blocks never split
//!   mid-block (the nano_mvp guard pattern). [`LeStmt::Trap`] renders as the
//!   lane's deterministic trap, never UB.
//! - **Fail closed.** Any plan operator outside the supported surface is a
//!   structured [`LeDiag`]; nothing is silently substituted.
//!
//! ## Tail-call trampoline contract
//!
//! Both lanes emit `return_call*` (tens of thousands of tail-call
//! terminators across the corpus plans), and neither C nor Rust nor MiniBC
//! has native tail calls. The skeleton rewrites tail calls
//! representation-independently:
//!
//! - The tail-target set `T` is every direct defined tail-call target plus
//!   every `ref.func` target (conservative coverage for `call_ref`/indirect
//!   tails). Every member of `T` has a funcref table slot.
//! - Each `F ∈ T` is emitted as a **step function** `F$step(frame) -> frame`
//!   plus a plain **wrapper** `F(params) -> results`. The frame is a
//!   representation-allocated slot vector: slot 0 holds the next callee's
//!   funcref word (0 = done), slots 1.. hold the erased arguments/results
//!   ([`LeExpr::ToSlot`]/[`LeExpr::FromSlot`]).
//! - A tail call inside a step body stores the erased args into the frame,
//!   stores the callee word in slot 0, and returns the frame. The wrapper
//!   loops: dispatch the callee's step function through the module's step
//!   table (table 1) until slot 0 is null, then unerase the results.
//! - Plain calls to `F` call the wrapper. A tail call inside a function
//!   outside `T` runs a local driver loop (allocate frame, dispatch steps,
//!   unerase results) — the function itself stays plain. A tail call to an
//!   *imported* function can never re-enter the module, so it lowers to a
//!   plain call plus return.
//!
//! ## Root discipline
//!
//! For the CoreGC lane ([`Representation::collects`]), the skeleton emits
//! [`LeStmt::RootFrameEnter`] at function entry, [`LeStmt::RootStore`] after
//! every assignment of a reference-typed local, and [`LeStmt::RootFramePop`]
//! before every return, so a collector can run at any allocation or call
//! with an exact root set. Targets render them as calls into the three
//! well-known runtime entry points `js_coregc_root_enter` /
//! `js_coregc_root_store` / `js_coregc_root_pop`; the nano lane never emits
//! them. Trap exits abandon the frame; the runtime resets its shadow stack
//! at the artifact entry boundary.

pub mod c_render;
pub mod c_runtime;
pub mod coregc;
pub mod debug;
pub mod exec;
pub mod nano;
pub mod rust_render;
pub mod rust_runtime;
mod skeleton;

pub use coregc::CoreGcRepresentation;
pub use nano::NanoRepresentation;
pub use skeleton::{Emitter, emit_low};

/// Emit a complete C99 translation unit for `plan` under representation `R`
/// (DreamComp #38): [`emit_low`] plus [`c_render::render_c_file`], combining
/// the fixed lane runtime with the rendered module glue.
pub fn emit_c<R: Representation>(
    plan: &crate::backend_plan::BackendPlan,
    rep: &R,
) -> Result<String, Vec<LeDiag>> {
    let module = emit_low(plan, rep)?;
    c_render::render_c_file(&module).map_err(|d| vec![d])
}

/// Emit a complete stable-Rust crate for `plan` under representation `R`
/// (DreamComp #40): [`emit_low`] plus [`rust_render::render_rust_file`].
pub fn emit_rust<R: Representation>(
    plan: &crate::backend_plan::BackendPlan,
    rep: &R,
) -> Result<String, Vec<LeDiag>> {
    let module = emit_low(plan, rep)?;
    rust_render::render_rust_file(&module).map_err(|d| vec![d])
}

use crate::backend_plan::{
    BackendPlan, PlanFuncId, PlanHeapType, PlanSigId, PlanSignature, TypeId,
};
use std::collections::BTreeMap;
use std::fmt;

/// Schema identifier carried by every emitted module.
pub const LOW_EMIT_SCHEMA: &str = "jsaw.low-emit.v1";

// ---------------------------------------------------------------------------
// Types and identities
// ---------------------------------------------------------------------------

/// A target-neutral physical value type. `Ref` is the one-word reference
/// scheme described in the module header. `Slot` is the erased word used
/// only inside tail-call frames. `I8`/`I16` are storage-only types: they
/// appear in layouts and arena accesses, never as local types.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LeType {
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Ref,
    /// Erased word inside tail-call frames; never used for plan values.
    Slot,
}

impl LeType {
    pub fn is_ref(self) -> bool {
        matches!(self, LeType::Ref)
    }

    /// Byte size of the storage form (arena layouts; `Ref` is one word).
    pub fn byte_size(self) -> u32 {
        match self {
            LeType::I8 => 1,
            LeType::I16 => 2,
            LeType::I32 | LeType::F32 => 4,
            LeType::I64 | LeType::F64 | LeType::Ref | LeType::Slot => 8,
        }
    }
}

impl fmt::Display for LeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::F32 => "f32",
            Self::F64 => "f64",
            Self::Ref => "ref",
            Self::Slot => "slot",
        };
        f.write_str(name)
    }
}

/// Local identity within one emitted function.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LeLocal(pub u32);
/// Function identity within the emitted module.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LeFuncId(pub u32);
/// Block identity within one emitted function.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LeBlockId(pub u32);

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

/// Scalar unary operators. The bare `Trunc*` conversions are *trapping*;
/// they only appear inside skeleton-owned guard helpers that check
/// NaN/range first, so targets may render them with their native conversion
/// (Rust `as` saturates — harmless after the guard).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LeUnOp {
    I32Eqz,
    I32WrapI64,
    I64ExtendI32S,
    I64ExtendI32U,
    I32Extend8S,
    I32Extend8U,
    I32Extend16S,
    I32Extend16U,
    I32TruncF64S,
    I32TruncF64U,
    I64TruncF64S,
    I64TruncF64U,
    I32TruncSatF64S,
    I32TruncSatF64U,
    I64TruncSatF64S,
    I64TruncSatF64U,
    F32DemoteF64,
    F64PromoteF32,
    F32Abs,
    F64Abs,
    F64Ceil,
    F64Floor,
    F64Sqrt,
    F64Trunc,
    F64Nearest,
    F64Neg,
    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    /// Bit-preserving f64 → i64 (slot erasure; NOT a truncation).
    F64ReinterpretI64,
    /// Bit-preserving i64 → f64.
    I64ReinterpretF64,
    F32ReinterpretI32,
    I32ReinterpretF32,
}

/// Scalar binary operators (i32/i64/f32/f64 arithmetic and comparisons).
/// On `Ref`-typed operands the i64 comparisons are word operations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LeBinOp {
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32LeS,
    I32LeU,
    I32GtS,
    I32GtU,
    I32GeS,
    I32GeU,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64LeS,
    I64LeU,
    I64GtS,
    I64GtU,
    I64GeS,
    I64GeU,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Eq,
    F64Ne,
    F64Lt,
    F64Le,
    F64Gt,
    F64Ge,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Eq,
    F32Ne,
    F32Lt,
    F32Le,
    F32Gt,
    F32Ge,
}

/// A call target.
#[derive(Clone, Debug, PartialEq)]
pub enum LeCallee {
    /// A direct call to a module function (defined or imported).
    Direct(LeFuncId),
    /// An indirect call through a function-pointer expression produced from
    /// a module table ([`LeExpr::TableElem`]).
    Ptr(Box<LeExpr>),
    /// A target-runtime entry point, bound by name (the runtime's symbol in
    /// C/Rust; an ext-op in MiniBC). Every name used must be declared by the
    /// representation's `emit_runtime` manifest.
    Runtime(String),
}

/// A target-neutral expression. Everything here is side-effect-free; calls
/// and stores are statements.
#[derive(Clone, Debug, PartialEq)]
pub enum LeExpr {
    Local(LeLocal),
    ConstI32(u32),
    ConstI64(u64),
    ConstF32(u32),
    ConstF64(u64),
    Unary(LeUnOp, Box<LeExpr>),
    Binary(LeBinOp, Box<LeExpr>, Box<LeExpr>),
    /// `(cond, if_true, if_false)` — the skeleton reorders from the plan's
    /// `(if_true, if_false, cond)` operand convention at lowering.
    Select(Box<LeExpr>, Box<LeExpr>, Box<LeExpr>),
    /// Global read (plan and representation globals; shared plumbing).
    GlobalGet(u32),
    /// The function stored at `index` of module table `table` (table 0:
    /// funcref values; table 1: step dispatch).
    TableElem { table: u32, index: Box<LeExpr> },
    /// Representation-owned arena load (nano only): `ty` load at byte
    /// offset `base + offset` from the arena base.
    ArenaLoad {
        ty: LeType,
        base: Box<LeExpr>,
        offset: u32,
    },
    /// Representation-owned arena store as an expression is deliberately
    /// absent; see [`LeStmt::ArenaStore`].
    /// Typed field access on a representation-declared layout (CoreGC
    /// native-struct targets).
    Field {
        layout: u32,
        field: u32,
        base: Box<LeExpr>,
    },
    /// Typed array element access on a representation-declared layout
    /// (CoreGC native-struct targets).
    Index {
        layout: u32,
        base: Box<LeExpr>,
        index: Box<LeExpr>,
    },
    /// Erase a typed word into a tail-frame slot word.
    ToSlot(LeType, Box<LeExpr>),
    /// Recover a typed word from a tail-frame slot word.
    FromSlot(LeType, Box<LeExpr>),
}

// ---------------------------------------------------------------------------
// Statements and bodies
// ---------------------------------------------------------------------------

/// A target-neutral statement.
#[derive(Clone, Debug, PartialEq)]
pub enum LeStmt {
    Assign(LeLocal, LeExpr),
    /// A call with explicit result locals (multi-result aware).
    Call {
        callee: LeCallee,
        args: Vec<LeExpr>,
        results: Vec<LeLocal>,
    },
    /// Representation-owned arena store (nano only).
    ArenaStore {
        ty: LeType,
        base: LeExpr,
        offset: u32,
        value: LeExpr,
    },
    /// Typed field store on a representation-declared layout (CoreGC).
    SetField {
        layout: u32,
        field: u32,
        base: LeExpr,
        value: LeExpr,
    },
    /// Typed array element store on a representation-declared layout.
    SetIndex {
        layout: u32,
        base: LeExpr,
        index: LeExpr,
        value: LeExpr,
    },
    GlobalSet(u32, LeExpr),
    /// Structured conditional. In plan functions it appears only as the
    /// skeleton's edge-move rendering of `CondBr`/`Select` terminators; in
    /// structured bodies it is free-form.
    If {
        cond: LeExpr,
        then: Vec<LeStmt>,
        else_: Vec<LeStmt>,
    },
    /// Structured loop (trampoline drivers and helpers only).
    Loop(Vec<LeStmt>),
    /// Break out of the innermost `Loop`.
    Break,
    Return(Vec<LeExpr>),
    /// Deterministic trap (renders per target; never UB).
    Trap,
    /// A plan `Ub` marker; rendered as a trap.
    Unreachable,
    /// Edge within a plan-function CFG (preceded by the parallel-move
    /// assignments into the target block's params).
    Goto(LeBlockId),
    /// A block label within a plan-function CFG body.
    Label(LeBlockId),
    /// CoreGC root discipline (never emitted under nano). Rendered by
    /// targets as calls to the well-known runtime entry points documented in
    /// the module header.
    RootFrameEnter {
        slots: u32,
    },
    RootStore {
        slot: u32,
        value: LeLocal,
    },
    RootFramePop,
}

/// A block inside a plan-function body.
#[derive(Clone, Debug, PartialEq)]
pub struct LeBlock {
    pub id: LeBlockId,
    /// Block parameter locals (assigned by incoming edges).
    pub params: Vec<LeLocal>,
    pub body: Vec<LeStmt>,
}

/// Function body shape.
#[derive(Clone, Debug, PartialEq)]
pub enum LeBody {
    /// A plan function or step function: flat labeled CFG; entry is
    /// `blocks[0]` and begins with `Label(0)`.
    Blocks(Vec<LeBlock>),
    /// A helper or trampoline wrapper: structured statements only
    /// (`If`/`Loop`/`Return`; no labels).
    Structured(Vec<LeStmt>),
    /// A runtime entry point supplied by the target's support runtime (not
    /// generated code). The contract is documented by the representation
    /// that declared it; target backends provide the implementation.
    Runtime,
}

/// One emitted function.
#[derive(Clone, Debug, PartialEq)]
pub struct LeFunc {
    pub name: String,
    pub params: Vec<(LeLocal, LeType)>,
    pub results: Vec<LeType>,
    /// Every local of the function (params and temporaries) with its type,
    /// in local-id order. Targets declare locals from this list.
    pub locals: Vec<(LeLocal, LeType)>,
    pub body: LeBody,
}

/// A representation-declared aggregate layout (CoreGC native-struct targets;
/// nano computes byte layouts internally and declares none).
///
/// Convention: layout 0 is always the allocation header; every other layout
/// implicitly begins with it. See `coregc::HEADER_LAYOUT`.
#[derive(Clone, Debug, PartialEq)]
pub struct LeLayout {
    /// Skeleton-assigned runtime type id ([`TypeIdTable`]); 0 for the header.
    pub type_id: u32,
    /// Field types in declaration order (empty for arrays).
    pub fields: Vec<LeType>,
    /// Element type when the layout is an array aggregate.
    pub array_element: Option<LeType>,
}

/// A representation-declared static data blob (e.g. the CoreGC descriptor
/// table consumed by the collector runtime).
#[derive(Clone, Debug, PartialEq)]
pub struct LeStatic {
    pub name: String,
    pub bytes: Vec<u8>,
}

/// A function table (table 0: funcref values, table 1: step dispatch).
/// Index 0 is the trapping stub in both.
#[derive(Clone, Debug, PartialEq)]
pub struct LeTable {
    pub name: String,
    pub elems: Vec<LeFuncId>,
}

/// A declared host import (from plan boundary records).
#[derive(Clone, Debug, PartialEq)]
pub struct LeImport {
    pub module: String,
    pub name: String,
    pub func: LeFuncId,
    pub params: Vec<LeType>,
    pub results: Vec<LeType>,
}

/// A declared export (from plan boundary records).
#[derive(Clone, Debug, PartialEq)]
pub struct LeExport {
    pub name: String,
    pub func: LeFuncId,
}

/// The target-neutral emitted module.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct LeModule {
    pub schema: String,
    /// Lane name ([`Representation::name`]).
    pub lane: String,
    pub layouts: Vec<LeLayout>,
    pub statics: Vec<LeStatic>,
    pub tables: Vec<LeTable>,
    pub imports: Vec<LeImport>,
    pub exports: Vec<LeExport>,
    /// Defined functions in [`LeFuncId`] order (ids are dense from 0).
    pub funcs: Vec<LeFunc>,
    /// Plan and representation globals, as lowered types with scalar inits.
    pub globals: Vec<(LeType, Option<u64>)>,
    /// The plan's start function, if any (run once at artifact init).
    pub start: Option<LeFuncId>,
}

// ---------------------------------------------------------------------------
// Diagnostics
// ---------------------------------------------------------------------------

/// A fail-closed emission diagnostic.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LeDiag {
    pub function: Option<PlanFuncId>,
    pub block: Option<u32>,
    pub inst: Option<u32>,
    pub message: String,
}

impl fmt::Display for LeDiag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "low-emit: {}", self.message)?;
        if let Some(func) = self.function {
            write!(f, " (function {func}")?;
            if let Some(block) = self.block {
                write!(f, ", block {block}")?;
            }
            if let Some(inst) = self.inst {
                write!(f, ", inst {inst}")?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Shared tables
// ---------------------------------------------------------------------------

/// Skeleton-assigned runtime type ids for aggregate signatures.
///
/// Struct signatures take ids `1..=struct_count`, array signatures take
/// `struct_count+1..=struct_count+array_count`, in plan signature order. `0`
/// never identifies an aggregate, so the abstract `struct`/`array` heap-type
/// tests are range comparisons. This mirrors nano_mvp's cast-mode scheme
/// with thin headers.
#[derive(Clone, Debug, Default)]
pub struct TypeIdTable {
    /// Plan signature index → runtime type id.
    pub ids: BTreeMap<u32, u32>,
    pub struct_count: u32,
    pub array_count: u32,
}

impl TypeIdTable {
    pub fn build(plan: &BackendPlan) -> Self {
        // Two passes: struct ids are 1..=S in plan order, array ids
        // S+1..=S+A. (A single pass would hand an array an id a later
        // struct reuses whenever arrays precede structs in plan order.)
        let mut table = TypeIdTable::default();
        for (index, sig) in plan.signatures.iter().enumerate() {
            if matches!(sig, PlanSignature::Struct { .. }) {
                table.struct_count += 1;
                table.ids.insert(index as u32, table.struct_count);
            }
        }
        for (index, sig) in plan.signatures.iter().enumerate() {
            if matches!(sig, PlanSignature::Array { .. }) {
                table.array_count += 1;
                table
                    .ids
                    .insert(index as u32, table.struct_count + table.array_count);
            }
        }
        table
    }

    pub fn of(&self, sig: PlanSigId) -> Option<u32> {
        self.ids.get(&sig.0).copied()
    }
}

// ---------------------------------------------------------------------------
// Helper construction
// ---------------------------------------------------------------------------

/// Builder for structured helper bodies: allocates typed temporaries and
/// appends statements.
pub struct HelperBuilder {
    next: u32,
    /// All locals (params first) with types.
    pub locals: Vec<(LeLocal, LeType)>,
    pub stmts: Vec<LeStmt>,
}

impl HelperBuilder {
    fn new(params: &[LeType]) -> Self {
        let locals = params
            .iter()
            .enumerate()
            .map(|(i, ty)| (LeLocal(i as u32), *ty))
            .collect::<Vec<_>>();
        HelperBuilder {
            next: params.len() as u32,
            locals,
            stmts: Vec::new(),
        }
    }

    pub fn temp(&mut self, ty: LeType) -> LeLocal {
        let local = LeLocal(self.next);
        self.next += 1;
        self.locals.push((local, ty));
        local
    }

    pub fn emit(&mut self, stmt: LeStmt) {
        self.stmts.push(stmt);
    }

    /// Assign `expr` to a fresh temporary of type `ty` and return it.
    pub fn assign(&mut self, ty: LeType, expr: LeExpr) -> LeLocal {
        let local = self.temp(ty);
        self.emit(LeStmt::Assign(local, expr));
        local
    }
}

/// The read-only table bundle shared by every emission context. All fields
/// are fixed before the function walk begins.
/// Read-only shared tables; `Copy` is implemented manually so the
/// representation type need not be `Copy`.
pub struct Tables<'a, R: Representation> {
    /// The plan under emission.
    pub plan: &'a BackendPlan,
    /// The active representation.
    pub rep: &'a R,
    /// Runtime type ids of aggregate signatures (skeleton-assigned).
    pub type_ids: &'a TypeIdTable,
    /// Funcref table slot of a tail-target function (skeleton-assigned).
    pub table_slots: &'a BTreeMap<PlanFuncId, u32>,
    /// Plan function index -> emitted wrapper/plain function id.
    pub func_ids: &'a BTreeMap<u32, LeFuncId>,
    /// Plan function index -> step function id (tail-target set only).
    pub step_ids: &'a BTreeMap<u32, LeFuncId>,
    /// Tail-frame size in slots: 1 (callee) + max(params, results) over the
    /// tail-target set.
    pub frame_slots: u32,
    /// First helper function id.
    pub helper_base: u32,
    /// Whether the active representation collects (CoreGC root discipline).
    pub collects: bool,
}

impl<R: Representation> Clone for Tables<'_, R> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<R: Representation> Copy for Tables<'_, R> {}

/// Immutable per-function emission state (skeleton-side; representation
/// implementations should not need it).
pub struct FnState {
    /// Plan value index -> local.
    pub value_locals: Vec<LeLocal>,
    /// Ref-typed local -> CoreGC root slot.
    pub ref_slots: BTreeMap<LeLocal, u32>,
    /// Step form: the frame local (id 0).
    pub step_frame: Option<LeLocal>,
    /// Reserved driver temps for plain-form non-import tail calls
    /// (frame, current-callee).
    pub driver: Option<(LeLocal, LeLocal)>,
    /// Lowered parameter types (the step form takes only the frame).
    pub param_tys: Vec<LeType>,
    /// Lowered result types (the step form returns the frame).
    pub result_tys: Vec<LeType>,
}

/// The emission context handed to [`Representation`] methods and used by the
/// skeleton. Implementations append straight-line statements and may define
/// helper functions (which is how mid-block traps stay out of plan blocks),
/// but can never split the current plan block.
pub struct InstCx<'a, R: Representation> {
    /// Read-only shared tables.
    pub t: Tables<'a, R>,
    /// Per-function immutable state.
    pub f: &'a FnState,
    /// All locals by id (plan values first; temps appended).
    pub local_tys: &'a mut Vec<LeType>,
    /// Module-level helper pool.
    pub helpers: &'a mut Vec<LeFunc>,
    /// Helper name -> id cache (module-wide).
    pub helper_names: &'a mut BTreeMap<String, LeFuncId>,
    /// Module-level diagnostics sink.
    pub diags: &'a mut Vec<LeDiag>,
    /// Sink for the current statements.
    pub stmts: Vec<LeStmt>,
    /// Current function/block/inst for diagnostics (Nones at module level).
    pub at: (Option<PlanFuncId>, Option<u32>, Option<u32>),
}

impl<'a, R: Representation> InstCx<'a, R> {
    /// Allocate a fresh typed temporary local.
    pub fn temp(&mut self, ty: LeType) -> LeLocal {
        let local = LeLocal(self.local_tys.len() as u32);
        self.local_tys.push(ty);
        local
    }

    /// The type of a local.
    pub fn ty_of(&self, local: LeLocal) -> LeType {
        self.local_tys[local.0 as usize]
    }

    /// Append one statement.
    pub fn emit(&mut self, stmt: LeStmt) {
        self.stmts.push(stmt);
    }

    /// Assign `expr` to a fresh temporary of type `ty` and return it.
    pub fn assign_temp(&mut self, ty: LeType, expr: LeExpr) -> LeLocal {
        let local = self.temp(ty);
        self.emit(LeStmt::Assign(local, expr));
        local
    }

    /// Define (or return the cached) helper function `name`.
    pub fn helper(
        &mut self,
        name: &str,
        params: Vec<LeType>,
        results: Vec<LeType>,
        build: impl FnOnce(&mut HelperBuilder, &[LeLocal]),
    ) -> LeFuncId {
        if let Some(id) = self.helper_names.get(name) {
            return *id;
        }
        let id = LeFuncId(self.t.helper_base + self.helpers.len() as u32);
        let param_locals: Vec<LeLocal> = (0..params.len() as u32).map(LeLocal).collect();
        let mut b = HelperBuilder::new(&params);
        build(&mut b, &param_locals);
        self.helpers.push(LeFunc {
            name: name.to_string(),
            params: param_locals.into_iter().zip(params).collect(),
            results,
            locals: b.locals,
            body: LeBody::Structured(b.stmts),
        });
        self.helper_names.insert(name.to_string(), id);
        id
    }

    /// Record a diagnostic at the current location.
    pub fn error(&mut self, message: impl Into<String>) {
        self.diags.push(LeDiag {
            function: self.at.0,
            block: self.at.1,
            inst: self.at.2,
            message: message.into(),
        });
    }
}


/// Module-level sink for [`Representation::emit_runtime`]: registers helper
/// and runtime functions after all plan functions.
pub struct RuntimeSink<'a> {
    funcs: &'a mut Vec<LeFunc>,
    next_id: &'a mut u32,
}

impl RuntimeSink<'_> {
    pub fn new<'a>(funcs: &'a mut Vec<LeFunc>, next_id: &'a mut u32) -> RuntimeSink<'a> {
        RuntimeSink { funcs, next_id }
    }

    /// Register a generated helper with a structured body.
    pub fn helper(
        &mut self,
        name: &str,
        params: Vec<LeType>,
        results: Vec<LeType>,
        build: impl FnOnce(&mut HelperBuilder, &[LeLocal]),
    ) -> LeFuncId {
        let id = LeFuncId(*self.next_id);
        *self.next_id += 1;
        let param_locals: Vec<LeLocal> = (0..params.len() as u32).map(LeLocal).collect();
        let mut b = HelperBuilder::new(&params);
        build(&mut b, &param_locals);
        self.funcs.push(LeFunc {
            name: name.to_string(),
            params: param_locals.into_iter().zip(params).collect(),
            results,
            locals: b.locals,
            body: LeBody::Structured(b.stmts),
        });
        id
    }

    /// Declare a target-runtime entry point (implemented by the target's
    /// support runtime, not generated).
    pub fn runtime_fn(&mut self, name: &str, params: Vec<LeType>, results: Vec<LeType>) -> LeFuncId {
        let id = LeFuncId(*self.next_id);
        *self.next_id += 1;
        let locals: Vec<(LeLocal, LeType)> = params
            .iter()
            .enumerate()
            .map(|(i, t)| (LeLocal(i as u32), *t))
            .collect();
        self.funcs.push(LeFunc {
            name: name.to_string(),
            params: locals.clone(),
            results,
            locals,
            body: LeBody::Runtime,
        });
        id
    }
}

/// Push every expression a statement directly contains onto `stack`.
pub(crate) fn push_stmt_exprs<'a>(s: &'a LeStmt, stack: &mut Vec<&'a LeExpr>) {
    match s {
        LeStmt::Assign(_, e) => stack.push(e),
        LeStmt::Call { args, .. } => stack.extend(args.iter()),
        LeStmt::ArenaStore { base, value, .. } => {
            stack.push(base);
            stack.push(value);
        }
        LeStmt::SetField { base, value, .. } => {
            stack.push(base);
            stack.push(value);
        }
        LeStmt::SetIndex {
            base,
            index,
            value,
            ..
        } => {
            stack.push(base);
            stack.push(index);
            stack.push(value);
        }
        LeStmt::GlobalSet(_, e) => stack.push(e),
        LeStmt::If { cond, .. } => stack.push(cond),
        LeStmt::Return(values) => stack.extend(values.iter()),
        _ => {}
    }
}

/// Push every direct child expression of `e` onto `stack`.
pub(crate) fn push_expr_children<'a>(e: &'a LeExpr, stack: &mut Vec<&'a LeExpr>) {
    match e {
        LeExpr::Unary(_, x) => stack.push(x),
        LeExpr::Binary(_, a, b) => {
            stack.push(a);
            stack.push(b);
        }
        LeExpr::Select(c, t, f) => {
            stack.push(c);
            stack.push(t);
            stack.push(f);
        }
        LeExpr::TableElem { index, .. } => stack.push(index),
        LeExpr::ArenaLoad { base, .. } => stack.push(base),
        LeExpr::Field { base, .. } => stack.push(base),
        LeExpr::Index { base, index, .. } => {
            stack.push(base);
            stack.push(index);
        }
        LeExpr::ToSlot(_, x) | LeExpr::FromSlot(_, x) => stack.push(x),
        _ => {}
    }
}

/// Whether a plan reference type is a *function* word (bare funcref or a
/// typed `(ref $funcsig)`): funcref words are table-slot indices, not heap
/// references, so representations lower them as `I64` — never rooted,
/// scanned, or field-tracked.
pub(crate) fn plan_heap_is_func_word(plan: &crate::backend_plan::BackendPlan, heap: &crate::backend_plan::PlanHeapType) -> bool {
    match heap {
        crate::backend_plan::PlanHeapType::FuncRef | crate::backend_plan::PlanHeapType::NoFunc => true,
        crate::backend_plan::PlanHeapType::Sig(sig) => matches!(
            plan.signatures.get(sig.index()),
            Some(crate::backend_plan::PlanSignature::Func { .. })
        ),
        _ => false,
    }
}

/// The zero constant for a type (used by `StructNewDefault`).
pub(crate) fn zero_of(ty: LeType) -> LeExpr {
    match ty {
        LeType::F32 => LeExpr::ConstF32(0),
        LeType::F64 => LeExpr::ConstF64(0),
        LeType::I64 | LeType::Ref | LeType::Slot => LeExpr::ConstI64(0),
        _ => LeExpr::ConstI32(0),
    }
}

// ---------------------------------------------------------------------------
// Representation trait
// ---------------------------------------------------------------------------

/// The representation-sensitive emission surface, implemented once per lane
/// (CoreGC, nano) and shared by every target emitter (C, Rust, MiniBC).
///
/// All instruction-lowering methods append to `cx.stmts` and must be
/// straight-line: control flow belongs in helpers defined through
/// [`InstCx::helper`].
pub trait Representation: Sized {
    /// Lane name for diagnostics and the module header (`"coregc"`/`"nano"`).
    fn name(&self) -> &'static str;

    /// Whether this lane has a garbage collector that may run at any
    /// allocation or call. When true, the skeleton emits root-discipline
    /// statements; when false they are omitted.
    fn collects(&self) -> bool;

    /// The physical type of a plan type. Scalar types map to themselves;
    /// every reference type maps to the one-word [`LeType::Ref`].
    fn lower_type(&self, plan: &BackendPlan, ty: TypeId) -> Result<LeType, LeDiag>;

    /// Declare layouts, statics, and lane globals the lane needs. Called
    /// once, before any function is emitted.
    fn declare(&self, plan: &BackendPlan, module: &mut LeModule) -> Result<(), LeDiag>;

    /// Emit the lane's runtime helper functions (allocator, guards, cast
    /// helpers) and declare runtime entry points. Called once, after all
    /// plan functions.
    fn emit_runtime(&self, plan: &BackendPlan, sink: &mut RuntimeSink) -> Result<(), LeDiag>;

    // -- aggregate operations -------------------------------------------------

    /// `StructNew` (`Some(fields)`) or `StructNewDefault` (`None`).
    fn struct_new(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        fields: Option<&[LeExpr]>,
        out: LeLocal,
    ) -> Result<(), LeDiag>;

    fn struct_get(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        field: u32,
        recv: LeExpr,
        out: LeLocal,
    ) -> Result<(), LeDiag>;

    fn struct_set(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        field: u32,
        recv: LeExpr,
        value: LeExpr,
    ) -> Result<(), LeDiag>;

    /// `ArrayNewDefault` (`items: None`, length `len`) or `ArrayNewFixed`
    /// (`items: Some`).
    fn array_new(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        len: LeExpr,
        items: Option<&[LeExpr]>,
        out: LeLocal,
    ) -> Result<(), LeDiag>;

    /// `ArrayGet` (`packed: None`) or the packed `ArrayGetS`/`ArrayGetU`
    /// forms (`Some(signed)`); the element width comes from the signature's
    /// `PlanStorage`.
    fn array_get(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        arr: LeExpr,
        index: LeExpr,
        packed: Option<bool>,
        out: LeLocal,
    ) -> Result<(), LeDiag>;

    fn array_set(
        &self,
        cx: &mut InstCx<Self>,
        sig: PlanSigId,
        arr: LeExpr,
        index: LeExpr,
        value: LeExpr,
    ) -> Result<(), LeDiag>;

    fn array_len(&self, cx: &mut InstCx<Self>, arr: LeExpr, out: LeLocal) -> Result<(), LeDiag>;

    fn array_copy(
        &self,
        cx: &mut InstCx<Self>,
        dst: LeExpr,
        dst_index: LeExpr,
        src: LeExpr,
        src_index: LeExpr,
        len: LeExpr,
    ) -> Result<(), LeDiag>;

    // -- reference tests and casts --------------------------------------------

    /// Emit a reference test producing an i32 0/1 in `out`.
    fn ref_test(
        &self,
        cx: &mut InstCx<Self>,
        heap: &PlanHeapType,
        nullable: bool,
        recv: LeExpr,
        out: LeLocal,
    ) -> Result<(), LeDiag>;

    /// Emit a reference cast producing the (unchanged word) reference in
    /// `out`, trapping deterministically on failure.
    fn ref_cast(
        &self,
        cx: &mut InstCx<Self>,
        heap: &PlanHeapType,
        nullable: bool,
        recv: LeExpr,
        out: LeLocal,
    ) -> Result<(), LeDiag>;

    // -- function references ----------------------------------------------------

    /// The funcref word for `target` (skeleton-assigned table `slot`).
    fn funcref_value(&self, target: PlanFuncId, slot: u32) -> LeExpr;

    /// Recover the callable for a funcref word from [`Self::funcref_value`].
    fn callable(&self, value: LeExpr, sig: PlanSigId) -> LeCallee;

    // -- trampoline frame storage -------------------------------------------------

    /// Allocate a tail-call frame of `slots` slot-words into `out` (a
    /// `Ref`). Slot 0 is the next-callee word; slots 1.. are arguments and
    /// results.
    fn frame_alloc(&self, cx: &mut InstCx<Self>, slots: u32, out: LeLocal) -> Result<(), LeDiag>;

    /// Store a slot word into a frame slot.
    fn frame_store(&self, cx: &mut InstCx<Self>, frame: &LeLocal, slot: u32, value: LeExpr);

    /// Load a slot word from a frame slot. May emit statements (e.g. a
    /// runtime call) and returns the word expression.
    fn frame_load(&self, cx: &mut InstCx<Self>, frame: &LeLocal, slot: u32) -> LeExpr;
}

// ---------------------------------------------------------------------------
// Scalar lowering tables (shared)
// ---------------------------------------------------------------------------

pub(crate) fn scalar_binop(name: &str) -> Option<LeBinOp> {
    use LeBinOp::*;
    Some(match name {
        "I32Add" => I32Add,
        "I32Sub" => I32Sub,
        "I32Mul" => I32Mul,
        "I32DivS" => I32DivS,
        "I32DivU" => I32DivU,
        "I32RemS" => I32RemS,
        "I32RemU" => I32RemU,
        "I32And" => I32And,
        "I32Or" => I32Or,
        "I32Xor" => I32Xor,
        "I32Shl" => I32Shl,
        "I32ShrS" => I32ShrS,
        "I32ShrU" => I32ShrU,
        "I32Eq" => I32Eq,
        "I32Ne" => I32Ne,
        "I32LtS" => I32LtS,
        "I32LtU" => I32LtU,
        "I32LeS" => I32LeS,
        "I32LeU" => I32LeU,
        "I32GtS" => I32GtS,
        "I32GtU" => I32GtU,
        "I32GeS" => I32GeS,
        "I32GeU" => I32GeU,
        "I64Add" => I64Add,
        "I64Sub" => I64Sub,
        "I64Mul" => I64Mul,
        "I64DivS" => I64DivS,
        "I64DivU" => I64DivU,
        "I64RemS" => I64RemS,
        "I64RemU" => I64RemU,
        "I64And" => I64And,
        "I64Or" => I64Or,
        "I64Xor" => I64Xor,
        "I64Shl" => I64Shl,
        "I64ShrS" => I64ShrS,
        "I64ShrU" => I64ShrU,
        "I64Eq" => I64Eq,
        "I64Ne" => I64Ne,
        "I64LtS" => I64LtS,
        "I64LtU" => I64LtU,
        "I64LeS" => I64LeS,
        "I64LeU" => I64LeU,
        "I64GtS" => I64GtS,
        "I64GtU" => I64GtU,
        "I64GeS" => I64GeS,
        "I64GeU" => I64GeU,
        "F64Add" => F64Add,
        "F64Sub" => F64Sub,
        "F64Mul" => F64Mul,
        "F64Div" => F64Div,
        "F64Min" => F64Min,
        "F64Max" => F64Max,
        "F64Eq" => F64Eq,
        "F64Ne" => F64Ne,
        "F64Lt" => F64Lt,
        "F64Le" => F64Le,
        "F64Gt" => F64Gt,
        "F64Ge" => F64Ge,
        "F32Add" => F32Add,
        "F32Sub" => F32Sub,
        "F32Mul" => F32Mul,
        "F32Div" => F32Div,
        "F32Eq" => F32Eq,
        "F32Ne" => F32Ne,
        "F32Lt" => F32Lt,
        "F32Le" => F32Le,
        "F32Gt" => F32Gt,
        "F32Ge" => F32Ge,
        _ => return None,
    })
}

pub(crate) fn scalar_unop(name: &str) -> Option<LeUnOp> {
    use LeUnOp::*;
    Some(match name {
        "I32Eqz" => I32Eqz,
        "I32WrapI64" => I32WrapI64,
        "I64ExtendI32S" => I64ExtendI32S,
        "I64ExtendI32U" => I64ExtendI32U,
        "I32Extend8S" => I32Extend8S,
        "I32Extend8U" => I32Extend8U,
        "I32Extend16S" => I32Extend16S,
        "I32Extend16U" => I32Extend16U,
        "I32TruncSatF64S" => I32TruncSatF64S,
        "I32TruncSatF64U" => I32TruncSatF64U,
        "I64TruncSatF64S" => I64TruncSatF64S,
        "I64TruncSatF64U" => I64TruncSatF64U,
        "F32DemoteF64" => F32DemoteF64,
        "F64PromoteF32" => F64PromoteF32,
        "F32Abs" => F32Abs,
        "F64Abs" => F64Abs,
        "F64Ceil" => F64Ceil,
        "F64Floor" => F64Floor,
        "F64Sqrt" => F64Sqrt,
        "F64Trunc" => F64Trunc,
        "F64Nearest" => F64Nearest,
        "F64Neg" => F64Neg,
        "F64ConvertI32S" => F64ConvertI32S,
        "F64ConvertI32U" => F64ConvertI32U,
        "F64ConvertI64S" => F64ConvertI64S,
        "F64ConvertI64U" => F64ConvertI64U,
        _ => return None,
    })
}

/// Trapping conversions lowered through skeleton-owned guard helpers so the
/// trap is explicit in the IR: `(op, from, to, min, max)`; the value traps
/// unless it is not NaN and `min < v < max` (open bounds, matching wasm's
/// trapping-conversion ranges).
pub(crate) fn trapping_unop(name: &str) -> Option<(LeUnOp, LeType, LeType, f64, f64)> {
    use LeUnOp::*;
    Some(match name {
        "I32TruncF64S" => (I32TruncF64S, LeType::F64, LeType::I32, -2147483649.0, 2147483648.0),
        "I32TruncF64U" => (I32TruncF64U, LeType::F64, LeType::I32, -1.0, 4294967296.0),
        "I64TruncF64S" => (
            I64TruncF64S,
            LeType::F64,
            LeType::I64,
            -9223372036854775809.0,
            9223372036854775808.0,
        ),
        "I64TruncF64U" => (
            I64TruncF64U,
            LeType::F64,
            LeType::I64,
            -1.0,
            18446744073709551616.0,
        ),
        _ => return None,
    })
}

/// Shared heap-test classification for `ref_test`/`ref_cast`
/// implementations. Returns the cast mode and expected type id, mirroring
/// nano_mvp's scheme: mode 0 = exact type id, mode 1 = any struct
/// (`1..=struct_hi`), mode 2 = any array (`> struct_hi`), mode 3 = any
/// non-null non-funcref reference (`any`/`eq`; i31 passes), mode 4 = i31
/// (odd word).
pub(crate) fn heap_test_mode(
    plan: &BackendPlan,
    type_ids: &TypeIdTable,
    heap: &PlanHeapType,
) -> Result<(u32, u32), String> {
    match heap {
        PlanHeapType::Sig(sig) => {
            let id = type_ids
                .of(*sig)
                .ok_or_else(|| format!("signature {sig} is not an aggregate"))?;
            Ok((0, id))
        }
        PlanHeapType::Struct => Ok((1, type_ids.struct_count)),
        PlanHeapType::Array => Ok((2, type_ids.struct_count)),
        PlanHeapType::Any | PlanHeapType::Eq => Ok((3, 0)),
        PlanHeapType::I31 => Ok((4, 0)),
        PlanHeapType::FuncRef | PlanHeapType::Exn | PlanHeapType::NoExn => Err(format!(
            "heap type {heap:?} is outside the low-level cast surface"
        )),
        PlanHeapType::Bottom | PlanHeapType::NoExtern | PlanHeapType::NoFunc => {
            // Bottom tests are always false; only reachable via null.
            Ok((5, 0))
        }
        PlanHeapType::ExternRef => Err("externref is outside the low-level cast surface".into()),
    }
}
