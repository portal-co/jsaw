//! Stable, immutable, target-independent `BackendPlan` (DreamComp #17, #25).
//!
//! A `BackendPlan` is the shared pre-encoding representation consumed later by
//! the WASM MVP, C, Rust, and MiniBC emitters. It is derived from a typed
//! Waffle [`Module`] plus nano boundary metadata, but it never exposes raw
//! Waffle entity indices as stable artifact identity:
//!
//! - function IDs follow the module's logical function order
//!   (`funcs.entries()` traversal, i.e. source content order);
//! - block IDs follow a canonical reverse-postorder from the entry block,
//!   with unreachable blocks appended in source order;
//! - value IDs follow (block, parameter index, instruction index) order;
//! - signature IDs are structurally interned through a fixpoint over canonical
//!   shape strings, so they do not depend on Waffle arena allocation order or
//!   pointer identity.
//!
//! Planning is fail-closed: malformed CFG, layout, call, completion, or
//! boundary metadata produces structured [`BackendPlanDiagnostic`] values
//! rather than a partially valid plan. Planning the same module twice (and
//! regardless of internal map/set iteration order — only `Vec`/`BTreeMap`
//! structures are used) produces identical JSON and an identical SHA3-256
//! fingerprint.
//!
//! This module is an inspection/adapter seam only. It does not rewire
//! `emit_coregc` or any other backend, and jsaw exception values remain
//! ordinary typed aggregates here: no Wasm exception ABI is introduced, so a
//! `Throw` completion is representable in the schema but is recorded only by
//! later layers that own the jsaw adapter convention.

use portal_pc_waffle::{
    Block, EntityRef, ExportKind, Func, FuncDecl, FunctionBody, HeapType, ImportKind, Module,
    Operator, Signature, SignatureData, StorageType, Terminator, Type, Value, ValueDef,
};
use serde::{Deserialize, Serialize};
use sha3::{Digest, Sha3_256};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use crate::nano::{NanoLifetime, NanoValueRepresentation};

/// Schema identifier carried by every plan.
pub const BACKEND_PLAN_SCHEMA: &str = "jsaw.backend-plan.v1";
/// Domain separator for fingerprinting.
const FINGERPRINT_DOMAIN: &[u8] = b"portal-jsc-backend-plan\0";
/// Bound on the signature-canonicalization fixpoint. Recursive type groups
/// converge well before this; the bound only guarantees termination.
const SIGNATURE_FIXPOINT_ROUNDS: usize = 16;

macro_rules! plan_id {
    ($(#[$meta:meta])* $name:ident) => {
        $(#[$meta])*
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        pub struct $name(pub u32);

        impl $name {
            pub const fn index(self) -> usize {
                self.0 as usize
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

plan_id!(
    /// Canonical function identity: logical module function order.
    PlanFuncId
);
plan_id!(
    /// Canonical block identity within one function: RPO from entry.
    PlanBlockId
);
plan_id!(
    /// Canonical value identity within one function.
    PlanValueId
);
plan_id!(
    /// Interned canonical type identity.
    TypeId
);
plan_id!(
    /// Structurally interned signature identity.
    PlanSigId
);

/// Canonical heap type. `Sig` references the plan's structural signature
/// table rather than a Waffle entity index.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanHeapType {
    Sig(PlanSigId),
    FuncRef,
    ExternRef,
    Any,
    Eq,
    I31,
    Struct,
    Array,
    /// The abstract `none` heap type (bottom of the internal hierarchy).
    Bottom,
    NoExtern,
    NoFunc,
    Exn,
    NoExn,
}

/// Canonical, target-independent logical value type.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanType {
    I32,
    I64,
    F32,
    F64,
    V128,
    Ref { nullable: bool, heap: PlanHeapType },
}

/// Field storage for aggregate layouts.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanStorage {
    I8,
    I16,
    Val(TypeId),
}

/// One aggregate field: storage plus mutability.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct PlanField {
    pub storage: PlanStorage,
    pub mutable: bool,
}

/// A structurally interned signature. Structurally identical signatures share
/// one ID regardless of how many Waffle signature entities denoted them.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanSignature {
    Func {
        params: Vec<TypeId>,
        returns: Vec<TypeId>,
        shared: bool,
    },
    Struct {
        fields: Vec<PlanField>,
        shared: bool,
    },
    Array {
        element: PlanField,
        shared: bool,
    },
    Import {
        like: PlanHeapType,
        shared: bool,
    },
    None,
}

/// What a reference layout points at.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanRefTarget {
    Aggregate(PlanSigId),
    FuncSig(PlanSigId),
    Dynamic(PlanHeapType),
}

/// Per-value layout record. Layouts are logical: aggregate field order and
/// storage classes are preserved, but physical offsets belong to emitters.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanLayout {
    /// A zero-or-multi-result tuple value with no standalone type.
    Tuple { elements: Vec<TypeId> },
    /// A plain scalar; `bytes` is the logical width.
    Scalar { bytes: u8 },
    /// A struct/array aggregate handled through the logical arena.
    Aggregate { sig: PlanSigId },
    /// A reference value.
    Ref { nullable: bool, target: PlanRefTarget },
}

/// How one plan value came to be.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanValueDef {
    BlockParam { block: PlanBlockId, index: u32 },
    Operator { block: PlanBlockId, inst: u32 },
    PickOutput {
        block: PlanBlockId,
        inst: u32,
        source: PlanValueId,
        index: u32,
    },
    Alias { target: PlanValueId },
}

/// One canonical value with its representation and layout records.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanValue {
    pub ty: Option<TypeId>,
    pub def: PlanValueDef,
    pub representation: NanoValueRepresentation,
    pub lifetime: NanoLifetime,
    pub layout: PlanLayout,
}

/// Call identity for direct, indirect, and function-reference calls.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanCallKind {
    Direct { callee: PlanFuncId },
    Indirect { table: u32 },
    Ref,
}

/// A call site (instruction-level calls are never tail calls).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanCall {
    pub kind: PlanCallKind,
    pub signature: PlanSigId,
    pub tail: bool,
}

/// Logical arena operation classification for WasmGC instructions.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanArenaOp {
    Alloc,
    Read,
    Write,
    Grow,
    Len,
    TestCast,
    Eq,
}

/// A constant payload for constant operators. All payloads are bit-exact:
/// integers are stored as their unsigned bit pattern, floats as IEEE bits.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanConst {
    I32(u32),
    I64(u64),
    F32(u32),
    F64(u64),
}

/// One instruction in the flat block-parameter CFG.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanInst {
    /// Stable operator name (the Waffle variant name). Names are schema-tied
    /// to the waffle-ir version used to build the plan.
    pub operator: String,
    pub args: Vec<PlanValueId>,
    pub results: Vec<PlanValueId>,
    pub result_types: Vec<TypeId>,
    pub call: Option<PlanCall>,
    pub arena: Option<PlanArenaOp>,
    /// Constant payload when the operator is a scalar constant.
    pub const_value: Option<PlanConst>,
    /// Proven function identity when the operator is `ref.func`.
    pub ref_func: Option<PlanFuncId>,
}

/// A flat block-parameter edge.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanTarget {
    pub block: PlanBlockId,
    pub args: Vec<PlanValueId>,
}

/// Block terminator. `TailCall` covers direct, indirect, and `call_ref`
/// forms; `Trap` is an unconditional abnormal completion.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanTerminator {
    Br {
        target: PlanTarget,
    },
    CondBr {
        cond: PlanValueId,
        if_true: PlanTarget,
        if_false: PlanTarget,
    },
    Select {
        value: PlanValueId,
        targets: Vec<PlanTarget>,
        default: PlanTarget,
    },
    Return {
        values: Vec<PlanValueId>,
    },
    TailCall {
        call: PlanCallKind,
        signature: PlanSigId,
        args: Vec<PlanValueId>,
    },
    Trap,
    Ub,
}

/// One basic block in the flat block-parameter CFG.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanBlock {
    pub params: Vec<PlanValueId>,
    pub insts: Vec<PlanInst>,
    pub terminator: PlanTerminator,
}

/// Completion kinds. jsaw encodes exceptions as ordinary aggregate values, so
/// `Throw` is declared by higher layers; structural planning records `Return`
/// and `Trap`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CompletionKind {
    Return,
    Throw,
    Trap,
}

/// The completion set of a function, with the logical result arity.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompletionShape {
    pub kinds: Vec<CompletionKind>,
    pub results: Vec<TypeId>,
}

/// Import vs. defined function.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanFunctionKind {
    Import { module: String, name: String },
    Defined,
}

/// One canonical function.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanFunction {
    pub name: String,
    pub kind: PlanFunctionKind,
    pub signature: PlanSigId,
    pub entry: Option<PlanBlockId>,
    pub blocks: Vec<PlanBlock>,
    pub values: Vec<PlanValue>,
    pub completions: CompletionShape,
}

/// Canonical table record (`elements` uses `None` for null funcref slots).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanTable {
    pub element: TypeId,
    pub initial: u64,
    pub max: Option<u64>,
    pub table64: bool,
    pub elements: Option<Vec<Option<PlanFuncId>>>,
}

/// Canonical global record.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanGlobal {
    pub ty: TypeId,
    pub mutable: bool,
    pub init: Option<u64>,
}

/// Canonical memory record.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanMemory {
    pub initial_pages: u64,
    pub maximum_pages: Option<u64>,
    pub memory64: bool,
    pub shared: bool,
}

/// Boundary (host/import/export) direction.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BoundaryDirection {
    Import,
    Export,
}

/// What a boundary record binds.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BoundaryTarget {
    Func(PlanFuncId),
    Table(u32),
    Global(u32),
    Memory(u32),
    ControlTag(u32),
    Type(PlanSigId),
}

/// Function ABI at a boundary: logical params/results and completions.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BoundaryAbi {
    pub params: Vec<TypeId>,
    pub results: Vec<TypeId>,
    pub completions: CompletionShape,
}

/// One import/export boundary record.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BoundaryRecord {
    pub direction: BoundaryDirection,
    /// Importing module name (imports only).
    pub module: Option<String>,
    pub name: String,
    pub target: BoundaryTarget,
    pub abi: Option<BoundaryAbi>,
}

/// What a target runtime must provide to execute this plan.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuntimeRequirements {
    pub needs_arena: bool,
    pub needs_linear_memory: bool,
    pub needs_tables: bool,
    pub uses_indirect_calls: bool,
    pub uses_ref_calls: bool,
    pub uses_tail_calls: bool,
    pub uses_multi_result: bool,
    pub uses_simd: bool,
    pub uses_atomics: bool,
    pub may_trap: bool,
}

/// Structured, fail-closed diagnostic kinds.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DiagnosticKind {
    UnsupportedDecl,
    UnsupportedType,
    BlockParamArity,
    TypeMismatch,
    UndefinedValue,
    ValueOwnership,
    DefAfterUse,
    AliasCycle,
    InvalidTerminator,
    ReturnArity,
    CallShape,
    BoundaryAbi,
    SpaceExhausted,
}

fn diagnostic_rank(kind: DiagnosticKind) -> u8 {
    match kind {
        DiagnosticKind::UnsupportedDecl => 0,
        DiagnosticKind::UnsupportedType => 1,
        DiagnosticKind::BlockParamArity => 2,
        DiagnosticKind::TypeMismatch => 3,
        DiagnosticKind::UndefinedValue => 4,
        DiagnosticKind::ValueOwnership => 5,
        DiagnosticKind::DefAfterUse => 6,
        DiagnosticKind::AliasCycle => 7,
        DiagnosticKind::InvalidTerminator => 8,
        DiagnosticKind::ReturnArity => 9,
        DiagnosticKind::CallShape => 10,
        DiagnosticKind::BoundaryAbi => 11,
        DiagnosticKind::SpaceExhausted => 12,
    }
}

/// One structured planning diagnostic.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BackendPlanDiagnostic {
    pub kind: DiagnosticKind,
    pub function: Option<PlanFuncId>,
    pub block: Option<PlanBlockId>,
    pub inst: Option<u32>,
    pub message: String,
}

impl fmt::Display for BackendPlanDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}", self.kind, self.message)?;
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

/// The immutable, deterministic pre-encoding plan.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BackendPlan {
    pub schema: String,
    pub types: Vec<PlanType>,
    pub signatures: Vec<PlanSignature>,
    pub tables: Vec<PlanTable>,
    pub globals: Vec<PlanGlobal>,
    pub memories: Vec<PlanMemory>,
    pub functions: Vec<PlanFunction>,
    pub boundaries: Vec<BoundaryRecord>,
    pub runtime: RuntimeRequirements,
    pub start: Option<PlanFuncId>,
}

impl BackendPlan {
    /// Build and validate a plan from a typed Waffle module.
    ///
    /// The result is all-or-nothing: any diagnostic fails the whole plan.
    pub fn from_module(module: &Module<'_>) -> Result<Self, Vec<BackendPlanDiagnostic>> {
        let mut builder = Builder::new(module);
        builder.build()
    }

    /// Deterministic JSON serialization. The plan contains no maps, so key
    /// order is the declared struct order and identical plans serialize
    /// identically.
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).expect("BackendPlan serialization is infallible")
    }

    /// Domain-separated, length-delimited canonical bytes for fingerprinting.
    pub fn canonical_bytes(&self) -> Vec<u8> {
        let json = self.to_json();
        let mut bytes = Vec::with_capacity(FINGERPRINT_DOMAIN.len() + 8 + json.len());
        bytes.extend_from_slice(FINGERPRINT_DOMAIN);
        bytes.extend_from_slice(&(json.len() as u64).to_be_bytes());
        bytes.extend_from_slice(json.as_bytes());
        bytes
    }

    /// Lowercase hexadecimal SHA3-256 fingerprint of the canonical bytes.
    pub fn fingerprint(&self) -> String {
        let digest = Sha3_256::digest(self.canonical_bytes());
        digest.iter().map(|byte| format!("{byte:02x}")).collect()
    }
}

// ---------------------------------------------------------------------------
// Raw (session-local) signature shapes for structural interning.
//
// Raw shapes reference Waffle `Signature` entities; the fixpoint below turns
// them into allocation-order-independent canonical IDs.
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum RawHeap {
    Sig(Signature),
    FuncRef,
    ExternRef,
    Any,
    Eq,
    I31,
    Struct,
    Array,
    Bottom,
    NoExtern,
    NoFunc,
    Exn,
    NoExn,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum RawType {
    I32,
    I64,
    F32,
    F64,
    V128,
    Ref { nullable: bool, heap: RawHeap },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum RawStorage {
    I8,
    I16,
    Val(RawType),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct RawField {
    storage: RawStorage,
    mutable: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum RawSig {
    Func {
        params: Vec<RawType>,
        returns: Vec<RawType>,
        shared: bool,
    },
    Struct {
        fields: Vec<RawField>,
        shared: bool,
    },
    Array {
        element: RawField,
        shared: bool,
    },
    Import {
        like: RawHeap,
        shared: bool,
    },
    None,
}

fn raw_heap(heap: &HeapType) -> RawHeap {
    match heap {
        HeapType::FuncRef => RawHeap::FuncRef,
        HeapType::ExternRef => RawHeap::ExternRef,
        HeapType::Sig { sig_index } => RawHeap::Sig(*sig_index),
        HeapType::Any => RawHeap::Any,
        HeapType::Eq => RawHeap::Eq,
        HeapType::I31 => RawHeap::I31,
        HeapType::Struct => RawHeap::Struct,
        HeapType::Array => RawHeap::Array,
        HeapType::None => RawHeap::Bottom,
        HeapType::NoExtern => RawHeap::NoExtern,
        HeapType::NoFunc => RawHeap::NoFunc,
        HeapType::Exn => RawHeap::Exn,
        HeapType::NoExn => RawHeap::NoExn,
        // HeapType is non-exhaustive; unknown future kinds degrade to a
        // dynamic reference rather than leaking entity identity.
        _ => RawHeap::Any,
    }
}

fn raw_type(ty: &Type) -> RawType {
    match ty {
        Type::I32 => RawType::I32,
        Type::I64 => RawType::I64,
        Type::F32 => RawType::F32,
        Type::F64 => RawType::F64,
        Type::V128 => RawType::V128,
        Type::Heap(reference) => RawType::Ref {
            nullable: reference.nullable,
            heap: raw_heap(&reference.value),
        },
        // Type is non-exhaustive; degrade unknown future kinds to i32 and let
        // the signature consumer decide whether the plan is usable.
        _ => RawType::I32,
    }
}

fn raw_storage(storage: &StorageType) -> RawStorage {
    match storage {
        StorageType::I8 => RawStorage::I8,
        StorageType::I16 => RawStorage::I16,
        StorageType::Val(ty) => RawStorage::Val(raw_type(ty)),
        _ => RawStorage::I8,
    }
}

/// Encode a raw shape with nested signature references replaced by their
/// previous-round canonical strings (round zero renders them as `?`).
fn raw_sig_shape(sig: &RawSig, previous: Option<&BTreeMap<Signature, String>>) -> String {
    fn heap(heap: &RawHeap, previous: Option<&BTreeMap<Signature, String>>) -> String {
        match (heap, previous) {
            (RawHeap::Sig(signature), Some(round)) => format!(
                "sig({})",
                round.get(signature).map(String::as_str).unwrap_or("?")
            ),
            (RawHeap::Sig(_), None) => "sig(?)".to_owned(),
            (RawHeap::FuncRef, _) => "funcref".to_owned(),
            (RawHeap::ExternRef, _) => "externref".to_owned(),
            (RawHeap::Any, _) => "any".to_owned(),
            (RawHeap::Eq, _) => "eq".to_owned(),
            (RawHeap::I31, _) => "i31".to_owned(),
            (RawHeap::Struct, _) => "struct".to_owned(),
            (RawHeap::Array, _) => "array".to_owned(),
            (RawHeap::Bottom, _) => "none".to_owned(),
            (RawHeap::NoExtern, _) => "noextern".to_owned(),
            (RawHeap::NoFunc, _) => "nofunc".to_owned(),
            (RawHeap::Exn, _) => "exn".to_owned(),
            (RawHeap::NoExn, _) => "noexn".to_owned(),
        }
    }
    fn ty(ty: &RawType, previous: Option<&BTreeMap<Signature, String>>) -> String {
        match ty {
            RawType::I32 => "i32".to_owned(),
            RawType::I64 => "i64".to_owned(),
            RawType::F32 => "f32".to_owned(),
            RawType::F64 => "f64".to_owned(),
            RawType::V128 => "v128".to_owned(),
            RawType::Ref { nullable, heap: h } => {
                format!("ref:{}:{}", u8::from(*nullable), heap(h, previous))
            }
        }
    }
    fn field(field: &RawField, previous: Option<&BTreeMap<Signature, String>>) -> String {
        let storage = match &field.storage {
            RawStorage::I8 => "i8".to_owned(),
            RawStorage::I16 => "i16".to_owned(),
            RawStorage::Val(value) => ty(value, previous),
        };
        format!("{storage}:{}", u8::from(field.mutable))
    }
    match sig {
        RawSig::Func {
            params,
            returns,
            shared,
        } => format!(
            "func({})->({}):{}",
            params
                .iter()
                .map(|param| ty(param, previous))
                .collect::<Vec<_>>()
                .join(","),
            returns
                .iter()
                .map(|result| ty(result, previous))
                .collect::<Vec<_>>()
                .join(","),
            u8::from(*shared)
        ),
        RawSig::Struct { fields, shared } => format!(
            "struct({}):{}",
            fields
                .iter()
                .map(|entry| field(entry, previous))
                .collect::<Vec<_>>()
                .join(","),
            u8::from(*shared)
        ),
        RawSig::Array { element, shared } => {
            format!("array({}):{}", field(element, previous), u8::from(*shared))
        }
        RawSig::Import { like, shared } => {
            format!("import({}):{}", heap(like, previous), u8::from(*shared))
        }
        RawSig::None => "none".to_owned(),
    }
}

// ---------------------------------------------------------------------------
// Builder
// ---------------------------------------------------------------------------

struct Builder<'m> {
    module: &'m Module<'m>,
    diagnostics: Vec<BackendPlanDiagnostic>,
    types: BTreeMap<PlanType, TypeId>,
    type_list: Vec<PlanType>,
    sig_ids: BTreeMap<Signature, PlanSigId>,
    signatures: Vec<PlanSignature>,
    func_ids: BTreeMap<Func, PlanFuncId>,
    runtime: RuntimeRequirements,
}

impl<'m> Builder<'m> {
    fn new(module: &'m Module<'m>) -> Self {
        Self {
            module,
            diagnostics: Vec::new(),
            types: BTreeMap::new(),
            type_list: Vec::new(),
            sig_ids: BTreeMap::new(),
            signatures: Vec::new(),
            func_ids: BTreeMap::new(),
            runtime: RuntimeRequirements::default(),
        }
    }

    fn diagnostic(
        &mut self,
        kind: DiagnosticKind,
        function: Option<PlanFuncId>,
        block: Option<PlanBlockId>,
        inst: Option<u32>,
        message: impl Into<String>,
    ) {
        self.diagnostics.push(BackendPlanDiagnostic {
            kind,
            function,
            block,
            inst,
            message: message.into(),
        });
    }

    fn intern_type(&mut self, ty: PlanType) -> Option<TypeId> {
        if let Some(id) = self.types.get(&ty) {
            return Some(*id);
        }
        let Ok(index) = u32::try_from(self.type_list.len()) else {
            self.diagnostic(
                DiagnosticKind::SpaceExhausted,
                None,
                None,
                None,
                "canonical type space exhausted",
            );
            return None;
        };
        let id = TypeId(index);
        self.type_list.push(ty.clone());
        self.types.insert(ty, id);
        Some(id)
    }

    /// Intern all module signatures structurally, independent of Waffle
    /// allocation order, via a bounded fixpoint over canonical shape strings.
    fn intern_signatures(&mut self) {
        let mut raws: Vec<(Signature, RawSig)> = Vec::new();
        for (signature, data) in self.module.signatures.entries() {
            let raw = match data {
                SignatureData::Func {
                    params,
                    returns,
                    shared,
                } => RawSig::Func {
                    params: params.iter().map(raw_type).collect(),
                    returns: returns.iter().map(raw_type).collect(),
                    shared: *shared,
                },
                SignatureData::Struct { fields, shared } => RawSig::Struct {
                    fields: fields
                        .iter()
                        .map(|field| RawField {
                            storage: raw_storage(&field.value),
                            mutable: field.mutable,
                        })
                        .collect(),
                    shared: *shared,
                },
                SignatureData::Array { ty, shared } => RawSig::Array {
                    element: RawField {
                        storage: raw_storage(&ty.value),
                        mutable: ty.mutable,
                    },
                    shared: *shared,
                },
                SignatureData::Import { like, shared } => RawSig::Import {
                    like: raw_heap(like),
                    shared: *shared,
                },
                SignatureData::None => RawSig::None,
                _ => RawSig::None,
            };
            raws.push((signature, raw));
        }

        // Round zero uses `?` for nested references; each later round
        // substitutes the previous round's strings. Recursive groups converge;
        // the round bound only guarantees termination.
        let mut current: BTreeMap<Signature, String> = raws
            .iter()
            .map(|(signature, raw)| (*signature, raw_sig_shape(raw, None)))
            .collect();
        for _ in 0..SIGNATURE_FIXPOINT_ROUNDS {
            let next: BTreeMap<Signature, String> = raws
                .iter()
                .map(|(signature, raw)| (*signature, raw_sig_shape(raw, Some(&current))))
                .collect();
            if next == current {
                break;
            }
            current = next;
        }

        // Structural interning: identical final shapes share one ID. Recursive
        // twin groups merge into a single self-referential record, which is
        // the correct equirecursive reading.
        let mut shapes: Vec<(&String, Signature)> = current
            .iter()
            .map(|(signature, shape)| (shape, *signature))
            .collect();
        shapes.sort();
        let mut shape_ids: BTreeMap<&String, PlanSigId> = BTreeMap::new();
        for (shape, _) in &shapes {
            if shape_ids.contains_key(*shape) {
                continue;
            }
            let Ok(index) = u32::try_from(self.signatures.len()) else {
                self.diagnostic(
                    DiagnosticKind::SpaceExhausted,
                    None,
                    None,
                    None,
                    "canonical signature space exhausted",
                );
                return;
            };
            let id = PlanSigId(index);
            self.signatures.push(PlanSignature::None);
            shape_ids.insert(*shape, id);
        }
        for (shape, signature) in shapes {
            if let Some(id) = shape_ids.get(shape) {
                self.sig_ids.insert(signature, *id);
            }
        }

        // Materialize interned signatures in canonical ID order.
        let mut materialized: Vec<(PlanSigId, RawSig)> = Vec::new();
        for (signature, raw) in &raws {
            let Some(id) = self.sig_ids.get(signature).copied() else {
                continue;
            };
            if materialized.iter().any(|(existing, _)| *existing == id) {
                continue;
            }
            materialized.push((id, raw.clone()));
        }
        materialized.sort_by_key(|(id, _)| *id);
        for (id, raw) in materialized {
            let converted = self.convert_signature(&raw);
            let slot = id.index();
            if slot < self.signatures.len() {
                self.signatures[slot] = converted;
            }
        }
    }

    fn convert_heap(&self, heap: &RawHeap) -> PlanHeapType {
        match heap {
            RawHeap::Sig(signature) => PlanHeapType::Sig(
                self.sig_ids
                    .get(signature)
                    .copied()
                    .unwrap_or(PlanSigId(u32::MAX)),
            ),
            RawHeap::FuncRef => PlanHeapType::FuncRef,
            RawHeap::ExternRef => PlanHeapType::ExternRef,
            RawHeap::Any => PlanHeapType::Any,
            RawHeap::Eq => PlanHeapType::Eq,
            RawHeap::I31 => PlanHeapType::I31,
            RawHeap::Struct => PlanHeapType::Struct,
            RawHeap::Array => PlanHeapType::Array,
            RawHeap::Bottom => PlanHeapType::Bottom,
            RawHeap::NoExtern => PlanHeapType::NoExtern,
            RawHeap::NoFunc => PlanHeapType::NoFunc,
            RawHeap::Exn => PlanHeapType::Exn,
            RawHeap::NoExn => PlanHeapType::NoExn,
        }
    }

    fn convert_type(&mut self, ty: &RawType) -> Option<TypeId> {
        let plan = match ty {
            RawType::I32 => PlanType::I32,
            RawType::I64 => PlanType::I64,
            RawType::F32 => PlanType::F32,
            RawType::F64 => PlanType::F64,
            RawType::V128 => PlanType::V128,
            RawType::Ref { nullable, heap } => PlanType::Ref {
                nullable: *nullable,
                heap: self.convert_heap(heap),
            },
        };
        self.intern_type(plan)
    }

    fn convert_waffle_type(&mut self, ty: &Type) -> Option<TypeId> {
        self.convert_type(&raw_type(ty))
    }

    fn convert_storage(&mut self, storage: &RawStorage) -> Option<PlanStorage> {
        Some(match storage {
            RawStorage::I8 => PlanStorage::I8,
            RawStorage::I16 => PlanStorage::I16,
            RawStorage::Val(ty) => PlanStorage::Val(self.convert_type(ty)?),
        })
    }

    fn convert_signature(&mut self, raw: &RawSig) -> PlanSignature {
        match raw {
            RawSig::Func {
                params,
                returns,
                shared,
            } => PlanSignature::Func {
                params: params
                    .iter()
                    .map(|param| self.convert_type(param))
                    .collect::<Option<Vec<_>>>()
                    .unwrap_or_default(),
                returns: returns
                    .iter()
                    .map(|result| self.convert_type(result))
                    .collect::<Option<Vec<_>>>()
                    .unwrap_or_default(),
                shared: *shared,
            },
            RawSig::Struct { fields, shared } => PlanSignature::Struct {
                fields: fields
                    .iter()
                    .map(|field| {
                        Some(PlanField {
                            storage: self.convert_storage(&field.storage)?,
                            mutable: field.mutable,
                        })
                    })
                    .collect::<Option<Vec<_>>>()
                    .unwrap_or_default(),
                shared: *shared,
            },
            RawSig::Array { element, shared } => PlanSignature::Array {
                element: PlanField {
                    storage: self
                        .convert_storage(&element.storage)
                        .unwrap_or(PlanStorage::I8),
                    mutable: element.mutable,
                },
                shared: *shared,
            },
            RawSig::Import { like, shared } => PlanSignature::Import {
                like: self.convert_heap(like),
                shared: *shared,
            },
            RawSig::None => PlanSignature::None,
        }
    }

    fn signature_of_func(&self, func: Func) -> Option<Signature> {
        let decl = self.module.funcs.get(func)?;
        match decl {
            FuncDecl::Import(signature, _) | FuncDecl::Body(signature, _, _) => Some(*signature),
            _ => None,
        }
    }

    /// Parameter and result types of a function signature, interned into this
    /// plan's canonical type table.
    fn signature_types(&self, signature: Signature) -> Option<(Vec<RawType>, Vec<RawType>)> {
        let data = self.module.signatures.get(signature)?;
        let SignatureData::Func {
            params, returns, ..
        } = data
        else {
            return None;
        };
        Some((
            params.iter().map(raw_type).collect(),
            returns.iter().map(raw_type).collect(),
        ))
    }

    fn func_signature_types(&mut self, func: Func) -> Option<(Vec<TypeId>, Vec<TypeId>)> {
        let signature = self.signature_of_func(func)?;
        let (params, returns) = self.signature_types(signature)?;
        let params = params
            .iter()
            .map(|param| self.convert_type(param))
            .collect::<Option<Vec<_>>>()?;
        let returns = returns
            .iter()
            .map(|result| self.convert_type(result))
            .collect::<Option<Vec<_>>>()?;
        Some((params, returns))
    }

    fn signature_returns(&mut self, signature: Signature) -> Vec<TypeId> {
        let Some((_, returns)) = self.signature_types(signature) else {
            return Vec::new();
        };
        returns
            .iter()
            .map(|result| self.convert_type(result))
            .collect::<Option<Vec<_>>>()
            .unwrap_or_default()
    }

    fn build(&mut self) -> Result<BackendPlan, Vec<BackendPlanDiagnostic>> {
        self.intern_signatures();

        // Canonical function IDs: logical module function order.
        for (func, _) in self.module.funcs.entries() {
            let Ok(index) = u32::try_from(self.func_ids.len()) else {
                self.diagnostic(
                    DiagnosticKind::SpaceExhausted,
                    None,
                    None,
                    None,
                    "canonical function space exhausted",
                );
                break;
            };
            self.func_ids.insert(func, PlanFuncId(index));
        }

        // Tables, globals, memories (content order).
        let mut tables = Vec::new();
        for (_, table) in self.module.tables.entries() {
            let element = self.convert_waffle_type(&table.ty);
            let elements = table.func_elements.as_ref().map(|elements| {
                elements
                    .iter()
                    .map(|func| {
                        if *func == Func::invalid() {
                            None
                        } else {
                            self.func_ids.get(func).copied()
                        }
                    })
                    .collect::<Vec<_>>()
            });
            let Some(element) = element else {
                continue;
            };
            tables.push(PlanTable {
                element,
                initial: table.initial,
                max: table.max,
                table64: table.table64,
                elements,
            });
            self.runtime.needs_tables = true;
        }
        let mut globals = Vec::new();
        for (_, global) in self.module.globals.entries() {
            let Some(ty) = self.convert_waffle_type(&global.ty) else {
                continue;
            };
            globals.push(PlanGlobal {
                ty,
                mutable: global.mutable,
                init: global.value,
            });
        }
        let mut memories = Vec::new();
        for (_, memory) in self.module.memories.entries() {
            memories.push(PlanMemory {
                initial_pages: memory.initial_pages as u64,
                maximum_pages: memory.maximum_pages.map(|pages| pages as u64),
                memory64: memory.memory64,
                shared: memory.shared,
            });
            self.runtime.needs_linear_memory = true;
        }

        // Import module/name lookup for function imports.
        let mut func_import_names: BTreeMap<Func, (String, String)> = BTreeMap::new();
        for import in &self.module.imports {
            if let ImportKind::Func(func) = import.kind {
                func_import_names
                    .entry(func)
                    .or_insert_with(|| (import.module.clone(), import.name.clone()));
            }
        }

        // Functions.
        let mut functions = Vec::new();
        for (func, decl) in self.module.funcs.entries() {
            let id = self.func_ids[&func];
            let planned = self.plan_function(func, id, decl, &func_import_names);
            functions.push(planned);
        }

        // Start function shape.
        let mut start = None;
        if let Some(start_func) = self.module.start_func {
            match self.func_ids.get(&start_func).copied() {
                Some(id) => {
                    if let Some((params, returns)) = self.func_signature_types(start_func) {
                        if !params.is_empty() || !returns.is_empty() {
                            self.diagnostic(
                                DiagnosticKind::BoundaryAbi,
                                Some(id),
                                None,
                                None,
                                "start function must have no parameters and no results",
                            );
                        }
                    }
                    start = Some(id);
                }
                None => self.diagnostic(
                    DiagnosticKind::BoundaryAbi,
                    None,
                    None,
                    None,
                    format!(
                        "start function entity {} does not exist",
                        start_func.index()
                    ),
                ),
            }
        }

        // Boundaries.
        let boundaries = self.plan_boundaries(&functions);

        // Runtime requirement aggregation.
        for function in &functions {
            if function.completions.results.len() > 1 {
                self.runtime.uses_multi_result = true;
            }
            for value in &function.values {
                if value.representation == NanoValueRepresentation::ArenaRef {
                    self.runtime.needs_arena = true;
                }
                if value.ty.is_none() {
                    self.runtime.uses_multi_result = true;
                }
            }
            for block in &function.blocks {
                for inst in &block.insts {
                    if inst.result_types.len() > 1 {
                        self.runtime.uses_multi_result = true;
                    }
                    let name = inst.operator.as_str();
                    if operator_uses_simd(name) {
                        self.runtime.uses_simd = true;
                    }
                    if operator_uses_atomics(name) {
                        self.runtime.uses_atomics = true;
                    }
                    if operator_may_trap(name) {
                        self.runtime.may_trap = true;
                    }
                    if let Some(call) = &inst.call {
                        match call.kind {
                            PlanCallKind::Indirect { .. } => {
                                self.runtime.uses_indirect_calls = true;
                                self.runtime.needs_tables = true;
                            }
                            PlanCallKind::Ref => self.runtime.uses_ref_calls = true,
                            PlanCallKind::Direct { .. } => {}
                        }
                    }
                    if inst.arena.is_some() {
                        self.runtime.needs_arena = true;
                    }
                }
                match &block.terminator {
                    PlanTerminator::TailCall { call, .. } => {
                        self.runtime.uses_tail_calls = true;
                        self.runtime.may_trap = true;
                        match call {
                            PlanCallKind::Indirect { .. } => {
                                self.runtime.uses_indirect_calls = true;
                                self.runtime.needs_tables = true;
                            }
                            PlanCallKind::Ref => self.runtime.uses_ref_calls = true,
                            PlanCallKind::Direct { .. } => {}
                        }
                    }
                    PlanTerminator::Trap => self.runtime.may_trap = true,
                    _ => {}
                }
            }
        }

        self.diagnostics.sort_by(|left, right| {
            (
                left.function,
                left.block,
                left.inst,
                diagnostic_rank(left.kind),
                &left.message,
            )
                .cmp(&(
                    right.function,
                    right.block,
                    right.inst,
                    diagnostic_rank(right.kind),
                    &right.message,
                ))
        });
        if !self.diagnostics.is_empty() {
            return Err(self.diagnostics.clone());
        }
        Ok(BackendPlan {
            schema: BACKEND_PLAN_SCHEMA.to_owned(),
            types: self.type_list.clone(),
            signatures: self.signatures.clone(),
            tables,
            globals,
            memories,
            functions,
            boundaries,
            runtime: self.runtime.clone(),
            start,
        })
    }

    fn plan_function(
        &mut self,
        func: Func,
        id: PlanFuncId,
        decl: &FuncDecl<'_>,
        import_names: &BTreeMap<Func, (String, String)>,
    ) -> PlanFunction {
        match decl {
            FuncDecl::Import(signature, name) => {
                let (module_name, import_name) = import_names
                    .get(&func)
                    .cloned()
                    .unwrap_or_else(|| (String::new(), name.clone()));
                let results = self.signature_returns(*signature);
                PlanFunction {
                    name: name.clone(),
                    kind: PlanFunctionKind::Import {
                        module: module_name,
                        name: import_name,
                    },
                    signature: self
                        .sig_ids
                        .get(signature)
                        .copied()
                        .unwrap_or(PlanSigId(u32::MAX)),
                    entry: None,
                    blocks: Vec::new(),
                    values: Vec::new(),
                    completions: CompletionShape {
                        kinds: vec![CompletionKind::Return],
                        results,
                    },
                }
            }
            FuncDecl::Body(signature, name, body) => {
                let results = self.signature_returns(*signature);
                self.check_body_signature(body, *signature, id);
                let (blocks, values, completions) = self.plan_body(id, body);
                PlanFunction {
                    name: name.clone(),
                    kind: PlanFunctionKind::Defined,
                    signature: self
                        .sig_ids
                        .get(signature)
                        .copied()
                        .unwrap_or(PlanSigId(u32::MAX)),
                    entry: if blocks.is_empty() {
                        None
                    } else {
                        Some(PlanBlockId(0))
                    },
                    blocks,
                    values,
                    completions: CompletionShape {
                        kinds: completions,
                        results,
                    },
                }
            }
            unsupported => {
                self.diagnostic(
                    DiagnosticKind::UnsupportedDecl,
                    Some(id),
                    None,
                    None,
                    format!(
                        "function {} ({}) has a declaration form the plan cannot consume",
                        func.index(),
                        unsupported.name(),
                    ),
                );
                PlanFunction {
                    name: unsupported.name().to_owned(),
                    kind: PlanFunctionKind::Defined,
                    signature: PlanSigId(u32::MAX),
                    entry: None,
                    blocks: Vec::new(),
                    values: Vec::new(),
                    completions: CompletionShape {
                        kinds: vec![CompletionKind::Return],
                        results: Vec::new(),
                    },
                }
            }
        }
    }

    fn check_body_signature(&mut self, body: &FunctionBody, signature: Signature, id: PlanFuncId) {
        let Some(data) = self.module.signatures.get(signature) else {
            self.diagnostic(
                DiagnosticKind::UnsupportedDecl,
                Some(id),
                None,
                None,
                "function signature entity does not exist",
            );
            return;
        };
        let SignatureData::Func {
            params, returns, ..
        } = data
        else {
            self.diagnostic(
                DiagnosticKind::UnsupportedDecl,
                Some(id),
                None,
                None,
                "function signature is not a function signature",
            );
            return;
        };
        if params.len() != body.n_params {
            self.diagnostic(
                DiagnosticKind::CallShape,
                Some(id),
                None,
                None,
                format!(
                    "body has {} parameters but signature declares {}",
                    body.n_params,
                    params.len()
                ),
            );
        }
        let declared: Vec<RawType> = returns.iter().map(raw_type).collect();
        let actual: Vec<RawType> = body.rets.iter().map(raw_type).collect();
        if declared != actual {
            self.diagnostic(
                DiagnosticKind::ReturnArity,
                Some(id),
                None,
                None,
                "body result types do not match the signature",
            );
        }
    }

    fn plan_body(
        &mut self,
        func: PlanFuncId,
        body: &FunctionBody,
    ) -> (Vec<PlanBlock>, Vec<PlanValue>, Vec<CompletionKind>) {
        let order = canonical_block_order(body);
        let mut block_ids: BTreeMap<Block, PlanBlockId> = BTreeMap::new();
        for (index, block) in order.iter().enumerate() {
            block_ids.insert(*block, PlanBlockId(index as u32));
        }

        // Canonical value IDs: block params first (in canonical block order),
        // then instruction results per block.
        let mut value_ids: BTreeMap<Value, PlanValueId> = BTreeMap::new();
        for block in &order {
            for (_, param) in &body.blocks[*block].params {
                let next = value_ids.len() as u32;
                value_ids.entry(*param).or_insert(PlanValueId(next));
            }
        }
        for block in &order {
            for record in &body.blocks[*block].insts {
                if let Some(value) = record.pure_core() {
                    let next = value_ids.len() as u32;
                    value_ids.entry(value).or_insert(PlanValueId(next));
                }
            }
        }

        let value_types = self.resolve_value_types(func, body, &value_ids);

        // Definition positions for def-before-use validation: block params sit
        // at position -1, instructions at their list index, terminator uses at
        // one past the last instruction.
        let mut def_pos: BTreeMap<Value, (u32, i64)> = BTreeMap::new();
        for (block_index, block) in order.iter().enumerate() {
            for (_, param) in &body.blocks[*block].params {
                def_pos.insert(*param, (block_index as u32, -1));
            }
            for (inst_index, record) in body.blocks[*block].insts.iter().enumerate() {
                if let Some(value) = record.pure_core() {
                    def_pos.insert(value, (block_index as u32, inst_index as i64));
                }
            }
        }
        let dominators = compute_dominators(body, &order);

        // Build values with representation and layout records.
        let mut values: Vec<PlanValue> = Vec::new();
        let mut ordered_values: Vec<(PlanValueId, Value)> = value_ids
            .iter()
            .map(|(value, id)| (*id, *value))
            .collect();
        ordered_values.sort_by_key(|(id, _)| *id);
        for (plan_id, value) in &ordered_values {
            let def = &body.values[*value];
            let (plan_def, ty) = match def {
                ValueDef::BlockParam(block, index, def_ty) => {
                    let block = block_ids
                        .get(block)
                        .copied()
                        .unwrap_or(PlanBlockId(u32::MAX));
                    (
                        PlanValueDef::BlockParam {
                            block,
                            index: *index,
                        },
                        self.convert_waffle_type(def_ty),
                    )
                }
                ValueDef::Operator(_, _, _) => {
                    let (block, inst) = def_pos
                        .get(value)
                        .copied()
                        .map(|(block, inst)| (PlanBlockId(block), inst.max(0) as u32))
                        .unwrap_or((PlanBlockId(u32::MAX), u32::MAX));
                    (
                        PlanValueDef::Operator { block, inst },
                        value_types.get(value).copied().flatten(),
                    )
                }
                ValueDef::PickOutput(source, index, def_ty) => {
                    let (block, inst) = def_pos
                        .get(value)
                        .copied()
                        .map(|(block, inst)| (PlanBlockId(block), inst.max(0) as u32))
                        .unwrap_or((PlanBlockId(u32::MAX), u32::MAX));
                    let source = value_ids
                        .get(source)
                        .copied()
                        .unwrap_or(PlanValueId(u32::MAX));
                    (
                        PlanValueDef::PickOutput {
                            block,
                            inst,
                            source,
                            index: *index,
                        },
                        self.convert_waffle_type(def_ty),
                    )
                }
                ValueDef::Alias(target) => {
                    let target = value_ids
                        .get(target)
                        .copied()
                        .unwrap_or(PlanValueId(u32::MAX));
                    (
                        PlanValueDef::Alias { target },
                        value_types.get(value).copied().flatten(),
                    )
                }
                ValueDef::Placeholder(_) | ValueDef::None => {
                    self.diagnostic(
                        DiagnosticKind::UndefinedValue,
                        Some(func),
                        None,
                        None,
                        format!("value {} is an unresolved placeholder", value.index()),
                    );
                    (
                        PlanValueDef::Alias {
                            target: PlanValueId(u32::MAX),
                        },
                        None,
                    )
                }
            };
            let layout = self.layout_for(ty, def);
            let representation = representation_for(ty.map(|id| self.type_list[id.index()].clone()), &layout);
            let _ = plan_id;
            values.push(PlanValue {
                ty,
                def: plan_def,
                representation,
                // The plan is static: arena-resident values live for the
                // artifact instance, everything else is call-lived. Boundary
                // pinning is expressed by the boundary ABI records.
                lifetime: if representation == NanoValueRepresentation::ArenaRef {
                    NanoLifetime::Artifact
                } else {
                    NanoLifetime::Call
                },
                layout,
            });
        }

        // Build blocks (instructions + terminators), collecting uses for
        // def-before-use validation.
        let mut uses: Vec<(u32, i64, Value)> = Vec::new();
        let mut blocks: Vec<PlanBlock> = Vec::new();
        let mut completion_kinds: BTreeSet<CompletionKind> = BTreeSet::new();
        for (block_index, block) in order.iter().enumerate() {
            let plan_block_id = PlanBlockId(block_index as u32);
            let def = &body.blocks[*block];

            let mut params = Vec::new();
            for (param_index, (_, param_value)) in def.params.iter().enumerate() {
                if let Some(id) = value_ids.get(param_value).copied() {
                    params.push(id);
                }
                match body.values.get(*param_value) {
                    Some(ValueDef::BlockParam(owner, index, _)) => {
                        if *owner != *block || *index as usize != param_index {
                            self.diagnostic(
                                DiagnosticKind::ValueOwnership,
                                Some(func),
                                Some(plan_block_id),
                                None,
                                format!(
                                    "block parameter {} is registered for block {} index {}",
                                    param_value.index(),
                                    owner.index(),
                                    index
                                ),
                            );
                        }
                    }
                    _ => self.diagnostic(
                        DiagnosticKind::ValueOwnership,
                        Some(func),
                        Some(plan_block_id),
                        None,
                        format!(
                            "block parameter {} is not a block parameter definition",
                            param_value.index()
                        ),
                    ),
                }
            }

            let mut insts = Vec::new();
            for (inst_index, record) in def.insts.iter().enumerate() {
                let Some(value) = record.pure_core() else {
                    continue;
                };
                let inst = self.plan_inst(
                    func,
                    plan_block_id,
                    inst_index as u32,
                    body,
                    value,
                    &value_ids,
                    &mut uses,
                );
                // Ownership: instruction results must be placed in this block.
                if body.value_blocks[value] != *block {
                    self.diagnostic(
                        DiagnosticKind::ValueOwnership,
                        Some(func),
                        Some(plan_block_id),
                        Some(inst_index as u32),
                        format!(
                            "instruction result {} is owned by block {} but listed in block {}",
                            value.index(),
                            body.value_blocks[value].index(),
                            block.index()
                        ),
                    );
                }
                insts.push(inst);
            }

            let terminator = self.plan_terminator(
                func,
                plan_block_id,
                body,
                *block,
                &block_ids,
                &value_ids,
                &mut uses,
                &mut completion_kinds,
            );
            blocks.push(PlanBlock {
                params,
                insts,
                terminator,
            });
        }

        // Predecessor argument validation: arity and types vs block params.
        for (block_index, block) in order.iter().enumerate() {
            let plan_block_id = PlanBlockId(block_index as u32);
            let def = &body.blocks[*block];
            for pred in &def.preds {
                let Some(pred_id) = block_ids.get(pred).copied() else {
                    continue;
                };
                let Some(terminator) = body.blocks[*pred].terminator.pure_core() else {
                    continue;
                };
                let mut targets = Vec::new();
                terminator.visit_targets(|target| targets.push(target.clone()));
                for target in targets {
                    if target.block != *block {
                        continue;
                    }
                    if target.args.len() != def.params.len() {
                        self.diagnostic(
                            DiagnosticKind::BlockParamArity,
                            Some(func),
                            Some(pred_id),
                            None,
                            format!(
                                "predecessor passes {} arguments to block {} with {} parameters",
                                target.args.len(),
                                plan_block_id,
                                def.params.len()
                            ),
                        );
                        continue;
                    }
                    for (arg, (param_ty, _)) in target.args.iter().zip(def.params.iter()) {
                        let arg_ty = value_types
                            .get(arg)
                            .copied()
                            .flatten()
                            .and_then(|id| self.type_list.get(id.index()).cloned());
                        let param_plan = self.convert_waffle_type(param_ty);
                        let matches = match (arg_ty, param_plan) {
                            (Some(arg_ty), Some(param_plan)) => {
                                self.type_list.get(param_plan.index()) == Some(&arg_ty)
                            }
                            _ => true, // unknown types are diagnosed elsewhere
                        };
                        if !matches {
                            self.diagnostic(
                                DiagnosticKind::TypeMismatch,
                                Some(func),
                                Some(pred_id),
                                None,
                                format!(
                                    "predecessor argument type does not match parameter type of block {}",
                                    plan_block_id
                                ),
                            );
                        }
                    }
                }
            }
        }

        // Def-before-use validation (dominance-based).
        for (block_order, position, value) in uses {
            let Some(&(def_block, def_position)) = def_pos.get(&value) else {
                self.diagnostic(
                    DiagnosticKind::UndefinedValue,
                    Some(func),
                    Some(PlanBlockId(block_order)),
                    None,
                    format!("value {} is used but never defined", value.index()),
                );
                continue;
            };
            let defined = if def_block == block_order {
                def_position < position
            } else {
                dominators[block_order as usize].contains(&def_block)
            };
            if !defined {
                self.diagnostic(
                    DiagnosticKind::DefAfterUse,
                    Some(func),
                    Some(PlanBlockId(block_order)),
                    None,
                    format!(
                        "value {} is used in block {} before its definition in block {}",
                        value.index(),
                        block_order,
                        def_block
                    ),
                );
            }
        }

        (blocks, values, completion_kinds.into_iter().collect())
    }

    fn resolve_value_types(
        &mut self,
        func: PlanFuncId,
        body: &FunctionBody,
        value_ids: &BTreeMap<Value, PlanValueId>,
    ) -> BTreeMap<Value, Option<TypeId>> {
        let mut result: BTreeMap<Value, Option<TypeId>> = BTreeMap::new();
        for value in value_ids.keys() {
            let mut seen: BTreeSet<Value> = BTreeSet::new();
            let mut current = *value;
            let mut cycle = false;
            let resolved = loop {
                if !seen.insert(current) {
                    cycle = true;
                    break None;
                }
                match body.values.get(current) {
                    Some(ValueDef::Alias(target)) => current = *target,
                    Some(def) => {
                        let tys = def.tys(&body.type_pool);
                        if tys.len() == 1 {
                            break Some(tys[0]);
                        }
                        break None;
                    }
                    None => break None,
                }
            };
            if cycle {
                self.diagnostic(
                    DiagnosticKind::AliasCycle,
                    Some(func),
                    None,
                    None,
                    format!("value {} is part of an alias cycle", current.index()),
                );
            }
            let converted = match resolved {
                Some(ty) => self.convert_waffle_type(&ty),
                None => None,
            };
            result.insert(*value, converted);
        }
        result
    }

    #[allow(clippy::too_many_arguments)]
    fn plan_inst(
        &mut self,
        func: PlanFuncId,
        block: PlanBlockId,
        inst_index: u32,
        body: &FunctionBody,
        value: Value,
        value_ids: &BTreeMap<Value, PlanValueId>,
        uses: &mut Vec<(u32, i64, Value)>,
    ) -> PlanInst {
        let def = &body.values[value];
        let mut args: Vec<PlanValueId> = Vec::new();
        let mut call = None;
        let mut arena = None;
        let mut result_types = Vec::new();
        let mut results = Vec::new();
        let mut const_value = None;
        let mut ref_func = None;
        let operator;

        match def {
            ValueDef::Operator(op, arg_list, ty_list) => {
                operator = operator_name(op);
                let raw_args: Vec<Value> = body.arg_pool[*arg_list].to_vec();
                for arg in raw_args {
                    uses.push((block.0, inst_index as i64, arg));
                    match value_ids.get(&arg) {
                        Some(id) => args.push(*id),
                        None => {
                            self.diagnostic(
                                DiagnosticKind::UndefinedValue,
                                Some(func),
                                Some(block),
                                Some(inst_index),
                                format!(
                                    "operator argument value {} is not defined in this function",
                                    arg.index()
                                ),
                            );
                            args.push(PlanValueId(u32::MAX));
                        }
                    }
                }
                let raw_result_types: Vec<Type> = body.type_pool[*ty_list].to_vec();
                for ty in &raw_result_types {
                    if let Some(id) = self.convert_waffle_type(ty) {
                        result_types.push(id);
                    }
                }
                if let Some(id) = value_ids.get(&value) {
                    results.push(*id);
                }
                arena = arena_op(&operator);
                const_value = match op {
                    Operator::I32Const { value } => Some(PlanConst::I32(*value)),
                    Operator::I64Const { value } => Some(PlanConst::I64(*value)),
                    Operator::F32Const { value } => Some(PlanConst::F32(*value)),
                    Operator::F64Const { value } => Some(PlanConst::F64(*value)),
                    _ => None,
                };
                ref_func = match op {
                    Operator::RefFunc { func_index } => {
                        self.func_ids.get(func_index).copied()
                    }
                    _ => None,
                };
                call = self.plan_call_operator(
                    func,
                    block,
                    inst_index,
                    op,
                    args.len(),
                    raw_result_types.len(),
                );
            }
            ValueDef::PickOutput(source, index, _) => {
                operator = "pick_output".to_owned();
                uses.push((block.0, inst_index as i64, *source));
                match value_ids.get(source) {
                    Some(id) => args.push(*id),
                    None => {
                        self.diagnostic(
                            DiagnosticKind::UndefinedValue,
                            Some(func),
                            Some(block),
                            Some(inst_index),
                            format!(
                                "pick_output source value {} is not defined in this function",
                                source.index()
                            ),
                        );
                        args.push(PlanValueId(u32::MAX));
                    }
                }
                // Validate the projection shape.
                match body.values.get(*source) {
                    Some(ValueDef::Operator(_, _, source_tys)) => {
                        let arity = body.type_pool[*source_tys].len();
                        if arity < 2 {
                            self.diagnostic(
                                DiagnosticKind::CallShape,
                                Some(func),
                                Some(block),
                                Some(inst_index),
                                "pick_output source is not a multi-result operator",
                            );
                        } else if *index as usize >= arity {
                            self.diagnostic(
                                DiagnosticKind::CallShape,
                                Some(func),
                                Some(block),
                                Some(inst_index),
                                format!("pick_output index {index} exceeds source arity {arity}"),
                            );
                        }
                    }
                    _ => self.diagnostic(
                        DiagnosticKind::CallShape,
                        Some(func),
                        Some(block),
                        Some(inst_index),
                        "pick_output source is not an operator value",
                    ),
                }
                if let Some(id) = value_ids.get(&value) {
                    results.push(*id);
                }
                if let Some(ty) = def.ty(&body.type_pool) {
                    if let Some(id) = self.convert_waffle_type(&ty) {
                        result_types.push(id);
                    }
                }
            }
            ValueDef::Alias(target) => {
                operator = "alias".to_owned();
                uses.push((block.0, inst_index as i64, *target));
                match value_ids.get(target) {
                    Some(id) => args.push(*id),
                    None => {
                        self.diagnostic(
                            DiagnosticKind::UndefinedValue,
                            Some(func),
                            Some(block),
                            Some(inst_index),
                            format!(
                                "alias target value {} is not defined in this function",
                                target.index()
                            ),
                        );
                        args.push(PlanValueId(u32::MAX));
                    }
                }
                if let Some(id) = value_ids.get(&value) {
                    results.push(*id);
                }
            }
            ValueDef::Placeholder(_) | ValueDef::None => {
                operator = "invalid".to_owned();
                self.diagnostic(
                    DiagnosticKind::UndefinedValue,
                    Some(func),
                    Some(block),
                    Some(inst_index),
                    format!("value {} has no definition", value.index()),
                );
            }
            ValueDef::BlockParam(..) => {
                operator = "block_param".to_owned();
                self.diagnostic(
                    DiagnosticKind::ValueOwnership,
                    Some(func),
                    Some(block),
                    Some(inst_index),
                    format!(
                        "block parameter value {} is listed as an instruction",
                        value.index()
                    ),
                );
            }
        }

        PlanInst {
            operator,
            args,
            results,
            result_types,
            call,
            arena,
            const_value,
            ref_func,
        }
    }

    fn plan_call_operator(
        &mut self,
        func: PlanFuncId,
        block: PlanBlockId,
        inst_index: u32,
        op: &Operator,
        value_arity: usize,
        result_arity: usize,
    ) -> Option<PlanCall> {
        match op {
            Operator::Call { function_index } => {
                let callee = *function_index;
                let Some(callee_id) = self.func_ids.get(&callee).copied() else {
                    self.diagnostic(
                        DiagnosticKind::CallShape,
                        Some(func),
                        Some(block),
                        Some(inst_index),
                        format!("call target function {} does not exist", callee.index()),
                    );
                    return None;
                };
                let Some(signature) = self.signature_of_func(callee) else {
                    self.diagnostic(
                        DiagnosticKind::CallShape,
                        Some(func),
                        Some(block),
                        Some(inst_index),
                        format!(
                            "call target function {} has no consumable signature",
                            callee.index()
                        ),
                    );
                    return None;
                };
                self.check_call_shape(
                    func,
                    Some(block),
                    Some(inst_index),
                    signature,
                    value_arity,
                    result_arity,
                );
                Some(PlanCall {
                    kind: PlanCallKind::Direct { callee: callee_id },
                    signature: self
                        .sig_ids
                        .get(&signature)
                        .copied()
                        .unwrap_or(PlanSigId(u32::MAX)),
                    tail: false,
                })
            }
            Operator::CallIndirect {
                sig_index,
                table_index,
            } => {
                if self.module.tables.get(*table_index).is_none() {
                    self.diagnostic(
                        DiagnosticKind::CallShape,
                        Some(func),
                        Some(block),
                        Some(inst_index),
                        format!(
                            "call_indirect table {} does not exist",
                            table_index.index()
                        ),
                    );
                }
                // The last argument is the table index operand.
                let value_args = value_arity.saturating_sub(1);
                if value_arity == 0 {
                    self.diagnostic(
                        DiagnosticKind::CallShape,
                        Some(func),
                        Some(block),
                        Some(inst_index),
                        "call_indirect requires at least the table index argument",
                    );
                }
                self.check_call_shape(
                    func,
                    Some(block),
                    Some(inst_index),
                    *sig_index,
                    value_args,
                    result_arity,
                );
                Some(PlanCall {
                    kind: PlanCallKind::Indirect {
                        table: table_index.index() as u32,
                    },
                    signature: self
                        .sig_ids
                        .get(sig_index)
                        .copied()
                        .unwrap_or(PlanSigId(u32::MAX)),
                    tail: false,
                })
            }
            Operator::CallRef { sig_index } => {
                // The last argument is the function reference operand.
                let value_args = value_arity.saturating_sub(1);
                if value_arity == 0 {
                    self.diagnostic(
                        DiagnosticKind::CallShape,
                        Some(func),
                        Some(block),
                        Some(inst_index),
                        "call_ref requires at least the function reference argument",
                    );
                }
                self.check_call_shape(
                    func,
                    Some(block),
                    Some(inst_index),
                    *sig_index,
                    value_args,
                    result_arity,
                );
                Some(PlanCall {
                    kind: PlanCallKind::Ref,
                    signature: self
                        .sig_ids
                        .get(sig_index)
                        .copied()
                        .unwrap_or(PlanSigId(u32::MAX)),
                    tail: false,
                })
            }
            _ => None,
        }
    }

    fn check_call_shape(
        &mut self,
        func: PlanFuncId,
        block: Option<PlanBlockId>,
        inst: Option<u32>,
        signature: Signature,
        value_arity: usize,
        result_arity: usize,
    ) {
        let Some((params, returns)) = self.signature_types(signature) else {
            self.diagnostic(
                DiagnosticKind::CallShape,
                Some(func),
                block,
                inst,
                format!(
                    "call signature entity {} is not a function signature",
                    signature.index()
                ),
            );
            return;
        };
        if value_arity != params.len() {
            self.diagnostic(
                DiagnosticKind::CallShape,
                Some(func),
                block,
                inst,
                format!(
                    "call passes {value_arity} value arguments but the signature declares {} parameters",
                    params.len()
                ),
            );
        }
        if result_arity != returns.len() {
            self.diagnostic(
                DiagnosticKind::CallShape,
                Some(func),
                block,
                inst,
                format!(
                    "call produces {result_arity} results but the signature declares {} results",
                    returns.len()
                ),
            );
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn plan_terminator(
        &mut self,
        func: PlanFuncId,
        block: PlanBlockId,
        body: &FunctionBody,
        raw_block: Block,
        block_ids: &BTreeMap<Block, PlanBlockId>,
        value_ids: &BTreeMap<Value, PlanValueId>,
        uses: &mut Vec<(u32, i64, Value)>,
        completion_kinds: &mut BTreeSet<CompletionKind>,
    ) -> PlanTerminator {
        let use_position = body.blocks[raw_block].insts.len() as i64;

        let Some(terminator) = body.blocks[raw_block].terminator.pure_core() else {
            self.diagnostic(
                DiagnosticKind::InvalidTerminator,
                Some(func),
                Some(block),
                None,
                format!("block {} has no terminator", raw_block.index()),
            );
            return PlanTerminator::Trap;
        };
        match terminator {
            Terminator::Br { target } => {
                let target = self.map_plan_target(func, block, &target, block_ids, value_ids, uses, use_position);
                PlanTerminator::Br { target }
            }
            Terminator::CondBr {
                cond,
                if_true,
                if_false,
            } => {
                uses.push((block.0, use_position, cond));
                let cond = value_ids.get(&cond).copied().unwrap_or_else(|| {
                    self.undefined_use(func, block, cond);
                    PlanValueId(u32::MAX)
                });
                let if_true =
                    self.map_plan_target(func, block, &if_true, block_ids, value_ids, uses, use_position);
                let if_false =
                    self.map_plan_target(func, block, &if_false, block_ids, value_ids, uses, use_position);
                PlanTerminator::CondBr {
                    cond,
                    if_true,
                    if_false,
                }
            }
            Terminator::Select {
                value,
                targets,
                default,
            } => {
                uses.push((block.0, use_position, value));
                let value = value_ids.get(&value).copied().unwrap_or_else(|| {
                    self.undefined_use(func, block, value);
                    PlanValueId(u32::MAX)
                });
                let targets = targets
                    .iter()
                    .map(|target| {
                        self.map_plan_target(func, block, target, block_ids, value_ids, uses, use_position)
                    })
                    .collect();
                let default =
                    self.map_plan_target(func, block, &default, block_ids, value_ids, uses, use_position);
                PlanTerminator::Select {
                    value,
                    targets,
                    default,
                }
            }
            Terminator::Return { values } => {
                let arity = values.len();
                let mapped = values
                    .iter()
                    .map(|value| {
                        uses.push((block.0, use_position, *value));
                        value_ids.get(value).copied().unwrap_or_else(|| {
                            self.undefined_use(func, block, *value);
                            PlanValueId(u32::MAX)
                        })
                    })
                    .collect::<Vec<_>>();
                self.check_return_arity(func, Some(block), body, arity);
                completion_kinds.insert(CompletionKind::Return);
                PlanTerminator::Return { values: mapped }
            }
            Terminator::ReturnCall { func: callee, args } => {
                let arity = args.len();
                let args = args
                    .iter()
                    .map(|arg| {
                        uses.push((block.0, use_position, *arg));
                        value_ids.get(arg).copied().unwrap_or_else(|| {
                            self.undefined_use(func, block, *arg);
                            PlanValueId(u32::MAX)
                        })
                    })
                    .collect::<Vec<_>>();
                let kind = match self.func_ids.get(&callee).copied() {
                    Some(callee_id) => PlanCallKind::Direct { callee: callee_id },
                    None => {
                        self.diagnostic(
                            DiagnosticKind::CallShape,
                            Some(func),
                            Some(block),
                            None,
                            format!(
                                "tail call target function {} does not exist",
                                callee.index()
                            ),
                        );
                        PlanCallKind::Direct {
                            callee: PlanFuncId(u32::MAX),
                        }
                    }
                };
                let signature = self.signature_of_func(callee);
                if let Some(signature) = signature {
                    self.check_tail_call(func, block, body, signature, arity);
                }
                PlanTerminator::TailCall {
                    call: kind,
                    signature: signature
                        .and_then(|sig| self.sig_ids.get(&sig).copied())
                        .unwrap_or(PlanSigId(u32::MAX)),
                    args,
                }
            }
            Terminator::ReturnCallIndirect { sig, table, args } => {
                let arity = args.len().saturating_sub(1);
                if args.is_empty() {
                    self.diagnostic(
                        DiagnosticKind::CallShape,
                        Some(func),
                        Some(block),
                        None,
                        "tail call_indirect requires at least the table index argument",
                    );
                }
                let args = args
                    .iter()
                    .map(|arg| {
                        uses.push((block.0, use_position, *arg));
                        value_ids.get(arg).copied().unwrap_or_else(|| {
                            self.undefined_use(func, block, *arg);
                            PlanValueId(u32::MAX)
                        })
                    })
                    .collect::<Vec<_>>();
                if self.module.tables.get(table).is_none() {
                    self.diagnostic(
                        DiagnosticKind::CallShape,
                        Some(func),
                        Some(block),
                        None,
                        format!(
                            "tail call_indirect table {} does not exist",
                            table.index()
                        ),
                    );
                }
                self.check_tail_call(func, block, body, sig, arity);
                PlanTerminator::TailCall {
                    call: PlanCallKind::Indirect {
                        table: table.index() as u32,
                    },
                    signature: self.sig_ids.get(&sig).copied().unwrap_or(PlanSigId(u32::MAX)),
                    args,
                }
            }
            Terminator::ReturnCallRef { sig, args } => {
                let arity = args.len().saturating_sub(1);
                if args.is_empty() {
                    self.diagnostic(
                        DiagnosticKind::CallShape,
                        Some(func),
                        Some(block),
                        None,
                        "tail call_ref requires at least the function reference argument",
                    );
                }
                let args = args
                    .iter()
                    .map(|arg| {
                        uses.push((block.0, use_position, *arg));
                        value_ids.get(arg).copied().unwrap_or_else(|| {
                            self.undefined_use(func, block, *arg);
                            PlanValueId(u32::MAX)
                        })
                    })
                    .collect::<Vec<_>>();
                self.check_tail_call(func, block, body, sig, arity);
                PlanTerminator::TailCall {
                    call: PlanCallKind::Ref,
                    signature: self.sig_ids.get(&sig).copied().unwrap_or(PlanSigId(u32::MAX)),
                    args,
                }
            }
            Terminator::Unreachable => {
                completion_kinds.insert(CompletionKind::Trap);
                PlanTerminator::Trap
            }
            Terminator::UB => PlanTerminator::Ub,
            Terminator::None => {
                self.diagnostic(
                    DiagnosticKind::InvalidTerminator,
                    Some(func),
                    Some(block),
                    None,
                    format!("block {} has no terminator", raw_block.index()),
                );
                PlanTerminator::Trap
            }
            _ => {
                self.diagnostic(
                    DiagnosticKind::InvalidTerminator,
                    Some(func),
                    Some(block),
                    None,
                    "unrecognized terminator form",
                );
                PlanTerminator::Trap
            }
        }
    }

    fn undefined_use(&mut self, func: PlanFuncId, block: PlanBlockId, value: Value) {
        self.diagnostic(
            DiagnosticKind::UndefinedValue,
            Some(func),
            Some(block),
            None,
            format!("value {} is not defined in this function", value.index()),
        );
    }

    fn map_plan_target(
        &mut self,
        func: PlanFuncId,
        block: PlanBlockId,
        target: &portal_pc_waffle::BlockTarget,
        block_ids: &BTreeMap<Block, PlanBlockId>,
        value_ids: &BTreeMap<Value, PlanValueId>,
        uses: &mut Vec<(u32, i64, Value)>,
        use_position: i64,
    ) -> PlanTarget {
        let args = target
            .args
            .iter()
            .map(|arg| {
                uses.push((block.0, use_position, *arg));
                value_ids.get(arg).copied().unwrap_or_else(|| {
                    self.undefined_use(func, block, *arg);
                    PlanValueId(u32::MAX)
                })
            })
            .collect::<Vec<_>>();
        let target_block = match block_ids.get(&target.block) {
            Some(id) => *id,
            None => {
                self.diagnostic(
                    DiagnosticKind::InvalidTerminator,
                    Some(func),
                    Some(block),
                    None,
                    format!(
                        "terminator targets block {} which does not exist",
                        target.block.index()
                    ),
                );
                PlanBlockId(u32::MAX)
            }
        };
        PlanTarget {
            block: target_block,
            args,
        }
    }

    fn check_return_arity(
        &mut self,
        func: PlanFuncId,
        block: Option<PlanBlockId>,
        body: &FunctionBody,
        arity: usize,
    ) {
        if arity != body.rets.len() {
            self.diagnostic(
                DiagnosticKind::ReturnArity,
                Some(func),
                block,
                None,
                format!(
                    "return has {arity} values but the function declares {} results",
                    body.rets.len()
                ),
            );
        }
    }

    fn check_tail_call(
        &mut self,
        func: PlanFuncId,
        block: PlanBlockId,
        body: &FunctionBody,
        signature: Signature,
        value_arity: usize,
    ) {
        let Some((params, returns)) = self.signature_types(signature) else {
            self.diagnostic(
                DiagnosticKind::CallShape,
                Some(func),
                Some(block),
                None,
                format!(
                    "tail call signature entity {} is not a function signature",
                    signature.index()
                ),
            );
            return;
        };
        if value_arity != params.len() {
            self.diagnostic(
                DiagnosticKind::CallShape,
                Some(func),
                Some(block),
                None,
                format!(
                    "tail call passes {value_arity} value arguments but the signature declares {} parameters",
                    params.len()
                ),
            );
        }
        // Frame replacement requires the callee results to match the caller's
        // declared results exactly.
        let caller: Vec<RawType> = body.rets.iter().map(raw_type).collect();
        if returns != caller {
            self.diagnostic(
                DiagnosticKind::ReturnArity,
                Some(func),
                Some(block),
                None,
                "tail call result types must match the caller's result types for frame replacement",
            );
        }
    }

    fn layout_for(&self, ty: Option<TypeId>, _def: &ValueDef) -> PlanLayout {
        let Some(ty) = ty else {
            // Multi-result tuple values have no standalone logical type.
            return PlanLayout::Tuple {
                elements: Vec::new(),
            };
        };
        let Some(plan_ty) = self.type_list.get(ty.index()).cloned() else {
            return PlanLayout::Scalar { bytes: 0 };
        };
        match plan_ty {
            PlanType::I32 | PlanType::F32 => PlanLayout::Scalar { bytes: 4 },
            PlanType::I64 | PlanType::F64 => PlanLayout::Scalar { bytes: 8 },
            PlanType::V128 => PlanLayout::Scalar { bytes: 16 },
            PlanType::Ref { nullable, heap } => {
                let target = match &heap {
                    PlanHeapType::Sig(sig) => match self.signatures.get(sig.index()) {
                        Some(PlanSignature::Struct { .. }) | Some(PlanSignature::Array { .. }) => {
                            PlanRefTarget::Aggregate(*sig)
                        }
                        Some(PlanSignature::Func { .. }) | Some(PlanSignature::Import { .. }) => {
                            PlanRefTarget::FuncSig(*sig)
                        }
                        _ => PlanRefTarget::Dynamic(heap.clone()),
                    },
                    other => PlanRefTarget::Dynamic(other.clone()),
                };
                match &target {
                    PlanRefTarget::Aggregate(sig) => PlanLayout::Aggregate { sig: *sig },
                    _ => PlanLayout::Ref { nullable, target },
                }
            }
        }
    }

    fn plan_boundaries(&mut self, functions: &[PlanFunction]) -> Vec<BoundaryRecord> {
        let mut boundaries = Vec::new();

        for import in &self.module.imports {
            let (target, abi) = match import.kind {
                ImportKind::Func(func) => match self.func_ids.get(&func).copied() {
                    Some(id) => {
                        let abi = self.boundary_abi(func);
                        (BoundaryTarget::Func(id), abi)
                    }
                    None => {
                        self.diagnostic(
                            DiagnosticKind::BoundaryAbi,
                            None,
                            None,
                            None,
                            format!(
                                "import {}::{} references function {} which does not exist",
                                import.module,
                                import.name,
                                func.index()
                            ),
                        );
                        continue;
                    }
                },
                ImportKind::Table(table) => (BoundaryTarget::Table(table.index() as u32), None),
                ImportKind::Global(global) => {
                    (BoundaryTarget::Global(global.index() as u32), None)
                }
                ImportKind::Memory(memory) => {
                    (BoundaryTarget::Memory(memory.index() as u32), None)
                }
                ImportKind::ControlTag(tag) => {
                    (BoundaryTarget::ControlTag(tag.index() as u32), None)
                }
                ImportKind::Type(signature) => (
                    BoundaryTarget::Type(
                        self.sig_ids
                            .get(&signature)
                            .copied()
                            .unwrap_or(PlanSigId(u32::MAX)),
                    ),
                    None,
                ),
                _ => continue,
            };
            if let BoundaryTarget::Func(id) = target {
                if !matches!(
                    functions[id.index()].kind,
                    PlanFunctionKind::Import { .. }
                ) {
                    self.diagnostic(
                        DiagnosticKind::BoundaryAbi,
                        Some(id),
                        None,
                        None,
                        format!(
                            "import {}::{} references a function not declared as an import",
                            import.module, import.name
                        ),
                    );
                }
            }
            boundaries.push(BoundaryRecord {
                direction: BoundaryDirection::Import,
                module: Some(import.module.clone()),
                name: import.name.clone(),
                target,
                abi,
            });
        }

        let mut seen_names: BTreeSet<&str> = BTreeSet::new();
        let mut reported_duplicates: BTreeSet<&str> = BTreeSet::new();
        for export in &self.module.exports {
            if !seen_names.insert(export.name.as_str())
                && reported_duplicates.insert(export.name.as_str())
            {
                self.diagnostic(
                    DiagnosticKind::BoundaryAbi,
                    None,
                    None,
                    None,
                    format!("duplicate export name {}", export.name),
                );
            }
            let (target, abi) = match export.kind {
                ExportKind::Func(func) => match self.func_ids.get(&func).copied() {
                    Some(id) => {
                        let abi = self.boundary_abi(func);
                        (BoundaryTarget::Func(id), abi)
                    }
                    None => {
                        self.diagnostic(
                            DiagnosticKind::BoundaryAbi,
                            None,
                            None,
                            None,
                            format!(
                                "export {} references function {} which does not exist",
                                export.name,
                                func.index()
                            ),
                        );
                        continue;
                    }
                },
                ExportKind::Table(table) => (BoundaryTarget::Table(table.index() as u32), None),
                ExportKind::Global(global) => {
                    (BoundaryTarget::Global(global.index() as u32), None)
                }
                ExportKind::Memory(memory) => {
                    (BoundaryTarget::Memory(memory.index() as u32), None)
                }
                ExportKind::ControlTag(tag) => {
                    (BoundaryTarget::ControlTag(tag.index() as u32), None)
                }
                ExportKind::Type(signature) => (
                    BoundaryTarget::Type(
                        self.sig_ids
                            .get(&signature)
                            .copied()
                            .unwrap_or(PlanSigId(u32::MAX)),
                    ),
                    None,
                ),
                _ => continue,
            };
            boundaries.push(BoundaryRecord {
                direction: BoundaryDirection::Export,
                module: None,
                name: export.name.clone(),
                target,
                abi,
            });
        }
        boundaries
    }

    fn boundary_abi(&mut self, func: Func) -> Option<BoundaryAbi> {
        let (params, results) = self.func_signature_types(func)?;
        Some(BoundaryAbi {
            params,
            results: results.clone(),
            completions: CompletionShape {
                kinds: vec![CompletionKind::Return],
                results,
            },
        })
    }
}

/// Default representation policy: scalars stay scalar, typed aggregates live
/// in the logical arena, function references and dynamic/host values are
/// boundary-represented, and multi-result tuples are inline aggregates.
fn representation_for(ty: Option<PlanType>, layout: &PlanLayout) -> NanoValueRepresentation {
    match (ty, layout) {
        (None, _) => NanoValueRepresentation::InlineAggregate,
        (Some(PlanType::Ref { .. }), PlanLayout::Aggregate { .. }) => NanoValueRepresentation::ArenaRef,
        (Some(PlanType::Ref { .. }), _) => NanoValueRepresentation::Boundary,
        (Some(_), _) => NanoValueRepresentation::Scalar,
    }
}

/// Canonical block order: reverse postorder from the entry following
/// terminator successor order, then any unreachable blocks in source order.
fn canonical_block_order(body: &FunctionBody) -> Vec<Block> {
    let mut order = Vec::new();
    let mut visited = vec![false; body.blocks.len()];
    if body.blocks.len() > body.entry.index() {
        // Iterative postorder over explicit terminator successors.
        let mut stack: Vec<(Block, bool)> = vec![(body.entry, false)];
        while let Some((block, expanded)) = stack.pop() {
            if expanded {
                order.push(block);
                continue;
            }
            if visited[block.index()] {
                continue;
            }
            visited[block.index()] = true;
            stack.push((block, true));
            let mut successors = Vec::new();
            body.blocks[block]
                .terminator
                .visit_successors(|succ| successors.push(succ));
            // Push in reverse so the first successor is visited first.
            for succ in successors.into_iter().rev() {
                if !visited[succ.index()] {
                    stack.push((succ, false));
                }
            }
        }
        order.reverse();
    }
    // Unreachable blocks keep source (entity) order, deterministically.
    for (block, _) in body.blocks.entries() {
        if !visited[block.index()] {
            order.push(block);
        }
    }
    order
}

/// Standard forward dominator sets over the canonical block order. Blocks
/// unreachable from the entry keep the permissive full set, so def-before-use
/// checks are strict for reachable code and lenient for dead code.
fn compute_dominators(body: &FunctionBody, order: &[Block]) -> Vec<BTreeSet<u32>> {
    let position: BTreeMap<Block, u32> = order
        .iter()
        .enumerate()
        .map(|(index, block)| (*block, index as u32))
        .collect();
    let all: BTreeSet<u32> = (0..order.len() as u32).collect();
    let mut dominators: Vec<BTreeSet<u32>> = vec![all; order.len()];
    let entry_pos = position.get(&body.entry).copied().unwrap_or(0);
    if !order.is_empty() {
        dominators[entry_pos as usize] = BTreeSet::from([entry_pos]);
    }
    let mut changed = true;
    while changed {
        changed = false;
        for (index, block) in order.iter().enumerate() {
            if index == entry_pos as usize {
                continue;
            }
            let reachable_preds: Vec<u32> = body.blocks[*block]
                .preds
                .iter()
                .filter_map(|pred| position.get(pred).copied())
                .filter(|pred| *pred != index as u32)
                .collect();
            if reachable_preds.is_empty() {
                continue; // unreachable: keep the permissive full set
            }
            let mut meet: BTreeSet<u32> = (0..order.len() as u32).collect();
            for pred in reachable_preds {
                meet = meet
                    .intersection(&dominators[pred as usize])
                    .copied()
                    .collect();
            }
            meet.insert(index as u32);
            if meet != dominators[index] {
                dominators[index] = meet;
                changed = true;
            }
        }
    }
    dominators
}

/// Stable operator name: the Waffle variant name, with payload data removed.
fn operator_name(op: &Operator) -> String {
    let debug = format!("{op:?}");
    let end = debug
        .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
        .unwrap_or(debug.len());
    debug[..end].to_string()
}

/// Logical arena classification for WasmGC operators.
fn arena_op(name: &str) -> Option<PlanArenaOp> {
    Some(match name {
        "StructNew" | "StructNewDefault" | "ArrayNew" | "ArrayNewFixed" | "ArrayNewDefault"
        | "ArrayNewData" | "ArrayNewElem" | "RefI31" => PlanArenaOp::Alloc,
        "StructGet" | "StructGetS" | "StructGetU" | "ArrayGet" | "ArrayGetS" | "ArrayGetU"
        | "I31GetS" | "I31GetU" => PlanArenaOp::Read,
        "StructSet" | "ArraySet" | "ArrayFill" | "ArrayCopy" | "ArrayInitData"
        | "ArrayInitElem" => PlanArenaOp::Write,
        "ArrayLen" => PlanArenaOp::Len,
        "RefTest" | "RefCast" => PlanArenaOp::TestCast,
        "RefEq" => PlanArenaOp::Eq,
        _ => return None,
    })
}

fn operator_uses_simd(name: &str) -> bool {
    const SIMD_PREFIXES: [&str; 7] = [
        "I8x16", "I16x8", "I32x4", "I64x2", "F32x4", "F64x2", "V128",
    ];
    SIMD_PREFIXES.iter().any(|prefix| name.starts_with(prefix))
}

fn operator_uses_atomics(name: &str) -> bool {
    name.contains("Atomic")
}

/// Conservative trap classification: anything that can observably trap.
fn operator_may_trap(name: &str) -> bool {
    const TRAP_PREFIXES: [&str; 6] = ["I32Div", "I64Div", "I32Rem", "I64Rem", "Memory", "Atomic"];
    TRAP_PREFIXES.iter().any(|prefix| name.starts_with(prefix))
        || name.contains("Load")
        || name.contains("Store")
        || name.contains("Trunc")
        || matches!(
            name,
            "CallIndirect"
                | "StructGet"
                | "StructGetS"
                | "StructGetU"
                | "StructSet"
                | "ArrayGet"
                | "ArrayGetS"
                | "ArrayGetU"
                | "ArraySet"
                | "ArrayLen"
                | "ArrayCopy"
                | "ArrayFill"
                | "RefCast"
                | "RefAsNonNull"
                | "I31GetS"
                | "I31GetU"
        )
}

#[cfg(test)]
mod tests;
