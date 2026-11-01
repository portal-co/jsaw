//! Wasm binary modules as closed-world module-set members (wasm-esm,
//! portal-co/jsaw#14; `docs/plan-wasm-esm-modules.md`).
//!
//! A `.wasm` key in a [`ModuleSet`](crate::ModuleSet) is a Wasm binary
//! module: JavaScript imports its function exports with ordinary named ESM
//! imports (`import { add } from "./math.wasm"`), and the binary's own type
//! section is the ABI declaration — no FFI manifest entry is needed.
//!
//! This module is the deep module at the binary seam: it owns validation,
//! feature-profile classification, and the export/import surface. It does
//! not know about JS values, adapters, or backend policy; lowering (Waffle
//! ingestion and merge) is phase 2 of the plan.
//!
//! Fail-closed rules (plan §§3.2, 5.4, and the Phase 0 audit):
//!
//! - A Wasm module with any imports is rejected for now; import
//!   satisfaction (FFI manifest / intra-set edges) is a later phase.
//! - Passive/declared element segments and passive data segments are
//!   rejected explicitly even though no profile enables bulk-memory yet:
//!   `waffle-frontend` currently drops them silently, so a future profile
//!   bump must not let them through unnoticed.
//! - Non-funcref tables are rejected (the frontend's element ingestion
//!   assumes funcref tables).
//! - Active data/element offset expressions must be plain constants; the
//!   frontend cannot rebase `global.get` bases.

use std::collections::BTreeMap;
use std::marker::PhantomData;

use portal_pc_waffle::frontend::{FrontendOptions, expand_all_funcs, from_wasm_bytes};
use portal_pc_waffle::{FuncDecl, Module};
use wasmparser::{ExternalKind, Parser, Payload, TypeRef, ValType, Validator, WasmFeatures};

use crate::repr::ConvertError;

/// A cumulative Wasm feature profile (plan §3.3). Ordering is significant:
/// each profile includes every earlier one.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum WasmProfile {
    /// Wasm 1.0 core plus mutable globals (`WasmFeatures::WASM1`).
    Mvp,
    /// + block/function multi-results.
    MultiValue,
    /// + 64-bit linear memory.
    Memory64,
    /// + multiple named memories.
    MultiMemory,
    /// + WasmGC types and operators (with function-references).
    Gc,
    /// + fixed-width SIMD (`v128`). v128 never crosses the JS boundary —
    /// exports with v128 signatures fail closed at the adapter
    /// (portal-co/jsaw#7).
    Simd,
    /// + tail calls. Tail-calling members are ineligible for late binding
    /// and take the static merge path.
    TailCall,
}

impl WasmProfile {
    /// All profiles in cumulative order, lowest first.
    pub(crate) const ALL: [WasmProfile; 7] = [
        WasmProfile::Mvp,
        WasmProfile::MultiValue,
        WasmProfile::Memory64,
        WasmProfile::MultiMemory,
        WasmProfile::Gc,
        WasmProfile::Simd,
        WasmProfile::TailCall,
    ];

    fn features(self) -> WasmFeatures {
        let mut features = WasmFeatures::WASM1;
        if self >= WasmProfile::MultiValue {
            features.insert(WasmFeatures::MULTI_VALUE);
        }
        if self >= WasmProfile::Memory64 {
            features.insert(WasmFeatures::MEMORY64);
        }
        if self >= WasmProfile::MultiMemory {
            features.insert(WasmFeatures::MULTI_MEMORY);
        }
        if self >= WasmProfile::Gc {
            features.insert(WasmFeatures::GC);
            features.insert(WasmFeatures::FUNCTION_REFERENCES);
            // GC modules use funcref/externref in tables and signatures;
            // reference-types is the subsumed base proposal.
            features.insert(WasmFeatures::REFERENCE_TYPES);
        }
        if self >= WasmProfile::Simd {
            features.insert(WasmFeatures::SIMD);
        }
        if self >= WasmProfile::TailCall {
            features.insert(WasmFeatures::TAIL_CALL);
        }
        features
    }

    pub(crate) fn name(self) -> &'static str {
        match self {
            WasmProfile::Mvp => "mvp",
            WasmProfile::MultiValue => "multi-value",
            WasmProfile::Memory64 => "memory64",
            WasmProfile::MultiMemory => "multi-memory",
            WasmProfile::Gc => "gc",
            WasmProfile::Simd => "simd",
            WasmProfile::TailCall => "tail-call",
        }
    }
}

/// The internal-hierarchy abstract heap types jsaw adapters can cast to and
/// from (`anyref` family; plan §5.3).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum WasmAbstractHeap {
    Any,
    None,
    Eq,
    I31,
    Struct,
    Array,
}

/// A reference-typed value position (plan §5.2: the signature carries the
/// actual heap type when the gc profile is on).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum WasmRefType {
    /// `(ref (null) $t)` — `index` is the member-local type index.
    Concrete { nullable: bool, index: u32 },
    /// An abstract internal-hierarchy reference (`any`, `none`, `eq`,
    /// `i31`, `struct`, `array`).
    Abstract {
        nullable: bool,
        heap: WasmAbstractHeap,
    },
    /// `funcref`/`nofunc` — JS function objects are GC structs, not
    /// funcrefs, so this fails closed at the adapter.
    OpaqueFunc { nullable: bool },
    /// `externref`/`noextern` — the external hierarchy is disjoint from
    /// jsaw values; fails closed at the adapter.
    OpaqueExtern { nullable: bool },
}

impl WasmRefType {
    pub(crate) fn nullable(&self) -> bool {
        match self {
            WasmRefType::Concrete { nullable, .. }
            | WasmRefType::Abstract { nullable, .. }
            | WasmRefType::OpaqueFunc { nullable }
            | WasmRefType::OpaqueExtern { nullable } => *nullable,
        }
    }
}

/// One value position in a Wasm function signature, as declared by the
/// binary's type section.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum WasmValType {
    I32,
    I64,
    F32,
    F64,
    V128,
    Ref(WasmRefType),
}

impl WasmValType {
    fn from_wasmparser(ty: ValType) -> Self {
        match ty {
            ValType::I32 => WasmValType::I32,
            ValType::I64 => WasmValType::I64,
            ValType::F32 => WasmValType::F32,
            ValType::F64 => WasmValType::F64,
            ValType::V128 => WasmValType::V128,
            ValType::Ref(reference) => {
                let nullable = reference.is_nullable();
                WasmValType::Ref(match reference.heap_type() {
                    wasmparser::HeapType::Concrete(index) => WasmRefType::Concrete {
                        nullable,
                        index: index.as_module_index().unwrap_or(u32::MAX),
                    },
                    wasmparser::HeapType::Abstract { ty, .. } => match ty {
                        wasmparser::AbstractHeapType::Any => WasmRefType::Abstract {
                            nullable,
                            heap: WasmAbstractHeap::Any,
                        },
                        wasmparser::AbstractHeapType::None => WasmRefType::Abstract {
                            nullable,
                            heap: WasmAbstractHeap::None,
                        },
                        wasmparser::AbstractHeapType::Eq => WasmRefType::Abstract {
                            nullable,
                            heap: WasmAbstractHeap::Eq,
                        },
                        wasmparser::AbstractHeapType::I31 => WasmRefType::Abstract {
                            nullable,
                            heap: WasmAbstractHeap::I31,
                        },
                        wasmparser::AbstractHeapType::Struct => WasmRefType::Abstract {
                            nullable,
                            heap: WasmAbstractHeap::Struct,
                        },
                        wasmparser::AbstractHeapType::Array => WasmRefType::Abstract {
                            nullable,
                            heap: WasmAbstractHeap::Array,
                        },
                        wasmparser::AbstractHeapType::Func
                        | wasmparser::AbstractHeapType::NoFunc => {
                            WasmRefType::OpaqueFunc { nullable }
                        }
                        wasmparser::AbstractHeapType::Extern
                        | wasmparser::AbstractHeapType::NoExtern => {
                            WasmRefType::OpaqueExtern { nullable }
                        }
                        // exn/noexn (exception handling) and continuations
                        // are out of scope (portal-co/jsaw#7).
                        _ => WasmRefType::OpaqueExtern { nullable },
                    },
                })
            }
        }
    }
}

/// A Wasm function signature derived from the binary's own type section.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct WasmFuncSignature {
    pub(crate) params: Vec<WasmValType>,
    /// A real Wasm signature has real results; multi-result signatures
    /// appear under the multi-value profile.
    pub(crate) results: Vec<WasmValType>,
}

/// One entry of a Wasm member's export surface.
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum WasmExport {
    Func {
        /// The export's index in the member's own function index space
        /// (imported functions first, then defined ones).
        index: u32,
        signature: WasmFuncSignature,
    },
    Memory,
    /// A global export, read live as a JS value (portal-co/jsaw#10):
    /// `index` is the member-local global index, `ty` its value type.
    Global { index: u32, ty: WasmValType },
    Table,
    Tag,
}

/// One entry of a Wasm module's import section. Only function imports are
/// accepted (plan §5.4); every other kind is rejected at ingestion.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct WasmImport {
    pub(crate) module: String,
    pub(crate) field: String,
    /// The binary's declared import signature (function imports only).
    pub(crate) signature: WasmFuncSignature,
}

/// A validated Wasm binary module-set member: the ingested Waffle IR (all
/// bodies expanded, no borrows of the original bytes), its exact feature
/// requirement, and its export/import surfaces.
///
/// The Waffle module keeps the binary's name section verbatim as debug
/// names ("keep WASM names", plan §5.1).
// `imports` is always empty today (phase 1 rejects Wasm-side imports) and
// is read by the phase implementing plan §5.4.
#[derive(Clone, Debug)]
#[allow(dead_code)]
pub(crate) struct WasmModulePlan {
    pub(crate) path: String,
    pub(crate) profile: WasmProfile,
    pub(crate) module: Module<'static>,
    pub(crate) exports: BTreeMap<String, WasmExport>,
    pub(crate) imports: Vec<WasmImport>,
    /// Late binding (portal-co/jsaw#6): when the member was converted to
    /// its GC-native instance form, the layout. `None` means the static
    /// single-instance merge path (members with tables/indirect calls).
    pub(crate) instance: Option<InstanceLayout>,
}

/// A statically resolved import of a Wasm member's function export.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct WasmModuleImportSpec {
    /// Module-set key of the `.wasm` member.
    pub(crate) path: String,
    /// The Wasm export name (a genuinely textual domain — no hygiene).
    pub(crate) export: String,
    /// The binary's own declared signature.
    pub(crate) signature: WasmFuncSignature,
}

/// Validate `bytes` as a Wasm module-set member and extract its surfaces.
///
/// `ceiling` is the highest accepted profile; a module requiring a feature
/// above it fails with a diagnostic naming the feature, the module path,
/// and the ceiling (plan §3.3).
pub(crate) fn parse_wasm_module(
    path: &str,
    bytes: &[u8],
    ceiling: WasmProfile,
) -> Result<WasmModulePlan, ConvertError> {
    // Component binaries share the `\0asm` magic but carry a layer field in
    // the version word (low 16 bits: 0 = core module, 1 = component). Name
    // the Component Model non-goal rather than surfacing a bare validation
    // error (portal-co/jsaw#5).
    if bytes.len() >= 8 && &bytes[0..4] == b"\0asm" {
        let layer = u16::from_le_bytes([bytes[6], bytes[7]]);
        if layer != 0 {
            return Err(ConvertError::invalid(format!(
                "Wasm member {path:?} is a Component Model binary (layer {layer}), not a core \
                 module; component ingestion is deferred (portal-co/jsaw#5)"
            )));
        }
    }
    // Determine the exact profile requirement: validate against each
    // profile from the lowest up; the first that accepts the module is its
    // requirement. Validating at the ceiling alone would accept but not
    // classify; validating lowest-up gives both classification and a
    // precise "needs feature X" diagnostic.
    let mut profile = None;
    let mut last_error = None;
    for candidate in WasmProfile::ALL {
        let mut validator = Validator::new_with_features(candidate.features());
        match validator.validate_all(bytes) {
            Ok(_) => {
                profile = Some(candidate);
                break;
            }
            Err(error) => last_error = Some((candidate, error)),
        }
    }
    let profile = profile.ok_or_else(|| {
        let (_, error) = last_error.expect("ALL is nonempty");
        ConvertError::invalid(format!(
            "Wasm module {path:?} failed validation under every supported profile \
             (mvp..gc): {error}"
        ))
    })?;
    if profile > ceiling {
        return Err(ConvertError::invalid(format!(
            "Wasm module {path:?} requires the {} profile but the configured ceiling is {}; \
             raise `WasmProfile` to include it",
            profile.name(),
            ceiling.name(),
        )));
    }

    // Indexed by the module's full type index space; non-function types
    // (GC struct/array) hold None so type indices stay aligned.
    let mut func_types: Vec<Option<WasmFuncSignature>> = Vec::new();
    let mut exports: BTreeMap<String, WasmExport> = BTreeMap::new();
    let mut imports: Vec<WasmImport> = Vec::new();
    // Type indices of imported functions come before defined ones; the
    // export section references the combined function index space.
    let mut imported_func_types: Vec<u32> = Vec::new();
    let mut defined_func_types: Vec<u32> = Vec::new();
    let mut global_types: Vec<WasmValType> = Vec::new();
    for payload in Parser::new(0).parse_all(bytes) {
        let payload = payload.map_err(|error| {
            ConvertError::invalid(format!("Wasm module {path:?} could not be parsed: {error}"))
        })?;
        match payload {
            Payload::TypeSection(reader) => {
                for rec_group in reader {
                    let rec_group = rec_group.map_err(|error| {
                        ConvertError::invalid(format!(
                            "Wasm module {path:?} has an unreadable type section: {error}"
                        ))
                    })?;
                    for sub in rec_group.into_types() {
                        // waffle's `SignatureData` drops subtyping
                        // (`is_final: true, supertype_idx: None` always, see
                        // waffle-ir module.rs), so a subtyped member would
                        // silently mis-encode: fail closed.
                        if !sub.is_final || sub.supertype_idx.is_some() {
                            return Err(ConvertError::invalid(format!(
                                "Wasm module {path:?} declares a subtype (sub/extends); Waffle \
                                 does not preserve subtyping, so such members are rejected \
                                 until that is fixed"
                            )));
                        }
                        let wasmparser::CompositeInnerType::Func(func) = &sub.composite_type.inner
                        else {
                            // GC struct/array types: validated already; the
                            // None keeps the type index space aligned.
                            func_types.push(None);
                            continue;
                        };
                        func_types.push(Some(WasmFuncSignature {
                            params: func
                                .params()
                                .iter()
                                .map(|ty| WasmValType::from_wasmparser(*ty))
                                .collect(),
                            results: func
                                .results()
                                .iter()
                                .map(|ty| WasmValType::from_wasmparser(*ty))
                                .collect(),
                        }));
                    }
                }
            }
            Payload::ImportSection(reader) => {
                for import in reader {
                    let import = import.map_err(|error| {
                        ConvertError::invalid(format!(
                            "Wasm module {path:?} has an unreadable import section: {error}"
                        ))
                    })?;
                    let TypeRef::Func(ty) = import.ty else {
                        // Memory/global/table/tag imports of a Wasm member
                        // interact with multi-memory identity and CoreGC's
                        // static layout (plan §5.4): fail closed.
                        return Err(ConvertError::invalid(format!(
                            "Wasm module {path:?} imports {}.{:?} which is not a function; \
                             non-function imports of Wasm members are not supported \
                             (portal-co/jsaw#10)",
                            import.module, import.name,
                        )));
                    };
                    imported_func_types.push(ty);
                    let signature =
                        func_types
                            .get(ty as usize)
                            .cloned()
                            .flatten()
                            .ok_or_else(|| {
                                ConvertError::invalid(format!(
                                    "Wasm module {path:?} import {}.{:?} references unknown or \
                                 non-function type index {ty}",
                                    import.module, import.name,
                                ))
                            })?;
                    imports.push(WasmImport {
                        module: import.module.to_string(),
                        field: import.name.to_string(),
                        signature,
                    });
                }
            }
            Payload::FunctionSection(reader) => {
                for func in reader {
                    let ty = func.map_err(|error| {
                        ConvertError::invalid(format!(
                            "Wasm module {path:?} has an unreadable function section: {error}"
                        ))
                    })?;
                    defined_func_types.push(ty);
                }
            }
            Payload::TableSection(reader) => {
                for table in reader {
                    let table = table.map_err(|error| {
                        ConvertError::invalid(format!(
                            "Wasm module {path:?} has an unreadable table section: {error}"
                        ))
                    })?;
                    if !table.ty.element_type.is_func_ref() {
                        return Err(ConvertError::invalid(format!(
                            "Wasm module {path:?} declares a non-funcref table; the Waffle \
                             frontend's element ingestion assumes funcref tables (plan Phase 0 \
                             audit)"
                        )));
                    }
                }
            }
            Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export.map_err(|error| {
                        ConvertError::invalid(format!(
                            "Wasm module {path:?} has an unreadable export section: {error}"
                        ))
                    })?;
                    let entry = match export.kind {
                        ExternalKind::Func => {
                            let index = export.index as usize;
                            let ty = if index < imported_func_types.len() {
                                imported_func_types[index]
                            } else {
                                defined_func_types
                                    .get(index - imported_func_types.len())
                                    .copied()
                                    .ok_or_else(|| {
                                        ConvertError::invalid(format!(
                                            "Wasm module {path:?} export {:?} references an \
                                             unknown function index {index}",
                                            export.name
                                        ))
                                    })?
                            };
                            let signature = func_types
                                .get(ty as usize)
                                .cloned()
                                .flatten()
                                .ok_or_else(|| {
                                    ConvertError::invalid(format!(
                                        "Wasm module {path:?} export {:?} references unknown or \
                                         non-function type index {ty}",
                                        export.name
                                    ))
                                })?;
                            WasmExport::Func {
                                index: export.index,
                                signature,
                            }
                        }
                        ExternalKind::Memory => WasmExport::Memory,
                        ExternalKind::Global => {
                            let gty = global_types.get(export.index as usize).copied()
                                .ok_or_else(|| {
                                    ConvertError::invalid(format!(
                                        "Wasm module {path:?} export {:?} references an unknown \
                                         global index {}",
                                        export.name, export.index
                                    ))
                                })?;
                            WasmExport::Global {
                                index: export.index,
                                ty: gty,
                            }
                        }
                        ExternalKind::Table => WasmExport::Table,
                        ExternalKind::Tag => WasmExport::Tag,
                    };
                    if exports.insert(export.name.to_string(), entry).is_some() {
                        return Err(ConvertError::invalid(format!(
                            "Wasm module {path:?} exports {:?} twice",
                            export.name
                        )));
                    }
                }
            }
            Payload::GlobalSection(reader) => {
                for global in reader {
                    let global = global.map_err(|error| {
                        ConvertError::invalid(format!(
                            "Wasm module {path:?} has an unreadable global section: {error}"
                        ))
                    })?;
                    global_types.push(WasmValType::from_wasmparser(global.ty.content_type));
                }
            }
            Payload::DataSection(reader) => {
                for segment in reader {
                    let segment = segment.map_err(|error| {
                        ConvertError::invalid(format!(
                            "Wasm module {path:?} has an unreadable data section: {error}"
                        ))
                    })?;
                    match &segment.kind {
                        wasmparser::DataKind::Passive => {
                            return Err(ConvertError::invalid(format!(
                                "Wasm module {path:?} has a passive data segment; \
                                 waffle-frontend currently drops passive segments silently \
                                 (plan Phase 0 audit), so they are rejected until that is fixed"
                            )));
                        }
                        wasmparser::DataKind::Active { offset_expr, .. } => {
                            reject_nonconst_offset(path, offset_expr, "data segment")?;
                        }
                    }
                }
            }
            Payload::ElementSection(reader) => {
                for element in reader {
                    let element = element.map_err(|error| {
                        ConvertError::invalid(format!(
                            "Wasm module {path:?} has an unreadable element section: {error}"
                        ))
                    })?;
                    match &element.kind {
                        wasmparser::ElementKind::Passive | wasmparser::ElementKind::Declared => {
                            return Err(ConvertError::invalid(format!(
                                "Wasm module {path:?} has a passive or declared element segment; \
                                 waffle-frontend currently drops those silently (plan Phase 0 \
                                 audit), so they are rejected until that is fixed"
                            )));
                        }
                        wasmparser::ElementKind::Active { offset_expr, .. } => {
                            reject_nonconst_offset(path, offset_expr, "element segment")?;
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Function imports are recorded and satisfied at merge time from the
    // FFI manifest (plan §5.4); nothing to reject here anymore.

    // Ingest into Waffle IR now, so ingestion errors surface at insertion
    // time and phase-2 merging works on owned, fully-expanded IR.
    let mut module =
        from_wasm_bytes(bytes, &FrontendOptions { debug: false }).map_err(|error| {
            ConvertError::invalid(format!(
                "Wasm module {path:?} failed Waffle ingestion: {error:#}"
            ))
        })?;
    if module.start_func.is_some() {
        return Err(ConvertError::invalid(format!(
            "Wasm module {path:?} has a start function; the merged module's initialization order \
             is owned by jsaw, so member start functions are rejected"
        )));
    }
    expand_all_funcs(&mut module).map_err(|error| {
        ConvertError::invalid(format!(
            "Wasm module {path:?} failed Waffle body expansion: {error:#}"
        ))
    })?;
    // The frontend cannot represent data/element segment index spaces, so
    // `array.new_data`/`array.new_elem`/`array.init_*` would silently
    // reference garbage (plan Phase 0 audit): reject members using them.
    for (_, decl) in module.funcs.entries() {
        if let FuncDecl::Body(_, _, body) = decl {
            for (_value, def) in body.values.entries() {
                if let portal_pc_waffle::ValueDef::Operator(op, _, _) = def {
                    use portal_pc_waffle::Operator as O;
                    if matches!(
                        op,
                        O::ArrayNewData { .. }
                            | O::ArrayNewElem { .. }
                            | O::ArrayInitData { .. }
                            | O::ArrayInitElem { .. }
                    ) {
                        return Err(ConvertError::invalid(format!(
                            "Wasm module {path:?} uses {op:?}, which references a data/element \
                             segment index space the Waffle frontend does not preserve (plan \
                             Phase 0 audit)"
                        )));
                    }
                }
            }
        }
    }
    let mut module = strip_to_static(path, module)?;

    // Late binding: convert eligible members to GC-native instance form.
    let instance = if eligible_for_late_binding(&module) {
        Some(instancify(path, &mut module)?)
    } else {
        None
    };

    Ok(WasmModulePlan {
        path: path.to_string(),
        profile,
        module,
        exports,
        imports,
        instance,
    })
}

/// Move a fully-expanded ingested module to `Module<'static>` without
/// `unsafe`: after [`expand_all_funcs`], no `FuncDecl::Lazy` remains, so no
/// value can still borrow the original bytes.
fn strip_to_static(path: &str, module: Module<'_>) -> Result<Module<'static>, ConvertError> {
    let mut stripped = Module::empty();
    for decl in module.funcs.into_vec() {
        let decl = match decl {
            FuncDecl::Import(sig, name) => FuncDecl::Import(sig, name),
            FuncDecl::Body(sig, name, body) => FuncDecl::Body(sig, name, body),
            FuncDecl::Compiled(sig, name, bytes) => FuncDecl::Compiled(sig, name, bytes),
            FuncDecl::None(_) => FuncDecl::None(PhantomData),
            // `Lazy` is feature-gated and impossible after
            // `expand_all_funcs`; the wildcard also covers it.
            other => {
                if matches!(other, FuncDecl::Lazy(..)) {
                    return Err(ConvertError::invalid(format!(
                        "Wasm module {path:?} retained a lazy body after expansion"
                    )));
                }
                return Err(ConvertError::invalid(format!(
                    "Wasm module {path:?} has an unhandled function declaration form"
                )));
            }
        };
        stripped.funcs.push(decl);
    }
    stripped.signatures = module.signatures;
    stripped.globals = module.globals;
    stripped.tables = module.tables;
    stripped.imports = module.imports;
    stripped.exports = module.exports;
    stripped.memories = module.memories;
    stripped.control_tags = module.control_tags;
    stripped.start_func = None;
    stripped.debug = module.debug;
    stripped.debug_map = module.debug_map;
    stripped.custom_sections = module.custom_sections;
    Ok(stripped)
}

/// Active segment offsets must be plain constants: the Waffle frontend
/// cannot rebase `global.get` bases or other init expressions (plan Phase 0
/// audit).
fn reject_nonconst_offset(
    path: &str,
    offset: &wasmparser::ConstExpr<'_>,
    what: &str,
) -> Result<(), ConvertError> {
    let mut operators = offset.get_operators_reader().into_iter();
    match (operators.next(), operators.next(), operators.next()) {
        (Some(Ok(op)), Some(Ok(wasmparser::Operator::End)), None)
            if matches!(
                op,
                wasmparser::Operator::I32Const { .. }
                    | wasmparser::Operator::I64Const { .. }
                    | wasmparser::Operator::F32Const { .. }
                    | wasmparser::Operator::F64Const { .. }
            ) =>
        {
            Ok(())
        }
        _ => Err(ConvertError::invalid(format!(
            "Wasm module {path:?} has a {what} with a non-constant offset expression; only \
             constant offsets can be rebased into the merged module"
        ))),
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    // Minimal Wasm binary builders (deterministic; no `wat` dependency).
    pub(crate) fn uleb(mut value: u64) -> Vec<u8> {
        let mut out = vec![];
        loop {
            let mut byte = (value & 0x7f) as u8;
            value >>= 7;
            if value != 0 {
                byte |= 0x80;
            }
            out.push(byte);
            if value == 0 {
                return out;
            }
        }
    }

    pub(crate) fn section(id: u8, payload: Vec<u8>) -> Vec<u8> {
        let mut out = vec![id];
        out.extend(uleb(payload.len() as u64));
        out.extend(payload);
        out
    }

    pub(crate) fn module(sections: &[Vec<u8>]) -> Vec<u8> {
        let mut out = b"\0asm\x01\0\0\0".to_vec();
        for s in sections {
            out.extend_from_slice(s);
        }
        out
    }

    pub(crate) fn vec_of(items: Vec<Vec<u8>>) -> Vec<u8> {
        let mut out = uleb(items.len() as u64);
        for item in items {
            out.extend(item);
        }
        out
    }

    pub(crate) fn func_type(params: &[u8], results: &[u8]) -> Vec<u8> {
        let mut out = vec![0x60];
        out.extend(uleb(params.len() as u64));
        out.extend_from_slice(params);
        out.extend(uleb(results.len() as u64));
        out.extend_from_slice(results);
        out
    }

    pub(crate) fn name(text: &str) -> Vec<u8> {
        let mut out = uleb(text.len() as u64);
        out.extend(text.as_bytes());
        out
    }

    pub(crate) const I32: u8 = 0x7f;

    pub(crate) fn code_body(body: &[u8]) -> Vec<u8> {
        let mut b = uleb(body.len() as u64);
        b.extend_from_slice(body);
        b
    }
    pub(crate) const I64: u8 = 0x7e;

    /// `(module (func (export "add") (param i32 i32) (result i32)
    ///   local.get 0 local.get 1 i32.add))`
    pub(crate) fn add_module() -> Vec<u8> {
        module(&[
            section(1, vec_of(vec![func_type(&[I32, I32], &[I32])])),
            section(3, vec_of(vec![uleb(0)])),
            section(
                7,
                vec_of(vec![{
                    let mut e = name("add");
                    e.push(0x00); // func
                    e.extend(uleb(0));
                    e
                }]),
            ),
            section(
                10,
                vec_of(vec![{
                    let body = [0x00, 0x20, 0x00, 0x20, 0x01, 0x6a, 0x0b];
                    let mut b = uleb(body.len() as u64);
                    b.extend_from_slice(&body);
                    b
                }]),
            ),
        ])
    }

    #[test]
    fn ingests_an_mvp_module_and_extracts_its_export_surface() {
        let plan = parse_wasm_module("math.wasm", &add_module(), WasmProfile::Mvp)
            .expect("an MVP module should validate at the mvp ceiling");
        assert_eq!(plan.profile, WasmProfile::Mvp);
        assert!(plan.imports.is_empty());
        let Some(WasmExport::Func { signature, .. }) = plan.exports.get("add") else {
            panic!("expected the add function export, got {:?}", plan.exports);
        };
        assert_eq!(
            signature,
            &WasmFuncSignature {
                params: vec![WasmValType::I32, WasmValType::I32],
                results: vec![WasmValType::I32],
            }
        );
    }

    #[test]
    fn component_binaries_get_a_named_diagnostic() {
        // Component Model header: \0asm, version 0x0d, layer 1.
        let bytes = b"\0asm\x0d\x00\x01\x00";
        let error = parse_wasm_module("comp.wasm", bytes, WasmProfile::Mvp)
            .expect_err("components are not core modules");
        assert!(error.message.contains("Component Model"), "{error}");
        assert!(error.message.contains("jsaw#5"), "{error}");
        // A core module header (layer 0) still parses normally.
        parse_wasm_module("math.wasm", &add_module(), WasmProfile::Mvp)
            .expect("core modules are unaffected");
    }

    #[test]
    fn rejects_corrupt_bytes() {
        let error = parse_wasm_module("bad.wasm", b"\0asm\x01\0\0", WasmProfile::Mvp)
            .expect_err("truncated bytes must be rejected");
        assert!(error.message.contains("bad.wasm"), "{error}");
    }

    #[test]
    fn classifies_and_gates_multi_value() {
        // (func (export "pair") (result i32 i64) i32.const 1 i64.const 2)
        let bytes = module(&[
            section(1, vec_of(vec![func_type(&[], &[I32, I64])])),
            section(3, vec_of(vec![uleb(0)])),
            section(
                7,
                vec_of(vec![{
                    let mut e = name("pair");
                    e.push(0x00);
                    e.extend(uleb(0));
                    e
                }]),
            ),
            section(
                10,
                vec_of(vec![{
                    let body = [0x00, 0x41, 0x01, 0x42, 0x02, 0x0b];
                    let mut b = uleb(body.len() as u64);
                    b.extend_from_slice(&body);
                    b
                }]),
            ),
        ]);
        let error = parse_wasm_module("pair.wasm", &bytes, WasmProfile::Mvp)
            .expect_err("multi-value must fail at the mvp ceiling");
        assert!(error.message.contains("multi-value"), "{error}");
        let plan = parse_wasm_module("pair.wasm", &bytes, WasmProfile::MultiValue)
            .expect("multi-value ceiling should accept the module");
        assert_eq!(plan.profile, WasmProfile::MultiValue);
        let Some(WasmExport::Func { signature, .. }) = plan.exports.get("pair") else {
            panic!("expected the pair function export");
        };
        assert_eq!(signature.results, vec![WasmValType::I32, WasmValType::I64]);
    }

    #[test]
    fn classifies_and_gates_memory64() {
        // (memory (export "mem") i64 1) — limits flags 0x04 = memory64.
        let bytes = module(&[
            section(5, vec_of(vec![vec![0x04, 0x01]])),
            section(
                7,
                vec_of(vec![{
                    let mut e = name("mem");
                    e.push(0x02); // memory
                    e.extend(uleb(0));
                    e
                }]),
            ),
        ]);
        let error = parse_wasm_module("m64.wasm", &bytes, WasmProfile::MultiValue)
            .expect_err("memory64 must fail below the memory64 ceiling");
        assert!(error.message.contains("memory64"), "{error}");
        let plan = parse_wasm_module("m64.wasm", &bytes, WasmProfile::Memory64)
            .expect("memory64 ceiling should accept the module");
        assert_eq!(plan.profile, WasmProfile::Memory64);
        assert_eq!(plan.exports.get("mem"), Some(&WasmExport::Memory));
    }

    #[test]
    fn classifies_and_gates_multi_memory() {
        // Two memories: (memory 1) (memory 1).
        let bytes = module(&[section(5, vec_of(vec![vec![0x00, 0x01], vec![0x00, 0x01]]))]);
        let error = parse_wasm_module("mm.wasm", &bytes, WasmProfile::Memory64)
            .expect_err("multi-memory must fail below the multi-memory ceiling");
        assert!(error.message.contains("multi-memory"), "{error}");
        let plan = parse_wasm_module("mm.wasm", &bytes, WasmProfile::MultiMemory)
            .expect("multi-memory ceiling should accept the module");
        assert_eq!(plan.profile, WasmProfile::MultiMemory);
    }

    #[test]
    fn rejects_non_function_imports() {
        // (import "env" "mem" (memory 1)) — memory imports of a Wasm member
        // are fail-closed (plan §5.4, portal-co/jsaw#10).
        let bytes = module(&[section(
            2,
            vec_of(vec![{
                let mut i = name("env");
                i.extend(name("mem"));
                i.push(0x02); // memory
                i.extend([0x00, 0x01]); // limits: min 1
                i
            }]),
        )]);
        let error = parse_wasm_module("imp.wasm", &bytes, WasmProfile::Mvp)
            .expect_err("non-function imports are rejected");
        assert!(error.message.contains("not a function"), "{error}");
    }

    #[test]
    fn rejects_non_funcref_tables() {
        // (table 1 externref) under the gc ceiling (reference types validate
        // there); the funcref-table guard must still reject it.
        let bytes = module(&[section(
            4,
            vec_of(vec![{
                let mut t = vec![0x6f]; // externref
                t.extend([0x00, 0x01]); // limits: min 1
                t
            }]),
        )]);
        let error = parse_wasm_module("tbl.wasm", &bytes, WasmProfile::Gc)
            .expect_err("non-funcref tables are rejected per the Phase 0 audit");
        assert!(error.message.contains("non-funcref table"), "{error}");
    }

    #[test]
    fn rejects_subtyped_gc_types() {
        // (type (sub (struct))) — non-final sub. Waffle drops subtyping
        // (is_final: true always), so this must fail closed.
        let bytes = module(&[section(1, vec_of(vec![vec![0x50, 0x00, 0x5f, 0x00]]))]);
        let error = parse_wasm_module("sub.wasm", &bytes, WasmProfile::Gc)
            .expect_err("subtyped members are rejected until waffle preserves subtyping");
        assert!(error.message.contains("subtype"), "{error}");
    }

    #[test]
    fn accepts_function_imports_with_signatures() {
        // (import "env" "f" (func (param i32) (result i32)))
        let sig = || vec![0x60, 0x01, 0x7f, 0x01, 0x7f];
        let bytes = module(&[
            section(1, vec_of(vec![sig()])),
            section(
                2,
                vec_of(vec![{
                    let mut i = name("env");
                    i.extend(name("f"));
                    i.push(0x00);
                    i.extend(uleb(0));
                    i
                }]),
            ),
        ]);
        let plan = parse_wasm_module("imp.wasm", &bytes, WasmProfile::Mvp)
            .expect("function imports are recorded for merge-time FFI satisfaction");
        assert_eq!(plan.imports.len(), 1);
        assert_eq!(plan.imports[0].module, "env");
        assert_eq!(plan.imports[0].field, "f");
        assert_eq!(plan.imports[0].signature.params, vec![WasmValType::I32]);
    }

    #[test]
    fn classifies_simd_and_tail_call_modules() {
        // (func (local i32) drop) with a v128.const: needs SIMD.
        let simd = module(&[
            section(1, vec_of(vec![func_type(&[], &[])])),
            section(3, vec_of(vec![uleb(0)])),
            section(
                10,
                vec_of(vec![code_body(&[
                    0x00, 0xfd, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1a, 0x0b,
                ])]),
            ),
        ]);
        let error = parse_wasm_module("s.wasm", &simd, WasmProfile::Gc)
            .expect_err("simd must fail below the simd ceiling");
        assert!(error.message.contains("simd"), "{error}");
        let plan = parse_wasm_module("s.wasm", &simd, WasmProfile::Simd)
            .expect("simd ceiling accepts");
        assert_eq!(plan.profile, WasmProfile::Simd);

        // return_call: needs tail-call.
        let tc = module(&[
            section(1, vec_of(vec![func_type(&[], &[])])),
            section(3, vec_of(vec![uleb(0)])),
            section(10, vec_of(vec![code_body(&[0x00, 0x12, 0x00, 0x0b])])),
        ]);
        let error = parse_wasm_module("t.wasm", &tc, WasmProfile::Simd)
            .expect_err("tail-call must fail below the tail-call ceiling");
        assert!(error.message.contains("tail-call"), "{error}");
        let plan = parse_wasm_module("t.wasm", &tc, WasmProfile::TailCall)
            .expect("tail-call ceiling accepts");
        assert_eq!(plan.profile, WasmProfile::TailCall);
    }

    #[test]
    fn classifies_gc_modules() {
        // (type (struct)) — a GC type section entry with no functions.
        let bytes = module(&[section(
            1,
            vec_of(vec![vec![0x5f, 0x00]]), // struct with 0 fields
        )]);
        let error = parse_wasm_module("gc.wasm", &bytes, WasmProfile::MultiMemory)
            .expect_err("gc must fail below the gc ceiling");
        assert!(error.message.contains("gc"), "{error}");
        let plan = parse_wasm_module("gc.wasm", &bytes, WasmProfile::Gc)
            .expect("the gc ceiling should accept a GC type section");
        assert_eq!(plan.profile, WasmProfile::Gc);
    }
}

// ---------------------------------------------------------------------
// Late binding: GC-native instance representation (portal-co/jsaw#6).
//
// A Wasm member without tables, `ref.func`, or indirect/reference calls is
// converted to a GC-native instance form: its globals become fields of an
// instance struct, each linear memory becomes a GC `(array (mut i8))`
// field, and every member function gains a trailing instance parameter.
// Instantiation is a synthesised function that allocates the struct and
// applies data segments as byte stores, so member state is late-bound
// (created at module init, or per `~wasm` `instantiate`) and multiple
// instances have disjoint state. Ineligible members keep the static merge
// path with exactly one implicit instance (today's behavior).

use portal_pc_waffle::{
    Block, BlockTarget, EntityRef, EntityVec, Func, FunctionBody, HeapType, Memory,
    MemoryArg, Operator, Signature, SignatureData, StorageType, Terminator, Type, Value,
    ValueDef, WithMutablility, WithNullable,
};

/// Layout of a late-bound member's instance representation.
#[derive(Clone, Debug)]
pub(crate) struct InstanceLayout {
    /// Member-local type index of the instance struct.
    pub(crate) ty: u32,
    /// Member-local func index of the synthesised instantiation function,
    /// `() -> (ref inst)`.
    pub(crate) instantiate: u32,
}

/// A member ineligible for late binding keeps the static single-instance
/// merge. Tables, `ref.func`, and call_ref/call_indirect signatures change
/// under the instance transform, so those members fail over to static.
fn eligible_for_late_binding(module: &Module<'_>) -> bool {
    if module.tables.len() != 0 {
        return false;
    }
    for (_, decl) in module.funcs.entries() {
        let FuncDecl::Body(_, _, body) = decl else {
            continue;
        };
        for (_value, def) in body.values.entries() {
            if let ValueDef::Operator(op, _, _) = def
                && matches!(
                    op,
                    Operator::RefFunc { .. }
                        | Operator::CallRef { .. }
                        | Operator::CallIndirect { .. }
                ) {
                    return false;
                }
        }
        for (_block, def) in body.blocks.entries() {
            if matches!(
                &def.terminator.terminator,
                Terminator::ReturnCall { .. }
                    | Terminator::ReturnCallIndirect { .. }
                    | Terminator::ReturnCallRef { .. }
            ) {
                return false;
            }
        }
    }
    true
}

/// Everything the body transform needs to resolve member state.
#[derive(Clone)]
struct InstanceCtx {
    inst_ty: Signature,
    inst_ref_null: Type,
    i8_array: Signature,
    /// Member global index → instance field index.
    global_field: Vec<usize>,
    /// Member memory index → instance field index.
    memory_field: Vec<usize>,
    /// Per memory: (memory64, max bytes).
    memory_limits: Vec<(bool, u64)>,
    /// Calls to indices below this are host imports: no instance argument.
    import_count: usize,
    inst_value: Value,
}

impl InstanceCtx {
    fn memory_field(&self, memory: Memory) -> usize {
        self.memory_field[memory.index()]
    }
}

/// Convert a member module to its GC-native instance form. Returns the
/// layout; the module is rewritten in place (globals and memories are
/// emptied — they live in instances now).
fn instancify(path: &str, module: &mut Module<'static>) -> Result<InstanceLayout, ConvertError> {
    let i8_array = module.signatures.push(SignatureData::Array {
        ty: WithMutablility {
            value: StorageType::I8,
            mutable: true,
        },
        shared: false,
    });
    let i8_array_null = Type::Heap(WithNullable {
        value: HeapType::Sig {
            sig_index: i8_array,
        },
        nullable: true,
    });
    let mut fields: Vec<WithMutablility<StorageType>> = vec![];
    let mut global_field = vec![];
    for global in module.globals.values() {
        global_field.push(fields.len());
        fields.push(WithMutablility {
            value: StorageType::Val(global.ty),
            mutable: global.mutable,
        });
    }
    let mut memory_field = vec![];
    let mut memory_limits = vec![];
    for memory in module.memories.values() {
        memory_field.push(fields.len());
        fields.push(WithMutablility {
            value: StorageType::Val(i8_array_null),
            mutable: true,
        });
        let max_pages = memory.maximum_pages.unwrap_or(if memory.memory64 {
            1 << 48
        } else {
            65536
        }) as u64;
        memory_limits.push((memory.memory64, max_pages.saturating_mul(65536)));
    }
    let inst_ty = module.signatures.push(SignatureData::Struct {
        fields,
        shared: false,
    });
    let inst_ref = Type::Heap(WithNullable {
        value: HeapType::Sig {
            sig_index: inst_ty,
        },
        nullable: false,
    });
    let inst_ref_null = Type::Heap(WithNullable {
        value: HeapType::Sig {
            sig_index: inst_ty,
        },
        nullable: true,
    });
    let import_count = module
        .funcs
        .values()
        .take_while(|decl| matches!(decl, FuncDecl::Import(..)))
        .count();

    // Every defined member function gains a trailing instance parameter.
    let func_ids: Vec<Func> = module.funcs.entries().map(|(f, _)| f).collect();
    let mut rewrites: Vec<(Func, Signature)> = vec![];
    for func in &func_ids {
        let FuncDecl::Body(sig, _, _) = &module.funcs[*func] else {
            continue;
        };
        let SignatureData::Func {
            params, returns, ..
        } = &module.signatures[*sig]
        else {
            return Err(ConvertError::invalid(format!(
                "Wasm module {path:?} function has a non-function signature"
            )));
        };
        let mut params = params.clone();
        params.push(inst_ref_null);
        let new_sig = module.signatures.push(SignatureData::Func {
            params,
            returns: returns.clone(),
            shared: false,
        });
        rewrites.push((*func, new_sig));
    }
    for (func, new_sig) in rewrites.drain(..) {
        let decl = std::mem::replace(&mut module.funcs[func], FuncDecl::None(PhantomData));
        let FuncDecl::Body(_, name, body) = decl else {
            unreachable!("filtered above");
        };
        let ctx = InstanceCtx {
            inst_ty,
            inst_ref_null,
            i8_array,
            global_field: global_field.clone(),
            memory_field: memory_field.clone(),
            memory_limits: memory_limits.clone(),
            import_count,
            inst_value: Value::invalid(),
        };
        let (new_body, _) = instancify_body(path, module, &body, new_sig, &ctx)?;
        module.funcs[func] = FuncDecl::Body(new_sig, name, new_body);
    }

    // The instantiation function: allocate the struct, initialise globals
    // from their constant inits, allocate memory arrays, apply data
    // segments as byte stores.
    let instantiate_sig = module.signatures.push(SignatureData::Func {
        params: vec![],
        returns: vec![inst_ref],
        shared: false,
    });
    let global_inits: Vec<(Type, Option<u64>)> = module
        .globals
        .values()
        .map(|global| (global.ty, global.value))
        .collect();
    let memory_inits: Vec<(u64, Vec<(usize, Vec<u8>)>)> = module
        .memories
        .values()
        .map(|memory| {
            (
                memory.initial_pages as u64 * 65536,
                memory
                    .segments
                    .iter()
                    .map(|segment| (segment.offset, segment.data.clone()))
                    .collect(),
            )
        })
        .collect();
    let mut body = FunctionBody::new(module, instantiate_sig);
    let block = body.entry;
    let mut field_values = vec![];
    for (ty, init) in global_inits {
        field_values.push(match ty {
            Type::I32 => body.add_op(
                block,
                Operator::I32Const {
                    value: init.unwrap_or(0) as u32,
                },
                &[],
                &[Type::I32],
            ),
            Type::I64 => body.add_op(
                block,
                Operator::I64Const {
                    value: init.unwrap_or(0),
                },
                &[],
                &[Type::I64],
            ),
            Type::F32 => body.add_op(
                block,
                Operator::F32Const {
                    value: init.unwrap_or(0) as u32,
                },
                &[],
                &[Type::F32],
            ),
            Type::F64 => body.add_op(
                block,
                Operator::F64Const {
                    value: init.unwrap_or(0),
                },
                &[],
                &[Type::F64],
            ),
            Type::Heap(heap) => body.add_op(
                block,
                Operator::RefNull { ty: Type::Heap(heap) },
                &[],
                &[Type::Heap(heap)],
            ),
            other => {
                return Err(ConvertError::invalid(format!(
                    "Wasm module {path:?} has a global of type {other:?} that cannot be                      initialised in an instance"
                )));
            }
        });
    }
    for (initial_bytes, segments) in &memory_inits {
        if *initial_bytes > i32::MAX as u64 {
            return Err(ConvertError::invalid(format!(
                "Wasm module {path:?} memory is too large for the GC-native representation                  ({initial_bytes} bytes > i32::MAX)"
            )));
        }
        let len = body.add_op(
            block,
            Operator::I32Const {
                value: *initial_bytes as u32,
            },
            &[],
            &[Type::I32],
        );
        let array = body.add_op(
            block,
            Operator::ArrayNewDefault { sig: i8_array },
            &[len],
            &[Type::Heap(WithNullable {
                value: HeapType::Sig {
                    sig_index: i8_array,
                },
                nullable: false,
            })],
        );
        for (offset, data) in segments {
            for (index, byte) in data.iter().enumerate() {
                let address = body.add_op(
                    block,
                    Operator::I32Const {
                        value: (offset + index) as u32,
                    },
                    &[],
                    &[Type::I32],
                );
                let value = body.add_op(
                    block,
                    Operator::I32Const {
                        value: *byte as u32,
                    },
                    &[],
                    &[Type::I32],
                );
                body.add_op(
                    block,
                    Operator::ArraySet { sig: i8_array },
                    &[array, address, value],
                    &[],
                );
            }
        }
        field_values.push(array);
    }
    let instance = body.add_op(
        block,
        Operator::StructNew { sig: inst_ty },
        &field_values,
        &[inst_ref],
    );
    body.set_terminator(block, Terminator::Return {
        values: vec![instance],
    });
    let instantiate = module.funcs.push(FuncDecl::Body(
        instantiate_sig,
        format!("{path}:__instantiate"),
        body,
    ));

    // Globals and memories live in instances now.
    module.memories = EntityVec::default();
    module.globals = EntityVec::default();

    Ok(InstanceLayout {
        ty: inst_ty.index() as u32,
        instantiate: instantiate.index() as u32,
    })
}

/// Rewrite one member body as a fresh function: global accesses become
/// instance field accesses, memory ops become byte-wise GC array accesses,
/// and internal calls thread the instance. The instance parameter is the
/// trailing parameter. A full rebuild (rather than in-place edits) keeps
/// CFG succ/pred bookkeeping trivially consistent across trap splits.
fn instancify_body(
    _path: &str,
    module: &Module<'static>,
    old: &FunctionBody,
    new_sig: Signature,
    ctx: &InstanceCtx,
) -> Result<(FunctionBody, Value), ConvertError> {
    let mut body = FunctionBody::new(module, new_sig);
    let mut values: std::collections::HashMap<Value, Value> = std::collections::HashMap::new();
    let mut block_map: std::collections::HashMap<Block, Block> = std::collections::HashMap::new();

    // Entry block params: the original params, then the instance.
    let new_entry: Vec<Value> = body.blocks[body.entry]
        .params
        .iter()
        .map(|(_, value)| *value)
        .collect();
    let inst_value = new_entry[old.n_params];
    for (index, (_, old_value)) in old.blocks[old.entry].params.iter().enumerate() {
        values.insert(*old_value, new_entry[index]);
    }
    block_map.insert(old.entry, body.entry);
    let ctx = InstanceCtx {
        inst_value,
        ..ctx.clone()
    };

    // Create the remaining blocks and their params.
    for (b, def) in old.blocks.entries() {
        if b == old.entry {
            continue;
        }
        let nb = body.add_block();
        block_map.insert(b, nb);
        for (ty, old_value) in &def.params {
            let nv = body.add_blockparam(nb, *ty);
            values.insert(*old_value, nv);
        }
    }

    // Fill each block.
    for (b, def) in old.blocks.entries() {
        let mut cur = block_map[&b];
        for record in &def.insts {
            let v = record.value;
            match &old.values[v] {
                ValueDef::BlockParam(..) | ValueDef::None | ValueDef::Placeholder(_) => {}
                ValueDef::PickOutput(from, index, ty) => {
                    let nv = body.add_value(ValueDef::PickOutput(values[from], *index, *ty));
                    body.append_to_block(cur, nv);
                    values.insert(v, nv);
                }
                ValueDef::Alias(to) => {
                    values.insert(v, values[to]);
                }
                ValueDef::Operator(op, args, tys) => {
                    let arg_vec: Vec<Value> = old.arg_pool[*args]
                        .iter()
                        .map(|a| values[a])
                        .collect();
                    let ty_vec: Vec<Type> = old.type_pool[*tys].to_vec();
                    match op {
                        Operator::GlobalGet { global_index } => {
                            let nv = body.add_op(
                                cur,
                                Operator::StructGet {
                                    sig: ctx.inst_ty,
                                    idx: ctx.global_field[global_index.index()],
                                },
                                &[ctx.inst_value],
                                &ty_vec,
                            );
                            values.insert(v, nv);
                        }
                        Operator::GlobalSet { global_index } => {
                            let nv = body.add_op(
                                cur,
                                Operator::StructSet {
                                    sig: ctx.inst_ty,
                                    idx: ctx.global_field[global_index.index()],
                                },
                                &[ctx.inst_value, arg_vec[0]],
                                &[],
                            );
                            values.insert(v, nv);
                        }
                        Operator::Call { function_index } => {
                            let mut a = arg_vec.clone();
                            if function_index.index() >= ctx.import_count {
                                a.push(ctx.inst_value);
                            }
                            let nv = body.add_op(
                                cur,
                                Operator::Call {
                                    function_index: *function_index,
                                },
                                &a,
                                &ty_vec,
                            );
                            values.insert(v, nv);
                        }
                        op if memory_load(op).is_some() => {
                            let (width, signed, result_ty) = memory_load(op).expect("matched");
                            let memarg = match op {
                                Operator::I32Load { memory }
                                | Operator::I64Load { memory }
                                | Operator::F32Load { memory }
                                | Operator::F64Load { memory }
                                | Operator::I32Load8S { memory }
                                | Operator::I32Load8U { memory }
                                | Operator::I32Load16S { memory }
                                | Operator::I32Load16U { memory }
                                | Operator::I64Load8S { memory }
                                | Operator::I64Load8U { memory }
                                | Operator::I64Load16S { memory }
                                | Operator::I64Load16U { memory }
                                | Operator::I64Load32S { memory }
                                | Operator::I64Load32U { memory } => *memory,
                                _ => unreachable!(),
                            };
                            let (nb, nv) = expand_memory_load(
                                &mut body, cur, arg_vec[0], memarg, width, signed,
                                result_ty, &ctx,
                            )?;
                            cur = nb;
                            values.insert(v, nv);
                        }
                        op if memory_store(op).is_some() => {
                            let (width, value_ty) = memory_store(op).expect("matched");
                            let memarg = match op {
                                Operator::I32Store { memory }
                                | Operator::I64Store { memory }
                                | Operator::F32Store { memory }
                                | Operator::F64Store { memory }
                                | Operator::I32Store8 { memory }
                                | Operator::I32Store16 { memory }
                                | Operator::I64Store8 { memory }
                                | Operator::I64Store16 { memory }
                                | Operator::I64Store32 { memory } => *memory,
                                _ => unreachable!(),
                            };
                            cur = expand_memory_store(
                                &mut body, cur, arg_vec[0], arg_vec[1], memarg, width, value_ty,
                                &ctx,
                            );
                            // Stores produce no result; nothing to map.
                        }
                        Operator::MemorySize { mem } => {
                            let nv = expand_memory_size(&mut body, cur, *mem, &ctx);
                            values.insert(v, nv);
                        }
                        Operator::MemoryGrow { mem } => {
                            let (nb, nv) =
                                expand_memory_grow(&mut body, cur, arg_vec[0], *mem, &ctx)?;
                            cur = nb;
                            values.insert(v, nv);
                        }
                        _ => {
                            let nv = body.add_op(cur, *op, &arg_vec, &ty_vec);
                            values.insert(v, nv);
                        }
                    }
                }
            }
        }
        let terminator = map_terminator(&def.terminator.terminator, &values, &block_map);
        body.set_terminator(cur, terminator);
    }
    Ok((body, inst_value))
}

/// Map a terminator's block targets and values into the rebuilt body.
fn map_terminator(
    term: &Terminator,
    values: &std::collections::HashMap<Value, Value>,
    block_map: &std::collections::HashMap<Block, Block>,
) -> Terminator {
    let target = |t: &BlockTarget| BlockTarget {
        block: block_map[&t.block],
        args: t.args.iter().map(|a| values[a]).collect(),
    };
    match term {
        Terminator::Br { target: t } => Terminator::Br { target: target(t) },
        Terminator::CondBr {
            cond,
            if_true,
            if_false,
        } => Terminator::CondBr {
            cond: values[cond],
            if_true: target(if_true),
            if_false: target(if_false),
        },
        Terminator::Select {
            value,
            targets,
            default,
        } => Terminator::Select {
            value: values[value],
            targets: targets.iter().map(&target).collect(),
            default: target(default),
        },
        Terminator::Return { values: vals } => Terminator::Return {
            values: vals.iter().map(|v| values[v]).collect(),
        },
        // Rejected by the eligibility check.
        Terminator::ReturnCall { .. }
        | Terminator::ReturnCallIndirect { .. }
        | Terminator::ReturnCallRef { .. } => {
            unreachable!("tail/reference calls are ineligible for late binding")
        }
        Terminator::Unreachable => Terminator::Unreachable,
        Terminator::UB => Terminator::UB,
        Terminator::None => Terminator::None,
        _ => unreachable!("non-exhaustive terminator"),
    }
}
/// Load/store op metadata: (access width in bytes, sign extension, value
/// type).
fn memory_load(op: &Operator) -> Option<(u32, Option<bool>, Type)> {
    use Operator as O;
    Some(match op {
        O::I32Load { .. } => (4, None, Type::I32),
        O::I64Load { .. } => (8, None, Type::I64),
        O::F32Load { .. } => (4, None, Type::F32),
        O::F64Load { .. } => (8, None, Type::F64),
        O::I32Load8S { .. } => (1, Some(true), Type::I32),
        O::I32Load8U { .. } => (1, Some(false), Type::I32),
        O::I32Load16S { .. } => (2, Some(true), Type::I32),
        O::I32Load16U { .. } => (2, Some(false), Type::I32),
        O::I64Load8S { .. } => (1, Some(true), Type::I64),
        O::I64Load8U { .. } => (1, Some(false), Type::I64),
        O::I64Load16S { .. } => (2, Some(true), Type::I64),
        O::I64Load16U { .. } => (2, Some(false), Type::I64),
        O::I64Load32S { .. } => (4, Some(true), Type::I64),
        O::I64Load32U { .. } => (4, Some(false), Type::I64),
        _ => return None,
    })
}

/// Store op metadata: (access width in bytes, value type).
fn memory_store(op: &Operator) -> Option<(u32, Type)> {
    use Operator as O;
    Some(match op {
        O::I32Store { .. } => (4, Type::I32),
        O::I64Store { .. } => (8, Type::I64),
        O::F32Store { .. } => (4, Type::F32),
        O::F64Store { .. } => (8, Type::F64),
        O::I32Store8 { .. } => (1, Type::I32),
        O::I32Store16 { .. } => (2, Type::I32),
        O::I64Store8 { .. } => (1, Type::I64),
        O::I64Store16 { .. } => (2, Type::I64),
        O::I64Store32 { .. } => (4, Type::I64),
        _ => return None,
    })
}

/// The instance's byte array for `memory`.
fn instance_memory_array(
    body: &mut FunctionBody,
    block: Block,
    memory: Memory,
    ctx: &InstanceCtx,
) -> Value {
    body.add_op(
        block,
        Operator::StructGet {
            sig: ctx.inst_ty,
            idx: ctx.memory_field(memory),
        },
        &[ctx.inst_value],
        &[Type::Heap(WithNullable {
            value: HeapType::Sig {
                sig_index: ctx.i8_array,
            },
            nullable: true,
        })],
    )
}

/// Effective address (i64) = address + immediate offset, plus a bounds
/// check that traps out of bounds (splitting the block). Returns the
/// continuation block and the i64 effective address.
fn effective_address(
    body: &mut FunctionBody,
    block: Block,
    addr: Value,
    memarg: &MemoryArg,
    width: u32,
    array: Value,
    memory64: bool,
) -> (Block, Value) {
    let wide_addr = if memory64 {
        addr
    } else {
        body.add_op(block, Operator::I64ExtendI32U, &[addr], &[Type::I64])
    };
    let offset = body.add_op(
        block,
        Operator::I64Const { value: memarg.offset },
        &[],
        &[Type::I64],
    );
    let ea = body.add_op(block, Operator::I64Add, &[wide_addr, offset], &[Type::I64]);
    let width_value = body.add_op(
        block,
        Operator::I64Const {
            value: width as u64,
        },
        &[],
        &[Type::I64],
    );
    let limit = body.add_op(block, Operator::I64Add, &[ea, width_value], &[Type::I64]);
    let len = body.add_op(block, Operator::ArrayLen, &[array], &[Type::I32]);
    let wide_len = body.add_op(block, Operator::I64ExtendI32U, &[len], &[Type::I64]);
    let out_of_bounds = body.add_op(block, Operator::I64GtU, &[limit, wide_len], &[Type::I32]);
    let trap = body.add_block();
    let cont = body.add_block();
    body.set_terminator(
        block,
        Terminator::CondBr {
            cond: out_of_bounds,
            if_true: BlockTarget {
                block: trap,
                args: vec![],
            },
            if_false: BlockTarget {
                block: cont,
                args: vec![],
            },
        },
    );
    body.set_terminator(trap, Terminator::Unreachable);
    (cont, ea)
}

/// One byte read from the instance memory array at `ea + index`.
fn read_byte(
    body: &mut FunctionBody,
    block: Block,
    array: Value,
    ea: Value,
    index: u32,
    ctx: &InstanceCtx,
) -> Value {
    let address = if index == 0 {
        body.add_op(block, Operator::I32WrapI64, &[ea], &[Type::I32])
    } else {
        let offset = body.add_op(
            block,
            Operator::I64Const {
                value: index as u64,
            },
            &[],
            &[Type::I64],
        );
        let wide = body.add_op(block, Operator::I64Add, &[ea, offset], &[Type::I64]);
        body.add_op(block, Operator::I32WrapI64, &[wide], &[Type::I32])
    };
    body.add_op(
        block,
        Operator::ArrayGetU {
            sig: ctx.i8_array,
        },
        &[array, address],
        &[Type::I32],
    )
}

/// Assemble bytes little-endian into an i32.
fn assemble_i32(body: &mut FunctionBody, block: Block, bytes: &[Value]) -> Value {
    let mut acc = bytes[0];
    for (i, byte) in bytes.iter().enumerate().skip(1) {
        let shift = body.add_op(
            block,
            Operator::I32Const {
                value: (i * 8) as u32,
            },
            &[],
            &[Type::I32],
        );
        let shifted = body.add_op(block, Operator::I32Shl, &[*byte, shift], &[Type::I32]);
        acc = body.add_op(block, Operator::I32Or, &[acc, shifted], &[Type::I32]);
    }
    acc
}

/// Assemble `bytes` little-endian into an i64.
fn assemble_i64(body: &mut FunctionBody, block: Block, bytes: &[Value]) -> Value {
    let mut acc: Option<Value> = None;
    for (i, byte) in bytes.iter().enumerate() {
        let wide = body.add_op(block, Operator::I64ExtendI32U, &[*byte], &[Type::I64]);
        let shifted = if i == 0 {
            wide
        } else {
            let shift = body.add_op(
                block,
                Operator::I64Const {
                    value: (i * 8) as u64,
                },
                &[],
                &[Type::I64],
            );
            body.add_op(block, Operator::I64Shl, &[wide, shift], &[Type::I64])
        };
        acc = Some(match acc {
            None => shifted,
            Some(acc) => body.add_op(block, Operator::I64Or, &[acc, shifted], &[Type::I64]),
        });
    }
    acc.expect("at least one byte")
}

/// Sign-extend an i32 of `width` bytes to full width.
fn sign_extend_i32(body: &mut FunctionBody, block: Block, value: Value, width: u32) -> Value {
    let shift = body.add_op(
        block,
        Operator::I32Const {
            value: (32 - width * 8),
        },
        &[],
        &[Type::I32],
    );
    let shifted = body.add_op(block, Operator::I32Shl, &[value, shift], &[Type::I32]);
    body.add_op(block, Operator::I32ShrS, &[shifted, shift], &[Type::I32])
}

/// Sign-extend an i64 of `width` bytes to full width.
fn sign_extend_i64(body: &mut FunctionBody, block: Block, value: Value, width: u32) -> Value {
    let shift = body.add_op(
        block,
        Operator::I64Const {
            value: (64 - width * 8) as u64,
        },
        &[],
        &[Type::I64],
    );
    let shifted = body.add_op(block, Operator::I64Shl, &[value, shift], &[Type::I64]);
    body.add_op(block, Operator::I64ShrS, &[shifted, shift], &[Type::I64])
}

/// Expand a memory load into instance-array byte reads (little-endian).
/// Returns the continuation block (bounds-check split) and the result.
fn expand_memory_load(
    body: &mut FunctionBody,
    block: Block,
    addr: Value,
    memarg: MemoryArg,
    width: u32,
    signed: Option<bool>,
    result_ty: Type,
    ctx: &InstanceCtx,
) -> Result<(Block, Value), ConvertError> {
    let memory64 = ctx.memory_limits[memarg.memory.index()].0;
    let array = instance_memory_array(body, block, memarg.memory, ctx);
    let (cur, ea) = effective_address(body, block, addr, &memarg, width, array, memory64);
    let bytes: Vec<Value> = (0..width)
        .map(|i| read_byte(body, cur, array, ea, i, ctx))
        .collect();
    let result = match (result_ty, signed) {
        (Type::I32, None) | (Type::I32, Some(false)) => assemble_i32(body, cur, &bytes),
        (Type::I32, Some(true)) => {
            let raw = assemble_i32(body, cur, &bytes);
            sign_extend_i32(body, cur, raw, width)
        }
        (Type::I64, None) | (Type::I64, Some(false)) => assemble_i64(body, cur, &bytes),
        (Type::I64, Some(true)) => {
            let raw = assemble_i64(body, cur, &bytes);
            sign_extend_i64(body, cur, raw, width)
        }
        (Type::F32, None) => {
            let raw = assemble_i32(body, cur, &bytes);
            body.add_op(cur, Operator::F32ReinterpretI32, &[raw], &[Type::F32])
        }
        (Type::F64, None) => {
            let raw = assemble_i64(body, cur, &bytes);
            body.add_op(cur, Operator::F64ReinterpretI64, &[raw], &[Type::F64])
        }
        _ => unreachable!(),
    };
    Ok((cur, result))
}

/// Expand a memory store into instance-array byte writes (little-endian).
/// Returns the continuation block.
fn expand_memory_store(
    body: &mut FunctionBody,
    block: Block,
    addr: Value,
    stored: Value,
    memarg: MemoryArg,
    width: u32,
    value_ty: Type,
    ctx: &InstanceCtx,
) -> Block {
    let memory64 = ctx.memory_limits[memarg.memory.index()].0;
    let array = instance_memory_array(body, block, memarg.memory, ctx);
    let (cur, ea) = effective_address(body, block, addr, &memarg, width, array, memory64);
    // Normalise the stored value to raw bits.
    let bits = match value_ty {
        Type::I32 | Type::I64 => stored,
        Type::F32 => body.add_op(cur, Operator::I32ReinterpretF32, &[stored], &[Type::I32]),
        Type::F64 => body.add_op(cur, Operator::I64ReinterpretF64, &[stored], &[Type::I64]),
        _ => unreachable!(),
    };
    for i in 0..width {
        let byte = if value_ty == Type::I32 {
            let shifted = if i == 0 {
                bits
            } else {
                let shift = body.add_op(
                    cur,
                    Operator::I32Const {
                        value: (i * 8),
                    },
                    &[],
                    &[Type::I32],
                );
                body.add_op(cur, Operator::I32ShrU, &[bits, shift], &[Type::I32])
            };
            let mask = body.add_op(cur, Operator::I32Const { value: 0xff }, &[], &[Type::I32]);
            body.add_op(cur, Operator::I32And, &[shifted, mask], &[Type::I32])
        } else {
            let shifted = if i == 0 {
                bits
            } else {
                let shift = body.add_op(
                    cur,
                    Operator::I64Const {
                        value: (i * 8) as u64,
                    },
                    &[],
                    &[Type::I64],
                );
                body.add_op(cur, Operator::I64ShrU, &[bits, shift], &[Type::I64])
            };
            let mask = body.add_op(cur, Operator::I64Const { value: 0xff }, &[], &[Type::I64]);
            let masked = body.add_op(cur, Operator::I64And, &[shifted, mask], &[Type::I64]);
            body.add_op(cur, Operator::I32WrapI64, &[masked], &[Type::I32])
        };
        write_byte(body, cur, array, ea, i, byte, ctx);
    }
    cur
}

/// One byte written to the instance memory array at `ea + index`.
fn write_byte(
    body: &mut FunctionBody,
    block: Block,
    array: Value,
    ea: Value,
    index: u32,
    byte: Value,
    ctx: &InstanceCtx,
) {
    let address = if index == 0 {
        body.add_op(block, Operator::I32WrapI64, &[ea], &[Type::I32])
    } else {
        let offset = body.add_op(
            block,
            Operator::I64Const {
                value: index as u64,
            },
            &[],
            &[Type::I64],
        );
        let wide = body.add_op(block, Operator::I64Add, &[ea, offset], &[Type::I64]);
        body.add_op(block, Operator::I32WrapI64, &[wide], &[Type::I32])
    };
    body.add_op(
        block,
        Operator::ArraySet {
            sig: ctx.i8_array,
        },
        &[array, address, byte],
        &[],
    );
}

/// Expand `memory.size` against the instance array (current pages).
fn expand_memory_size(
    body: &mut FunctionBody,
    block: Block,
    memory: Memory,
    ctx: &InstanceCtx,
) -> Value {
    let array = instance_memory_array(body, block, memory, ctx);
    let len = body.add_op(block, Operator::ArrayLen, &[array], &[Type::I32]);
    let shift = body.add_op(block, Operator::I32Const { value: 16 }, &[], &[Type::I32]);
    let pages32 = body.add_op(block, Operator::I32ShrU, &[len, shift], &[Type::I32]);
    if ctx.memory_limits[memory.index()].0 {
        body.add_op(block, Operator::I64ExtendI32U, &[pages32], &[Type::I64])
    } else {
        pages32
    }
}

/// Expand `memory.grow`: allocate a bigger array, copy, store back.
/// Returns the continuation block and the previous size in pages (or -1).
fn expand_memory_grow(
    body: &mut FunctionBody,
    block: Block,
    delta: Value,
    memory: Memory,
    ctx: &InstanceCtx,
) -> Result<(Block, Value), ConvertError> {
    let (memory64, max_bytes) = ctx.memory_limits[memory.index()];
    let array = instance_memory_array(body, block, memory, ctx);
    let old_bytes = body.add_op(block, Operator::ArrayLen, &[array], &[Type::I32]);
    let wide_old = body.add_op(block, Operator::I64ExtendI32U, &[old_bytes], &[Type::I64]);
    let wide_delta = if memory64 {
        delta
    } else {
        body.add_op(block, Operator::I64ExtendI32U, &[delta], &[Type::I64])
    };
    let page_const = body.add_op(
        block,
        Operator::I64Const { value: 65536 },
        &[],
        &[Type::I64],
    );
    let delta_bytes = body.add_op(
        block,
        Operator::I64Mul,
        &[wide_delta, page_const],
        &[Type::I64],
    );
    let new_bytes = body.add_op(
        block,
        Operator::I64Add,
        &[wide_old, delta_bytes],
        &[Type::I64],
    );
    // Failure also covers arrays larger than i32::MAX (GC array length is
    // an i32), whichever limit is smaller.
    let cap = max_bytes.min(i32::MAX as u64);
    let max_value = body.add_op(block, Operator::I64Const { value: cap }, &[], &[Type::I64]);
    let too_big = body.add_op(block, Operator::I64GtU, &[new_bytes, max_value], &[Type::I32]);
    let fail = body.add_block();
    let ok = body.add_block();
    let join = body.add_block();
    body.set_terminator(
        block,
        Terminator::CondBr {
            cond: too_big,
            if_true: BlockTarget {
                block: fail,
                args: vec![],
            },
            if_false: BlockTarget {
                block: ok,
                args: vec![],
            },
        },
    );
    // Success: new array, copy, store back, return old pages.
    let narrow_new = body.add_op(ok, Operator::I32WrapI64, &[new_bytes], &[Type::I32]);
    let new_array = body.add_op(
        ok,
        Operator::ArrayNewDefault {
            sig: ctx.i8_array,
        },
        &[narrow_new],
        &[Type::Heap(WithNullable {
            value: HeapType::Sig {
                sig_index: ctx.i8_array,
            },
            nullable: false,
        })],
    );
    let zero = body.add_op(ok, Operator::I32Const { value: 0 }, &[], &[Type::I32]);
    body.add_op(
        ok,
        Operator::ArrayCopy {
            dest: ctx.i8_array,
            src: ctx.i8_array,
        },
        &[new_array, zero, array, zero, old_bytes],
        &[],
    );
    body.add_op(
        ok,
        Operator::StructSet {
            sig: ctx.inst_ty,
            idx: ctx.memory_field(memory),
        },
        &[ctx.inst_value, new_array],
        &[],
    );
    let shift64 = body.add_op(ok, Operator::I64Const { value: 16 }, &[], &[Type::I64]);
    let old_pages_wide = body.add_op(ok, Operator::I64ShrU, &[wide_old, shift64], &[Type::I64]);
    let ok_result = if memory64 {
        old_pages_wide
    } else {
        body.add_op(ok, Operator::I32WrapI64, &[old_pages_wide], &[Type::I32])
    };
    body.set_terminator(
        ok,
        Terminator::Br {
            target: BlockTarget {
                block: join,
                args: vec![ok_result],
            },
        },
    );
    // Failure: -1.
    let fail_result = if memory64 {
        body.add_op(fail, Operator::I64Const { value: u64::MAX }, &[], &[Type::I64])
    } else {
        body.add_op(
            fail,
            Operator::I32Const { value: u32::MAX },
            &[],
            &[Type::I32],
        )
    };
    body.set_terminator(
        fail,
        Terminator::Br {
            target: BlockTarget {
                block: join,
                args: vec![fail_result],
            },
        },
    );
    let result_ty = if memory64 { Type::I64 } else { Type::I32 };
    let join_param = body.add_blockparam(join, result_ty);
    Ok((join, join_param))
}
