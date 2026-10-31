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
}

impl WasmProfile {
    /// All profiles in cumulative order, lowest first.
    pub(crate) const ALL: [WasmProfile; 5] = [
        WasmProfile::Mvp,
        WasmProfile::MultiValue,
        WasmProfile::Memory64,
        WasmProfile::MultiMemory,
        WasmProfile::Gc,
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
        features
    }

    pub(crate) fn name(self) -> &'static str {
        match self {
            WasmProfile::Mvp => "mvp",
            WasmProfile::MultiValue => "multi-value",
            WasmProfile::Memory64 => "memory64",
            WasmProfile::MultiMemory => "multi-memory",
            WasmProfile::Gc => "gc",
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
    /// Any reference type. The concrete heap type is recorded in the GC
    /// phase (plan §5.2); until then this stays an opaque boundary token.
    Ref,
}

impl WasmValType {
    fn from_wasmparser(ty: ValType) -> Self {
        match ty {
            ValType::I32 => WasmValType::I32,
            ValType::I64 => WasmValType::I64,
            ValType::F32 => WasmValType::F32,
            ValType::F64 => WasmValType::F64,
            ValType::V128 => WasmValType::V128,
            ValType::Ref(_) => WasmValType::Ref,
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
    Global,
    Table,
    Tag,
}

/// One entry of a Wasm module's import section.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct WasmImport {
    pub(crate) module: String,
    pub(crate) field: String,
    pub(crate) kind: &'static str,
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

    let mut func_types: Vec<WasmFuncSignature> = Vec::new();
    let mut exports: BTreeMap<String, WasmExport> = BTreeMap::new();
    let mut imports: Vec<WasmImport> = Vec::new();
    // Type indices of imported functions come before defined ones; the
    // export section references the combined function index space.
    let mut imported_func_types: Vec<u32> = Vec::new();
    let mut defined_func_types: Vec<u32> = Vec::new();
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
                        let wasmparser::CompositeInnerType::Func(func) = &sub.composite_type.inner
                        else {
                            // GC struct/array types: validated already; only
                            // function types are referenced by exports in
                            // this phase.
                            continue;
                        };
                        func_types.push(WasmFuncSignature {
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
                        });
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
                    if let TypeRef::Func(ty) = import.ty {
                        imported_func_types.push(ty);
                    }
                    imports.push(WasmImport {
                        module: import.module.to_string(),
                        field: import.name.to_string(),
                        kind: match import.ty {
                            TypeRef::Func(_) => "function",
                            TypeRef::Table(_) => "table",
                            TypeRef::Memory(_) => "memory",
                            TypeRef::Global(_) => "global",
                            TypeRef::Tag(_) => "tag",
                        },
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
                            WasmExport::Func {
                                index: export.index,
                                signature: func_types.get(ty as usize).cloned().ok_or_else(
                                    || {
                                        ConvertError::invalid(format!(
                                            "Wasm module {path:?} export {:?} references an \
                                             unknown type index {ty}",
                                            export.name
                                        ))
                                    },
                                )?,
                            }
                        }
                        ExternalKind::Memory => WasmExport::Memory,
                        ExternalKind::Global => WasmExport::Global,
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

    if !imports.is_empty() {
        let summary: Vec<String> = imports
            .iter()
            .map(|import| format!("{}.{:?} ({})", import.module, import.field, import.kind))
            .collect();
        return Err(ConvertError::invalid(format!(
            "Wasm module {path:?} declares imports ({}) which cannot be resolved yet; import \
             satisfaction via the FFI manifest or intra-set edges is a later wasm-esm phase \
             (plan §5.4)",
            summary.join(", "),
        )));
    }

    // Ingest into Waffle IR now, so ingestion errors surface at insertion
    // time and phase-2 merging works on owned, fully-expanded IR.
    let mut module = from_wasm_bytes(bytes, &FrontendOptions { debug: false }).map_err(|error| {
        ConvertError::invalid(format!("Wasm module {path:?} failed Waffle ingestion: {error:#}"))
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
    let module = strip_to_static(path, module)?;

    Ok(WasmModulePlan {
        path: path.to_string(),
        profile,
        module,
        exports,
        imports,
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
    fn rejects_modules_with_imports_for_now() {
        // (import "env" "f" (func)) — plan §5.4 import resolution is a
        // later phase.
        let bytes = module(&[
            section(1, vec_of(vec![func_type(&[], &[])])),
            section(
                2,
                vec_of(vec![{
                    let mut i = name("env");
                    i.extend(name("f"));
                    i.push(0x00); // func
                    i.extend(uleb(0));
                    i
                }]),
            ),
        ]);
        let error = parse_wasm_module("imp.wasm", &bytes, WasmProfile::Mvp)
            .expect_err("imports are not resolvable yet");
        assert!(error.message.contains("env"), "{error}");
        assert!(error.message.contains("cannot be resolved"), "{error}");
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
