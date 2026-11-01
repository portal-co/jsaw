//! Multi-module ingestion: the [`ModuleSet`] plus relative-path resolution
//! and export-surface flattening for [`convert_modules`](crate::convert_modules).
//!
//! A module set is a closed world: every import specifier must resolve, by
//! lexical relative-path normalization, to another key in the set. Import
//! linking (the `LoadId` integration) lives in `conv.rs`; this module owns
//! everything that only needs the [`SModule`] metadata (imports, exports,
//! hoisted function declarations).

use std::collections::{BTreeMap, BTreeSet};

use portal_jsc_swc_ssa::{
    SFunc,
    module::{ExportSpec, SModule},
};
use portal_jsc_swc_util::{ImportMap, ImportMapper};
use swc_ecma_ast::{Id as Ident, ImportSpecifier};

use crate::{
    repr::ConvertError,
    wasm_boundary::{WasmImportSpec, parse_wasm_import_specifier},
    wasm_esm::{WasmModuleImportSpec, WasmModulePlan, WasmProfile},
};

/// One member of the closed module set: a JavaScript ES module lowered to
/// SSA, or a validated Wasm binary module (wasm-esm, portal-co/jsaw#14).
pub(crate) enum ModuleMember<'s> {
    /// A normal JavaScript module.
    Js(&'s SModule),
    /// A Wasm binary module; its type section is the ABI declaration.
    Wasm(WasmModulePlan),
}

/// A closed set of ES modules that [`convert_modules`](crate::convert_modules)
/// can link together into one WasmGC module.
///
/// Keys are module paths as provided by the embedder (e.g. `"src/main.js"`).
/// Relative import specifiers resolve against the importing module's key:
/// `import … from './lib.js'` in `src/main.js` looks up `src/lib.js`.
/// Keys ending in `.wasm` name Wasm binary members (see
/// [`ModuleSet::insert_wasm`]).
///
/// Deterministic ordering ([`BTreeMap`]) keeps lowering and generated Wasm
/// reproducible regardless of insertion order.
#[derive(Default)]
pub struct ModuleSet<'s> {
    modules: BTreeMap<String, ModuleMember<'s>>,
}

impl<'s> ModuleSet<'s> {
    pub fn new() -> Self {
        Self {
            modules: BTreeMap::new(),
        }
    }

    /// Insert a JavaScript module under `path`. Duplicate paths are a
    /// [`ConvertError`]. `.wasm` keys are reserved for Wasm binary members
    /// and must go through [`ModuleSet::insert_wasm`], keeping the member
    /// sum explicit.
    pub fn insert(
        &mut self,
        path: impl Into<String>,
        module: &'s SModule,
    ) -> Result<(), ConvertError> {
        let path = path.into();
        if path.ends_with(".wasm") {
            return Err(ConvertError::invalid(format!(
                "module path {path:?} ends in .wasm: Wasm binary members must be inserted with \
                 `ModuleSet::insert_wasm` (wasm-esm, portal-co/jsaw#14)"
            )));
        }
        if self
            .modules
            .insert(path.clone(), ModuleMember::Js(module))
            .is_some()
        {
            return Err(ConvertError::invalid(format!(
                "duplicate module path {path:?} in the module set"
            )));
        }
        Ok(())
    }

    /// Insert a Wasm binary module under `path`, validated at the MVP
    /// profile ceiling (plan §3.3). The binary's type section becomes the
    /// ABI declaration for named imports from JavaScript.
    pub fn insert_wasm(
        &mut self,
        path: impl Into<String>,
        bytes: &[u8],
    ) -> Result<(), ConvertError> {
        self.insert_wasm_with_profile(path, bytes, WasmProfile::Mvp)
    }

    /// [`ModuleSet::insert_wasm`] with an explicit profile ceiling.
    pub fn insert_wasm_with_profile(
        &mut self,
        path: impl Into<String>,
        bytes: &[u8],
        ceiling: WasmProfile,
    ) -> Result<(), ConvertError> {
        let path = path.into();
        if !path.ends_with(".wasm") {
            return Err(ConvertError::invalid(format!(
                "Wasm binary member {path:?} must use a .wasm module-set key so relative \
                 specifiers classify unambiguously"
            )));
        }
        let plan = crate::wasm_esm::parse_wasm_module(&path, bytes, ceiling)?;
        if self
            .modules
            .insert(path.clone(), ModuleMember::Wasm(plan))
            .is_some()
        {
            return Err(ConvertError::invalid(format!(
                "duplicate module path {path:?} in the module set"
            )));
        }
        Ok(())
    }

    /// A one-module set, the shape [`convert_module`](crate::convert_module)
    /// accepts.
    pub fn single(path: impl Into<String>, module: &'s SModule) -> Self {
        let mut set = Self::new();
        set.modules.insert(path.into(), ModuleMember::Js(module));
        set
    }

    /// The JavaScript module under `path`. A Wasm binary member under the
    /// same key is a distinct error: callers that iterate every key must
    /// use [`ModuleSet::js_modules`] instead of assuming all members are
    /// JavaScript.
    pub(crate) fn get(&self, path: &str) -> Result<&'s SModule, ConvertError> {
        match self.modules.get(path) {
            Some(ModuleMember::Js(module)) => Ok(module),
            Some(ModuleMember::Wasm(_)) => Err(ConvertError::invalid(format!(
                "module {path:?} is a Wasm binary member of the module set, not a JavaScript \
                 module"
            ))),
            None => Err(ConvertError::invalid(format!(
                "module {path:?} is not in the module set"
            ))),
        }
    }

    /// The Wasm binary member under `path`, if any.
    pub(crate) fn get_wasm(&self, path: &str) -> Option<&WasmModulePlan> {
        match self.modules.get(path) {
            Some(ModuleMember::Wasm(plan)) => Some(plan),
            _ => None,
        }
    }

    /// Every JavaScript member, in deterministic key order. Wasm binary
    /// members have no SSA body and are skipped; lowering code that only
    /// knows how to walk `SModule`s must iterate this rather than `keys()`.
    pub(crate) fn js_modules(&self) -> impl Iterator<Item = (&String, &'s SModule)> {
        self.modules
            .iter()
            .filter_map(|(path, member)| match member {
                ModuleMember::Js(module) => Some((path, *module)),
                ModuleMember::Wasm(_) => None,
            })
    }

    /// Every Wasm binary member, in deterministic key order (the merge
    /// order, plan §5.1).
    pub(crate) fn wasm_modules(&self) -> impl Iterator<Item = (&String, &WasmModulePlan)> {
        self.modules
            .iter()
            .filter_map(|(path, member)| match member {
                ModuleMember::Wasm(plan) => Some((path, plan)),
                ModuleMember::Js(_) => None,
            })
    }

    pub(crate) fn contains(&self, path: &str) -> bool {
        self.modules.contains_key(path)
    }

    pub(crate) fn keys(&self) -> impl Iterator<Item = &String> {
        self.modules.keys()
    }
}

/// Resolve a relative module specifier against the importing module's path.
///
/// Only `./…` and `../…` specifiers are supported: a standalone WasmGC
/// module has no host module registry, so bare specifiers have nothing to
/// resolve against. Normalization is purely lexical (no extension
/// fallback): the result must be an exact key in the set.
pub(crate) fn resolve_specifier(importer: &str, specifier: &str) -> Result<String, ConvertError> {
    if !(specifier.starts_with("./") || specifier.starts_with("../")) {
        return Err(ConvertError::invalid(format!(
            "unsupported module specifier {specifier:?} in module {importer:?}: only relative \
             specifiers ('./…', '../…') can be resolved within a module set"
        )));
    }
    let directory = match importer.rfind('/') {
        Some(end) => &importer[..=end],
        None => "",
    };
    let mut segments: Vec<&str> = directory
        .split('/')
        .filter(|segment| !segment.is_empty() && *segment != ".")
        .collect();
    for segment in specifier.split('/') {
        match segment {
            "" | "." => {}
            ".." => {
                if segments.pop().is_none() {
                    return Err(ConvertError::invalid(format!(
                        "module specifier {specifier:?} in module {importer:?} escapes the module \
                         set root"
                    )));
                }
            }
            other => segments.push(other),
        }
    }
    Ok(segments.join("/"))
}

/// Resolve the specifier to a module of `set`, with an error naming both
/// sides when the target is missing.
pub(crate) fn resolve_specifier_target<'a>(
    set: &'a ModuleSet<'a>,
    importer: &str,
    specifier: &str,
) -> Result<(&'a str, &'a ModuleMember<'a>), ConvertError> {
    let target = resolve_specifier(importer, specifier)?;
    let module = set.modules.get(&target).ok_or_else(|| {
        ConvertError::invalid(format!(
            "module {target:?} (imported by {importer:?} from {specifier:?}) is not in the module \
             set"
        ))
    })?;
    // Re-borrow the canonical key so callers can hold it independently of
    // the owned normalized path.
    let key = set
        .keys()
        .find(|key| key.as_str() == target)
        .expect("verified present above");
    Ok((key, module))
}

/// Every export name a module's surface provides, following re-export
/// chains (`visiting` guards against circular `export *` chains; a cycle
/// contributes nothing new — an actually-needed name through a cycle fails
/// later in [`resolve_export`] with a sharper diagnostic).
pub(crate) fn module_export_names(
    set: &ModuleSet<'_>,
    path: &str,
    visiting: &mut BTreeSet<String>,
) -> Result<BTreeSet<String>, ConvertError> {
    if !visiting.insert(path.to_string()) {
        return Ok(BTreeSet::new());
    }
    // A Wasm member's surface is its export section (plan §3.2).
    if let Some(plan) = set.get_wasm(path) {
        return Ok(plan.exports.keys().cloned().collect());
    }
    let module = set.get(path)?;
    let mut names = BTreeSet::new();
    for export in &module.exports {
        match export {
            ExportSpec::Local { exported, .. } => {
                names.insert(exported.to_string());
            }
            ExportSpec::DefaultFunc { .. } | ExportSpec::DefaultExpr { .. } => {
                names.insert("default".to_owned());
            }
            ExportSpec::Reexport {
                names: reexported, ..
            } => {
                for (_, exported) in reexported {
                    names.insert(exported.to_string());
                }
            }
            ExportSpec::ReexportAll { source, ns, .. } => {
                if let Some(ns_name) = ns {
                    names.insert(ns_name.to_string());
                }
                let (target, _) = resolve_specifier_target(set, path, source.as_ref())?;
                names.extend(module_export_names(set, target, visiting)?);
            }
        }
    }
    Ok(names)
}

/// Resolve exported `name` of the module at `path` to its hoisted function
/// declaration.
///
/// Precedence mirrors ESM: local declarations and the module's own default
/// function shadow re-exports; explicit named re-exports shadow star
/// re-exports; `default` never travels through a star. Returns `Ok(None)`
/// when the name is not exported at all. Non-function targets are
/// [`ConvertError`]s — linking must never silently degrade to an undefined
/// binding.
pub(crate) fn resolve_export<'a>(
    set: &'a ModuleSet<'a>,
    path: &str,
    name: &str,
    visiting: &mut BTreeSet<(String, String)>,
) -> Result<Option<ExportTarget<'a>>, ConvertError> {
    if !visiting.insert((path.to_string(), name.to_string())) {
        return Err(ConvertError::invalid(format!(
            "circular re-export chain while resolving {name:?} from {path:?}"
        )));
    }
    // A Wasm member resolves its own export section directly (plan §5.2).
    if let Some(plan) = set.get_wasm(path) {
        return match plan.exports.get(name) {
            None => Ok(None),
            Some(crate::wasm_esm::WasmExport::Func { .. }) => Ok(Some(ExportTarget::Wasm {
                // Re-key to the canonical module-set key so the identity is
                // stable regardless of the spelling that resolved here.
                path: set
                    .keys()
                    .find(|key| key.as_str() == path)
                    .expect("verified present above")
                    .as_str(),
                export: name.to_string(),
            })),
            Some(_) => Err(ConvertError::invalid(format!(
                "Wasm module export {path:?}.{name:?} is not a function; non-function exports \
                 cannot be linked (portal-co/jsaw#10)"
            ))),
        };
    }
    let module = set.get(path)?;
    // 1. Explicit exports of this module.
    for export in &module.exports {
        match export {
            ExportSpec::Local {
                local, exported, ..
            } if exported == name => {
                return match module.funcs.get(&local.0) {
                    Some(function) => Ok(Some(ExportTarget::Js(function))),
                    None => Err(ConvertError::invalid(format!(
                        "export {name:?} of {path:?} is not a hoisted function declaration; only \
                         function exports can be linked across modules in this milestone"
                    ))),
                };
            }
            ExportSpec::DefaultFunc { func_name } if name == "default" => {
                return match module.funcs.get(func_name) {
                    Some(function) => Ok(Some(ExportTarget::Js(function))),
                    None => Err(ConvertError::invalid(format!(
                        "default export of {path:?} has no hoisted function body"
                    ))),
                };
            }
            ExportSpec::DefaultExpr { .. } if name == "default" => {
                return Err(ConvertError::invalid(format!(
                    "export default <expr> in {path:?} is not supported yet: only `export default \
                     function` can be linked across modules"
                )));
            }
            ExportSpec::ReexportAll {
                ns: Some(ns_name), ..
            } if ns_name == name => {
                return Err(ConvertError::invalid(format!(
                    "namespace object export {name:?} (export * as ns) in {path:?} is not \
                     supported yet"
                )));
            }
            ExportSpec::Reexport { source, names, .. } => {
                if let Some((original, _)) = names.iter().find(|(_, exported)| exported == name) {
                    let (target, _) = resolve_specifier_target(set, path, source.as_ref())?;
                    return resolve_export(set, target, original.as_ref(), visiting);
                }
            }
            _ => {}
        }
    }
    // 2. Star re-exports. A name provided by two different starred modules
    //    is ambiguous and rejected (a conservative subset of the spec's
    //    "unbound" rule); the same function through two paths is fine.
    if name != "default" {
        let mut found: Option<(ExportTarget<'a>, &str)> = None;
        for export in &module.exports {
            if let ExportSpec::ReexportAll {
                source, ns: None, ..
            } = export
            {
                let (target, _) = resolve_specifier_target(set, path, source.as_ref())?;
                if let Some(resolved) = resolve_export(set, target, name, visiting)? {
                    match &found {
                        Some((previous, previous_path)) => {
                            if !same_export_target(previous, &resolved) {
                                return Err(ConvertError::invalid(format!(
                                    "ambiguous star export {name:?} from {path:?}: provided by \
                                     both {previous_path:?} and {target:?}"
                                )));
                            }
                        }
                        None => found = Some((resolved, target)),
                    }
                }
            }
        }
        if let Some((resolved, _)) = found {
            return Ok(Some(resolved));
        }
    }
    Ok(None)
}

/// Star-export identity: the same declaration reached through two star
/// paths is not ambiguous (pointer identity for JS declarations; the
/// canonical (path, export) pair for Wasm members).
fn same_export_target(a: &ExportTarget<'_>, b: &ExportTarget<'_>) -> bool {
    match (a, b) {
        (ExportTarget::Js(a), ExportTarget::Js(b)) => std::ptr::eq(*a, *b),
        (
            ExportTarget::Wasm {
                path: a_path,
                export: a_export,
            },
            ExportTarget::Wasm {
                path: b_path,
                export: b_export,
            },
        ) => a_path == b_path && a_export == b_export,
        _ => false,
    }
}

/// A statically resolved export: either a JavaScript hoisted function or a
/// Wasm member's function export (wasm-esm phase 2, plan §5.2).
#[derive(Clone)]
pub(crate) enum ExportTarget<'a> {
    /// A JavaScript hoisted function declaration.
    Js(&'a SFunc),
    /// A function export of a `.wasm` module-set member. `path` is the
    /// canonical module-set key; `export` is the binary's export name.
    Wasm { path: &'a str, export: String },
}

/// A statically resolved imported binding.
///
/// Relative ESM imports link to a source function in this closed module set;
/// the reserved `wasm:` namespace names an external, typed Wasm function.
#[derive(Clone)]
pub(crate) enum ImportTarget<'a> {
    /// A normal relative ESM import resolved within [`ModuleSet`].
    InternalFunction {
        /// Module key of the module that declares the target function.
        module: String,
        function: &'a SFunc,
    },
    /// A named import from `wasm:<module>` with an explicitly declared ABI.
    WasmHost(WasmImportSpec),
    /// A compiler-recognized intrinsic module. Intrinsics are call-only:
    /// they do not have a runtime function object and are intercepted by the
    /// WasmGC lowering before ordinary call dispatch.
    Intrinsic { module: String, name: String },
    /// A named function import from a `.wasm` module-set member (wasm-esm,
    /// portal-co/jsaw#14). The signature is derived from the binary's own
    /// type section, not the FFI manifest.
    WasmModule(WasmModuleImportSpec),
    /// A namespace import (`import * as m`) from a `.wasm` module-set
    /// member: a plain JS object of the member's function exports, bound to
    /// the default instance for late-bound members (portal-co/jsaw#9).
    WasmNamespace {
        /// Module-set key of the `.wasm` member.
        path: String,
    },
    /// A global export of a late-bound `.wasm` member, read live from the
    /// default instance's field (portal-co/jsaw#10). Read-only: import
    /// bindings are never assigned in ES modules.
    WasmGlobal {
        /// Module-set key of the `.wasm` member.
        path: String,
        /// Member-local global index (== instance field index, globals
        /// come first in the instance layout).
        index: u32,
        /// The global's value type.
        ty: crate::wasm_esm::WasmValType,
    },
}

/// Build one module's import table: every referenced imported binding →
/// the hoisted function declaration it links to.
///
/// The table is keyed by the binding's resolved [`Ident`], exactly what an
/// `SValue::LoadId` carries. Eager construction means every declared import
/// of every module in the set must resolve, even in a module the entry
/// never reaches: a closed set should not contain dangling imports.
pub(crate) fn build_import_table<'a>(
    set: &'a ModuleSet<'a>,
    path: &str,
    module: &'a SModule,
    ffi: &crate::ffi::FfiManifest,
) -> Result<BTreeMap<Ident, ImportTarget<'a>>, ConvertError> {
    validate_wasm_host_import_declarations(path, module, ffi)?;
    let mut table = BTreeMap::new();
    let mapper = module.import_mapper();
    // Referenced bindings only. Imported bindings are recorded in each
    // function's `decls` (the import declaration lives in the module
    // scope), so `externals()` — which filters out declarations — would
    // miss them; match against `refs()` instead.
    let mut externals: BTreeSet<Ident> = module.body.cfg.refs();
    for function in module.funcs.values() {
        externals.extend(function.cfg.refs());
    }
    for id in externals {
        let Some((specifier, kind)) = mapper.import_of(&id) else {
            continue;
        };
        let specifier = specifier.to_atom_lossy().to_string();
        if let Some(host_module) = parse_wasm_import_specifier(&specifier) {
            let host_module = host_module?;
            let name = match kind {
                ImportMap::Named { name } => name.to_string(),
                ImportMap::Default => {
                    return Err(ConvertError::invalid(format!(
                        "Wasm host import from {specifier:?} in module {path:?} must be a named \
                         import, not a default import"
                    )));
                }
                ImportMap::Star => {
                    return Err(ConvertError::invalid(format!(
                        "Wasm host import from {specifier:?} in module {path:?} must be a named \
                         import, not a namespace import"
                    )));
                }
            };
            crate::wasm_boundary::reject_legacy_suffix(&name, "Wasm host import")?;
            let Some(signature) = ffi.import(host_module, &name) else {
                return Err(ConvertError::invalid(format!(
                    "Wasm host import {specifier:?}.{name:?} in module {path:?} has no FFI                      declaration; declare it in the FFI manifest (`ConvertOptions::ffi`)"
                )));
            };
            table.insert(
                id,
                ImportTarget::WasmHost(WasmImportSpec {
                    module: host_module.to_owned(),
                    field: name,
                    signature: signature.clone(),
                }),
            );
            continue;
        }
        // `~ffi` is the DreamCompat-style FFI factory namespace (DreamComp
        // #37): `import { ffi, struct, opaque } from "~ffi"`. The bindings
        // are call-only compiler intrinsics, intercepted before ordinary
        // call dispatch; using one as a value is rejected by the LoadId
        // path. `callback`/`contains` are recognized but fail closed at the
        // call site (jsaw does not implement them yet).
        if specifier == "~ffi" {
            let name = match kind {
                ImportMap::Named { name } => name.to_string(),
                ImportMap::Default | ImportMap::Star => {
                    return Err(ConvertError::invalid(format!(
                        "intrinsic import from {specifier:?} in module {path:?} must be named"
                    )));
                }
            };
            match name.as_str() {
                "ffi" | "struct" | "opaque" | "callback" | "contains" => {}
                other => {
                    return Err(ConvertError::invalid(format!(
                        "unknown ~ffi intrinsic {other:?} (expected ffi/struct/opaque/callback/contains)"
                    )));
                }
            }
            table.insert(
                id,
                ImportTarget::Intrinsic {
                    module: specifier.clone(),
                    name,
                },
            );
            continue;
        }
        // `~wasm` is the Wasm-member intrinsic namespace
        // (portal-co/jsaw#6): `instantiate` creates a fresh GC-native
        // instance object of a late-bound member. Call-only, like the
        // other intrinsic namespaces.
        if specifier == "~wasm" {
            let name = match kind {
                ImportMap::Named { name } => name.to_string(),
                ImportMap::Default | ImportMap::Star => {
                    return Err(ConvertError::invalid(format!(
                        "intrinsic import from {specifier:?} in module {path:?} must be named"
                    )));
                }
            };
            if name != "instantiate" {
                return Err(ConvertError::invalid(format!(
                    "unknown ~wasm intrinsic {name:?} (expected instantiate)"
                )));
            }
            table.insert(
                id,
                ImportTarget::Intrinsic {
                    module: specifier.clone(),
                    name,
                },
            );
            continue;
        }
        // `~weakmap` is a compiler intrinsic namespace, not an ES module in
        // the closed module set. Preserve the lexical binding so call
        // lowering can intercept it before the normal function path.
        if specifier == "~weakmap" {
            let name = match kind {
                ImportMap::Named { name } => name.to_string(),
                ImportMap::Default | ImportMap::Star => {
                    return Err(ConvertError::invalid(format!(
                        "intrinsic import from {specifier:?} in module {path:?} must be named"
                    )));
                }
            };
            table.insert(
                id,
                ImportTarget::Intrinsic {
                    module: specifier.clone(),
                    name,
                },
            );
            continue;
        }
        let (target_path, member) = resolve_specifier_target(set, path, &specifier)?;
        if let ModuleMember::Wasm(plan) = member {
            // A `.wasm` member: the binary's export section is the import
            // surface (plan §3.2). Named imports only; default/namespace
            // imports fail closed (namespace objects: portal-co/jsaw#9).
            let name = match kind {
                ImportMap::Named { name } => name.to_string(),
                ImportMap::Default => {
                    return Err(ConvertError::invalid(format!(
                        "Wasm module {target_path:?} has no default export (imported by {path:?} \
                         from {specifier:?}); use a named import"
                    )));
                }
                ImportMap::Star => {
                    table.insert(
                        id,
                        ImportTarget::WasmNamespace {
                            path: target_path.to_owned(),
                        },
                    );
                    continue;
                }
            };
            let export = plan.exports.get(&name).ok_or_else(|| {
                ConvertError::invalid(format!(
                    "Wasm module {target_path:?} does not export {name:?} (imported by {path:?} \
                     from {specifier:?})"
                ))
            })?;
            let export = export.clone();
            if let crate::wasm_esm::WasmExport::Global { index, ty } = export {
                if plan.instance.is_none() {
                    return Err(ConvertError::invalid(format!(
                        "Wasm module export {target_path:?}.{name:?} is a global of a static \
                         member (tables/indirect calls); global reads need the late-bound \
                         instance form (portal-co/jsaw#10)"
                    )));
                }
                table.insert(
                    id,
                    ImportTarget::WasmGlobal {
                        path: target_path.to_owned(),
                        index,
                        ty,
                    },
                );
                continue;
            }
            let crate::wasm_esm::WasmExport::Func { signature, .. } = export else {
                return Err(ConvertError::invalid(format!(
                    "Wasm module export {target_path:?}.{name:?} is not a function or global; \
                     importing memories, tables, or tags as JavaScript values is not supported \
                     yet (portal-co/jsaw#10)"
                )));
            };
            table.insert(
                id,
                ImportTarget::WasmModule(WasmModuleImportSpec {
                    path: target_path.to_owned(),
                    export: name,
                    signature: signature.clone(),
                }),
            );
            continue;
        }
        let name = match kind {
            ImportMap::Named { name } => name.to_string(),
            ImportMap::Default => "default".to_owned(),
            ImportMap::Star => {
                return Err(ConvertError::invalid(format!(
                    "namespace imports (import * as …) are not supported yet (imported by \
                     {path:?} from {specifier:?})"
                )));
            }
        };
        let resolved =
            resolve_export(set, target_path, &name, &mut BTreeSet::new())?.ok_or_else(|| {
                ConvertError::invalid(format!(
                    "module {target_path:?} does not export {name:?} (imported by {path:?} from \
                     {specifier:?})"
                ))
            })?;
        match resolved {
            ExportTarget::Js(function) => {
                table.insert(
                    id,
                    ImportTarget::InternalFunction {
                        module: target_path.to_owned(),
                        function,
                    },
                );
            }
            ExportTarget::Wasm {
                path: wasm_path,
                export,
            } => {
                // A JavaScript module re-exporting a Wasm member's function:
                // the import links directly to the Wasm member (plan §5.2).
                let plan = set.get_wasm(wasm_path).expect("resolved above");
                let Some(crate::wasm_esm::WasmExport::Func { signature, .. }) =
                    plan.exports.get(&export)
                else {
                    unreachable!("resolve_export only yields Wasm targets for function exports");
                };
                table.insert(
                    id,
                    ImportTarget::WasmModule(WasmModuleImportSpec {
                        path: wasm_path.to_owned(),
                        export,
                        signature: signature.clone(),
                    }),
                );
            }
        }
    }
    Ok(table)
}

/// Validate every declared host import, including imports not presently
/// referenced by a lowered function. A malformed ABI declaration must not be
/// hidden merely because dead source did not create a `LoadId`.
fn validate_wasm_host_import_declarations(
    path: &str,
    module: &SModule,
    ffi: &crate::ffi::FfiManifest,
) -> Result<(), ConvertError> {
    let mut signatures: BTreeMap<(String, String), ()> = BTreeMap::new();
    for declaration in &module.imports {
        let specifier = declaration.src.value.to_atom_lossy().to_string();
        // `~ffi` declarations validate their member names even when unused.
        if specifier == "~ffi" {
            for import in &declaration.specifiers {
                let ImportSpecifier::Named(named) = import else {
                    return Err(ConvertError::invalid(format!(
                        "intrinsic import from {specifier:?} in module {path:?} must be named"
                    )));
                };
                let name = named
                    .imported
                    .as_ref()
                    .map(|name| name.atom().to_string())
                    .unwrap_or_else(|| named.local.sym.to_string());
                crate::wasm_boundary::reject_legacy_suffix(&name, "intrinsic import")?;
                match name.as_str() {
                    "ffi" | "struct" | "opaque" | "callback" | "contains" => {}
                    other => {
                        return Err(ConvertError::invalid(format!(
                            "unknown ~ffi intrinsic {other:?} (expected \
                             ffi/struct/opaque/callback/contains)"
                        )));
                    }
                }
            }
            continue;
        }
        let Some(host_module) = parse_wasm_import_specifier(&specifier) else {
            continue;
        };
        let host_module = host_module?;
        for import in &declaration.specifiers {
            let ImportSpecifier::Named(named) = import else {
                let kind = match import {
                    ImportSpecifier::Default(_) => "default",
                    ImportSpecifier::Namespace(_) => "namespace",
                    ImportSpecifier::Named(_) => unreachable!("matched above"),
                };
                return Err(ConvertError::invalid(format!(
                    "Wasm host import from {specifier:?} in module {path:?} must be a named \
                     import, not a {kind} import"
                )));
            };
            let imported_name = named
                .imported
                .as_ref()
                .map(|name| name.atom().to_string())
                .unwrap_or_else(|| named.local.sym.to_string());
            crate::wasm_boundary::reject_legacy_suffix(&imported_name, "Wasm host import")?;
            if ffi.import(host_module, &imported_name).is_none() {
                return Err(ConvertError::invalid(format!(
                    "Wasm host import {specifier:?}.{imported_name:?} in module {path:?} has no \
                     FFI declaration; declare it in the FFI manifest (`ConvertOptions::ffi`)"
                )));
            }
            signatures.insert((host_module.to_string(), imported_name), ());
        }
    }
    Ok(())
}

/// The main module's complete export surface, flattened.
pub(crate) fn flatten_module_export_surface<'a>(
    set: &'a ModuleSet<'a>,
    entry: &str,
) -> Result<(Vec<(String, ExportTarget<'a>)>, BTreeSet<String>), ConvertError> {
    let module = set.get(entry)?;
    let mut exported: Vec<(String, ExportTarget<'a>)> = Vec::new();
    // Every name the surface provides, including non-function exports that
    // keep their name reserved (no Wasm export) exactly like today's
    // function-only ABI.
    let mut declared_names: BTreeSet<String> = BTreeSet::new();
    let mut star_sources: Vec<String> = Vec::new();
    for export in &module.exports {
        match export {
            ExportSpec::Local {
                local,
                exported: name,
                ..
            } => {
                declared_names.insert(name.to_string());
                // Non-function local exports skip silently: the existing
                // function-only ABI never exported them either.
                if let Some(function) = module.funcs.get(&local.0) {
                    exported.push((name.to_string(), ExportTarget::Js(function)));
                }
            }
            ExportSpec::DefaultFunc { func_name } => {
                let function = module.funcs.get(func_name).ok_or_else(|| {
                    ConvertError::invalid(format!(
                        "default export of {entry:?} has no hoisted function body"
                    ))
                })?;
                declared_names.insert("default".to_owned());
                exported.push(("default".to_owned(), ExportTarget::Js(function)));
            }
            ExportSpec::DefaultExpr { .. } => {
                return Err(ConvertError::invalid(format!(
                    "export default <expr> in {entry:?} is not supported yet: only `export \
                     default function` can become a Wasm export"
                )));
            }
            ExportSpec::Reexport { source, names, .. } => {
                let (target, _) = resolve_specifier_target(set, entry, source.as_ref())?;
                for (original, exported_name) in names {
                    let resolved =
                        resolve_export(set, target, original.as_ref(), &mut BTreeSet::new())?
                            .ok_or_else(|| {
                                ConvertError::invalid(format!(
                                    "module {target:?} does not export {original:?} (re-exported \
                                     by {entry:?} as {exported_name:?})"
                                ))
                            })?;
                    declared_names.insert(exported_name.to_string());
                    exported.push((exported_name.to_string(), resolved));
                }
            }
            ExportSpec::ReexportAll { ns: Some(_), .. } => {
                return Err(ConvertError::invalid(format!(
                    "namespace re-exports (export * as ns from …) in {entry:?} are not supported \
                     yet"
                )));
            }
            ExportSpec::ReexportAll {
                source, ns: None, ..
            } => {
                let (target, _) = resolve_specifier_target(set, entry, source.as_ref())?;
                star_sources.push(target.to_owned());
            }
        }
    }
    // Star re-exports fill every name the main module does not declare
    // itself; `default` never travels through a star. Local declarations
    // shadow star exports, so `declared_names` must hold only the module's
    // own names here — star-provided names are appended below.
    let mut local_names: BTreeSet<String> = BTreeSet::new();
    for export in &module.exports {
        if let ExportSpec::Local { exported: name, .. } = export {
            local_names.insert(name.to_string());
        }
    }
    // Star-origin identity mirrors `same_export_target`: the same
    // declaration through two star paths is not ambiguous.
    #[derive(PartialEq, Eq)]
    enum StarOrigin {
        Js(usize),
        Wasm(String, String),
    }
    let mut star_seen: BTreeMap<String, StarOrigin> = BTreeMap::new();
    for source in &star_sources {
        for name in module_export_names(set, source, &mut BTreeSet::new())? {
            if name == "default" || local_names.contains(&name) {
                continue;
            }
            let resolved =
                resolve_export(set, source, &name, &mut BTreeSet::new())?.ok_or_else(|| {
                    ConvertError::invalid(format!(
                        "module {source:?} does not export {name:?} (star-exported by {entry:?})"
                    ))
                })?;
            let key = match &resolved {
                ExportTarget::Js(function) => StarOrigin::Js(*function as *const SFunc as usize),
                ExportTarget::Wasm { path, export } => {
                    StarOrigin::Wasm(path.to_string(), export.clone())
                }
            };
            match star_seen.insert(name.clone(), key) {
                Some(previous)
                    if previous
                        != match &resolved {
                            ExportTarget::Js(function) => {
                                StarOrigin::Js(*function as *const SFunc as usize)
                            }
                            ExportTarget::Wasm { path, export } => {
                                StarOrigin::Wasm(path.to_string(), export.clone())
                            }
                        } =>
                {
                    return Err(ConvertError::invalid(format!(
                        "ambiguous star export {name:?} in {entry:?}: provided by more than one \
                         starred module"
                    )));
                }
                _ => {}
            }
            declared_names.insert(name.clone());
            exported.push((name, resolved));
        }
    }
    Ok((exported, declared_names))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ingest::{parse_module_source, with_globals};
    use crate::wasm_esm::WasmValType;
    use crate::wasm_esm::tests as wasm_fixtures;

    fn js_module(source: &str) -> &'static SModule {
        Box::leak(Box::new(
            parse_module_source("main.js", source).expect("JS fixture should lower"),
        ))
    }

    #[test]
    fn js_insert_rejects_wasm_keys() {
        with_globals(|| {
            let mut set = ModuleSet::new();
            let error = set
                .insert("sneaky.wasm", js_module("export function f() {}"))
                .expect_err("a .wasm key must not accept JavaScript sources");
            assert!(error.message.contains("insert_wasm"), "{error}");
        });
    }

    #[test]
    fn wasm_insert_requires_wasm_keys_and_rejects_duplicates() {
        let mut set = ModuleSet::new();
        let error = set
            .insert_wasm("math.bin", &wasm_fixtures::add_module())
            .expect_err("a Wasm member must use a .wasm key");
        assert!(error.message.contains(".wasm"), "{error}");
        set.insert_wasm("math.wasm", &wasm_fixtures::add_module())
            .expect("valid Wasm member");
        with_globals(|| {
            let error = set
                .insert("math.wasm", js_module("export function f() {}"))
                .expect_err("a JS/wasm path collision is a duplicate");
            assert!(error.message.contains(".wasm"), "{error}");
        });
        let error = set
            .insert_wasm("math.wasm", &wasm_fixtures::add_module())
            .expect_err("a duplicate Wasm path must be rejected");
        assert!(error.message.contains("duplicate"), "{error}");
    }

    #[test]
    fn get_distinguishes_wasm_members_from_js() {
        let mut set = ModuleSet::new();
        set.insert_wasm("math.wasm", &wasm_fixtures::add_module())
            .expect("valid Wasm member");
        let Err(error) = set.get("math.wasm") else {
            panic!("get() is the JavaScript accessor");
        };
        assert!(error.message.contains("Wasm binary member"), "{error}");
        assert!(set.get_wasm("math.wasm").is_some());
        assert_eq!(set.js_modules().count(), 0);
    }

    #[test]
    fn named_import_from_wasm_member_links_with_binary_signature() {
        with_globals(|| {
            let mut set = ModuleSet::new();
            set.insert_wasm("lib/math.wasm", &wasm_fixtures::add_module())
                .expect("valid Wasm member");
            let main = js_module(
                "import { add } from './lib/math.wasm'; \
                 export function run(a, b) { return add(a, b); }",
            );
            set.insert("main.js", main).expect("JS member");
            let table =
                build_import_table(&set, "main.js", main, &crate::ffi::FfiManifest::default())
                    .expect("the wasm import should link");
            let target = table.values().next().expect("one import binding");
            let ImportTarget::WasmModule(spec) = target else {
                panic!("expected a WasmModule target");
            };
            assert_eq!(spec.path, "lib/math.wasm");
            assert_eq!(spec.export, "add");
            assert_eq!(
                spec.signature.params,
                vec![WasmValType::I32, WasmValType::I32]
            );
        });
    }

    #[test]
    fn wasm_member_import_kinds_fail_closed() {
        with_globals(|| {
            let set_with = |source: &str| -> Result<(), ConvertError> {
                let mut set = ModuleSet::new();
                set.insert_wasm("lib/math.wasm", &wasm_fixtures::add_module())
                    .expect("valid Wasm member");
                let main = js_module(source);
                set.insert("main.js", main).expect("JS member");
                build_import_table(&set, "main.js", main, &crate::ffi::FfiManifest::default())?;
                Ok(())
            };
            let fail = |source: &str| {
                let Err(error) = set_with(source) else {
                    panic!("import kind must be rejected")
                };
                error
            };
            let error = fail(
                "import add from './lib/math.wasm'; \
                 export function run(a, b) { return add(a, b); }",
            );
            assert!(error.message.contains("no default export"), "{error}");
            // Namespace imports of Wasm members link (portal-co/jsaw#9):
            // they materialize as an object of the member's exports.
            let mut ok_set = ModuleSet::new();
            ok_set
                .insert_wasm("lib/math.wasm", &wasm_fixtures::add_module())
                .expect("valid Wasm member");
            let main = js_module(
                "import * as math from './lib/math.wasm'; \
                 export function run(a, b) { return math.add(a, b); }",
            );
            ok_set.insert("main.js", main).expect("JS member");
            let table =
                build_import_table(&ok_set, "main.js", main, &crate::ffi::FfiManifest::default())
                    .expect("namespace import links");
            assert!(table
                .values()
                .any(|target| matches!(target, ImportTarget::WasmNamespace { .. })));
            let error = fail(
                "import { missing } from './lib/math.wasm'; \
                 export function run() { return missing(); }",
            );
            assert!(error.message.contains("does not export"), "{error}");
        });
    }

    #[test]
    fn wasm_member_reexport_resolves_to_wasm_target() {
        with_globals(|| {
            let mut set = ModuleSet::new();
            set.insert_wasm("lib/math.wasm", &wasm_fixtures::add_module())
                .expect("valid Wasm member");
            let main = js_module("export { add } from './lib/math.wasm';");
            set.insert("main.js", main).expect("JS member");
            let (exported, _) = flatten_module_export_surface(&set, "main.js")
                .expect("re-exporting a Wasm function export resolves in phase 2");
            let [(name, target)] = exported.as_slice() else {
                panic!("expected exactly one export, got {}", exported.len());
            };
            assert_eq!(name, "add");
            let ExportTarget::Wasm { path, export } = target else {
                panic!("expected a Wasm export target");
            };
            assert_eq!(*path, "lib/math.wasm");
            assert_eq!(export, "add");
        });
    }
}
