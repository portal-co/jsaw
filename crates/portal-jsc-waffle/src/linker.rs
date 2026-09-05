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
    module::{ExportSpec, SModule},
    SFunc,
};
use portal_jsc_swc_util::{ImportMap, ImportMapper};
use swc_ecma_ast::Id as Ident;

use crate::repr::ConvertError;

/// A closed set of ES modules that [`convert_modules`](crate::convert_modules)
/// can link together into one WasmGC module.
///
/// Keys are module paths as provided by the embedder (e.g. `"src/main.js"`).
/// Relative import specifiers resolve against the importing module's key:
/// `import … from './lib.js'` in `src/main.js` looks up `src/lib.js`.
///
/// Deterministic ordering ([`BTreeMap`]) keeps lowering and generated Wasm
/// reproducible regardless of insertion order.
#[derive(Default)]
pub struct ModuleSet<'s> {
    modules: BTreeMap<String, &'s SModule>,
}

impl<'s> ModuleSet<'s> {
    pub fn new() -> Self {
        Self {
            modules: BTreeMap::new(),
        }
    }

    /// Insert a module under `path`. Duplicate paths are a
    /// [`ConvertError`].
    pub fn insert(
        &mut self,
        path: impl Into<String>,
        module: &'s SModule,
    ) -> Result<(), ConvertError> {
        let path = path.into();
        if self.modules.insert(path.clone(), module).is_some() {
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
        set.modules.insert(path.into(), module);
        set
    }

    pub(crate) fn get(&self, path: &str) -> Result<&'s SModule, ConvertError> {
        self.modules.get(path).copied().ok_or_else(|| {
            ConvertError::invalid(format!("module {path:?} is not in the module set"))
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
) -> Result<(&'a str, &'a SModule), ConvertError> {
    let target = resolve_specifier(importer, specifier)?;
    let module = set.get(&target).map_err(|_| {
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
            ExportSpec::Reexport { names: reexported, .. } => {
                for (_, exported) in reexported {
                    names.insert(exported.to_string());
                }
            }
            ExportSpec::ReexportAll { source, ns, .. } => {
                if let Some(ns_name) = ns {
                    names.insert(ns_name.to_string());
                }
                let (target, _) = resolve_specifier_target(set, path, &source.to_string())?;
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
) -> Result<Option<&'a SFunc>, ConvertError> {
    if !visiting.insert((path.to_string(), name.to_string())) {
        return Err(ConvertError::invalid(format!(
            "circular re-export chain while resolving {name:?} from {path:?}"
        )));
    }
    let module = set.get(path)?;
    // 1. Explicit exports of this module.
    for export in &module.exports {
        match export {
            ExportSpec::Local { local, exported, .. } if exported.to_string() == name => {
                return match module.funcs.get(&local.0) {
                    Some(function) => Ok(Some(function)),
                    None => Err(ConvertError::invalid(format!(
                        "export {name:?} of {path:?} is not a hoisted function declaration; only \
                         function exports can be linked across modules in this milestone"
                    ))),
                };
            }
            ExportSpec::DefaultFunc { func_name } if name == "default" => {
                return match module.funcs.get(func_name) {
                    Some(function) => Ok(Some(function)),
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
            ExportSpec::ReexportAll { ns: Some(ns_name), .. } if ns_name.to_string() == name => {
                return Err(ConvertError::invalid(format!(
                    "namespace object export {name:?} (export * as ns) in {path:?} is not \
                     supported yet"
                )));
            }
            ExportSpec::Reexport { source, names, .. } => {
                if let Some((original, _)) =
                    names.iter().find(|(_, exported)| exported.to_string() == name)
                {
                    let (target, _) = resolve_specifier_target(set, path, &source.to_string())?;
                    return resolve_export(set, target, &original.to_string(), visiting);
                }
            }
            _ => {}
        }
    }
    // 2. Star re-exports. A name provided by two different starred modules
    //    is ambiguous and rejected (a conservative subset of the spec's
    //    "unbound" rule); the same function through two paths is fine.
    if name != "default" {
        let mut found: Option<(&'a SFunc, &str)> = None;
        for export in &module.exports {
            if let ExportSpec::ReexportAll { source, ns: None, .. } = export {
                let (target, _) = resolve_specifier_target(set, path, &source.to_string())?;
                if let Some(function) = resolve_export(set, target, name, visiting)? {
                    match found {
                        Some((previous, previous_path)) => {
                            if !std::ptr::eq(previous, function) {
                                return Err(ConvertError::invalid(format!(
                                    "ambiguous star export {name:?} from {path:?}: provided by \
                                     both {previous_path:?} and {target:?}"
                                )));
                            }
                        }
                        None => found = Some((function, target)),
                    }
                }
            }
        }
        if let Some((function, _)) = found {
            return Ok(Some(function));
        }
    }
    Ok(None)
}

/// A statically resolved cross-module import binding.
#[derive(Clone)]
pub(crate) struct ImportTarget<'a> {
    /// Module key of the module that declares the target function.
    pub(crate) module: String,
    pub(crate) function: &'a SFunc,
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
) -> Result<BTreeMap<Ident, ImportTarget<'a>>, ConvertError> {
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
        let (target_path, _) = resolve_specifier_target(set, path, &specifier)?;
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
        let function = resolve_export(set, target_path, &name, &mut BTreeSet::new())?.ok_or_else(
            || {
                ConvertError::invalid(format!(
                    "module {target_path:?} does not export {name:?} (imported by {path:?} from \
                     {specifier:?})"
                ))
            },
        )?;
        table.insert(
            id,
            ImportTarget {
                module: target_path.to_owned(),
                function,
            },
        );
    }
    Ok(table)
}

/// The main module's complete export surface, flattened.
pub(crate) fn flatten_module_export_surface<'a>(
    set: &'a ModuleSet<'a>,
    entry: &str,
) -> Result<(Vec<(String, &'a SFunc)>, BTreeSet<String>), ConvertError> {
    let module = set.get(entry)?;
    let mut exported: Vec<(String, &'a SFunc)> = Vec::new();
    // Every name the surface provides, including non-function exports that
    // keep their name reserved (no Wasm export) exactly like today's
    // function-only ABI.
    let mut declared_names: BTreeSet<String> = BTreeSet::new();
    let mut star_sources: Vec<String> = Vec::new();
    for export in &module.exports {
        match export {
            ExportSpec::Local { local, exported: name, .. } => {
                declared_names.insert(name.to_string());
                // Non-function local exports skip silently: the existing
                // function-only ABI never exported them either.
                if let Some(function) = module.funcs.get(&local.0) {
                    exported.push((name.to_string(), function));
                }
            }
            ExportSpec::DefaultFunc { func_name } => {
                let function = module.funcs.get(func_name).ok_or_else(|| {
                    ConvertError::invalid(format!(
                        "default export of {entry:?} has no hoisted function body"
                    ))
                })?;
                declared_names.insert("default".to_owned());
                exported.push(("default".to_owned(), function));
            }
            ExportSpec::DefaultExpr { .. } => {
                return Err(ConvertError::invalid(format!(
                    "export default <expr> in {entry:?} is not supported yet: only `export \
                     default function` can become a Wasm export"
                )));
            }
            ExportSpec::Reexport { source, names, .. } => {
                let (target, _) = resolve_specifier_target(set, entry, &source.to_string())?;
                for (original, exported_name) in names {
                    let function =
                        resolve_export(set, target, &original.to_string(), &mut BTreeSet::new())?
                            .ok_or_else(|| {
                                ConvertError::invalid(format!(
                                    "module {target:?} does not export {original:?} (re-exported \
                                     by {entry:?} as {exported_name:?})"
                                ))
                            })?;
                    declared_names.insert(exported_name.to_string());
                    exported.push((exported_name.to_string(), function));
                }
            }
            ExportSpec::ReexportAll { ns: Some(_), .. } => {
                return Err(ConvertError::invalid(format!(
                    "namespace re-exports (export * as ns from …) in {entry:?} are not supported \
                     yet"
                )));
            }
            ExportSpec::ReexportAll { source, ns: None, .. } => {
                let (target, _) = resolve_specifier_target(set, entry, &source.to_string())?;
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
    let mut star_seen: BTreeMap<String, usize> = BTreeMap::new();
    for source in &star_sources {
        for name in module_export_names(set, source, &mut BTreeSet::new())? {
            if name == "default" || local_names.contains(&name) {
                continue;
            }
            let function = resolve_export(set, source, &name, &mut BTreeSet::new())?.ok_or_else(
                || {
                    ConvertError::invalid(format!(
                        "module {source:?} does not export {name:?} (star-exported by {entry:?})"
                    ))
                },
            )?;
            let key = function as *const SFunc as usize;
            match star_seen.insert(name.clone(), key) {
                Some(previous) if previous != key => {
                    return Err(ConvertError::invalid(format!(
                        "ambiguous star export {name:?} in {entry:?}: provided by more than one \
                         starred module"
                    )));
                }
                _ => {}
            }
            declared_names.insert(name.clone());
            exported.push((name, function));
        }
    }
    Ok((exported, declared_names))
}
