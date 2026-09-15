//! Revision planning and fragment-cache interfaces for development-mode hot
//! code reloading.
//!
//! This module plans immutable revision artifacts. It never mutates a running
//! Wasm instance: activation is the selected runtime's responsibility.

use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use portal_jsc_swc_ssa::module::{FunctionFingerprint, semantic_fingerprint, source_fingerprint};
use sha3::{Digest, Sha3_256};
use swc_common::{FileName, SourceMap, sync::Lrc};
use swc_ecma_ast::{
    Decl, DefaultDecl, EsVersion, Expr, FnExpr, Function, Module, ModuleDecl, ModuleItem, Stmt,
};
use swc_ecma_parser::{EsSyntax, Syntax, parse_file_as_module};

use crate::{ConvertError, ConvertOptions, ModuleSet, convert_modules, module_set_from_sources};

/// Version marker for serialized cache/revision records.
pub const REVISION_SCHEMA: &str = "jsaw.revision.v1";

/// A content-derived revision ID. A runtime must verify this against its active
/// base before applying a runtime-specific delta.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ContentRevisionId(String);

impl ContentRevisionId {
    /// Stable lowercase hexadecimal representation suitable for manifests.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// The runtime contract selected for a compilation revision.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReloadProfile {
    /// Produce a complete artifact; the host restarts the application.
    Restart,
    /// Produce a complete immutable artifact; the runtime switches instances
    /// between exported calls and resets state unless it owns migration.
    Reinstantiate,
    /// Request a stable-dispatch delta. This planner reports eligibility only;
    /// a runtime owns the payload and may fall back to re-instantiation.
    DispatchDelta,
}

impl ReloadProfile {
    fn tag(self) -> &'static str {
        match self {
            Self::Restart => "restart",
            Self::Reinstantiate => "reinstantiate",
            Self::DispatchDelta => "dispatch-delta",
        }
    }
}

/// An externally textual source-module key plus a fingerprint of its source.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleFingerprint {
    pub path: String,
    pub fingerprint: FunctionFingerprint,
}

/// A source-fragment cache. Cache implementations may evict entries or treat
/// corrupt persisted records as misses; the planner validates fingerprints
/// before reporting reuse.
pub trait FragmentCache {
    fn get(&self, module: &str) -> Option<FunctionFingerprint>;
    fn put(&mut self, module: String, fingerprint: FunctionFingerprint);
}

/// In-memory cache adapter for editor/daemon sessions.
#[derive(Clone, Debug, Default)]
pub struct MemoryFragmentCache {
    entries: BTreeMap<String, FunctionFingerprint>,
}

impl FragmentCache for MemoryFragmentCache {
    fn get(&self, module: &str) -> Option<FunctionFingerprint> {
        self.entries.get(module).copied()
    }

    fn put(&mut self, module: String, fingerprint: FunctionFingerprint) {
        self.entries.insert(module, fingerprint);
    }
}

/// Why a module cannot use an ordinary body-only replacement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FullRevisionReason {
    /// A static import, export, or re-export surface changed.
    ModuleSurfaceChanged,
    /// A top-level executable statement changed, so initialization/state may
    /// differ even if a hoisted function body did not.
    TopLevelChanged,
    /// A reloadable function's observable calling convention changed.
    FunctionAbiChanged,
    /// The requested base was not the planner's immediately prior revision.
    BaseRevisionUnavailable,
}

/// jsaw's runtime-independent account of cache reuse and invalidation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReloadPlan {
    pub reused_modules: BTreeSet<String>,
    pub changed_modules: BTreeSet<String>,
    pub full_revision_reasons: BTreeMap<String, FullRevisionReason>,
    pub delta_eligible: bool,
}

/// Immutable revision metadata.
#[derive(Clone, Debug)]
pub struct RevisionManifest {
    pub content_id: ContentRevisionId,
    pub base: Option<ContentRevisionId>,
    pub profile: ReloadProfile,
    pub modules: Vec<ModuleFingerprint>,
    pub reload_plan: ReloadPlan,
}

/// The complete immutable artifact for one revision. It is the portable
/// fallback whenever the selected runtime cannot apply a delta.
#[derive(Clone, Debug)]
pub struct RevisionArtifact {
    pub wasm: Vec<u8>,
}

/// Result of planning and assembling one revision.
#[derive(Clone, Debug)]
pub struct RevisionOutput {
    pub revision: RevisionManifest,
    pub artifact: RevisionArtifact,
    /// A runtime-neutral delta *plan*. It never contains mutable Wasm binary
    /// edits; a selected runtime turns these stable-key operations into its
    /// own payload or falls back to `artifact`.
    pub delta: Option<ModuleDelta>,
}

/// A base-checked set of function-cell replacements. Function keys are source
/// keys, never Wasm function/type indices.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleDelta {
    pub base: ContentRevisionId,
    pub target: ContentRevisionId,
    pub operations: Vec<DeltaOperation>,
}

/// One compatible replacement requested by a dispatch-mode revision.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeltaOperation {
    pub function: String,
    pub abi: FunctionFingerprint,
    pub body: FunctionFingerprint,
}

#[derive(Clone, Debug)]
struct ModuleAnalysis {
    surface: FunctionFingerprint,
    functions: BTreeMap<String, FunctionAnalysis>,
}

#[derive(Clone, Debug)]
struct FunctionAnalysis {
    abi: FunctionFingerprint,
    body: FunctionFingerprint,
}

/// A runtime activation error. The runtime checks the base content ID rather
/// than trusting a host-provided sequence number.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ActivationError {
    BaseMismatch {
        active: Option<ContentRevisionId>,
        requested: Option<ContentRevisionId>,
    },
}

/// One immutable artifact selected by the reference reinstantiate adapter.
/// Existing calls retain this `Arc`; later calls read the adapter's current
/// artifact. This models the only portable Wasm behavior: no call frame is
/// rewritten when a revision becomes active.
#[derive(Clone, Debug)]
pub struct ActiveRevision {
    pub content_id: ContentRevisionId,
    pub artifact: Arc<RevisionArtifact>,
}

/// Reference runtime adapter for the `Reinstantiate` profile.
///
/// It deliberately does not instantiate Wasm itself: an embedding runtime
/// owns that task. It gives that runtime the correct switch-point contract and
/// verifies base identity before atomically selecting the next immutable
/// artifact. A real Wasmtime host can instantiate `artifact.wasm` before
/// calling `activate`, then use the same selection rule for its instance.
#[derive(Clone, Debug, Default)]
pub struct ReinstantiatingActivator {
    active: Option<ActiveRevision>,
}

/// Reference implementation of a runtime-specific stable dispatch table.
///
/// It models only the runtime contract: stable source function keys map to an
/// opaque implementation fingerprint. A real engine supplies code objects or
/// table entries instead. Updates are planned on a cloned table and committed
/// only after base validation, so a rejected delta cannot leave a partial swap.
#[derive(Clone, Debug, Default)]
pub struct DispatchDeltaActivator {
    active: Option<ContentRevisionId>,
    cells: BTreeMap<String, FunctionFingerprint>,
}

impl DispatchDeltaActivator {
    pub fn active_revision(&self) -> Option<&ContentRevisionId> {
        self.active.as_ref()
    }

    pub fn implementation_of(&self, function: &str) -> Option<FunctionFingerprint> {
        self.cells.get(function).copied()
    }

    /// Apply one base-checked delta atomically from the reference runtime's
    /// perspective. Actual Wasm code compilation/table mutation remains the
    /// host runtime's adapter behind this same contract.
    pub fn apply(&mut self, delta: &ModuleDelta) -> Result<(), ActivationError> {
        if self.active.as_ref() != Some(&delta.base) {
            return Err(ActivationError::BaseMismatch {
                active: self.active.clone(),
                requested: Some(delta.base.clone()),
            });
        }
        let mut next_cells = self.cells.clone();
        for operation in &delta.operations {
            next_cells.insert(operation.function.clone(), operation.body);
        }
        self.cells = next_cells;
        self.active = Some(delta.target.clone());
        Ok(())
    }

    /// Seed the first dispatch revision. It has no base because there is no
    /// previously active dispatch table.
    pub fn initialize(
        &mut self,
        revision: ContentRevisionId,
        operations: impl IntoIterator<Item = DeltaOperation>,
    ) -> Result<(), ActivationError> {
        if self.active.is_some() {
            return Err(ActivationError::BaseMismatch {
                active: self.active.clone(),
                requested: None,
            });
        }
        self.cells = operations
            .into_iter()
            .map(|operation| (operation.function, operation.body))
            .collect();
        self.active = Some(revision);
        Ok(())
    }
}

impl ReinstantiatingActivator {
    /// Snapshot the artifact for an exported call. Holding the returned value
    /// keeps an in-flight call on its original revision across later switches.
    pub fn begin_call(&self) -> Option<ActiveRevision> {
        self.active.clone()
    }

    /// Select a complete immutable artifact for subsequent calls.
    pub fn activate(&mut self, output: &RevisionOutput) -> Result<(), ActivationError> {
        let active = self
            .active
            .as_ref()
            .map(|revision| revision.content_id.clone());
        if output.revision.base != active {
            return Err(ActivationError::BaseMismatch {
                active,
                requested: output.revision.base.clone(),
            });
        }
        self.active = Some(ActiveRevision {
            content_id: output.revision.content_id.clone(),
            artifact: Arc::new(output.artifact.clone()),
        });
        Ok(())
    }
}

/// Input to the single deep revision-planning operation.
pub struct RevisionRequest<'a> {
    pub entry: &'a str,
    pub sources: &'a BTreeMap<String, String>,
    pub options: ConvertOptions,
    pub profile: ReloadProfile,
    pub base: Option<ContentRevisionId>,
}

/// The HCR compiler seam. It owns deterministic source hashing, cache lookup,
/// conservative invalidation, complete-artifact assembly, and content IDs.
pub struct RevisionCompiler<C> {
    cache: C,
    previous: Option<RevisionManifest>,
    previous_analysis: Option<BTreeMap<String, ModuleAnalysis>>,
}

impl<C: FragmentCache> RevisionCompiler<C> {
    pub fn new(cache: C) -> Self {
        Self {
            cache,
            previous: None,
            previous_analysis: None,
        }
    }

    pub fn cache(&self) -> &C {
        &self.cache
    }

    /// Plan and assemble one immutable source-module revision.
    ///
    /// Fingerprints are calculated before cache lookup, so invalid source never
    /// aliases valid cached source. This phase is intentionally conservative:
    /// any changed module requires complete artifact assembly. Dispatch-mode
    /// lowering may loosen only proven-safe function-body edges in a later
    /// phase.
    pub fn compile(
        &mut self,
        request: RevisionRequest<'_>,
    ) -> Result<RevisionOutput, ConvertError> {
        if !request.sources.contains_key(request.entry) {
            return Err(ConvertError::invalid(format!(
                "entry module {:?} is not in the revision source set",
                request.entry
            )));
        }

        let expected_base = self
            .previous
            .as_ref()
            .map(|revision| revision.content_id.clone());
        let base_available = request.base == expected_base;
        let mut modules = Vec::with_capacity(request.sources.len());
        let mut analyses = BTreeMap::new();
        let mut reused_modules = BTreeSet::new();
        let mut changed_modules = BTreeSet::new();
        let mut full_revision_reasons = BTreeMap::new();
        let mut operations = Vec::new();
        let mut dispatch_compatible = request.profile == ReloadProfile::DispatchDelta
            && request.base.is_some()
            && base_available;

        for (path, source) in request.sources {
            let fingerprint = source_fingerprint(source).map_err(|error| {
                ConvertError::invalid(format!(
                    "parse of revision module {path:?} failed: {error:?}"
                ))
            })?;
            let analysis = analyze_module(path, source)?;
            if self.cache.get(path) == Some(fingerprint) {
                reused_modules.insert(path.clone());
            } else {
                changed_modules.insert(path.clone());
                let prior = self
                    .previous_analysis
                    .as_ref()
                    .and_then(|all| all.get(path));
                match prior {
                    Some(prior) if prior.surface != analysis.surface => {
                        dispatch_compatible = false;
                        full_revision_reasons
                            .insert(path.clone(), FullRevisionReason::ModuleSurfaceChanged);
                    }
                    Some(prior) => {
                        if !collect_delta_operations(prior, &analysis, &mut operations) {
                            dispatch_compatible = false;
                            full_revision_reasons
                                .insert(path.clone(), FullRevisionReason::FunctionAbiChanged);
                        }
                    }
                    None => {
                        dispatch_compatible = false;
                        full_revision_reasons
                            .insert(path.clone(), FullRevisionReason::TopLevelChanged);
                    }
                }
            }
            self.cache.put(path.clone(), fingerprint);
            analyses.insert(path.clone(), analysis);
            modules.push(ModuleFingerprint {
                path: path.clone(),
                fingerprint,
            });
        }
        modules.sort_by(|left, right| left.path.cmp(&right.path));

        if !base_available && request.base.is_some() {
            full_revision_reasons.insert(
                request.entry.to_owned(),
                FullRevisionReason::BaseRevisionUnavailable,
            );
        }

        let content_id = content_id(request.entry, &modules, &request.options, request.profile);
        let delta_eligible =
            dispatch_compatible && !operations.is_empty() && full_revision_reasons.is_empty();
        let revision = RevisionManifest {
            content_id,
            base: request.base.clone(),
            profile: request.profile,
            modules,
            reload_plan: ReloadPlan {
                reused_modules,
                changed_modules,
                full_revision_reasons,
                delta_eligible,
            },
        };
        let artifact =
            assemble_complete_artifact(request.entry, request.sources, &request.options)?;
        let delta = if delta_eligible {
            Some(ModuleDelta {
                base: request
                    .base
                    .clone()
                    .expect("delta eligibility requires a base"),
                target: revision.content_id.clone(),
                operations,
            })
        } else {
            None
        };
        self.previous = Some(revision.clone());
        self.previous_analysis = Some(analyses);
        Ok(RevisionOutput {
            revision,
            artifact,
            delta,
        })
    }
}

fn analyze_module(path: &str, source: &str) -> Result<ModuleAnalysis, ConvertError> {
    let source_map: Lrc<SourceMap> = Lrc::new(SourceMap::default());
    let file = source_map.new_source_file(
        Lrc::new(FileName::Custom(path.to_owned())),
        source.to_owned(),
    );
    let mut errors = Vec::new();
    let module = parse_file_as_module(
        &file,
        Syntax::Es(EsSyntax::default()),
        EsVersion::Es2022,
        None,
        &mut errors,
    )
    .map_err(|error| {
        ConvertError::invalid(format!(
            "parse of revision module {path:?} failed: {error:?}"
        ))
    })?;
    if !errors.is_empty() {
        return Err(ConvertError::invalid(format!(
            "parse of revision module {path:?} reported diagnostics: {errors:?}"
        )));
    }

    let mut surface = String::new();
    let mut functions = BTreeMap::new();
    for item in module.body {
        match item {
            ModuleItem::Stmt(Stmt::Decl(Decl::Fn(function))) => {
                insert_function(
                    &mut functions,
                    function.ident.sym.to_string(),
                    *function.function,
                )?;
            }
            ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) => match export.decl {
                Decl::Fn(function) => {
                    surface.push_str("export:");
                    surface.push_str(&function.ident.sym);
                    surface.push('\n');
                    insert_function(
                        &mut functions,
                        function.ident.sym.to_string(),
                        *function.function,
                    )?;
                }
                declaration => append_surface_item(
                    &mut surface,
                    ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(swc_ecma_ast::ExportDecl {
                        decl: declaration,
                        ..export
                    })),
                ),
            },
            ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultDecl(export)) => match export.decl {
                DefaultDecl::Fn(function) => {
                    surface.push_str("export:default\n");
                    let name = function
                        .ident
                        .as_ref()
                        .map(|ident| ident.sym.to_string())
                        .unwrap_or_else(|| "*default*".to_owned());
                    insert_function(&mut functions, name, *function.function)?;
                }
                declaration => append_surface_item(
                    &mut surface,
                    ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultDecl(
                        swc_ecma_ast::ExportDefaultDecl {
                            decl: declaration,
                            ..export
                        },
                    )),
                ),
            },
            item => append_surface_item(&mut surface, item),
        }
    }
    Ok(ModuleAnalysis {
        surface: semantic_fingerprint(&surface),
        functions,
    })
}

fn append_surface_item(surface: &mut String, item: ModuleItem) {
    let module = Module {
        span: swc_common::DUMMY_SP,
        body: vec![item],
        shebang: None,
    };
    surface.push_str(&swc_ecma_codegen::to_code(&module));
    surface.push('\n');
}

fn insert_function(
    functions: &mut BTreeMap<String, FunctionAnalysis>,
    key: String,
    function: Function,
) -> Result<(), ConvertError> {
    if functions.contains_key(&key) {
        return Err(ConvertError::invalid(format!(
            "duplicate reloadable function key {key:?} in one module"
        )));
    }
    let body = Expr::Fn(FnExpr {
        ident: None,
        function: Box::new(function.clone()),
    });
    let mut abi_function = function;
    abi_function.body = None;
    let abi = Expr::Fn(FnExpr {
        ident: None,
        function: Box::new(abi_function),
    });
    functions.insert(
        key,
        FunctionAnalysis {
            abi: semantic_fingerprint(&swc_ecma_codegen::to_code(&abi)),
            body: semantic_fingerprint(&swc_ecma_codegen::to_code(&body)),
        },
    );
    Ok(())
}

fn collect_delta_operations(
    prior: &ModuleAnalysis,
    current: &ModuleAnalysis,
    operations: &mut Vec<DeltaOperation>,
) -> bool {
    if prior.functions.len() != current.functions.len() {
        return false;
    }
    for (key, current_function) in &current.functions {
        let Some(prior_function) = prior.functions.get(key) else {
            return false;
        };
        if prior_function.abi != current_function.abi {
            return false;
        }
        if prior_function.body != current_function.body {
            operations.push(DeltaOperation {
                function: key.clone(),
                abi: current_function.abi,
                body: current_function.body,
            });
        }
    }
    true
}

fn assemble_complete_artifact(
    entry: &str,
    sources: &BTreeMap<String, String>,
    options: &ConvertOptions,
) -> Result<RevisionArtifact, ConvertError> {
    let pairs: Vec<(&str, &str)> = sources
        .iter()
        .map(|(path, source)| (path.as_str(), source.as_str()))
        .collect();
    let set: ModuleSet<'_> = module_set_from_sources(pairs)?;
    let mut module = portal_pc_waffle::Module::empty();
    convert_modules(entry, &set, &mut module, options)?;
    let wasm = portal_pc_waffle::to_wasm_bytes(&module).map_err(|error| {
        ConvertError::invalid(format!("revision Wasm emission failed: {error}"))
    })?;
    Ok(RevisionArtifact { wasm })
}

fn content_id(
    entry: &str,
    modules: &[ModuleFingerprint],
    options: &ConvertOptions,
    profile: ReloadProfile,
) -> ContentRevisionId {
    let mut hash = Sha3_256::new();
    hash.update(REVISION_SCHEMA.as_bytes());
    hash.update([0]);
    write_field(&mut hash, entry.as_bytes());
    write_field(&mut hash, profile.tag().as_bytes());
    write_field(&mut hash, &[options.numeric_exports as u8]);
    match &options.gc_export_suffix {
        Some(suffix) => write_field(&mut hash, suffix.as_bytes()),
        None => write_field(&mut hash, b"<none>"),
    }
    for module in modules {
        write_field(&mut hash, module.path.as_bytes());
        write_field(&mut hash, module.fingerprint.to_hex().as_bytes());
    }
    ContentRevisionId(hex(&hash.finalize()))
}

fn write_field(hash: &mut Sha3_256, bytes: &[u8]) {
    hash.update((bytes.len() as u64).to_le_bytes());
    hash.update(bytes);
}

fn hex(bytes: &[u8]) -> String {
    let mut output = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        use std::fmt::Write as _;
        write!(output, "{byte:02x}").expect("writing to String cannot fail");
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sources(main: &str) -> BTreeMap<String, String> {
        BTreeMap::from([
            ("main.js".to_owned(), main.to_owned()),
            (
                "lib.js".to_owned(),
                "export function add(x) { return x + 1; }".to_owned(),
            ),
        ])
    }

    fn request<'a>(sources: &'a BTreeMap<String, String>) -> RevisionRequest<'a> {
        RevisionRequest {
            entry: "main.js",
            sources,
            options: ConvertOptions::default(),
            profile: ReloadProfile::Reinstantiate,
            base: None,
        }
    }

    #[test]
    fn identical_revision_reuses_modules_and_has_deterministic_content_id() {
        let inputs = sources("export function run(x) { return x + 1; }");
        let mut compiler = RevisionCompiler::new(MemoryFragmentCache::default());
        let first = compiler.compile(request(&inputs)).expect("first revision");
        let second = compiler.compile(request(&inputs)).expect("second revision");
        assert_eq!(first.revision.content_id, second.revision.content_id);
        assert!(!first.artifact.wasm.is_empty());
        assert_eq!(
            second.revision.reload_plan.reused_modules,
            BTreeSet::from(["lib.js".to_owned(), "main.js".to_owned()])
        );
        assert!(second.revision.reload_plan.changed_modules.is_empty());
    }

    #[test]
    fn changed_source_requires_a_complete_revision_in_the_conservative_phase() {
        let initial = sources("export function run(x) { return x + 1; }");
        let changed = sources("export function run(x) { return x + 2; }");
        let mut compiler = RevisionCompiler::new(MemoryFragmentCache::default());
        compiler
            .compile(request(&initial))
            .expect("initial revision");
        let revision = compiler
            .compile(request(&changed))
            .expect("changed revision");
        assert_eq!(
            revision.revision.reload_plan.changed_modules,
            BTreeSet::from(["main.js".to_owned()])
        );
        // Reinstantiate always ships a complete immutable artifact, but a
        // function-body-only edit does not falsely claim a top-level/module
        // surface incompatibility.
        assert!(
            revision
                .revision
                .reload_plan
                .full_revision_reasons
                .is_empty()
        );
        assert!(!revision.revision.reload_plan.delta_eligible);
    }

    #[test]
    fn dispatch_delta_updates_cells_atomically_after_base_validation() {
        let initial = sources("export function run(x) { return x + 1; }");
        let changed = sources("export function run(x) { return x + 2; }");
        let mut compiler = RevisionCompiler::new(MemoryFragmentCache::default());
        let first = compiler
            .compile(RevisionRequest {
                profile: ReloadProfile::DispatchDelta,
                ..request(&initial)
            })
            .expect("initial revision");
        let second = compiler
            .compile(RevisionRequest {
                profile: ReloadProfile::DispatchDelta,
                base: Some(first.revision.content_id.clone()),
                ..request(&changed)
            })
            .expect("changed revision");
        let delta = second.delta.clone().expect("compatible delta");
        let mut runtime = DispatchDeltaActivator::default();
        runtime
            .initialize(first.revision.content_id.clone(), [])
            .expect("initial dispatch table");
        runtime.apply(&delta).expect("base-compatible delta");
        assert_eq!(runtime.active_revision(), Some(&second.revision.content_id));
        assert_eq!(
            runtime.implementation_of("run"),
            Some(delta.operations[0].body)
        );

        let stale = ModuleDelta {
            base: ContentRevisionId("stale-base".to_owned()),
            ..delta
        };
        assert!(matches!(
            runtime.apply(&stale),
            Err(ActivationError::BaseMismatch { .. })
        ));
        assert_eq!(runtime.active_revision(), Some(&second.revision.content_id));
    }

    #[test]
    fn dispatch_profile_plans_a_same_abi_function_body_delta() {
        let initial = sources("export function run(x) { return x + 1; }");
        let changed = sources("export function run(x) { return x + 2; }");
        let mut compiler = RevisionCompiler::new(MemoryFragmentCache::default());
        let first = compiler
            .compile(RevisionRequest {
                profile: ReloadProfile::DispatchDelta,
                ..request(&initial)
            })
            .expect("initial revision");
        let second = compiler
            .compile(RevisionRequest {
                profile: ReloadProfile::DispatchDelta,
                base: Some(first.revision.content_id.clone()),
                ..request(&changed)
            })
            .expect("changed revision");
        let delta = second
            .delta
            .expect("same-ABI body edit should plan a delta");
        assert_eq!(delta.base, first.revision.content_id);
        assert_eq!(delta.target, second.revision.content_id);
        assert_eq!(delta.operations.len(), 1);
        assert_eq!(delta.operations[0].function, "run");
        assert!(second.revision.reload_plan.delta_eligible);
    }

    #[test]
    fn dispatch_profile_rejects_an_arity_change_as_a_delta() {
        let initial = sources("export function run(x) { return x + 1; }");
        let changed = sources("export function run(x, y) { return x + y; }");
        let mut compiler = RevisionCompiler::new(MemoryFragmentCache::default());
        let first = compiler
            .compile(RevisionRequest {
                profile: ReloadProfile::DispatchDelta,
                ..request(&initial)
            })
            .expect("initial revision");
        let second = compiler
            .compile(RevisionRequest {
                profile: ReloadProfile::DispatchDelta,
                base: Some(first.revision.content_id.clone()),
                ..request(&changed)
            })
            .expect("changed revision");
        assert!(second.delta.is_none());
        assert_eq!(
            second
                .revision
                .reload_plan
                .full_revision_reasons
                .get("main.js"),
            Some(&FullRevisionReason::FunctionAbiChanged)
        );
    }

    #[test]
    fn reinstantiate_switch_keeps_inflight_calls_on_the_prior_artifact() {
        let initial = sources("export function run(x) { return x + 1; }");
        let changed = sources("export function run(x) { return x + 2; }");
        let mut compiler = RevisionCompiler::new(MemoryFragmentCache::default());
        let first = compiler
            .compile(request(&initial))
            .expect("initial revision");
        let mut activator = ReinstantiatingActivator::default();
        activator.activate(&first).expect("initial activation");
        let in_flight = activator.begin_call().expect("active revision");

        let second = compiler
            .compile(RevisionRequest {
                base: Some(first.revision.content_id.clone()),
                ..request(&changed)
            })
            .expect("changed revision");
        activator.activate(&second).expect("compatible switch");
        let current = activator.begin_call().expect("new active revision");
        assert_eq!(in_flight.content_id, first.revision.content_id);
        assert_eq!(current.content_id, second.revision.content_id);
        assert_ne!(in_flight.content_id, current.content_id);
    }

    #[test]
    fn reinstantiate_switch_rejects_a_stale_base() {
        let inputs = sources("export function run(x) { return x + 1; }");
        let mut compiler = RevisionCompiler::new(MemoryFragmentCache::default());
        let output = compiler.compile(request(&inputs)).expect("revision");
        let mut activator = ReinstantiatingActivator::default();
        let stale = RevisionOutput {
            revision: RevisionManifest {
                base: Some(ContentRevisionId("not-active".to_owned())),
                ..output.revision.clone()
            },
            artifact: output.artifact.clone(),
            delta: None,
        };
        assert!(matches!(
            activator.activate(&stale),
            Err(ActivationError::BaseMismatch { .. })
        ));
    }

    #[test]
    fn unknown_delta_base_is_rejected_as_a_full_revision() {
        let inputs = sources("export function run(x) { return x + 1; }");
        let mut compiler = RevisionCompiler::new(MemoryFragmentCache::default());
        let revision = compiler
            .compile(RevisionRequest {
                profile: ReloadProfile::DispatchDelta,
                base: Some(ContentRevisionId("not-the-active-base".to_owned())),
                ..request(&inputs)
            })
            .expect("revision planning should remain deterministic");
        assert_eq!(
            revision
                .revision
                .reload_plan
                .full_revision_reasons
                .get("main.js"),
            Some(&FullRevisionReason::BaseRevisionUnavailable)
        );
    }
}
