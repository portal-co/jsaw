//! Revision planning and fragment-cache interfaces for development-mode hot
//! code reloading.
//!
//! This module plans immutable revision artifacts. It never mutates a running
//! Wasm instance: activation is the selected runtime's responsibility.

use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use portal_jsc_swc_ssa::module::{FunctionFingerprint, source_fingerprint};
use sha3::{Digest, Sha3_256};

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
}

impl<C: FragmentCache> RevisionCompiler<C> {
    pub fn new(cache: C) -> Self {
        Self {
            cache,
            previous: None,
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
        let mut reused_modules = BTreeSet::new();
        let mut changed_modules = BTreeSet::new();
        let mut full_revision_reasons = BTreeMap::new();

        for (path, source) in request.sources {
            let fingerprint = source_fingerprint(source).map_err(|error| {
                ConvertError::invalid(format!(
                    "parse of revision module {path:?} failed: {error:?}"
                ))
            })?;
            if self.cache.get(path) == Some(fingerprint) {
                reused_modules.insert(path.clone());
            } else {
                changed_modules.insert(path.clone());
                // Source-level caching cannot yet prove a body-only change is
                // independent from module init/layout. Require a full revision.
                full_revision_reasons.insert(path.clone(), FullRevisionReason::TopLevelChanged);
            }
            self.cache.put(path.clone(), fingerprint);
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
        let delta_eligible = request.profile == ReloadProfile::DispatchDelta
            && request.base.is_some()
            && base_available
            && changed_modules.is_empty();
        let revision = RevisionManifest {
            content_id,
            base: request.base,
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
        self.previous = Some(revision.clone());
        Ok(RevisionOutput { revision, artifact })
    }
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
        assert_eq!(
            revision
                .revision
                .reload_plan
                .full_revision_reasons
                .get("main.js"),
            Some(&FullRevisionReason::TopLevelChanged)
        );
        assert!(!revision.revision.reload_plan.delta_eligible);
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
