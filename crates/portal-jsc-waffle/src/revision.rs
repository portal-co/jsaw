//! Revision planning and fragment-cache interfaces for development-mode hot
//! code reloading.
//!
//! This module intentionally plans immutable revision artifacts. It does not
//! mutate a Wasm module instance: activation is the selected runtime's job.
//! The portable contract is a deterministic reload plan; `DispatchDelta` is a
//! runtime-specific optimization whose payload is deliberately opaque here.

use std::collections::{BTreeMap, BTreeSet};

use portal_jsc_swc_ssa::module::{FunctionFingerprint, source_fingerprint};
use sha3::{Digest, Sha3_256};

use crate::{ConvertError, ConvertOptions};

/// Version marker for serialized cache/revision records.
pub const REVISION_SCHEMA: &str = "jsaw.revision.v1";

/// A content-derived revision ID. It is safe to use as a cache namespace, but
/// a runtime must still compare it to its active base before applying a delta.
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
    /// between exported calls and resets state unless it has an explicit
    /// compatible migration adapter.
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

/// A source fragment cache. Cache implementations may evict entries or treat
/// malformed persisted records as misses; the planner always validates the
/// fingerprint before reporting reuse.
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

/// Immutable revision metadata. The complete WasmGC/Java/Swift artifacts are
/// emitted by the existing target pipeline in the next phase; this phase
/// establishes their cache/reload identity and safety classification.
#[derive(Clone, Debug)]
pub struct RevisionManifest {
    pub content_id: ContentRevisionId,
    pub base: Option<ContentRevisionId>,
    pub profile: ReloadProfile,
    pub modules: Vec<ModuleFingerprint>,
    pub reload_plan: ReloadPlan,
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
/// conservative invalidation, and content ID construction. Callers do not
/// inspect fragment cache internals or create partial module revisions.
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

    /// Plan one immutable source-module revision.
    ///
    /// Fingerprints are calculated before cache lookup, so an invalid source
    /// never aliases a valid cached record. This phase is deliberately
    /// conservative: any changed module requires a complete revision. Later
    /// dispatch-mode lowering can loosen only proven-safe body-only edges.
    pub fn compile(
        &mut self,
        request: RevisionRequest<'_>,
    ) -> Result<RevisionManifest, ConvertError> {
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
                // This source-level phase cannot yet prove whether a change
                // is a function body, a module surface, or top-level state.
                // Require a complete revision instead of risking stale init.
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
        self.previous = Some(revision.clone());
        Ok(revision)
    }
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
        assert_eq!(first.content_id, second.content_id);
        assert_eq!(
            second.reload_plan.reused_modules,
            BTreeSet::from(["lib.js".to_owned(), "main.js".to_owned()])
        );
        assert!(second.reload_plan.changed_modules.is_empty());
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
            revision.reload_plan.changed_modules,
            BTreeSet::from(["main.js".to_owned()])
        );
        assert_eq!(
            revision.reload_plan.full_revision_reasons.get("main.js"),
            Some(&FullRevisionReason::TopLevelChanged)
        );
        assert!(!revision.reload_plan.delta_eligible);
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
            revision.reload_plan.full_revision_reasons.get("main.js"),
            Some(&FullRevisionReason::BaseRevisionUnavailable)
        );
    }
}
