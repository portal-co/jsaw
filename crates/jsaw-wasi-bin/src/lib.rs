//! The compile pipeline the wasip1 binary runs, factored into a library
//! so it is unit-testable natively: read the manifest, read sources,
//! compile, write outputs, produce the result JSON.

use std::collections::BTreeMap;
use std::fmt;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
use portal_jsc_common::assumptions::{
    AssumptionDiagnostic, AssumptionRegistry, AssumptionRequest, AssumptionSet,
};
use portal_jsc_waffle::{ConvertOptions, ModuleSet, convert_modules, module_set_from_sources};

pub mod manifest;

/// The canonical assumption registry, re-exported so hosts and tests can
/// resolve and verify digests without a second copy of the vocabulary.
pub use portal_jsc_common::assumptions;

use manifest::{AssumptionDiagnosticReport, AssumptionEntry, AssumptionReport};
pub use manifest::{Emit, MANIFEST_VERSION, Manifest, Options};

/// Everything a successful compilation produced.
#[derive(Debug)]
pub struct CompileOutput {
    /// Wasm-exported function names of the compiled module.
    pub exports: Vec<String>,
    /// Output-root-relative path to bytes.
    pub outputs: BTreeMap<String, Vec<u8>>,
    /// Resolved assumption set metadata (present even when empty).
    pub assumptions: AssumptionReport,
}

/// Compilation failure, distinguishing fail-closed assumption resolution
/// (machine-readable diagnostics) from ordinary pipeline errors.
#[derive(Debug)]
pub enum CompileError {
    Assumptions(Vec<AssumptionDiagnosticReport>),
    Other(anyhow::Error),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Assumptions(diagnostics) => write!(
                f,
                "{} assumption request(s) could not be resolved",
                diagnostics.len()
            ),
            Self::Other(error) => write!(f, "{error:#}"),
        }
    }
}

impl std::error::Error for CompileError {}

impl From<anyhow::Error> for CompileError {
    fn from(error: anyhow::Error) -> Self {
        Self::Other(error)
    }
}

fn report_entry(request: &AssumptionRequest) -> AssumptionEntry {
    AssumptionEntry {
        name: request.name().to_string(),
        parameter: request.parameter().map(|p| p.as_str().to_string()),
    }
}

fn report_diagnostic(diagnostic: &AssumptionDiagnostic) -> AssumptionDiagnosticReport {
    match diagnostic {
        AssumptionDiagnostic::Unknown { name, index } => AssumptionDiagnosticReport::Unknown {
            name: name.clone(),
            index: *index,
        },
        AssumptionDiagnostic::Duplicate {
            name,
            index,
            first_index,
        } => AssumptionDiagnosticReport::Duplicate {
            name: name.clone(),
            index: *index,
            first_index: *first_index,
        },
        AssumptionDiagnostic::Conflict {
            name,
            index,
            conflicts_with,
            conflict_index,
            reason,
        } => AssumptionDiagnosticReport::Conflict {
            name: name.clone(),
            index: *index,
            conflicts_with: conflicts_with.clone(),
            conflict_index: *conflict_index,
            reason: reason.clone(),
        },
        AssumptionDiagnostic::Unsupported {
            name,
            index,
            reason,
        } => AssumptionDiagnosticReport::Unsupported {
            name: name.clone(),
            index: *index,
            reason: reason.clone(),
        },
        AssumptionDiagnostic::Unfulfilled {
            name,
            index,
            reason,
        } => AssumptionDiagnosticReport::Unfulfilled {
            name: name.clone(),
            index: *index,
            reason: reason.clone(),
        },
    }
}

/// Resolve the manifest's requested assumptions through the canonical
/// registry. Resolution is fail-closed: any diagnostic rejects the entire
/// compilation before any lowering runs.
pub fn resolve_assumptions(
    manifest: &Manifest,
) -> Result<(AssumptionSet, AssumptionReport), CompileError> {
    let registry = AssumptionRegistry::new();
    let requests: Vec<AssumptionRequest> = manifest
        .options
        .assumptions
        .iter()
        .map(|input| match input.parameter() {
            Some(parameter) => {
                AssumptionRequest::with_parameter(input.name().to_string(), parameter.to_string())
            }
            None => AssumptionRequest::new(input.name().to_string()),
        })
        .collect();
    let set = registry.resolve(requests).map_err(|error| {
        CompileError::Assumptions(error.diagnostics().iter().map(report_diagnostic).collect())
    })?;
    let report = AssumptionReport {
        schema_version: registry.schema_version(),
        registry_version: registry.version(),
        requested: set.requested().iter().map(report_entry).collect(),
        effective: set.effective().iter().map(report_entry).collect(),
        digest: set.digest(),
    };
    Ok((set, report))
}

/// Run a compilation from in-memory sources (no filesystem). This is the
/// target-independent core: given the manifest and every module's source,
/// produce the outputs as an in-memory map of output-root-relative path
/// to bytes. Both the binary (which then writes the files under the
/// preopened output root) and tests use this.
pub fn compile_to_outputs(
    manifest: &Manifest,
    sources: &BTreeMap<String, String>,
) -> Result<CompileOutput, CompileError> {
    if manifest.version != MANIFEST_VERSION {
        return Err(anyhow::anyhow!(
            "unsupported manifest version {} (this compiler understands {})",
            manifest.version,
            MANIFEST_VERSION
        )
        .into());
    }
    if !manifest.modules.iter().any(|m| m == &manifest.entry) {
        return Err(
            anyhow::anyhow!("entry module {:?} is not in the module set", manifest.entry).into(),
        );
    }

    // Resolve assumptions before any heavy work: invalid requests fail the
    // whole compilation closed, with machine-readable diagnostics.
    let (assumptions, assumption_report) = resolve_assumptions(manifest)?;

    // Feed the sources to the ingestion pipeline in sorted order (the set
    // is a BTreeMap anyway; sorting just keeps diagnostics deterministic).
    let mut pairs: Vec<(&str, &str)> = Vec::with_capacity(manifest.modules.len());
    for key in &manifest.modules {
        let source = sources
            .get(key)
            .with_context(|| format!("module {key:?} listed in the manifest has no source"))?;
        pairs.push((key.as_str(), source.as_str()));
    }
    let mut set: ModuleSet<'_> = module_set_from_sources(pairs)
        .map_err(|error| anyhow::anyhow!("ingestion failed: {error}"))?;

    // Wasm binary members ride in the manifest as base64 (the manifest is
    // JSON); each becomes a closed-world member under its `.wasm` key.
    for member in &manifest.wasm {
        let bytes =
            base64::Engine::decode(&base64::engine::general_purpose::STANDARD, &member.base64)
                .with_context(|| format!("Wasm member {:?} is not valid base64", member.path))?;
        set.insert_wasm(&member.path, &bytes).map_err(|error| {
            anyhow::anyhow!("Wasm member {:?} was rejected: {error}", member.path)
        })?;
    }

    let options = ConvertOptions {
        numeric_exports: manifest.options.numeric_exports,
        gc_export_suffix: manifest.options.gc_export_suffix.clone(),
        assumptions,
        ffi: manifest.ffi.clone(),
    };
    let mut module = portal_pc_waffle::Module::empty();
    convert_modules(&manifest.entry, &set, &mut module, &options)
        .map_err(|error| anyhow::anyhow!("lowering failed: {error}"))?;

    // Collect the public Wasm function exports for the result.
    let mut exports: Vec<String> = module
        .exports
        .iter()
        .filter(|e| matches!(e.kind, portal_pc_waffle::ExportKind::Func(_)))
        .map(|e| e.name.clone())
        .collect();
    exports.sort();

    let mut outputs: BTreeMap<String, Vec<u8>> = BTreeMap::new();

    if let Some(wasm_path) = &manifest.emit.wasm {
        let bytes = portal_pc_waffle::to_wasm_bytes(&module).context("Wasm emission failed")?;
        outputs.insert(wasm_path.clone(), bytes);
    }
    if let Some(java_dir) = &manifest.emit.java {
        let sources = portal_jsc_jvm_emit::emit_java(&module).context("Java emission failed")?;
        for (path, content) in &sources.files {
            outputs.insert(join_rel(java_dir, path), content.clone().into_bytes());
        }
    }
    if let Some(swift_dir) = &manifest.emit.swift {
        let sources =
            portal_jsc_swift_emit::emit_swift(&module).context("Swift emission failed")?;
        for (path, content) in &sources.files {
            outputs.insert(join_rel(swift_dir, path), content.clone().into_bytes());
        }
    }

    Ok(CompileOutput {
        exports,
        outputs,
        assumptions: assumption_report,
    })
}

/// Join an output-root-relative directory and a file within it using `/`
/// (the manifest's path convention, independent of the host OS).
fn join_rel(dir: &str, file: &str) -> String {
    if dir.is_empty() {
        file.to_string()
    } else {
        format!("{}/{}", dir.trim_end_matches('/'), file)
    }
}

/// Read every manifest-listed module's source from `src_root`.
pub fn read_sources(src_root: &Path, manifest: &Manifest) -> Result<BTreeMap<String, String>> {
    let mut sources = BTreeMap::new();
    for key in &manifest.modules {
        let path = safe_join(src_root, key)?;
        let content = std::fs::read_to_string(&path)
            .with_context(|| format!("could not read module source {}", path.display()))?;
        sources.insert(key.clone(), content);
    }
    Ok(sources)
}

/// Write every output under `out_root`, creating parent directories.
pub fn write_outputs(out_root: &Path, outputs: &BTreeMap<String, Vec<u8>>) -> Result<()> {
    for (rel, bytes) in outputs {
        let path = safe_join(out_root, rel)?;
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("could not create {}", parent.display()))?;
        }
        std::fs::write(&path, bytes)
            .with_context(|| format!("could not write {}", path.display()))?;
    }
    Ok(())
}

/// Join a root-relative manifest path, rejecting anything that escapes
/// the root (absolute paths or `..` components).
fn safe_join(root: &Path, rel: &str) -> Result<PathBuf> {
    let rel_path = Path::new(rel);
    if rel_path.is_absolute() {
        bail!("manifest path {rel:?} must be relative");
    }
    let mut out = root.to_path_buf();
    for component in rel_path.components() {
        match component {
            std::path::Component::Normal(part) => out.push(part),
            std::path::Component::CurDir => {}
            _ => bail!("manifest path {rel:?} must not contain `..` or other special components"),
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use base64::Engine;
    use base64::engine::general_purpose::STANDARD;

    /// `(module (func (export "add") (param i32 i32) (result i32) ...))`
    fn add_module_base64() -> String {
        let mut bytes = b"\0asm\x01\0\0\0".to_vec();
        bytes.extend([0x01, 0x07, 0x01, 0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f]);
        bytes.extend([0x03, 0x02, 0x01, 0x00]);
        bytes.extend([0x07, 0x07, 0x01, 0x03, b'a', b'd', b'd', 0x00, 0x00]);
        bytes.extend([
            0x0a, 0x09, 0x01, 0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0x6a, 0x0b,
        ]);
        STANDARD.encode(bytes)
    }

    fn manifest(source: &str) -> (Manifest, BTreeMap<String, String>) {
        let manifest: Manifest = serde_json::from_value(serde_json::json!({
            "version": 1,
            "entry": "main.js",
            "modules": ["main.js"],
            "wasm": [{ "path": "math.wasm", "base64": add_module_base64() }],
        }))
        .expect("manifest JSON");
        let sources = BTreeMap::from([("main.js".to_string(), source.to_string())]);
        (manifest, sources)
    }

    #[test]
    fn manifest_wasm_members_join_the_closed_set() {
        let (manifest, sources) =
            manifest("import { add } from './math.wasm'; export function run() { return 1; }");
        compile_to_outputs(&manifest, &sources)
            .expect("an uncalled wasm import must not fail compilation");
    }

    #[test]
    fn manifest_wasm_calls_compile_since_phase_2() {
        let (manifest, sources) = manifest(
            "import { add } from './math.wasm'; export function run(a, b) { return add(a, b); }",
        );
        compile_to_outputs(&manifest, &sources)
            .expect("calls into wasm members lower since phase 2");
    }

    #[test]
    fn manifest_rejects_bad_base64() {
        let manifest: Manifest = serde_json::from_value(serde_json::json!({
            "version": 1,
            "entry": "main.js",
            "modules": ["main.js"],
            "wasm": [{ "path": "math.wasm", "base64": "!!! not base64 !!!" }],
        }))
        .expect("manifest JSON");
        let sources = BTreeMap::from([(
            "main.js".to_string(),
            "export function run() { return 1; }".to_string(),
        )]);
        let error =
            compile_to_outputs(&manifest, &sources).expect_err("invalid base64 must be rejected");
        assert!(format!("{error:#}").contains("base64"), "{error}");
    }
}
