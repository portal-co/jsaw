//! The versioned JSON interface between the Gradle plugin (or any host)
//! and the jsaw compiler wasm binary.
//!
//! The host writes a [`Manifest`] to the binary's stdin and preopens two
//! directories: the source root (read-only) and an output root
//! (read-write). The binary reads the listed modules from the source
//! root, compiles, writes the requested outputs under the output root,
//! and prints a single [`Result`] JSON line to stdout. Keeping this in a
//! small library lets both the binary and the host's tests share the
//! exact schema.

use serde::{Deserialize, Serialize};

/// Current manifest schema version.
pub const MANIFEST_VERSION: u32 = 1;

/// What to compile and what to emit. Paths in `modules` are relative to
/// the preopened source root and double as the linker module keys (so
/// `./a.js` specifiers resolve against them). Paths in `emit` are
/// relative to the preopened output root.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Manifest {
    pub version: u32,
    /// Entry module key; must be one of `modules`.
    pub entry: String,
    /// Every module in the closed world, as source-root-relative keys.
    pub modules: Vec<String>,
    #[serde(default)]
    pub options: Options,
    #[serde(default)]
    pub emit: Emit,
    /// The structured host FFI manifest (DreamComp #37): every `wasm:` host
    /// import and every host-ABI export must be declared here.
    #[serde(default)]
    pub ffi: portal_jsc_waffle::FfiManifest,
}

/// One requested assumption in the manifest. The plain-string form names an
/// assumption without parameters; the object form carries an optional
/// canonical parameter. Names and resolution semantics are the canonical
/// `portal_jsc_common::assumptions` registry's; this type is only the wire
/// representation.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum AssumptionInput {
    /// `"no-jit"`
    Name(String),
    /// `{"name": "no-jit", "parameter": "..."}`
    Full {
        name: String,
        #[serde(default)]
        parameter: Option<String>,
    },
}

impl AssumptionInput {
    pub fn name(&self) -> &str {
        match self {
            Self::Name(name) => name,
            Self::Full { name, .. } => name,
        }
    }

    pub fn parameter(&self) -> Option<&str> {
        match self {
            Self::Name(_) => None,
            Self::Full { parameter, .. } => parameter.as_deref(),
        }
    }
}

/// One assumption name (plus optional canonical parameter) in a result report.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct AssumptionEntry {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameter: Option<String>,
}

/// The resolved assumption metadata attached to every successful result.
/// This is the seed of the certification manifest: it carries the canonical
/// registry/schema revisions, the requested and effective sets, and the
/// exact SHA3-256 digest of the canonical encoding.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct AssumptionReport {
    pub schema_version: u16,
    pub registry_version: u16,
    pub requested: Vec<AssumptionEntry>,
    pub effective: Vec<AssumptionEntry>,
    /// Lowercase hex SHA3-256 of the canonical set encoding.
    pub digest: String,
}

/// Machine-readable mirror of one canonical assumption diagnostic.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum AssumptionDiagnosticReport {
    Unknown {
        name: String,
        index: usize,
    },
    Duplicate {
        name: String,
        index: usize,
        first_index: usize,
    },
    Conflict {
        name: String,
        index: usize,
        conflicts_with: String,
        conflict_index: usize,
        reason: String,
    },
    Unsupported {
        name: String,
        index: usize,
        reason: String,
    },
    Unfulfilled {
        name: String,
        index: usize,
        reason: String,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Options {
    /// Mirrors `ConvertOptions::numeric_exports`.
    #[serde(default = "default_true")]
    pub numeric_exports: bool,
    /// Mirrors `ConvertOptions::gc_export_suffix`.
    #[serde(default)]
    pub gc_export_suffix: Option<String>,
    /// Requested named assumptions (canonical registry schema v1). Empty by
    /// default, which preserves the legacy no-assumptions behavior.
    #[serde(default)]
    pub assumptions: Vec<AssumptionInput>,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            numeric_exports: true,
            gc_export_suffix: None,
            assumptions: Vec::new(),
        }
    }
}

fn default_true() -> bool {
    true
}

/// Emission targets; each absent/`null` target is skipped.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Emit {
    /// Output-root-relative path of the WasmGC `.wasm` file to write.
    #[serde(default)]
    pub wasm: Option<String>,
    /// Output-root-relative directory to write the Java sources into.
    #[serde(default)]
    pub java: Option<String>,
    /// Output-root-relative directory to write the Swift sources into.
    #[serde(default)]
    pub swift: Option<String>,
}

/// The single stdout line the binary prints on completion.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "status", rename_all = "lowercase")]
pub enum Result {
    Ok {
        /// Wasm-exported function names of the compiled module.
        exports: Vec<String>,
        /// Every output-root-relative path the binary wrote.
        outputs: Vec<String>,
        /// Resolved assumption set and its canonical digest. Present even
        /// when no assumptions were requested (empty set, empty-set digest).
        assumptions: AssumptionReport,
    },
    Error {
        error: String,
    },
    /// Assumption resolution failed before any compilation. Distinct from
    /// the generic `error` so hosts can react to bad requests without
    /// parsing prose; the process exits nonzero.
    #[serde(rename = "assumption_error")]
    AssumptionError {
        error: String,
        diagnostics: Vec<AssumptionDiagnosticReport>,
    },
}

impl Result {
    pub fn ok(exports: Vec<String>, outputs: Vec<String>, assumptions: AssumptionReport) -> Self {
        Result::Ok {
            exports,
            outputs,
            assumptions,
        }
    }

    pub fn error(message: impl Into<String>) -> Self {
        Result::Error {
            error: message.into(),
        }
    }

    pub fn assumption_error(diagnostics: Vec<AssumptionDiagnosticReport>) -> Self {
        Result::AssumptionError {
            error: format!("{} assumption request(s) could not be resolved", diagnostics.len()),
            diagnostics,
        }
    }
}
