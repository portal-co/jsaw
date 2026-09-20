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
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Options {
    /// Mirrors `ConvertOptions::numeric_exports`.
    #[serde(default = "default_true")]
    pub numeric_exports: bool,
    /// Mirrors `ConvertOptions::gc_export_suffix`.
    #[serde(default)]
    pub gc_export_suffix: Option<String>,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            numeric_exports: true,
            gc_export_suffix: None,
        }
    }
}

fn default_true() -> bool {
    true
}

/// Emission targets; each absent/`null` target is skipped.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Emit {
    /// Output-root-relative path of the native WasmGC `.wasm` file to write.
    #[serde(default)]
    pub wasm: Option<String>,
    /// Output-root-relative path of the pure core-Wasm CoreGC artifact.
    /// This lowers managed references to the generated linear-memory
    /// collector, so MVP-oriented consumers such as wasm-blitz do not need
    /// WasmGC proposal support.
    #[serde(default)]
    pub coregc_wasm: Option<String>,
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
    },
    Error {
        error: String,
    },
}

impl Result {
    pub fn ok(exports: Vec<String>, outputs: Vec<String>) -> Self {
        Result::Ok { exports, outputs }
    }

    pub fn error(message: impl Into<String>) -> Self {
        Result::Error {
            error: message.into(),
        }
    }
}
