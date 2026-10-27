//! Structured FFI declarations (DreamComp #37).
//!
//! Every host capability is a declared import with a typed ABI, supplied by
//! the host through [`FfiManifest`] (carried on
//! [`crate::ConvertOptions::ffi`]). The source-level surface is plain ESM:
//! `import { add } from "wasm:env"` binds the manifest's `env.add`
//! declaration; the legacy `$wasm$` name-suffix grammar is removed (any such
//! name fails closed with a migration diagnostic).
//!
//! Exports are declared in the same manifest: an entry in
//! [`FfiManifest::exports`] gives the named source export a raw host ABI
//! wrapper under its plain name (the generic numeric export for that name is
//! suppressed). Undeclared exports keep the default ABI.
//!
//! Manifest entries that are never imported/exported are allowed (hosts may
//! share one broad manifest across compilations); a `wasm:` import or a
//! declared export with no corresponding source name fails closed.
//!
//! Determinism: manifests are `BTreeMap`-keyed; identical manifests produce
//! identical boundary records.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt;

/// One scalar or value position in a host ABI.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FfiType {
    I32,
    I64,
    F32,
    F64,
    /// A full JS value (the lane's value representation at the boundary).
    Ref,
}

impl fmt::Display for FfiType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::F32 => "f32",
            Self::F64 => "f64",
            Self::Ref => "ref",
        };
        f.write_str(name)
    }
}

/// The structured ABI of one host function (or boundary export).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FfiBoundarySignature {
    pub params: Vec<FfiType>,
    pub result: Option<FfiType>,
}

/// One host module's declared functions.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct FfiModule {
    pub functions: BTreeMap<String, FfiBoundarySignature>,
}

/// The complete host FFI surface for one compilation.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct FfiManifest {
    /// Host module name (the `wasm:<module>` part) → declared functions.
    #[serde(default)]
    pub modules: BTreeMap<String, FfiModule>,
    /// Source export name → boundary ABI.
    #[serde(default)]
    pub exports: BTreeMap<String, FfiBoundarySignature>,
}

/// A manifest validation error.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FfiManifestError(pub String);

impl fmt::Display for FfiManifest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let modules: Vec<&String> = self.modules.keys().collect();
        write!(f, "FFI manifest (modules: {modules:?})")
    }
}

impl FfiManifest {
    /// Validate the manifest: no empty names, no `$wasm$` remnants.
    pub fn validate(&self) -> Result<(), FfiManifestError> {
        for (module, m) in &self.modules {
            if module.is_empty() {
                return Err(FfiManifestError(
                    "FFI manifest has an empty module name".into(),
                ));
            }
            for name in m.functions.keys() {
                if name.is_empty() {
                    return Err(FfiManifestError(format!(
                        "FFI manifest has an empty function name in module {module:?}"
                    )));
                }
            }
        }
        for name in self.exports.keys() {
            if name.is_empty() {
                return Err(FfiManifestError(
                    "FFI manifest has an empty export name".into(),
                ));
            }
            if name.contains("$wasm$") {
                return Err(FfiManifestError(format!(
                    "FFI manifest export {name:?} uses the removed $wasm$ suffix grammar; \
                     declare the plain export name"
                )));
            }
        }
        Ok(())
    }

    /// The declared ABI for a host import.
    pub fn import(&self, module: &str, field: &str) -> Option<&FfiBoundarySignature> {
        self.modules.get(module)?.functions.get(field)
    }
}
