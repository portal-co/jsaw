//! Host-boundary types for the explicit `wasm:` import namespace.
//!
//! Since DreamComp #37, host ABIs are declared structurally in the FFI
//! manifest ([`crate::ffi::FfiManifest`]); this module owns only the `wasm:`
//! specifier grammar and the legacy-suffix rejection diagnostic.

use crate::ffi::FfiBoundarySignature;
use crate::repr::ConvertError;

/// One scalar or value position in the host ABI.
pub(crate) type WasmBoundaryType = crate::ffi::FfiType;

/// The host ABI of one boundary crossing.
pub(crate) type WasmBoundarySignature = FfiBoundarySignature;

/// A manifest-resolved `wasm:<module>` named import.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct WasmImportSpec {
    pub(crate) module: String,
    pub(crate) field: String,
    pub(crate) signature: WasmBoundarySignature,
}

/// Return the host module part of a `wasm:<module>` specifier.
///
/// `None` means that the specifier is not in the reserved namespace; `Err`
/// means that it is reserved but malformed, so callers must not fall back to
/// normal ESM resolution.
pub(crate) fn parse_wasm_import_specifier(specifier: &str) -> Option<Result<&str, ConvertError>> {
    let module = specifier.strip_prefix("wasm:")?;
    Some(if module.is_empty() {
        Err(ConvertError::invalid(
            "Wasm host module specifier \"wasm:\" has an empty module name".to_string(),
        ))
    } else {
        Ok(module)
    })
}

/// Reject the removed `$wasm$` name-suffix grammar (DreamComp #37): host
/// ABIs now come from the FFI manifest.
pub(crate) fn reject_legacy_suffix(name: &str, what: &str) -> Result<(), ConvertError> {
    if name.contains("$wasm$") {
        return Err(ConvertError::invalid(format!(
            "{what} {name:?} uses the removed `$wasm$` suffix grammar; declare the ABI in the \
             FFI manifest (`ConvertOptions::ffi`)"
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognizes_reserved_host_specifiers() {
        assert_eq!(
            parse_wasm_import_specifier("wasm:env").unwrap().unwrap(),
            "env"
        );
        assert!(parse_wasm_import_specifier("wasm:").unwrap().is_err());
        assert!(parse_wasm_import_specifier("./wasm:env").is_none());
    }

    #[test]
    fn rejects_legacy_suffix_names() {
        assert!(reject_legacy_suffix("run$wasm$i32$i32", "export").is_err());
        assert!(reject_legacy_suffix("run", "export").is_ok());
    }
}
