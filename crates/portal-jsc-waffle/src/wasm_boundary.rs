//! Parsing and identity rules for the explicit host-Wasm source boundary.
//!
//! This module deliberately owns the `wasm:` specifier and `$wasm$` suffix
//! grammar. Linker and backend code receive parsed data rather than each
//! interpreting source strings independently.

use crate::repr::ConvertError;

/// One scalar or nullable dynamic-reference position in the host ABI.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum WasmBoundaryType {
    I32,
    I64,
    F32,
    F64,
    Ref,
}

/// The one-result host ABI declared by a `$wasm$` suffix.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct WasmBoundarySignature {
    pub(crate) params: Vec<WasmBoundaryType>,
    pub(crate) result: Option<WasmBoundaryType>,
}

/// A parsed `wasm:<module>` named import.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct WasmImportSpec {
    pub(crate) module: String,
    pub(crate) field: String,
    pub(crate) signature: WasmBoundarySignature,
}

/// A parsed entry-surface export whose source name declares a host ABI.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct WasmExportSpec {
    pub(crate) field: String,
    pub(crate) signature: WasmBoundarySignature,
}

/// Return the host module part of a `wasm:<module>` specifier.
///
/// `None` means that the specifier is not in the reserved namespace; `Err`
/// means that it is reserved but malformed, so callers must not fall back to
/// normal ESM resolution.
pub(crate) fn parse_wasm_import_specifier(
    specifier: &str,
) -> Option<Result<&str, ConvertError>> {
    let module = specifier.strip_prefix("wasm:")?;
    Some(if module.is_empty() {
        Err(ConvertError::invalid(format!(
            "Wasm host module specifier {specifier:?} has an empty module name"
        )))
    } else {
        Ok(module)
    })
}

/// Parse `<field>$wasm$<params>$<result>` and strip the suffix from `field`.
pub(crate) fn parse_wasm_boundary_name(
    name: &str,
) -> Result<(String, WasmBoundarySignature), ConvertError> {
    let marker = "$wasm$";
    let marker_index = name.rfind(marker).ok_or_else(|| {
        ConvertError::invalid(format!(
            "Wasm boundary binding {name:?} is missing the required `$wasm$` signature marker"
        ))
    })?;
    let field = &name[..marker_index];
    let suffix = &name[marker_index + marker.len()..];
    if field.is_empty() {
        return Err(ConvertError::invalid(format!(
            "Wasm boundary binding {name:?} has an empty host field name"
        )));
    }
    if field.contains(marker) {
        return Err(ConvertError::invalid(format!(
            "Wasm boundary binding {name:?} contains more than one `$wasm$` signature marker"
        )));
    }
    let mut parts = suffix.split('$');
    let params = parts.next().expect("split always yields a first part");
    let result = parts.next().ok_or_else(|| {
        ConvertError::invalid(format!(
            "Wasm boundary binding {name:?} must have `$wasm$<params>$<result>` suffix syntax"
        ))
    })?;
    if parts.next().is_some() {
        return Err(ConvertError::invalid(format!(
            "Wasm boundary binding {name:?} has more than one result separator"
        )));
    }

    let params = if params.is_empty() {
        Vec::new()
    } else {
        params
            .split('_')
            .map(|token| parse_type(name, token, false))
            .collect::<Result<Vec<_>, _>>()?
    };
    let result = if result == "v" {
        None
    } else {
        Some(parse_type(name, result, true)?)
    };
    Ok((
        field.to_owned(),
        WasmBoundarySignature { params, result },
    ))
}

/// Parse a host import declaration after its source namespace and named-import
/// form have been checked by the linker.
pub(crate) fn wasm_import_spec(
    module: &str,
    imported_name: &str,
) -> Result<WasmImportSpec, ConvertError> {
    let (field, signature) = parse_wasm_boundary_name(imported_name)?;
    Ok(WasmImportSpec {
        module: module.to_owned(),
        field,
        signature,
    })
}

/// Parse a suffix-marked entry export.
pub(crate) fn wasm_export_spec(exported_name: &str) -> Result<WasmExportSpec, ConvertError> {
    let (field, signature) = parse_wasm_boundary_name(exported_name)?;
    Ok(WasmExportSpec { field, signature })
}

fn parse_type(
    full_name: &str,
    token: &str,
    result_position: bool,
) -> Result<WasmBoundaryType, ConvertError> {
    match token {
        "i32" => Ok(WasmBoundaryType::I32),
        "i64" => Ok(WasmBoundaryType::I64),
        "f32" => Ok(WasmBoundaryType::F32),
        "f64" => Ok(WasmBoundaryType::F64),
        "ref" => Ok(WasmBoundaryType::Ref),
        "v" if !result_position => Err(ConvertError::invalid(format!(
            "Wasm boundary binding {full_name:?} uses `v` as a parameter; `v` is valid only as the result token"
        ))),
        "" => Err(ConvertError::invalid(format!(
            "Wasm boundary binding {full_name:?} has an empty ABI type token"
        ))),
        _ => Err(ConvertError::invalid(format!(
            "Wasm boundary binding {full_name:?} has unsupported ABI type token {token:?}"
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_named_host_abi_and_empty_parameter_list() {
        let (field, signature) = parse_wasm_boundary_name("clock$wasm$$i64").unwrap();
        assert_eq!(field, "clock");
        assert_eq!(signature.params, Vec::<WasmBoundaryType>::new());
        assert_eq!(signature.result, Some(WasmBoundaryType::I64));

        let (field, signature) = parse_wasm_boundary_name("add$wasm$i32_i32$i32").unwrap();
        assert_eq!(field, "add");
        assert_eq!(
            signature.params,
            vec![WasmBoundaryType::I32, WasmBoundaryType::I32]
        );
        assert_eq!(signature.result, Some(WasmBoundaryType::I32));
    }

    #[test]
    fn parses_void_and_reference_abi() {
        let (field, signature) = parse_wasm_boundary_name("log$wasm$ref$v").unwrap();
        assert_eq!(field, "log");
        assert_eq!(signature.params, vec![WasmBoundaryType::Ref]);
        assert_eq!(signature.result, None);
    }

    #[test]
    fn rejects_malformed_boundary_names() {
        for name in [
            "missing_marker",
            "$wasm$i32$i32",
            "f$wasm$i32",
            "f$wasm$i32$i32$i32",
            "f$wasm$v$i32",
            "f$wasm$i32$",
            "f$wasm$i32_i128$i32",
            "f$wasm$i32_$i32",
            "f$wasm$a$wasm$i32$i32",
        ] {
            assert!(
                parse_wasm_boundary_name(name).is_err(),
                "{name:?} must be rejected"
            );
        }
    }

    #[test]
    fn recognizes_reserved_host_specifiers() {
        assert_eq!(
            parse_wasm_import_specifier("wasm:env")
                .unwrap()
                .unwrap(),
            "env"
        );
        assert!(parse_wasm_import_specifier("wasm:").unwrap().is_err());
        assert!(parse_wasm_import_specifier("./wasm:env").is_none());
    }
}
