//! Identifier sanitization shared by the Java and Swift renderers.
//!
//! Wasm-side names (export names, function names) are arbitrary text;
//! generated identifiers must be valid, keyword-free, and deterministic in
//! both target languages.

/// Characters allowed verbatim in generated identifiers.
fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Replace non-identifier characters with `_` and prefix `_` when the
/// result would start with a digit.
pub fn sanitize(name: &str) -> String {
    let mut out: String = name.chars().map(|c| if is_ident_char(c) { c } else { '_' }).collect();
    if out.is_empty() {
        out.push('_');
    }
    if out.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        out.insert(0, '_');
    }
    out
}

const JAVA_KEYWORDS: &[&str] = &[
    "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const",
    "continue", "default", "do", "double", "else", "enum", "extends", "final", "finally", "float",
    "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long",
    "native", "new", "package", "private", "protected", "public", "return", "short", "static",
    "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "try",
    "void", "volatile", "while", "true", "false", "null", "var", "yield", "record", "sealed",
    "permits",
];

const SWIFT_KEYWORDS: &[&str] = &[
    "associatedtype", "class", "deinit", "enum", "extension", "fileprivate", "func", "import",
    "init", "inout", "internal", "let", "open", "operator", "private", "precedencegroup",
    "protocol", "public", "rethrows", "static", "struct", "subscript", "typealias", "var",
    "break", "case", "catch", "continue", "default", "defer", "do", "else", "fallthrough", "for",
    "guard", "if", "in", "repeat", "return", "throw", "switch", "where", "while", "as", "Any",
    "false", "is", "nil", "self", "Self", "super", "throws", "true", "try", "Type", "Protocol",
    "async", "await", "actor", "isolated", "nonisolated", "some", "any", "macro",
];

/// Sanitize and escape a Java identifier (suffix `_` on keywords).
pub fn java(name: &str) -> String {
    let s = sanitize(name);
    if JAVA_KEYWORDS.contains(&s.as_str()) {
        format!("{s}_")
    } else {
        s
    }
}

/// Sanitize and escape a Swift identifier (backtick keywords).
pub fn swift(name: &str) -> String {
    let s = sanitize(name);
    if SWIFT_KEYWORDS.contains(&s.as_str()) {
        format!("`{s}`")
    } else {
        s
    }
}

/// The canonical generated name for a function: `f{index}`.
pub fn func_name(index: usize) -> String {
    format!("f{index}")
}

/// The canonical generated name for a function's trampoline-protocol body
/// method: `f{index}$step` (Java-legal; the Swift backend uses its own).
pub fn step_name(index: usize) -> String {
    format!("f{index}$step")
}

/// The canonical generated class name for a struct signature: `S{index}`.
pub fn struct_name(index: usize) -> String {
    format!("S{index}")
}

/// The canonical generated name for a local.
pub fn local_name(index: u32) -> String {
    format!("l{index}")
}

/// The canonical generated name for a branch label.
pub fn label_name(index: u32) -> String {
    format!("b{index}")
}
