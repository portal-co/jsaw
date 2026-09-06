//! Minimal YAML frontmatter scanner for test262 test files.
//!
//! test262 embeds metadata in a leading `/*--- ... ---*/` comment. Only the
//! subset of the format the runner consumes is parsed: `flags:`, `negative:`
//! and `includes:`. Nested/flow YAML is deliberately not supported — the
//! runner fails closed (reports the test as skipped) on anything it cannot
//! interpret.

use serde::Serialize;
use std::collections::BTreeSet;

/// Extracted frontmatter metadata.
#[derive(Debug, Default, Clone)]
pub struct Frontmatter {
    pub flags: BTreeSet<String>,
    /// `(phase, type)` from the `negative:` block, when present.
    pub negative: Option<Negative>,
    pub includes: Vec<String>,
    /// Human-readable description (for reports).
    pub description: String,
}

#[derive(Debug, Default, Clone, Serialize)]
pub struct Negative {
    pub phase: String,
    #[serde(rename = "type")]
    pub kind: String,
}

/// Strip and parse the frontmatter block. Returns the remaining source and
/// the metadata. Errors when the file opens with a frontmatter-looking block
/// that cannot be parsed conservatively.
pub fn strip(source: &str) -> Result<(String, Frontmatter), String> {
    let trimmed = source.trim_start_matches(['\u{feff}']).trim_start();
    let Some(rest) = trimmed.strip_prefix("/*---") else {
        return Ok((source.to_owned(), Frontmatter::default()));
    };
    let Some(end) = rest.find("---*/") else {
        // A `/*---` without a closing marker is a plain comment; leave the
        // source untouched.
        return Ok((source.to_owned(), Frontmatter::default()));
    };
    let yaml = &rest[..end];
    let remaining = &rest[end + "---*/".len()..];
    let meta = parse_yaml(yaml)?;
    Ok((remaining.to_owned(), meta))
}

fn parse_yaml(yaml: &str) -> Result<Frontmatter, String> {
    let mut meta = Frontmatter::default();
    let mut section: Section = Section::None;
    for raw_line in yaml.lines() {
        let line = raw_line.trim_end();
        let trimmed = line.trim_start();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        // Nested entries are two-space indented.
        let indent = line.len() - trimmed.len();
        match section {
            Section::None => {
                if let Some((key, value)) = split_kv(trimmed) {
                    match key {
                        "flags" => {
                            if value.is_empty() {
                                section = Section::Flags;
                            } else {
                                parse_list(value, &mut meta.flags);
                            }
                        }
                        "includes" => {
                            if value.is_empty() {
                                section = Section::Includes;
                            } else {
                                meta.includes = parse_str_list(value);
                            }
                        }
                        "description" => meta.description = unquote(value).to_owned(),
                        "negative" => {
                            if value.is_empty() {
                                section = Section::Negative;
                            } else {
                                return Err(format!("unsupported inline negative: {value}"));
                            }
                        }
                        _ => {} // esid, info, author, ... ignored
                    }
                }
            }
            Section::Flags => {
                if indent == 0 {
                    section = Section::None;
                    retry_key(trimmed, &mut meta, &mut section);
                } else if let Some(item) = trimmed.strip_prefix("- ") {
                    meta.flags.insert(unquote(item).to_owned());
                }
            }
            Section::Includes => {
                if indent == 0 {
                    section = Section::None;
                    retry_key(trimmed, &mut meta, &mut section);
                } else if let Some(item) = trimmed.strip_prefix("- ") {
                    meta.includes.push(unquote(item).to_owned());
                }
            }
            Section::Negative => {
                if indent == 0 {
                    section = Section::None;
                    retry_key(trimmed, &mut meta, &mut section);
                } else if let Some((key, value)) = split_kv(trimmed) {
                    let negative = meta.negative.get_or_insert_with(Default::default);
                    match key {
                        "phase" => negative.phase = unquote(value).to_owned(),
                        "type" => negative.kind = unquote(value).to_owned(),
                        _ => {}
                    }
                }
            }
        }
    }
    if let Some(negative) = &meta.negative {
        if negative.phase.is_empty() || negative.kind.is_empty() {
            return Err("negative block missing phase or type".to_owned());
        }
    }
    Ok(meta)
}

fn retry_key(trimmed: &str, meta: &mut Frontmatter, section: &mut Section) {
    if let Some((key, value)) = split_kv(trimmed) {
        match key {
            "flags" => {
                if value.is_empty() {
                    *section = Section::Flags;
                } else {
                    parse_list(value, &mut meta.flags);
                }
            }
            "includes" => {
                if value.is_empty() {
                    *section = Section::Includes;
                } else {
                    meta.includes = parse_str_list(value);
                }
            }
            "negative" if value.is_empty() => *section = Section::Negative,
            _ => {}
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Section {
    None,
    Flags,
    Includes,
    Negative,
}

fn split_kv(trimmed: &str) -> Option<(&str, &str)> {
    let (key, value) = trimmed.split_once(':')?;
    Some((key.trim(), value.trim()))
}

fn parse_list(value: &str, into: &mut BTreeSet<String>) {
    for item in value.trim_start_matches('[').trim_end_matches(']').split(',') {
        let item = item.trim();
        if !item.is_empty() {
            into.insert(unquote(item).to_owned());
        }
    }
}

fn parse_str_list(value: &str) -> Vec<String> {
    value
        .trim_start_matches('[')
        .trim_end_matches(']')
        .split(',')
        .map(|item| unquote(item.trim()).to_owned())
        .filter(|item| !item.is_empty())
        .collect()
}

fn unquote(value: &str) -> &str {
    let value = value.trim();
    if (value.starts_with('"') && value.ends_with('"') && value.len() >= 2)
        || (value.starts_with('\'') && value.ends_with('\'') && value.len() >= 2)
    {
        &value[1..value.len() - 1]
    } else {
        value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_flags_includes_and_negative() {
        let source = r#"/*---
esid: sec-array.prototype.foreach
description: forEach visits holes
flags: [onlyStrict]
includes: [compareArray.js, wellKnownIntrinsicObjects.js]
negative:
  phase: runtime
  type: Test262Error
---*/var x = 1;"#;
        let (remaining, meta) = strip(source).unwrap();
        assert_eq!(remaining, "var x = 1;");
        assert!(meta.flags.contains("onlyStrict"));
        assert_eq!(meta.includes, vec!["compareArray.js", "wellKnownIntrinsicObjects.js"]);
        let negative = meta.negative.unwrap();
        assert_eq!(negative.phase, "runtime");
        assert_eq!(negative.kind, "Test262Error");
    }

    #[test]
    fn leaves_files_without_frontmatter_alone() {
        let (remaining, meta) = strip("var x = 1;\n/*--- not frontmatter ---*/").unwrap();
        assert_eq!(remaining, "var x = 1;\n/*--- not frontmatter ---*/");
        assert!(meta.flags.is_empty());
    }

    #[test]
    fn multiline_flags() {
        let source = "/*---\nflags:\n  - module\n  - raw\n---*/x;";
        let (_, meta) = strip(source).unwrap();
        assert!(meta.flags.contains("module"));
        assert!(meta.flags.contains("raw"));
    }
}
