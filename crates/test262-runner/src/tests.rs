//! Test file selection, strict/sloppy variant expansion, and harness
//! concatenation for the test262 runner.

use std::path::{Path, PathBuf};

use crate::frontmatter::Frontmatter;

/// A concrete compile-and-run unit derived from one test file.
#[derive(Debug, Clone)]
pub struct Variant {
    /// Test path relative to the test262 root, e.g.
    /// `built-ins/TypedArray/from/from-arraybuffer-custom-proto.js`.
    pub rel_path: String,
    /// `default`, `strict`, `sloppy` or `raw`.
    pub variant: &'static str,
    /// Full source to compile (frontmatter stripped, harness prepended,
    /// strictness directive applied).
    pub source: String,
    pub meta: Frontmatter,
}

/// Expand one test file into the variants the runner will attempt.
///
/// Variant matrix (mirrors the official runner semantics for the subset of
/// flags we honor):
/// - `raw`: only the raw variant, no strictness directive, no harness
///   prepended (raw tests manage their own environment).
/// - `module`: only the default variant; `use strict` is implied by the
///   module grammar and strict/sloppy do not apply.
/// - `onlyStrict`: only the strict variant.
/// - `noStrict`: only the default (sloppy-preserving) variant.
/// - otherwise: default and strict variants.
pub fn expand(
    rel_path: &Path,
    source: &str,
    meta: Frontmatter,
    harness_dir: &Path,
) -> Result<Vec<Variant>, String> {
    let rel = rel_path.to_string_lossy().replace('\\', "/");
    let is_module = meta.flags.contains("module");
    let is_raw = meta.flags.contains("raw");
    let only_strict = meta.flags.contains("onlyStrict");
    let no_strict = meta.flags.contains("noStrict");

    let mut variants = Vec::new();
    if is_raw {
        variants.push(build_variant(
            &rel,
            "raw",
            source.to_owned(),
            &meta,
            harness_dir,
            false,
            false,
        )?);
        return Ok(variants);
    }
    if is_module {
        variants.push(build_variant(
            &rel,
            "default",
            source.to_owned(),
            &meta,
            harness_dir,
            false,
            true,
        )?);
        return Ok(variants);
    }
    if only_strict || no_strict {
        if only_strict {
            variants.push(build_variant(
                &rel,
                "strict",
                source.to_owned(),
                &meta,
                harness_dir,
                true,
                false,
            )?);
        } else {
            variants.push(build_variant(
                &rel,
                "sloppy",
                source.to_owned(),
                &meta,
                harness_dir,
                false,
                false,
            )?);
        }
        return Ok(variants);
    }
    variants.push(build_variant(
        &rel,
        "default",
        source.to_owned(),
        &meta,
        harness_dir,
        false,
        false,
    )?);
    variants.push(build_variant(
        &rel,
        "strict",
        source.to_owned(),
        &meta,
        harness_dir,
        true,
        false,
    )?);
    Ok(variants)
}

#[allow(clippy::too_many_arguments)]
fn build_variant(
    rel_path: &str,
    variant: &'static str,
    mut source: String,
    meta: &Frontmatter,
    harness_dir: &Path,
    force_strict: bool,
    _is_module: bool,
) -> Result<Variant, String> {
    if variant != "raw" {
        // Prepend the implicit assert.js plus each `includes:` file, in
        // order. (Older checkouts expect their monolithic harness.js instead;
        // it is prepended when present.)
        let mut preamble = String::new();
        let legacy = harness_dir.join("harness.js");
        if legacy.is_file() {
            preamble.push_str(
                &std::fs::read_to_string(&legacy)
                    .map_err(|error| format!("reading harness.js: {error}"))?,
            );
        } else {
            let assert_js = harness_dir.join("assert.js");
            preamble.push_str(
                &std::fs::read_to_string(&assert_js)
                    .map_err(|error| format!("reading assert.js: {error}"))?,
            );
        }
        for include in &meta.includes {
            let include_path = harness_dir.join(include);
            let contents = std::fs::read_to_string(&include_path)
                .map_err(|error| format!("reading include {include}: {error}"))?;
            preamble.push('\n');
            preamble.push_str(&contents);
        }
        source.insert_str(0, &preamble);
        if force_strict {
            // Inserted after the harness so the directive governs only the
            // test body, matching the official runner's per-file wrapping
            // well enough for our purposes.
            source.insert_str(0, "\"use strict\";\n");
        }
    }
    Ok(Variant {
        rel_path: rel_path.to_owned(),
        variant,
        source,
        meta: meta.clone(),
    })
}

/// Collect test files under `root` matching any of the `suite_globs`
/// (prefix-style filters such as `built-ins/Math` — a file matches when its
/// relative path starts with the filter). Returns paths relative to `root`.
pub fn collect_tests(root: &Path, suite_filters: &[String]) -> anyhow::Result<Vec<PathBuf>> {
    let mut tests = Vec::new();
    let base = root.join("test");
    walk(&base, &base, &mut |rel, path| {
        if !rel.starts_with("harness/") && rel.ends_with(".js") {
            if suite_filters.is_empty()
                || suite_filters.iter().any(|filter| rel.starts_with(filter.as_str()))
            {
                tests.push(path.to_owned());
            }
        }
    })?;
    tests.sort();
    Ok(tests)
}

fn walk(
    base: &Path,
    dir: &Path,
    visit: &mut impl FnMut(String, &Path),
) -> anyhow::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            walk(base, &path, visit)?;
        } else {
            let rel = path
                .strip_prefix(base)?
                .to_string_lossy()
                .replace('\\', "/");
            visit(rel, &path);
        }
    }
    Ok(())
}
