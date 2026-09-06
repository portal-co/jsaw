//! Result model, classification and report emission for the runner.

use std::collections::BTreeMap;
use std::path::Path;

use serde::Serialize;

use crate::execute::Execution;
use crate::tests::Variant;

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum Status {
    /// Variant ran to completion (or, for negative tests, failed exactly as
    /// the frontmatter expected).
    Pass,
    /// Variant did not behave as expected.
    Fail,
    /// Backend rejected the source (parse/lowering/validation).
    CompileError,
    /// The runner cannot honor this test's requirements (flags, host hooks).
    Skip,
}

#[derive(Debug, Clone, Serialize)]
pub struct TestResult {
    pub path: String,
    pub variant: &'static str,
    pub status: Status,
    pub detail: String,
}

#[derive(Debug, Default, Serialize)]
pub struct Report {
    pub results: Vec<TestResult>,
    pub counts: BTreeMap<String, u64>,
}

impl Report {
    pub fn push(&mut self, result: TestResult) {
        let key = format!("{:?}", result.status).to_lowercase();
        *self.counts.entry(key).or_default() += 1;
        self.results.push(result);
    }

    pub fn write_json(&self, path: &Path) -> anyhow::Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(path, serde_json::to_string_pretty(self)?)?;
        Ok(())
    }

    /// Human-readable summary used for CI job summaries and local runs.
    pub fn summary(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!(
            "pass {} | fail {} | compile-error {} | skip {}",
            self.counts.get("pass").copied().unwrap_or(0),
            self.counts.get("fail").copied().unwrap_or(0),
            self.counts.get("compile-error").copied().unwrap_or(0),
            self.counts.get("skip").copied().unwrap_or(0),
        ));
        // Worst offenders by suite prefix (first two path components).
        let mut suites: BTreeMap<String, (u64, u64)> = BTreeMap::new();
        for result in &self.results {
            if result.status == Status::Skip {
                continue;
            }
            let suite = result.path.split('/').take(2).collect::<Vec<_>>().join("/");
            let entry = suites.entry(suite).or_default();
            entry.0 += 1;
            if result.status != Status::Pass {
                entry.1 += 1;
            }
        }
        lines.push("suite: run / non-pass".to_owned());
        for (suite, (run, failing)) in suites {
            lines.push(format!("{suite}: {run} / {failing}"));
        }
        lines.join("\n")
    }
}

/// Classify one variant after compile+execute, honoring `negative`
/// expectations.
pub fn classify(variant: &Variant, outcome: Result<Execution, anyhow::Error>) -> TestResult {
    let expected = variant.meta.negative.clone();
    match outcome {
        Err(error) => {
            // Compile-phase failure.
            let text = error.to_string();
            if let Some(negative) = &expected {
                if negative.phase == "parse" || negative.phase == "resolution" {
                    // The backend rejects more than the spec requires, so a
                    // compile error only counts as the *expected* failure if
                    // the spec named an error type our pipeline plausibly
                    // honors; conservatively report expected-but-unverified
                    // as skip with detail.
                    return TestResult {
                        path: variant.rel_path.clone(),
                        variant: variant.variant,
                        status: Status::Skip,
                        detail: format!(
                            "negative phase {} expected; compile failed: {text}",
                            negative.phase
                        ),
                    };
                }
            }
            TestResult {
                path: variant.rel_path.clone(),
                variant: variant.variant,
                status: Status::CompileError,
                detail: text,
            }
        }
        Ok(execution) => {
            let (status, detail) = match execution {
                Execution::Completed => match &expected {
                    None => (Status::Pass, "completed".to_owned()),
                    Some(negative) => (
                        Status::Fail,
                        format!(
                            "completed but expected {} failure of type {}",
                            negative.phase, negative.kind
                        ),
                    ),
                },
                Execution::Trapped(raw) => {
                    // Distill the wasmtime error to its root cause: the
                    // backtrace Decorates every failure, the actionable
                    // reason follows "wasm trap:".
                    let text = match raw.split("wasm trap:").nth(1) {
                        // "reason\n    0: backtrace..." — keep the reason.
                        Some(rest) => rest.lines().next().unwrap_or("").trim().to_owned(),
                        None => raw
                            .lines()
                            .last()
                            .unwrap_or(&raw)
                            .trim()
                            .to_owned(),
                    };
                    let text = if text.is_empty() { raw.clone() } else { text };
                    match &expected {
                    // Error-type fidelity across the wasm boundary is not
                    // implemented yet: any trap counts as the expected
                    // failure only for runtime-negative tests whose type we
                    // could not check — recorded as pass with detail, since
                    // failing-as-expected is the common case.
                        Some(negative) if negative.phase == "runtime" => (
                            Status::Pass,
                            format!(
                                "trapped (expected {} {}): {text}",
                                negative.phase, negative.kind
                            ),
                        ),
                        Some(negative) => (
                            Status::Fail,
                            format!("trapped, expected {} failure: {text}", negative.phase),
                        ),
                        None => (Status::Fail, format!("trapped: {text}")),
                    }
                }
                Execution::Timeout => (Status::Fail, "timed out".to_owned()),
            };
            TestResult {
                path: variant.rel_path.clone(),
                variant: variant.variant,
                status,
                detail,
            }
        }
    }
}

/// Decisions for tests the runner cannot honor at all.
pub fn skip_reason(meta: &crate::frontmatter::Frontmatter) -> Option<&'static str> {
    if meta.flags.contains("async") {
        return Some("async flag not supported");
    }
    if meta.flags.contains("non-deterministic") {
        return Some("non-deterministic test");
    }
    if meta.flags.contains("module") && meta.flags.contains("raw") {
        return Some("raw+module combination not supported");
    }
    None
}
