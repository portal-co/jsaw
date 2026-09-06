//! test262 compatibility runner for portal-jsc-waffle.
//!
//! Compiles test262 tests through the swc → CFG → TAC → SSA → WasmGC
//! pipeline and executes them under wasmtime, classifying each result and
//! emitting a JSON report. See docs/plan-test262-compatibility.md.

mod execute;
mod frontmatter;
mod report;
mod tests;

use std::path::{Path, PathBuf};
use std::time::Duration;

use anyhow::{Context, bail};
use report::{Report, Status, TestResult};

#[derive(Debug, Default)]
struct Args {
    test262_root: PathBuf,
    suites: Vec<String>,
    limit: usize,
    timeout_ms: u64,
    report: PathBuf,
    filters: Vec<String>,
}

fn parse_args() -> anyhow::Result<Args> {
    let mut args = Args {
        limit: 0,
        timeout_ms: 1000,
        report: PathBuf::from("test262-report.json"),
        ..Default::default()
    };
    let mut argv = std::env::args().skip(1);
    while let Some(flag) = argv.next() {
        let mut value = |name: &str| -> anyhow::Result<String> {
            argv.next().ok_or_else(|| anyhow::anyhow!("--{name} requires a value"))
        };
        match flag.as_str() {
            "--test262-root" => args.test262_root = PathBuf::from(value("test262-root")?),
            "--suite" => args.suites.push(value("suite")?),
            "--limit" => args.limit = value("limit")?.parse()?,
            "--timeout-ms" => args.timeout_ms = value("timeout-ms")?.parse()?,
            "--report" => args.report = PathBuf::from(value("report")?),
            "--filter" => args.filters.push(value("filter")?),
            other => bail!("unknown argument {other}; expected --test262-root <dir> [--suite <prefix>] [--filter <substr>] [--limit N] [--timeout-ms N] [--report <file>]"),
        }
    }
    if args.test262_root.as_os_str().is_empty() {
        bail!("--test262-root <dir> is required");
    }
    Ok(args)
}

fn main() -> anyhow::Result<()> {
    let args = parse_args()?;
    let harness = execute::harness_dir(&args.test262_root);
    let all = tests::collect_tests(&args.test262_root, &args.suites)
        .context("collecting test files")?;
    let filtered: Vec<_> = all
        .into_iter()
        .filter(|path| {
            args.filters.is_empty()
                || args
                    .filters
                    .iter()
                    .any(|f| path.to_string_lossy().contains(f.as_str()))
        })
        .take(if args.limit == 0 {
            usize::MAX
        } else {
            args.limit
        })
        .collect();
    if filtered.is_empty() {
        bail!("no test files matched the given suites/filters");
    }

    let timeout = Duration::from_millis(args.timeout_ms);
    let mut report = Report::default();
    let total = filtered.len();
    for (index, path) in filtered.iter().enumerate() {
        let rel = path
            .strip_prefix(args.test262_root.join("test"))
            .unwrap_or(path);
        run_one(&harness, rel, path, timeout, &mut report);
        if (index + 1) % 50 == 0 || index + 1 == total {
            eprintln!("[{}/{}]", index + 1, total);
        }
    }

    eprintln!("{}", report.summary());
    report.write_json(&args.report)?;
    eprintln!("report written to {}", args.report.display());
    Ok(())
}

fn run_one(
    harness: &Path,
    rel: &Path,
    path: &Path,
    timeout: Duration,
    report: &mut Report,
) {
    let source = match std::fs::read_to_string(path) {
        Ok(source) => source,
        Err(error) => {
            report.push(TestResult {
                path: rel.to_string_lossy().into_owned(),
                variant: "default",
                status: Status::Skip,
                detail: format!("unreadable: {error}"),
            });
            return;
        }
    };
    let (body, meta) = match frontmatter::strip(&source) {
        Ok(pair) => pair,
        Err(error) => {
            report.push(TestResult {
                path: rel.to_string_lossy().into_owned(),
                variant: "default",
                status: Status::Skip,
                detail: format!("frontmatter: {error}"),
            });
            return;
        }
    };
    if let Some(reason) = report::skip_reason(&meta) {
        report.push(TestResult {
            path: rel.to_string_lossy().into_owned(),
            variant: "default",
            status: Status::Skip,
            detail: reason.to_owned(),
        });
        return;
    }
    let variants = match tests::expand(rel, &body, meta, harness) {
        Ok(variants) => variants,
        Err(error) => {
            report.push(TestResult {
                path: rel.to_string_lossy().into_owned(),
                variant: "default",
                status: Status::Skip,
                detail: format!("variant expansion: {error}"),
            });
            return;
        }
    };
    for variant in variants {
        let outcome = execute::compile(&variant.source).and_then(|bytes| {
            execute::execute(&bytes, timeout).map_err(|error| anyhow::anyhow!(error))
        });
        report.push(report::classify(&variant, outcome));
    }
}
