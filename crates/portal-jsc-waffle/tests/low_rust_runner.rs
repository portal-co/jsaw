//! Local runner for the Rust backend (DreamComp #40): corpus cases × both
//! lanes → stable Rust → `rustc` → run → compare against the corpus
//! expectation (the same rule `corpus_parity` uses for the wasm lanes).
//!
//! rustc costs minutes per artifact (the corpus programs embed the full
//! primordial library), so the default sweep is a diverse subset; the full
//! corpus runs with `JSAW_RUST_RUNNER=full`. Acceptance #3: with
//! `JSAW_RUST_ARTIFACT_REPORT=<path>` set, writes a JSON report of
//! per-case/lane Rust artifact byte sizes.
//!
//! Fails closed (with a prerequisite message) when `rustc` is unavailable —
//! see AGENTS.md's fail-closed test-environment rule.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use serde::Deserialize;

use portal_jsc_waffle::low_emit::exec::{self, LowLane};

#[derive(Debug, Deserialize)]
struct Corpus {
    cases: Vec<CorpusCase>,
}

#[derive(Debug, Deserialize, Clone)]
struct CorpusCase {
    name: String,
    module: String,
    export: String,
    args: Vec<String>,
    expected: f64,
}

/// The default sweep: diverse feature coverage at bounded cost.
const DEFAULT_SUBSET: [&str; 6] = [
    "add",
    "class_inheritance_super",
    "map_basic",
    "weakmap_native_intrinsic",
    "proxy_get_set_has",
    "reflect_trap_mirror",
];

fn corpus_path() -> std::path::PathBuf {
    std::env::var("JSAW_CORPUS_JSON").map(std::path::PathBuf::from).unwrap_or_else(|_| {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../jsaw-wasi-bin/tests/corpus/jsaw_corpus.json")
    })
}

fn plan_for(source: &str) -> portal_jsc_waffle::BackendPlan {
    let mut sources = BTreeMap::new();
    sources.insert("main.js".to_string(), source.to_string());
    let set = portal_jsc_waffle::module_set_from_sources(
        sources.iter().map(|(k, v)| (k.as_str(), v.as_str())),
    )
    .expect("ingestion");
    let mut module = portal_pc_waffle::Module::empty();
    portal_jsc_waffle::convert_modules(
        "main.js",
        &set,
        &mut module,
        &portal_jsc_waffle::ConvertOptions::default(),
    )
    .expect("lowering");
    portal_jsc_waffle::BackendPlan::from_module(&module).expect("plan")
}

struct LaneResult {
    status: &'static str,
    detail: String,
    artifact_bytes: usize,
}

fn run_lane(case: &CorpusCase, lane: LowLane, dir: &Path) -> LaneResult {
    let plan = plan_for(&case.module);
    // Deterministic re-emission (acceptance: same plan → byte-identical).
    let a = exec::emit_low_artifact(&plan, lane).expect("emit");
    let b = exec::emit_low_artifact(&plan, lane).expect("re-emit");
    assert_eq!(a, b, "{} emission is not deterministic", lane.name());
    let args: Vec<f64> = case
        .args
        .iter()
        .map(|x| x.trim().parse().expect("arg literal"))
        .collect();
    let outcome = exec::run_low_case(&plan, lane, &case.export, &args, case.expected, dir);
    LaneResult {
        status: outcome.status,
        detail: outcome.detail,
        artifact_bytes: outcome.artifact_bytes,
    }
}

#[test]
fn rust_backend_corpus_parity() {
    assert!(
        LowLane::RustNano.toolchain_available(),
        "Rust backend runner prerequisite: no rustc on PATH (fail-closed per AGENTS.md)"
    );
    let corpus: Corpus =
        serde_json::from_str(&std::fs::read_to_string(corpus_path()).expect("corpus file"))
            .expect("corpus parses");
    let full = std::env::var("JSAW_RUST_RUNNER").as_deref() == Ok("full");

    let work_root = std::env::temp_dir().join(format!("jsaw_rs_runner_{}", std::process::id()));
    let queue: Mutex<Vec<(CorpusCase, LowLane)>> = Mutex::new(
        corpus
            .cases
            .into_iter()
            .filter(|c| !c.module.contains("~Natives_"))
            .filter(|c| full || DEFAULT_SUBSET.contains(&c.name.as_str()))
            .flat_map(|c| [LowLane::RustNano, LowLane::RustCoregc].into_iter().map(move |l| (c.clone(), l)))
            .collect(),
    );
    let results: Mutex<Vec<(String, LowLane, LaneResult)>> = Mutex::new(Vec::new());
    let workers = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
        .min(8);
    std::thread::scope(|scope| {
        for _ in 0..workers {
            let queue = &queue;
            let results = &results;
            let work_root = &work_root;
            scope.spawn(move || loop {
                let next = queue.lock().unwrap().pop();
                let Some((case, lane)) = next else { break };
                let dir = work_root.join(format!("{}_{}", case.name, lane.name()));
                std::fs::create_dir_all(&dir).expect("workdir");
                let r = run_lane(&case, lane, &dir);
                let _ = std::fs::remove_dir_all(&dir);
                results.lock().unwrap().push((case.name.clone(), lane, r));
            });
        }
    });

    let results = results.into_inner().unwrap();
    let mut pass = 0usize;
    let mut fail = 0usize;
    let mut failures: Vec<String> = Vec::new();
    let mut report_rows: Vec<serde_json::Value> = Vec::new();
    for (name, lane, r) in &results {
        report_rows.push(serde_json::json!({
            "case": name,
            "lane": lane.name(),
            "status": r.status,
            "artifact_bytes": r.artifact_bytes,
        }));
        if r.status == "pass" {
            pass += 1;
        } else {
            fail += 1;
            failures.push(format!("{name} [{}]: {}", lane.name(), r.detail));
        }
    }
    eprintln!(
        "rust-backend corpus{}: {pass} pass, {fail} fail ({} case-lanes)",
        if full { " (full)" } else { " (subset)" },
        results.len()
    );
    if let Ok(path) = std::env::var("JSAW_RUST_ARTIFACT_REPORT") {
        std::fs::write(
            &path,
            serde_json::to_string_pretty(&serde_json::json!({
                "schema": "jsaw.rust-artifact-report.v1",
                "rows": report_rows,
            }))
            .expect("report"),
        )
        .expect("write report");
        eprintln!("wrote {path}");
    }
    let _ = std::fs::remove_dir_all(&work_root);
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
