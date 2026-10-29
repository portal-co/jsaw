//! Local runner for the MiniBC backend (DreamComp #39): every
//! non-`~Natives_` corpus case × both lanes → MiniBC bytecode → the stock
//! `MiniInterp` (via `dreamcompiler-minibc`'s `interp` feature) → compare
//! against the corpus expectation, mirroring `low_c_runner.rs`. Also
//! records per-case/lane artifact bytes with
//! `JSAW_MINIBC_ARTIFACT_REPORT=<path>` (schema
//! `jsaw.minibc-artifact-report.v1`).
//!
//! No external toolchain is needed (the interpreter is in-process), so this
//! runner never reports `unavailable`.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use serde::Deserialize;

use portal_jsc_waffle::low_emit::{self, exec::{self, LowLane}};

#[derive(Debug, Deserialize)]
struct Corpus {
    cases: Vec<CorpusCase>,
}

#[derive(Clone, Debug, Deserialize)]
struct CorpusCase {
    name: String,
    module: String,
    export: String,
    args: Vec<String>,
    expected: f64,
}

fn corpus_path() -> PathBuf {
    std::env::var("JSAW_CORPUS_JSON").map(PathBuf::from).unwrap_or_else(|_| {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../jsaw-wasi-bin/tests/corpus/jsaw_corpus.json")
    })
}


/// jsaw-local corpus extension (merged after the vendored DreamComp corpus).
fn ext_cases() -> Vec<CorpusCase> {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../jsaw-wasi-bin/tests/corpus/jsaw_corpus_ext.json");
    #[derive(Deserialize)]
    struct Ext {
        cases: Vec<CorpusCase>,
    }
    serde_json::from_str::<Ext>(&std::fs::read_to_string(path).expect("ext corpus"))
        .expect("ext corpus parses")
        .cases
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

fn emit_bytes(plan: &portal_jsc_waffle::BackendPlan, lane: LowLane) -> Vec<u8> {
    let (_, artifact) = match lane {
        LowLane::MinibcNano => {
            low_emit::emit_minibc(plan, &low_emit::NanoRepresentation).expect("emit")
        }
        LowLane::MinibcCoregc => {
            low_emit::emit_minibc(plan, &low_emit::CoreGcRepresentation).expect("emit")
        }
        _ => unreachable!(),
    };
    artifact.bytes
}

fn run_lane(case: &CorpusCase, lane: LowLane, dir: &Path) -> LaneResult {
    let plan = plan_for(&case.module);
    // Deterministic re-emission (same plan → byte-identical artifact).
    let a = emit_bytes(&plan, lane);
    let b = emit_bytes(&plan, lane);
    assert_eq!(a, b, "{} emission is not deterministic", lane.name());
    // Structural IR validation and the numeric-ABI export shape.
    let low = match lane {
        LowLane::MinibcNano => low_emit::emit_low(&plan, &low_emit::NanoRepresentation),
        _ => low_emit::emit_low(&plan, &low_emit::CoreGcRepresentation),
    }
    .expect("re-emit IR");
    assert!(low_emit::debug::validate(&low).is_empty(), "IR validation failed");
    let Some(export) = low.exports.iter().find(|e| e.name == case.export) else {
        return LaneResult {
            status: "fail",
            detail: format!("export {:?} not in the low-level module", case.export),
            artifact_bytes: a.len(),
        };
    };
    let ef = &low.funcs[export.func.0 as usize];
    let numeric = ef.results.len() == 1
        && ef.params.iter().all(|(_, t)| *t == low_emit::LeType::F64)
        && ef.results[0] == low_emit::LeType::F64;
    if !numeric {
        return LaneResult {
            status: "unsupported",
            detail: "export is not in the numeric f64 ABI".into(),
            artifact_bytes: a.len(),
        };
    }
    if ef.params.len() != case.args.len() {
        return LaneResult {
            status: "fail",
            detail: format!(
                "export arity {} != case args {}",
                ef.params.len(),
                case.args.len()
            ),
            artifact_bytes: a.len(),
        };
    }
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
fn minibc_backend_corpus_parity() {
    let corpus: Corpus = serde_json::from_str(
        &std::fs::read_to_string(corpus_path()).expect("corpus file"),
    )
    .expect("corpus parses");

    let work_root = std::env::temp_dir().join(format!("jsaw_minibc_runner_{}", std::process::id()));
    let mut all_cases = corpus.cases;
    all_cases.extend(ext_cases());
    let queue: Mutex<Vec<(CorpusCase, LowLane)>> = Mutex::new(
        all_cases
            .into_iter()
            .filter(|c| !c.module.contains("~Natives_"))
            .flat_map(|c| {
                [LowLane::MinibcNano, LowLane::MinibcCoregc]
                    .into_iter()
                    .filter(|l| {
                        std::env::var("JSLANE")
                            .map(|f| l.name() == f)
                            .unwrap_or(true)
                    })
                    .map(move |l| (c.clone(), l))
            })
            .collect(),
    );
    let results: Mutex<Vec<(String, LowLane, LaneResult)>> = Mutex::new(Vec::new());
    let workers = std::env::var("JSAW_WORKERS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| {
            std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(4)
        })
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
    let mut unsupported = 0usize;
    let mut failures: Vec<String> = Vec::new();
    let mut report_rows: Vec<serde_json::Value> = Vec::new();
    for (name, lane, r) in &results {
        report_rows.push(serde_json::json!({
            "case": name,
            "lane": lane.name(),
            "status": r.status,
            "artifact_bytes": r.artifact_bytes,
        }));
        match r.status {
            "pass" => pass += 1,
            "unsupported" => unsupported += 1,
            _ => {
                fail += 1;
                failures.push(format!("{} [{}]: {}", name, lane.name(), r.detail));
            }
        }
    }
    eprintln!("minibc-backend corpus: {pass} pass, {fail} fail, {unsupported} unsupported");
    if let Ok(path) = std::env::var("JSAW_MINIBC_ARTIFACT_REPORT") {
        std::fs::write(
            &path,
            serde_json::to_string_pretty(&serde_json::json!({
                "schema": "jsaw.minibc-artifact-report.v1",
                "rows": report_rows,
            }))
            .expect("report"),
        )
        .expect("write report");
        eprintln!("wrote {path}");
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    let _ = std::fs::remove_dir_all(&work_root);
}
