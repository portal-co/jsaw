//! Local runner for the C backend (DreamComp #38): every non-`~Natives_`
//! corpus case × both lanes → C99 → `$CC` → run → compare against the
//! corpus expectation (the same rule `corpus_parity` uses for the wasm
//! lanes). Proves acceptance #1 (compiles clean C99) and #2 (semantic
//! parity) without waiting on the #41 parity-harness integration.
//!
//! Acceptance #3: with `JSAW_C_ARTIFACT_REPORT=<path>` set, writes a JSON
//! report of per-case/lane C artifact byte sizes.
//!
//! Fails closed (with a prerequisite message) when no C compiler is
//! available — see AGENTS.md's fail-closed test-environment rule.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use serde::Deserialize;

use portal_jsc_waffle::low_emit::{self, exec::{self, LowLane}};

#[derive(Debug, Deserialize)]
struct Corpus {
    cases: Vec<CorpusCase>,
}

#[derive(Debug, Deserialize)]
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
    // Deterministic re-emission (acceptance: same plan → byte-identical C).
    let a = exec::emit_low_artifact(&plan, lane).expect("emit");
    let b = exec::emit_low_artifact(&plan, lane).expect("re-emit");
    assert_eq!(a, b, "{} emission is not deterministic", lane.name());
    // Structural IR validation and the numeric-ABI export shape.
    let low = match lane {
        LowLane::CNano => low_emit::emit_low(&plan, &low_emit::NanoRepresentation),
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
fn c_backend_corpus_parity() {
    assert!(
        LowLane::CNano.toolchain_available(),
        "C backend runner prerequisite: no C compiler on PATH; \
         set $CC to one (fail-closed per AGENTS.md)"
    );
    let corpus: Corpus = serde_json::from_str(
        &std::fs::read_to_string(corpus_path()).expect("corpus file"),
    )
    .expect("corpus parses");

    let work = std::env::temp_dir().join(format!("jsaw_c_runner_{}", std::process::id()));
    let mut pass = 0usize;
    let mut fail = 0usize;
    let mut unsupported = 0usize;
    let mut failures: Vec<String> = Vec::new();
    let mut report_rows: Vec<serde_json::Value> = Vec::new();
    for case in &corpus.cases {
        if case.module.contains("~Natives_") {
            continue;
        }
        for lane in [LowLane::CNano, LowLane::CCoregc] {
            let dir = work.join(format!("{}_{}", case.name, lane.name()));
            std::fs::create_dir_all(&dir).expect("workdir");
            let r = run_lane(case, lane, &dir);
            report_rows.push(serde_json::json!({
                "case": case.name,
                "lane": lane.name(),
                "status": r.status,
                "artifact_bytes": r.artifact_bytes,
            }));
            match r.status {
                "pass" => pass += 1,
                "unsupported" => unsupported += 1,
                _ => {
                    fail += 1;
                    failures.push(format!("{} [{}]: {}", case.name, lane.name(), r.detail));
                }
            }
            let _ = std::fs::remove_dir_all(&dir);
        }
    }
    eprintln!("c-backend corpus: {pass} pass, {fail} fail, {unsupported} unsupported");
    if let Ok(path) = std::env::var("JSAW_C_ARTIFACT_REPORT") {
        std::fs::write(
            &path,
            serde_json::to_string_pretty(&serde_json::json!({
                "schema": "jsaw.c-artifact-report.v1",
                "rows": report_rows,
            }))
            .expect("report"),
        )
        .expect("write report");
        eprintln!("wrote {path}");
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    let _ = std::fs::remove_dir_all(&work);
}
