//! Cross-repository corpus parity runner (DreamComp #28 foundation).
//!
//! Reads the DreamComp-generated `jsaw_corpus.json` (issue #24), compiles
//! every case through jsaw's WasmGC pipeline, executes the requested export
//! under wasmtime, and compares against the DreamComp expected results. Each
//! case emits one machine-readable JSON line; the final line is a summary.
//!
//! The corpus path defaults to the sibling DreamComp checkout and can be
//! overridden with `JSAW_CORPUS_JSON`. Cases whose result is unavailable in
//! this lane (e.g. non-numeric ABI, or a backend capability nano has not
//! landed yet) are reported with explicit `unavailable` markers, never
//! silently skipped.

use std::collections::BTreeMap;
use std::path::PathBuf;
use std::time::Instant;

use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Corpus {
    schema_version: u32,
    source_fingerprint: String,
    cases: Vec<CorpusCase>,
}

#[derive(Debug, Deserialize)]
struct CorpusCase {
    id: String,
    name: String,
    module: String,
    export: String,
    args: Vec<String>,
    expected: f64,
}

fn corpus_path() -> PathBuf {
    std::env::var("JSAW_CORPUS_JSON").map(PathBuf::from).unwrap_or_else(|_| {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(
            "../../../dreamcomp/crates/helper/dreamcompiler-e2e-tests/tests/corpus/jsaw_corpus.json",
        )
    })
}

/// Execute the module's numeric export under wasmtime.
///
/// wasmtime is built without its `std` feature here, so `wasmtime::Error`
/// does not implement `std::error::Error` and cannot feed `anyhow`'s `?`
/// conversion; every fallible call maps through Display instead.
fn run_export(
    module_bytes: &[u8],
    export: &str,
    args: &[f64],
) -> anyhow::Result<Option<f64>> {
    let mut config = wasmtime::Config::new();
    config.wasm_function_references(true);
    config.wasm_gc(true);
    let engine = wasmtime::Engine::new(&config).map_err(|e| anyhow::anyhow!("engine: {e}"))?;
    let module = wasmtime::Module::new(&engine, module_bytes)
        .map_err(|e| anyhow::anyhow!("module: {e}"))?;
    let mut store = wasmtime::Store::new(&engine, ());
    let linker: wasmtime::Linker<()> = wasmtime::Linker::new(&engine);
    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|e| anyhow::anyhow!("instantiate: {e}"))?;
    let func = instance
        .get_func(&mut store, export)
        .ok_or_else(|| anyhow::anyhow!("export {export:?} not found"))?;
    let params: Vec<wasmtime::Val> = args.iter().map(|a| wasmtime::Val::F64(a.to_bits())).collect();
    let mut results = vec![wasmtime::Val::F64(0)];
    func.call(&mut store, &params, &mut results)
        .map_err(|e| anyhow::anyhow!("call: {e}"))?;
    Ok(match results.first() {
        Some(wasmtime::Val::F64(bits)) => Some(f64::from_bits(*bits)),
        _ => None,
    })
}

#[test]
fn dreamcomp_corpus_runs_identically_through_jsaw_wasmbc() {
    let path = corpus_path();
    let corpus: Corpus = serde_json::from_str(
        &std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read {}: {e}", path.display())),
    )
    .expect("corpus JSON must parse");
    assert_eq!(corpus.schema_version, 1, "unsupported corpus schema");

    let mut pass = 0usize;
    let mut fail = 0usize;
    let mut unsupported = 0usize;
    let mut unavailable = 0usize;
    for case in &corpus.cases {
        let started = Instant::now();
        let report = run_case(case);
        let elapsed_ms = started.elapsed().as_secs_f64() * 1000.0;
        match &report.status[..] {
            "pass" => pass += 1,
            "fail" => fail += 1,
            "unsupported" => unsupported += 1,
            _ => unavailable += 1,
        }
        println!(
            "{}",
            serde_json::json!({
                "id": case.id,
                "name": case.name,
                "lane": "wasmbc",
                "status": report.status,
                "expected": case.expected,
                "actual": report.actual,
                "error": report.error,
                "compile_and_run_ms": elapsed_ms,
                "alloc_count": serde_json::Value::Null,
                "alloc_bytes": serde_json::Value::Null,
                "peak_live_bytes": serde_json::Value::Null,
                "retained_objects": serde_json::Value::Null,
                "metrics_unavailable": ["alloc_count", "alloc_bytes", "peak_live_bytes", "retained_objects"],
            })
        );
    }
    println!(
        "{}",
        serde_json::json!({
            "summary": true,
            "lane": "wasmbc",
            "source_fingerprint": corpus.source_fingerprint,
            "cases": corpus.cases.len(),
            "pass": pass,
            "fail": fail,
            "unsupported": unsupported,
            "unavailable": unavailable,
        })
    );
    assert_eq!(fail, 0, "{fail} corpus case(s) mismatched DreamComp expectations");
    assert!(
        unsupported <= 5,
        "unexpected unsupported cases: {unsupported} (at most the five ~Natives_inlineme families are allowed)"
    );
    assert_eq!(pass + unsupported, corpus.cases.len());
}

struct CaseReport {
    status: String,
    actual: Option<f64>,
    error: Option<String>,
}

fn run_case(case: &CorpusCase) -> CaseReport {
    // Cases exercising DreamComp-native sentinels (`~Natives_*`, e.g. the
    // `inlineme` inlining hint) are outside jsaw's WasmGC lane by design:
    // jsaw has no DreamCompat natives. Report them as explicitly unsupported
    // rather than failing or silently passing.
    if case.module.contains("~Natives_") {
        return CaseReport {
            status: "unsupported".into(),
            actual: None,
            error: Some("requires a DreamComp native sentinel, not available in jsaw".into()),
        };
    }
    match try_case(case) {
        Ok(actual) => {
            if actual == case.expected || (actual.is_nan() && case.expected.is_nan()) {
                CaseReport {
                    status: "pass".into(),
                    actual: Some(actual),
                    error: None,
                }
            } else {
                CaseReport {
                    status: "fail".into(),
                    actual: Some(actual),
                    error: Some(format!(
                        "expected {}, got {actual}",
                        case.expected
                    )),
                }
            }
        }
        Err(error) => CaseReport {
            status: "fail".into(),
            actual: None,
            error: Some(format!("{error:#}")),
        },
    }
}

fn try_case(case: &CorpusCase) -> anyhow::Result<f64> {
    let args: Vec<f64> = case
        .args
        .iter()
        .map(|a| a.trim().parse::<f64>())
        .collect::<Result<_, _>>()
        .map_err(|e| anyhow::anyhow!("invalid arg literal: {e}"))?;

    let mut sources: BTreeMap<String, String> = BTreeMap::new();
    sources.insert("main.js".to_string(), case.module.clone());
    let set = portal_jsc_waffle::module_set_from_sources(
        sources.iter().map(|(k, v)| (k.as_str(), v.as_str())),
    )
    .map_err(|e| anyhow::anyhow!("ingestion failed: {e}"))?;
    let mut module = portal_pc_waffle::Module::empty();
    portal_jsc_waffle::convert_modules(
        "main.js",
        &set,
        &mut module,
        &portal_jsc_waffle::ConvertOptions::default(),
    )
    .map_err(|e| anyhow::anyhow!("lowering failed: {e}"))?;
    let bytes = portal_pc_waffle::to_wasm_bytes(&module)?;
    run_export(&bytes, &case.export, &args)?
        .ok_or_else(|| anyhow::anyhow!("export did not return an f64"))
}
