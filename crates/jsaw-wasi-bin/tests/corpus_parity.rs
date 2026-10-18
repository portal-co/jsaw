//! Cross-repository corpus parity runner (DreamComp #28).
//!
//! Runs every case of the DreamComp-generated corpus (issue #24) through both
//! jsaw lanes — the WasmGC path (`wasmbc`) and the nano WASM-MVP emitter
//! (`nano_mvp`) — executes the requested export under wasmtime, and compares
//! against the DreamComp expected results. Each case emits one
//! machine-readable JSON line; the final line is a summary. When
//! `JSAW_PARITY_REPORT` names a path, the full report (schema
//! `jsaw.corpus-parity.v1`) is also written there for CI publication.
//!
//! The corpus is vendored at `tests/corpus/jsaw_corpus.json`; a regenerated
//! DreamComp-side artifact can be supplied via `JSAW_CORPUS_JSON`, but its
//! source fingerprint must match `EXPECTED_SOURCE_FINGERPRINT`, so a stale or
//! silently-mutated corpus fails the gate.
//!
//! Metrics: the nano lane reads the allocator counters the emitter maintains
//! under `NanoMvpConfig::instrument_alloc` (`alloc_count`, `alloc_bytes`,
//! `peak_live_bytes` = bump-cursor high water, `retained_objects` — degenerate
//! for a no-free bump arena, still real). The wasmbc lane runs inside
//! wasmtime's GC, which exposes no allocator counters: all four metrics are
//! reported as explicit `unavailable` markers there.
//!
//! Known DreamComp baseline failures (`DREAMCOMP_BASELINE_FAILURES`) stay
//! visible in the summary; no test is deleted or weakened to make parity
//! green.

use std::collections::BTreeMap;
use std::path::PathBuf;
use std::time::Instant;

use serde::Deserialize;

/// The corpus schema the runner understands.
const EXPECTED_SCHEMA_VERSION: u32 = 1;
/// Stale-corpus tripwire: the source fingerprint of the vendored corpus.
/// When the corpus is regenerated in DreamComp, update the vendored file and
/// this pin in the same commit.
const EXPECTED_SOURCE_FINGERPRINT: &str = "source-59c5e67c95b95773";
/// DreamComp baseline failures recorded at M0 (docs/jsaw-baselines.md in the
/// DreamComp repo). They stay visible in the report; they are not jsaw
/// regressions and they are not silently converted into passes.
const DREAMCOMP_BASELINE_FAILURES: [&str; 3] = [
    "rpc_recv_cps_placeholder_without_dispatch",
    "rpc_recv_fast_ir_tag_emitted",
    "rpc_recv_placeholder_compiles_and_returns_undefined",
];
/// The lanes every case runs through.
const LANES: [&str; 2] = ["wasmbc", "nano_mvp"];

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
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/corpus/jsaw_corpus.json")
    })
}

/// Process peak RSS in bytes (0 where unsupported).
fn peak_rss_bytes() -> u64 {
    #[cfg(unix)]
    {
        let mut usage: libc::rusage = unsafe { std::mem::zeroed() };
        if unsafe { libc::getrusage(libc::RUSAGE_SELF, &mut usage) } == 0 {
            // ru_maxrss: bytes on Linux, KiB on macOS.
            let raw = usage.ru_maxrss as u64;
            return if cfg!(target_os = "macos") { raw } else { raw * 1024 };
        }
    }
    0
}

fn alloc_metric(g: Option<wasmtime::Global>, store: &mut wasmtime::Store<()>) -> Option<u64> {
    match g?.get(store) {
        wasmtime::Val::I64(v) => u64::try_from(v).ok(),
        wasmtime::Val::I32(v) => u64::try_from(v).ok(),
        _ => None,
    }
}

/// Allocator metrics from an instrumented nano instance: (alloc_count,
/// alloc_bytes, peak_live_bytes, retained_objects).
fn read_alloc_metrics(
    instance: &wasmtime::Instance,
    store: &mut wasmtime::Store<()>,
) -> Option<(u64, u64, u64, u64)> {
    let count = alloc_metric(instance.get_global(&mut *store, "js_nano_alloc_count"), store)?;
    let bytes = alloc_metric(instance.get_global(&mut *store, "js_nano_alloc_bytes"), store)?;
    let cursor = alloc_metric(instance.get_global(&mut *store, "js_nano_arena_cursor"), store)?;
    let peak_live = cursor.saturating_sub(16);
    // The bump arena never frees, so every allocation is retained.
    Some((count, bytes, peak_live, count))
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
    lane: &str,
) -> anyhow::Result<(Option<f64>, Option<(u64, u64, u64, u64)>)> {
    let mut config = wasmtime::Config::new();
    if lane == "wasmbc" {
        config.wasm_function_references(true);
        config.wasm_gc(true);
    }
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
    let value = match results.first() {
        Some(wasmtime::Val::F64(bits)) => Some(f64::from_bits(*bits)),
        _ => None,
    };
    let metrics = read_alloc_metrics(&instance, &mut store);
    Ok((value, metrics))
}

struct CaseReport {
    status: String,
    actual: Option<f64>,
    error: Option<String>,
    /// (alloc_count, alloc_bytes, peak_live_bytes, retained_objects)
    metrics: Option<(u64, u64, u64, u64)>,
}

fn semantic_mode(case: &CorpusCase) -> &'static str {
    match case.name.as_str() {
        "weakmap_emulated" => "emulated-strong",
        name if name.starts_with("weakmap_native_intrinsic") => "native-weak-key-slot",
        _ => "ordinary",
    }
}

fn run_case(
    case: &CorpusCase,
    lane: &str,
    plan_a: Option<&portal_jsc_waffle::BackendPlan>,
) -> CaseReport {
    // Cases exercising DreamComp-native sentinels (`~Natives_*`, e.g. the
    // `inlineme` inlining hint) are outside jsaw by design: jsaw has no
    // DreamCompat natives. Report them as explicitly unsupported rather than
    // failing or silently passing.
    if case.module.contains("~Natives_") {
        return CaseReport {
            status: "unsupported".into(),
            actual: None,
            error: Some("requires a DreamComp native sentinel, not available in jsaw".into()),
            metrics: None,
        };
    }
    match try_case(case, lane, plan_a) {
        Ok((actual, metrics)) => {
            if actual == case.expected || (actual.is_nan() && case.expected.is_nan()) {
                CaseReport {
                    status: "pass".into(),
                    actual: Some(actual),
                    error: None,
                    metrics,
                }
            } else {
                CaseReport {
                    status: "fail".into(),
                    actual: Some(actual),
                    error: Some(format!("expected {}, got {actual}", case.expected)),
                    metrics,
                }
            }
        }
        Err(error) => CaseReport {
            status: "fail".into(),
            actual: None,
            error: Some(format!("{error:#}")),
            metrics: None,
        },
    }
}

fn try_case(
    case: &CorpusCase,
    lane: &str,
    plan_a: Option<&portal_jsc_waffle::BackendPlan>,
) -> anyhow::Result<(f64, Option<(u64, u64, u64, u64)>)> {
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
    match lane {
        "wasmbc" => {
            let bytes = portal_pc_waffle::to_wasm_bytes(&module)?;
            let (value, _) = run_export(&bytes, &case.export, &args, lane)?;
            value
                .map(|v| (v, None))
                .ok_or_else(|| anyhow::anyhow!("export did not return an f64"))
        }
        "nano_mvp" => {
            let plan = match plan_a {
                Some(p) => p,
                None => {
                    anyhow::bail!("nano_mvp lane requires a precomputed BackendPlan")
                }
            };
            let bytes = portal_jsc_waffle::emit_nano_mvp_with_config(
                plan,
                &portal_jsc_waffle::NanoMvpConfig {
                    instrument_alloc: true,
                    max_pages: None,
                },
            )
            .map_err(|d| anyhow::anyhow!("nano MVP diagnostics: {d:?}"))?;
            let (value, metrics) = run_export(&bytes, &case.export, &args, lane)?;
            value
                .map(|v| (v, metrics))
                .ok_or_else(|| anyhow::anyhow!("export did not return an f64"))
        }
        other => anyhow::bail!("unknown lane {other:?}"),
    }
}

#[test]
fn dreamcomp_corpus_runs_identically_through_jsaw_wasmbc() {
    let path = corpus_path();
    let corpus: Corpus = serde_json::from_str(
        &std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read {}: {e}", path.display())),
    )
    .expect("corpus JSON must parse");
    assert_eq!(
        corpus.schema_version, EXPECTED_SCHEMA_VERSION,
        "unsupported corpus schema"
    );
    // Stale-corpus gate: the artifact under test must be the pinned
    // regeneration. Bump the vendored file and the pin together.
    assert_eq!(
        corpus.source_fingerprint, EXPECTED_SOURCE_FINGERPRINT,
        "stale corpus: expected {EXPECTED_SOURCE_FINGERPRINT}, got {}",
        corpus.source_fingerprint
    );

    let mut case_outcomes: Vec<serde_json::Value> = Vec::new();
    let mut pass = 0usize;
    let mut fail = 0usize;
    let mut unsupported = 0usize;
    let mut unavailable = 0usize;
    for case in &corpus.cases {
        // Deterministic-plan gate: plan construction must be fingerprint-
        // stable within the process.
        let plan_a = if case.module.contains("~Natives_") {
            None
        } else {
            let mut sources: BTreeMap<String, String> = BTreeMap::new();
            sources.insert("main.js".to_string(), case.module.clone());
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
            let plan = portal_jsc_waffle::BackendPlan::from_module(&module)
                .expect("plan diagnostics");
            let plan_b = portal_jsc_waffle::BackendPlan::from_module(&module)
                .expect("plan diagnostics (repeat)");
            assert_eq!(
                plan.fingerprint(),
                plan_b.fingerprint(),
                "nondeterministic plan fingerprint for {}",
                case.name
            );
            Some(plan)
        };
        for lane in LANES {
            let started = Instant::now();
            let report = run_case(case, lane, plan_a.as_ref());
            let elapsed_ms = started.elapsed().as_secs_f64() * 1000.0;
            match &report.status[..] {
                "pass" => pass += 1,
                "fail" => fail += 1,
                "unsupported" => unsupported += 1,
                _ => unavailable += 1,
            }
            let outcome = serde_json::json!({
                "id": case.id,
                "name": case.name,
                "lane": lane,
                "semantic_mode": semantic_mode(case),
                "status": report.status,
                "expected": case.expected,
                "actual": report.actual,
                "error": report.error,
                "compile_and_run_ms": elapsed_ms,
                "plan_fingerprint": plan_a.as_ref().map(|p| p.fingerprint()),
                "alloc_count": report.metrics.map(|m| m.0),
                "alloc_bytes": report.metrics.map(|m| m.1),
                "peak_live_bytes": report.metrics.map(|m| m.2),
                "retained_objects": report.metrics.map(|m| m.3),
                "metrics_unavailable": if report.metrics.is_some() {
                    serde_json::json!([])
                } else {
                    serde_json::json!(["alloc_count", "alloc_bytes", "peak_live_bytes", "retained_objects"])
                },
            });
            println!("{outcome}");
            case_outcomes.push(outcome);
        }
    }
    let summary = serde_json::json!({
        "summary": true,
        "lanes": LANES,
        "source_fingerprint": corpus.source_fingerprint,
        "case_results": corpus.cases.len() * LANES.len(),
        "pass": pass,
        "fail": fail,
        "unsupported": unsupported,
        "unavailable": unavailable,
        "dreamcomp_baseline_failures": DREAMCOMP_BASELINE_FAILURES,
        "process_peak_rss_bytes": peak_rss_bytes(),
    });
    println!("{summary}");

    if let Ok(report_path) = std::env::var("JSAW_PARITY_REPORT") {
        let report = serde_json::json!({
            "schema": "jsaw.corpus-parity.v1",
            "corpus": {
                "source_fingerprint": corpus.source_fingerprint,
                "schema_version": corpus.schema_version,
            },
            "summary": summary,
            "cases": case_outcomes,
        });
        std::fs::write(&report_path, serde_json::to_string_pretty(&report).unwrap())
            .unwrap_or_else(|e| panic!("could not write {report_path}: {e}"));
    }

    assert_eq!(fail, 0, "{fail} corpus case(s) mismatched DreamComp expectations");
    // Five ~Natives_inlineme families per lane (DreamCompat sentinels are
    // outside jsaw), nothing else may be unsupported or unavailable.
    assert!(
        unsupported <= 5 * LANES.len(),
        "unexpected unsupported cases: {unsupported} (at most the five ~Natives_inlineme families per lane are allowed)"
    );
    assert_eq!(unavailable, 0);
    assert_eq!(pass + unsupported, corpus.cases.len() * LANES.len());
}
