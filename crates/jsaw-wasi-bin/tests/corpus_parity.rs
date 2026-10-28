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
/// The wasm lanes every case runs through (unchanged, acceptance #4).
const LANES: [&str; 2] = ["wasmbc", "nano_mvp"];
/// The low-level lanes (DreamComp #41): C and Rust backends × both
/// representations, compiled per case (per-case isolation replaces batch
/// bisection — see the issue comment) and executed in parallel.
const LOW_LANES: [portal_jsc_waffle::low_emit::exec::LowLane; 4] =
    portal_jsc_waffle::low_emit::exec::LowLane::ALL;

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
    /// Emitted artifact size in bytes (the output-size gate metric, #20).
    artifact_bytes: Option<u64>,
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
            artifact_bytes: None,
        };
    }
    match try_case(case, lane, plan_a) {
        Ok((actual, artifact_bytes, metrics)) => {
            if actual == case.expected || (actual.is_nan() && case.expected.is_nan()) {
                CaseReport {
                    status: "pass".into(),
                    actual: Some(actual),
                    error: None,
                    metrics,
                    artifact_bytes: Some(artifact_bytes),
                }
            } else {
                CaseReport {
                    status: "fail".into(),
                    actual: Some(actual),
                    error: Some(format!("expected {}, got {actual}", case.expected)),
                    metrics,
                    artifact_bytes: Some(artifact_bytes),
                }
            }
        }
        Err(error) => CaseReport {
            status: "fail".into(),
            actual: None,
            error: Some(format!("{error:#}")),
            metrics: None,
            artifact_bytes: None,
        },
    }
}

fn try_case(
    case: &CorpusCase,
    lane: &str,
    plan_a: Option<&portal_jsc_waffle::BackendPlan>,
) -> anyhow::Result<(f64, u64, Option<(u64, u64, u64, u64)>)> {
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
            let byte_len = bytes.len() as u64;
            let (value, _) = run_export(&bytes, &case.export, &args, lane)?;
            value
                .map(|v| (v, byte_len, None))
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
            let byte_len = bytes.len() as u64;
            let (value, metrics) = run_export(&bytes, &case.export, &args, lane)?;
            value
                .map(|v| (v, byte_len, metrics))
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
                "artifact_bytes": report.artifact_bytes,
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
    // ---- Phase 2: the low-level lanes (parallel, per-case isolation).
    // Toolchain-missing lanes report `unavailable` per case (fail-closed).
    let lane_filter: Option<Vec<String>> = std::env::var("JSAW_PARITY_LANES")
        .ok()
        .map(|v| v.split(',').map(|s| s.trim().to_string()).collect());
    let low_enabled = |lane: &str| {
        lane_filter
            .as_ref()
            .map(|f| f.iter().any(|x| x == lane))
            .unwrap_or(true)
    };
    let low_queue: std::sync::Mutex<Vec<(usize, portal_jsc_waffle::low_emit::exec::LowLane)>> =
        std::sync::Mutex::new(
            corpus
                .cases
                .iter()
                .enumerate()
                .filter(|(_, c)| !c.module.contains("~Natives_"))
                .flat_map(|(i, _)| {
                    LOW_LANES
                        .iter()
                        .copied()
                        .filter(move |l| low_enabled(l.name()))
                        .map(move |l| (i, l))
                })
                .collect(),
        );
    let low_results: std::sync::Mutex<Vec<serde_json::Value>> = std::sync::Mutex::new(Vec::new());
    let low_counts: std::sync::Mutex<(usize, usize, usize, usize)> =
        std::sync::Mutex::new((0, 0, 0, 0));
    let workers = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
        .min(8);
    let work_root = std::env::temp_dir().join(format!("jsaw_parity_low_{}", std::process::id()));
    std::thread::scope(|scope| {
        for _ in 0..workers {
            let low_queue = &low_queue;
            let low_results = &low_results;
            let low_counts = &low_counts;
            let corpus = &corpus;
            let work_root = &work_root;
            scope.spawn(move || loop {
                let next = low_queue.lock().unwrap().pop();
                let Some((case_idx, lane)) = next else { break };
                let case = &corpus.cases[case_idx];
                let started = Instant::now();
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
                let args: Vec<f64> = case
                    .args
                    .iter()
                    .map(|a| a.trim().parse::<f64>())
                    .collect::<Result<_, _>>()
                    .expect("arg literal");
                let dir = work_root.join(format!("{}_{}", case.name, lane.name()));
                std::fs::create_dir_all(&dir).expect("workdir");
                let outcome = portal_jsc_waffle::low_emit::exec::run_low_case(
                    &plan,
                    lane,
                    &case.export,
                    &args,
                    case.expected,
                    &dir,
                );
                let _ = std::fs::remove_dir_all(&dir);
                let elapsed_ms = started.elapsed().as_secs_f64() * 1000.0;
                {
                    let mut c = low_counts.lock().unwrap();
                    match outcome.status {
                        "pass" => c.0 += 1,
                        "fail" => c.1 += 1,
                        "unsupported" => c.2 += 1,
                        _ => c.3 += 1,
                    }
                }
                low_results.lock().unwrap().push(serde_json::json!({
                    "id": case.id,
                    "name": case.name,
                    "lane": lane.name(),
                    "semantic_mode": semantic_mode(case),
                    "status": outcome.status,
                    "expected": case.expected,
                    "actual": outcome.value_bits.map(f64::from_bits),
                    "error": if outcome.detail.is_empty() { None } else { Some(outcome.detail.clone()) },
                    "compile_and_run_ms": elapsed_ms,
                    "plan_fingerprint": None::<String>,
                    "artifact_bytes": outcome.artifact_bytes as u64,
                    "metrics_unavailable": ["alloc_count", "alloc_bytes", "peak_live_bytes", "retained_objects"],
                }));
            });
        }
    });
    let _ = std::fs::remove_dir_all(&work_root);
    let (low_pass, low_fail, low_unsupported, low_unavailable) =
        *low_counts.lock().unwrap();
    let mut low_results = low_results.into_inner().unwrap();
    low_results.sort_by(|a, b| {
        (a["name"].as_str(), a["lane"].as_str()).cmp(&(b["name"].as_str(), b["lane"].as_str()))
    });
    for row in low_results {
        println!("{row}");
        case_outcomes.push(row);
    }

    let all_lanes: Vec<String> = LANES
        .iter()
        .map(|s| s.to_string())
        .chain(LOW_LANES.iter().map(|l| l.name().to_string()))
        .collect();
    let summary = serde_json::json!({
        "summary": true,
        "lanes": all_lanes,
        "source_fingerprint": corpus.source_fingerprint,
        "case_results": corpus.cases.len() * LANES.len()
            + low_pass + low_fail + low_unsupported + low_unavailable,
        "pass": pass + low_pass,
        "fail": fail + low_fail,
        "unsupported": unsupported + low_unsupported,
        "unavailable": unavailable + low_unavailable,
        "wasm_lanes": { "pass": pass, "fail": fail, "unsupported": unsupported, "unavailable": unavailable },
        "low_lanes": { "pass": low_pass, "fail": low_fail, "unsupported": low_unsupported, "unavailable": low_unavailable },
        "dreamcomp_baseline_failures": DREAMCOMP_BASELINE_FAILURES,
        "process_peak_rss_bytes": peak_rss_bytes(),
    });
    println!("{summary}");

    if let Ok(report_path) = std::env::var("JSAW_PARITY_REPORT") {
        let report = serde_json::json!({
            "schema": "jsaw.corpus-parity.v2",
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
    // Five ~Natives_inlineme families per wasm lane (DreamCompat sentinels
    // are outside jsaw), nothing else may be unsupported or unavailable.
    assert!(
        unsupported <= 5 * LANES.len(),
        "unexpected unsupported cases: {unsupported} (at most the five ~Natives_inlineme families per wasm lane are allowed)"
    );
    assert_eq!(unavailable, 0);
    assert_eq!(pass + unsupported, corpus.cases.len() * LANES.len());
    // Low-level lanes (#41): no failures; toolchains must be present
    // (fail-closed), and nothing may be unsupported outside the ~Natives_
    // families (which never enter the low-level phase at all).
    assert_eq!(low_fail, 0, "low-level lane failures (see case rows)");
    assert_eq!(
        low_unavailable, 0,
        "low-level lane toolchain unavailable (fail-closed per AGENTS.md)"
    );
    assert_eq!(low_unsupported, 0);
}
