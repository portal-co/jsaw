//! Compilation and execution of one test variant through the
//! portal-jsc-waffle WasmGC backend, with wasmtime epoch-interruption
//! timeouts.

use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use portal_jsc_swc_cfg::module::CfgModule;
use portal_jsc_swc_ssa::module::SModule;
use portal_jsc_swc_tac::module::TModule;
use portal_pc_waffle::Module;
use swc_common::{FileName, Globals, SourceMap, GLOBALS, sync::Lrc};
use swc_ecma_ast::{EsVersion, Module as SwcModule};
use swc_ecma_parser::{EsSyntax, Syntax, parse_file_as_module};

pub const RUN_ENTRY: &str = "$262run";

/// Compile variant source to WasmGC bytes. `Err` carries the backend's
/// error text (compile-phase failures).
pub fn compile(source: &str) -> anyhow::Result<Vec<u8>> {
    GLOBALS.set(&Globals::default(), || compile_inner(source))
}

fn compile_inner(source: &str) -> anyhow::Result<Vec<u8>> {
    let cm: Lrc<SourceMap> = Lrc::new(SourceMap::default());
    let file = cm.new_source_file(
        Lrc::new(FileName::Custom("test262-variant.js".into())),
        source.to_owned(),
    );
    let mut errors = vec![];
    let parsed = parse_file_as_module(
        &file,
        Syntax::Es(EsSyntax::default()),
        EsVersion::Es2022,
        None,
        &mut errors,
    )
    .map_err(|error| anyhow::anyhow!("parse error: {error:?}"))?;
    if !errors.is_empty() {
        return Err(anyhow::anyhow!("parse diagnostics: {errors:?}"));
    }
    let module = SwcModule {
        span: parsed.span,
        body: parsed.body,
        shebang: parsed.shebang,
    };
    let cfg = CfgModule::try_from(module).map_err(|error| anyhow::anyhow!("CFG: {error}"))?;
    let tac = TModule::try_from(cfg).map_err(|error| anyhow::anyhow!("TAC: {error}"))?;
    let ssa = SModule::try_from(tac).map_err(|error| anyhow::anyhow!("SSA: {error}"))?;
    let options = portal_jsc_waffle::ConvertOptions {
        run_entry_export: Some(RUN_ENTRY.to_owned()),
        ..Default::default()
    };
    let mut wasm = Module::empty();
    portal_jsc_waffle::convert_module(&ssa, &mut wasm, &options)
        .map_err(|error| anyhow::anyhow!("waffle: {error}"))?;
    let bytes = portal_pc_waffle::to_wasm_bytes(&wasm)?;
    validate(&bytes)?;
    Ok(bytes)
}

fn validate(bytes: &[u8]) -> anyhow::Result<()> {
    let mut features = wasmparser::WasmFeatures::default();
    features.set(wasmparser::WasmFeatures::GC, true);
    wasmparser::Validator::new_with_features(features)
        .validate_all(bytes)
        .map_err(|error| anyhow::anyhow!("wasm validation: {error}"))?;
    Ok(())
}

/// Outcome of executing a variant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Execution {
    /// Entry returned status 0 within the timeout.
    Completed,
    /// Entry trapped (thrown JS error, OOB, ...) within the timeout.
    Trapped(String),
    /// Exceeded the timeout (epoch interruption).
    Timeout,
}

/// Execute the compiled variant's run entry under wasmtime with an
/// epoch-based timeout.
pub fn execute(bytes: &[u8], timeout: Duration) -> anyhow::Result<Execution> {
    use wasmtime::{Instance, Module as WasmtimeModule, Store, Val};

    let engine = shared_engine();
    // Drive the epoch from a background thread at ~1ms granularity; the
    // store budget is set in epochs below.
    let engine_epoch = engine.clone();
    let stop = Arc::new(AtomicBool::new(false));
    let stop_ticker = stop.clone();
    let ticker = std::thread::spawn(move || {
        while !stop_ticker.load(Ordering::Relaxed) {
            std::thread::sleep(Duration::from_millis(1));
            engine_epoch.increment_epoch();
        }
    });

    let result = (|| -> anyhow::Result<Execution> {
        let module = WasmtimeModule::new(&engine, bytes).map_err(wasmtime_error_to_anyhow)?;
        let mut store = Store::new(&engine, ());
        store.set_epoch_deadline(epochs_for(timeout));
        // On deadline, report a timeout rather than trapping by default.
        store.epoch_deadline_trap();
        let instance =
            Instance::new(&mut store, &module, &[]).map_err(wasmtime_error_to_anyhow)?;
        let function = instance
            .get_func(&mut store, RUN_ENTRY)
            .ok_or_else(|| anyhow::anyhow!("missing run entry {RUN_ENTRY}"))?;
        let mut outputs = [Val::I32(1)];
        match function.call(&mut store, &[], &mut outputs) {
            Ok(()) => match outputs[0] {
                Val::I32(0) => Ok(Execution::Completed),
                Val::I32(status) => Ok(Execution::Trapped(format!("status {status}"))),
                ref value => Err(anyhow::anyhow!("run entry returned {value:?}")),
            },
            Err(error) => {
                let text = format!("{error:#}");
                if text.contains("epoch deadline reached") {
                    Ok(Execution::Timeout)
                } else {
                    Ok(Execution::Trapped(text))
                }
            }
        }
    })();

    stop.store(true, Ordering::Relaxed);
    let _ = ticker.join();
    result
}

fn epochs_for(timeout: Duration) -> u64 {
    // Ticks at ~1ms.
    u64::try_from(timeout.as_millis()).unwrap_or(u64::MAX).max(1)
}

fn wasmtime_error_to_anyhow(error: wasmtime::Error) -> anyhow::Error {
    // wasmtime's no_std Error renders the trap reason inline in Display
    // (last frame: "...: wasm trap: <reason>"); there is no source chain to
    // walk because `wasmtime::Error` does not implement `StdError`.
    anyhow::anyhow!(error.to_string())
}

/// One shared engine for the whole run. Profiling (sample(1)) showed
/// per-variant `Engine::new` plus Cranelift's egraph optimization pass
/// dominating wall time on the generated WasmGC; opt level `none` skips
/// exactly that pass (irrelevant for our tiny test bodies) and the shared
/// engine amortizes setup across thousands of variants.
fn shared_engine() -> wasmtime::Engine {
    static ENGINE: std::sync::OnceLock<wasmtime::Engine> = std::sync::OnceLock::new();
    ENGINE
        .get_or_init(|| {
            let mut config = wasmtime::Config::new();
            config.wasm_gc(true);
            config.wasm_function_references(true);
            config.epoch_interruption(true);
            config.cranelift_opt_level(wasmtime::OptLevel::None);
            wasmtime::Engine::new(&config).expect("wasmtime engine should initialize")
        })
        .clone()
}

/// Locate the test262 harness directory. The 2026 upstream layout keeps
/// per-concern harness files in `<root>/harness/` (assert.js, sta.js,
/// propertyHelper.js, ...); older checkouts used `test/harness/`. Prefer the
/// new layout, fall back to the old one.
pub fn harness_dir(test262_root: &Path) -> std::path::PathBuf {
    let new = test262_root.join("harness");
    if new.join("assert.js").is_file() {
        return new;
    }
    let old = test262_root.join("test").join("harness");
    if old.join("harness.js").is_file() || old.join("sta.js").is_file() {
        return old;
    }
    new
}
