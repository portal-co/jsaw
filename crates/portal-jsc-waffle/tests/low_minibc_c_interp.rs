//! MiniBC cross-interpreter acceptance test (DreamComp #39 acceptance #1):
//! jsaw-emitted MiniBC artifacts execute under the **generated C interpreter
//! header** (`dreamcompiler-minibc-c-interp`) with results identical to the
//! corpus expectation — the same values the stock Rust `MiniInterp` produces
//! (covered by `low_minibc_runner.rs`). The C driver
//! (`low_emit::minibc_c_host`) implements the jsaw host-op family and the
//! `StdExtOp` control-flow protocol in C, so the generated header itself
//! runs unmodified.
//!
//! Fails closed when no C compiler is available (AGENTS.md).

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use serde::Deserialize;

use dreamcompiler_minibc_c_interp::{CCodegenConfig, CInterpreterCompiler, MiniBc};
use portal_jsc_waffle::low_emit::{self, exec::LowLane};

#[derive(Clone, Debug, Deserialize)]
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

fn cc() -> Option<String> {
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".into());
    std::process::Command::new(&cc)
        .arg("--version")
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|_| cc)
}

/// Generate the interpreter header with the full stock MiniBC opcode set
/// (including `AluRaw`/`AluUn`/`Trap` — the jsaw extensions).
fn generate_header() -> String {
    let mut compiler = CInterpreterCompiler::with_config(CCodegenConfig {
        function_name: "mn_interpret".into(),
        result_struct_name: "MnResult".into(),
        store_size: 256,
        num_registers: 256,
        exit_opcode: 0xFF,
    });
    compiler.add_opcode_enum::<MiniBc>();
    compiler.generate()
}

struct LaneResult {
    status: &'static str,
    detail: String,
}

fn run_case_c_interp(case: &CorpusCase, lane: LowLane, header: &str, cc: &str, dir: &Path) -> LaneResult {
    let plan = plan_for(&case.module);
    let (module, artifact) = match lane {
        LowLane::MinibcNano => {
            low_emit::emit_minibc(&plan, &low_emit::NanoRepresentation).expect("emit")
        }
        LowLane::MinibcCoregc => {
            low_emit::emit_minibc(&plan, &low_emit::CoreGcRepresentation).expect("emit")
        }
        _ => unreachable!(),
    };
    if !artifact.exports.contains_key(&case.export) {
        return LaneResult {
            status: "fail",
            detail: format!("export {:?} not emitted", case.export),
        };
    }
    // Numeric-ABI check (same rule as the other low lanes).
    let low = match lane {
        LowLane::MinibcNano => low_emit::emit_low(&plan, &low_emit::NanoRepresentation),
        _ => low_emit::emit_low(&plan, &low_emit::CoreGcRepresentation),
    }
    .expect("re-emit IR");
    let export = low
        .exports
        .iter()
        .find(|e| e.name == case.export)
        .expect("export");
    let ef = &low.funcs[export.func.0 as usize];
    if !(ef.results.len() == 1
        && ef.params.iter().all(|(_, t)| *t == low_emit::LeType::F64)
        && ef.results[0] == low_emit::LeType::F64)
    {
        return LaneResult {
            status: "unsupported",
            detail: "export is not in the numeric f64 ABI".into(),
        };
    }

    std::fs::create_dir_all(dir).expect("workdir");
    std::fs::write(dir.join("artifact.minibc"), &artifact.bytes).expect("write artifact");
    let meta = low_emit::minibc_c_host::c_meta_source(&module, &artifact, &case.export);
    let source = format!("{header}\n{meta}\n{}", low_emit::minibc_c_host::C_HOST_HARNESS);
    let src_path = dir.join("driver.c");
    let bin_path = dir.join("driver");
    std::fs::write(&src_path, source).expect("write driver");
    let compile = std::process::Command::new(cc)
        .args(["-std=c11", "-O1", "-w", "-o"])
        .arg(&bin_path)
        .arg(&src_path)
        .arg("-lm")
        .output()
        .expect("cc spawn");
    if !compile.status.success() {
        return LaneResult {
            status: "fail",
            detail: format!(
                "compile: {}",
                String::from_utf8_lossy(&compile.stderr)
                    .lines()
                    .filter(|l| l.contains("error"))
                    .take(3)
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
        };
    }
    let args: Vec<String> = case
        .args
        .iter()
        .map(|x| {
            let v: f64 = x.trim().parse().expect("arg literal");
            format!("{:016x}", v.to_bits())
        })
        .collect();
    let run = std::process::Command::new(&bin_path)
        .arg(dir.join("artifact.minibc"))
        .args(&args)
        .output()
        .expect("run");
    if !run.status.success() {
        return LaneResult {
            status: "fail",
            detail: format!("driver exited with {} (trap or host failure)", run.status),
        };
    }
    let text = String::from_utf8_lossy(&run.stdout).trim().to_string();
    let Ok(bits) = u64::from_str_radix(&text, 16) else {
        return LaneResult {
            status: "fail",
            detail: format!("unparseable output {text:?}"),
        };
    };
    let actual = f64::from_bits(bits);
    if actual == case.expected || (actual.is_nan() && case.expected.is_nan()) {
        LaneResult {
            status: "pass",
            detail: String::new(),
        }
    } else {
        LaneResult {
            status: "fail",
            detail: format!("expected {}, got {actual}", case.expected),
        }
    }
}

#[test]
fn minibc_c_interp_corpus() {
    let Some(cc) = cc() else {
        panic!("MiniBC C-interp acceptance prerequisite: no C compiler on PATH; set $CC (fail-closed per AGENTS.md)");
    };
    let corpus: Corpus = serde_json::from_str(
        &std::fs::read_to_string(corpus_path()).expect("corpus file"),
    )
    .expect("corpus parses");
    let header = generate_header();

    let work_root = std::env::temp_dir().join(format!("jsaw_minibc_cinterp_{}", std::process::id()));
    let mut all_cases = corpus.cases;
    all_cases.extend(ext_cases());
    let queue: Mutex<Vec<(CorpusCase, LowLane)>> = Mutex::new(
        all_cases
            .into_iter()
            .filter(|c| !c.module.contains("~Natives_"))
            .flat_map(|c| {
                [LowLane::MinibcNano, LowLane::MinibcCoregc]
                    .into_iter()
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
            let header = &header;
            let cc = &cc;
            scope.spawn(move || loop {
                let next = queue.lock().unwrap().pop();
                let Some((case, lane)) = next else { break };
                let dir = work_root.join(format!("{}_{}", case.name, lane.name()));
                let r = run_case_c_interp(&case, lane, header, cc, &dir);
                if std::env::var("JSKEEP").is_err() {
                    let _ = std::fs::remove_dir_all(&dir);
                }
                results.lock().unwrap().push((case.name.clone(), lane, r));
            });
        }
    });

    let results = results.into_inner().unwrap();
    let mut pass = 0usize;
    let mut fail = 0usize;
    let mut unsupported = 0usize;
    let mut failures: Vec<String> = Vec::new();
    for (name, lane, r) in &results {
        match r.status {
            "pass" => pass += 1,
            "unsupported" => unsupported += 1,
            _ => {
                fail += 1;
                failures.push(format!("{} [{}]: {}", name, lane.name(), r.detail));
            }
        }
    }
    eprintln!("minibc C-interp corpus: {pass} pass, {fail} fail, {unsupported} unsupported");
    if std::env::var("JSKEEP").is_err() {
        let _ = std::fs::remove_dir_all(&work_root);
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
