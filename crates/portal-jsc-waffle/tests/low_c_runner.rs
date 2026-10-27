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

fn cc_available(cc: &str) -> bool {
    std::process::Command::new(cc)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

struct LaneResult {
    status: &'static str,
    detail: String,
    artifact_bytes: usize,
}

fn run_lane(
    case: &CorpusCase,
    lane: &str,
    dir: &Path,
    cc: &str,
) -> LaneResult {
    let plan = plan_for(&case.module);
    let emit = |lane: &str| match lane {
        "nano" => portal_jsc_waffle::low_emit::emit_c(
            &plan,
            &portal_jsc_waffle::low_emit::NanoRepresentation,
        ),
        _ => portal_jsc_waffle::low_emit::emit_c(
            &plan,
            &portal_jsc_waffle::low_emit::CoreGcRepresentation,
        ),
    };
    let c = match emit(lane) {
        Ok(c) => c,
        Err(diags) => {
            return LaneResult {
                status: "fail",
                detail: format!(
                    "emit diags: {}",
                    diags.iter()
                        .map(|d| d.to_string())
                        .collect::<Vec<_>>()
                        .join("; ")
                ),
                artifact_bytes: 0,
            };
        }
    };
    // Deterministic re-emission (acceptance: same plan → byte-identical C).
    let c2 = emit(lane).expect("re-emit");
    assert_eq!(c, c2, "{lane} emission is not deterministic");

    // The numeric-ABI runner: the export wrapper must be (f64...) -> f64.
    let low = match lane {
        "nano" => portal_jsc_waffle::low_emit::emit_low(
            &plan,
            &portal_jsc_waffle::low_emit::NanoRepresentation,
        ),
        _ => portal_jsc_waffle::low_emit::emit_low(
            &plan,
            &portal_jsc_waffle::low_emit::CoreGcRepresentation,
        ),
    }
    .expect("re-emit IR");
    assert!(
        portal_jsc_waffle::low_emit::debug::validate(&low).is_empty(),
        "IR validation failed"
    );
    let Some(export) = low.exports.iter().find(|e| e.name == case.export) else {
        return LaneResult {
            status: "fail",
            detail: format!("export {:?} not in the low-level module", case.export),
            artifact_bytes: c.len(),
        };
    };
    let ef = &low.funcs[export.func.0 as usize];
    let numeric = ef.results.len() == 1
        && ef
            .params
            .iter()
            .chain(std::iter::once(&(portal_jsc_waffle::low_emit::LeLocal(0), ef.results[0])))
            .all(|(_, t)| *t == portal_jsc_waffle::low_emit::LeType::F64);
    if !numeric {
        return LaneResult {
            status: "unsupported",
            detail: "export is not in the numeric f64 ABI".into(),
            artifact_bytes: c.len(),
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
            artifact_bytes: c.len(),
        };
    }

    let sym = format!(
        "js_export_{}",
        portal_jsc_waffle::low_emit::c_render::c_ident(&case.export)
    );
    let arg_exprs: Vec<String> = case
        .args
        .iter()
        .map(|a| {
            let v: f64 = a.trim().parse().expect("arg literal");
            format!("db(UINT64_C(0x{:016x}))", v.to_bits())
        })
        .collect();
    let main = format!(
        "#include <stdint.h>\n#include <stdio.h>\n#include <string.h>\n\
         extern void js_start(void);\nextern double {sym}({params});\n\
         static double db(uint64_t b){{double d;memcpy(&d,&b,8);return d;}}\n\
         int main(void){{ js_start(); double r = {sym}({args}); \
         uint64_t b; memcpy(&b,&r,8); printf(\"%016llx\\n\",(unsigned long long)b); return 0; }}\n",
        params = vec!["double"; case.args.len()].join(", "),
        args = arg_exprs.join(", "),
    );
    std::fs::write(dir.join("m.c"), &c).expect("write module");
    std::fs::write(dir.join("main.c"), &main).expect("write main");
    let out = std::process::Command::new(cc)
        .args(["-std=c99", "-O0", "-w", "-o"])
        .arg(dir.join("a.out"))
        .arg(dir.join("m.c"))
        .arg(dir.join("main.c"))
        .arg("-lm")
        .output()
        .expect("cc spawn");
    if !out.status.success() {
        return LaneResult {
            status: "fail",
            detail: format!(
                "cc failed: {}",
                String::from_utf8_lossy(&out.stderr)
                    .lines()
                    .take(3)
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
            artifact_bytes: c.len(),
        };
    }
    let run = std::process::Command::new(dir.join("a.out")).output().expect("run");
    if !run.status.success() {
        return LaneResult {
            status: "fail",
            detail: format!(
                "artifact exited with {} (trap or crash)",
                run.status
            ),
            artifact_bytes: c.len(),
        };
    }
    let text = String::from_utf8_lossy(&run.stdout).trim().to_string();
    let Ok(bits) = u64::from_str_radix(&text, 16) else {
        return LaneResult {
            status: "fail",
            detail: format!("unparseable output {text:?}"),
            artifact_bytes: c.len(),
        };
    };
    let actual = f64::from_bits(bits);
    if actual == case.expected || (actual.is_nan() && case.expected.is_nan()) {
        LaneResult {
            status: "pass",
            detail: String::new(),
            artifact_bytes: c.len(),
        }
    } else {
        LaneResult {
            status: "fail",
            detail: format!("expected {}, got {actual}", case.expected),
            artifact_bytes: c.len(),
        }
    }
}

#[test]
fn c_backend_corpus_parity() {
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());
    assert!(
        cc_available(&cc),
        "C backend runner prerequisite: no C compiler ({cc:?}) on PATH; \
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
        for lane in ["nano", "coregc"] {
            let dir = work.join(format!("{}_{lane}", case.name));
            std::fs::create_dir_all(&dir).expect("workdir");
            let r = run_lane(case, lane, &dir, &cc);
            report_rows.push(serde_json::json!({
                "case": case.name,
                "lane": lane,
                "status": r.status,
                "artifact_bytes": r.artifact_bytes,
            }));
            match r.status {
                "pass" => pass += 1,
                "unsupported" => unsupported += 1,
                _ => {
                    fail += 1;
                    failures.push(format!("{} [{lane}]: {}", case.name, r.detail));
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
