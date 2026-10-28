//! Manifest assumption wiring (DreamComp issue #29): the canonical
//! `portal-jsc-common` AssumptionSet is resolvable through the jsaw manifest,
//! reported in the result metadata, and fails closed with machine-readable
//! diagnostics. Covers both the native `compile_to_outputs` path and the
//! wasm32-wasip1 binary.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;

use jsaw_wasi_bin::assumptions::{AssumptionId, AssumptionRegistry};
use jsaw_wasi_bin::manifest::{AssumptionInput, Manifest, Options};
use jsaw_wasi_bin::{CompileError, compile_to_outputs};

const FIXTURE: &[(&str, &str)] = &[(
    "index.js",
    "export function run(a) { return a + 1; }",
)];

fn sources() -> BTreeMap<String, String> {
    FIXTURE
        .iter()
        .map(|(p, s)| (p.to_string(), s.to_string()))
        .collect()
}

fn manifest_with(assumptions: Vec<AssumptionInput>) -> Manifest {
    Manifest {
        version: jsaw_wasi_bin::MANIFEST_VERSION,
        entry: "index.js".to_string(),
        modules: FIXTURE.iter().map(|(p, _)| p.to_string()).collect(),
        options: Options {
            assumptions,
            ..Default::default()
        },
        ffi: Default::default(),
        emit: Default::default(),
    }
}

/// The empty set's digest is pinned by the canonical registry's own tests;
/// repeating it here proves the manifest reports exactly that digest.
const EMPTY_DIGEST: &str = "0e4ac136214c1121e2fee705282a1dc4b4222e1efc5d87ca8bc46efd8ba18d27";

#[test]
fn default_manifest_reports_empty_assumptions() {
    let output = compile_to_outputs(&manifest_with(vec![]), &sources()).expect("compile");
    let report = &output.assumptions;
    assert_eq!(report.schema_version, 1);
    assert_eq!(report.registry_version, 1);
    assert!(report.requested.is_empty());
    assert!(report.effective.is_empty());
    assert_eq!(report.digest, EMPTY_DIGEST);
    assert!(output.exports.contains(&"run".to_string()));
}

#[test]
fn all_eight_named_assumptions_resolve_through_the_manifest() {
    let all = AssumptionId::ALL
        .iter()
        .map(|id| AssumptionInput::Name(id.name().to_string()))
        .collect();
    let output = compile_to_outputs(&manifest_with(all), &sources()).expect("compile");
    let report = &output.assumptions;
    let names: Vec<&str> = report.effective.iter().map(|e| e.name.as_str()).collect();
    let expected: Vec<&str> = AssumptionId::ALL.iter().map(|id| id.name()).collect();
    assert_eq!(names, expected, "canonical stable-ID order, not request order");
    assert_eq!(report.requested, report.effective);
    assert_ne!(report.digest, EMPTY_DIGEST);

    // The digest is order-independent across manifest request orderings.
    let reversed = AssumptionId::ALL
        .iter()
        .rev()
        .map(|id| AssumptionInput::Name(id.name().to_string()))
        .collect();
    let other = compile_to_outputs(&manifest_with(reversed), &sources()).expect("compile");
    assert_eq!(other.assumptions.digest, report.digest);
}

#[test]
fn assumption_digest_matches_the_canonical_registry() {
    let manifest = manifest_with(vec![AssumptionInput::Name("no-jit".to_string())]);
    let output = compile_to_outputs(&manifest, &sources()).expect("compile");
    let expected = AssumptionRegistry::new()
        .resolve([AssumptionId::NoJit])
        .unwrap()
        .digest();
    assert_eq!(output.assumptions.digest, expected);
}

#[test]
fn unknown_duplicate_and_conflicting_requests_fail_closed() {
    let unknown = compile_to_outputs(
        &manifest_with(vec![AssumptionInput::Name("not-an-assumption".to_string())]),
        &sources(),
    )
    .expect_err("unknown assumptions must fail");
    let CompileError::Assumptions(diagnostics) = unknown else {
        panic!("unknown assumption must be an assumption error");
    };
    assert!(
        matches!(
            diagnostics.as_slice(),
            [jsaw_wasi_bin::manifest::AssumptionDiagnosticReport::Unknown { .. }]
        ),
        "diagnostics: {diagnostics:?}"
    );

    let duplicate = compile_to_outputs(
        &manifest_with(vec![
            AssumptionInput::Name("no-jit".to_string()),
            AssumptionInput::Name("no-jit".to_string()),
        ]),
        &sources(),
    )
    .expect_err("duplicates must fail");
    let CompileError::Assumptions(diagnostics) = duplicate else {
        panic!("duplicate assumption must be an assumption error");
    };
    assert!(matches!(
        diagnostics.as_slice(),
        [jsaw_wasi_bin::manifest::AssumptionDiagnosticReport::Duplicate { .. }]
    ));

    let conflict = compile_to_outputs(
        &manifest_with(vec![
            AssumptionInput::Full {
                name: "no-jit".to_string(),
                parameter: Some("a".to_string()),
            },
            AssumptionInput::Full {
                name: "no-jit".to_string(),
                parameter: Some("b".to_string()),
            },
        ]),
        &sources(),
    )
    .expect_err("parameter conflicts must fail");
    let CompileError::Assumptions(diagnostics) = conflict else {
        panic!("conflicting parameters must be an assumption error");
    };
    assert!(matches!(
        diagnostics.as_slice(),
        [jsaw_wasi_bin::manifest::AssumptionDiagnosticReport::Conflict { .. }]
    ));
}

// ── wasip1 binary coverage ──────────────────────────────────────────────────

fn compiler_wasm() -> PathBuf {
    let status = Command::new(std::env::var("CARGO").unwrap_or_else(|_| "cargo".into()))
        .args(["build", "-p", "jsaw-wasi-bin", "--target", "wasm32-wasip1", "--release"])
        .status()
        .expect("cargo build for wasm32-wasip1 should run");
    assert!(status.success(), "wasm32-wasip1 build should succeed");
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/wasm32-wasip1/release/jsaw-wasi-bin.wasm");
    assert!(path.exists(), "jsaw-wasi-bin.wasm should exist after build");
    path
}

fn run_compiler(src: &Path, out: &Path, manifest_json: &str) -> (String, bool) {
    let mut config = wasmtime::Config::new();
    config.wasm_multi_memory(false);
    let engine = wasmtime::Engine::new(&config).expect("engine");
    let module = wasmtime::Module::new(&engine, std::fs::read(compiler_wasm()).unwrap())
        .expect("compiler wasm should parse");

    let mut linker: wasmtime::Linker<wasmtime_wasi::p1::WasiP1Ctx> = wasmtime::Linker::new(&engine);
    wasmtime_wasi::p1::add_to_linker_sync(&mut linker, |ctx| ctx)
        .expect("WASI p1 imports should link");

    let stdin = wasmtime_wasi::p2::pipe::MemoryInputPipe::new(manifest_json.as_bytes().to_vec());
    let stdout = wasmtime_wasi::p2::pipe::MemoryOutputPipe::new(1 << 24);
    let stdout_handle = stdout.clone();

    let mut builder = wasmtime_wasi::WasiCtx::builder();
    builder
        .stdin(stdin)
        .stdout(stdout)
        .arg("jsaw-compiler")
        .arg("--src")
        .arg("/src")
        .arg("--out")
        .arg("/out")
        .preopened_dir(src, "/src", wasmtime_wasi::DirPerms::READ, wasmtime_wasi::FilePerms::READ)
        .expect("preopen src")
        .preopened_dir(out, "/out", wasmtime_wasi::DirPerms::all(), wasmtime_wasi::FilePerms::all())
        .expect("preopen out");
    let mut store = wasmtime::Store::new(&engine, builder.build_p1());

    let instance = linker
        .instantiate(&mut store, &module)
        .expect("compiler wasm should instantiate");
    let start = instance
        .get_typed_func::<(), ()>(&mut store, "_start")
        .expect("_start export");
    let success = start.call(&mut store, ()).is_ok();

    drop(store);
    (String::from_utf8(stdout_handle.contents().to_vec()).unwrap(), success)
}

fn fixture_dirs() -> (tempfile::TempDir, tempfile::TempDir) {
    let src = tempfile::tempdir().unwrap();
    let out = tempfile::tempdir().unwrap();
    for (path, source) in FIXTURE {
        std::fs::write(src.path().join(path), source).unwrap();
    }
    (src, out)
}

#[test]
fn wasip1_binary_reports_and_enforces_assumptions() {
    let (src, out) = fixture_dirs();
    let manifest = manifest_with(vec![
        AssumptionInput::Name("no-jit".to_string()),
        AssumptionInput::Full {
            name: "natives".to_string(),
            parameter: None,
        },
    ]);
    let expected_digest = AssumptionRegistry::new()
        .resolve([AssumptionId::NoJit, AssumptionId::Natives])
        .unwrap()
        .digest();

    let (stdout, success) = run_compiler(
        src.path(),
        out.path(),
        &serde_json::to_string(&manifest).unwrap(),
    );
    assert!(success, "compiler should exit 0");
    let line = stdout.lines().next().expect("a result line");
    let result: serde_json::Value = serde_json::from_str(line).expect("result JSON");
    assert_eq!(result["status"], "ok", "result: {line}");
    assert_eq!(result["assumptions"]["digest"], expected_digest);
    let names: Vec<&str> = result["assumptions"]["effective"]
        .as_array()
        .unwrap()
        .iter()
        .map(|entry| entry["name"].as_str().unwrap())
        .collect();
    assert_eq!(names, ["no-jit", "natives"]);

    // An unknown assumption fails closed: nonzero exit, machine-readable
    // diagnostics, no prose-only error.
    let (src, out) = fixture_dirs();
    let bad = manifest_with(vec![AssumptionInput::Name("bogus".to_string())]);
    let (stdout, success) = run_compiler(
        src.path(),
        out.path(),
        &serde_json::to_string(&bad).unwrap(),
    );
    assert!(!success, "unknown assumptions must exit nonzero");
    let line = stdout.lines().next().expect("a result line");
    let result: serde_json::Value = serde_json::from_str(line).expect("result JSON");
    assert_eq!(result["status"], "assumption_error", "result: {line}");
    assert_eq!(result["diagnostics"][0]["kind"], "unknown");
    assert_eq!(result["diagnostics"][0]["name"], "bogus");
}
