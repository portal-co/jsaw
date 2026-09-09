//! Milestone 12 smoke test: build the wasip1 binary, run it under
//! wasmtime with WASI preopens on a multi-module fixture, and assert the
//! produced WasmGC module validates with the GC feature enabled.

use std::path::{Path, PathBuf};
use std::process::Command;

use wasmtime::{Config, Engine, Linker, Module, Store};

/// The multi-module fixture: an entry importing a helper from a sibling,
/// plus a deep tail-recursive function (exercises the O(1) tail path in
/// the emitted module — though M12 only asserts validation, not execution).
const FIXTURE: &[(&str, &str)] = &[
    (
        "index.js",
        "import { triple } from './lib.js';\n\
         export function run(a) { return triple(a) + 1; }\n\
         export function count(n) {\n\
           let f = function(k, acc) { if (k <= 0) { return acc; } return f(k - 1, acc + k); };\n\
           return f(n, 0);\n\
         }",
    ),
    ("lib.js", "export function triple(v) { return v * 3; }"),
];

fn manifest() -> jsaw_wasi_bin::manifest::Manifest {
    manifest_with_emit(jsaw_wasi_bin::manifest::Emit {
        wasm: Some("module.wasm".to_string()),
        java: None,
        swift: None,
    })
}

fn manifest_with_emit(emit: jsaw_wasi_bin::manifest::Emit) -> jsaw_wasi_bin::manifest::Manifest {
    jsaw_wasi_bin::manifest::Manifest {
        version: jsaw_wasi_bin::manifest::MANIFEST_VERSION,
        entry: "index.js".to_string(),
        modules: FIXTURE.iter().map(|(p, _)| p.to_string()).collect(),
        options: Default::default(),
        emit,
    }
}

fn fixture_sources() -> std::collections::BTreeMap<String, String> {
    FIXTURE
        .iter()
        .map(|(p, s)| (p.to_string(), s.to_string()))
        .collect()
}

/// Build the wasip1 binary (cargo makes this a no-op when it is fresh)
/// and return its path. Done unconditionally so the test always runs the
/// current source, never a stale artifact.
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

/// Run the compiler wasm under wasmtime with `src` and `out` preopened,
/// feeding `manifest_json` to stdin, and return (stdout, stderr, success).
fn run_compiler(src: &Path, out: &Path, manifest_json: &str) -> (String, String, bool) {
    let mut config = Config::new();
    config.wasm_multi_memory(false);
    let engine = Engine::new(&config).expect("engine");
    let module = Module::new(&engine, std::fs::read(compiler_wasm()).unwrap())
        .expect("compiler wasm should parse");

    let mut linker: Linker<wasmtime_wasi::p1::WasiP1Ctx> = Linker::new(&engine);
    wasmtime_wasi::p1::add_to_linker_sync(&mut linker, |ctx| ctx)
        .expect("WASI p1 imports should link");

    let stdin = wasmtime_wasi::p2::pipe::MemoryInputPipe::new(manifest_json.as_bytes().to_vec());
    let stdout = wasmtime_wasi::p2::pipe::MemoryOutputPipe::new(1 << 24);
    let stderr = wasmtime_wasi::p2::pipe::MemoryOutputPipe::new(1 << 20);
    let stdout_handle = stdout.clone();
    let stderr_handle = stderr.clone();

    let mut builder = wasmtime_wasi::WasiCtx::builder();
    builder
        .stdin(stdin)
        .stdout(stdout)
        .stderr(stderr)
        .arg("jsaw-compiler")
        .arg("--src")
        .arg("/src")
        .arg("--out")
        .arg("/out")
        .preopened_dir(src, "/src", wasmtime_wasi::DirPerms::READ, wasmtime_wasi::FilePerms::READ)
        .expect("preopen src")
        .preopened_dir(out, "/out", wasmtime_wasi::DirPerms::all(), wasmtime_wasi::FilePerms::all())
        .expect("preopen out");
    let mut store = Store::new(&engine, builder.build_p1());

    let instance = linker
        .instantiate(&mut store, &module)
        .expect("compiler wasm should instantiate");
    let start = instance
        .get_typed_func::<(), ()>(&mut store, "_start")
        .expect("_start export");
    let success = start.call(&mut store, ()).is_ok();

    drop(store);
    let out_str = String::from_utf8(stdout_handle.contents().to_vec()).unwrap();
    let err_str = String::from_utf8(stderr_handle.contents().to_vec()).unwrap();
    (out_str, err_str, success)
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
fn native_driver_sees_exports() {
    // The same driver that runs under wasip1 must, natively, see the
    // module's exports — this isolates a wasip1-only lowering divergence
    // from a driver bug.
    let (exports, outputs) =
        jsaw_wasi_bin::compile_to_outputs(&manifest(), &fixture_sources()).expect("native compile");
    assert!(
        exports.contains(&"run".to_string()),
        "native exports should contain run: {exports:?}"
    );
    assert!(outputs.contains_key("module.wasm"));
}

#[test]
fn compiles_module_set_to_valid_wasmgc_under_wasmtime() {
    let (src, out) = fixture_dirs();
    let manifest_json = serde_json::to_string(&manifest()).unwrap();
    let (stdout, stderr, success) = run_compiler(src.path(), out.path(), &manifest_json);
    assert!(success, "compiler should exit 0; stderr: {stderr}");

    // Parse the single stdout JSON line.
    let line = stdout.lines().next().expect("a result line");
    let result: serde_json::Value = serde_json::from_str(line).expect("result should be JSON");
    assert_eq!(result["status"], "ok", "compile should succeed: {line}");
    let exports: Vec<&str> = result["exports"]
        .as_array()
        .unwrap()
        .iter()
        .map(|e| e.as_str().unwrap())
        .collect();
    assert!(exports.contains(&"run"), "exports should contain run: {exports:?}");
    assert!(exports.contains(&"count"), "exports should contain count: {exports:?}");

    // The emitted WasmGC module must exist and validate with GC enabled.
    let wasm_path = out.path().join("module.wasm");
    assert!(wasm_path.exists(), "module.wasm should be written");
    let bytes = std::fs::read(&wasm_path).unwrap();
    let mut features = wasmparser::WasmFeatures::default();
    features.set(wasmparser::WasmFeatures::GC, true);
    let mut validator = wasmparser::Validator::new_with_features(features);
    validator
        .validate_all(&bytes)
        .expect("emitted WasmGC should validate");
}

#[test]
fn reports_a_manifest_error_cleanly() {
    let (src, out) = fixture_dirs();
    // Entry not in the module set is a structured error, exit code 1.
    let mut bad = manifest();
    bad.entry = "nope.js".to_string();
    let (stdout, _stderr, success) =
        run_compiler(src.path(), out.path(), &serde_json::to_string(&bad).unwrap());
    assert!(!success, "bad manifest should exit nonzero");
    let line = stdout.lines().next().expect("a result line");
    let result: serde_json::Value = serde_json::from_str(line).unwrap();
    assert_eq!(result["status"], "error");
    assert!(
        result["error"].as_str().unwrap().contains("not in the module set"),
        "error should mention the entry: {line}"
    );
}

/// Milestone 13: the Java and Swift the wasip1 binary emits must be
/// byte-identical to the same emitters run natively on the same module.
/// This pins the wasm build's determinism and proves the emitters work
/// identically inside the sandbox.
#[test]
fn java_and_swift_outputs_match_native_emission_byte_for_byte() {
    let emit = jsaw_wasi_bin::manifest::Emit {
        wasm: None,
        java: Some("java".to_string()),
        swift: Some("swift".to_string()),
    };
    let manifest = manifest_with_emit(emit);

    // Native emission.
    let (native_exports, native_outputs) =
        jsaw_wasi_bin::compile_to_outputs(&manifest, &fixture_sources()).expect("native compile");
    assert!(
        native_outputs.keys().any(|p| p.starts_with("java/")),
        "native emission should produce java outputs"
    );
    assert!(
        native_outputs.keys().any(|p| p.starts_with("swift/")),
        "native emission should produce swift outputs"
    );

    // wasip1 emission.
    let (src, out) = fixture_dirs();
    let (stdout, stderr, success) =
        run_compiler(src.path(), out.path(), &serde_json::to_string(&manifest).unwrap());
    assert!(success, "compiler should exit 0; stderr: {stderr}");
    let line = stdout.lines().next().expect("a result line");
    let result: serde_json::Value = serde_json::from_str(line).unwrap();
    assert_eq!(result["status"], "ok", "compile should succeed: {line}");

    // Collect the written files into the same path->bytes shape.
    let mut wasip1_outputs: std::collections::BTreeMap<String, Vec<u8>> =
        std::collections::BTreeMap::new();
    for top in ["java", "swift"] {
        let dir = out.path().join(top);
        let mut stack = vec![dir.clone()];
        while let Some(d) = stack.pop() {
            for entry in std::fs::read_dir(&d).unwrap() {
                let entry = entry.unwrap();
                let path = entry.path();
                if path.is_dir() {
                    stack.push(path);
                } else {
                    let rel = path.strip_prefix(out.path()).unwrap().to_string_lossy().replace('\\', "/");
                    wasip1_outputs.insert(rel, std::fs::read(&path).unwrap());
                }
            }
        }
    }

    // Byte-for-byte equality on every emitted file, and the same export list.
    assert_eq!(
        native_outputs, wasip1_outputs,
        "wasip1 and native emission must be byte-identical"
    );
    let wasip1_exports: Vec<String> = result["exports"]
        .as_array()
        .unwrap()
        .iter()
        .map(|e| e.as_str().unwrap().to_string())
        .collect();
    assert_eq!(native_exports, wasip1_exports, "export lists must match");
}
