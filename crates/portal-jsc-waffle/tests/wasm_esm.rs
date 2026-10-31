//! wasm-esm integration (portal-co/jsaw#14, docs/plan-wasm-esm-modules.md):
//! Wasm binary members of the closed module set link through
//! `build_import_table`, merge into the output module, and execute through
//! boxed-JS adapters and raw re-export wrappers.

use portal_jsc_waffle::{ModuleSet, WasmProfile, convert_modules, module_set_from_sources};
use portal_pc_waffle::{EntityRef, Module};
use wasmtime::{Config, Engine, Instance, Store, Val};

// ---------------------------------------------------------------------------
// Minimal Wasm binary builders (deterministic; no `wat` dependency).

fn uleb(mut value: u64) -> Vec<u8> {
    let mut out = vec![];
    loop {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0x80;
        }
        out.push(byte);
        if value == 0 {
            return out;
        }
    }
}

fn section(id: u8, payload: Vec<u8>) -> Vec<u8> {
    let mut out = vec![id];
    out.extend(uleb(payload.len() as u64));
    out.extend(payload);
    out
}

fn module(sections: &[Vec<u8>]) -> Vec<u8> {
    let mut out = b"\0asm\x01\0\0\0".to_vec();
    for s in sections {
        out.extend_from_slice(s);
    }
    out
}

fn vec_of(items: Vec<Vec<u8>>) -> Vec<u8> {
    let mut out = uleb(items.len() as u64);
    for item in items {
        out.extend(item);
    }
    out
}

fn func_type(params: &[u8], results: &[u8]) -> Vec<u8> {
    let mut out = vec![0x60];
    out.extend(uleb(params.len() as u64));
    out.extend_from_slice(params);
    out.extend(uleb(results.len() as u64));
    out.extend_from_slice(results);
    out
}

fn name(text: &str) -> Vec<u8> {
    let mut out = uleb(text.len() as u64);
    out.extend(text.as_bytes());
    out
}

fn export_entry(field: &str, kind: u8, index: u32) -> Vec<u8> {
    let mut e = name(field);
    e.push(kind);
    e.extend(uleb(index as u64));
    e
}

fn code_body(body: &[u8]) -> Vec<u8> {
    let mut b = uleb(body.len() as u64);
    b.extend_from_slice(body);
    b
}

const I32: u8 = 0x7f;
const I64: u8 = 0x7e;
const F32: u8 = 0x7d;

/// `(module (func (export "add") (param i32 i32) (result i32)
///   local.get 0 local.get 1 i32.add))`
fn add_module() -> Vec<u8> {
    module(&[
        section(1, vec_of(vec![func_type(&[I32, I32], &[I32])])),
        section(3, vec_of(vec![uleb(0)])),
        section(7, vec_of(vec![export_entry("add", 0x00, 0)])),
        section(
            10,
            vec_of(vec![code_body(&[0x00, 0x20, 0x00, 0x20, 0x01, 0x6a, 0x0b])]),
        ),
    ])
}

/// `(module (func (export "add64") (param i64 i64) (result i64) ...))`
fn add64_module() -> Vec<u8> {
    module(&[
        section(1, vec_of(vec![func_type(&[I64, I64], &[I64])])),
        section(3, vec_of(vec![uleb(0)])),
        section(7, vec_of(vec![export_entry("add64", 0x00, 0)])),
        section(
            10,
            vec_of(vec![code_body(&[0x00, 0x20, 0x00, 0x20, 0x01, 0x7c, 0x0b])]),
        ),
    ])
}

/// `(module (func (export "answer") (result i32) i32.const 42))`
fn answer_module() -> Vec<u8> {
    module(&[
        section(1, vec_of(vec![func_type(&[], &[I32])])),
        section(3, vec_of(vec![uleb(0)])),
        section(7, vec_of(vec![export_entry("answer", 0x00, 0)])),
        section(10, vec_of(vec![code_body(&[0x00, 0x41, 0x2a, 0x0b])])),
    ])
}

/// `(module (func (export "demote") (param f32) (result f32) local.get 0))`
fn f32_module() -> Vec<u8> {
    module(&[
        section(1, vec_of(vec![func_type(&[F32], &[F32])])),
        section(3, vec_of(vec![uleb(0)])),
        section(7, vec_of(vec![export_entry("demote", 0x00, 0)])),
        section(10, vec_of(vec![code_body(&[0x00, 0x20, 0x00, 0x0b])])),
    ])
}

/// `(module (memory 1) (data (i32.const 4) "hi!")
///   (func (export "read8") (param i32) (result i32)
///     local.get 0 i32.load8_u))`
fn memory_module() -> Vec<u8> {
    module(&[
        section(1, vec_of(vec![func_type(&[I32], &[I32])])),
        section(3, vec_of(vec![uleb(0)])),
        section(5, vec_of(vec![vec![0x00, 0x01]])),
        section(7, vec_of(vec![export_entry("read8", 0x00, 0)])),
        section(
            10,
            vec_of(vec![code_body(&[0x00, 0x20, 0x00, 0x2d, 0x00, 0x00, 0x0b])]),
        ),
        section(
            11,
            vec_of(vec![{
                // flags 0 (active, memory 0), offset i32.const 4, bytes "hi!"
                let mut d = vec![0x00, 0x41, 0x04, 0x0b];
                d.extend(uleb(3));
                d.extend(b"hi!");
                d
            }]),
        ),
    ])
}

/// `(module (func (export "pair") (result i32 i32) i32.const 1 i32.const 2))`
fn pair_module() -> Vec<u8> {
    module(&[
        section(1, vec_of(vec![func_type(&[], &[I32, I32])])),
        section(3, vec_of(vec![uleb(0)])),
        section(7, vec_of(vec![export_entry("pair", 0x00, 0)])),
        section(
            10,
            vec_of(vec![code_body(&[0x00, 0x41, 0x01, 0x41, 0x02, 0x0b])]),
        ),
    ])
}

// ---------------------------------------------------------------------------
// Compilation and execution helpers.

fn convert(
    entry: &str,
    set: &ModuleSet<'_>,
) -> Result<Module<'static>, portal_jsc_waffle::ConvertError> {
    let mut module = portal_pc_waffle::Module::empty();
    convert_modules(entry, set, &mut module, &Default::default())?;
    Ok(module)
}

fn wasm_bytes(module: &Module<'_>) -> Vec<u8> {
    portal_pc_waffle::to_wasm_bytes(module).expect("Wasm emission should succeed")
}

/// Call an exported function with the given raw Wasm signature.
fn execute_raw(bytes: &[u8], name: &str, args: &[Val], results: &mut [Val]) {
    let mut config = Config::new();
    config.wasm_gc(true);
    config.wasm_function_references(true);
    config.wasm_memory64(true);
    config.wasm_multi_memory(true);
    let engine = Engine::new(&config).expect("Wasmtime engine should support WasmGC");
    let module = wasmtime::Module::new(&engine, bytes).expect("Wasmtime should compile the module");
    let mut store = Store::new(&engine, ());
    let instance =
        Instance::new(&mut store, &module, &[]).expect("Wasmtime should instantiate the module");
    let function = instance
        .get_func(&mut store, name)
        .unwrap_or_else(|| panic!("missing export {name:?}"));
    function
        .call(&mut store, args, results)
        .unwrap_or_else(|error| panic!("Wasmtime call {name:?} failed: {error:#}"));
}

/// Call a numeric (`f64 × arity -> f64`) jsaw export in Wasmtime and Node.
fn execute_numeric_in_all_runtimes(bytes: &[u8], name: &str, args: &[f64]) -> (f64, f64) {
    let inputs: Vec<Val> = args
        .iter()
        .copied()
        .map(|value| Val::F64(value.to_bits()))
        .collect();
    let mut outputs = [Val::F64(0)];
    execute_raw(bytes, name, &inputs, &mut outputs);
    let wasmtime = match outputs[0] {
        Val::F64(bits) => f64::from_bits(bits),
        ref value => panic!("numeric export returned {value:?}, expected f64"),
    };

    use std::process::Command;
    static NEXT_FIXTURE: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let fixture = std::env::temp_dir().join(format!(
        "jsaw-wasm-esm-{}-{}.wasm",
        std::process::id(),
        NEXT_FIXTURE.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
    ));
    std::fs::write(&fixture, bytes).expect("should write temporary Wasm fixture");
    let script = r#"
        import { readFile } from 'node:fs/promises';
        const [path, name, ...args] = process.argv.slice(1);
        const bytes = await readFile(path);
        const { instance } = await WebAssembly.instantiate(bytes);
        const value = instance.exports[name](...args.map(Number));
        process.stdout.write(String(value));
    "#;
    let result = Command::new("node")
        .args(["--input-type=module", "-e", script])
        .arg(&fixture)
        .arg(name)
        .args(args.iter().map(ToString::to_string))
        .output();
    let _ = std::fs::remove_file(&fixture);
    let output = result.expect("Node.js is required for WasmGC execution tests");
    assert!(
        output.status.success(),
        "Node.js instantiation/call failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let node = String::from_utf8(output.stdout)
        .expect("Node output should be UTF-8")
        .parse()
        .expect("Node numeric result should parse as f64");
    (wasmtime, node)
}

// ---------------------------------------------------------------------------
// Tests.

#[test]
fn uncalled_wasm_imports_link_and_lowering_stays_unchanged() {
    // The import is declared but never called: no adapter is needed and
    // conversion must succeed exactly as before.
    let mut set = module_set_from_sources([(
        "main.js",
        "import { add } from './math.wasm'; export function run() { return 41 + 1; }",
    )])
    .expect("sources");
    set.insert_wasm("math.wasm", &add_module())
        .expect("valid Wasm member");
    let module = convert("main.js", &set).expect("an uncalled wasm import must convert");
    let bytes = wasm_bytes(&module);
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[]);
    assert_eq!(wasmtime, 42.0);
    assert_eq!(node, 42.0);
}

#[test]
fn js_calls_wasm_member_through_boxed_adapter() {
    let mut set = module_set_from_sources([(
        "main.js",
        "import { add } from './math.wasm'; export function run(a, b) { return add(a, b); }",
    )])
    .expect("sources");
    set.insert_wasm("math.wasm", &add_module())
        .expect("valid Wasm member");
    let module = convert("main.js", &set).expect("wasm member call should lower");
    let bytes = wasm_bytes(&module);
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[19.0, 23.0]);
    assert_eq!(wasmtime, 42.0, "Wasmtime");
    assert_eq!(node, 42.0, "Node");
}

#[test]
fn aliased_import_reuses_the_same_adapter() {
    let mut set = module_set_from_sources([(
        "main.js",
        "import { add as plus, add as sum } from './math.wasm'; \
         export function run(a, b) { return plus(a, b) + sum(1, 2); }",
    )])
    .expect("sources");
    set.insert_wasm("math.wasm", &add_module())
        .expect("valid Wasm member");
    let module = convert("main.js", &set).expect("aliased wasm imports should lower");
    let bytes = wasm_bytes(&module);
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[19.0, 23.0]);
    assert_eq!(wasmtime, 45.0, "Wasmtime");
    assert_eq!(node, 45.0, "Node");
}

#[test]
fn no_arg_and_f32_adapters_round_trip() {
    let mut set = module_set_from_sources([(
        "main.js",
        "import { answer } from './answer.wasm'; \
         import { demote } from './f32.wasm'; \
         export function run() { return answer(); } \
         export function roundtrip(x) { return demote(x); }",
    )])
    .expect("sources");
    set.insert_wasm("answer.wasm", &answer_module())
        .expect("valid Wasm member");
    set.insert_wasm("f32.wasm", &f32_module())
        .expect("valid Wasm member");
    let module = convert("main.js", &set).expect("adapters should lower");
    let bytes = wasm_bytes(&module);
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[]);
    assert_eq!(wasmtime, 42.0, "Wasmtime");
    assert_eq!(node, 42.0, "Node");
    // f32 demote/promote makes the f32 rounding observable through the f64
    // numeric export: 0.1 is not exactly representable in f32.
    let expected = 0.1f32 as f64;
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "roundtrip", &[0.1]);
    assert_eq!(wasmtime, expected, "Wasmtime f32 rounding");
    assert_eq!(node, expected, "Node f32 rounding");
}

#[test]
fn memory_and_data_segments_pass_through() {
    let mut set = module_set_from_sources([(
        "main.js",
        "import { read8 } from './mem.wasm'; export function run(i) { return read8(i); }",
    )])
    .expect("sources");
    set.insert_wasm("mem.wasm", &memory_module())
        .expect("valid Wasm member");
    let module = convert("main.js", &set).expect("memory member should lower");
    let bytes = wasm_bytes(&module);
    // The data segment wrote "hi!" at offset 4.
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[4.0]);
    assert_eq!(wasmtime, b'h' as f64, "Wasmtime");
    assert_eq!(node, b'h' as f64, "Node");
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[6.0]);
    assert_eq!(wasmtime, b'!' as f64, "Wasmtime");
    assert_eq!(node, b'!' as f64, "Node");
}

#[test]
fn raw_reexport_passthrough_keeps_the_binary_signature() {
    let mut set = module_set_from_sources([(
        "main.js",
        "export { add } from './math.wasm'; export { add64 } from './math64.wasm'; \
         export function run(a, b) { return a + b; }",
    )])
    .expect("sources");
    set.insert_wasm("math.wasm", &add_module())
        .expect("valid Wasm member");
    set.insert_wasm("math64.wasm", &add64_module())
        .expect("valid Wasm member");
    let module = convert("main.js", &set).expect("re-exports should lower");
    let bytes = wasm_bytes(&module);

    // Raw i32 signature, exactly as the binary declared it.
    let mut out = [Val::I32(0)];
    execute_raw(&bytes, "add", &[Val::I32(19), Val::I32(23)], &mut out);
    assert!(matches!(out[0], Val::I32(42)), "{:?}", out[0]);
    // Raw i64 signature — no BigInt boxing on the passthrough path.
    let mut out = [Val::I64(0)];
    execute_raw(
        &bytes,
        "add64",
        &[Val::I64(1 << 40), Val::I64(1 << 41)],
        &mut out,
    );
    assert!(
        matches!(out[0], Val::I64(v) if v == (1 << 40) + (1 << 41)),
        "{:?}",
        out[0]
    );
    // The JS export still has its numeric ABI.
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[20.0, 22.0]);
    assert_eq!(wasmtime, 42.0);
    assert_eq!(node, 42.0);
}

#[test]
fn shim_module_reexport_links_through_to_the_wasm_member() {
    let mut set = module_set_from_sources([
        (
            "main.js",
            "import { add } from './shim.js'; \
             export function run(a, b) { return add(a, b); }",
        ),
        ("shim.js", "export { add } from './math.wasm';"),
    ])
    .expect("sources");
    set.insert_wasm("math.wasm", &add_module())
        .expect("valid Wasm member");
    let module = convert("main.js", &set).expect("shim re-export should link");
    let bytes = wasm_bytes(&module);
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[40.0, 2.0]);
    assert_eq!(wasmtime, 42.0, "Wasmtime");
    assert_eq!(node, 42.0, "Node");
}

#[test]
fn multi_result_adapters_pack_into_a_js_array() {
    let mut set = module_set_from_sources([(
        "main.js",
        "import { pair } from './pair.wasm'; \
         export function run() { const p = pair(); return p[0] + p[1] * 10; } \
         export function len() { return pair().length; }",
    )])
    .expect("sources");
    set.insert_wasm_with_profile("pair.wasm", &pair_module(), WasmProfile::MultiValue)
        .expect("multi-value member at the right ceiling");
    let module = convert("main.js", &set).expect("multi-result adapter should lower");
    let bytes = wasm_bytes(&module);
    // pair() = [1, 2]; run() = 1 + 2 * 10 = 21.
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[]);
    assert_eq!(wasmtime, 21.0, "Wasmtime");
    assert_eq!(node, 21.0, "Node");
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "len", &[]);
    assert_eq!(wasmtime, 2.0, "Wasmtime");
    assert_eq!(node, 2.0, "Node");
}

/// `(module (memory i64 1 65537) (data (i64.const 4) "hi!")
///   (func (export "read") (result i32) i64.const 4 i32.load8_u))`
///
/// The declared maximum is 65537 pages — just over 4 GiB, impossible for a
/// 32-bit memory — while behavioral reads stay at offset 4.
fn memory64_module() -> Vec<u8> {
    module(&[
        section(1, vec_of(vec![func_type(&[], &[I32])])),
        section(3, vec_of(vec![uleb(0)])),
        section(
            5,
            vec_of(vec![{
                // limits: flags 0x05 (memory64, with maximum), min 1,
                // max 65537.
                let mut m = vec![0x05, 0x01];
                m.extend(uleb(65537));
                m
            }]),
        ),
        section(7, vec_of(vec![export_entry("read", 0x00, 0)])),
        section(
            10,
            vec_of(vec![code_body(&[0x00, 0x42, 0x04, 0x2d, 0x00, 0x00, 0x0b])]),
        ),
        section(
            11,
            vec_of(vec![{
                // flags 0 (active, memory 0), offset i64.const 4, bytes "hi!"
                let mut d = vec![0x00, 0x42, 0x04, 0x0b];
                d.extend(uleb(3));
                d.extend(b"hi!");
                d
            }]),
        ),
    ])
}

#[test]
fn memory64_members_merge_and_execute() {
    let mut set = module_set_from_sources([(
        "main.js",
        "import { read } from './m64.wasm'; export function run() { return read(); }",
    )])
    .expect("sources");
    set.insert_wasm_with_profile("m64.wasm", &memory64_module(), WasmProfile::Memory64)
        .expect("memory64 member at the right ceiling");
    let module = convert("main.js", &set).expect("memory64 member should merge");
    // The member's memory must survive into the artifact as a 64-bit memory.
    assert!(
        module.memories.values().any(|memory| memory.memory64),
        "the merged memory must keep its memory64 flag"
    );
    let bytes = wasm_bytes(&module);
    // Data segments survive the rebase: offset 4 holds 'h'.
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[]);
    assert_eq!(wasmtime, b'h' as f64, "Wasmtime");
    assert_eq!(node, b'h' as f64, "Node");
}

#[test]
fn gc_members_fail_closed_at_merge() {
    // (type (struct)) — a GC type section entry with no functions.
    let gc = module(&[section(1, vec_of(vec![vec![0x5f, 0x00]]))]);
    let mut set = module_set_from_sources([("main.js", "export function run() { return 1; }")])
        .expect("sources");
    set.insert_wasm_with_profile("g.wasm", &gc, WasmProfile::Gc)
        .expect("gc member at the gc ceiling");
    let error = convert("main.js", &set).expect_err("GC merge is phase 5");
    assert!(error.message.contains("phase 5"), "{error}");
}

#[test]
fn wasm_member_is_reachable_through_relative_directories() {
    let mut set = module_set_from_sources([(
        "src/main.js",
        "import { add } from '../lib/math.wasm'; export function run(a, b) { return add(a, b); }",
    )])
    .expect("sources");
    set.insert_wasm("lib/math.wasm", &add_module())
        .expect("valid Wasm member");
    let module = convert("src/main.js", &set).expect("relative wasm import should link");
    let bytes = wasm_bytes(&module);
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[1.0, 2.0]);
    assert_eq!(wasmtime, 3.0);
    assert_eq!(node, 3.0);
}

/// `(module (memory 1) (memory 1)
///   (data (memory 0) (i32.const 4) "hi!")
///   (data (memory 1) (i32.const 0) "yo")
///   (func (export "read") (param i32 i32) (result i32)
///     local.get 0 i32.load8_u mem=0
///     local.get 1 i32.load8_u mem=1
///     i32.add))`
///
/// Non-aliasing is observable: mem0[4]='h' and mem1[1]='o' only when the
/// two memories and their data segments stay distinct.
fn dual_memory_module() -> Vec<u8> {
    // Multi-memory memarg: align with bit 6 set, then the memory index,
    // THEN the offset (wasmparser reads memidx before offset).
    let memarg = |align: u8, offset: u8, mem: u8| {
        if mem == 0 {
            vec![align, offset]
        } else {
            vec![align | 0x40, mem, offset]
        }
    };
    let data = |mem: u8, offset: u8, bytes: &[u8]| {
        // flags 0x02: active with explicit memory index
        let mut d = vec![0x02, mem, 0x41, offset, 0x0b];
        d.extend(uleb(bytes.len() as u64));
        d.extend(bytes);
        d
    };
    let mut body = vec![0x00, 0x20, 0x00, 0x2d];
    body.extend(memarg(0, 0, 0));
    body.extend([0x20, 0x01, 0x2d]);
    body.extend(memarg(0, 0, 1));
    body.extend([0x6a, 0x0b]);
    module(&[
        section(1, vec_of(vec![func_type(&[I32, I32], &[I32])])),
        section(3, vec_of(vec![uleb(0)])),
        section(5, vec_of(vec![vec![0x00, 0x01], vec![0x00, 0x01]])),
        section(7, vec_of(vec![export_entry("read", 0x00, 0)])),
        section(10, vec_of(vec![code_body(&body)])),
        section(11, vec_of(vec![data(0, 4, b"hi!"), data(1, 0, b"yo")])),
    ])
}

/// Run a numeric export of a CoreGC artifact (pure core Wasm; multi-memory
/// and memory64 enabled, GC off).
fn execute_core(bytes: &[u8], name: &str, args: &[f64]) -> f64 {
    let mut config = Config::new();
    config.wasm_multi_memory(true);
    config.wasm_memory64(true);
    let engine = Engine::new(&config).expect("engine");
    let module = wasmtime::Module::new(&engine, bytes).expect("core module compiles");
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[]).expect("core module instantiates");
    let function = instance
        .get_func(&mut store, name)
        .unwrap_or_else(|| panic!("missing export {name:?}"));
    let inputs: Vec<Val> = args
        .iter()
        .copied()
        .map(|value| Val::F64(value.to_bits()))
        .collect();
    let mut outputs = [Val::F64(0)];
    function
        .call(&mut store, &inputs, &mut outputs)
        .unwrap_or_else(|error| panic!("core call {name:?} failed: {error:#}"));
    match outputs[0] {
        Val::F64(bits) => f64::from_bits(bits),
        ref value => panic!("export returned {value:?}, expected f64"),
    }
}

#[test]
fn multi_memory_members_merge_and_execute_through_coregc() {
    let mut set = module_set_from_sources([(
        "main.js",
        "import { read } from './dual.wasm'; export function run(a, b) { return read(a, b); }",
    )])
    .expect("sources");
    set.insert_wasm_with_profile("dual.wasm", &dual_memory_module(), WasmProfile::MultiMemory)
        .expect("multi-memory member at the right ceiling");
    let module = convert("main.js", &set).expect("multi-memory member should merge");

    // Native WasmGC: mem0[4]='h' + mem1[1]='o' = 215.
    let bytes = wasm_bytes(&module);
    let (wasmtime, node) = execute_numeric_in_all_runtimes(&bytes, "run", &[4.0, 1.0]);
    assert_eq!(wasmtime, 215.0, "Wasmtime");
    assert_eq!(node, 215.0, "Node");

    // CoreGC: source memories are carried after the runtime memory.
    let artifact = portal_jsc_waffle::emit_coregc(&module, &Default::default())
        .expect("coregc lowering accepts member memories");
    assert!(
        artifact.module.memories.len() >= 3,
        "coregc output should carry the runtime memory plus both member memories, got {}",
        artifact.module.memories.len()
    );
    let core_bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core encodes");
    let forced = execute_core(&core_bytes, "run", &[4.0, 1.0]);
    assert_eq!(forced, 215.0, "coregc must match the native result");
}

