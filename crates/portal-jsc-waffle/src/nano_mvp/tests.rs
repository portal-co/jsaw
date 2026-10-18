use super::*;

use portal_pc_waffle::Module;
use crate::{BACKEND_PLAN_SCHEMA, PlanBlockId};

/// Lower JS to a BackendPlan (through the ordinary WasmGC converter), emit
/// the nano MVP module, validate it as pure core Wasm (GC off), and execute
/// the named export under wasmtime.
fn compile_plan_and_run(source: &str, export: &str, args: &[f64]) -> f64 {
    let set = crate::module_set_from_sources([("main.js", source)]).unwrap();
    let mut module = Module::empty();
    crate::convert_modules(
        "main.js",
        &set,
        &mut module,
        &crate::ConvertOptions::default(),
    )
    .unwrap();
    let plan = BackendPlan::from_module(&module).unwrap_or_else(|d| panic!("plan: {d:?}"));
    let bytes = emit_nano_mvp(&plan).unwrap_or_else(|d| panic!("emit: {d:?}"));

    // Pure core Wasm: must validate with GC and tail-call features off... GC
    // off at least; tail calls are core Wasm since the proposal merged.
    let mut features = wasmparser::WasmFeatures::default();
    features.remove(wasmparser::WasmFeatures::GC);
    features.insert(wasmparser::WasmFeatures::TAIL_CALL);
    features.insert(wasmparser::WasmFeatures::MULTI_VALUE);
    wasmparser::Validator::new_with_features(features)
        .validate_all(&bytes)
        .expect("nano MVP module must validate as core Wasm");

    let mut config = wasmtime::Config::new();
    config.wasm_tail_call(true);
    config.wasm_multi_memory(false);
    let engine = wasmtime::Engine::new(&config).expect("engine");
    let module = wasmtime::Module::new(&engine, &bytes).expect("parse");
    let mut store = wasmtime::Store::new(&engine, ());
    let linker: wasmtime::Linker<()> = wasmtime::Linker::new(&engine);
    let instance = linker.instantiate(&mut store, &module).expect("instantiate");
    let func = instance
        .get_func(&mut store, export)
        .unwrap_or_else(|| panic!("export {export:?}"));
    let params: Vec<wasmtime::Val> =
        args.iter().map(|a| wasmtime::Val::F64(a.to_bits())).collect();
    let mut results = vec![wasmtime::Val::F64(0)];
    func.call(&mut store, &params, &mut results).expect("call");
    match results[0] {
        wasmtime::Val::F64(bits) => f64::from_bits(bits),
        ref other => panic!("expected f64, got {other:?}"),
    }
}

fn plan_of(source: &str) -> BackendPlan {
    let set = crate::module_set_from_sources([("main.js", source)]).unwrap();
    let mut module = Module::empty();
    crate::convert_modules(
        "main.js",
        &set,
        &mut module,
        &crate::ConvertOptions::default(),
    )
    .unwrap();
    BackendPlan::from_module(&module).unwrap_or_else(|d| panic!("plan: {d:?}"))
}

#[test]
fn scalar_arithmetic_executes() {
    assert_eq!(
        compile_plan_and_run("export function add(a, b) { return a + b; }", "add", &[3.0, 4.0]),
        7.0
    );
    assert_eq!(
        compile_plan_and_run("export function sub(a, b) { return a - b; }", "sub", &[10.0, 3.0]),
        7.0
    );
}

#[test]
fn conditional_and_loop_execute() {
    assert_eq!(
        compile_plan_and_run("export function t(c) { return c ? 1 : 2; }", "t", &[1.0]),
        1.0
    );
    assert_eq!(
        compile_plan_and_run("export function t(c) { return c ? 1 : 2; }", "t", &[0.0]),
        2.0
    );
    assert_eq!(
        compile_plan_and_run(
            "export function sum(n) { let t = 0; for (let i = 0; i < n; i++) { t += i; } return t; }",
            "sum",
            &[5.0]
        ),
        10.0
    );
}

#[test]
fn non_tail_and_tail_calls_execute() {
    assert_eq!(
        compile_plan_and_run(
            "function addOne(x) { return x + 1; }\n\
             export function combine(a, b) { return addOne(a) + addOne(b); }",
            "combine",
            &[3.0, 4.0]
        ),
        9.0
    );
    assert_eq!(
        compile_plan_and_run(
            "export function count(n) { let f = function(k, acc) { if (k <= 0) { return acc; } return f(k - 1, acc + k); }; return f(n, 0); }",
            "count",
            &[5.0]
        ),
        15.0
    );
}

#[test]
fn object_fields_execute() {
    assert_eq!(
        compile_plan_and_run(
            "export function makeAndSum(x, y) { const pt = {x: x, y: y}; return pt.x + pt.y; }",
            "makeAndSum",
            &[3.0, 4.0]
        ),
        7.0
    );
}

#[test]
fn bytes_and_fingerprint_are_deterministic() {
    let source = "export function add(a, b) { return a + b; }";
    let a = emit_nano_mvp(&plan_of(source)).unwrap();
    let b = emit_nano_mvp(&plan_of(source)).unwrap();
    assert_eq!(a, b);
    assert_eq!(emit_nano_mvp_fingerprint(&a), emit_nano_mvp_fingerprint(&b));
}

/// Build a minimal hand-constructed plan with one unsupported instruction.
fn minimal_plan_with_op(op: &str) -> BackendPlan {
    BackendPlan {
        schema: BACKEND_PLAN_SCHEMA.into(),
        types: vec![PlanType::I32],
        signatures: vec![PlanSignature::Func {
            params: vec![],
            returns: vec![crate::backend_plan::TypeId(0)],
            shared: false,
        }],
        tables: vec![],
        globals: vec![],
        memories: vec![],
        functions: vec![PlanFunction {
            name: "f".into(),
            kind: crate::backend_plan::PlanFunctionKind::Defined,
            signature: PlanSigId(0),
            entry: Some(PlanBlockId(0)),
            blocks: vec![crate::backend_plan::PlanBlock {
                params: vec![],
                insts: vec![PlanInst {
                    operator: op.into(),
                    args: vec![],
                    results: vec![PlanValueId(0)],
                    result_types: vec![crate::backend_plan::TypeId(0)],
                    call: None,
                    arena: None,
                    const_value: None,
                    ref_func: None,
                    payload: Default::default(),
                }],
                terminator: PlanTerminator::Return { values: vec![PlanValueId(0)] },
            }],
            values: vec![crate::backend_plan::PlanValue {
                ty: Some(crate::backend_plan::TypeId(0)),
                def: PlanValueDef::Operator {
                    block: PlanBlockId(0),
                    inst: 0,
                },
                representation: crate::NanoValueRepresentation::Scalar,
                lifetime: crate::NanoLifetime::Call,
                layout: crate::backend_plan::PlanLayout::Scalar { bytes: 4 },
            }],
            completions: crate::backend_plan::CompletionShape {
                kinds: vec![crate::backend_plan::CompletionKind::Return],
                results: vec![crate::backend_plan::TypeId(0)],
            },
        }],
        boundaries: vec![],
        runtime: Default::default(),
        start: None,
    }
}

#[test]
fn instrumentation_tracks_allocations() {
    let plan = plan_of(
        "export function makeAndSum(x, y) { const pt = {x: x, y: y}; return pt.x + pt.y; }",
    );
    let bytes = emit_nano_mvp_with_config(
        &plan,
        &NanoMvpConfig {
            instrument_alloc: true,
            max_pages: None,
        },
    )
    .unwrap();
    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &bytes).expect("parse");
    let mut store = wasmtime::Store::new(&engine, ());
    let linker: wasmtime::Linker<()> = wasmtime::Linker::new(&engine);
    let instance = linker.instantiate(&mut store, &module).expect("instantiate");
    let func = instance.get_func(&mut store, "makeAndSum").expect("export");
    let params = vec![wasmtime::Val::F64(3.0f64.to_bits()), wasmtime::Val::F64(4.0f64.to_bits())];
    let mut results = vec![wasmtime::Val::F64(0)];
    func.call(&mut store, &params, &mut results).expect("call");

    let count = match instance.get_global(&mut store, "js_nano_alloc_count") {
        Some(g) => match g.get(&mut store) {
            wasmtime::Val::I64(v) => v,
            ref other => panic!("alloc count global: {other:?}"),
        },
        None => panic!("js_nano_alloc_count not exported"),
    };
    let alloc_bytes = match instance.get_global(&mut store, "js_nano_alloc_bytes") {
        Some(g) => match g.get(&mut store) {
            wasmtime::Val::I64(v) => v,
            ref other => panic!("alloc bytes global: {other:?}"),
        },
        None => panic!("js_nano_alloc_bytes not exported"),
    };
    let cursor = match instance.get_global(&mut store, "js_nano_arena_cursor") {
        Some(g) => match g.get(&mut store) {
            wasmtime::Val::I32(v) => v as i64,
            ref other => panic!("arena cursor global: {other:?}"),
        },
        None => panic!("js_nano_arena_cursor not exported"),
    };
    assert!(count > 0, "allocating program must count allocations");
    assert!(alloc_bytes > 0, "allocating program must count bytes");
    assert!(cursor > ARENA_BASE as i64, "cursor must advance past the base");
    assert!(
        alloc_bytes <= cursor - ARENA_BASE as i64,
        "counted bytes ({alloc_bytes}) cannot exceed used arena ({})",
        cursor - ARENA_BASE as i64
    );
}

#[test]
fn arena_exhaustion_traps_deterministically() {
    // One-page arena; the loop allocates far more than 64KiB, so the
    // allocator's memory.grow fails against the cap and traps rather than
    // silently corrupting.
    let plan = plan_of(
        "export function churn(n) { let last = null; for (let i = 0; i < n; i++) { last = {v: i}; } return last === null ? 0 : 1; }",
    );
    let bytes = emit_nano_mvp_with_config(
        &plan,
        &NanoMvpConfig {
            instrument_alloc: true,
            max_pages: Some(1),
        },
    )
    .unwrap();
    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::new(&engine, &bytes).expect("parse");
    let mut store = wasmtime::Store::new(&engine, ());
    let linker: wasmtime::Linker<()> = wasmtime::Linker::new(&engine);
    let instance = linker.instantiate(&mut store, &module).expect("instantiate");
    let func = instance.get_func(&mut store, "churn").expect("export");
    let params = vec![wasmtime::Val::F64(100000.0f64.to_bits())];
    let mut results = vec![wasmtime::Val::F64(0)];
    let outcome = func.call(&mut store, &params, &mut results);
    assert!(
        outcome.is_err(),
        "exhausting a 1-page arena must trap, not silently succeed"
    );
}

#[test]
fn unsupported_operators_fail_closed() {
    let errors = emit_nano_mvp(&minimal_plan_with_op("V128Load"))
        .expect_err("a SIMD operator must not emit");
    assert!(
        errors.iter().any(|e| e.kind == "unsupported_operator"),
        "diagnostics: {errors:?}"
    );

    // The same plan with a supported constant instead emits cleanly.
    let mut plan = minimal_plan_with_op("I32Const");
    plan.functions[0].blocks[0].insts[0].const_value = Some(PlanConst::I32(42));
    emit_nano_mvp(&plan).expect("a constant plan must emit");
}

#[test]
fn classes_smoke() {
    assert_eq!(
        compile_plan_and_run(
            "export function make(x, y) { class P { constructor(x, y) { this.x = x; this.y = y; } sum() { return this.x + this.y; } } return new P(x, y).sum(); }",
            "make",
            &[3.0, 4.0]
        ),
        7.0
    );
}

#[test]
fn class_inheritance_and_super_execute() {
    // extends, super() construction, and super.method() dispatch through the
    // prototype chain.
    assert_eq!(
        compile_plan_and_run(
            "export function f(a, b) { class A { constructor(x) { this.x = x; } get() { return this.x; } } class B extends A { constructor(x, y) { super(x); this.y = y; } get() { return super.get() + this.y; } } return new B(a, b).get(); }",
            "f",
            &[3.0, 4.0]
        ),
        7.0
    );
}

#[test]
fn class_statics_instanceof_and_accessors_execute() {
    // Static methods (inherited through the constructor's [[Prototype]]),
    // instanceof over the chain, and getter/setter descriptors on the
    // prototype.
    assert_eq!(
        compile_plan_and_run(
            "export function f(a, b) { class A { static twice(x) { return x + x; } get v() { return this.backing; } set v(x) { this.backing = x + 1; } } class B extends A {} const p = new B(); p.v = a; return B.twice(a) + p.v + (p instanceof A ? 1 : 0); }",
            "f",
            &[3.0, 4.0]
        ),
        11.0
    );
}

#[test]
fn private_fields_and_brand_checks_execute() {
    // Instance private fields with per-instance storage and `#x in obj`
    // brand checks (same class true, foreign object and other class's
    // same-named private false).
    assert_eq!(
        compile_plan_and_run(
            "export function f(a, b) { class A { #x; constructor(v) { this.#x = v; } get() { return this.#x; } static has(o) { return #x in o; } } class B { #x = 100; } return new A(a).get() + new A(b).get() + (A.has(new A(0)) ? 1 : 0) + (A.has(new B()) ? 50 : 0); }",
            "f",
            &[3.0, 4.0]
        ),
        8.0
    );
}

#[test]
fn private_methods_accessors_and_statics_execute() {
    // Private method calls, a private getter/setter pair, and a static
    // private field mutated through a static method.
    assert_eq!(
        compile_plan_and_run(
            "export function f(a, b) { class C { static #count = 0; #v = 0; static bump(v) { this.#count += v; return this.#count; } get val() { return this.#v; } set val(x) { this.#v = x; } #twice(v) { return v * 2; } run(v) { return this.#twice(v); } } const c = new C(); c.val = a; return c.run(c.val) + C.bump(b); }",
            "f",
            &[3.0, 4.0]
        ),
        10.0
    );
}

#[test]
fn proxy_traps_execute_nano() {
    // get/set/has traps plus forwarding through the nano lane.
    assert_eq!(
        compile_plan_and_run(
            "export function f(a, b) { const p = new Proxy({ y: b }, { get(t, k) { if (k === \"x\") { return a; } return t.y; }, set(t, k, v) { t.z = v + 1; } }); p.q = a; return p.x + p.y + p.z; }",
            "f",
            &[3.0, 4.0]
        ),
        11.0
    );
}

#[test]
fn proxy_apply_and_construct_execute_nano() {
    assert_eq!(
        compile_plan_and_run(
            "export function f(a, b) { const p = new Proxy(function(x) { return x * 2; }, { apply(t, thisArg, args) { return t(args[0]) + 1; }, construct(t, args) { return { x: args[0] * 10 }; } }); return p(a) + new p(b).x; }",
            "f",
            &[3.0, 4.0]
        ),
        47.0
    );
}

#[test]
fn reflect_trap_mirror_executes_nano() {
    // Reflect.get/set/has mirror the proxy traps through the nano lane.
    assert_eq!(
        compile_plan_and_run(
            "export function f(a, b) { const t = { y: b }; const p = new Proxy(t, { get(tt, k) { if (k === \"x\") { return a; } return tt.y; }, has(tt, k) { return k === \"w\"; } }); return Reflect.get(p, \"x\") + Reflect.get(p, \"y\") + (Reflect.has(p, \"w\") ? 1 : 0); }",
            "f",
            &[3.0, 4.0]
        ),
        8.0
    );
}

#[test]
fn reflect_apply_construct_execute_nano() {
    assert_eq!(
        compile_plan_and_run(
            "export function f(a, b) { function P(x) { this.x = x; } P.prototype = { get() { return this.x; } }; const p = new Proxy(function(x) { return x * 2; }, { apply(t, thisArg, args) { return t(args[0]) + 1; } }); return Reflect.apply(p, null, [a]) + Reflect.construct(P, [b]).get(); }",
            "f",
            &[3.0, 4.0]
        ),
        11.0
    );
}
