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
