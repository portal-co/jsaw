//! Unit coverage for the low-level emitter skeleton (DreamComp #36):
//! minimal known-shape emission on both lanes and fail-closed behavior.

use std::collections::BTreeMap;

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

#[test]
fn trivial_function_emits_on_both_lanes() {
    let plan = plan_for("export function f(a) { return a + 1; }");
    for (lane, module) in [
        (
            "nano",
            portal_jsc_waffle::low_emit::emit_low(
                &plan,
                &portal_jsc_waffle::low_emit::NanoRepresentation,
            ),
        ),
        (
            "coregc",
            portal_jsc_waffle::low_emit::emit_low(
                &plan,
                &portal_jsc_waffle::low_emit::CoreGcRepresentation,
            ),
        ),
    ] {
        let module = module.unwrap_or_else(|d| panic!("{lane} diagnostics: {d:?}"));
        assert_eq!(module.lane, lane);
        assert!(!module.funcs.is_empty(), "{lane}: no functions");
        assert!(
            module.exports.iter().any(|e| e.name == "a" || e.name.len() > 0),
            "{lane}: no exports"
        );
        assert!(
            portal_jsc_waffle::low_emit::debug::validate(&module).is_empty(),
            "{lane}: structural violations"
        );
    }
}

#[test]
fn coregc_declares_header_layouts_and_runtime() {
    let plan = plan_for("export function f(a) { return { x: a }.x; }");
    let module = portal_jsc_waffle::low_emit::emit_low(
        &plan,
        &portal_jsc_waffle::low_emit::CoreGcRepresentation,
    )
    .expect("emit");
    // Layout 0 is the allocation header.
    assert_eq!(module.layouts[0].type_id, 0);
    assert_eq!(module.layouts[0].fields.len(), 4);
    assert!(
        module
            .statics
            .iter()
            .any(|s| s.name == "js_coregc_descriptors"),
        "no descriptor static"
    );
    for name in [
        "js_coregc_alloc",
        "js_coregc_root_enter",
        "js_coregc_root_store",
        "js_coregc_root_pop",
    ] {
        assert!(
            module.funcs.iter().any(|f| f.name == name),
            "missing runtime fn {name}"
        );
    }
}

#[test]
fn nano_declares_arena_runtime() {
    let plan = plan_for("export function f(a) { return a + 1; }");
    let module = portal_jsc_waffle::low_emit::emit_low(
        &plan,
        &portal_jsc_waffle::low_emit::NanoRepresentation,
    )
    .expect("emit");
    assert!(
        module
            .funcs
            .iter()
            .any(|f| f.name == "js_nano_arena_ensure"),
        "nano runtime entry point missing"
    );
}

#[test]
fn tail_call_chain_uses_the_step_protocol() {
    // Mutual recursion through an indirect call forces the trampoline.
    let plan = plan_for(
        "export function f(n) { if (n <= 0) return 0; const g = (m) => f(m - 1); return g(n); }",
    );
    let module = portal_jsc_waffle::low_emit::emit_low(
        &plan,
        &portal_jsc_waffle::low_emit::NanoRepresentation,
    )
    .expect("emit");
    assert!(
        module.funcs.iter().any(|f| f.name.ends_with("$step")),
        "no step functions"
    );
    assert!(
        module.tables.iter().any(|t| t.name == "step"),
        "no step table"
    );
    assert!(
        portal_jsc_waffle::low_emit::debug::validate(&module).is_empty(),
        "trampoline structural violations"
    );
}

#[test]
fn ffi_imports_flow_through_boundary_records_only() {
    // DreamComp #37 acceptance: the low-level emitter consumes only
    // BoundaryRecord/PlanFunctionKind::Import — never source strings.
    let source = "import { add } from 'wasm:env'; export function run(x) { return add(x, 1); }";
    let mut sources = BTreeMap::new();
    sources.insert("main.js".to_string(), source.to_string());
    let set = portal_jsc_waffle::module_set_from_sources(
        sources.iter().map(|(k, v)| (k.as_str(), v.as_str())),
    )
    .expect("ingestion");
    let mut options = portal_jsc_waffle::ConvertOptions::default();
    options.ffi.modules.insert(
        "env".to_string(),
        portal_jsc_waffle::FfiModule {
            functions: [(
                "add".to_string(),
                portal_jsc_waffle::FfiBoundarySignature {
                    params: vec![
                        portal_jsc_waffle::FfiType::I32,
                        portal_jsc_waffle::FfiType::I32,
                    ],
                    result: Some(portal_jsc_waffle::FfiType::I32),
                },
            )]
            .into_iter()
            .collect(),
        },
    );
    options.ffi.exports.insert(
        "run".to_string(),
        portal_jsc_waffle::FfiBoundarySignature {
            params: vec![portal_jsc_waffle::FfiType::I32],
            result: Some(portal_jsc_waffle::FfiType::I32),
        },
    );
    let mut module = portal_pc_waffle::Module::empty();
    portal_jsc_waffle::convert_modules("main.js", &set, &mut module, &options).expect("lowering");
    let plan = portal_jsc_waffle::BackendPlan::from_module(&module).expect("plan");
    let low = portal_jsc_waffle::low_emit::emit_low(
        &plan,
        &portal_jsc_waffle::low_emit::NanoRepresentation,
    )
    .expect("emit");
    assert!(
        low.imports
            .iter()
            .any(|i| i.module == "env" && i.name == "add"
                && i.params == [portal_jsc_waffle::low_emit::LeType::I32; 2]
                && i.results == [portal_jsc_waffle::low_emit::LeType::I32]),
        "structured import missing: {:?}",
        low.imports
    );
    assert!(
        low.exports.iter().any(|e| e.name == "run"),
        "boundary export missing: {:?}",
        low.exports
    );
    assert!(portal_jsc_waffle::low_emit::debug::validate(&low).is_empty());
}
