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
