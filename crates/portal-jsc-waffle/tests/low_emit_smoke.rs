//! Smoke consumer for the low-level emitter skeleton (DreamComp #36):
//! proves both representation implementations cover the corpus operator
//! surface and that emission is deterministic.

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
fn corpus_emits_on_both_lanes_deterministically() {
    let corpus_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../jsaw-wasi-bin/tests/corpus/jsaw_corpus.json");
    let corpus: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&corpus_path).expect("corpus file"))
            .expect("corpus parses");

    let mut emitted: BTreeMap<String, usize> = BTreeMap::new();
    let mut diags_out: Vec<String> = Vec::new();
    for case in corpus["cases"].as_array().expect("cases") {
        let name = case["name"].as_str().expect("name").to_string();
        let module_src = case["module"].as_str().expect("module");
        if module_src.contains("~Natives_") {
            continue;
        }
        let plan = plan_for(module_src);
        for (lane, render) in [
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
            match render {
                Ok(module) => {
                    let violations = portal_jsc_waffle::low_emit::debug::validate(&module);
                    assert!(
                        violations.is_empty(),
                        "{name}/{lane}: structural violations: {violations:?}"
                    );
                    let a = portal_jsc_waffle::low_emit::debug::render_debug(&module);
                    // Determinism: emit again from the same plan.
                    let module2 = match lane {
                        "nano" => portal_jsc_waffle::low_emit::emit_low(
                            &plan,
                            &portal_jsc_waffle::low_emit::NanoRepresentation,
                        ),
                        _ => portal_jsc_waffle::low_emit::emit_low(
                            &plan,
                            &portal_jsc_waffle::low_emit::CoreGcRepresentation,
                        ),
                    }
                    .expect("re-emit");
                    let b = portal_jsc_waffle::low_emit::debug::render_debug(&module2);
                    assert_eq!(a, b, "nondeterministic emission for {name}/{lane}");
                    *emitted.entry(lane.to_string()).or_default() += 1;
                }
                Err(ds) => {
                    for d in &ds {
                        diags_out.push(format!("{name}/{lane}: {d}"));
                    }
                }
            }
        }
    }
    for d in &diags_out {
        eprintln!("{d}");
    }
    assert!(
        diags_out.is_empty(),
        "{} corpus diagnostics (first above)",
        diags_out.len()
    );
    assert_eq!(
        emitted.get("nano").copied().unwrap_or(0),
        emitted.get("coregc").copied().unwrap_or(0),
        "lane coverage mismatch: {emitted:?}"
    );
    assert!(
        emitted.get("nano").copied().unwrap_or(0) >= 40,
        "expected most corpus families to emit: {emitted:?}"
    );
    println!("emitted: {emitted:?}");
}
