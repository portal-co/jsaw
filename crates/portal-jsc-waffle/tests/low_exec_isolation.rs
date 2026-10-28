//! Per-case failure isolation for the low-level lanes (DreamComp #41,
//! acceptance #2): a case whose artifact cannot build (an undeclared-at-link
//! host import) is recorded fail-closed with its diagnostic, and the sibling
//! case is unaffected. With per-case compilation, this *is* the batch
//! bisection outcome — the offending case is identified directly.

use std::collections::BTreeMap;

use portal_jsc_waffle::low_emit::exec::{LowLane, run_low_case};

fn plan_for(source: &str, options: &portal_jsc_waffle::ConvertOptions) -> portal_jsc_waffle::BackendPlan {
    let mut sources = BTreeMap::new();
    sources.insert("main.js".to_string(), source.to_string());
    let set = portal_jsc_waffle::module_set_from_sources(
        sources.iter().map(|(k, v)| (k.as_str(), v.as_str())),
    )
    .expect("ingestion");
    let mut module = portal_pc_waffle::Module::empty();
    portal_jsc_waffle::convert_modules("main.js", &set, &mut module, options).expect("lowering");
    portal_jsc_waffle::BackendPlan::from_module(&module).expect("plan")
}

#[test]
fn broken_case_isolated_and_recorded() {
    assert!(
        LowLane::CNano.toolchain_available(),
        "prerequisite: no C compiler (fail-closed per AGENTS.md)"
    );
    let work = std::env::temp_dir().join(format!("jsaw_isolation_{}", std::process::id()));
    std::fs::create_dir_all(&work).unwrap();

    // The healthy sibling.
    let good_plan = plan_for(
        "export function run() { return 7; }",
        &portal_jsc_waffle::ConvertOptions::default(),
    );
    let gdir = work.join("good");
    std::fs::create_dir_all(&gdir).unwrap();
    let good = run_low_case(&good_plan, LowLane::CNano, "run", &[], 7.0, &gdir);

    // The broken case: a manifest-declared host import whose symbol no host
    // provides, so the artifact cannot link.
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
    let broken_plan = plan_for(
        "import { add } from 'wasm:env'; export function run(x) { return add(x, 1); }",
        &options,
    );
    let bdir = work.join("broken");
    std::fs::create_dir_all(&bdir).unwrap();
    let broken = run_low_case(&broken_plan, LowLane::CNano, "run", &[1.0], 2.0, &bdir);

    assert_eq!(good.status, "pass", "healthy case must pass: {}", good.detail);
    assert_eq!(broken.status, "fail", "broken case must fail closed");
    assert!(
        broken.detail.contains("compile:"),
        "broken case must record the compile diagnostic: {}",
        broken.detail
    );
    let _ = std::fs::remove_dir_all(&work);
}
