//! Smoke test for the MiniBC backend (DreamComp #39): emit corpus programs
//! through both lanes, execute under the stock `MiniInterp` with the jsaw
//! host-op table, and compare bit-exactly.

use std::collections::BTreeMap;



use dreamcompiler_minibc::interp::MiniInterp;
use portal_jsc_waffle::low_emit;

fn run_case(source: &str, lane: &str, export: &str, args: &[f64]) -> f64 {
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
    let plan = portal_jsc_waffle::BackendPlan::from_module(&module).expect("plan");
    let low = match lane {
        "nano" => low_emit::emit_low(&plan, &low_emit::NanoRepresentation),
        _ => low_emit::emit_low(&plan, &low_emit::CoreGcRepresentation),
    }
    .expect("emit_low");
    let artifact = low_emit::minibc_render::render_minibc(&low)
        .map_err(|ds| ds.iter().map(|d| d.to_string()).collect::<Vec<_>>().join("\n"))
        .expect("render_minibc");
    let entry = artifact.exports[export];
    if let Some(start) = artifact.start {
        let mut interp = MiniInterp::new(artifact.store_regs.max(64));
        let host = low_emit::minibc_host::JsawHost::new(&artifact, &low);
        host.install(&mut interp);
        interp
            .run(&artifact.bytes, start as usize)
            .expect("start runs");
    }
    let mut interp = MiniInterp::new(artifact.store_regs.max(64));
    let host = low_emit::minibc_host::JsawHost::new(&artifact, &low);
    host.install(&mut interp);
    for (i, a) in args.iter().enumerate() {
        interp.set_r4(i * 4, a.to_bits());
    }
    interp
        .run(&artifact.bytes, entry as usize)
        .expect("run");
    f64::from_bits(interp.get_r4(0))
}

#[test]
#[ignore = "DreamComp #39 in progress: emitter emits and runs but miscompiles the trampoline/call protocol (see issue)"]
fn minibc_smoke() {
    for lane in ["nano", "coregc"] {
        assert_eq!(run_case("export function run(x) { return x + 1; }", lane, "run", &[10.0]), 11.0, "{lane}");
        assert_eq!(
            run_case(
                "export function run(x) { let s = 0; for (let i = 0; i < x; i++) s += i; return s; }",
                lane,
                "run",
                &[10.0]
            ),
            45.0,
            "{lane}"
        );
        eprintln!("{lane}: smoke OK");
    }
}
