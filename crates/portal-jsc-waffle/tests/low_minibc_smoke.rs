//! Smoke test for the MiniBC backend (DreamComp #39): emit corpus programs
//! through both lanes, execute under the stock `MiniInterp` with the jsaw
//! host-op table, and compare bit-exactly.

use std::collections::BTreeMap;

struct StderrLog;
impl log::Log for StderrLog {
    fn enabled(&self, _: &log::Metadata) -> bool {
        true
    }
    fn log(&self, r: &log::Record) {
        eprintln!("{}", r.args());
    }
    fn flush(&self) {}
}



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
    if std::env::var("MBFUNCS").is_ok() {
        let mut f: Vec<&(String, u32)> = artifact.func_entries.iter().collect();
        f.sort_by_key(|(_, p)| *p);
        for (n, p) in f {
            eprintln!("func {p}: {n}");
        }
    }
    if let Ok(range) = std::env::var("MBRANGE") {
        let (a, b) = range.split_once(':').unwrap();
        let (a, b) = (a.parse::<usize>().unwrap(), b.parse::<usize>().unwrap());
        eprintln!("range {a}..{b}: {:02x?}", &artifact.bytes[a..b]);
    }
    if let Ok(pc) = std::env::var("MBPC") {
        let pc: u32 = pc.parse().unwrap();
        let mut cand: Vec<&(String, u32)> = artifact
            .func_entries
            .iter()
            .filter(|(_, p)| *p <= pc)
            .collect();
        cand.sort_by_key(|(_, p)| *p);
        eprintln!("pc {pc} is in function {:?}", cand.last());
    }
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
    if std::env::var("MBTRACE").is_ok() {
        let _ = log::set_logger(&StderrLog);
        log::set_max_level(log::LevelFilter::Trace);
        interp.set_trace(true);
    }
    for (i, a) in args.iter().enumerate() {
        interp.set_r4(i * 4, a.to_bits());
    }
    if let Err(e) = interp.run(&artifact.bytes, entry as usize) {
        let pc = match &e {
            dreamcompiler_minibc::interp::InterpError::Trapped { pc, .. } => Some(*pc),
            dreamcompiler_minibc::interp::InterpError::UnknownOpcode { pc, .. } => Some(*pc),
            dreamcompiler_minibc::interp::InterpError::MalformedBytecode { pc, .. } => {
                Some(*pc)
            }
            _ => None,
        };
        if let Some(pc) = pc {
            let p = pc as usize;
            eprintln!("err {e:?}");
            eprintln!("bytes @pc: {:02x?}", &artifact.bytes[p..(p + 24).min(artifact.bytes.len())]);
            let mut before: Vec<&(String, u32)> = artifact
                .func_entries
                .iter()
                .filter(|(_, q)| *q <= pc as u32)
                .collect();
            before.sort_by_key(|(_, q)| *q);
            eprintln!("in function: {:?}", before.last());
        }
        panic!("run failed");
    }
    f64::from_bits(interp.get_r4(0))
}

#[test]
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
        assert_eq!(
            run_case(
                "import { struct, contains } from '~ffi'; export function run(a) { const Bytes = struct('extern data: i32, len: i32'); const v = { x: a }; const T = contains(v); const S = struct(`extern header: i32, payload: ${T}, bytes: ${Bytes}`); const s = S(10, v, Bytes(2, 3)); return s.header + s.payload.x + s.bytes.len; }",
                lane,
                "run",
                &[5.0]
            ),
            18.0,
            "{lane}"
        );
        eprintln!("{lane}: smoke OK");
    }
}
