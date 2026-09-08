//! Milestone 7 gate: the skeleton Swift emission typechecks with `swiftc`.
//!
//! Uses the smallest module-mode fixture. When no Swift toolchain is
//! available the test reports the prerequisite and skips rather than
//! claiming coverage.

use std::path::PathBuf;
use std::process::Command;

use portal_jsc_swc_cfg::module::CfgModule;
use portal_jsc_swc_ssa::module::SModule;
use portal_jsc_swc_tac::module::TModule;
use portal_pc_waffle::Module;
use swc_common::{FileName, GLOBALS, Globals, SourceMap, sync::Lrc};
use swc_ecma_ast::EsVersion;
use swc_ecma_parser::{EsSyntax, Syntax, parse_file_as_module};

fn compile_module(source: &str) -> Module<'static> {
    GLOBALS.set(&Globals::default(), || {
        let cm: Lrc<SourceMap> = Lrc::new(SourceMap::default());
        let file = cm.new_source_file(
            Lrc::new(FileName::Custom("fixture.mjs".into())),
            source.to_owned(),
        );
        let mut errors = vec![];
        let source = parse_file_as_module(
            &file,
            Syntax::Es(EsSyntax::default()),
            EsVersion::Es2022,
            None,
            &mut errors,
        )
        .expect("module fixture should parse");
        assert!(errors.is_empty(), "parser diagnostics: {errors:?}");
        let cfg = CfgModule::try_from(source).expect("CFG lowering should succeed");
        let tac = TModule::try_from(cfg).expect("TAC lowering should succeed");
        let ssa = SModule::try_from(tac).expect("SSA lowering should succeed");
        let mut wasm = Module::empty();
        portal_jsc_waffle::convert_module(
            &ssa,
            &mut wasm,
            &portal_jsc_waffle::ConvertOptions::default(),
        )
        .expect("WasmGC module lowering should succeed");
        wasm
    })
}

/// Locate a working `swiftc`: $SWIFTC, then PATH.
fn swiftc() -> Option<PathBuf> {
    let mut candidates: Vec<PathBuf> = Vec::new();
    if let Ok(explicit) = std::env::var("SWIFTC") {
        candidates.push(PathBuf::from(explicit));
    }
    candidates.push(PathBuf::from("swiftc"));
    candidates.push(PathBuf::from("/usr/bin/swiftc"));
    candidates.into_iter().find(|candidate| {
        Command::new(candidate)
            .arg("-version")
            .output()
            .is_ok_and(|out| out.status.success())
    })
}

#[test]
fn skeleton_emission_typechecks_with_swiftc() {
    let Some(swiftc) = swiftc() else {
        eprintln!(
            "skipping swiftc gate: no working Swift toolchain found \
             (set SWIFTC, or install Xcode command line tools)"
        );
        return;
    };
    let module = compile_module("export function run(a) { return a + 1; }");
    let sources = portal_jsc_swift_emit::emit_swift(&module).expect("Swift emission should succeed");
    let dir = std::env::temp_dir().join(format!("swift_emit_gate_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("temp dir should be creatable");
    let mut files = Vec::new();
    for (path, content) in &sources.files {
        let full = dir.join(path);
        std::fs::write(&full, content).expect("source file should be writable");
        files.push(full);
    }
    let status = Command::new(swiftc)
        .arg("-typecheck")
        .args(&files)
        .status()
        .expect("swiftc should run");
    assert!(
        status.success(),
        "swiftc should typecheck the skeleton emission; sources:\n{}",
        sources.concatenated()
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// The emitted sources must include the struct classes, funcref boxes,
/// runtime, and `Mod`.
#[test]
fn skeleton_emission_shape() {
    let module = compile_module("export function run(a) { return a + 1; }");
    let sources = portal_jsc_swift_emit::emit_swift(&module).expect("Swift emission should succeed");
    let names: Vec<&str> = sources.files.keys().map(|s| s.as_str()).collect();
    assert!(names.iter().any(|n| *n == "Mod.swift"), "{names:?}");
    assert!(names.iter().any(|n| *n == "Runtime.swift"), "{names:?}");
    assert!(
        names.iter().any(|n| n.starts_with("S") && n.ends_with(".swift")),
        "struct classes should be emitted: {names:?}"
    );
    assert!(
        names.iter().any(|n| n.starts_with("Fn") && n.ends_with(".swift")),
        "funcref boxes should be emitted: {names:?}"
    );
    let mod_src = &sources.files["Mod.swift"];
    assert!(
        mod_src.contains("public static func run(_ a0: Double) -> Double"),
        "{mod_src}"
    );
    // Milestone 10: bodies are real SIR renderings, not stubs.
    assert!(!mod_src.contains("not yet emitted"), "{mod_src}");
}
