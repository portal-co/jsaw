//! Milestone 7 gate: the skeleton Java emission compiles with `javac`.
//!
//! Uses the smallest module-mode fixture. When no JDK is available the test
//! reports the prerequisite and skips rather than claiming coverage.

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

/// Locate a working `javac`: $JAVAC, $JAVA_HOME/bin/javac, PATH, then the
/// Homebrew openjdk keg. Returns None when no JDK is installed.
fn javac() -> Option<PathBuf> {
    let mut candidates: Vec<PathBuf> = Vec::new();
    if let Ok(explicit) = std::env::var("JAVAC") {
        candidates.push(PathBuf::from(explicit));
    }
    if let Ok(home) = std::env::var("JAVA_HOME") {
        candidates.push(PathBuf::from(home).join("bin/javac"));
    }
    candidates.push(PathBuf::from("javac"));
    candidates.push(PathBuf::from("/opt/homebrew/opt/openjdk/bin/javac"));
    candidates.into_iter().find(|candidate| {
        Command::new(candidate)
            .arg("-version")
            .output()
            .is_ok_and(|out| out.status.success())
    })
}

#[test]
fn skeleton_emission_compiles_with_javac() {
    let Some(javac) = javac() else {
        eprintln!(
            "skipping javac gate: no working JDK found \
             (set JAVAC or JAVA_HOME, or install openjdk)"
        );
        return;
    };
    let module = compile_module("export function run(a) { return a + 1; }");
    let sources = portal_jsc_jvm_emit::emit_java(&module).expect("Java emission should succeed");
    let dir = std::env::temp_dir().join(format!("jvm_emit_gate_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("temp dir should be creatable");
    let mut files = Vec::new();
    for (path, content) in &sources.files {
        let full = dir.join(path);
        std::fs::create_dir_all(full.parent().unwrap()).expect("package dirs should be creatable");
        std::fs::write(&full, content).expect("source file should be writable");
        files.push(full);
    }
    let status = Command::new(javac)
        .arg("-d")
        .arg(&dir)
        .args(&files)
        .status()
        .expect("javac should run");
    assert!(
        status.success(),
        "javac should compile the skeleton emission; sources:\n{}",
        sources.concatenated()
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// The emitted sources must include the struct classes, funcref
/// interfaces, runtime, and `Mod`.
#[test]
fn skeleton_emission_shape() {
    let module = compile_module("export function run(a) { return a + 1; }");
    let sources = portal_jsc_jvm_emit::emit_java(&module).expect("Java emission should succeed");
    let names: Vec<&str> = sources.files.keys().map(|s| s.as_str()).collect();
    assert!(names.iter().any(|n| n.ends_with("/Mod.java")), "{names:?}");
    assert!(names.iter().any(|n| n.ends_with("/W.java")), "{names:?}");
    assert!(
        names
            .iter()
            .any(|n| n.contains("/S") && n.ends_with(".java")),
        "struct classes should be emitted: {names:?}"
    );
    assert!(
        names
            .iter()
            .any(|n| n.contains("/I") && n.ends_with(".java")),
        "funcref interfaces should be emitted: {names:?}"
    );
    let mod_src = &sources.files["pc/portal/mob/Mod.java"];
    assert!(
        mod_src.contains("public static double run(double a0)"),
        "{mod_src}"
    );
    // Milestone 8: bodies are real SIR renderings, not stubs.
    assert!(!mod_src.contains("not yet emitted"), "{mod_src}");
}
