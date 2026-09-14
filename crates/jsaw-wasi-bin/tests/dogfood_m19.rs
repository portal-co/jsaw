//! Milestone 19: lower the real Stage-B compiler through the bounded-memory
//! jsaw-core module ingestion path.
//!
//! This test deliberately stops at Stage C. Java execution remains gated by
//! the independently tracked JVM 64 KiB method limit; the assertion here is
//! that the real, optimized Stage-B module is accepted as one closed module
//! set without retaining whole-module AST/CFG/TAC copies.

use std::{path::PathBuf, time::Instant};

use portal_jsc_waffle::{ConvertOptions, convert_modules, module_set_from_sources_lazy};

fn stage_b_compiler_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../target/dogfood-m15/compiler.js")
}

fn stage_b_sources() -> (String, String) {
    let compiler_path = stage_b_compiler_path();
    let compiler = std::fs::read_to_string(&compiler_path).unwrap_or_else(|error| {
        panic!(
            "Stage-B compiler JS is missing at {} ({error}); run \
             `cargo test -p jsaw-wasi-bin --test dogfood_m15 characterize_compiler_wasm_js \
             -- --nocapture` first",
            compiler_path.display()
        )
    });
    // blitz-js uses a bare WASI specifier. jsaw links closed relative module
    // sets, so adapt only the import declaration at this test seam; Stage B's
    // generated function bodies remain byte-for-byte the real artifact.
    let compiler = compiler.replace("from 'wasi_snapshot_preview1'", "from './wasi.js'");
    let wasi_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../wasi.js");
    let wasi = std::fs::read_to_string(&wasi_path)
        .unwrap_or_else(|error| panic!("could not read {}: {error}", wasi_path.display()));
    (compiler, wasi)
}

#[test]
#[ignore = "requires the generated 164 MiB Stage-B artifact; run explicitly with --ignored"]
fn m19_real_compiler_js_lowers_lazily_with_wasi_glue() {
    // The generated stack-machine functions contain deep expression trees.
    // Keep this bounded and explicit: a stack failure is a diagnosable jsaw
    // limit, not an excuse to consume unbounded host resources.
    std::thread::Builder::new()
        .name("m19-real-compiler-lowering".into())
        .stack_size(256 * 1024 * 1024)
        .spawn(|| {
            let started = Instant::now();
            let (compiler, wasi) = stage_b_sources();
            eprintln!(
                "M19: lazy-ingesting Stage-B compiler ({} bytes)",
                compiler.len()
            );
            let set = module_set_from_sources_lazy([
                ("compiler.js", compiler.as_str()),
                ("wasi.js", wasi.as_str()),
            ])
            .expect("the Stage-B compiler and WASI glue should lazily parse and link");
            eprintln!(
                "M19: ingestion completed in {:?}; lowering Stage C",
                started.elapsed()
            );

            let mut module = portal_pc_waffle::Module::empty();
            convert_modules("compiler.js", &set, &mut module, &ConvertOptions::default())
                .expect("jsaw should lower the real Stage-B compiler module");
            eprintln!("M19: Stage C completed in {:?}", started.elapsed());

            assert!(
                module.funcs.len() > 1_000,
                "the real compiler must not collapse to a stub"
            );
            assert!(
                module
                    .exports
                    .iter()
                    .any(|export| export.name == "__main_void"),
                "the compiler entry export must survive Stage C"
            );
        })
        .expect("should create a dedicated real-compiler lowering thread")
        .join()
        .expect("real-compiler lowering thread should not panic");
}
