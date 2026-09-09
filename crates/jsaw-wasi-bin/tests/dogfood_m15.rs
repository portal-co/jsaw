//! Milestone 15 (dogfood feasibility gate): characterize the JavaScript that
//! wasm-blitz's `blitz-js` backend emits for the real `jsaw-wasi-bin`
//! compiler wasm.
//!
//! Two things happen here:
//!
//! 1. `characterize_compiler_wasm_js` compiles the real
//!    `target/wasm32-wasip1/release/jsaw-wasi-bin.wasm` through blitz-js and
//!    writes `target/dogfood-m15/compiler.js`, plus a printed inventory of
//!    the JS surface jsaw would have to ingest (BigInt usage, stack-machine
//!    idioms, DataView intrinsics, the WASI import list). This is the
//!    feasibility-gate artifact: read its output to scope Milestones 16-18.
//!
//! 2. `trivial_core_wasm_round_trips_to_js` is the pipe-shape smoke: a tiny
//!    hand-encoded core-wasm add module is translated to ESM JS and asserted
//!    to contain the expected function/export shape, proving Stages A->B work
//!    on a trivial input before we commit to the big frontend work.
//!
//! Run with output visible:
//!   cargo test -p jsaw-wasi-bin --test dogfood_m15 -- --nocapture

use portal_solutions_blitz_common::wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module,
    TypeSection, ValType,
};
use portal_solutions_blitz_common::{
    dce_pass,
    ops::mach_operators,
    wasmparser::{self, FuncType as WpFuncType},
};
use portal_solutions_blitz_js::{
    JsWrite, State as JsState, js_emit_exports_esm, js_emit_imports_esm, js_module_preamble_esm,
};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};

/// Locate the real compiler wasm built by `cargo build -p jsaw-wasi-bin
/// --target wasm32-wasip1 --release`. Fail closed with guidance if absent.
fn compiler_wasm_path() -> PathBuf {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/wasm32-wasip1/release/jsaw-wasi-bin.wasm");
    assert!(
        path.is_file(),
        "compiler wasm not found at {} — run `cargo build -p jsaw-wasi-bin \
         --target wasm32-wasip1 --release` first",
        path.display()
    );
    path
}

/// Parse the signature tables blitz-js's driver needs: the `wasmparser`
/// func types (for `mach_operators`), the parallel `wasm_encoder` func types
/// (for `on_mach`), and the per-function-index signature indices (imports
/// first, then the function section).
fn parse_sigs(wasm: &[u8]) -> (Vec<WpFuncType>, Vec<portal_solutions_blitz_common::wasm_encoder::FuncType>, Vec<u32>) {
    let mut sigs_wp: Vec<WpFuncType> = Vec::new();
    let mut fsigs: Vec<u32> = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(wasm).flatten() {
        match payload {
            wasmparser::Payload::TypeSection(reader) => {
                for group in reader.into_iter().flatten() {
                    for subtype in group.into_types() {
                        if let wasmparser::CompositeInnerType::Func(ft) =
                            subtype.composite_type.inner
                        {
                            sigs_wp.push(ft);
                        }
                    }
                }
            }
            wasmparser::Payload::ImportSection(reader) => {
                for imp in reader.into_iter().flatten() {
                    if let wasmparser::TypeRef::Func(ty_idx) = imp.ty {
                        fsigs.push(ty_idx);
                    }
                }
            }
            wasmparser::Payload::FunctionSection(reader) => {
                fsigs.extend(reader.into_iter().flatten());
            }
            _ => {}
        }
    }
    let sigs_enc = sigs_wp
        .iter()
        .cloned()
        .map(|ft| {
            portal_solutions_blitz_common::wasm_encoder::FuncType::try_from(ft)
                .expect("func type converts to wasm_encoder")
        })
        .collect();
    (sigs_wp, sigs_enc, fsigs)
}

/// Imported function `(module, name)` pairs, in import order.
fn parse_imports(wasm: &[u8]) -> Vec<(String, String)> {
    let mut imports = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(wasm).flatten() {
        if let wasmparser::Payload::ImportSection(reader) = payload {
            for imp in reader.into_iter().flatten() {
                if matches!(imp.ty, wasmparser::TypeRef::Func(_)) {
                    imports.push((imp.module.to_owned(), imp.name.to_owned()));
                }
            }
        }
    }
    imports
}

/// Exported function `(full_wasm_index, name)` pairs.
fn parse_exports(wasm: &[u8]) -> Vec<(u32, String)> {
    let mut exports = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(wasm).flatten() {
        if let wasmparser::Payload::ExportSection(reader) = payload {
            for exp in reader.into_iter().flatten() {
                if matches!(exp.kind, wasmparser::ExternalKind::Func) {
                    exports.push((exp.index, exp.name.to_owned()));
                }
            }
        }
    }
    exports
}

/// The core blitz-js driver shared by the smoke and the real characterization:
/// translate `wasm` to an ESM JavaScript string. Imports are emitted as
/// `import { name as _import_N } from 'module'` and exports as
/// `export { $N as name }`. When `opt` is set, blitz-js's optimized stack
/// tracking is enabled (drastically less stack-weave bloat).
fn compile_wasm_to_esm_js_opt(wasm: &[u8], opt: bool) -> String {
    let (sigs_wp, sigs_enc, fsigs) = parse_sigs(wasm);
    let raw_imports = parse_imports(wasm);
    let raw_exports = parse_exports(wasm);
    let imports_ref: Vec<(&str, &str)> = raw_imports
        .iter()
        .map(|(m, n)| (m.as_str(), n.as_str()))
        .collect();
    let exports_ref: Vec<(u32, &str)> = raw_exports
        .iter()
        .map(|(idx, n)| (*idx, n.as_str()))
        .collect();

    let mut bodies: Vec<wasmparser::FunctionBody<'_>> = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(wasm).flatten() {
        if let wasmparser::Payload::CodeSectionEntry(body) = payload {
            bodies.push(body);
        }
    }

    let import_count = imports_ref.len() as u32;
    let raw_ops = mach_operators::<(), wasmparser::BinaryReaderError>(
        &bodies, &fsigs, &sigs_wp, import_count,
    );
    let ops = dce_pass!(raw_ops);

    let mut out = String::new();
    js_module_preamble_esm(&mut out).expect("preamble");
    js_emit_imports_esm(&mut out, &imports_ref).expect("imports");

    let mut state = JsState::default();
    if opt {
        state.enable_opt(portal_solutions_blitz_opt::OptState::default);
    }
    let mut reencoder = portal_solutions_blitz_common::wasm_encoder::reencode::RoundtripReencoder;
    for op in ops {
        let op = op.expect("mach op");
        JsWrite::on_mach(
            &mut out,
            &sigs_enc,
            &fsigs,
            &[],
            &imports_ref,
            &mut state,
            &op,
            &mut reencoder,
        )
        .expect("on_mach");
    }

    js_emit_exports_esm(&mut out, &exports_ref).expect("exports");
    out
}

/// Count occurrences of `needle` in `haystack`.
fn count(haystack: &str, needle: &str) -> usize {
    haystack.matches(needle).count()
}

/// The pipe-shape smoke: a trivial hand-encoded core-wasm add module
/// translates to ESM JS with the expected shape.
#[test]
fn trivial_core_wasm_round_trips_to_js() {
    // (func (param i32 i32) (result i32) local.get 0 local.get 1 i32.add)
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types
        .ty()
        .function([ValType::I32, ValType::I32], [ValType::I32]);
    module.section(&types);
    let mut functions = FunctionSection::new();
    functions.function(0);
    module.section(&functions);
    let mut exports = ExportSection::new();
    exports.export("add", ExportKind::Func, 0);
    module.section(&exports);
    let mut code = CodeSection::new();
    let mut func = Function::new([]);
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);
    code.function(&func);
    module.section(&code);
    let wasm = module.finish();

    let js = compile_wasm_to_esm_js_opt(&wasm, false);
    assert!(
        js.contains("export {$0 as add};"),
        "expected an ESM export of the add function; got:\n{js}"
    );
    assert!(
        js.contains("$0=") || js.contains("function $0"),
        "expected a compiled body for $0; got:\n{js}"
    );
    // NOTE: blitz-js models ALL wasm values as BigInt (i32 included — every
    // function masks i32 math with `& mask32` and defines toInt/toUint). So
    // even an i32-only module uses BigInt. The real assertion is the shape.
    assert!(
        js.contains("asUintN"),
        "expected BigInt fixed-width helpers in the compiled body; got:\n{js}"
    );
    // i32 math is masked into 32 bits via mask32, confirming the all-BigInt model.
    assert!(
        js.contains("mask32"),
        "expected the i32 masking idiom; got:\n{js}"
    );

    // Execute BOTH non-opt and opt compiled output in Node and check the result,
    // proving the opt-mode stack tracking produces correct code (not just
    // non-crashing output). The fixes to Drop/Select/branch boundaries are only
    // trustworthy if the emitted JS computes the right answer.
    for (label, compiled) in [("non-opt", js), ("opt", compile_wasm_to_esm_js_opt(&wasm, true))] {
        let got = node_call(&compiled, "add", &[40, 2]);
        assert_eq!(got, 42, "{label} compiled add(40,2) should be 42");
    }
}

/// Run an ESM module's exported `func` with BigInt i64 args in Node, returning
/// the first returned value as i64. Fails closed if Node is unavailable.
fn node_call(js: &str, func: &str, args: &[i64]) -> i64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static SEQ: AtomicU64 = AtomicU64::new(0);
    let dir = std::env::temp_dir().join(format!(
        "m15_exec_{}_{}",
        std::process::id(),
        SEQ.fetch_add(1, Ordering::SeqCst)
    ));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("m.mjs"), js).unwrap();
    let driver = format!(
        "import {{ {func} }} from './m.mjs';\n\
         const r = {func}(...process.argv.slice(2).map(BigInt));\n\
         const v = Array.isArray(r) ? r[0] : r;\n\
         console.log(v.toString());\n"
    );
    std::fs::write(dir.join("d.mjs"), driver).unwrap();
    let out = std::process::Command::new("node")
        .arg(dir.join("d.mjs"))
        .args(args.iter().map(|a| a.to_string()))
        .output()
        .expect("node must be available to validate compiled JS");
    assert!(
        out.status.success(),
        "node run failed for {func}: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout)
        .trim()
        .parse::<i64>()
        .expect("numeric result")
}

/// The feasibility gate itself: compile the real compiler wasm and inventory
/// the emitted JS. Writes target/dogfood-m15/compiler.js and prints the
/// inventory (use --nocapture).
#[test]
fn characterize_compiler_wasm_js() {
    let path = compiler_wasm_path();
    let wasm = std::fs::read(&path).expect("read compiler wasm");

    // ---- WASI import inventory (this scopes Milestone 18's glue) ----
    let imports = parse_imports(&wasm);
    let mut wasi_imports: Vec<String> = imports
        .iter()
        .map(|(m, n)| format!("{m}::{n}"))
        .collect();
    wasi_imports.sort();
    wasi_imports.dedup();

    // ---- the translation (opt mode: drastically less stack-weave bloat) ----
    let js = compile_wasm_to_esm_js_opt(&wasm, true);

    // ---- persist the artifact ----
    let out_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../target/dogfood-m15");
    std::fs::create_dir_all(&out_dir).expect("create dogfood-m15 dir");
    let js_path = out_dir.join("compiler.js");
    std::fs::write(&js_path, &js).expect("write compiler.js");

    // ---- the inventory (Milestones 16-18 scope) ----
    let exports = parse_exports(&wasm);
    let report = format!(
        "=== Milestone 15 dogfood inventory ===\n\
         source wasm : {}\n\
         emitted js  : {} ({} bytes)\n\
         \n\
         -- WASI imports the glue (Milestone 18) must implement ({}) --\n{}\n\
         \n\
         -- module shape --\n\
         func imports total : {}\n\
         func exports       : {} ({})\n\
         \n\
         -- BigInt / i64 surface (Milestone 16) --\n\
         BigInt literal `n` suffix (approx) : {}\n\
         BigInt.asUintN                     : {}\n\
         BigInt.asIntN                      : {}\n\
         \n\
         -- stack-machine & control idioms (Milestone 17) --\n\
         array spread `[...`                : {}\n\
         .pop() / .push(                    : {} / {}\n\
         comma-op sequence `(a=` (approx)   : {}\n\
         __sig metadata                     : {}\n\
         return_call                        : {}\n\
         call_indirect ($table_             : {}\n\
         \n\
         -- linear-memory intrinsics (Milestone 17) --\n\
         $mem_dv DataView                   : {}\n\
         $mem.set(                          : {}\n\
         memory.grow                        : {}\n\
         \n\
         note: counts are substring approximations to guide scoping, not exact op counts.",
        path.display(),
        js_path.display(),
        js.len(),
        wasi_imports.len(),
        wasi_imports
            .iter()
            .map(|s| format!("  {s}\n"))
            .collect::<String>(),
        imports.len(),
        exports.len(),
        exports
            .iter()
            .map(|(_, n)| n.as_str())
            .collect::<Vec<_>>()
            .join(", "),
        count(&js, "n,") + count(&js, "n)") + count(&js, "n]"),
        count(&js, "asUintN"),
        count(&js, "asIntN"),
        count(&js, "[..."),
        count(&js, ".pop()"),
        count(&js, ".push("),
        count(&js, "(a="),
        count(&js, "__sig"),
        count(&js, "return_call"),
        count(&js, "$table_"),
        count(&js, "$mem_dv"),
        count(&js, "$mem.set("),
        count(&js, "memory.grow"),
    );

    // Persist and print.
    let report_path = out_dir.join("inventory.txt");
    std::fs::write(&report_path, &report).expect("write inventory.txt");
    println!("\n{report}");
    eprintln!("inventory written to {}", report_path.display());

    // ---- assertions: the translation succeeded and is non-trivial ----
    assert!(!js.is_empty(), "blitz-js produced no output");
    assert!(
        js.contains("export {"),
        "expected ESM exports in the compiler JS"
    );
    // A rustc wasip1 binary is i64-heavy: the BigInt surface must be present,
    // confirming Milestone 16 (BigInt in jsaw) is load-bearing.
    assert!(
        js.contains("asUintN") || js.contains("asIntN"),
        "expected BigInt fixed-width ops in the compiler JS — if absent, the \
         i64 surface assumption is wrong and Milestone 16 needs rescoping"
    );
    let _unused: &Path = &path;
}
