//! wasm-esm phase 1 integration: Wasm binary members of the closed module
//! set link through `build_import_table`, and calls into them fail closed
//! with a named not-yet-lowered diagnostic until phase 2 lands
//! (docs/plan-wasm-esm-modules.md, portal-co/jsaw#14).

use portal_jsc_waffle::{ModuleSet, convert_modules, module_set_from_sources};

/// `(module (func (export "add") (param i32 i32) (result i32)
///   local.get 0 local.get 1 i32.add))`
fn add_module() -> Vec<u8> {
    let mut out = b"\0asm\x01\0\0\0".to_vec();
    // type section: one (i32, i32) -> i32 func type
    out.extend([0x01, 0x07, 0x01, 0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f]);
    // function section: func 0 has type 0
    out.extend([0x03, 0x02, 0x01, 0x00]);
    // export section: "add" -> func 0
    out.extend([0x07, 0x07, 0x01, 0x03, b'a', b'd', b'd', 0x00, 0x00]);
    // code section: one body
    out.extend([
        0x0a, 0x09, 0x01, 0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0x6a, 0x0b,
    ]);
    out
}

fn convert(entry: &str, set: &ModuleSet<'_>) -> Result<(), portal_jsc_waffle::ConvertError> {
    let mut module = portal_pc_waffle::Module::empty();
    convert_modules(entry, set, &mut module, &Default::default())?;
    Ok(())
}

#[test]
fn uncalled_wasm_imports_link_and_lowering_stays_unchanged() {
    // The import is declared but never called: no adapter is needed yet and
    // conversion must succeed exactly as before.
    let mut set = module_set_from_sources([(
        "main.js",
        "import { add } from './math.wasm'; export function run() { return 41 + 1; }",
    )])
    .expect("sources");
    set.insert_wasm("math.wasm", &add_module())
        .expect("valid Wasm member");
    convert("main.js", &set).expect("an uncalled wasm import must not fail conversion");
}

#[test]
fn calls_into_wasm_members_fail_closed_in_phase_1() {
    let mut set = module_set_from_sources([(
        "main.js",
        "import { add } from './math.wasm'; export function run(a, b) { return add(a, b); }",
    )])
    .expect("sources");
    set.insert_wasm("math.wasm", &add_module())
        .expect("valid Wasm member");
    let error =
        convert("main.js", &set).expect_err("calling a Wasm member must fail closed in phase 1");
    assert!(error.message.contains("not yet lowered"), "{error}");
    assert!(error.message.contains("math.wasm"), "{error}");
}

#[test]
fn wasm_member_is_reachable_through_relative_directories() {
    let mut set = module_set_from_sources([(
        "src/main.js",
        "import { add } from '../lib/math.wasm'; export function run(a, b) { return add(a, b); }",
    )])
    .expect("sources");
    set.insert_wasm("lib/math.wasm", &add_module())
        .expect("valid Wasm member");
    let error = convert("src/main.js", &set)
        .expect_err("the linked wasm import reaches the phase-1 diagnostic");
    assert!(error.message.contains("lib/math.wasm"), "{error}");
}
