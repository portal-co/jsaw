use portal_jsc_swc_cfg::module::CfgModule;
use portal_jsc_swc_ssa::{SFunc, SValue, module::SModule};
use portal_jsc_swc_tac::{Item, module::TModule};
use portal_pc_waffle::{ExportKind, FuncDecl, Module, Operator, Type, ValueDef};
use swc_common::{FileName, GLOBALS, Globals, SourceMap, sync::Lrc};
use swc_ecma_ast::{EsVersion, Module as SwcModule, ModuleItem};
use swc_ecma_parser::{EsSyntax, Syntax, parse_file_as_module, parse_file_as_script};
use wasmtime::{Config, Engine, Instance, Module as WasmtimeModule, Store, Val};

fn lower(source: &str) -> Result<Module<'static>, portal_jsc_waffle::ConvertError> {
    let ssa = script_ssa(source);
    let mut wasm = Module::empty();
    portal_jsc_waffle::convert(&ssa, &mut wasm)?;
    Ok(wasm)
}

fn script_ssa(source: &str) -> SFunc {
    GLOBALS.set(&Globals::default(), || {
        let cm: Lrc<SourceMap> = Lrc::new(SourceMap::default());
        let file = cm.new_source_file(
            Lrc::new(FileName::Custom("fixture.js".into())),
            source.to_owned(),
        );
        let mut errors = vec![];
        let script = parse_file_as_script(
            &file,
            Syntax::Es(EsSyntax::default()),
            EsVersion::Es2022,
            None,
            &mut errors,
        )
        .expect("fixture should parse");
        assert!(errors.is_empty(), "parser diagnostics: {errors:?}");
        let module = SwcModule {
            span: script.span,
            body: script.body.into_iter().map(ModuleItem::Stmt).collect(),
            shebang: script.shebang,
        };
        let cfg = CfgModule::try_from(module).expect("CFG lowering should succeed");
        let tac = TModule::try_from(cfg).expect("TAC lowering should succeed");
        SFunc::try_from(&tac.body).expect("SSA lowering should succeed")
    })
}

fn compile(source: &str) -> Module<'static> {
    lower(source).expect("WasmGC lowering should succeed")
}

fn lower_module(
    source: &str,
    options: &portal_jsc_waffle::ConvertOptions,
) -> Result<Module<'static>, portal_jsc_waffle::ConvertError> {
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
        portal_jsc_waffle::convert_module(&ssa, &mut wasm, options)?;
        Ok(wasm)
    })
}

fn compile_module(source: &str) -> Module<'static> {
    lower_module(source, &portal_jsc_waffle::ConvertOptions::default())
        .expect("WasmGC module lowering should succeed")
}

fn validate(module: &Module<'_>) {
    for (_, declaration) in module.funcs.entries() {
        if let FuncDecl::Body(_, _, body) = declaration {
            body.validate()
                .expect("generated Waffle function should validate");
        }
    }
    let bytes = portal_pc_waffle::to_wasm_bytes(module).expect("Wasm emission should succeed");
    let mut features = wasmparser::WasmFeatures::default();
    features.set(wasmparser::WasmFeatures::GC, true);
    wasmparser::Validator::new_with_features(features)
        .validate_all(&bytes)
        .expect("emitted WasmGC should validate");
}

fn wasm_bytes(module: &Module<'_>) -> Vec<u8> {
    portal_pc_waffle::to_wasm_bytes(module).expect("Wasm emission should succeed")
}

fn execute_in_wasmtime(bytes: &[u8], name: &str, args: &[f64]) -> f64 {
    let mut config = Config::new();
    config.wasm_gc(true);
    config.wasm_function_references(true);
    let engine = Engine::new(&config).expect("Wasmtime engine should support WasmGC");
    let module = WasmtimeModule::new(&engine, bytes).expect("Wasmtime should compile emitted Wasm");
    let mut store = Store::new(&engine, ());
    let instance =
        Instance::new(&mut store, &module, &[]).expect("Wasmtime should instantiate emitted Wasm");
    let function = instance
        .get_func(&mut store, name)
        .unwrap_or_else(|| panic!("missing Wasmtime export {name:?}"));
    let inputs = args
        .iter()
        .copied()
        .map(|value| Val::F64(value.to_bits()))
        .collect::<Vec<_>>();
    let mut outputs = [Val::F64(0)];
    function
        .call(&mut store, &inputs, &mut outputs)
        .unwrap_or_else(|error| panic!("Wasmtime call {name:?} failed: {error:#}"));
    match outputs[0] {
        Val::F64(bits) => f64::from_bits(bits),
        ref value => panic!("numeric export returned {value:?}, expected f64"),
    }
}

fn execute_in_node(bytes: &[u8], name: &str, args: &[f64]) -> f64 {
    use std::{
        fs,
        process::{Command, id},
        sync::atomic::{AtomicUsize, Ordering},
    };

    static NEXT_FIXTURE: AtomicUsize = AtomicUsize::new(0);
    let fixture = std::env::temp_dir().join(format!(
        "portal-jsc-waffle-{}-{}.wasm",
        id(),
        NEXT_FIXTURE.fetch_add(1, Ordering::Relaxed),
    ));
    fs::write(&fixture, bytes).expect("should write temporary Wasm fixture");
    let script = r#"
        import { readFile } from 'node:fs/promises';
        const [path, name, ...args] = process.argv.slice(1);
        const bytes = await readFile(path);
        const { instance } = await WebAssembly.instantiate(bytes);
        const value = instance.exports[name](...args.map(Number));
        process.stdout.write(String(value));
    "#;
    let result = Command::new("node")
        .args(["--input-type=module", "-e", script])
        .arg(&fixture)
        .arg(name)
        .args(args.iter().map(ToString::to_string))
        .output();
    let _ = fs::remove_file(&fixture);
    let output = result.expect("Node.js is required for WasmGC execution tests");
    assert!(
        output.status.success(),
        "Node.js instantiation/call failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout)
        .expect("Node output should be UTF-8")
        .parse()
        .expect("Node numeric result should parse as f64")
}

fn assert_executes_in_all_runtimes(module: &Module<'_>, name: &str, args: &[f64], expected: f64) {
    let bytes = wasm_bytes(module);
    for (runtime, result) in [
        ("Wasmtime", execute_in_wasmtime(&bytes, name, args)),
        ("Node.js", execute_in_node(&bytes, name, args)),
    ] {
        assert!(
            (result - expected).abs() < f64::EPSILON,
            "{runtime} returned {result} from {name:?}; expected {expected}",
        );
    }
}

#[test]
fn compiles_numeric_core_end_to_end() {
    let module = compile("let x = 1 + 2 * 3; x = x - 4; x;");
    validate(&module);
}

#[test]
fn compiles_objects_functions_and_constructors_end_to_end() {
    for source in [
        "let object = { name: 1, '1': 2 }; object.name = 3; object[1] = 4; object[1];",
        "function add(a, b) { return a + b; } add(1); add(1, 2, 3);",
        "let object = { value: 2, add: function(x) { return this.value + x; } }; object.add(3);",
        "function tail(f, x) { return f(x); } function id(x) { return x; } tail(id, 1);",
        "function Box() { this.value = 1; } let box = new Box();",
        "function Override() { return { value: 1 }; } let box = new Override();",
        "let object = { method: function() { let arrow = () => this; return arrow(); } }; object.method();",
        "function callable() {} callable.value = 1; callable.value;",
    ] {
        validate(&compile(source));
    }
}

#[test]
fn compiles_numeric_operators_and_cfg_with_raw_values() {
    let module = compile(
        "let a = 7, b = 3; \
         a + b; a - b; a * b; a / b; a % b; \
         a < b; a <= b; a > b; a >= b; a == b; a != b; a === b; a !== b; \
         a & b; a | b; a ^ b; a << b; a >> b; a >>> b; \
         +a; -a; !a; ~a; void a; a && b; a || b; a ?? b; \
         let merge; if (a < b) { merge = a + b; } else { merge = a - b; } merge;",
    );
    validate(&module);

    let mut f64 = false;
    let mut i32 = false;
    let mut raw_phi = false;
    for (_, declaration) in module.funcs.entries() {
        let Some(body) = declaration.body() else {
            continue;
        };
        raw_phi |= body
            .blocks
            .entries()
            .filter(|(block, _)| *block != body.entry)
            .flat_map(|(_, block)| block.params.iter().map(|(ty, _)| ty))
            .any(|ty| {
                matches!(
                    ty,
                    portal_pc_waffle::Type::F64 | portal_pc_waffle::Type::I32
                )
            });
        for (_, definition) in body.values.entries() {
            if let ValueDef::Operator(operator, _, types) = definition {
                f64 |= matches!(
                    operator,
                    Operator::F64Add
                        | Operator::F64Sub
                        | Operator::F64Mul
                        | Operator::F64Div
                        | Operator::F64Neg
                );
                i32 |= matches!(
                    operator,
                    Operator::I32And
                        | Operator::I32Or
                        | Operator::I32Xor
                        | Operator::I32Shl
                        | Operator::I32Eqz
                );
                let output = &body.type_pool[*types];
                f64 |= output.contains(&portal_pc_waffle::Type::F64);
                i32 |= output.contains(&portal_pc_waffle::Type::I32);
            }
        }
    }
    assert!(f64, "arithmetic should retain raw f64 Waffle values");
    assert!(
        i32,
        "boolean and bitwise paths should retain raw i32 Waffle values"
    );
    assert!(
        raw_phi,
        "compatible CFG joins should retain raw f64/i32 block parameters"
    );
}

#[test]
fn exports_es_module_functions_with_numeric_and_gc_abis() {
    let source = "
        export function add(a, b) { return a + b; }
        export { add as sum };
        export default function twice(value) { return value * 2; }
        export const ignored = 1;
    ";
    let options = portal_jsc_waffle::ConvertOptions {
        gc_export_suffix: Some("$gc".to_owned()),
        ..Default::default()
    };
    let module = lower_module(source, &options).expect("module lowering should succeed");
    validate(&module);

    let exports = module
        .exports
        .iter()
        .map(|export| (export.name.as_str(), &export.kind))
        .collect::<Vec<_>>();
    assert_eq!(
        exports.iter().map(|(name, _)| *name).collect::<Vec<_>>(),
        vec!["add", "add$gc", "sum", "sum$gc", "default", "default$gc"],
    );
    assert!(
        !exports.iter().any(|(name, _)| *name == "ignored"),
        "non-function exports must not receive a numeric wrapper"
    );
    for (name, kind) in exports {
        let ExportKind::Func(function) = kind else {
            panic!("{name:?} should be a function export");
        };
        let signature = match &module.funcs[*function] {
            FuncDecl::Body(signature, _, _) => signature,
            declaration => panic!("{name:?} should have a function body, got {declaration:?}"),
        };
        let portal_pc_waffle::SignatureData::Func {
            params, returns, ..
        } = &module.signatures[*signature]
        else {
            panic!("{name:?} should have a function signature");
        };
        if !name.ends_with("$gc") {
            assert!(params.iter().all(|ty| *ty == Type::F64));
            assert_eq!(returns, &vec![Type::F64]);
        }
    }
}

#[test]
fn executes_numeric_module_exports_in_wasmtime_and_node() {
    let module = compile_module(
        "
            export function compute(value) {
                if (value > 2) return value * 2;
                return value - 1;
            }
            export default function add(a, b) { return a + b; }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "compute", &[4.0], 8.0);
    assert_executes_in_all_runtimes(&module, "compute", &[1.0], 0.0);
    assert_executes_in_all_runtimes(&module, "default", &[2.5, 4.0], 6.5);
}

#[test]
fn executes_object_mutation_shape_changes_and_polymorphic_paths() {
    let module = compile_module(
        "
            export function mutate() {
                let object = {};
                object.count = 1;
                object.count = object.count + 2;
                object.extra = 4;
                return object.count + object.extra;
            }

            export function reshape() {
                let object = { slot: 1 };
                object.slot = {};
                object.slot.count = 4;
                object.slot.extra = 6;
                return object.slot.count + object.slot.extra;
            }

            export function polymorphic(flag) {
                let value = flag > 0 ? {} : 2;
                if (flag > 0) return 7;
                return 3;
            }
        ",
    );
    validate(&module);

    assert_executes_in_all_runtimes(&module, "mutate", &[], 7.0);
    assert_executes_in_all_runtimes(&module, "reshape", &[], 10.0);
    assert_executes_in_all_runtimes(&module, "polymorphic", &[1.0], 7.0);
    assert_executes_in_all_runtimes(&module, "polymorphic", &[0.0], 3.0);

    let reference_tests = module
        .funcs
        .entries()
        .filter_map(|(_, declaration)| declaration.body())
        .flat_map(|body| body.values.entries())
        .filter(|(_, definition)| {
            matches!(
                definition,
                ValueDef::Operator(Operator::RefTest { .. }, _, _)
            )
        })
        .count();
    assert!(
        reference_tests >= 2,
        "object lowering should refine anyref values with ref.test before ref.cast"
    );
}

#[test]
fn lowers_static_subarray_item_to_a_bounded_array_copy() {
    // The current TAC source converter emits this helper for array-rest
    // assignment. Build its already-normalized SSA form directly so this test
    // covers the lowerer's contract independently of TAC-pattern coverage.
    let mut ssa = script_ssa(
        "
            let source = [1, 2, 3];
            let rest = [0];
            let result = rest[0];
            result;
        ",
    );
    let arrays = ssa
        .cfg
        .values
        .iter()
        .filter_map(|(id, value)| {
            matches!(
                &value.value,
                SValue::Item {
                    item: Item::Arr { .. },
                    ..
                }
            )
            .then_some(id)
        })
        .collect::<Vec<_>>();
    assert!(
        arrays.len() >= 2,
        "fixture must retain source and destination array items"
    );
    ssa.cfg.values[arrays[1]].value = SValue::Item {
        item: Item::StaticSubArray {
            begin: 1,
            end: 0,
            wrapped: arrays[0],
        },
        span: None,
    };

    let mut module = Module::empty();
    portal_jsc_waffle::convert(&ssa, &mut module).expect("static subarray should lower");
    validate(&module);
    assert!(
        module.funcs.entries().any(|(_, declaration)| {
            declaration.body().is_some_and(|body| {
                body.values.entries().any(|(_, definition)| {
                    matches!(
                        definition,
                        ValueDef::Operator(Operator::ArrayCopy { .. }, _, _)
                    )
                })
            })
        }),
        "static subarray must allocate and copy its bounded range"
    );
}

#[test]
fn executes_arrays_arguments_and_static_destructuring_helpers() {
    let module = compile_module(
        "
            export function array_literal() {
                let values = [3, 4, 5];
                return values[0] * 100 + values[1] * 10 + values[2] + values.length;
            }

            export function arguments_visible(first, second) {
                return arguments[0] * 100 + arguments[1] * 10 + arguments.length;
            }

            export function object_rest() {
                let source = { hidden: 4, kept: 2, other: 3 };
                let { hidden, ...rest } = source;
                rest.kept = 7;
                return source.kept * 100 + rest.kept * 10 + rest.other;
            }
        ",
    );
    validate(&module);

    let mut array_new_fixed = false;
    let mut array_get = false;
    let mut array_len = false;
    for (_, declaration) in module.funcs.entries() {
        let Some(body) = declaration.body() else {
            continue;
        };
        for (_, definition) in body.values.entries() {
            let ValueDef::Operator(operator, _, _) = definition else {
                continue;
            };
            array_new_fixed |= matches!(operator, Operator::ArrayNewFixed { .. });
            array_get |= matches!(operator, Operator::ArrayGet { .. });
            array_len |= matches!(operator, Operator::ArrayLen);
        }
    }
    assert!(
        array_new_fixed,
        "array literals and calls should allocate arrays"
    );
    assert!(array_get, "array indexing should use WasmGC array.get");
    assert!(array_len, "array length should use WasmGC array.len");

    assert_executes_in_all_runtimes(&module, "array_literal", &[], 348.0);
    assert_executes_in_all_runtimes(&module, "arguments_visible", &[2.0, 3.0], 232.0);
    assert_executes_in_all_runtimes(&module, "object_rest", &[], 273.0);
}

#[test]
fn rejects_colliding_internal_gc_export_names() {
    let error = lower_module(
        "
            export function value(input) { return input; }
            export const value$gc = 1;
        ",
        &portal_jsc_waffle::ConvertOptions {
            gc_export_suffix: Some("$gc".to_owned()),
            ..Default::default()
        },
    )
    .expect_err("a generated internal export must not collide with a source export");
    assert!(error.to_string().contains("value$gc"));
}

#[test]
fn rejects_unsupported_forms_with_convert_error() {
    for (source, expected) in [
        ("let value = 2 ** 3;", "binary operator \"**\""),
        (
            "let object = {}; let value = 1 in object;",
            "binary operator \"in\"",
        ),
        (
            "let value = 1 instanceof 2;",
            "binary operator \"instanceof\"",
        ),
        ("let value = typeof 1;", "unary operator \"typeof\""),
        (
            "let object = {}; delete object.value;",
            "unary operator \"delete\"",
        ),
        (
            "let key = 1; let object = {}; object[key];",
            "dynamic property keys",
        ),
        (
            "function value() { return 'runtime string'; } value();",
            "runtime value",
        ),
    ] {
        let error = match lower(source) {
            Ok(_) => panic!("fixture should be rejected by convert: {source}"),
            Err(error) => error,
        };
        assert!(
            error.to_string().contains(expected),
            "expected {expected:?} in {error}"
        );
    }
}
