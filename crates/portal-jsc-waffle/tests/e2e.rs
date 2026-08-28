use portal_jsc_swc_cfg::module::CfgModule;
use portal_jsc_swc_ssa::{SFunc, SValue, module::SModule};
use portal_jsc_swc_tac::{Item, module::TModule};
use portal_pc_waffle::{
    ExportKind, FuncDecl, HeapType, Module, Operator, SignatureData, Terminator, Type, ValueDef,
};
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

            export function shaped_dynamic() {
                let object = { left: 2, right: 3 };
                let key = 'left';
                object[key] = 5;
                object.extra = 7;
                return object.left * 100 + object.right * 10 + object.extra;
            }
        ",
    );
    validate(&module);

    assert_executes_in_all_runtimes(&module, "mutate", &[], 7.0);
    assert_executes_in_all_runtimes(&module, "reshape", &[], 10.0);
    assert_executes_in_all_runtimes(&module, "polymorphic", &[1.0], 7.0);
    assert_executes_in_all_runtimes(&module, "polymorphic", &[0.0], 3.0);
    assert_executes_in_all_runtimes(&module, "shaped_dynamic", &[], 537.0);

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
    assert!(
        module.funcs.entries().any(|(_, declaration)| {
            matches!(
                declaration,
                FuncDecl::Body(_, name, body)
                    if name.starts_with("js_shape_lookup_")
                        && body.blocks.entries().any(|(_, block)| {
                            matches!(block.terminator.terminator, Terminator::ReturnCall { .. })
                        })
            )
        }),
        "shape lookup should tail-call its in-shape trie fallback"
    );
}

#[test]
fn executes_object_accessors_with_continuations() {
    let module = compile_module(
        "
            export function paired() {
                let object = {
                    value: 2,
                    get doubled() { return this.value * 2; },
                    set doubled(next) { this.value = next / 2; },
                };
                object.doubled = 10;
                let result = object.doubled;
                object.extra = 3;
                return result * 100 + object.value + object.extra;
            }

            export function getter_only() {
                let object = { get value() { return 7; } };
                object.value = 12;
                return object.value;
            }

            export function setter_only() {
                let object = {
                    stored: 1,
                    set value(next) { this.stored = next; },
                };
                object.value = 9;
                if (object.value) return 0;
                return object.stored;
            }

            export function dynamic() {
                let key = 'value';
                let object = {
                    stored: 4,
                    get value() { return this.stored; },
                    set value(next) { this.stored = next + 1; },
                };
                object[key] = 8;
                return object[key];
            }

            export function overwrite() {
                let object = {
                    get value() { return 1; },
                    value: 6,
                };
                return object.value;
            }

            export function accessor_call() {
                let object = {
                    factor: 3,
                    get method() {
                        return function(value) { return this.factor * value; };
                    },
                };
                return object.method(4);
            }

            export function accessor_tail(value) {
                let object = {
                    get invoke() {
                        return function(input) { return input + 1; };
                    },
                };
                return object.invoke(value);
            }

            export function rest_materializes_accessors() {
                let source = {
                    base: 5,
                    get value() { return this.base + 1; },
                    extra: 2,
                };
                let { base, ...rest } = source;
                return rest.value * 10 + rest.extra;
            }
        ",
    );
    validate(&module);

    assert_executes_in_all_runtimes(&module, "paired", &[], 1008.0);
    assert_executes_in_all_runtimes(&module, "getter_only", &[], 7.0);
    assert_executes_in_all_runtimes(&module, "setter_only", &[], 9.0);
    assert_executes_in_all_runtimes(&module, "dynamic", &[], 9.0);
    assert_executes_in_all_runtimes(&module, "overwrite", &[], 6.0);
    assert_executes_in_all_runtimes(&module, "accessor_call", &[], 12.0);
    assert_executes_in_all_runtimes(&module, "accessor_tail", &[4.0], 5.0);
    assert_executes_in_all_runtimes(&module, "rest_materializes_accessors", &[], 62.0);

    assert!(
        module.funcs.entries().any(|(_, declaration)| {
            declaration.body().is_some_and(|body| {
                body.values.entries().any(|(_, definition)| {
                    matches!(
                        definition,
                        ValueDef::Operator(Operator::CallRef { .. }, _, _)
                    )
                })
            })
        }),
        "accessor paths should invoke their getter or setter through the function adapter"
    );
    assert!(
        module.funcs.entries().any(|(_, declaration)| {
            declaration.body().is_some_and(|body| {
                body.values.entries().any(|(_, definition)| {
                    let ValueDef::Operator(Operator::RefTest { ty }, _, _) = definition else {
                        return false;
                    };
                    let Type::Heap(reference) = ty else {
                        return false;
                    };
                    let HeapType::Sig { sig_index } = reference.value else {
                        return false;
                    };
                    !reference.nullable
                        && matches!(
                            &module.signatures[sig_index],
                            SignatureData::Struct { fields, .. } if fields.len() == 2
                        )
                })
            })
        }),
        "property reads and writes should branch on a non-null descriptor tag"
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
fn executes_arrays_as_objects_with_named_fields() {
    let module = compile_module(
        "
            export function array_fields() {
                let values = [3, 4];
                values.label = 7;
                return values.label * 100 + values[0] * 10 + values[1] + values.length;
            }

            export function object_length_field() {
                let value = { length: 4 };
                value.length = 7;
                return value.length;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "array_fields", &[], 736.0);
    assert_executes_in_all_runtimes(&module, "object_length_field", &[], 7.0);
}

#[test]
fn executes_computed_array_writes_growth_and_length_resize() {
    let module = compile_module(
        "
            export function grow() {
                let values = [3];
                let index = 3;
                values[index] = 7;
                return values.length * 100 + values[3];
            }

            export function resize() {
                let values = [3, 4, 5];
                values.length = 1;
                return values.length * 10 + values[0];
            }

            export function static_index() {
                let values = [3];
                values[2] = 6;
                return values.length * 10 + values[2];
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "grow", &[], 407.0);
    assert_executes_in_all_runtimes(&module, "resize", &[], 13.0);
    assert_executes_in_all_runtimes(&module, "static_index", &[], 36.0);
}

#[test]
fn executes_arguments_as_unified_arrays() {
    let module = compile_module(
        "
            export function arguments_object(first) {
                arguments.label = 4;
                let index = 1;
                arguments[index] = first + 2;
                return arguments.label * 100 + arguments.length * 10 + arguments[index];
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "arguments_object", &[3.0], 425.0);
}

#[test]
fn executes_utf8_strings_with_utf16_length() {
    let module = compile_module(
        "
            export function ascii_length() { return 'abc'.length; }
            export function unicode_length() { return 'a😀'.length; }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "ascii_length", &[], 3.0);
    assert_executes_in_all_runtimes(&module, "unicode_length", &[], 3.0);
}

#[test]
fn executes_utf8_string_concatenation_with_lazy_utf16_cache() {
    let module = compile_module(
        "
            export function aliases() {
                let left = 'ab';
                let right = 'cd';
                return (left + right).length;
            }
            export function astral() {
                let value = 'a😀' + 'b';
                return value.length + value[1].length + value[2].length;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "aliases", &[], 4.0);
    assert_executes_in_all_runtimes(&module, "astral", &[], 6.0);
}

#[test]
fn executes_dynamic_string_fields_and_computed_calls() {
    let module = compile_module(
        "
            export function fields() {
                let key = 'score';
                let object = {};
                object[key] = 7;
                return object[key];
            }

            export function call() {
                let key = 'run';
                let object = { value: 4, run: function(add) { return this.value + add; } };
                return object[key](3);
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "fields", &[], 7.0);
    assert_executes_in_all_runtimes(&module, "call", &[], 7.0);
}

#[test]
fn executes_runtime_string_array_indices_before_named_fields() {
    let module = compile_module(
        "
            export function string_keys() {
                let values = [2];
                let index = '2';
                let length = 'length';
                let name = '02';
                values[index] = 7;
                values[name] = 5;
                return values[index] * 100 + values[length] * 10 + values[name];
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "string_keys", &[], 735.0);
}

#[test]
fn executes_utf16_string_indices() {
    let module = compile_module(
        "
            export function ascii() { return 'abc'[1].length; }
            export function astral() { return 'a😀'[1].length + 'a😀'[2].length; }
            export function aliased_astral() {
                let value = 'a😀';
                return value[1].length + value[2].length;
            }
            export function indexed_key() { return { b: 9 }['abc'[1]]; }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "ascii", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "astral", &[], 2.0);
    assert_executes_in_all_runtimes(&module, "aliased_astral", &[], 2.0);
    assert_executes_in_all_runtimes(&module, "indexed_key", &[], 9.0);
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

#[test]
fn executes_math_primordial() {
    let module = compile_module(
        "
            export function math_pi() {
                return (Math.PI > 3.14159 && Math.PI < 3.14160) ? 1 : 0;
            }

            export function math_sqrt() {
                return Math.sqrt(16);
            }

            export function math_abs() {
                return Math.abs(-5);
            }

            export function math_floor() {
                return Math.floor(3.7);
            }

            export function math_ceil() {
                return Math.ceil(3.2);
            }

            export function math_round_half_up() {
                // JS rounds .5 up, unlike Wasm's round-half-to-even f64.nearest.
                return Math.round(2.5);
            }

            export function math_trunc() {
                return Math.trunc(-3.9);
            }

            export function math_max() {
                return Math.max(1, 9);
            }

            export function math_min() {
                return Math.min(4, 2);
            }

            export function math_sign_negative() {
                return Math.sign(-7);
            }

            export function math_imul_overflow() {
                // Wraps as a 32-bit multiply rather than producing 3 * 2^30 exactly.
                return Math.imul(3, 1073741824);
            }

            export function math_fround_exact() {
                return Math.fround(2);
            }

            export function math_shadowed_by_local() {
                let Math = { PI: 42 };
                return Math.PI;
            }
        ",
    );
    validate(&module);

    assert_executes_in_all_runtimes(&module, "math_pi", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "math_sqrt", &[], 4.0);
    assert_executes_in_all_runtimes(&module, "math_abs", &[], 5.0);
    assert_executes_in_all_runtimes(&module, "math_floor", &[], 3.0);
    assert_executes_in_all_runtimes(&module, "math_ceil", &[], 4.0);
    assert_executes_in_all_runtimes(&module, "math_round_half_up", &[], 3.0);
    assert_executes_in_all_runtimes(&module, "math_trunc", &[], -3.0);
    assert_executes_in_all_runtimes(&module, "math_max", &[], 9.0);
    assert_executes_in_all_runtimes(&module, "math_min", &[], 2.0);
    assert_executes_in_all_runtimes(&module, "math_sign_negative", &[], -1.0);
    assert_executes_in_all_runtimes(&module, "math_imul_overflow", &[], -1073741824.0);
    assert_executes_in_all_runtimes(&module, "math_fround_exact", &[], 2.0);
    assert_executes_in_all_runtimes(&module, "math_shadowed_by_local", &[], 42.0);
}

#[test]
fn executes_array_primordial() {
    let module = compile_module(
        "
            export function array_is_array_true() {
                return Array.isArray([1, 2, 3]) ? 1 : 0;
            }

            export function array_is_array_false_object() {
                return Array.isArray({}) ? 1 : 0;
            }

            export function array_is_array_false_number() {
                return Array.isArray(5) ? 1 : 0;
            }
        ",
    );
    validate(&module);

    assert_executes_in_all_runtimes(&module, "array_is_array_true", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "array_is_array_false_object", &[], 0.0);
    assert_executes_in_all_runtimes(&module, "array_is_array_false_number", &[], 0.0);
}

#[test]
fn executes_reflect_primordial() {
    let module = compile_module(
        "
            export function reflect_get_basic() {
                let obj = { x: 42 };
                return Reflect.get(obj, 'x');
            }

            export function reflect_set_basic() {
                let obj = { x: 1 };
                Reflect.set(obj, 'x', 99);
                return obj.x;
            }

            export function reflect_has_true() {
                let obj = { x: 1 };
                return Reflect.has(obj, 'x') ? 1 : 0;
            }

            export function reflect_has_false() {
                let obj = { x: 1 };
                return Reflect.has(obj, 'y') ? 1 : 0;
            }

            export function reflect_delete_property() {
                let obj = { x: 1 };
                Reflect.deleteProperty(obj, 'x');
                return Reflect.has(obj, 'x') ? 1 : 0;
            }

            export function reflect_apply_basic() {
                function add(a, b) { return a + b; }
                return Reflect.apply(add, null, [3, 4]);
            }
        ",
    );
    validate(&module);

    assert_executes_in_all_runtimes(&module, "reflect_get_basic", &[], 42.0);
    assert_executes_in_all_runtimes(&module, "reflect_set_basic", &[], 99.0);
    assert_executes_in_all_runtimes(&module, "reflect_has_true", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "reflect_has_false", &[], 0.0);
    assert_executes_in_all_runtimes(&module, "reflect_delete_property", &[], 0.0);
    assert_executes_in_all_runtimes(&module, "reflect_apply_basic", &[], 7.0);
}

#[test]
fn executes_object_descriptor_manipulation() {
    let module = compile_module(
        "
            export function define_property_data() {
                let obj = {};
                Object.defineProperty(obj, 'x', { value: 42, writable: true, enumerable: true, configurable: true });
                return obj.x;
            }

            export function define_property_non_writable_blocks_write() {
                let obj = {};
                Object.defineProperty(obj, 'x', { value: 42, writable: false });
                obj.x = 100;
                return obj.x;
            }

            export function define_property_writable_allows_write() {
                let obj = {};
                Object.defineProperty(obj, 'x', { value: 42, writable: true });
                obj.x = 100;
                return obj.x;
            }

            export function define_property_accessor_getter() {
                let obj = {};
                Object.defineProperty(obj, 'x', { get: function() { return 7; } });
                return obj.x;
            }

            export function get_own_property_descriptor_value() {
                let obj = {};
                Object.defineProperty(obj, 'x', { value: 42, writable: false, enumerable: true, configurable: false });
                let d = Object.getOwnPropertyDescriptor(obj, 'x');
                return d.value;
            }

            export function get_own_property_descriptor_writable_flag() {
                let obj = {};
                Object.defineProperty(obj, 'x', { value: 42, writable: false, enumerable: true, configurable: false });
                let d = Object.getOwnPropertyDescriptor(obj, 'x');
                return d.writable ? 1 : 0;
            }

            export function get_own_property_descriptor_absent() {
                let obj = {};
                let d = Object.getOwnPropertyDescriptor(obj, 'missing');
                return !d ? 1 : 0;
            }
        ",
    );
    validate(&module);

    assert_executes_in_all_runtimes(&module, "define_property_data", &[], 42.0);
    assert_executes_in_all_runtimes(&module, "define_property_non_writable_blocks_write", &[], 42.0);
    assert_executes_in_all_runtimes(&module, "define_property_writable_allows_write", &[], 100.0);
    assert_executes_in_all_runtimes(&module, "define_property_accessor_getter", &[], 7.0);
    assert_executes_in_all_runtimes(&module, "get_own_property_descriptor_value", &[], 42.0);
    assert_executes_in_all_runtimes(
        &module,
        "get_own_property_descriptor_writable_flag",
        &[],
        0.0,
    );
    assert_executes_in_all_runtimes(&module, "get_own_property_descriptor_absent", &[], 1.0);
}

#[test]
fn executes_object_keys() {
    let module = compile_module(
        "
            export function keys_count() {
                let obj = { a: 1, b: 2, c: 3 };
                let keys = Object.keys(obj);
                return keys.length;
            }

            export function keys_first() {
                let obj = { z: 9 };
                let keys = Object.keys(obj);
                return obj[keys[0]];
            }

            export function keys_skip_non_enumerable() {
                let obj = {};
                Object.defineProperty(obj, 'hidden', { value: 1, enumerable: false });
                obj.visible = 2;
                let keys = Object.keys(obj);
                return keys.length;
            }

            export function keys_empty() {
                let obj = {};
                let keys = Object.keys(obj);
                return keys.length;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "keys_count", &[], 3.0);
    assert_executes_in_all_runtimes(&module, "keys_first", &[], 9.0);
    assert_executes_in_all_runtimes(&module, "keys_skip_non_enumerable", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "keys_empty", &[], 0.0);
}

#[test]
fn executes_object_values_entries_and_names() {
    let module = compile_module(
        "
            export function values_sum() {
                let obj = { a: 1, b: 2, c: 3 };
                let values = Object.values(obj);
                return values[0] + values[1] + values[2];
            }

            export function entries_first_value() {
                let obj = { x: 42 };
                let entries = Object.entries(obj);
                let pair = entries[0];
                return pair[1];
            }

            export function get_own_property_names_includes_non_enumerable() {
                let obj = {};
                Object.defineProperty(obj, 'hidden', { value: 1, enumerable: false });
                obj.visible = 2;
                let names = Object.getOwnPropertyNames(obj);
                return names.length;
            }

            export function get_own_property_descriptors_count() {
                let obj = { a: 1, b: 2 };
                let descriptors = Object.getOwnPropertyDescriptors(obj);
                return descriptors.a.value * 10 + descriptors.b.value;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "values_sum", &[], 6.0);
    assert_executes_in_all_runtimes(&module, "entries_first_value", &[], 42.0);
    assert_executes_in_all_runtimes(
        &module,
        "get_own_property_names_includes_non_enumerable",
        &[],
        2.0,
    );
    assert_executes_in_all_runtimes(&module, "get_own_property_descriptors_count", &[], 12.0);
}

#[test]
fn executes_object_assign_freeze_and_is_frozen() {
    let module = compile_module(
        "
            export function assign_merges_and_overwrites() {
                let target = { a: 1, b: 2 };
                let source = { b: 20, c: 30 };
                Object.assign(target, source);
                return target.a * 10000 + target.b * 100 + target.c;
            }

            export function freeze_blocks_write() {
                let obj = { a: 1 };
                Object.freeze(obj);
                obj.a = 99;
                return obj.a;
            }

            export function is_frozen_false_before_freeze() {
                let obj = { a: 1 };
                return Object.isFrozen(obj) ? 1 : 0;
            }

            export function is_frozen_true_after_freeze() {
                let obj = { a: 1 };
                Object.freeze(obj);
                return Object.isFrozen(obj) ? 1 : 0;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "assign_merges_and_overwrites", &[], 10000.0 + 2000.0 + 30.0);
    assert_executes_in_all_runtimes(&module, "freeze_blocks_write", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "is_frozen_false_before_freeze", &[], 0.0);
    assert_executes_in_all_runtimes(&module, "is_frozen_true_after_freeze", &[], 1.0);
}

#[test]
fn executes_reflect_own_keys() {
    let module = compile_module(
        "
            export function own_keys_includes_non_enumerable() {
                let obj = {};
                Object.defineProperty(obj, 'hidden', { value: 1, enumerable: false });
                obj.visible = 2;
                let keys = Reflect.ownKeys(obj);
                return keys.length;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "own_keys_includes_non_enumerable", &[], 2.0);
}

#[test]
fn executes_array_instance_methods() {
    let module = compile_module(
        "
            export function push_returns_new_length() {
                let arr = [1, 2];
                return arr.push(3, 4);
            }

            export function push_mutates_in_place() {
                let arr = [1, 2];
                arr.push(3);
                return arr[0] * 100 + arr[1] * 10 + arr[2];
            }

            export function pop_returns_last_and_shrinks() {
                let arr = [1, 2, 3];
                let last = arr.pop();
                let length = arr.length;
                return last * 100 + length;
            }

            export function pop_empty_returns_undefined_ish() {
                let arr = [];
                let last = arr.pop();
                return !last ? 1 : 0;
            }

            export function for_each_appends_doubled_values() {
                let arr = [1, 2, 3];
                arr.forEach(function (value, index, array) {
                    array.push(value * 2);
                });
                let length = arr.length;
                return length;
            }

            export function map_doubles_each_element() {
                let arr = [1, 2, 3];
                let doubled = arr.map(function (value) {
                    return value * 2;
                });
                return doubled[0] * 100 + doubled[1] * 10 + doubled[2];
            }

            export function plain_object_own_push_property_still_wins() {
                let obj = { push: 42 };
                return obj.push;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "push_returns_new_length", &[], 4.0);
    assert_executes_in_all_runtimes(&module, "push_mutates_in_place", &[], 123.0);
    assert_executes_in_all_runtimes(&module, "pop_returns_last_and_shrinks", &[], 302.0);
    assert_executes_in_all_runtimes(&module, "pop_empty_returns_undefined_ish", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "for_each_appends_doubled_values", &[], 6.0);
    assert_executes_in_all_runtimes(&module, "map_doubles_each_element", &[], 246.0);
    assert_executes_in_all_runtimes(
        &module,
        "plain_object_own_push_property_still_wins",
        &[],
        42.0,
    );
}


