use portal_jsc_swc_cfg::module::CfgModule;
use portal_jsc_swc_ssa::{SFunc, SValue, module::SModule};
use portal_jsc_swc_tac::{Item, module::TModule};
use portal_pc_waffle::{
    ExportKind, FuncDecl, FunctionBody, HeapType, Module, Operator, SignatureData, StorageType,
    Terminator, Type, ValueDef, WithMutablility,
};
use portal_pc_waffle::entity::EntityRef;
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
fn executes_primordial_fast_call_paths() {
    let module = compile_module(
        "
            export function numeric_sweep() {
                let sum = Math.abs(-2) + Math.floor(1.9) + Math.ceil(1.1)
                    + Math.trunc(2.9) + Math.sqrt(9) + Math.min(2, 3)
                    + Math.max(2, 3) + Math.sign(-8) + Math.round(2.5)
                    + Math.fround(1.5) + Math.imul(6, 7);
                return sum;
            }
            export function unshadowed_stays_fast() {
                let total = 0;
                total = total + Math.sqrt(16) + Math.abs(-4);
                return total * 10 + Math.min(1, 2);
            }
            export function shadowed_math_member_call() {
                let Math = { sqrt: function(x) { return x * x; } };
                return Math.sqrt(5);
            }
            export function param_named_math(Math) { return 1; }
            export function array_is_array_sweep() {
                return (Array.isArray([1]) ? 1 : 0) + (Array.isArray({}) ? 10 : 0)
                    + (Array.isArray(null) ? 100 : 0) + (Array.isArray(undefined) ? 1000 : 0)
                    + (Array.isArray(new Int8Array(1)) ? 10000 : 0);
            }
            export function typed_ctor_direct() {
                let values = new Uint8Array(3);
                values[1] = 250;
                return values.length * 1000 + values.byteLength * 10 + values[1];
            }
            export function typed_ctor_from_source() {
                let source = new Int16Array([5, 300]);
                let copy = new Uint8Array(source);
                return copy[0] * 100 + copy[1] + source.length;
            }
            export function typed_ctor_shadowed() {
                let Uint8Array = function(n) { return { tag: n * 3 }; };
                let fake = new Uint8Array(4);
                return fake.tag;
            }
        ",
    );
    validate(&module);
    // abs(-2) + floor(1.9) + ceil(1.1) + trunc(2.9) + sqrt(9) + min(2,3)
    // + max(2,3) + sign(-8) + round(2.5) + fround(1.5) + imul(6, 7)
    //   = 2 + 1 + 2 + 2 + 3 + 2 + 3 + (-1) + 3 + 1.5 + 42 = 60.5
    assert_executes_in_all_runtimes(&module, "numeric_sweep", &[], 60.5);
    assert_executes_in_all_runtimes(&module, "unshadowed_stays_fast", &[], 81.0);
    assert_executes_in_all_runtimes(&module, "shadowed_math_member_call", &[], 25.0);
    assert_executes_in_all_runtimes(&module, "param_named_math", &[0.0], 1.0);
    assert_executes_in_all_runtimes(&module, "array_is_array_sweep", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "typed_ctor_direct", &[], 3280.0);
    assert_executes_in_all_runtimes(&module, "typed_ctor_from_source", &[], 546.0);
    assert_executes_in_all_runtimes(&module, "typed_ctor_shadowed", &[], 12.0);
}

#[test]
fn executes_strict_equality_semantics() {
    let module = compile_module(
        "
            export function identity() {
                let a = { value: 1 };
                return a === a ? 1 : 0;
            }
            export function distinct_objects() {
                return { value: 1 } === { value: 1 } ? 1 : 0;
            }
            export function negated_identity() {
                let a = {};
                let b = {};
                return a !== b ? 1 : 0;
            }
            export function string_content() {
                let a = 'ab' + 'c';
                let b = 'a' + 'bc';
                return a === b && 'xyz' !== 'abc' ? 1 : 0;
            }
            export function string_vs_number() {
                return '5' === 5 || 5 === '5' ? 1 : 0;
            }
            export function boxed_number_crosses_call() {
                let object = { value: 5, get() { return this.value; } };
                let five = object.get();
                let number_part = five === 5 ? 1 : 0;
                let string_part = five === '5' ? 1 : 0;
                return number_part * 10 + string_part;
            }
            export function null_undefined_distinct() {
                let n = null;
                return n === null && !(n === undefined) && !(null === undefined) ? 1 : 0;
            }
            export function null_identity() {
                let n = null;
                return n === null && null === null ? 1 : 0;
            }
            export function same_view_identity() {
                let values = new Int8Array(2);
                let view = values.subarray(0, 2);
                return view === view && values !== view ? 1 : 0;
            }
            export function method_receiver_identity() {
                let object = { check(it) { return it === object; } };
                return object.check(object) && !object.check({}) ? 1 : 0;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "identity", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "distinct_objects", &[], 0.0);
    assert_executes_in_all_runtimes(&module, "negated_identity", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "string_content", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "string_vs_number", &[], 0.0);
    assert_executes_in_all_runtimes(&module, "boxed_number_crosses_call", &[], 10.0);
    assert_executes_in_all_runtimes(&module, "null_undefined_distinct", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "null_identity", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "same_view_identity", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "method_receiver_identity", &[], 1.0);
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
fn executes_native_wasm_gc_typed_array_integer_storage() {
    let module = compile_module(
        "
            export function run() {
                let values = new Int8Array(2);
                values[0] = 257;
                values[1] = -129;
                values[4] = 99;
                return values.length * 10000 + values.byteLength * 1000 + values[0] * 10 + values[1];
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "run", &[], 22137.0);
}

#[test]
fn executes_native_wasm_gc_typed_array_numeric_kinds() {
    let module = compile_module(
        "
            export function run() {
                let u8 = new Uint8Array(1);
                let u16 = new Uint16Array(1);
                let u32 = new Uint32Array(1);
                let i16 = new Int16Array(1);
                let i32 = new Int32Array(1);
                let f32 = new Float32Array(1);
                let f64 = new Float64Array(1);
                u8[0] = 4294967551;
                u16[0] = 65535;
                u32[0] = 4294967295;
                i16[0] = -1;
                i32[0] = -1;
                f32[0] = 16777217;
                f64[0] = 0.5;
                return u8[0] + u16[0] + u32[0] + i16[0] + i32[0] + f32[0] + f64[0];
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "run", &[], 4311810299.5);
}

#[test]
fn executes_native_wasm_gc_typed_array_views_and_metadata() {
    let module = compile_module(
        "
            export function view() {
                let values = new Uint8ClampedArray(4);
                values[0] = -3;
                values[1] = 1.5;
                values[2] = 255.5;
                let view = values.subarray(1, 3);
                view[0] = 9;
                return values[0] * 1000 + values[1] * 100 + values[2] * 10 + view.length;
            }
            export function metadata() {
                let values = new Float64Array(3);
                return Float64Array.BYTES_PER_ELEMENT * 100 + values.byteLength * 10 + (Array.isArray(values) ? 1 : 0);
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "view", &[], 3452.0);
    assert_executes_in_all_runtimes(&module, "metadata", &[], 1040.0);
}

#[test]
fn executes_native_wasm_gc_typed_array_sources_and_set() {
    let module = compile_module(
        "
            export function run() {
                let source = new Uint16Array([257, 3]);
                let copied = new Int8Array(source);
                let destination = new Uint8Array(4);
                destination.set([4, 5], 1);
                let overlap = new Uint8Array([1, 2, 3]);
                overlap.set(overlap.subarray(0, 2), 1);
                let copied_and_offset = copied[0] * 10000 + copied[1] * 1000 + destination[0] * 100 + destination[1] * 10 + destination[2];
                return copied_and_offset * 1000 + overlap[0] * 100 + overlap[1] * 10 + overlap[2];
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "run", &[], 13045112.0);
}

#[test]
fn compiles_native_wasm_gc_typed_array_constructor() {
    let _ = compile_module("export function make() { return new Uint8Array(2).length; }");
}

#[test]
fn validates_native_wasm_gc_typed_array_constructor() {
    let module = compile_module("export function make() { let a = new Uint8Array(2); a[0] = 3; return a[0]; }");
    validate(&module);
}

#[test]
fn executes_native_wasm_gc_typed_array_constructor() {
    let module = compile_module("export function make() { let a = new Uint8Array(2); a[0] = 3; return a[0]; }");
    validate(&module);
    assert_executes_in_all_runtimes(&module, "make", &[], 3.0);
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
#[test]
fn for_each_closure_writes_to_captured_array() {
    let module = compile_module(
        "
            export function for_each_push_to_other_array() {
                let arr = [10, 20, 30];
                let out = [];
                arr.forEach(function (value) {
                    out.push(value);
                });
                return out.length;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "for_each_push_to_other_array", &[], 3.0);
}

#[test]
fn for_each_closure_reads_and_writes_captured_variable() {
    let module = compile_module(
        "
            export function for_each_read_capture() {
                let arr = [10, 20, 30];
                let out = 7;
                let last = 0;
                arr.forEach(function (value) {
                    last = out;
                });
                return last;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "for_each_read_capture", &[], 7.0);
}

#[test]
fn call_then_prop_read_across_separate_statements() {
    // Same object, same mutating call, same later property read as the
    // `#[ignore]`d cases below -- but as separate statements rather than
    // operands of one arithmetic expression. This shape is unaffected by
    // the waffle- backend bug documented there, and must keep working.
    let module = compile_module(
        "
            export function push_then_length() {
                let arr = [1, 2, 3];
                arr.push(4);
                return arr.length;
            }
            export function plain_call_then_prop_read() {
                let o = { a: 1, b: 2 };
                function mutate(obj) {
                    obj.a = 99;
                }
                mutate(o);
                return o.a;
            }
            export function plain_call_then_other_prop_read() {
                let o = { a: 1, b: 2 };
                function mutate(obj) {
                    obj.a = 99;
                }
                return mutate(o), o.a;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "push_then_length", &[], 4.0);
    assert_executes_in_all_runtimes(&module, "plain_call_then_prop_read", &[], 99.0);
    assert_executes_in_all_runtimes(&module, "plain_call_then_other_prop_read", &[], 99.0);
}

#[test]
fn call_then_prop_read_combined_in_one_expression() {
    // Regression test for a waffle- backend bug: `Trees::compute`
    // (treeify.rs) could treeify a pure, single-use value across a block
    // boundary -- e.g. a call's boxed result used only by a later block's
    // arithmetic -- without checking that the consumer and producer share
    // a block. Since the Wasm value stack doesn't survive a branch, this
    // silently relocated side-effecting computations (including the call
    // itself) to wherever the owning value ended up, reordering them
    // relative to code the source placed earlier (like this test's
    // property read). Fixed upstream in
    // ~/Code-local/portal-hot/waffle-'s treeify.rs by requiring
    // same-block placement before claiming ownership.
    let module = compile_module(
        "
            export function fn_call_plus_prop_arith() {
                let o = { a: 1 };
                function mutateAndReturn(obj) {
                    obj.a = 99;
                    return 7;
                }
                return mutateAndReturn(o) * 100 + o.a;
            }
            export function push_call_plus_prop_arith() {
                let arr = [1, 2, 3];
                return arr.push(4) * 100 + arr.length;
            }
            export function pop_combined_expr() {
                let arr = [1, 2, 3];
                return arr.pop() * 100 + arr.length;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "fn_call_plus_prop_arith", &[], 799.0);
    assert_executes_in_all_runtimes(&module, "push_call_plus_prop_arith", &[], 404.0);
    assert_executes_in_all_runtimes(&module, "pop_combined_expr", &[], 302.0);
}

















#[test]
fn provable_math_call_reaches_fast_core_directly() {
    let module = compile_module(
        "
            export function run() { return Math.sqrt(9) + Math.abs(-1); }
        ",
    );
    validate(&module);
    let mut fast_core_called = false;
    for (_, decl) in module.funcs.entries() {
        if let FuncDecl::Body(_, name, body) = decl {
            if name.starts_with("js_body_") {
                for (_, def) in body.values.entries() {
                    if let ValueDef::Operator(Operator::Call { function_index }, _, _) = def {
                        let target = &module.funcs[*function_index];
                        if target.name().starts_with("js_fast_core_") {
                            fast_core_called = true;
                        }
                    }
                }
            }
        }
    }
    assert!(fast_core_called, "provable Math call must reach the fast core directly");
}

#[test]
fn executes_provenance_tracked_function_calls() {
    let module = compile_module(
        "
            export function local_direct() {
                let five = function() { return 5; };
                return five() + five() * 10;
            }
            export function local_with_args(x) {
                let double = function(v) { return v * 2; };
                return double(x) + double(x) / 2;
            }
            export function reassigned_local() {
                let f = function() { return 1; };
                let tag = 0;
                if (f() === 1) { tag = 10; }
                f = function() { return 2; };
                return tag + f() * 100;
            }
            export function method_direct() {
                let object = { v: 3, add: function(x) { return this.v + x; } };
                return object.add(4) + object.add(4) * 10;
            }
            export function method_overwritten() {
                let object = { v: 3, add: function(x) { return this.v + x; } };
                let first = object.add(1);
                object.add = function(x) { return 100 + x; };
                return first + object.add(2) * 10;
            }
            export function detached_method() {
                let object = { v: 3, add: function(x) { return (this === undefined ? 1 : 3) + x; } };
                let g = object.add;
                return g(4);
            }
            export function number_return_kind(x) {
                let inc = function(v) { return v + 1; };
                return inc(x) * 10 + inc(x);
            }
            export function mixed_return(x) {
                let pick = function(v) { return v > 0 ? 1 : v; };
                let echo = function(w) { return 42; };
                let r = pick(x);
                let w = echo(r);
                return w + (r > 0 ? 1 : 2);
            }
            export function shadowed_by_param(f) {
                return f === undefined ? 7 : 1;
            }
        ",
    );
    validate(&module);
    let bytes = wasm_bytes(&module);

    assert_eq!(
        execute_in_wasmtime(&bytes, "local_direct", &[]),
        55.0,
        "two calls through a single-assignment function literal"
    );
    assert_eq!(
        execute_in_wasmtime(&bytes, "local_with_args", &[7.0]),
        21.0,
        "direct native call passes positionally boxed formals"
    );
    assert_eq!(
        execute_in_wasmtime(&bytes, "reassigned_local", &[]),
        210.0,
        "the tag check must fall back after reassignment"
    );
    assert_eq!(
        execute_in_wasmtime(&bytes, "method_direct", &[]),
        77.0,
        "method dispatch keeps `this` wired to the receiver"
    );
    assert_eq!(
        execute_in_wasmtime(&bytes, "method_overwritten", &[]),
        4.0 + 102.0 * 10.0,
        "an overwritten method slot must take the fallback path (first=3+1, override=100+2)"
    );
    assert_eq!(
        execute_in_wasmtime(&bytes, "detached_method", &[]),
        5.0,
        "a detached method loses its receiver"
    );
    assert_eq!(
        execute_in_wasmtime(&bytes, "number_return_kind", &[4.0]),
        55.0,
        "a single-Number-kind callee returns raw f64 into the caller"
    );
    assert_eq!(
        execute_in_wasmtime(&bytes, "mixed_return", &[1.0]),
        43.0,
        "a mixed-kind callee keeps the boxed ABI and still works"
    );
    assert_eq!(
        execute_in_wasmtime(&bytes, "mixed_return", &[-1.0]),
        44.0,
        "a mixed-kind callee keeps the boxed ABI and still works"
    );
    assert_eq!(
        execute_in_wasmtime(&bytes, "shadowed_by_param", &[0.0]),
        1.0,
        "a parameter named like a literal local is a plain value call"
    );
}

#[test]
fn provenance_local_call_reaches_native_body_directly() {
    let module = compile_module(
        "
            export function run() {
                let five = function() { return 5; };
                let x = five();
                return x;
            }
        ",
    );
    validate(&module);
    let mut native_called = false;
    let mut body_count = 0;
    for (_, decl) in module.funcs.entries() {
        if let FuncDecl::Body(_, name, body) = decl {
            if name.starts_with("js_body_") {
                for (_, def) in body.values.entries() {
                    if let ValueDef::Operator(Operator::Call { function_index }, _, _) = def {
                        body_count += 1;
                        if module.funcs[*function_index].name().starts_with("js_body_") {
                            native_called = true;
                        }
                    }
                }
            }
        }
    }
    assert!(native_called, "a provenance-tracked call must hit the native body directly");
}

#[test]
fn provenance_single_kind_native_returns_raw() {
    let module = compile_module(
        "
            export function run(x) {
                let five = function() { return 5; };
                let echo = function(w) { return w; };
                return five() + echo(x);
            }
        ",
    );
    validate(&module);
    let mut raw_f64 = false;
    let mut boxed = false;
    for (_, decl) in module.funcs.entries() {
        if let FuncDecl::Body(sig, name, _) = decl {
            let returns = match &module.signatures[*sig] {
                SignatureData::Func { returns, .. } => returns.clone(),
                _ => continue,
            };
            if name.starts_with("js_body_") {
                if returns == vec![Type::F64] {
                    raw_f64 = true;
                }
                if returns.len() == 1 && returns[0] != Type::F64 {
                    boxed = true;
                }
            }
        }
    }
    assert!(raw_f64, "a single-Number-kind native body must declare an f64 return");
    assert!(boxed, "the Reference-kind callee must keep the boxed ABI");
}

#[test]
fn executes_boolean_exports_and_lazy_string_member_reads() {
    // Regression: every exported function used to RefCast its boxed result
    // to the number struct, so returning any non-number (even the boolean
    // of `1 === 1`) trapped `wasm trap: cast failure` at the export
    // boundary. The export wrapper now coerces through the ToNumber
    // surface instead.
    let module = compile_module(
        "
            export function const_boolean() { return 1 === 1; }
            export function context_compare(x) { let y = x; return y === x; }
            export function loose_context_compare(x) { let y = x; return y == x; }
            export function boolean_identity(x) { return x === x; }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "const_boolean", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "context_compare", &[1.0], 1.0);
    assert_executes_in_all_runtimes(&module, "loose_context_compare", &[1.0], 1.0);
    assert_executes_in_all_runtimes(&module, "boolean_identity", &[1.0], 1.0);

    // Regression: member reads on lazily produced strings (the result of a
    // string index read, whose static String representation is lost at the
    // block boundary) fell through to the generic property lookup and read
    // `undefined`. `length` must answer from the UTF-16 representation and
    // numeric indexes from the string's code units.
    let module = compile_module(
        "
            export function indexed_length() { return 'abc'[1].length; }
            export function indexed_length_binding() {
                let unit = 'abc'[1];
                return unit.length;
            }
            export function indexed_length_via_concat() { return ('x' + 'abc'[1]).length; }
            export function indexed_return() { return 'abc'[1] === 'b' ? 1 : 0; }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "indexed_length", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "indexed_length_binding", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "indexed_length_via_concat", &[], 2.0);
    assert_executes_in_all_runtimes(&module, "indexed_return", &[], 1.0);
}

#[test]
fn executes_ternary_returned_from_exported_functions() {
    // Regression: in the module pipeline, converting a terminator whose
    // expression splits blocks (`return c ? a : b;`) landed the term on the
    // pre-split block, so the function returned the condition instead of
    // the selected arm (or trapped downstream). Direct returns, let-bound
    // returns, and calls through literals must all select the right arm.
    let module = compile_module(
        "
            export function direct(x) { return x > 0 ? x : {}; }
            export function number_or_zero(x) { return x > 0 ? x : 0; }
            export function let_bound(x) { let z = x > 0 ? x : {}; return z; }
            export function through_literal(x) {
                let pick = function(v) { return v > 0 ? v : {}; };
                return pick(x);
            }
            export function through_literal_sum(x) {
                let pick = function(v) { return v > 0 ? v : 0; };
                let y = pick(x);
                return y + 0;
            }
            export function through_literal_object_arm(x) {
                let pick = function(v) { return v > 0 ? v : {}; };
                let y = pick(x);
                // `{} + 0` is NaN in JavaScript (ToNumber of a plain
                // object), and NaN compares false; assert via a
                // comparison so the numeric export ABI stays f64.
                return y + 0 > 0 ? 1 : 0;
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "direct", &[1.0], 1.0);
    assert_executes_in_all_runtimes(&module, "number_or_zero", &[1.0], 1.0);
    assert_executes_in_all_runtimes(&module, "number_or_zero", &[-1.0], 0.0);
    assert_executes_in_all_runtimes(&module, "let_bound", &[1.0], 1.0);
    assert_executes_in_all_runtimes(&module, "through_literal", &[1.0], 1.0);
    assert_executes_in_all_runtimes(&module, "through_literal_sum", &[1.0], 1.0);
    assert_executes_in_all_runtimes(&module, "through_literal_sum", &[-1.0], 0.0);
    assert_executes_in_all_runtimes(&module, "through_literal_object_arm", &[-1.0], 0.0);
}

// ── Multi-module ingestion ────────────────────────────────────────────────

type Fixture<'a> = &'a [(&'a str, &'a str)];

fn lower_modules(
    fixtures: Fixture<'_>,
    entry: &str,
    options: &portal_jsc_waffle::ConvertOptions,
) -> Result<Module<'static>, portal_jsc_waffle::ConvertError> {
    GLOBALS.set(&Globals::default(), || {
        let cm: Lrc<SourceMap> = Lrc::new(SourceMap::default());
        let mut set = portal_jsc_waffle::ModuleSet::new();
        for (path, source) in fixtures {
            let file = cm.new_source_file(
                Lrc::new(FileName::Custom((*path).into())),
                (*source).to_owned(),
            );
            let mut errors = vec![];
            let module = parse_file_as_module(
                &file,
                Syntax::Es(EsSyntax::default()),
                EsVersion::Es2022,
                None,
                &mut errors,
            )
            .expect("module fixture should parse");
            assert!(errors.is_empty(), "parser diagnostics: {errors:?}");
            let cfg = CfgModule::try_from(module).expect("CFG lowering should succeed");
            let tac = TModule::try_from(cfg).expect("TAC lowering should succeed");
            let ssa = SModule::try_from(tac).expect("SSA lowering should succeed");
            set.insert(*path, Box::leak(Box::new(ssa)))?;
        }
        let mut wasm = Module::empty();
        portal_jsc_waffle::convert_modules(entry, &set, &mut wasm, options)?;
        Ok(wasm)
    })
}

fn compile_modules(fixtures: Fixture<'_>, entry: &str) -> Module<'static> {
    lower_modules(fixtures, entry, &portal_jsc_waffle::ConvertOptions::default())
        .expect("multi-module lowering should succeed")
}

#[test]
fn links_cross_module_imports_and_executes_in_all_runtimes() {
    let fixtures: Fixture<'_> = &[
        ("main.mjs", "import { add } from './lib.js'; export function run(a) { return add(a, 10); }"),
        ("lib.js", "export function add(a, b) { return a + b; }"),
    ];
    let module = compile_modules(fixtures, "main.mjs");
    validate(&module);
    assert_executes_in_all_runtimes(&module, "run", &[5.0], 15.0);
}

#[test]
fn links_transitive_import_chains() {
    let fixtures: Fixture<'_> = &[
        ("main.mjs", "import { c } from './a.js'; export function run(a) { return c(a) + 1; }"),
        ("a.js", "import { b } from './b.js'; export function c(v) { return b(v) * 2; }"),
        ("b.js", "export function b(v) { return v + 3; }"),
    ];
    let module = compile_modules(fixtures, "main.mjs");
    validate(&module);
    assert_executes_in_all_runtimes(&module, "run", &[4.0], 15.0);
}

#[test]
fn links_mutual_recursion_across_modules() {
    let fixtures: Fixture<'_> = &[
        (
            "main.mjs",
            "import { even } from './parity.js'; export function run(a) { return even(a); }",
        ),
        (
            "parity.js",
            "import { odd } from './flip.js'; export function even(n) { if (n === 0) return 1; return odd(n - 1); }",
        ),
        (
            "flip.js",
            "import { even } from './parity.js'; export function odd(n) { if (n === 0) return 0; return even(n - 1); }",
        ),
    ];
    let module = compile_modules(fixtures, "main.mjs");
    validate(&module);
    assert_executes_in_all_runtimes(&module, "run", &[10.0], 1.0);
    assert_executes_in_all_runtimes(&module, "run", &[7.0], 0.0);
}

#[test]
fn links_default_imports() {
    let fixtures: Fixture<'_> = &[
        ("main.mjs", "import triple from './lib.js'; export function run(a) { return triple(a); }"),
        ("lib.js", "export default function(v) { return v * 3; }"),
    ];
    let module = compile_modules(fixtures, "main.mjs");
    validate(&module);
    assert_executes_in_all_runtimes(&module, "run", &[6.0], 18.0);
}

#[test]
fn exports_full_main_module_surface_with_reexports() {
    let fixtures: Fixture<'_> = &[
        (
            "main.mjs",
            "export { helper as util } from './lib.js'; export * from './extra.js'; export function local() { return 1; } export const constant = 2;",
        ),
        ("lib.js", "export function helper() { return 2; }"),
        ("extra.js", "export function alpha(v) { return v; } export function beta(v) { return v; } export default function() { return 9; }"),
    ];
    let options = portal_jsc_waffle::ConvertOptions {
        gc_export_suffix: Some("$gc".to_owned()),
        ..Default::default()
    };
    let module = lower_modules(fixtures, "main.mjs", &options)
        .expect("re-export lowering should succeed");
    validate(&module);
    let mut names: Vec<_> = module.exports.iter().map(|e| e.name.as_str()).collect();
    names.sort();
    assert_eq!(
        names,
        vec![
            "alpha", "alpha$gc", "beta", "beta$gc", "local", "local$gc", "util", "util$gc",
        ],
        "star re-exports fill the surface minus local names and default; \
         non-function locals keep only a name reservation"
    );
    assert_executes_in_all_runtimes(&module, "alpha", &[1.0], 1.0);
    assert_executes_in_all_runtimes(&module, "util", &[], 2.0);
}

#[test]
fn star_exports_do_not_shadow_local_declarations() {
    let fixtures: Fixture<'_> = &[
        ("main.mjs", "export * from './lib.js'; export function pick() { return 1; }"),
        ("lib.js", "export function pick() { return 2; } export function other() { return 3; }"),
    ];
    let module = compile_modules(fixtures, "main.mjs");
    validate(&module);
    let names: Vec<_> = module.exports.iter().map(|e| e.name.as_str()).collect();
    assert!(names.contains(&"other"), "star export survives: {names:?}");
    assert_executes_in_all_runtimes(&module, "pick", &[], 1.0);
}

#[test]
fn rejects_missing_and_bare_module_specifiers() {
    let missing: Fixture<'_> = &[("main.mjs", "import { x } from './nope.js'; export function run() { return x(); }")];
    let error = lower_modules(missing, "main.mjs", &Default::default())
        .expect_err("a missing target module must be rejected");
    assert!(error.to_string().contains("nope.js"), "{error}");
    assert!(error.to_string().contains("main.mjs"), "{error}");

    let bare: Fixture<'_> = &[("main.mjs", "import fs from 'fs'; export function run() { return fs(); }")];
    let error = lower_modules(bare, "main.mjs", &Default::default())
        .expect_err("a bare specifier must be rejected");
    assert!(error.to_string().contains("only relative"), "{error}");
}

#[test]
fn rejects_unlinkable_import_forms() {
    let star: Fixture<'_> = &[
        ("main.mjs", "import * as ns from './lib.js'; export function run() { return ns.f(); }"),
        ("lib.js", "export function f() { return 1; }"),
    ];
    let error = lower_modules(star, "main.mjs", &Default::default())
        .expect_err("namespace imports must be rejected in this milestone");
    assert!(error.to_string().contains("namespace"), "{error}");

    let value: Fixture<'_> = &[
        ("main.mjs", "import { count } from './lib.js'; export function run() { return count + 1; }"),
        ("lib.js", "export const count = 1;"),
    ];
    let error = lower_modules(value, "main.mjs", &Default::default())
        .expect_err("non-function imports must be rejected");
    assert!(error.to_string().contains("not a hoisted function"), "{error}");

    let missing_name: Fixture<'_> = &[
        ("main.mjs", "import { absent } from './lib.js'; export function run() { return absent(); }"),
        ("lib.js", "export function present() { return 1; }"),
    ];
    let error = lower_modules(missing_name, "main.mjs", &Default::default())
        .expect_err("an unexported name must be rejected");
    assert!(error.to_string().contains("absent"), "{error}");
}

#[test]
fn rejects_unsupported_main_module_export_forms() {
    let default_expr: Fixture<'_> = &[("main.mjs", "export default 1 + 2;")];
    let error = lower_modules(default_expr, "main.mjs", &Default::default())
        .expect_err("export default <expr> must be rejected, not silently skipped");
    assert!(error.to_string().contains("export default"), "{error}");

    let star_as: Fixture<'_> = &[
        ("main.mjs", "export * as ns from './lib.js'; export function run() { return 0; }"),
        ("lib.js", "export function f() { return 1; }"),
    ];
    let error = lower_modules(star_as, "main.mjs", &Default::default())
        .expect_err("export * as ns must be rejected in this milestone");
    assert!(error.to_string().contains("not supported"), "{error}");
}

#[test]
fn rejects_missing_entry_and_duplicate_paths() {
    let fixtures: Fixture<'_> = &[("lib.js", "export function f() { return 1; }")];
    let error = lower_modules(fixtures, "absent.mjs", &Default::default())
        .expect_err("a missing entry must be rejected");
    assert!(error.to_string().contains("absent.mjs"), "{error}");

    GLOBALS.set(&Globals::default(), || {
        let cm: Lrc<SourceMap> = Lrc::new(SourceMap::default());
        let file = cm.new_source_file(
            Lrc::new(FileName::Custom("a.js".into())),
            "export function f() { return 1; }".to_owned(),
        );
        let mut errors = vec![];
        let module = parse_file_as_module(
            &file,
            Syntax::Es(EsSyntax::default()),
            EsVersion::Es2022,
            None,
            &mut errors,
        )
        .expect("module fixture should parse");
        let cfg = CfgModule::try_from(module).expect("CFG lowering should succeed");
        let tac = TModule::try_from(cfg).expect("TAC lowering should succeed");
        let ssa = SModule::try_from(tac).expect("SSA lowering should succeed");
        let leaked: &'static SModule = Box::leak(Box::new(ssa));
        let mut set = portal_jsc_waffle::ModuleSet::new();
        set.insert("a.js", leaked).expect("first insert should succeed");
        let error = set
            .insert("a.js", leaked)
            .expect_err("a duplicate module path must be rejected");
        assert!(error.to_string().contains("duplicate"), "{error}");
    });
}

#[test]
fn rejects_ambiguous_star_exports() {
    let fixtures: Fixture<'_> = &[
        ("main.mjs", "export * from './a.js'; export * from './b.js';"),
        ("a.js", "export function clash() { return 1; }"),
        ("b.js", "export function clash() { return 2; }"),
    ];
    let error = lower_modules(fixtures, "main.mjs", &Default::default())
        .expect_err("two different functions under one star-exported name must be rejected");
    assert!(error.to_string().contains("ambiguous"), "{error}");
}

#[test]
fn rejects_circular_reexport_chains() {
    let fixtures: Fixture<'_> = &[
        ("main.mjs", "export { ping } from './a.js'; export function run() { return 0; }"),
        ("a.js", "export { pong } from './b.js'; export const ping = 1;"),
        ("b.js", "export { ping } from './a.js'; export const pong = 2;"),
    ];
    let error = lower_modules(fixtures, "main.mjs", &Default::default())
        .expect_err("a circular re-export chain must be rejected");
    assert!(
        error.to_string().contains("circular") || error.to_string().contains("not a hoisted"),
        "{error}"
    );
}

// ── Tail calls ────────────────────────────────────────────────────────────

/// Count static `return_call` terminators (direct tail calls) in a module.
fn count_return_call(module: &Module<'_>) -> usize {
    let mut count = 0;
    for (_, decl) in module.funcs.entries() {
        if let FuncDecl::Body(_, _, body) = decl {
            for block in body.blocks.iter() {
                if let Terminator::ReturnCall { .. } = body.blocks[block].terminator.terminator {
                    count += 1;
                }
            }
        }
    }
    count
}

#[test]
fn executes_deep_tail_recursion_end_to_end() {
    // `return f(...)` is a `TTerm::Tail` in jsaw-core SSA: the frame-
    // replacing tail call must keep 100k-deep self-recursion through a
    // provenance-tracked local literal within the host stack, where an
    // accumulating form would overflow.
    let module = compile_module(
        "
            export function run(n) {
                let f = function(k, acc) {
                    if (k <= 0) { return acc; }
                    return f(k - 1, acc + k);
                };
                return f(n, 0);
            }
        ",
    );
    validate(&module);
    let bytes = wasm_bytes(&module);
    for (runtime, result) in [
        ("Wasmtime", execute_in_wasmtime(&bytes, "run", &[100_000.0])),
        ("Node.js", execute_in_node(&bytes, "run", &[100_000.0])),
    ] {
        assert!(
            (result - 5_000_050_000.0).abs() < f64::EPSILON,
            "{runtime} deep tail recursion returned {result}; expected 100000",
        );
    }
}

#[test]
fn executes_mutual_tail_recursion_end_to_end() {
    // Even/odd as two mutually-referencing locals: `odd` closes over
    // `even` before `even` holds a literal, so `odd`'s tail call cannot be
    // proven fresh and keeps the guarded/generic adapter path — one host
    // frame per hop. Depth is bounded (1000) to document that the fallback
    // is a *correct* tail call but not O(1); the O(1) claim is covered by
    // `executes_deep_tail_recursion_end_to_end` (a single fresh literal).
    let module = compile_module(
        "
            export function run(n) {
                let even = null;
                let odd = function(k) { if (k === 0) { return 0; } return even(k - 1); };
                even = function(k) { if (k === 0) { return 1; } return odd(k - 1); };
                return even(n);
            }
        ",
    );
    validate(&module);
    let bytes = wasm_bytes(&module);
    for (runtime, result) in [
        ("Wasmtime", execute_in_wasmtime(&bytes, "run", &[1000.0])),
        ("Node.js", execute_in_node(&bytes, "run", &[1000.0])),
    ] {
        assert!(
            (result - 1.0).abs() < f64::EPSILON,
            "{runtime} mutual tail recursion returned {result}; expected 1",
        );
    }
}

#[test]
fn direct_tail_dispatch_to_proven_literal_uses_return_call() {
    // A provable self-recursive tail call dispatches straight to the
    // native body: `return f(k - 1, acc + k)` keeps the literal origin,
    // so at least one static `return_call` must exist in the module.
    let module = compile_module(
        "
            export function run(n) {
                let f = function(k, acc) {
                    if (k <= 0) { return acc; }
                    return f(k - 1, acc + k);
                };
                return f(n, 0);
            }
        ",
    );
    assert!(
        count_return_call(&module) > 0,
        "a proven tail callee must emit a static return_call"
    );
}

#[test]
fn tail_call_result_still_correct_through_guarded_dispatch() {
    // A retained (non-fresh) literal keeps the runtime tag check: the fast
    // arm tail-calls the native body, the slow arm keeps the generic
    // adapter tail call. Both arms must return the callee's result.
    let module = compile_module(
        "
            export function run(n) {
                let f = function(x) { return x + 1; };
                let saved = f;
                return f(n);
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "run", &[41.0], 42.0);
}

#[test]
fn tail_call_fallback_arm_matches_generic_dispatch_result() {
    // Rebinding between the literal and the tail call must route through
    // the fallback and still produce the rebound callee's result.
    let module = compile_module(
        "
            export function run(n) {
                let f = function(x) { return x + 1; };
                let probe = f;
                f = function(x) { return x + 2; };
                if (n === 1000) { return f(n); }
                return f(n);
            }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "run", &[41.0], 43.0);
}

#[test]
fn dump_ssa_values_for_recursion() {
    let ssa = script_ssa(
        "
            function run(n) {
                if (n <= 0) { return 0; }
                return run(n - 1);
            }
            run(3);
        ",
    );
    for (id, value) in ssa.cfg.values.iter() {
        println!("VALUE {id:?} = {:?}", value.value);
    }
}














#[test]
fn multi_return_propagates_through_multi_kind_caller() {
    // `middle` returns `pick(w)` directly — its analysis is exactly
    // pick's multi-kind union, so middle's native ABI is also the union
    // and the call result forwards without a per-arm re-lowering (the
    // union flows through as one value into middle's return packing).
    // The topmost export splits the tag and converts to f64.
    let module = compile_module(
        "
            export function run(x) {
                let pick = function(v) { if (v > 0) { return v * 1.5; } return null; };
                let middle = function(w) { return pick(w); };
                return middle(x);
            }
        ",
    );
    validate(&module);
    let bytes = wasm_bytes(&module);
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[2.0]), 3.0);
    // null through ToNumber is 0.
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[-1.0]), 0.0);
}

#[test]
fn multi_return_core_inspection_shows_union_abi() {
    let module = compile_module(
        "
            export function run(x) {
                let pick = function(v) { if (v > 0) { return v * 1.5; } return null; };
                return pick(x) + 1;
            }
        ",
    );
    validate(&module);
    // The pick native's signature returns the multi union struct type.
    let multi = wasm_bytes_signature_check(&module);
    assert!(multi, "pick native should return the multi union type");
}

fn wasm_bytes_signature_check(module: &Module) -> bool {
    let mut found = false;
    let multi_ty = {
        // Locate the multi struct: a 4-field struct (i32, anyref, i32, f64).
        for (_, sig) in module.signatures.entries() {
            if let SignatureData::Struct { fields, .. } = sig {
                if fields.len() == 4 {
                    let tys: Vec<Type> = fields
                        .iter()
                        .map(|f: &WithMutablility<StorageType>| match f.value {
                            StorageType::Val(t) => t,
                            _ => Type::I32,
                        })
                        .collect();
                    // tag i32, r anyref, i i32, f f64
                    if tys.len() == 4
                        && tys[0] == Type::I32
                        && tys[2] == Type::I32
                        && tys[3] == Type::F64
                    {
                        found = true;
                    }
                }
            }
        }
        found
    };
    let _ = multi_ty;
    found
}

#[test]
fn guarded_multi_kind_callee_produces_correct_values_on_both_arms() {
    // A retained (non-fresh) multi-kind literal: the tag check's fast arm
    // unpacks the union per kind; the slow arm boxes through the adapter.
    let module = compile_module(
        "
            export function run(x) {
                let pick = function(v) { if (v > 0) { return v * 1.5; } return null; };
                let probe = pick;
                let a = probe(x);
                let b = pick(x);
                return (a + 1) + (b === null ? 100 : 200) * (a > 0 ? 1 : 0);
            }
        ",
    );
    validate(&module);
    let bytes = wasm_bytes(&module);
    // x=2: a=3.0, b=3.0 -> (3+1) + 200*1 = 204
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[2.0]), 204.0);
    // x=-1: a=null -> +1 is 1 (ToNumber(null)=0, +1=1); b=null -> 100; a>0 false -> 0 => 1 + 0 = 1
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[-1.0]), 1.0);
}

#[test]
fn multi_kind_singleton_regression_no_union_in_all_single_module() {
    // A module whose functions are all single-kind must not use the
    // multi union ABI: no function signature may return the 4-field
    // (i32, anyref, i32, f64) struct (the struct *type* is minted eagerly
    // with the module's other fixed layouts, which is harmless).
    let module = compile_module(
        "
            export function run(x) {
                let inc = function(v) { return v + 1; };
                let flag = function(v) { return v > 0; };
                let str = function(v) { return '' + v; };
                return inc(x) + (flag(x) ? 1 : 0) + str(x).length;
            }
        ",
    );
    validate(&module);
    // Locate the multi struct type, then assert no func signature returns it.
    let multi_sig = module.signatures.entries().find_map(|(sid, sig)| {
        matches!(sig, SignatureData::Struct { fields, .. } if {
            fields.len() == 4
                && {
                    let tys: Vec<Type> = fields
                        .iter()
                        .map(|f: &WithMutablility<StorageType>| match f.value {
                            StorageType::Val(t) => t,
                            _ => Type::I32,
                        })
                        .collect();
                    tys[0] == Type::I32 && tys[2] == Type::I32 && tys[3] == Type::F64
                }
        }).then_some(sid)
    });
    if let Some(multi_sig) = multi_sig {
        let multi_ret = portal_pc_waffle::Type::Heap(portal_pc_waffle::WithNullable {
            value: portal_pc_waffle::HeapType::Sig { sig_index: multi_sig },
            nullable: false,
        });
        for (_, func) in module.funcs.entries() {
            if let FuncDecl::Body(sig, name, _) = func {
                if let SignatureData::Func { returns, .. } = &module.signatures[*sig] {
                    assert_ne!(
                        returns.as_slice(),
                        &[multi_ret],
                        "function {name} must not return the multi union"
                    );
                }
            }
        }
    }
}

#[test]
fn tail_call_out_of_multi_kind_caller_downgrades_to_call_and_pack() {
    // middle is multi-kind AND contains a tail call. With tail-kind
    // unioning (Milestone 4) middle's tail into the same-set union core
    // pick is a direct frame-replacing `ReturnCall` (same union struct,
    // kinds contained) — the former interlock downgrade no longer fires;
    // this test now pins the forwarding behavior and the value semantics.
    let module = compile_module(
        "
            export function run(x) {
                let pick = function(v) { if (v > 0) { return v * 1.5; } return null; };
                let middle = function(w) { if (w === 0) { return null; } return pick(w); };
                return middle(x);
            }
        ",
    );
    validate(&module);
    let bytes = wasm_bytes(&module);
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[2.0]), 3.0);
    // w=0 -> null; x=-1 -> pick(-1) = null -> 0 via ToNumber.
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[0.0]), 0.0);
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[-1.0]), 0.0);
}

#[test]
fn multi_call_site_splits_into_tag_continuations_per_payload_kind() {
    // Inspection: a consumer of a multi-kind callee must branch on the
    // runtime tag, and each continuation consumes the raw payload (an
    // f64 arithmetic consumer sees the raw f64, no boxed round trip).
    let module = compile_module(
        "
            export function run(x) {
                let pick = function(v) { if (v > 0) { return v * 1.5; } return null; };
                return pick(x) + 1;
            }
        ",
    );
    validate(&module);
    // The run export calls the pick adapter (generic path), whose result is
    // boxed; the *direct* call site in run's own body is not present since
    // pick is reached through the export machinery — so assert on the pick
    // native's signature returning its dedicated union layout. pick's
    // kinds are {Number, Reference}: layout `rf` = (tag i32, r anyref,
    // f f64), 3 fields, no i32 slot.
    let multi_ret_count = module.signatures.entries().filter(|(_, sig)| {
        matches!(sig, SignatureData::Func { returns, .. } if returns.len() == 1 && {
            matches!(&returns[0], Type::Heap(w) if {
                matches!(&w.value, portal_pc_waffle::HeapType::Sig { sig_index } if {
                    matches!(&module.signatures[*sig_index], SignatureData::Struct { fields, .. } if {
                        fields.len() == 3 && {
                            let tys: Vec<Type> = fields
                                .iter()
                                .map(|f: &WithMutablility<StorageType>| match f.value {
                                    StorageType::Val(t) => t,
                                    _ => Type::I32,
                                })
                                .collect();
                            tys[0] == Type::I32
                                && tys[1]
                                    == Type::Heap(portal_pc_waffle::WithNullable {
                                        value: portal_pc_waffle::HeapType::Any,
                                        nullable: true,
                                    })
                                && tys[2] == Type::F64
                        }
                    })
                })
            })
        })
    }).count();
    assert!(
        multi_ret_count >= 1,
        "at least one native must return the dedicated rf union layout"
    );
}

#[test]
fn raw_tail_recursion_stays_raw_and_constant_stack() {
    // A self-recursive core whose kinds are provably {Number} keeps the
    // raw f64 ABI *and* the frame-replacing tail: the callee's native
    // return type equals the caller's, so `return f(n - 1)` is a direct
    // `return_call` with no boxing anywhere on the recursion path.
    let module = compile_module(
        "
            export function run(n) {
                let f = function(k) {
                    if (k <= 0) { return 0; }
                    return f(k - 1);
                };
                return f(n);
            }
        ",
    );
    validate(&module);
    // The f native must return raw f64 and tail-call itself: find the
    // body whose signature returns f64 and check it contains a
    // `return_call` to its own function index.
    let mut raw_f64 = false;
    let mut self_tail = false;
    for (fid, decl) in module.funcs.entries() {
        let FuncDecl::Body(sig, name, body) = decl else {
            continue;
        };
        if !name.starts_with("js_body") {
            continue;
        }
        let is_raw = matches!(
            &module.signatures[*sig],
            SignatureData::Func { returns, .. } if returns.first() == Some(&Type::F64)
        );
        if !is_raw {
            continue;
        }
        raw_f64 = true;
        for block in body.blocks.iter() {
            if let Terminator::ReturnCall { func, .. } = body.blocks[block].terminator.terminator {
                if func == fid {
                    self_tail = true;
                }
            }
        }
    }
    assert!(raw_f64, "self-recursive core should keep the raw f64 ABI");
    assert!(
        self_tail,
        "self-tail should be a direct return_call to the same native"
    );
    let bytes = wasm_bytes(&module);
    for (runtime, result) in [
        ("Wasmtime", execute_in_wasmtime(&bytes, "run", &[100_000.0])),
        ("Node.js", execute_in_node(&bytes, "run", &[100_000.0])),
    ] {
        assert_eq!(result, 0.0, "{runtime} raw tail recursion at depth 100000");
    }
}

#[test]
fn union_tail_recursion_forwards_untouched() {
    // A self-recursive core returning a number on one branch and null on
    // the other analyzes as {Number, Reference}: the union ABI. Its own
    // tail forwards the packed union as-is (same struct type, kinds
    // contained), so the recursion path must have no tag-splitting. The
    // O(1) stack claim is observed by executing at depth 100000.
    let module = compile_module(
        "
            export function run(n) {
                let f = function(k) {
                    if (k <= 0) { return null; }
                    if (k === 1) { return 1.5; }
                    return f(k - 2);
                };
                return f(n);
            }
        ",
    );
    validate(&module);
    let bytes = wasm_bytes(&module);
    // f(6): 6 -> 4 -> 2 -> 0 => null (ToNumber(null) = 0)
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[6.0]), 0.0);
    // f(7): 7 -> 5 -> 3 -> 1 => 1.5
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[7.0]), 1.5);
    // Depth 100000 only survives if the tail is frame-replacing.
    for (runtime, result) in [
        ("Wasmtime", execute_in_wasmtime(&bytes, "run", &[100_000.0])),
        ("Node.js", execute_in_node(&bytes, "run", &[100_000.0])),
    ] {
        assert_eq!(result, 0.0, "{runtime} union tail recursion at depth 100000");
    }
}

#[test]
fn unknown_tail_callee_falls_back_to_reference() {
    // A tail call to a callee whose provenance is unknown (a parameter
    // holding an arbitrary function) must not promise a raw/union ABI:
    // the boxed join is the only safe claim. The unknown-callee tail
    // keeps a generic adapter hop from a boxed caller and everything
    // still executes correctly.
    let module = compile_module(
        "
            export function run(n) {
                let go = function(cb, k) {
                    if (k <= 0) { return 42; }
                    return cb(cb, k - 1);
                };
                return go(go, n);
            }
        ",
    );
    validate(&module);
    let bytes = wasm_bytes(&module);
    for (runtime, result) in [
        ("Wasmtime", execute_in_wasmtime(&bytes, "run", &[500.0])),
        ("Node.js", execute_in_node(&bytes, "run", &[500.0])),
    ] {
        assert_eq!(result, 42.0, "{runtime} unknown-callee tail recursion");
    }
}

#[test]
fn mismatched_tail_abis_downgrade_and_stay_correct() {
    // A {Number}-only core tail-calls a {Number, Reference} union core:
    // the callee's kinds are not contained in the caller's, so the tail
    // downgrades to a framed direct call + convert + return — correct on
    // both arms, just not frame-replacing (bounded depth here).
    let module = compile_module(
        "
            export function run(n) {
                let pick = function(k) {
                    if (k <= 0) { return null; }
                    return 2.5;
                };
                let consumer = function(k) {
                    if (k <= 0) { return 7; }
                    return pick(k - 1);
                };
                let v = consumer(n);
                return v === null ? 0 : v + 1;
            }
        ",
    );
    validate(&module);
    let bytes = wasm_bytes(&module);
    // run(3): consumer(3) -> pick(2) = 2.5 -> +1 = 3.5
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[3.0]), 3.5);
    // run(-1): consumer(-1) = 7 -> 8
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[-1.0]), 8.0);
}

#[test]
fn cross_module_tail_chain_executes_on_both_parities() {
    // An import binding's origin is statically proven (ESM bindings
    // cannot be rebound), so a cross-module tail dispatches straight to
    // the linked native body where the ABIs match. Values must be exact
    // on both parities of the even/odd-style cycle.
    let fixtures: Fixture<'_> = &[
        (
            "main.mjs",
            "import { down } from './a.js'; export function run(n) { return down(n); }",
        ),
        (
            "a.js",
            "import { up } from './b.js'; export function down(n) { if (n <= 0) return 1; return up(n - 1); }",
        ),
        (
            "b.js",
            "import { down } from './a.js'; export function up(n) { if (n <= 0) return 2; return down(n - 1); }",
        ),
    ];
    let module = compile_modules(fixtures, "main.mjs");
    validate(&module);
    let bytes = wasm_bytes(&module);
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[101.0]), 2.0);
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[100.0]), 1.0);
}

/// Find a native `js_body_*` body whose signature returns a struct with
/// exactly `ty` field types (by value equality), returning its body id.
fn body_returning_struct_with_fields<'m>(
    module: &'m Module<'_>,
    want: &[Type],
) -> Option<(usize, &'m FunctionBody)> {
    let mut found = None;
    for (fid, decl) in module.funcs.entries() {
        let FuncDecl::Body(sig, name, body) = decl else {
            continue;
        };
        if !name.starts_with("js_body") {
            continue;
        }
        if let SignatureData::Func { returns, .. } = &module.signatures[*sig] {
            if returns.len() != 1 {
                continue;
            }
            let is_match = matches!(&returns[0], Type::Heap(w) if {
                matches!(&w.value, portal_pc_waffle::HeapType::Sig { sig_index } if {
                    matches!(&module.signatures[*sig_index], SignatureData::Struct { fields, .. } if {
                        fields.len() == want.len()
                            && fields.iter().zip(want).all(|(f, t)| match f.value {
                                StorageType::Val(ty) => &ty == t,
                                _ => false,
                            })
                    })
                })
            });
            if is_match {
                found = Some((fid.index(), body));
            }
        }
    }
    found
}

#[test]
fn boolean_float_core_returns_dedicated_layout() {
    // A {Boolean, Number} core returns the dedicated `if` layout
    // (tag i32, i i32, f f64) — no reference slot, no dead fields.
    let module = compile_module(
        "
            export function run(n) {
                let f = function(k) {
                    if (k === 0) { return false; }
                    return k * 1.5;
                };
                return f(n) === false ? -1 : f(n) + 1;
            }
        ",
    );
    validate(&module);
    let (_, _) = body_returning_struct_with_fields(
        &module,
        &[Type::I32, Type::I32, Type::F64],
    )
    .expect("{Boolean, Number} core must return the dedicated if layout");
    // No core in this module should return any layout carrying an anyref
    // slot: the only multi set here is {Boolean, Number}.
    let bytes = wasm_bytes(&module);
    // f(0) = false -> -1
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[0.0]), -1.0);
    // f(2) = 3.0 -> 4.0
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[2.0]), 4.0);
}

#[test]
fn ref_float_core_returns_dedicated_layout() {
    // The {Number, Reference} pick core returns (tag, r, f) with no i32
    // slot — the dedicated `rf` layout rather than the fat 4-field one.
    let module = compile_module(
        "
            export function run(x) {
                let pick = function(v) { if (v > 0) { return v * 1.5; } return null; };
                let v = pick(x);
                return v === null ? -7 : v + 1;
            }
        ",
    );
    validate(&module);
    body_returning_struct_with_fields(
        &module,
        &[Type::I32, repr_anyref(), Type::F64],
    )
    .expect("{Number, Reference} core must return the dedicated rf layout");
    let bytes = wasm_bytes(&module);
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[2.0]), 4.0);
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[-1.0]), -7.0);
}

fn repr_anyref() -> Type {
    Type::Heap(portal_pc_waffle::WithNullable {
        value: portal_pc_waffle::HeapType::Any,
        nullable: true,
    })
}

#[test]
fn forwarding_chain_has_no_interior_unpacks() {
    // a -> b -> c -> export, all links `return next(x);` on the same
    // {Number, Reference} set: every interior link must tail-forward the
    // packed union untouched (ReturnCall, no tag chain), and only the
    // export's adapter prologue unpacks. Depth-100000 execution pins O(1).
    let module = compile_module(
        "
            export function run(n) {
                let a = function(k) { if (k <= 0) { return null; } if (k === 1) { return 1.5; } return b(k - 2); };
                let b = function(k) { return a(k); };
                let c = function(k) { return b(k); };
                return c(n);
            }
        ",
    );
    validate(&module);
    // Every interior body returning the rf layout must contain a
    // ReturnCall and no CondBr tag chains feeding StructGets. Count
    // unpack chains by counting I32Eq comparisons against the tag values
    // inside the rf-returning bodies.
    let mut interior = 0;
    let mut with_return_call = 0;
    for (_, body) in body_returning_struct_with_fields_all(&module) {
        interior += 1;
        let has_rc = body.blocks.iter().any(|b| {
            matches!(
                body.blocks[b].terminator.terminator,
                Terminator::ReturnCall { .. }
            )
        });
        if has_rc {
            with_return_call += 1;
        }
    }
    assert!(interior >= 2, "chain should have at least two union cores");
    assert_eq!(
        with_return_call,
        interior,
        "every same-set link must tail-forward via ReturnCall"
    );
    let bytes = wasm_bytes(&module);
    for (runtime, result) in [
        ("Wasmtime", execute_in_wasmtime(&bytes, "run", &[100_000.0])),
        ("Node.js", execute_in_node(&bytes, "run", &[100_000.0])),
    ] {
        assert_eq!(result, 0.0, "{runtime} forwarding chain at depth 100000");
    }
}

fn body_returning_struct_with_fields_all<'m>(
    module: &'m Module<'_>,
) -> Vec<(usize, &'m FunctionBody)> {
    let mut found = Vec::new();
    for (fid, decl) in module.funcs.entries() {
        let FuncDecl::Body(sig, name, body) = decl else {
            continue;
        };
        if !name.starts_with("js_body") {
            continue;
        }
        if let SignatureData::Func { returns, .. } = &module.signatures[*sig] {
            if returns.len() == 1 {
                let is_rf = matches!(&returns[0], Type::Heap(w) if {
                    matches!(&w.value, portal_pc_waffle::HeapType::Sig { sig_index } if {
                        matches!(&module.signatures[*sig_index], SignatureData::Struct { fields, .. } if {
                            fields.len() == 3
                        })
                    })
                });
                if is_rf {
                    found.push((fid.index(), body));
                }
            }
        }
    }
    found
}

#[test]
fn different_set_cores_repack_across_the_boundary() {
    // A {Boolean, Number} core returns into a {Number, Reference} core:
    // different dedicated types, so the tail downgrades to a framed call
    // and the caller repacks into its own layout. Values stay exact on
    // both arms.
    let module = compile_module(
        "
            export function run(n) {
                let bf = function(k) {
                    if (k === 2) { return false; }
                    return k * 1.5;
                };
                let rf = function(k) {
                    if (k <= 0) { return null; }
                    return bf(k);
                };
                let v = rf(n);
                return v === null ? -3 : v === false ? -2 : v + 1;
            }
        ",
    );
    validate(&module);
    let bytes = wasm_bytes(&module);
    // rf(1) -> bf(1) = 1.5 -> 2.5
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[1.0]), 2.5);
    // rf(2) -> bf(2) = false -> -2
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[2.0]), -2.0);
    // rf(-1) -> null -> -3
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[-1.0]), -3.0);
}

#[test]
fn dedicated_layouts_differ_between_kind_sets() {
    // Inspection: within one module, a {Boolean, Number} core and a
    // {Number, Reference} core return *different* struct types — the
    // structural identity that keeps cross-set tail forwarding a
    // validation error instead of a silent trap.
    let module = compile_module(
        "
            export function run(n) {
                let bf = function(k) {
                    if (k === 0) { return false; }
                    return k * 1.5;
                };
                let rf = function(k) {
                    if (k <= 0) { return null; }
                    return k * 1.5;
                };
                return bf(n) === false ? rf(n) + 1 : 9;
            }
        ",
    );
    validate(&module);
    let if_layout = body_returning_struct_with_fields(
        &module,
        &[Type::I32, Type::I32, Type::F64],
    )
    .expect("bf core must return the if layout");
    let rf_layout = body_returning_struct_with_fields(
        &module,
        &[Type::I32, repr_anyref(), Type::F64],
    )
    .expect("rf core must return the rf layout");
    assert_ne!(
        if_layout.0, rf_layout.0,
        "different kind sets must produce different struct types"
    );
    let bytes = wasm_bytes(&module);
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[0.0]), 1.0);
    assert_eq!(execute_in_wasmtime(&bytes, "run", &[2.0]), 9.0);
}






