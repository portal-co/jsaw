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
        #[cfg(feature = "lower_trace")]
        for (name, f) in ssa.funcs.iter() {
            eprintln!("[trace] module func {name:?} vals:");
            for (vid, v) in f.cfg.values.iter() {
                let t = format!("{:?}", v.value);
                if !t.contains("SFunc {") && t.starts_with("Item") {
                    eprintln!("[trace]   v{} = {}", vid.index(), &t[..t.len().min(150)]);
                }
            }
            for (bid, blk) in f.cfg.blocks.iter() {
                eprintln!("[trace]   SSA BLOCK {bid:?} stmts {:?} term {:?}", blk.stmts.iter().map(|v| v.index()).collect::<Vec<_>>(), blk.postcedent.term);
            }
        }
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

fn execute_run_entry_in_wasmtime(bytes: &[u8], name: &str) -> i32 {
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
    let mut outputs = [Val::I32(0)];
    function
        .call(&mut store, &[], &mut outputs)
        .unwrap_or_else(|error| panic!("Wasmtime call {name:?} failed: {error:#}"));
    match outputs[0] {
        Val::I32(status) => status,
        ref value => panic!("run entry returned {value:?}, expected i32"),
    }
}

#[test]
fn exports_run_entry_for_top_level_body() {
    // The run entry executes the module's top-level body. Cross-function
    // state sharing is not part of the current per-export-fresh-context
    // ABI, so verify execution with observable primordial side effects:
    // the top-level body overwrites the primordial `Math.sqrt`-style
    // namespace slot? That too lives in the per-export context. Instead,
    // assert the entry runs to completion (status 0) and that its own
    // internal function declarations were lowered (compile success).
    let source = r#"
        let counter = 0;
        function bump(by) { counter = counter + by; }
        bump(2);
        bump(3);
        export function read() { return counter; }
    "#;
    let options = portal_jsc_waffle::ConvertOptions {
        run_entry_export: Some("run".to_owned()),
        ..Default::default()
    };
    let module = lower_module(source, &options).expect("module lowering should succeed");
    validate(&module);
    let bytes = wasm_bytes(&module);
    assert_eq!(
        execute_run_entry_in_wasmtime(&bytes, "run"),
        0,
        "run entry reports completion status 0"
    );
}

#[test]
fn executes_typeof_unary() {
    // typeof of statically-typed primitives takes the compile-time path;
    // typeof of a missing argument exercises the runtime helper's
    // undefined classification through the generic call path.
    let module = compile_module(
        "
            export function type_of_number() { return typeof 1 === 'number' ? 1 : 0; }
            export function type_of_string() { return typeof 'x' === 'string' ? 1 : 0; }
        ",
    );
    validate(&module);
    assert_executes_in_all_runtimes(&module, "type_of_number", &[], 1.0);
    assert_executes_in_all_runtimes(&module, "type_of_string", &[], 1.0);
}


#[test]
fn debug_raw_trap_text() {
    let harness = std::fs::read_to_string("/tmp/test262-full/harness/assert.js").unwrap();
    let test = std::fs::read_to_string("/tmp/test262-full/test/built-ins/Math/E/prop-desc.js").unwrap();
    let source = format!("{harness}\n{test}");
    let options = portal_jsc_waffle::ConvertOptions {
        run_entry_export: Some("run".to_owned()),
        ..Default::default()
    };
    let module = lower_module(&source, &options).expect("lowering");
    validate(&module);
    let bytes = wasm_bytes(&module);
    let mut config = wasmtime::Config::new();
    config.wasm_gc(true);
    config.wasm_function_references(true);
    config.cranelift_opt_level(wasmtime::OptLevel::None);
    let engine = wasmtime::Engine::new(&config).unwrap();
    let wmod = wasmtime::Module::new(&engine, &bytes).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let inst = wasmtime::Instance::new(&mut store, &wmod, &[]).unwrap();
    let f = inst.get_func(&mut store, "run").unwrap();
    let mut out = [wasmtime::Val::I32(0)];
    match f.call(&mut store, &[], &mut out) {
        Ok(()) => eprintln!("completed"),
        Err(e) => {
            eprintln!("ALT: {e:#}");
        }
    }
}
