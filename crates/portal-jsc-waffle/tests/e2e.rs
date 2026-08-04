use portal_jsc_swc_cfg::module::CfgModule;
use portal_jsc_swc_ssa::SFunc;
use portal_jsc_swc_tac::module::TModule;
use portal_pc_waffle::{FuncDecl, Module, Operator, ValueDef};
use swc_common::{FileName, GLOBALS, Globals, SourceMap, sync::Lrc};
use swc_ecma_ast::{EsVersion, Module as SwcModule, ModuleItem};
use swc_ecma_parser::{EsSyntax, Syntax, parse_file_as_script};

fn lower(source: &str) -> Result<Module<'static>, portal_jsc_waffle::ConvertError> {
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
        let ssa = SFunc::try_from(&tac.body).expect("SSA lowering should succeed");
        let mut wasm = Module::empty();
        portal_jsc_waffle::convert(&ssa, &mut wasm)?;
        Ok(wasm)
    })
}

fn compile(source: &str) -> Module<'static> {
    lower(source).expect("WasmGC lowering should succeed")
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
