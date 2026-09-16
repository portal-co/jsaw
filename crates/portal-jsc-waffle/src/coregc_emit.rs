//! Public orchestration for the coregc backend
//! (`docs/plan-coregc-atomic-collector-and-lowering.md` §5).
//!
//! This module implements no algorithm of its own: `emit_coregc` sequences
//! inventory -> descriptors -> runtime -> lowering -> whole-artifact
//! verification, and is the only public entry point. An artifact is either a
//! fully validated, fully rooted, fully scanned-and-swept core-Wasm module,
//! or a `CoreGcError` naming the first unsupported source construct.

use portal_pc_waffle::{Export, ExportKind, FuncDecl, Module};

use crate::{
    coregc::{CoreGcError, CoreGcInventory},
    coregc_layout::CoreGcDescriptorTable,
    coregc_lower, coregc_runtime,
};

pub use crate::coregc_runtime::CoreGcOptions;

/// A core-Wasm artifact plus the inventory and descriptor table that
/// describe its managed heap.
#[derive(Clone, Debug)]
pub struct CoreGcArtifact {
    pub module: Module<'static>,
    pub inventory: CoreGcInventory,
    pub descriptors: CoreGcDescriptorTable,
}

/// Lower `source` (jsaw's typed WasmGC IR) to a pure core-Wasm module with
/// an embedded mark-and-sweep runtime.
///
/// The result is all-or-nothing: either every function lowered and the
/// output validates with default core-Wasm features (no GC/reference-types/
/// function-references proposals), or this returns an error and produces no
/// artifact at all.
pub fn emit_coregc(
    source: &Module<'_>,
    options: &CoreGcOptions,
) -> Result<CoreGcArtifact, CoreGcError> {
    let inventory = CoreGcInventory::build(source)?;
    let descriptors = CoreGcDescriptorTable::build(&inventory)?;
    let mut module = Module::empty();
    let runtime = coregc_runtime::build(&mut module, options, &descriptors)?;
    let lowered = coregc_lower::lower(source, &inventory, &descriptors, &runtime, &mut module)?;
    for (name, func) in lowered.exports {
        module.exports.push(Export {
            name,
            kind: ExportKind::Func(func),
        });
    }
    if options.export_runtime_debug {
        module.exports.push(Export {
            name: "__coregc_collect".to_owned(),
            kind: ExportKind::Func(runtime.collect),
        });
        module.exports.push(Export {
            name: "__coregc_trap_code".to_owned(),
            kind: ExportKind::Global(runtime.trap_code),
        });
        module.exports.push(Export {
            name: "__coregc_debug_addr".to_owned(),
            kind: ExportKind::Global(runtime.debug_addr),
        });
        module.exports.push(Export {
            name: "__coregc_debug_type".to_owned(),
            kind: ExportKind::Global(runtime.debug_type),
        });
        module.exports.push(Export {
            name: "__coregc_root_head".to_owned(),
            kind: ExportKind::Global(runtime.root_head),
        });
        module.exports.push(Export {
            name: "__coregc_block_list_head".to_owned(),
            kind: ExportKind::Global(runtime.block_list_head),
        });
        module.exports.push(Export {
            name: "__coregc_root_bump".to_owned(),
            kind: ExportKind::Global(runtime.root_bump),
        });
        module.exports.push(Export {
            name: "memory".to_owned(),
            kind: ExportKind::Memory(runtime.memory),
        });
    }

    // Whole-artifact verification gate (docs plan §9): every body passes
    // Waffle's own IR validation, the encoded module passes default
    // core-Wasm validation, and no GC-proposal instruction or type survives.
    for (_, decl) in module.funcs.entries() {
        if let FuncDecl::Body(_, name, body) = decl {
            body.validate().map_err(|error| CoreGcError {
                message: format!("coregc produced an invalid function '{name}': {error}"),
            })?;
        }
    }
    let bytes = portal_pc_waffle::to_wasm_bytes(&module).map_err(|error| CoreGcError {
        message: format!("coregc artifact failed to encode: {error}"),
    })?;
    verify_core_only(&bytes)?;

    Ok(CoreGcArtifact {
        module,
        inventory,
        descriptors,
    })
}

/// Prove the encoded artifact uses only default core-Wasm features: the
/// validator's default feature set already rejects GC/reference-types/
/// function-references constructs; the explicit scan below additionally
/// names any GC-shaped construct for diagnostics.
fn verify_core_only(bytes: &[u8]) -> Result<(), CoreGcError> {
    wasmparser::Validator::new()
        .validate_all(bytes)
        .map_err(|error| CoreGcError {
            message: format!("coregc artifact does not validate with default core features: {error}"),
        })?;
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        match payload {
            Ok(wasmparser::Payload::TypeSection(reader)) => {
                for entry in reader {
                    let group = entry.map_err(|error| CoreGcError {
                        message: format!("coregc artifact has an unreadable type entry: {error}"),
                    })?;
                    for subtype in group.types() {
                        if !matches!(
                            subtype.composite_type.inner,
                            wasmparser::CompositeInnerType::Func(_)
                        ) {
                            return Err(CoreGcError {
                                message: "coregc artifact contains a non-function type section \
                                          entry (GC aggregate types must be erased by lowering)"
                                    .to_owned(),
                            });
                        }
                    }
                }
            }
            Ok(wasmparser::Payload::CodeSectionEntry(body)) => {
                let reader = body.get_operators_reader().map_err(|error| CoreGcError {
                    message: format!("coregc artifact has an unreadable function body: {error}"),
                })?;
                for op in reader {
                    let op = op.map_err(|error| CoreGcError {
                        message: format!("coregc artifact has an unreadable operator: {error}"),
                    })?;
                    let name = format!("{op:?}");
                    for gc in [
                        "StructNew", "StructGet", "StructSet", "ArrayNew", "ArrayGet", "ArraySet",
                        "ArrayLen", "RefCast", "RefTest", "RefI31", "I31Get", "AnyConvert",
                        "ExternConvert", "RefFunc", "CallRef", "ReturnCallRef",
                    ] {
                        if name.starts_with(gc) {
                            return Err(CoreGcError {
                                message: format!(
                                    "coregc artifact contains a GC/reference-type instruction {name}"
                                ),
                            });
                        }
                    }
                }
            }
            _ => {}
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::coregc_lower::tests_fixtures;
    use wasmtime::{Engine, Instance, Module as WasmtimeModule, Store, TypedFunc};

    fn instantiate(artifact: &CoreGcArtifact) -> (Store<()>, Instance) {
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("encodes");
        let engine = Engine::default();
        let module = WasmtimeModule::new(&engine, bytes).expect("compiles");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("instantiates");
        (store, instance)
    }

    #[test]
    fn list_sum_through_the_public_entry_point() {
        let (source, _) = tests_fixtures::list_sum_source();
        let artifact = emit_coregc(
            &source,
            &CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        )
        .expect("lowering");
        let (mut store, instance) = instantiate(&artifact);
        let build: TypedFunc<i32, i32> = instance
            .get_typed_func(&mut store, "build")
            .expect("build export");
        assert_eq!(build.call(&mut store, 20).expect("build(20)"), (0..20).sum::<i32>());
    }

    #[test]
    fn array_sum_through_the_public_entry_point() {
        let source = tests_fixtures::array_sum_source();
        let artifact = emit_coregc(
            &source,
            &CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        )
        .expect("lowering");
        let (mut store, instance) = instantiate(&artifact);
        let build: TypedFunc<i32, i32> = instance
            .get_typed_func(&mut store, "build_array")
            .expect("build_array export");
        assert_eq!(
            build.call(&mut store, 20).expect("build_array(20)"),
            (0..20).sum::<i32>()
        );
    }

    #[test]
    fn direct_call_through_the_public_entry_point() {
        let source = tests_fixtures::direct_call_source();
        let artifact = emit_coregc(
            &source,
            &CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        )
        .expect("lowering");
        let (mut store, instance) = instantiate(&artifact);
        let caller: TypedFunc<(i32, i32), i32> = instance
            .get_typed_func(&mut store, "caller")
            .expect("caller export");
        assert_eq!(caller.call(&mut store, (10, 32)).expect("caller"), 42);
    }

    #[test]
    fn call_ref_dispatch_through_the_public_entry_point() {
        let source = tests_fixtures::call_ref_source();
        let artifact = emit_coregc(&source, &CoreGcOptions::default()).expect("lowering");
        let (mut store, instance) = instantiate(&artifact);
        let apply: TypedFunc<(i32, i32), i32> = instance
            .get_typed_func(&mut store, "apply")
            .expect("apply export");
        assert_eq!(apply.call(&mut store, (0, 41)).expect("add_one"), 42);
        assert_eq!(apply.call(&mut store, (1, 21)).expect("double"), 42);
        assert!(apply.call(&mut store, (2, 0)).is_err(), "null funcref traps");
    }

    #[test]
    fn unsupported_operators_fail_closed_with_a_named_diagnostic() {
        let source = tests_fixtures::select_terminator_source();
        let error = emit_coregc(&source, &CoreGcOptions::default())
            .expect_err("br_table (Terminator::Select) must be rejected");
        assert!(error.message.contains("Select"), "{error}");
    }

    #[test]
    fn return_call_ref_through_a_null_reference_traps_at_runtime() {
        let source = tests_fixtures::tail_call_source();
        let artifact = emit_coregc(&source, &CoreGcOptions::default())
            .expect("tail calls are supported since stage 2");
        let (mut store, instance) = instantiate(&artifact);
        let tail_caller: TypedFunc<i32, i32> = instance
            .get_typed_func(&mut store, "tail_caller")
            .expect("tail_caller export");
        assert!(
            tail_caller.call(&mut store, 41).is_err(),
            "a null callee must trap, not silently succeed"
        );
    }

    /// Differential check: the same fixture runs under native WasmGC
    /// (Wasmtime with GC enabled) and under coregc (default features), and
    /// both agree on results.
    #[test]
    fn differential_native_wasmgc_vs_coregc() {
        let (source, _) = tests_fixtures::list_sum_source();
        let native_bytes = portal_pc_waffle::to_wasm_bytes(&source).expect("native encodes");
        let mut config = wasmtime::Config::new();
        config.wasm_gc(true);
        config.wasm_function_references(true);
        let native_engine = Engine::new(&config).expect("GC engine");
        let native_module = WasmtimeModule::new(&native_engine, native_bytes).expect("native GC module");
        let mut native_store = Store::new(&native_engine, ());
        let native_instance = Instance::new(&mut native_store, &native_module, &[])
            .expect("native instance");
        let native_build: TypedFunc<i32, i32> = native_instance
            .get_typed_func(&mut native_store, "build")
            .expect("native build");

        let artifact = emit_coregc(
            &source,
            &CoreGcOptions {
                collect_threshold_bytes: 0,
                ..CoreGcOptions::default()
            },
        )
        .expect("coregc lowering");
        let (mut core_store, core_instance) = instantiate(&artifact);
        let core_build: TypedFunc<i32, i32> = core_instance
            .get_typed_func(&mut core_store, "build")
            .expect("coregc build");

        for n in [0, 1, 2, 5, 20] {
            let native = native_build.call(&mut native_store, n).expect("native call");
            let core = core_build.call(&mut core_store, n).expect("coregc call");
            assert_eq!(native, core, "differential mismatch at n={n}");
        }
    }
}
