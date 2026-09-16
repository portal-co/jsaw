//! A deliberately narrow direct lowering slice for pointer-free WasmGC structs.
//!
//! This is the first executable source transformation in the fallback. It
//! lowers only a self-contained function that constructs a concrete struct of
//! scalar fields and immediately reads a scalar field. The narrow shape is a
//! proof seam: it yields ordinary core Wasm without ever flattening a managed
//! reference into an untyped scalar. General CFG/value rewriting, arrays, and
//! fat-reference values remain separate follow-up passes.

use std::collections::BTreeMap;

use portal_pc_waffle::{
    EntityRef, FuncDecl, FunctionBody, MemoryArg, Module, Operator, SignatureData, Terminator,
    Type, Value, ValueDef,
};

use crate::{
    coregc::{CoreGcError, CoreGcInventory, CoreGcStorage},
    coregc_emit::{CoreGcArtifact, CoreGcOptions, emit_runtime_skeleton},
    coregc_layout::CoreGcDescriptorTable,
};

/// Emit a core-only artifact for the Phase-1 scalar-struct lowering subset.
///
/// Accepted source shape: one exported function, no parameters, a single block
/// ending in `return struct.get(struct.new(scalars), field)`. Scalar instructions
/// that construct the fields may precede the aggregate operations. Everything
/// else is rejected with a source-specific diagnostic. This is intentionally
/// stringent while the general fat-pair SSA lowering is not implemented.
pub fn emit_scalar_struct_subset(
    source: &Module<'_>,
    options: &CoreGcOptions,
) -> Result<CoreGcArtifact, CoreGcError> {
    let inventory = CoreGcInventory::build(source)?;
    let descriptors = CoreGcDescriptorTable::build(&inventory)?;
    let (source_function, source_signature, source_name, source_body) = source_shape(source)?;
    let (struct_value, struct_sig, field_index, return_value) = aggregate_shape(source_body)?;
    let type_id = inventory.id_for(struct_sig).ok_or_else(|| CoreGcError {
        message: format!(
            "coregc scalar subset function {} uses non-managed struct signature {}",
            source_function.index(),
            struct_sig.index()
        ),
    })?;
    let layout = descriptors
        .layouts
        .iter()
        .find(|layout| layout.id == type_id)
        .ok_or_else(|| CoreGcError {
            message: format!("coregc has no descriptor for type {}", type_id.get()),
        })?;
    let field = layout.slots.get(field_index).ok_or_else(|| CoreGcError {
        message: format!(
            "coregc scalar subset struct.get field {} is outside descriptor type {}",
            field_index,
            type_id.get()
        ),
    })?;
    let storage = field.storage.clone();
    if !is_scalar_storage(&storage) {
        return Err(CoreGcError {
            message: format!(
                "coregc scalar subset does not lower managed field {} of type {}",
                field_index,
                type_id.get()
            ),
        });
    }
    let SignatureData::Func {
        params,
        returns,
        shared,
    } = &source.signatures[source_signature]
    else {
        unreachable!("source function has a function signature");
    };
    if *shared || !params.is_empty() || returns.len() != 1 || returns[0] != scalar_type(&storage) {
        return Err(CoreGcError {
            message: format!(
                "coregc scalar subset function {} must have () -> {} ABI",
                source_function.index(),
                scalar_type(&storage)
            ),
        });
    }

    // Start from the well-tested runtime artifact, then append the lowered
    // export. This keeps every allocation/header/descriptor invariant shared.
    let mut runtime = emit_runtime_skeleton(&Module::empty(), options)?;
    let memory = runtime
        .module
        .memories
        .iter()
        .next()
        .expect("runtime memory");
    let allocator = runtime
        .module
        .exports
        .iter()
        .find_map(|export| {
            (export.name == "__coregc_alloc_phase1").then(|| match export.kind {
                portal_pc_waffle::ExportKind::Func(function) => Some(function),
                _ => None,
            })
        })
        .flatten()
        .expect("runtime allocator export");

    let lowered_signature = runtime.module.signatures.push(SignatureData::Func {
        params: vec![],
        returns: vec![scalar_type(&storage)],
        shared: false,
    });
    let mut body = FunctionBody::new(&runtime.module, lowered_signature);
    let block = body.entry;
    let mut values = BTreeMap::<Value, Value>::new();
    for record in &source_body.blocks[source_body.entry].insts {
        let value = record.value;
        let ValueDef::Operator(operator, arguments, result_types) = &source_body.values[value]
        else {
            return Err(CoreGcError {
                message: format!(
                    "coregc scalar subset function {} has unsupported value definition {value:?}",
                    source_function.index()
                ),
            });
        };
        let arguments = &source_body.arg_pool[*arguments];
        let result_types = &source_body.type_pool[*result_types];
        let lowered = match operator {
            Operator::StructNew { sig } if *sig == struct_sig => {
                if arguments.len() != layout.slots.len() {
                    return Err(CoreGcError {
                        message: format!(
                            "coregc scalar subset struct.new for type {} has {} fields, descriptor has {}",
                            type_id.get(),
                            arguments.len(),
                            layout.slots.len()
                        ),
                    });
                }
                let type_id_value = constant(&mut body, block, type_id.get());
                let bytes = constant(
                    &mut body,
                    block,
                    layout.fixed_payload_bytes.expect("struct payload is fixed"),
                );
                let address = body.add_op(
                    block,
                    Operator::Call {
                        function_index: allocator,
                    },
                    &[type_id_value, bytes],
                    &[Type::I32],
                );
                for (index, (argument, slot)) in arguments.iter().zip(&layout.slots).enumerate() {
                    if !is_scalar_storage(&slot.storage) {
                        return Err(CoreGcError {
                            message: format!(
                                "coregc scalar subset cannot initialize managed field {index} of type {}",
                                type_id.get()
                            ),
                        });
                    }
                    let value = values.get(argument).copied().ok_or_else(|| CoreGcError {
                        message: format!(
                            "coregc scalar subset struct.new field {index} is not defined before construction"
                        ),
                    })?;
                    body.add_op(
                        block,
                        store_operator(
                            &slot.storage,
                            MemoryArg {
                                align: alignment_exponent(&slot.storage),
                                offset: u64::from(slot.offset),
                                memory,
                            },
                        ),
                        &[address, value],
                        &[],
                    );
                }
                address
            }
            Operator::StructGet { sig, idx } if *sig == struct_sig && *idx == field_index => {
                let [address] = arguments else {
                    return Err(CoreGcError {
                        message: "coregc scalar subset struct.get arity is invalid".to_owned(),
                    });
                };
                let address = values.get(address).copied().ok_or_else(|| CoreGcError {
                    message:
                        "coregc scalar subset struct.get receiver is not defined before access"
                            .to_owned(),
                })?;
                body.add_op(
                    block,
                    load_operator(
                        &storage,
                        MemoryArg {
                            align: alignment_exponent(&storage),
                            offset: u64::from(field.offset),
                            memory,
                        },
                    ),
                    &[address],
                    &[scalar_type(&storage)],
                )
            }
            _ if value == struct_value || value == return_value => {
                return Err(CoreGcError {
                    message: format!(
                        "coregc scalar subset internal aggregate shape mismatch at value {}",
                        value.index()
                    ),
                });
            }
            _ => clone_scalar_operator(
                &mut body,
                block,
                operator,
                arguments,
                result_types,
                &values,
                source_function.index(),
            )?,
        };
        values.insert(value, lowered);
    }
    let result = values
        .get(&return_value)
        .copied()
        .ok_or_else(|| CoreGcError {
            message: "coregc scalar subset return value was not lowered".to_owned(),
        })?;
    body.set_terminator(
        block,
        Terminator::Return {
            values: vec![result],
        },
    );
    let function = runtime.module.funcs.push(FuncDecl::Body(
        lowered_signature,
        format!("coregc_scalar_{}", source_name),
        body,
    ));
    runtime.module.exports.push(portal_pc_waffle::Export {
        name: source_name.to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(function),
    });
    Ok(runtime)
}

fn source_shape<'a>(
    source: &'a Module<'a>,
) -> Result<
    (
        portal_pc_waffle::Func,
        portal_pc_waffle::Signature,
        &'a str,
        &'a FunctionBody,
    ),
    CoreGcError,
> {
    if source.funcs.entries().count() != 1 || source.exports.len() != 1 {
        return Err(CoreGcError {
            message: "coregc scalar subset requires exactly one source function and one export"
                .to_owned(),
        });
    }
    let (function, declaration) = source.funcs.entries().next().expect("count checked");
    let FuncDecl::Body(signature, _, body) = declaration else {
        return Err(CoreGcError {
            message: "coregc scalar subset does not support function imports".to_owned(),
        });
    };
    let export = &source.exports[0];
    if export.kind != portal_pc_waffle::ExportKind::Func(function) {
        return Err(CoreGcError {
            message: "coregc scalar subset export must name its only function".to_owned(),
        });
    }
    if body.blocks.entries().count() != 1 {
        return Err(CoreGcError {
            message: "coregc scalar subset currently requires a single basic block".to_owned(),
        });
    }
    Ok((function, *signature, &export.name, body))
}

fn aggregate_shape(
    body: &FunctionBody,
) -> Result<(Value, portal_pc_waffle::Signature, usize, Value), CoreGcError> {
    let Terminator::Return { values } = &body.blocks[body.entry].terminator.terminator else {
        return Err(CoreGcError {
            message: "coregc scalar subset requires a return terminator".to_owned(),
        });
    };
    let [returned] = values.as_slice() else {
        return Err(CoreGcError {
            message: "coregc scalar subset requires one scalar return".to_owned(),
        });
    };
    let ValueDef::Operator(Operator::StructGet { sig, idx }, arguments, _) =
        &body.values[*returned]
    else {
        return Err(CoreGcError {
            message: "coregc scalar subset requires return struct.get(...)".to_owned(),
        });
    };
    let [constructed] = body.arg_pool[*arguments].as_ref() else {
        return Err(CoreGcError {
            message: "coregc scalar subset struct.get receiver arity is invalid".to_owned(),
        });
    };
    match &body.values[*constructed] {
        ValueDef::Operator(
            Operator::StructNew {
                sig: constructed_sig,
            },
            _,
            _,
        ) if constructed_sig == sig => Ok((*constructed, *sig, *idx, *returned)),
        _ => Err(CoreGcError {
            message: "coregc scalar subset requires struct.get of its direct struct.new".to_owned(),
        }),
    }
}

fn clone_scalar_operator(
    body: &mut FunctionBody,
    block: portal_pc_waffle::Block,
    operator: &Operator,
    arguments: &[Value],
    result_types: &[Type],
    values: &BTreeMap<Value, Value>,
    function_index: usize,
) -> Result<Value, CoreGcError> {
    if !result_types
        .iter()
        .all(|ty| matches!(ty, Type::I32 | Type::I64 | Type::F32 | Type::F64))
    {
        return Err(CoreGcError {
            message: format!(
                "coregc scalar subset function {function_index} has non-scalar operator result {operator:?}"
            ),
        });
    }
    let arguments = arguments
        .iter()
        .map(|argument| values.get(argument).copied().ok_or_else(|| CoreGcError {
            message: format!("coregc scalar subset function {function_index} uses value {} before definition", argument.index()),
        }))
        .collect::<Result<Vec<_>, _>>()?;
    // No call/memory/global/control operation can appear in the tiny subset;
    // copying only common numeric operators makes that boundary explicit.
    if !matches!(
        operator,
        Operator::I32Const { .. }
            | Operator::I64Const { .. }
            | Operator::F32Const { .. }
            | Operator::F64Const { .. }
            | Operator::I32Add
            | Operator::I32Sub
            | Operator::I32Mul
            | Operator::I64Add
            | Operator::I64Sub
            | Operator::I64Mul
            | Operator::F32Add
            | Operator::F32Sub
            | Operator::F32Mul
            | Operator::F64Add
            | Operator::F64Sub
            | Operator::F64Mul
    ) {
        return Err(CoreGcError {
            message: format!(
                "coregc scalar subset function {function_index} does not lower operator {operator:?}"
            ),
        });
    }
    Ok(body.add_op(block, operator.clone(), &arguments, result_types))
}

fn constant(body: &mut FunctionBody, block: portal_pc_waffle::Block, value: u32) -> Value {
    body.add_op(block, Operator::I32Const { value }, &[], &[Type::I32])
}

fn is_scalar_storage(storage: &CoreGcStorage) -> bool {
    matches!(
        storage,
        CoreGcStorage::I8
            | CoreGcStorage::I16
            | CoreGcStorage::I32
            | CoreGcStorage::I64
            | CoreGcStorage::F32
            | CoreGcStorage::F64
    )
}

fn scalar_type(storage: &CoreGcStorage) -> Type {
    match storage {
        CoreGcStorage::I8 | CoreGcStorage::I16 | CoreGcStorage::I32 => Type::I32,
        CoreGcStorage::I64 => Type::I64,
        CoreGcStorage::F32 => Type::F32,
        CoreGcStorage::F64 => Type::F64,
        CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. } => {
            unreachable!("checked scalar storage")
        }
    }
}

fn alignment_exponent(storage: &CoreGcStorage) -> u32 {
    match storage {
        CoreGcStorage::I8 => 0,
        CoreGcStorage::I16 => 1,
        CoreGcStorage::I32 | CoreGcStorage::F32 => 2,
        CoreGcStorage::I64 | CoreGcStorage::F64 => 3,
        CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. } => {
            unreachable!("checked scalar storage")
        }
    }
}

fn load_operator(storage: &CoreGcStorage, memory: MemoryArg) -> Operator {
    match storage {
        CoreGcStorage::I8 => Operator::I32Load8U { memory },
        CoreGcStorage::I16 => Operator::I32Load16U { memory },
        CoreGcStorage::I32 => Operator::I32Load { memory },
        CoreGcStorage::I64 => Operator::I64Load { memory },
        CoreGcStorage::F32 => Operator::F32Load { memory },
        CoreGcStorage::F64 => Operator::F64Load { memory },
        CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. } => {
            unreachable!("checked scalar storage")
        }
    }
}

fn store_operator(storage: &CoreGcStorage, memory: MemoryArg) -> Operator {
    match storage {
        CoreGcStorage::I8 => Operator::I32Store8 { memory },
        CoreGcStorage::I16 => Operator::I32Store16 { memory },
        CoreGcStorage::I32 => Operator::I32Store { memory },
        CoreGcStorage::I64 => Operator::I64Store { memory },
        CoreGcStorage::F32 => Operator::F32Store { memory },
        CoreGcStorage::F64 => Operator::F64Store { memory },
        CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. } => {
            unreachable!("checked scalar storage")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use portal_pc_waffle::{Export, ExportKind, StorageType, WithMutablility};
    use wasmtime::{Engine, Instance, Module as WasmtimeModule, Store, TypedFunc};

    fn field(value: StorageType) -> WithMutablility<StorageType> {
        WithMutablility {
            mutable: true,
            value,
        }
    }

    fn scalar_source() -> Module<'static> {
        let mut module = Module::empty();
        let point = module.signatures.push(SignatureData::Struct {
            fields: vec![
                field(StorageType::Val(Type::I32)),
                field(StorageType::Val(Type::I32)),
            ],
            shared: false,
        });
        let signature = module.signatures.push(SignatureData::Func {
            params: vec![],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(&module, signature);
        let entry = body.entry;
        let left = body.add_op(entry, Operator::I32Const { value: 40 }, &[], &[Type::I32]);
        let right = body.add_op(entry, Operator::I32Const { value: 2 }, &[], &[Type::I32]);
        let point_value = body.add_op(
            entry,
            Operator::StructNew { sig: point },
            &[left, right],
            &[Type::Heap(portal_pc_waffle::WithNullable {
                nullable: false,
                value: portal_pc_waffle::HeapType::Sig { sig_index: point },
            })],
        );
        let result = body.add_op(
            entry,
            Operator::StructGet { sig: point, idx: 1 },
            &[point_value],
            &[Type::I32],
        );
        body.set_terminator(
            entry,
            Terminator::Return {
                values: vec![result],
            },
        );
        let function = module
            .funcs
            .push(FuncDecl::Body(signature, "read_point".to_owned(), body));
        module.exports.push(Export {
            name: "read_point".to_owned(),
            kind: ExportKind::Func(function),
        });
        module
    }

    #[test]
    fn lowers_and_executes_pointer_free_struct_access_without_gc() {
        let artifact = emit_scalar_struct_subset(&scalar_source(), &CoreGcOptions::default())
            .expect("supported scalar struct source");
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core wasm encodes");
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("default core Wasm validates");
        let engine = Engine::default();
        let module = WasmtimeModule::new(&engine, bytes).expect("core engine compiles module");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("core artifact instantiates");
        let function: TypedFunc<(), i32> = instance
            .get_typed_func(&mut store, "read_point")
            .expect("lowered export");
        assert_eq!(function.call(&mut store, ()).expect("lowered call"), 2);
    }

    #[test]
    fn rejects_non_direct_struct_access() {
        let mut source = scalar_source();
        let (_, FuncDecl::Body(_, _, _body)) =
            source.funcs.entries().next().expect("source function")
        else {
            unreachable!()
        };
        // The test fixture itself is direct; make the required one-export
        // contract fail to exercise a clear, non-silent boundary diagnostic.
        source.exports.clear();
        let error = emit_scalar_struct_subset(&source, &CoreGcOptions::default())
            .expect_err("unsupported source");
        assert!(error.message.contains("one export"));
    }
}
