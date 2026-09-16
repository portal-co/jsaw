//! Phase-1 lowering for pointer-free scalar arrays.
//!
//! This slice deliberately accepts one exported, single-block function and
//! lowers `array.new_default`, `array.get`, and `array.len` when the array
//! element is scalar. Array references are kept as internal payload addresses;
//! they never cross the public ABI. Managed/fat references remain rejected.

use std::collections::BTreeMap;

use portal_pc_waffle::{
    EntityRef, FuncDecl, FunctionBody, MemoryArg, Module, Operator, SignatureData, Terminator,
    Type, Value, ValueDef,
};

use crate::{
    coregc::{CoreGcError, CoreGcInventory, CoreGcStorage, CoreGcTypeKind},
    coregc_emit::{CoreGcArtifact, CoreGcOptions, emit_runtime_skeleton},
    coregc_layout::CoreGcDescriptorTable,
};

/// Lower a pointer-free scalar-array function to core Wasm.
///
/// The accepted source shape is intentionally explicit: exactly one exported
/// function, one basic block, scalar public parameters/returns, and no source
/// memory/global/table/import. Aggregate values may occur internally, but only
/// scalar array elements are lowered.
pub fn emit_scalar_array_subset(
    source: &Module<'_>,
    options: &CoreGcOptions,
) -> Result<CoreGcArtifact, CoreGcError> {
    let inventory = CoreGcInventory::build(source)?;
    let descriptors = CoreGcDescriptorTable::build(&inventory)?;
    let (source_function, source_signature, export_name, source_body) = source_shape(source)?;
    let SignatureData::Func {
        params,
        returns,
        shared,
    } = &source.signatures[source_signature]
    else {
        unreachable!("source_shape guarantees a function signature");
    };
    if *shared {
        return Err(error(
            "coregc scalar array lowering does not support shared functions",
        ));
    }
    let params = params
        .iter()
        .map(core_public_type)
        .collect::<Result<Vec<_>, _>>()?;
    let returns = returns
        .iter()
        .map(core_public_type)
        .collect::<Result<Vec<_>, _>>()?;

    let mut runtime = emit_runtime_skeleton(&Module::empty(), options)?;
    let runtime_memory = runtime
        .module
        .memories
        .iter()
        .next()
        .expect("runtime memory");
    runtime.module.memories[runtime_memory].segments[0].data = descriptors.bytes.clone();
    runtime.inventory = inventory.clone();
    runtime.descriptors = descriptors.clone();
    let memory = runtime_memory;
    let allocator = runtime_function(&runtime, "__coregc_alloc_phase1")?;
    let bounds = runtime_function(&runtime, "__coregc_array_bounds_phase1")?;

    let lowered_signature = runtime.module.signatures.push(SignatureData::Func {
        params,
        returns: returns.clone(),
        shared: false,
    });
    let mut lowered = FunctionBody::new(&runtime.module, lowered_signature);
    let block = lowered.entry;
    let mut values = BTreeMap::<Value, Value>::new();

    // Copy scalar source parameters into the lowered entry block. A source
    // aggregate parameter would require the Phase-3 fat-pair ABI.
    for (index, (_, source_value)) in source_body.blocks[source_body.entry]
        .params
        .iter()
        .enumerate()
    {
        let source_ty = source_body.blocks[source_body.entry].params[index].0;
        let ty = core_public_type(&source_ty)?;
        let lowered_value = lowered.blocks[block].params[index].1;
        values.insert(*source_value, lowered_value);
        debug_assert_eq!(lowered.blocks[block].params[index].0, ty);
    }

    for record in &source_body.blocks[source_body.entry].insts {
        let source_value = record.value;
        let ValueDef::Operator(operator, arguments, result_types) =
            &source_body.values[source_value]
        else {
            return Err(error(format!(
                "coregc scalar array function {} has unsupported value definition {}",
                source_function.index(),
                source_value.index()
            )));
        };
        let arguments = &source_body.arg_pool[*arguments];
        let result_types = &source_body.type_pool[*result_types];
        let result = match operator {
            Operator::ArrayNewDefault { sig } => {
                let [length] = arguments.as_ref() else {
                    return Err(error("coregc array.new_default requires one length"));
                };
                let length = mapped(&values, *length)?;
                let layout = array_layout(&runtime.inventory, &runtime.descriptors, *sig)?;
                let element = &layout.slots[0].storage;
                require_scalar(element, "array.new_default element")?;
                let stride = layout.array_stride.expect("array layout has a stride");
                let payload_prefix = constant(&mut lowered, block, 4);
                let stride_value = constant(&mut lowered, block, stride);
                let byte_length = lowered.add_op(
                    block,
                    Operator::I32Mul,
                    &[length, stride_value],
                    &[Type::I32],
                );
                let bytes = lowered.add_op(
                    block,
                    Operator::I32Add,
                    &[byte_length, payload_prefix],
                    &[Type::I32],
                );
                let type_id = constant(
                    &mut lowered,
                    block,
                    runtime.inventory.id_for(*sig).unwrap().get(),
                );
                let address = lowered.add_op(
                    block,
                    Operator::Call {
                        function_index: allocator,
                    },
                    &[type_id, bytes],
                    &[Type::I32],
                );
                // The length is the first word of the array payload. The
                // allocator returns the payload address, not the header.
                lowered.add_op(
                    block,
                    Operator::I32Store {
                        memory: MemoryArg {
                            align: 2,
                            offset: 0,
                            memory,
                        },
                    },
                    &[address, length],
                    &[],
                );
                address
            }
            Operator::ArrayGet { sig } => {
                let [array, index] = arguments.as_ref() else {
                    return Err(error("coregc array.get requires array and index"));
                };
                let array = mapped(&values, *array)?;
                let index = mapped(&values, *index)?;
                let layout = array_layout(&runtime.inventory, &runtime.descriptors, *sig)?;
                let element = &layout.slots[0].storage;
                require_scalar(element, "array.get element")?;
                let checked_index_value = constant(
                    &mut lowered,
                    block,
                    runtime.inventory.id_for(*sig).unwrap().get(),
                );
                let checked_index = lowered.add_op(
                    block,
                    Operator::Call {
                        function_index: bounds,
                    },
                    &[array, checked_index_value, index],
                    &[Type::I32],
                );
                let stride_value = constant(&mut lowered, block, layout.array_stride.unwrap());
                let index_offset = lowered.add_op(
                    block,
                    Operator::I32Mul,
                    &[checked_index, stride_value],
                    &[Type::I32],
                );
                let prefix = constant(&mut lowered, block, 4);
                let offset = lowered.add_op(
                    block,
                    Operator::I32Add,
                    &[index_offset, prefix],
                    &[Type::I32],
                );
                let address =
                    lowered.add_op(block, Operator::I32Add, &[array, offset], &[Type::I32]);
                lowered.add_op(
                    block,
                    load_operator(
                        element,
                        MemoryArg {
                            align: alignment(element),
                            offset: 0,
                            memory,
                        },
                    ),
                    &[address],
                    &[scalar_type(element)],
                )
            }
            Operator::ArrayLen => {
                let [array] = arguments.as_ref() else {
                    return Err(error("coregc array.len requires one array"));
                };
                let array = mapped(&values, *array)?;
                lowered.add_op(
                    block,
                    Operator::I32Load {
                        memory: MemoryArg {
                            align: 2,
                            offset: 0,
                            memory,
                        },
                    },
                    &[array],
                    &[Type::I32],
                )
            }
            Operator::ArraySet { sig } => {
                let [array, index, element] = arguments.as_ref() else {
                    return Err(error("coregc array.set requires array, index and element"));
                };
                let array = mapped(&values, *array)?;
                let index = mapped(&values, *index)?;
                let element = mapped(&values, *element)?;
                let layout = array_layout(&runtime.inventory, &runtime.descriptors, *sig)?;
                let storage = &layout.slots[0].storage;
                require_scalar(storage, "array.set element")?;
                let type_id = constant(
                    &mut lowered,
                    block,
                    runtime.inventory.id_for(*sig).unwrap().get(),
                );
                let checked_index = lowered.add_op(
                    block,
                    Operator::Call {
                        function_index: bounds,
                    },
                    &[array, type_id, index],
                    &[Type::I32],
                );
                let stride_value = constant(&mut lowered, block, layout.array_stride.unwrap());
                let index_offset = lowered.add_op(
                    block,
                    Operator::I32Mul,
                    &[checked_index, stride_value],
                    &[Type::I32],
                );
                let prefix = constant(&mut lowered, block, 4);
                let element_offset = lowered.add_op(
                    block,
                    Operator::I32Add,
                    &[index_offset, prefix],
                    &[Type::I32],
                );
                let element_address = lowered.add_op(
                    block,
                    Operator::I32Add,
                    &[array, element_offset],
                    &[Type::I32],
                );
                lowered.add_op(
                    block,
                    store_operator(
                        storage,
                        MemoryArg {
                            align: alignment(storage),
                            offset: 0,
                            memory,
                        },
                    ),
                    &[element_address, element],
                    &[],
                );
                // ArraySet has no result; track it as a zero scalar so later
                // scalar operators that consume it (rarely) stay type-correct.
                constant(&mut lowered, block, 0)
            }
            _ => copy_scalar_operator(
                &mut lowered,
                block,
                operator,
                arguments,
                result_types,
                &values,
            )?,
        };
        values.insert(source_value, result);
    }

    let Terminator::Return {
        values: source_returns,
    } = &source_body.blocks[source_body.entry].terminator.terminator
    else {
        return Err(error(
            "coregc scalar array lowering requires a return terminator",
        ));
    };
    let lowered_returns = source_returns
        .iter()
        .map(|value| mapped(&values, *value))
        .collect::<Result<Vec<_>, _>>()?;
    lowered.set_terminator(
        block,
        Terminator::Return {
            values: lowered_returns,
        },
    );
    let function = runtime.module.funcs.push(FuncDecl::Body(
        lowered_signature,
        format!("coregc_array_{export_name}"),
        lowered,
    ));
    runtime.module.exports.push(portal_pc_waffle::Export {
        name: export_name,
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
        String,
        &'a FunctionBody,
    ),
    CoreGcError,
> {
    if source.funcs.entries().count() != 1 || source.exports.len() != 1 {
        return Err(error(
            "coregc scalar array lowering requires exactly one function and export",
        ));
    }
    let (function, declaration) = source.funcs.entries().next().unwrap();
    let FuncDecl::Body(signature, _, body) = declaration else {
        return Err(error(
            "coregc scalar array lowering does not support imports",
        ));
    };
    if source.exports[0].kind != portal_pc_waffle::ExportKind::Func(function) {
        return Err(error("coregc scalar array export must name its function"));
    }
    if body.blocks.entries().count() != 1 {
        return Err(error(
            "coregc scalar array lowering currently requires one basic block",
        ));
    }
    Ok((function, *signature, source.exports[0].name.clone(), body))
}

fn runtime_function(
    runtime: &CoreGcArtifact,
    name: &str,
) -> Result<portal_pc_waffle::Func, CoreGcError> {
    runtime
        .module
        .exports
        .iter()
        .find_map(|export| {
            (export.name == name).then(|| match export.kind {
                portal_pc_waffle::ExportKind::Func(function) => Some(function),
                _ => None,
            })
        })
        .flatten()
        .ok_or_else(|| error(format!("coregc runtime is missing {name}")))
}

fn array_layout<'a>(
    inventory: &CoreGcInventory,
    descriptors: &'a CoreGcDescriptorTable,
    signature: portal_pc_waffle::Signature,
) -> Result<&'a crate::coregc_layout::CoreGcPayloadLayout, CoreGcError> {
    let id = inventory
        .id_for(signature)
        .ok_or_else(|| error(format!("unknown array signature {}", signature.index())))?;
    let layout = descriptors
        .layouts
        .iter()
        .find(|layout| layout.id == id)
        .ok_or_else(|| error(format!("missing array descriptor {}", id.get())))?;
    if layout.array_stride.is_none() || layout.slots.len() != 1 {
        return Err(error(format!(
            "signature {} is not an array descriptor",
            signature.index()
        )));
    }
    match inventory
        .types
        .iter()
        .find(|ty| ty.id == id)
        .map(|ty| &ty.kind)
    {
        Some(CoreGcTypeKind::Array { .. }) => Ok(layout),
        _ => Err(error(format!(
            "signature {} is not an array",
            signature.index()
        ))),
    }
}

fn mapped(values: &BTreeMap<Value, Value>, value: Value) -> Result<Value, CoreGcError> {
    values.get(&value).copied().ok_or_else(|| {
        error(format!(
            "source value {} is used before definition",
            value.index()
        ))
    })
}

fn copy_scalar_operator(
    body: &mut FunctionBody,
    block: portal_pc_waffle::Block,
    operator: &Operator,
    arguments: &[Value],
    result_types: &[Type],
    values: &BTreeMap<Value, Value>,
) -> Result<Value, CoreGcError> {
    if result_types.len() != 1
        || !result_types
            .iter()
            .all(|ty| matches!(ty, Type::I32 | Type::I64 | Type::F32 | Type::F64))
    {
        return Err(error(format!(
            "coregc scalar array lowering does not support operator {operator:?}"
        )));
    }
    let arguments = arguments
        .iter()
        .map(|value| mapped(values, *value))
        .collect::<Result<Vec<_>, _>>()?;
    if !matches!(
        operator,
        Operator::I32Const { .. }
            | Operator::I64Const { .. }
            | Operator::F32Const { .. }
            | Operator::F64Const { .. }
            | Operator::I32Add
            | Operator::I32Sub
            | Operator::I32Mul
            | Operator::I32Eq
            | Operator::I32Ne
            | Operator::I32LtU
            | Operator::I32GtU
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
        return Err(error(format!(
            "coregc scalar array lowering does not support operator {operator:?}"
        )));
    }
    Ok(body.add_op(block, operator.clone(), &arguments, result_types))
}

fn core_public_type(ty: &Type) -> Result<Type, CoreGcError> {
    if matches!(ty, Type::I32 | Type::I64 | Type::F32 | Type::F64) {
        Ok(*ty)
    } else {
        Err(error(format!(
            "coregc scalar array public ABI contains non-scalar type {ty:?}"
        )))
    }
}

fn require_scalar(storage: &CoreGcStorage, context: &str) -> Result<(), CoreGcError> {
    if matches!(
        storage,
        CoreGcStorage::I8
            | CoreGcStorage::I16
            | CoreGcStorage::I32
            | CoreGcStorage::I64
            | CoreGcStorage::F32
            | CoreGcStorage::F64
    ) {
        Ok(())
    } else {
        Err(error(format!(
            "coregc {context} must be scalar, got {storage:?}"
        )))
    }
}

fn scalar_type(storage: &CoreGcStorage) -> Type {
    match storage {
        CoreGcStorage::I8 | CoreGcStorage::I16 | CoreGcStorage::I32 => Type::I32,
        CoreGcStorage::I64 => Type::I64,
        CoreGcStorage::F32 => Type::F32,
        CoreGcStorage::F64 => Type::F64,
        CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. } => {
            unreachable!("require_scalar checked storage")
        }
    }
}

fn alignment(storage: &CoreGcStorage) -> u32 {
    match storage {
        CoreGcStorage::I8 => 0,
        CoreGcStorage::I16 => 1,
        CoreGcStorage::I32 | CoreGcStorage::F32 => 2,
        CoreGcStorage::I64 | CoreGcStorage::F64 => 3,
        CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. } => {
            unreachable!("require_scalar checked storage")
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
            unreachable!("require_scalar checked storage")
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
            unreachable!("require_scalar checked storage")
        }
    }
}

fn constant(body: &mut FunctionBody, block: portal_pc_waffle::Block, value: u32) -> Value {
    body.add_op(block, Operator::I32Const { value }, &[], &[Type::I32])
}

fn error(message: impl Into<String>) -> CoreGcError {
    CoreGcError {
        message: message.into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use portal_pc_waffle::{
        Export, ExportKind, HeapType, StorageType, WithMutablility, WithNullable,
    };
    use wasmtime::{Engine, Instance, Module as WasmtimeModule, Store, TypedFunc};

    fn field(value: StorageType) -> WithMutablility<StorageType> {
        WithMutablility {
            mutable: true,
            value,
        }
    }

    fn source() -> Module<'static> {
        let mut module = Module::empty();
        let array = module.signatures.push(SignatureData::Array {
            ty: field(StorageType::Val(Type::I32)),
            shared: false,
        });
        let function_signature = module.signatures.push(SignatureData::Func {
            params: vec![],
            returns: vec![Type::I32],
            shared: false,
        });
        let mut body = FunctionBody::new(&module, function_signature);
        let one = body.add_op(
            body.entry,
            Operator::I32Const { value: 1 },
            &[],
            &[Type::I32],
        );
        let zero = body.add_op(
            body.entry,
            Operator::I32Const { value: 0 },
            &[],
            &[Type::I32],
        );
        let value = body.add_op(
            body.entry,
            Operator::I32Const { value: 42 },
            &[],
            &[Type::I32],
        );
        let array_value = body.add_op(
            body.entry,
            Operator::ArrayNewDefault { sig: array },
            &[one],
            &[Type::Heap(WithNullable {
                nullable: false,
                value: HeapType::Sig { sig_index: array },
            })],
        );
        body.add_op(
            body.entry,
            Operator::ArraySet { sig: array },
            &[array_value, zero, value],
            &[],
        );
        let result = body.add_op(
            body.entry,
            Operator::ArrayGet { sig: array },
            &[array_value, zero],
            &[Type::I32],
        );
        body.set_terminator(
            body.entry,
            Terminator::Return {
                values: vec![result],
            },
        );
        let function = module.funcs.push(FuncDecl::Body(
            function_signature,
            "read_array".to_owned(),
            body,
        ));
        module.exports.push(Export {
            name: "read_array".to_owned(),
            kind: ExportKind::Func(function),
        });
        module
    }

    #[test]
    fn lowers_array_allocation_and_get() {
        let artifact = emit_scalar_array_subset(&source(), &CoreGcOptions::default())
            .expect("scalar array source");
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core wasm");
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("core validation");
        let engine = Engine::default();
        let module = WasmtimeModule::new(&engine, bytes).expect("core compilation");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("instantiation");
        let function: TypedFunc<(), i32> = instance
            .get_typed_func(&mut store, "read_array")
            .expect("export");
        assert_eq!(function.call(&mut store, ()).expect("call"), 42);
    }
}
