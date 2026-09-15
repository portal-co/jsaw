//! Inventory and feature gate for the pure-core-Wasm GC fallback.
//!
//! The native backend consumes Waffle's typed WasmGC IR. This module is the
//! first fallback seam: it inventories aggregate signatures before any core
//! lowering can erase their layouts, assigns deterministic runtime type IDs,
//! and rejects source IR shapes that a linear-memory runtime cannot represent
//! safely yet.

use std::collections::BTreeMap;

use portal_pc_waffle::{
    EntityRef, FuncDecl, HeapType, Module, Operator, Signature, SignatureData, StorageType, Type,
    ValueDef,
};

/// Schema carried by a coregc artifact and its descriptor manifest.
pub const COREGC_INVENTORY_SCHEMA: &str = "jsaw.coregc.inventory.v1";

/// A dense, nonzero runtime type ID. `0` is reserved for the null fat pointer.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CoreGcTypeId(u32);

impl CoreGcTypeId {
    pub const fn get(self) -> u32 {
        self.0
    }
}

/// One linear-memory payload kind represented by a WasmGC aggregate signature.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CoreGcTypeKind {
    Struct { fields: Vec<CoreGcStorage> },
    Array { element: CoreGcStorage },
}

/// Storage classification used by generated descriptors/scanners.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CoreGcStorage {
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    /// A concrete managed reference stored as `{address, actual_type_id}`.
    ManagedRef {
        nullable: bool,
        target: Signature,
    },
    /// A dynamic/reference-union value; phase 0 inventories it but does not
    /// claim that direct core lowering exists yet.
    DynamicRef {
        nullable: bool,
        heap: HeapType,
    },
}

/// A deterministic descriptor input. Runtime type IDs are assigned by canonical
/// layout key, never by a Waffle entity index exposed outside the compiler.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoreGcType {
    pub id: CoreGcTypeId,
    pub signature: Signature,
    pub kind: CoreGcTypeKind,
    pub shared: bool,
}

/// Type inventory required to generate a coregc descriptor table.
#[derive(Clone, Debug, Default)]
pub struct CoreGcInventory {
    pub schema: &'static str,
    pub types: Vec<CoreGcType>,
    /// Waffle signature -> generated nonzero runtime type ID.
    pub type_ids: BTreeMap<Signature, CoreGcTypeId>,
    /// Native WasmGC operations found before a core backend erases them.
    pub gc_operations: Vec<CoreGcOperation>,
}

/// A typed WasmGC operation that needs explicit fallback lowering.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoreGcOperation {
    pub function_index: usize,
    pub value_index: usize,
    pub name: &'static str,
}

/// A fail-closed unsupported-source diagnostic.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoreGcError {
    pub message: String,
}

impl std::fmt::Display for CoreGcError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl std::error::Error for CoreGcError {}

impl CoreGcInventory {
    /// Inventory the module's GC aggregate signatures.
    ///
    /// Type IDs are stable for equivalent signature sets even when entity
    /// allocation order differs: candidates are sorted by a canonical layout
    /// representation before IDs are assigned. The source `Signature` remains
    /// in the record solely for the lowering pass; it is never a runtime ID.
    pub fn build(module: &Module<'_>) -> Result<Self, CoreGcError> {
        let mut candidates = Vec::new();
        for (signature, data) in module.signatures.entries() {
            let kind = match data {
                SignatureData::Struct { fields, shared } => {
                    let fields = fields
                        .iter()
                        .map(|field| storage(field.value))
                        .collect::<Result<Vec<_>, _>>()?;
                    Some((CoreGcTypeKind::Struct { fields }, *shared))
                }
                SignatureData::Array { ty, shared } => {
                    let element = storage(ty.value)?;
                    Some((CoreGcTypeKind::Array { element }, *shared))
                }
                SignatureData::Func { .. } | SignatureData::Import { .. } | SignatureData::None => {
                    None
                }
                unsupported => {
                    return Err(CoreGcError {
                        message: format!(
                            "coregc does not recognize GC signature {}: {unsupported:?}",
                            signature.index()
                        ),
                    });
                }
            };
            if let Some((kind, shared)) = kind {
                if shared {
                    return Err(CoreGcError {
                        message: format!(
                            "coregc does not support shared GC signature {}",
                            signature.index()
                        ),
                    });
                }
                candidates.push((canonical_kind(&kind), signature, kind));
            }
        }
        candidates.sort_by(|left, right| left.0.cmp(&right.0).then(left.1.cmp(&right.1)));

        let mut inventory = Self {
            schema: COREGC_INVENTORY_SCHEMA,
            types: Vec::with_capacity(candidates.len()),
            type_ids: BTreeMap::new(),
            gc_operations: Vec::new(),
        };
        for (index, (_, signature, kind)) in candidates.into_iter().enumerate() {
            let id = u32::try_from(index + 1).map_err(|_| CoreGcError {
                message: "coregc type ID space exhausted".to_owned(),
            })?;
            let id = CoreGcTypeId(id);
            inventory.type_ids.insert(signature, id);
            inventory.types.push(CoreGcType {
                id,
                signature,
                kind,
                shared: false,
            });
        }
        for (function, declaration) in module.funcs.entries() {
            if let FuncDecl::Body(_, _, body) = declaration {
                for (value, definition) in body.values.entries() {
                    if let ValueDef::Operator(operator, _, _) = definition {
                        if let Some(name) = gc_operator_name(operator) {
                            inventory.gc_operations.push(CoreGcOperation {
                                function_index: function.index(),
                                value_index: value.index(),
                                name,
                            });
                        }
                    }
                }
            }
        }
        Ok(inventory)
    }

    pub fn id_for(&self, signature: Signature) -> Option<CoreGcTypeId> {
        self.type_ids.get(&signature).copied()
    }
}

/// Operations which cannot be silently reinterpreted by a core backend.
fn gc_operator_name(operator: &Operator) -> Option<&'static str> {
    Some(match operator {
        Operator::StructNew { .. } => "struct.new",
        Operator::StructGet { .. } => "struct.get",
        Operator::StructSet { .. } => "struct.set",
        Operator::StructNewDefault { .. } => "struct.new_default",
        Operator::StructGetS { .. } => "struct.get_s",
        Operator::StructGetU { .. } => "struct.get_u",
        Operator::ArrayNew { .. } => "array.new",
        Operator::ArrayNewFixed { .. } => "array.new_fixed",
        Operator::ArrayGet { .. } => "array.get",
        Operator::ArraySet { .. } => "array.set",
        Operator::ArrayFill { .. } => "array.fill",
        Operator::ArrayCopy { .. } => "array.copy",
        Operator::ArrayLen => "array.len",
        Operator::ArrayNewDefault { .. } => "array.new_default",
        Operator::ArrayNewData { .. } => "array.new_data",
        Operator::ArrayNewElem { .. } => "array.new_elem",
        Operator::ArrayGetS { .. } => "array.get_s",
        Operator::ArrayGetU { .. } => "array.get_u",
        Operator::ArrayInitData { .. } => "array.init_data",
        Operator::ArrayInitElem { .. } => "array.init_elem",
        Operator::RefEq => "ref.eq",
        Operator::RefI31 => "ref.i31",
        Operator::I31GetS => "i31.get_s",
        Operator::I31GetU => "i31.get_u",
        Operator::RefTest { .. } => "ref.test",
        Operator::RefCast { .. } => "ref.cast",
        _ => return None,
    })
}

fn storage(storage: StorageType) -> Result<CoreGcStorage, CoreGcError> {
    Ok(match storage {
        StorageType::I8 => CoreGcStorage::I8,
        StorageType::I16 => CoreGcStorage::I16,
        StorageType::Val(Type::I32) => CoreGcStorage::I32,
        StorageType::Val(Type::I64) => CoreGcStorage::I64,
        StorageType::Val(Type::F32) => CoreGcStorage::F32,
        StorageType::Val(Type::F64) => CoreGcStorage::F64,
        StorageType::Val(Type::Heap(reference)) => match reference.value {
            HeapType::Sig { sig_index } => CoreGcStorage::ManagedRef {
                nullable: reference.nullable,
                target: sig_index,
            },
            HeapType::Any | HeapType::Eq | HeapType::I31 | HeapType::Struct | HeapType::Array => {
                CoreGcStorage::DynamicRef {
                    nullable: reference.nullable,
                    heap: reference.value,
                }
            }
            unsupported => {
                return Err(CoreGcError {
                    message: format!("coregc does not support heap storage type {unsupported:?}"),
                });
            }
        },
        unsupported => {
            return Err(CoreGcError {
                message: format!("coregc does not support storage type {unsupported:?}"),
            });
        }
    })
}

fn canonical_kind(kind: &CoreGcTypeKind) -> String {
    match kind {
        CoreGcTypeKind::Struct { fields } => format!(
            "struct({})",
            fields
                .iter()
                .map(canonical_storage)
                .collect::<Vec<_>>()
                .join(",")
        ),
        CoreGcTypeKind::Array { element } => format!("array({})", canonical_storage(element)),
    }
}

fn canonical_storage(storage: &CoreGcStorage) -> String {
    match storage {
        CoreGcStorage::I8 => "i8".to_owned(),
        CoreGcStorage::I16 => "i16".to_owned(),
        CoreGcStorage::I32 => "i32".to_owned(),
        CoreGcStorage::I64 => "i64".to_owned(),
        CoreGcStorage::F32 => "f32".to_owned(),
        CoreGcStorage::F64 => "f64".to_owned(),
        CoreGcStorage::ManagedRef { nullable, target } => {
            format!("ref:{}:{}", u8::from(*nullable), target.index())
        }
        CoreGcStorage::DynamicRef { nullable, heap } => {
            format!("dynamic:{}:{heap:?}", u8::from(*nullable))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use portal_pc_waffle::{SignatureData, WithMutablility, WithNullable};

    fn field(value: StorageType) -> WithMutablility<StorageType> {
        WithMutablility {
            value,
            mutable: true,
        }
    }

    #[test]
    fn inventory_assigns_nonzero_ids_and_preserves_fat_reference_layout() {
        let mut module = Module::empty();
        let node = module.signatures.push(SignatureData::Struct {
            fields: vec![field(StorageType::Val(Type::I32))],
            shared: false,
        });
        let pair = module.signatures.push(SignatureData::Struct {
            fields: vec![field(StorageType::Val(Type::Heap(WithNullable {
                value: HeapType::Sig { sig_index: node },
                nullable: true,
            })))],
            shared: false,
        });

        let inventory = CoreGcInventory::build(&module).expect("supported inventory");
        assert_eq!(inventory.types.len(), 2);
        assert!(inventory.id_for(node).is_some());
        assert!(inventory.id_for(pair).is_some());
        let pair_descriptor = inventory
            .types
            .iter()
            .find(|descriptor| descriptor.signature == pair)
            .expect("pair descriptor");
        assert!(matches!(
            &pair_descriptor.kind,
            CoreGcTypeKind::Struct { fields }
                if matches!(fields.as_slice(), [CoreGcStorage::ManagedRef { nullable: true, target }] if *target == node)
        ));
    }

    #[test]
    fn inventory_records_native_gc_operations() {
        let mut module = Module::empty();
        let signature = module.signatures.push(SignatureData::Func {
            params: vec![],
            returns: vec![],
            shared: false,
        });
        let mut body = portal_pc_waffle::FunctionBody::new(&module, signature);
        let seven = body.add_op(
            body.entry,
            Operator::I32Const { value: 7 },
            &[],
            &[Type::I32],
        );
        body.add_op(
            body.entry,
            Operator::RefI31,
            &[seven],
            &[Type::Heap(WithNullable {
                nullable: false,
                value: HeapType::I31,
            })],
        );
        module
            .funcs
            .push(FuncDecl::Body(signature, "uses_i31".to_owned(), body));
        let inventory = CoreGcInventory::build(&module).expect("inventory");
        assert_eq!(
            inventory
                .gc_operations
                .iter()
                .map(|operation| operation.name)
                .collect::<Vec<_>>(),
            ["ref.i31"]
        );
    }

    #[test]
    fn inventory_rejects_shared_gc_types() {
        let mut module = Module::empty();
        module.signatures.push(SignatureData::Array {
            ty: field(StorageType::I8),
            shared: true,
        });
        let error = CoreGcInventory::build(&module).expect_err("shared GC type is unsupported");
        assert!(error.message.contains("shared"));
    }
}
