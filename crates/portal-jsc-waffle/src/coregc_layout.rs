//! Deterministic linear-memory layouts and descriptor encoding for coregc.
//!
//! Waffle GC signature indexes are compiler-internal. This module converts
//! their inventory into byte offsets and a compact table containing only stable
//! generated type IDs, so the generated runtime never depends on an entity
//! allocation order.

use crate::coregc::{CoreGcError, CoreGcInventory, CoreGcStorage, CoreGcTypeId, CoreGcTypeKind};
use portal_pc_waffle::EntityRef;

/// Descriptor-table magic for the v1 coregc runtime ABI (`CGD1`).
pub const COREGC_DESCRIPTOR_MAGIC: u32 = 0x4347_4431;
/// Binary descriptor-table schema version.
pub const COREGC_DESCRIPTOR_VERSION: u32 = 1;
const ARRAY_PAYLOAD_BYTES: u32 = u32::MAX;

/// Descriptor `kind` tag for a struct payload (already emitted by v1).
pub const COREGC_DESCRIPTOR_KIND_STRUCT: u32 = 1;
/// Descriptor `kind` tag for an array payload (already emitted by v1).
pub const COREGC_DESCRIPTOR_KIND_ARRAY: u32 = 2;
/// Descriptor `kind` tag reserved for function objects
/// (`docs/plan-coregc-atomic-collector-and-lowering.md` §6.7). No v1 code
/// path emits or interprets this kind; it is reserved now so the v2
/// function-reference/`call_ref` work never needs a second descriptor
/// version bump.
pub const COREGC_DESCRIPTOR_KIND_FUNCTION_OBJECT: u32 = 3;

/// A byte-addressed field/element layout in a managed payload.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoreGcSlotLayout {
    pub offset: u32,
    pub storage: CoreGcStorage,
}

/// Layout of a concrete managed type's payload, excluding the allocation header.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoreGcPayloadLayout {
    pub id: CoreGcTypeId,
    pub payload_alignment: u32,
    /// `None` means a variable-sized array payload.
    pub fixed_payload_bytes: Option<u32>,
    /// Struct fields, or the one array element layout at offset zero.
    pub slots: Vec<CoreGcSlotLayout>,
    /// Array element stride. `None` for structs.
    pub array_stride: Option<u32>,
}

/// Generated read-only runtime descriptor bytes and their decoded layouts.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoreGcDescriptorTable {
    pub layouts: Vec<CoreGcPayloadLayout>,
    pub bytes: Vec<u8>,
}

impl CoreGcDescriptorTable {
    /// Construct deterministic payload layouts and a self-contained binary table.
    pub fn build(inventory: &CoreGcInventory) -> Result<Self, CoreGcError> {
        let mut layouts = Vec::with_capacity(inventory.types.len());
        for ty in &inventory.types {
            let layout = match &ty.kind {
                CoreGcTypeKind::Struct { fields } => {
                    let mut offset = 0;
                    let mut payload_alignment = 1;
                    let mut slots = Vec::with_capacity(fields.len());
                    for storage in fields {
                        let alignment = storage_alignment(storage);
                        payload_alignment = payload_alignment.max(alignment);
                        offset = align_up(offset, alignment)?;
                        slots.push(CoreGcSlotLayout {
                            offset,
                            storage: storage.clone(),
                        });
                        offset = offset.checked_add(storage_size(storage)).ok_or_else(|| {
                            CoreGcError {
                                message: format!(
                                    "coregc struct type {} payload size overflows u32",
                                    ty.id.get()
                                ),
                            }
                        })?;
                    }
                    let fixed_payload_bytes = align_up(offset, payload_alignment)?;
                    CoreGcPayloadLayout {
                        id: ty.id,
                        payload_alignment,
                        fixed_payload_bytes: Some(fixed_payload_bytes),
                        slots,
                        array_stride: None,
                    }
                }
                CoreGcTypeKind::Array { element } => {
                    let payload_alignment = storage_alignment(element);
                    let stride = align_up(storage_size(element), payload_alignment)?;
                    CoreGcPayloadLayout {
                        id: ty.id,
                        payload_alignment: payload_alignment.max(4),
                        fixed_payload_bytes: None,
                        // Payload layout reserves a 4-byte length word before
                        // the element data, matching the generated array
                        // bounds check's `I32Load` at offset 0.
                        slots: vec![CoreGcSlotLayout {
                            offset: 4,
                            storage: element.clone(),
                        }],
                        array_stride: Some(stride),
                    }
                }
            };
            layouts.push(layout);
        }

        let mut bytes = Vec::new();
        push_u32(&mut bytes, COREGC_DESCRIPTOR_MAGIC);
        push_u32(&mut bytes, COREGC_DESCRIPTOR_VERSION);
        push_u32(
            &mut bytes,
            u32::try_from(layouts.len()).map_err(|_| CoreGcError {
                message: "coregc descriptor count exceeds u32".to_owned(),
            })?,
        );
        for layout in &layouts {
            let ty = inventory
                .types
                .iter()
                .find(|ty| ty.id == layout.id)
                .expect("layout originates from inventory type");
            push_u32(&mut bytes, layout.id.get());
            push_u32(
                &mut bytes,
                match ty.kind {
                    CoreGcTypeKind::Struct { .. } => COREGC_DESCRIPTOR_KIND_STRUCT,
                    CoreGcTypeKind::Array { .. } => COREGC_DESCRIPTOR_KIND_ARRAY,
                },
            );
            push_u32(&mut bytes, layout.payload_alignment);
            push_u32(
                &mut bytes,
                layout.fixed_payload_bytes.unwrap_or(ARRAY_PAYLOAD_BYTES),
            );
            push_u32(
                &mut bytes,
                u32::try_from(layout.slots.len()).map_err(|_| CoreGcError {
                    message: "coregc descriptor slot count exceeds u32".to_owned(),
                })?,
            );
            push_u32(&mut bytes, layout.array_stride.unwrap_or(0));
            for slot in &layout.slots {
                push_u32(&mut bytes, slot.offset);
                push_u32(&mut bytes, storage_tag(&slot.storage));
                push_u32(
                    &mut bytes,
                    storage_target_id(&slot.storage, inventory)?.unwrap_or(0),
                );
                push_u32(&mut bytes, storage_size(&slot.storage));
            }
        }
        Ok(Self { layouts, bytes })
    }
}

fn storage_alignment(storage: &CoreGcStorage) -> u32 {
    match storage {
        CoreGcStorage::I8 => 1,
        CoreGcStorage::I16 => 2,
        CoreGcStorage::I32 | CoreGcStorage::F32 => 4,
        CoreGcStorage::I64
        | CoreGcStorage::F64
        | CoreGcStorage::ManagedRef { .. }
        | CoreGcStorage::DynamicRef { .. } => 8,
    }
}

fn storage_size(storage: &CoreGcStorage) -> u32 {
    match storage {
        CoreGcStorage::I8 => 1,
        CoreGcStorage::I16 => 2,
        CoreGcStorage::I32 | CoreGcStorage::F32 => 4,
        CoreGcStorage::I64 | CoreGcStorage::F64 => 8,
        // Every managed reference has an address and its concrete runtime type.
        CoreGcStorage::ManagedRef { .. } | CoreGcStorage::DynamicRef { .. } => 8,
    }
}

fn storage_tag(storage: &CoreGcStorage) -> u32 {
    match storage {
        CoreGcStorage::I8 => 1,
        CoreGcStorage::I16 => 2,
        CoreGcStorage::I32 => 3,
        CoreGcStorage::I64 => 4,
        CoreGcStorage::F32 => 5,
        CoreGcStorage::F64 => 6,
        CoreGcStorage::ManagedRef { .. } => 7,
        CoreGcStorage::DynamicRef { .. } => 8,
    }
}

fn storage_target_id(
    storage: &CoreGcStorage,
    inventory: &CoreGcInventory,
) -> Result<Option<u32>, CoreGcError> {
    match storage {
        CoreGcStorage::ManagedRef { target, .. } => inventory
            .id_for(*target)
            .map(|id| Some(id.get()))
            .ok_or_else(|| CoreGcError {
                message: format!(
                    "coregc descriptor references aggregate signature {} absent from inventory",
                    target.index()
                ),
            }),
        CoreGcStorage::DynamicRef { .. } => Ok(None),
        _ => Ok(None),
    }
}

fn align_up(value: u32, alignment: u32) -> Result<u32, CoreGcError> {
    debug_assert!(alignment.is_power_of_two());
    value
        .checked_add(alignment - 1)
        .map(|value| value & !(alignment - 1))
        .ok_or_else(|| CoreGcError {
            message: "coregc payload layout overflows u32".to_owned(),
        })
}

fn push_u32(bytes: &mut Vec<u8>, value: u32) {
    bytes.extend_from_slice(&value.to_le_bytes());
}

#[cfg(test)]
mod tests {
    use super::*;
    use portal_pc_waffle::{
        HeapType, Module, SignatureData, StorageType, Type, WithMutablility, WithNullable,
    };

    fn field(value: StorageType) -> WithMutablility<StorageType> {
        WithMutablility {
            mutable: true,
            value,
        }
    }

    #[test]
    fn layouts_preserve_packed_offsets_and_fat_reference_width() {
        let mut module = Module::empty();
        let leaf = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        module.signatures.push(SignatureData::Struct {
            fields: vec![
                field(StorageType::I8),
                field(StorageType::Val(Type::I32)),
                field(StorageType::Val(Type::Heap(WithNullable {
                    nullable: true,
                    value: HeapType::Sig { sig_index: leaf },
                }))),
                field(StorageType::I16),
            ],
            shared: false,
        });
        let table =
            CoreGcDescriptorTable::build(&CoreGcInventory::build(&module).expect("inventory"))
                .expect("layout");
        let layout = table
            .layouts
            .iter()
            .find(|layout| layout.slots.len() == 4)
            .expect("four-field struct layout");
        assert_eq!(
            layout
                .slots
                .iter()
                .map(|slot| slot.offset)
                .collect::<Vec<_>>(),
            [0, 4, 8, 16]
        );
        assert_eq!(layout.fixed_payload_bytes, Some(24));
        assert_eq!(
            u32::from_le_bytes(table.bytes[0..4].try_into().unwrap()),
            COREGC_DESCRIPTOR_MAGIC
        );
    }

    #[test]
    fn array_descriptor_uses_fat_reference_stride() {
        let mut module = Module::empty();
        let node = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        module.signatures.push(SignatureData::Array {
            ty: field(StorageType::Val(Type::Heap(WithNullable {
                nullable: true,
                value: HeapType::Sig { sig_index: node },
            }))),
            shared: false,
        });
        let table =
            CoreGcDescriptorTable::build(&CoreGcInventory::build(&module).expect("inventory"))
                .expect("layout");
        let array = table
            .layouts
            .iter()
            .find(|layout| layout.array_stride.is_some())
            .expect("array layout");
        assert_eq!(array.array_stride, Some(8));
        assert_eq!(array.fixed_payload_bytes, None);
    }
}
