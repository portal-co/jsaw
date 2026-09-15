//! Phase-1 coregc artifact emitter.
//!
//! This emits a pure core-Wasm linear-memory runtime with a bounded bump
//! allocator and typed allocation headers. It is intentionally not yet the
//! full WasmGC-to-core lowering pass: source functions with WasmGC operations
//! fail closed rather than being encoded with incorrect semantics.

use portal_pc_waffle::{
    BlockTarget, FuncDecl, FunctionBody, GlobalData, MemoryArg, MemoryData, MemorySegment, Module,
    Operator, SignatureData, Terminator, Type,
};

use crate::{
    coregc::{CoreGcError, CoreGcInventory},
    coregc_layout::CoreGcDescriptorTable,
};

/// Fixed header ABI reserved for later mark/sweep phases.
pub const COREGC_HEADER_BYTES: u32 = 24;
const COREGC_MAGIC_ALLOCATED: u32 = 0xC0DE_0001;

/// Configuration for the phase-1 pure-Wasm runtime skeleton.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoreGcOptions {
    pub initial_pages: usize,
    pub maximum_pages: Option<usize>,
    pub heap_base: u32,
}

impl Default for CoreGcOptions {
    fn default() -> Self {
        Self {
            initial_pages: 2,
            maximum_pages: None,
            // First page is reserved for future descriptors/runtime metadata;
            // heap payloads begin aligned in the second page.
            heap_base: 0x1_0000,
        }
    }
}

/// A core-Wasm artifact plus the inventory that describes its future heap.
#[derive(Clone, Debug)]
pub struct CoreGcArtifact {
    pub module: Module<'static>,
    pub inventory: CoreGcInventory,
    /// Runtime descriptors copied into immutable reserved memory at offset zero.
    pub descriptors: CoreGcDescriptorTable,
}

/// Emit a core-only runtime skeleton from the typed WasmGC inventory.
///
/// The public allocator has `(type_id, payload_bytes) -> payload_address`.
/// `type_id == 0`, overflow, or a request beyond the fixed phase-1 heap traps.
/// The returned pointer is eight-byte aligned and points after a header whose
/// type ID and payload byte count can be read by later marker/validator code.
pub fn emit_runtime_skeleton(
    source: &Module<'_>,
    options: &CoreGcOptions,
) -> Result<CoreGcArtifact, CoreGcError> {
    if !source.funcs.entries().next().is_none()
        || !source.exports.is_empty()
        || !source.globals.entries().next().is_none()
        || !source.memories.iter().next().is_none()
    {
        return Err(CoreGcError {
            message: "coregc phase 1 only emits a runtime skeleton; source code lowering is not enabled yet"
                .to_owned(),
        });
    }
    if options.initial_pages == 0 {
        return Err(CoreGcError {
            message: "coregc requires at least one initial memory page".to_owned(),
        });
    }
    let initial_bytes = options
        .initial_pages
        .checked_mul(0x1_0000)
        .ok_or_else(|| CoreGcError {
            message: "coregc initial memory size overflows usize".to_owned(),
        })?;
    if usize::try_from(options.heap_base)
        .ok()
        .is_none_or(|base| base >= initial_bytes)
    {
        return Err(CoreGcError {
            message: "coregc heap base must lie inside initial memory".to_owned(),
        });
    }

    let inventory = CoreGcInventory::build(source)?;
    let descriptors = CoreGcDescriptorTable::build(&inventory)?;
    if usize::try_from(options.heap_base)
        .ok()
        .is_none_or(|heap_base| heap_base < descriptors.bytes.len())
    {
        return Err(CoreGcError {
            message: format!(
                "coregc heap base {} overlaps {} bytes of generated descriptors",
                options.heap_base,
                descriptors.bytes.len()
            ),
        });
    }
    let mut module = Module::empty();
    let memory = module.memories.push(MemoryData {
        initial_pages: options.initial_pages,
        maximum_pages: options.maximum_pages,
        segments: vec![MemorySegment {
            offset: 0,
            data: descriptors.bytes.clone(),
        }],
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    let bump = module.globals.push(GlobalData {
        ty: Type::I32,
        value: Some(u64::from(options.heap_base)),
        mutable: true,
    });
    let signature = module.signatures.push(SignatureData::Func {
        params: vec![Type::I32, Type::I32],
        returns: vec![Type::I32],
        shared: false,
    });
    let mut body = FunctionBody::new(&module, signature);
    let entry = body.entry;
    let type_id = body.blocks[entry].params[0].1;
    let payload_bytes = body.blocks[entry].params[1].1;
    let bump_value = body.add_op(
        entry,
        Operator::GlobalGet { global_index: bump },
        &[],
        &[Type::I32],
    );
    let header = i32_const(&mut body, entry, COREGC_HEADER_BYTES);
    let total = body.add_op(
        entry,
        Operator::I32Add,
        &[payload_bytes, header],
        &[Type::I32],
    );
    let seven = i32_const(&mut body, entry, 7);
    let rounded = body.add_op(entry, Operator::I32Add, &[total, seven], &[Type::I32]);
    let alignment_mask = i32_const(&mut body, entry, !7u32);
    let aligned_total = body.add_op(
        entry,
        Operator::I32And,
        &[rounded, alignment_mask],
        &[Type::I32],
    );
    let next = body.add_op(
        entry,
        Operator::I32Add,
        &[bump_value, aligned_total],
        &[Type::I32],
    );
    let wrapped = body.add_op(entry, Operator::I32LtU, &[next, bump_value], &[Type::I32]);
    let pages = body.add_op(
        entry,
        Operator::MemorySize { mem: memory },
        &[],
        &[Type::I32],
    );
    let shift = i32_const(&mut body, entry, 16);
    let limit = body.add_op(entry, Operator::I32Shl, &[pages, shift], &[Type::I32]);
    let out_of_memory = body.add_op(entry, Operator::I32GtU, &[next, limit], &[Type::I32]);
    let invalid_type = body.add_op(entry, Operator::I32Eqz, &[type_id], &[Type::I32]);
    let bad_size = body.add_op(
        entry,
        Operator::I32LtU,
        &[aligned_total, total],
        &[Type::I32],
    );
    let invalid = body.add_op(
        entry,
        Operator::I32Or,
        &[wrapped, out_of_memory],
        &[Type::I32],
    );
    let invalid = body.add_op(
        entry,
        Operator::I32Or,
        &[invalid, invalid_type],
        &[Type::I32],
    );
    let invalid = body.add_op(entry, Operator::I32Or, &[invalid, bad_size], &[Type::I32]);
    let fail = body.add_block();
    let commit = body.add_block();
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: invalid,
            if_true: BlockTarget {
                block: fail,
                args: vec![],
            },
            if_false: BlockTarget {
                block: commit,
                args: vec![],
            },
        },
    );
    body.set_terminator(fail, Terminator::Unreachable);

    // Header: magic, concrete type ID, payload byte count, next-allocation,
    // next-free, reserved. Phase 2 wires list fields into mark/sweep.
    let memory_arg = MemoryArg {
        align: 2,
        offset: 0,
        memory,
    };
    let magic = i32_const(&mut body, commit, COREGC_MAGIC_ALLOCATED);
    body.add_op(
        commit,
        Operator::I32Store { memory: memory_arg },
        &[bump_value, magic],
        &[],
    );
    body.add_op(
        commit,
        Operator::I32Store { memory: memory_arg },
        &[bump_value, type_id],
        &[],
    );
    body.add_op(
        commit,
        Operator::I32Store { memory: memory_arg },
        &[bump_value, payload_bytes],
        &[],
    );
    let zero = i32_const(&mut body, commit, 0);
    for offset in [12_u64, 16, 20] {
        body.add_op(
            commit,
            Operator::I32Store {
                memory: MemoryArg {
                    offset,
                    ..memory_arg
                },
            },
            &[bump_value, zero],
            &[],
        );
    }
    body.add_op(
        commit,
        Operator::GlobalSet { global_index: bump },
        &[next],
        &[],
    );
    let payload = body.add_op(
        commit,
        Operator::I32Add,
        &[bump_value, header],
        &[Type::I32],
    );
    body.set_terminator(
        commit,
        Terminator::Return {
            values: vec![payload],
        },
    );

    let allocator = module.funcs.push(FuncDecl::Body(
        signature,
        "__coregc_alloc_phase1".to_owned(),
        body,
    ));
    module.exports.push(portal_pc_waffle::Export {
        name: "__coregc_alloc_phase1".to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(allocator),
    });
    module.exports.push(portal_pc_waffle::Export {
        name: "memory".to_owned(),
        kind: portal_pc_waffle::ExportKind::Memory(memory),
    });

    Ok(CoreGcArtifact {
        module,
        inventory,
        descriptors,
    })
}

fn i32_const(
    body: &mut FunctionBody,
    block: portal_pc_waffle::Block,
    value: u32,
) -> portal_pc_waffle::Value {
    body.add_op(block, Operator::I32Const { value }, &[], &[Type::I32])
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasmtime::{Engine, Instance, Module as WasmtimeModule, Store, TypedFunc};

    fn allocator(artifact: &CoreGcArtifact) -> (Store<()>, TypedFunc<(i32, i32), i32>) {
        let engine = Engine::default();
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core wasm encodes");
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("artifact validates with default core features");
        let module = WasmtimeModule::new(&engine, bytes).expect("core engine compiles artifact");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("artifact instantiates");
        let allocator = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "__coregc_alloc_phase1")
            .expect("allocator export");
        (store, allocator)
    }

    #[test]
    fn runtime_skeleton_is_core_wasm_without_gc_features() {
        let artifact = emit_runtime_skeleton(&Module::empty(), &CoreGcOptions::default())
            .expect("empty source accepts the phase-1 skeleton");
        for (_, function) in artifact.module.funcs.entries() {
            if let FuncDecl::Body(_, _, body) = function {
                body.validate().expect("generated runtime IR validates");
            }
        }
        let (mut store, allocator) = allocator(&artifact);
        let first = allocator
            .call(&mut store, (1, 1))
            .expect("first allocation");
        let second = allocator
            .call(&mut store, (1, 8))
            .expect("second allocation");
        assert_eq!(
            first,
            0x1_0000 + i32::try_from(COREGC_HEADER_BYTES).unwrap()
        );
        assert_eq!(
            second,
            first + 32,
            "allocation includes aligned header and payload"
        );
    }

    #[test]
    fn runtime_skeleton_embeds_generated_descriptor_table() {
        let artifact = emit_runtime_skeleton(&Module::empty(), &CoreGcOptions::default())
            .expect("runtime skeleton");
        let memory = artifact
            .module
            .memories
            .iter()
            .next()
            .expect("runtime memory");
        let memory = &artifact.module.memories[memory];
        assert_eq!(memory.segments.len(), 1);
        assert_eq!(memory.segments[0].offset, 0);
        assert_eq!(memory.segments[0].data, artifact.descriptors.bytes);
        assert_eq!(
            u32::from_le_bytes(memory.segments[0].data[0..4].try_into().unwrap()),
            crate::coregc_layout::COREGC_DESCRIPTOR_MAGIC
        );
    }

    #[test]
    fn runtime_skeleton_traps_invalid_type_or_capacity_request() {
        let artifact = emit_runtime_skeleton(&Module::empty(), &CoreGcOptions::default())
            .expect("runtime skeleton");
        let (mut store, allocator) = allocator(&artifact);
        assert!(allocator.call(&mut store, (0, 8)).is_err());
        assert!(allocator.call(&mut store, (1, i32::MAX)).is_err());
    }

    #[test]
    fn runtime_skeleton_rejects_source_code_until_lowering_exists() {
        let mut source = Module::empty();
        let signature = source.signatures.push(SignatureData::Func {
            params: vec![],
            returns: vec![],
            shared: false,
        });
        let body = FunctionBody::new(&source, signature);
        source
            .funcs
            .push(FuncDecl::Body(signature, "source".to_owned(), body));
        let error = emit_runtime_skeleton(&source, &CoreGcOptions::default())
            .expect_err("unlowered source functions must fail closed");
        assert!(error.message.contains("source code lowering"));
    }
}
