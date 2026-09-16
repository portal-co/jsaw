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
    coregc_phase3::add_shadow_roots,
};

/// Fixed header ABI reserved for later mark/sweep phases.
pub const COREGC_HEADER_BYTES: u32 = 24;
const COREGC_MAGIC_ALLOCATED: u32 = 0xC0DE_0001;
pub(crate) const COREGC_FLAG_MARK: u32 = 1 << 1;

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
    let inventory = CoreGcInventory::build(source)?;
    if let Some(operation) = inventory.gc_operations.first() {
        return Err(CoreGcError {
            message: format!(
                "coregc phase 1 cannot lower {} in function {} at value {}; source code lowering is not enabled yet",
                operation.name, operation.function_index, operation.value_index
            ),
        });
    }
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
    // Head is a payload address. The allocation header stores the previous
    // head at offset 12, providing the intrusive list Phase 2 will sweep.
    let allocation_head = module.globals.push(GlobalData {
        ty: Type::I32,
        value: Some(0),
        mutable: true,
    });
    let root_stack_start = u32::try_from((descriptors.bytes.len() + 7) & !7)
        .map_err(|_| CoreGcError {
            message: "coregc descriptor length exceeds u32".to_owned(),
        })?
        .max(8);
    if root_stack_start >= options.heap_base {
        return Err(CoreGcError {
            message: "coregc descriptor table leaves no room for the shadow-root stack".to_owned(),
        });
    }
    // Shadow frames live below the managed heap and grow upward. They cannot
    // alias the bump heap, and a push checks the heap-base boundary.
    let root_head = module.globals.push(GlobalData {
        ty: Type::I32,
        value: Some(0),
        mutable: true,
    });
    let root_bump = module.globals.push(GlobalData {
        ty: Type::I32,
        value: Some(u64::from(root_stack_start)),
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
    let needs_grow = body.add_op(entry, Operator::I32GtU, &[next, limit], &[Type::I32]);
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
        &[wrapped, invalid_type],
        &[Type::I32],
    );
    let invalid = body.add_op(entry, Operator::I32Or, &[invalid, bad_size], &[Type::I32]);
    let fail = body.add_block();
    let grow_check = body.add_block();
    let grow = body.add_block();
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
                block: grow_check,
                args: vec![],
            },
        },
    );
    body.set_terminator(fail, Terminator::Unreachable);
    body.set_terminator(
        grow_check,
        Terminator::CondBr {
            cond: needs_grow,
            if_true: BlockTarget {
                block: grow,
                args: vec![],
            },
            if_false: BlockTarget {
                block: commit,
                args: vec![],
            },
        },
    );
    // `memory.grow` returns -1 on failure. Grow only the pages needed for
    // this allocation; an engine-enforced maximum therefore remains a trap.
    let deficit = body.add_op(grow, Operator::I32Sub, &[next, limit], &[Type::I32]);
    let page_mask = i32_const(&mut body, grow, 0xffff);
    let rounded_deficit = body.add_op(grow, Operator::I32Add, &[deficit, page_mask], &[Type::I32]);
    let page_shift = i32_const(&mut body, grow, 16);
    let grow_pages = body.add_op(
        grow,
        Operator::I32ShrU,
        &[rounded_deficit, page_shift],
        &[Type::I32],
    );
    let previous_pages = body.add_op(
        grow,
        Operator::MemoryGrow { mem: memory },
        &[grow_pages],
        &[Type::I32],
    );
    let grow_failed_value = i32_const(&mut body, grow, u32::MAX);
    let grow_failed = body.add_op(
        grow,
        Operator::I32Eq,
        &[previous_pages, grow_failed_value],
        &[Type::I32],
    );
    body.set_terminator(
        grow,
        Terminator::CondBr {
            cond: grow_failed,
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
        Operator::I32Store {
            memory: MemoryArg {
                offset: 4,
                ..memory_arg
            },
        },
        &[bump_value, type_id],
        &[],
    );
    body.add_op(
        commit,
        Operator::I32Store {
            memory: MemoryArg {
                offset: 8,
                ..memory_arg
            },
        },
        &[bump_value, payload_bytes],
        &[],
    );
    let previous_allocation = body.add_op(
        commit,
        Operator::GlobalGet {
            global_index: allocation_head,
        },
        &[],
        &[Type::I32],
    );
    body.add_op(
        commit,
        Operator::I32Store {
            memory: MemoryArg {
                offset: 12,
                ..memory_arg
            },
        },
        &[bump_value, previous_allocation],
        &[],
    );
    let zero = i32_const(&mut body, commit, 0);
    for offset in [16_u64, 20] {
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
    body.add_op(
        commit,
        Operator::GlobalSet {
            global_index: allocation_head,
        },
        &[payload],
        &[],
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
    let validator = add_ref_validator(&mut module, memory);
    let marker = add_mark_ref(&mut module, memory, validator);
    let array_bounds = add_array_bounds_check(&mut module, memory, validator);
    let roots = add_shadow_roots(&mut module, memory, options.heap_base, root_head, root_bump);
    let collect = add_empty_root_collect(
        &mut module,
        memory,
        options.heap_base,
        allocation_head,
        bump,
    );
    module.exports.push(portal_pc_waffle::Export {
        name: "__coregc_alloc_phase1".to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(allocator),
    });
    module.exports.push(portal_pc_waffle::Export {
        name: "__coregc_validate_ref_phase1".to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(validator),
    });
    module.exports.push(portal_pc_waffle::Export {
        name: "__coregc_mark_phase2".to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(marker),
    });
    module.exports.push(portal_pc_waffle::Export {
        name: "__coregc_array_bounds_phase1".to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(array_bounds),
    });
    module.exports.push(portal_pc_waffle::Export {
        name: "__coregc_push_frame_phase3".to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(roots.push),
    });
    module.exports.push(portal_pc_waffle::Export {
        name: "__coregc_pop_frame_phase3".to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(roots.pop),
    });
    module.exports.push(portal_pc_waffle::Export {
        name: "__coregc_root_store_phase3".to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(roots.store),
    });
    module.exports.push(portal_pc_waffle::Export {
        name: "__coregc_root_clear_phase3".to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(roots.clear),
    });
    module.exports.push(portal_pc_waffle::Export {
        name: "__coregc_collect_phase2".to_owned(),
        kind: portal_pc_waffle::ExportKind::Func(collect),
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

/// Add the runtime fat-reference validator shared by future scanners and
/// generated direct field accesses. Null is represented *only* as `(0, 0)`.
fn add_ref_validator(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
) -> portal_pc_waffle::Func {
    let signature = module.signatures.push(SignatureData::Func {
        params: vec![Type::I32, Type::I32],
        returns: vec![Type::I32],
        shared: false,
    });
    let mut body = FunctionBody::new(module, signature);
    let entry = body.entry;
    let address = body.blocks[entry].params[0].1;
    let type_id = body.blocks[entry].params[1].1;
    let null = body.add_block();
    let non_null = body.add_block();
    let null_ok = body.add_block();
    let validate = body.add_block();
    let success = body.add_block();
    let fail = body.add_block();
    let address_is_null = body.add_op(entry, Operator::I32Eqz, &[address], &[Type::I32]);
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: address_is_null,
            if_true: BlockTarget {
                block: null,
                args: vec![],
            },
            if_false: BlockTarget {
                block: non_null,
                args: vec![],
            },
        },
    );
    let null_type = body.add_op(null, Operator::I32Eqz, &[type_id], &[Type::I32]);
    body.set_terminator(
        null,
        Terminator::CondBr {
            cond: null_type,
            if_true: BlockTarget {
                block: null_ok,
                args: vec![],
            },
            if_false: BlockTarget {
                block: fail,
                args: vec![],
            },
        },
    );
    body.set_terminator(
        null_ok,
        Terminator::Return {
            values: vec![address],
        },
    );

    let zero_type = body.add_op(non_null, Operator::I32Eqz, &[type_id], &[Type::I32]);
    let header_bytes = i32_const(&mut body, non_null, COREGC_HEADER_BYTES);
    let before_heap = body.add_op(
        non_null,
        Operator::I32LtU,
        &[address, header_bytes],
        &[Type::I32],
    );
    let invalid = body.add_op(
        non_null,
        Operator::I32Or,
        &[zero_type, before_heap],
        &[Type::I32],
    );
    body.set_terminator(
        non_null,
        Terminator::CondBr {
            cond: invalid,
            if_true: BlockTarget {
                block: fail,
                args: vec![],
            },
            if_false: BlockTarget {
                block: validate,
                args: vec![],
            },
        },
    );
    let header_address = body.add_op(
        validate,
        Operator::I32Sub,
        &[address, header_bytes],
        &[Type::I32],
    );
    let memory_arg = MemoryArg {
        align: 2,
        offset: 0,
        memory,
    };
    let magic = body.add_op(
        validate,
        Operator::I32Load { memory: memory_arg },
        &[header_address],
        &[Type::I32],
    );
    let actual_type = body.add_op(
        validate,
        Operator::I32Load {
            memory: MemoryArg {
                offset: 4,
                ..memory_arg
            },
        },
        &[header_address],
        &[Type::I32],
    );
    let expected_magic = i32_const(&mut body, validate, COREGC_MAGIC_ALLOCATED);
    let bad_magic = body.add_op(
        validate,
        Operator::I32Ne,
        &[magic, expected_magic],
        &[Type::I32],
    );
    let bad_type = body.add_op(
        validate,
        Operator::I32Ne,
        &[actual_type, type_id],
        &[Type::I32],
    );
    let invalid = body.add_op(
        validate,
        Operator::I32Or,
        &[bad_magic, bad_type],
        &[Type::I32],
    );
    body.set_terminator(
        validate,
        Terminator::CondBr {
            cond: invalid,
            if_true: BlockTarget {
                block: fail,
                args: vec![],
            },
            if_false: BlockTarget {
                block: success,
                args: vec![],
            },
        },
    );
    body.set_terminator(
        success,
        Terminator::Return {
            values: vec![address],
        },
    );
    body.set_terminator(fail, Terminator::Unreachable);
    module.funcs.push(FuncDecl::Body(
        signature,
        "__coregc_validate_ref_phase1".to_owned(),
        body,
    ))
}

/// Validate a concrete array reference and its unsigned element index.
///
/// The returned index is intentionally identical to the argument, which makes
/// a following typed load/store explicit about its dominating bounds check.
fn add_array_bounds_check(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
    validator: portal_pc_waffle::Func,
) -> portal_pc_waffle::Func {
    let signature = module.signatures.push(SignatureData::Func {
        params: vec![Type::I32, Type::I32, Type::I32],
        returns: vec![Type::I32],
        shared: false,
    });
    let mut body = FunctionBody::new(module, signature);
    let entry = body.entry;
    let address = body.blocks[entry].params[0].1;
    let type_id = body.blocks[entry].params[1].1;
    let index = body.blocks[entry].params[2].1;
    let checked_address = body.add_op(
        entry,
        Operator::Call {
            function_index: validator,
        },
        &[address, type_id],
        &[Type::I32],
    );
    // Array payloads reserve their first word for length. Only generated
    // array accesses call this helper with a concrete array type ID.
    let length = body.add_op(
        entry,
        Operator::I32Load {
            memory: MemoryArg {
                align: 2,
                offset: 0,
                memory,
            },
        },
        &[checked_address],
        &[Type::I32],
    );
    let in_bounds = body.add_op(entry, Operator::I32LtU, &[index, length], &[Type::I32]);
    let success = body.add_block();
    let fail = body.add_block();
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: in_bounds,
            if_true: BlockTarget {
                block: success,
                args: vec![],
            },
            if_false: BlockTarget {
                block: fail,
                args: vec![],
            },
        },
    );
    body.set_terminator(
        success,
        Terminator::Return {
            values: vec![index],
        },
    );
    body.set_terminator(fail, Terminator::Unreachable);
    module.funcs.push(FuncDecl::Body(
        signature,
        "__coregc_array_bounds_phase1".to_owned(),
        body,
    ))
}

/// Mark a validated reference without recursion. Descriptor-driven child
/// traversal is added once reference-field lowering is enabled; this primitive
/// already establishes the header-bit ownership protocol used by roots.
fn add_mark_ref(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
    validator: portal_pc_waffle::Func,
) -> portal_pc_waffle::Func {
    let signature = module.signatures.push(SignatureData::Func {
        params: vec![Type::I32, Type::I32],
        returns: vec![],
        shared: false,
    });
    let mut body = FunctionBody::new(module, signature);
    let entry = body.entry;
    let address = body.blocks[entry].params[0].1;
    let type_id = body.blocks[entry].params[1].1;
    let checked = body.add_op(
        entry,
        Operator::Call {
            function_index: validator,
        },
        &[address, type_id],
        &[Type::I32],
    );
    let null = body.add_block();
    let mark = body.add_block();
    let address_is_null = body.add_op(entry, Operator::I32Eqz, &[checked], &[Type::I32]);
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: address_is_null,
            if_true: BlockTarget {
                block: null,
                args: vec![],
            },
            if_false: BlockTarget {
                block: mark,
                args: vec![],
            },
        },
    );
    body.set_terminator(null, Terminator::Return { values: vec![] });
    let header_bytes = i32_const(&mut body, mark, COREGC_HEADER_BYTES);
    let header = body.add_op(
        mark,
        Operator::I32Sub,
        &[checked, header_bytes],
        &[Type::I32],
    );
    let memory_arg = MemoryArg {
        align: 2,
        offset: 0,
        memory,
    };
    let flags = body.add_op(
        mark,
        Operator::I32Load { memory: memory_arg },
        &[header],
        &[Type::I32],
    );
    let mark_bit = i32_const(&mut body, mark, COREGC_FLAG_MARK);
    let marked = body.add_op(mark, Operator::I32Or, &[flags, mark_bit], &[Type::I32]);
    body.add_op(
        mark,
        Operator::I32Store { memory: memory_arg },
        &[header, marked],
        &[],
    );
    body.set_terminator(mark, Terminator::Return { values: vec![] });
    module.funcs.push(FuncDecl::Body(
        signature,
        "__coregc_mark_phase2".to_owned(),
        body,
    ))
}

/// Generate the Phase-2 empty-root collector. Until shadow frames exist, the
/// only sound collection point has an empty root set: walk every allocation,
/// clear its header, and reset the bump frontier. Stale fat references then
/// fail the existing header validator instead of becoming use-after-free.
fn add_empty_root_collect(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
    heap_base: u32,
    allocation_head: portal_pc_waffle::Global,
    bump: portal_pc_waffle::Global,
) -> portal_pc_waffle::Func {
    let signature = module.signatures.push(SignatureData::Func {
        params: vec![],
        returns: vec![],
        shared: false,
    });
    let mut body = FunctionBody::new(module, signature);
    let entry = body.entry;
    let loop_block = body.add_block();
    let clear_block = body.add_block();
    let done = body.add_block();
    let head = body.add_op(
        entry,
        Operator::GlobalGet {
            global_index: allocation_head,
        },
        &[],
        &[Type::I32],
    );
    body.set_terminator(
        entry,
        Terminator::Br {
            target: BlockTarget {
                block: loop_block,
                args: vec![head],
            },
        },
    );
    let current = body.add_blockparam(loop_block, Type::I32);
    let is_empty = body.add_op(loop_block, Operator::I32Eqz, &[current], &[Type::I32]);
    body.set_terminator(
        loop_block,
        Terminator::CondBr {
            cond: is_empty,
            if_true: BlockTarget {
                block: done,
                args: vec![],
            },
            if_false: BlockTarget {
                block: clear_block,
                args: vec![],
            },
        },
    );
    let memory_arg = MemoryArg {
        align: 2,
        offset: 0,
        memory,
    };
    let header_bytes = i32_const(&mut body, clear_block, COREGC_HEADER_BYTES);
    let header = body.add_op(
        clear_block,
        Operator::I32Sub,
        &[current, header_bytes],
        &[Type::I32],
    );
    // Read the next allocation before invalidating this header.
    let next = body.add_op(
        clear_block,
        Operator::I32Load {
            memory: MemoryArg {
                offset: 12,
                ..memory_arg
            },
        },
        &[header],
        &[Type::I32],
    );
    let zero = i32_const(&mut body, clear_block, 0);
    for offset in [0_u64, 4, 8, 12, 16, 20] {
        body.add_op(
            clear_block,
            Operator::I32Store {
                memory: MemoryArg {
                    offset,
                    ..memory_arg
                },
            },
            &[header, zero],
            &[],
        );
    }
    body.set_terminator(
        clear_block,
        Terminator::Br {
            target: BlockTarget {
                block: loop_block,
                args: vec![next],
            },
        },
    );
    let reset = i32_const(&mut body, done, heap_base);
    let done_zero = i32_const(&mut body, done, 0);
    body.add_op(
        done,
        Operator::GlobalSet {
            global_index: allocation_head,
        },
        &[done_zero],
        &[],
    );
    body.add_op(
        done,
        Operator::GlobalSet { global_index: bump },
        &[reset],
        &[],
    );
    body.set_terminator(done, Terminator::Return { values: vec![] });
    module.funcs.push(FuncDecl::Body(
        signature,
        "__coregc_collect_phase2".to_owned(),
        body,
    ))
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
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core wasm encodes");
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("artifact validates with default core features");
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
    fn allocator_bumps_aligned_and_traps_invalid_type_or_capacity() {
        let artifact = emit_runtime_skeleton(
            &Module::empty(),
            &CoreGcOptions {
                maximum_pages: Some(2),
                ..CoreGcOptions::default()
            },
        )
        .expect("runtime skeleton");
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
        assert!(allocator.call(&mut store, (0, 8)).is_err());
        assert!(allocator.call(&mut store, (1, i32::MAX)).is_err());
    }

    #[test]
    fn allocator_grows_memory_until_its_configured_cap() {
        let artifact = emit_runtime_skeleton(
            &Module::empty(),
            &CoreGcOptions {
                maximum_pages: Some(3),
                ..CoreGcOptions::default()
            },
        )
        .expect("runtime skeleton");
        let engine = Engine::default();
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core wasm encodes");
        let module = WasmtimeModule::new(&engine, bytes).expect("core engine compiles artifact");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("artifact instantiates");
        let allocator = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "__coregc_alloc_phase1")
            .expect("allocator export");
        let memory = instance
            .get_memory(&mut store, "memory")
            .expect("memory export");
        allocator
            .call(&mut store, (1, 70_000))
            .expect("allocation grows one page");
        assert_eq!(memory.size(&store), 3);
        assert!(allocator.call(&mut store, (1, 70_000)).is_err());
    }

    #[test]
    fn array_bounds_check_accepts_in_bounds_and_traps_out_of_bounds() {
        let artifact = emit_runtime_skeleton(&Module::empty(), &CoreGcOptions::default())
            .expect("runtime skeleton");
        let engine = Engine::default();
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core wasm encodes");
        let module = WasmtimeModule::new(&engine, bytes).expect("core engine compiles artifact");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("artifact instantiates");
        // Allocate an array whose payload begins with a length word (= 1).
        // The bounds helper only checks its own argument against that word.
        let allocator = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "__coregc_alloc_phase1")
            .expect("allocator export");
        let bounds = instance
            .get_typed_func::<(i32, i32, i32), i32>(&mut store, "__coregc_array_bounds_phase1")
            .expect("bounds export");
        let address = allocator.call(&mut store, (1, 8)).expect("allocation");
        // Write the length word at payload[0].
        let memory = instance
            .get_memory(&mut store, "memory")
            .expect("memory export");
        memory
            .write(&mut store, (address as u32) as usize, &1u32.to_le_bytes())
            .expect("length write");
        assert_eq!(
            bounds.call(&mut store, (address, 1, 0)).expect("index 0"),
            0
        );
        assert!(bounds.call(&mut store, (address, 1, 1)).is_err());
    }

    #[test]
    fn phase3_shadow_frames_store_clear_and_pop_in_lifo_order() {
        let artifact = emit_runtime_skeleton(&Module::empty(), &CoreGcOptions::default())
            .expect("runtime skeleton");
        let engine = Engine::default();
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core wasm encodes");
        let module = WasmtimeModule::new(&engine, bytes).expect("core engine compiles artifact");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("artifact instantiates");
        let push = instance
            .get_typed_func::<i32, i32>(&mut store, "__coregc_push_frame_phase3")
            .expect("push export");
        let pop = instance
            .get_typed_func::<i32, ()>(&mut store, "__coregc_pop_frame_phase3")
            .expect("pop export");
        let root_store = instance
            .get_typed_func::<(i32, i32, i32, i32), ()>(&mut store, "__coregc_root_store_phase3")
            .expect("root store export");
        let root_clear = instance
            .get_typed_func::<(i32, i32), ()>(&mut store, "__coregc_root_clear_phase3")
            .expect("root clear export");
        let memory = instance
            .get_memory(&mut store, "memory")
            .expect("memory export");
        let first = push.call(&mut store, 1).expect("first frame");
        root_store
            .call(&mut store, (first, 0, 123, 7))
            .expect("store root pair");
        let second = push.call(&mut store, 1).expect("nested frame");
        root_store
            .call(&mut store, (second, 0, 456, 8))
            .expect("nested root pair");
        pop.call(&mut store, second).expect("pop nested frame");
        root_clear
            .call(&mut store, (first, 0))
            .expect("clear parent root");
        pop.call(&mut store, first).expect("pop parent frame");
        let mut slot = [0; 8];
        memory
            .read(&store, usize::try_from(first).unwrap() + 8, &mut slot)
            .expect("frame slot read");
        assert_eq!(slot, [0; 8], "clear zeros both words of the fat root pair");
        assert!(pop.call(&mut store, first).is_err(), "double pop traps");
    }

    #[test]
    fn phase2_mark_sets_the_header_mark_bit_without_changing_type() {
        let artifact = emit_runtime_skeleton(&Module::empty(), &CoreGcOptions::default())
            .expect("runtime skeleton");
        let engine = Engine::default();
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core wasm encodes");
        let module = WasmtimeModule::new(&engine, bytes).expect("core engine compiles artifact");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("artifact instantiates");
        let allocator = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "__coregc_alloc_phase1")
            .expect("allocator export");
        let marker = instance
            .get_typed_func::<(i32, i32), ()>(&mut store, "__coregc_mark_phase2")
            .expect("marker export");
        let memory = instance
            .get_memory(&mut store, "memory")
            .expect("memory export");
        let address = allocator.call(&mut store, (1, 8)).expect("allocation");
        marker
            .call(&mut store, (address, 1))
            .expect("mark live pair");
        let mut flags = [0; 4];
        memory
            .read(
                &store,
                usize::try_from(address).unwrap() - usize::try_from(COREGC_HEADER_BYTES).unwrap(),
                &mut flags,
            )
            .expect("header flags read");
        assert_eq!(
            u32::from_le_bytes(flags),
            COREGC_MAGIC_ALLOCATED | COREGC_FLAG_MARK
        );
    }

    #[test]
    fn phase2_empty_root_collect_reclaims_and_invalidates_allocations() {
        let artifact = emit_runtime_skeleton(&Module::empty(), &CoreGcOptions::default())
            .expect("runtime skeleton");
        let engine = Engine::default();
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core wasm encodes");
        let module = WasmtimeModule::new(&engine, bytes).expect("core engine compiles artifact");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("artifact instantiates");
        let allocator = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "__coregc_alloc_phase1")
            .expect("allocator export");
        let validator = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "__coregc_validate_ref_phase1")
            .expect("validator export");
        let collect = instance
            .get_typed_func::<(), ()>(&mut store, "__coregc_collect_phase2")
            .expect("collector export");
        let first = allocator.call(&mut store, (1, 8)).expect("allocation");
        validator
            .call(&mut store, (first, 1))
            .expect("live before collection");
        collect.call(&mut store, ()).expect("empty-root collection");
        assert!(validator.call(&mut store, (first, 1)).is_err());
        let replacement = allocator
            .call(&mut store, (1, 8))
            .expect("reused allocation");
        assert_eq!(replacement, first, "collector resets the empty-root heap");
    }

    #[test]
    fn fat_reference_validator_accepts_only_matching_live_pairs() {
        let artifact = emit_runtime_skeleton(&Module::empty(), &CoreGcOptions::default())
            .expect("runtime skeleton");
        let engine = Engine::default();
        let bytes = portal_pc_waffle::to_wasm_bytes(&artifact.module).expect("core wasm encodes");
        let module = WasmtimeModule::new(&engine, bytes).expect("core engine compiles artifact");
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).expect("artifact instantiates");
        let allocator = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "__coregc_alloc_phase1")
            .expect("allocator export");
        let validator = instance
            .get_typed_func::<(i32, i32), i32>(&mut store, "__coregc_validate_ref_phase1")
            .expect("validator export");
        let address = allocator.call(&mut store, (1, 8)).expect("allocation");
        assert_eq!(
            validator.call(&mut store, (address, 1)).expect("live pair"),
            address
        );
        assert_eq!(validator.call(&mut store, (0, 0)).expect("null pair"), 0);
        assert!(validator.call(&mut store, (address, 2)).is_err());
        assert!(validator.call(&mut store, (0, 1)).is_err());
        assert!(validator.call(&mut store, (address, 0)).is_err());
    }

    #[test]
    fn runtime_skeleton_names_unsupported_gc_operation() {
        let mut source = Module::empty();
        let signature = source.signatures.push(SignatureData::Func {
            params: vec![],
            returns: vec![],
            shared: false,
        });
        let mut body = FunctionBody::new(&source, signature);
        let one = body.add_op(
            body.entry,
            Operator::I32Const { value: 1 },
            &[],
            &[Type::I32],
        );
        body.add_op(
            body.entry,
            Operator::RefI31,
            &[one],
            &[Type::Heap(portal_pc_waffle::WithNullable {
                nullable: false,
                value: portal_pc_waffle::HeapType::I31,
            })],
        );
        source
            .funcs
            .push(FuncDecl::Body(signature, "source".to_owned(), body));
        let error = emit_runtime_skeleton(&source, &CoreGcOptions::default())
            .expect_err("unlowered GC operations must fail closed");
        assert!(error.message.contains("ref.i31"));
        assert!(error.message.contains("function 0"));
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
