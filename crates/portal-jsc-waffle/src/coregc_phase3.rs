use portal_pc_waffle::{
    BlockTarget, FuncDecl, FunctionBody, MemoryArg, Module, Operator, SignatureData, Terminator,
    Type,
};

fn i32_const(
    body: &mut FunctionBody,
    block: portal_pc_waffle::Block,
    value: u32,
) -> portal_pc_waffle::Value {
    body.add_op(block, Operator::I32Const { value }, &[], &[Type::I32])
}

/// Functions that form the Phase-3 precise shadow-root ABI.
pub(crate) struct ShadowRootFunctions {
    pub(crate) push: portal_pc_waffle::Func,
    pub(crate) pop: portal_pc_waffle::Func,
    pub(crate) store: portal_pc_waffle::Func,
    pub(crate) clear: portal_pc_waffle::Func,
}

/// Generate frame management for exact `(address, type_id)` root pairs.
///
/// A frame is `{ previous: i32, slot_count: i32, slots: [addr, type] }` in
/// reserved linear memory. Frames are append-only during an activation; popping
/// resets the bump only after proving the frame is the stack head.
pub(crate) fn add_shadow_roots(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
    heap_base: u32,
    root_head: portal_pc_waffle::Global,
    root_bump: portal_pc_waffle::Global,
) -> ShadowRootFunctions {
    let push = add_root_push(module, memory, heap_base, root_head, root_bump);
    let pop = add_root_pop(module, memory, root_head, root_bump);
    let store = add_root_store(module, memory);
    let clear = add_root_clear(module, memory);
    ShadowRootFunctions {
        push,
        pop,
        store,
        clear,
    }
}

fn add_root_push(
    module: &mut Module<'static>,
    _memory: portal_pc_waffle::Memory,
    heap_base: u32,
    root_head: portal_pc_waffle::Global,
    root_bump: portal_pc_waffle::Global,
) -> portal_pc_waffle::Func {
    let signature = module.signatures.push(SignatureData::Func {
        params: vec![Type::I32],
        returns: vec![Type::I32],
        shared: false,
    });
    let mut body = FunctionBody::new(module, signature);
    let entry = body.entry;
    let slots = body.blocks[entry].params[0].1;
    let frame = body.add_op(
        entry,
        Operator::GlobalGet {
            global_index: root_bump,
        },
        &[],
        &[Type::I32],
    );
    let eight = i32_const(&mut body, entry, 8);
    let bytes = body.add_op(entry, Operator::I32Mul, &[slots, eight], &[Type::I32]);
    let header = i32_const(&mut body, entry, 8);
    let total = body.add_op(entry, Operator::I32Add, &[bytes, header], &[Type::I32]);
    let next = body.add_op(entry, Operator::I32Add, &[frame, total], &[Type::I32]);
    let wrapped = body.add_op(entry, Operator::I32LtU, &[next, frame], &[Type::I32]);
    let limit = i32_const(&mut body, entry, heap_base);
    let collides = body.add_op(entry, Operator::I32GtU, &[next, limit], &[Type::I32]);
    let invalid = body.add_op(entry, Operator::I32Or, &[wrapped, collides], &[Type::I32]);
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
    let previous = body.add_op(
        commit,
        Operator::GlobalGet {
            global_index: root_head,
        },
        &[],
        &[Type::I32],
    );
    let mem = MemoryArg {
        align: 2,
        offset: 0,
        memory: _memory,
    };
    body.add_op(
        commit,
        Operator::I32Store { memory: mem },
        &[frame, previous],
        &[],
    );
    body.add_op(
        commit,
        Operator::I32Store {
            memory: MemoryArg { offset: 4, ..mem },
        },
        &[frame, slots],
        &[],
    );
    body.add_op(
        commit,
        Operator::GlobalSet {
            global_index: root_head,
        },
        &[frame],
        &[],
    );
    body.add_op(
        commit,
        Operator::GlobalSet {
            global_index: root_bump,
        },
        &[next],
        &[],
    );
    body.set_terminator(
        commit,
        Terminator::Return {
            values: vec![frame],
        },
    );
    module.funcs.push(FuncDecl::Body(
        signature,
        "__coregc_push_frame_phase3".to_owned(),
        body,
    ))
}

fn add_root_pop(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
    root_head: portal_pc_waffle::Global,
    root_bump: portal_pc_waffle::Global,
) -> portal_pc_waffle::Func {
    let signature = module.signatures.push(SignatureData::Func {
        params: vec![Type::I32],
        returns: vec![],
        shared: false,
    });
    let mut body = FunctionBody::new(module, signature);
    let entry = body.entry;
    let frame = body.blocks[entry].params[0].1;
    let head = body.add_op(
        entry,
        Operator::GlobalGet {
            global_index: root_head,
        },
        &[],
        &[Type::I32],
    );
    let matches_head = body.add_op(entry, Operator::I32Eq, &[frame, head], &[Type::I32]);
    let fail = body.add_block();
    let commit = body.add_block();
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: matches_head,
            if_true: BlockTarget {
                block: commit,
                args: vec![],
            },
            if_false: BlockTarget {
                block: fail,
                args: vec![],
            },
        },
    );
    body.set_terminator(fail, Terminator::Unreachable);
    // Restore the linked-list head before rewinding the reserved frame region.
    let previous = body.add_op(
        commit,
        Operator::I32Load {
            memory: MemoryArg {
                align: 2,
                offset: 0,
                memory,
            },
        },
        &[frame],
        &[Type::I32],
    );
    body.add_op(
        commit,
        Operator::GlobalSet {
            global_index: root_head,
        },
        &[previous],
        &[],
    );
    body.add_op(
        commit,
        Operator::GlobalSet {
            global_index: root_bump,
        },
        &[frame],
        &[],
    );
    body.set_terminator(commit, Terminator::Return { values: vec![] });
    module.funcs.push(FuncDecl::Body(
        signature,
        "__coregc_pop_frame_phase3".to_owned(),
        body,
    ))
}

fn add_root_store(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
) -> portal_pc_waffle::Func {
    let signature = module.signatures.push(SignatureData::Func {
        params: vec![Type::I32, Type::I32, Type::I32, Type::I32],
        returns: vec![],
        shared: false,
    });
    let mut body = FunctionBody::new(module, signature);
    let entry = body.entry;
    let frame = body.blocks[entry].params[0].1;
    let slot = body.blocks[entry].params[1].1;
    let address = body.blocks[entry].params[2].1;
    let type_id = body.blocks[entry].params[3].1;
    let count = body.add_op(
        entry,
        Operator::I32Load {
            memory: MemoryArg {
                align: 2,
                offset: 4,
                memory,
            },
        },
        &[frame],
        &[Type::I32],
    );
    let valid = body.add_op(entry, Operator::I32LtU, &[slot, count], &[Type::I32]);
    let fail = body.add_block();
    let commit = body.add_block();
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: valid,
            if_true: BlockTarget {
                block: commit,
                args: vec![],
            },
            if_false: BlockTarget {
                block: fail,
                args: vec![],
            },
        },
    );
    body.set_terminator(fail, Terminator::Unreachable);
    let eight = i32_const(&mut body, commit, 8);
    let offset = body.add_op(commit, Operator::I32Mul, &[slot, eight], &[Type::I32]);
    let prefix = i32_const(&mut body, commit, 8);
    let offset = body.add_op(commit, Operator::I32Add, &[offset, prefix], &[Type::I32]);
    let pointer = body.add_op(commit, Operator::I32Add, &[frame, offset], &[Type::I32]);
    let mem = MemoryArg {
        align: 2,
        offset: 0,
        memory,
    };
    body.add_op(
        commit,
        Operator::I32Store { memory: mem },
        &[pointer, address],
        &[],
    );
    body.add_op(
        commit,
        Operator::I32Store {
            memory: MemoryArg { offset: 4, ..mem },
        },
        &[pointer, type_id],
        &[],
    );
    body.set_terminator(commit, Terminator::Return { values: vec![] });
    module.funcs.push(FuncDecl::Body(
        signature,
        "__coregc_root_store_phase3".to_owned(),
        body,
    ))
}

fn add_root_clear(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
) -> portal_pc_waffle::Func {
    let signature = module.signatures.push(SignatureData::Func {
        params: vec![Type::I32, Type::I32],
        returns: vec![],
        shared: false,
    });
    let mut body = FunctionBody::new(module, signature);
    let entry = body.entry;
    let frame = body.blocks[entry].params[0].1;
    let slot = body.blocks[entry].params[1].1;
    let count = body.add_op(
        entry,
        Operator::I32Load {
            memory: MemoryArg {
                align: 2,
                offset: 4,
                memory,
            },
        },
        &[frame],
        &[Type::I32],
    );
    let valid = body.add_op(entry, Operator::I32LtU, &[slot, count], &[Type::I32]);
    let fail = body.add_block();
    let commit = body.add_block();
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: valid,
            if_true: BlockTarget {
                block: commit,
                args: vec![],
            },
            if_false: BlockTarget {
                block: fail,
                args: vec![],
            },
        },
    );
    body.set_terminator(fail, Terminator::Unreachable);
    let eight = i32_const(&mut body, commit, 8);
    let offset = body.add_op(commit, Operator::I32Mul, &[slot, eight], &[Type::I32]);
    let prefix = i32_const(&mut body, commit, 8);
    let offset = body.add_op(commit, Operator::I32Add, &[offset, prefix], &[Type::I32]);
    let pointer = body.add_op(commit, Operator::I32Add, &[frame, offset], &[Type::I32]);
    let zero = i32_const(&mut body, commit, 0);
    let mem = MemoryArg {
        align: 2,
        offset: 0,
        memory,
    };
    body.add_op(
        commit,
        Operator::I32Store { memory: mem },
        &[pointer, zero],
        &[],
    );
    body.add_op(
        commit,
        Operator::I32Store {
            memory: MemoryArg { offset: 4, ..mem },
        },
        &[pointer, zero],
        &[],
    );
    body.set_terminator(commit, Terminator::Return { values: vec![] });
    module.funcs.push(FuncDecl::Body(
        signature,
        "__coregc_root_clear_phase3".to_owned(),
        body,
    ))
}

/// Generate the Phase-3 root-aware collection checkpoint. It marks every
/// exact root pair in the linked frame chain. Full descriptor scanning/sweep
/// stays behind Phase-2's operation gate, so this function deliberately does
/// not reclaim allocations yet: preserving a marked root is more important
/// than unsafely freeing an object whose descriptor scanner is not generated.
pub(crate) fn add_root_collect(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
    _allocation_head: portal_pc_waffle::Global,
    root_head: portal_pc_waffle::Global,
    marker: portal_pc_waffle::Func,
) -> portal_pc_waffle::Func {
    let signature = module.signatures.push(SignatureData::Func {
        params: vec![],
        returns: vec![],
        shared: false,
    });
    let mut body = FunctionBody::new(module, signature);
    let entry = body.entry;
    let loop_block = body.add_block();
    let slot_loop = body.add_block();
    let mark_slot = body.add_block();
    let next_frame = body.add_block();
    let done = body.add_block();
    let head = body.add_op(
        entry,
        Operator::GlobalGet {
            global_index: root_head,
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
    let frame = body.add_blockparam(loop_block, Type::I32);
    let frame_is_null = body.add_op(loop_block, Operator::I32Eqz, &[frame], &[Type::I32]);
    let first_slot = i32_const(&mut body, loop_block, 0);
    body.set_terminator(
        loop_block,
        Terminator::CondBr {
            cond: frame_is_null,
            if_true: BlockTarget {
                block: done,
                args: vec![],
            },
            if_false: BlockTarget {
                block: slot_loop,
                args: vec![frame, first_slot],
            },
        },
    );
    let slot_frame = body.add_blockparam(slot_loop, Type::I32);
    let slot = body.add_blockparam(slot_loop, Type::I32);
    let count = body.add_op(
        slot_loop,
        Operator::I32Load {
            memory: MemoryArg {
                align: 2,
                offset: 4,
                memory,
            },
        },
        &[slot_frame],
        &[Type::I32],
    );
    let exhausted = body.add_op(slot_loop, Operator::I32Eq, &[slot, count], &[Type::I32]);
    body.set_terminator(
        slot_loop,
        Terminator::CondBr {
            cond: exhausted,
            if_true: BlockTarget {
                block: next_frame,
                args: vec![slot_frame],
            },
            if_false: BlockTarget {
                block: mark_slot,
                args: vec![slot_frame, slot],
            },
        },
    );
    let mark_frame = body.add_blockparam(mark_slot, Type::I32);
    let mark_index = body.add_blockparam(mark_slot, Type::I32);
    let eight = i32_const(&mut body, mark_slot, 8);
    let slot_offset = body.add_op(
        mark_slot,
        Operator::I32Mul,
        &[mark_index, eight],
        &[Type::I32],
    );
    let header = i32_const(&mut body, mark_slot, 8);
    let slot_offset = body.add_op(
        mark_slot,
        Operator::I32Add,
        &[slot_offset, header],
        &[Type::I32],
    );
    let pointer = body.add_op(
        mark_slot,
        Operator::I32Add,
        &[mark_frame, slot_offset],
        &[Type::I32],
    );
    let address = body.add_op(
        mark_slot,
        Operator::I32Load {
            memory: MemoryArg {
                align: 2,
                offset: 0,
                memory,
            },
        },
        &[pointer],
        &[Type::I32],
    );
    let type_id = body.add_op(
        mark_slot,
        Operator::I32Load {
            memory: MemoryArg {
                align: 2,
                offset: 4,
                memory,
            },
        },
        &[pointer],
        &[Type::I32],
    );
    body.add_op(
        mark_slot,
        Operator::Call {
            function_index: marker,
        },
        &[address, type_id],
        &[],
    );
    let one = i32_const(&mut body, mark_slot, 1);
    let next_slot = body.add_op(
        mark_slot,
        Operator::I32Add,
        &[mark_index, one],
        &[Type::I32],
    );
    body.set_terminator(
        mark_slot,
        Terminator::Br {
            target: BlockTarget {
                block: slot_loop,
                args: vec![mark_frame, next_slot],
            },
        },
    );
    let next_input = body.add_blockparam(next_frame, Type::I32);
    let previous = body.add_op(
        next_frame,
        Operator::I32Load {
            memory: MemoryArg {
                align: 2,
                offset: 0,
                memory,
            },
        },
        &[next_input],
        &[Type::I32],
    );
    body.set_terminator(
        next_frame,
        Terminator::Br {
            target: BlockTarget {
                block: loop_block,
                args: vec![previous],
            },
        },
    );
    body.set_terminator(done, Terminator::Return { values: vec![] });
    module.funcs.push(FuncDecl::Body(
        signature,
        "__coregc_collect".to_owned(),
        body,
    ))
}
