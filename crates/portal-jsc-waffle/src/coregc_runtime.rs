//! Generated coregc runtime: allocator, validator, worklist, mark, sweep.
//!
//! Implements the frozen data contract in
//! `docs/plan-coregc-atomic-collector-and-lowering.md` §3 as generated core
//! Wasm code. This module owns the allocation header, free list, descriptor
//! interpretation, and the mark/sweep algorithm. It does not know how a
//! source `struct.new`/`array.get`/... maps to these primitives (that is
//! `coregc_lower`'s job) and it does not know the shadow-frame memory layout
//! beyond calling into `coregc_roots`'s generated root-walk.

use portal_pc_waffle::{
    BlockTarget, Func, FuncDecl, FunctionBody, Global, GlobalData, MemoryArg, MemoryData,
    MemorySegment, Module, Operator, SignatureData, Terminator, Type,
};

use crate::{
    coregc::CoreGcError,
    coregc_layout::{
        COREGC_DESCRIPTOR_KIND_ARRAY, COREGC_DESCRIPTOR_KIND_STRUCT, CoreGcDescriptorTable,
    },
    coregc_roots::{self, ShadowRootFunctions},
};

/// Bytes in the fixed allocation header (`docs/...` §3.2).
pub const COREGC_HEADER_BYTES: u32 = 24;

const FLAG_ALLOCATED: u32 = 1 << 0;
const FLAG_MARK: u32 = 1 << 1;
const FLAG_FREE: u32 = 1 << 2;

/// Bytes in the descriptor table's own header: `magic, version, count`
/// (three `u32` fields), matching `coregc_layout::CoreGcDescriptorTable::build`.
const DESCRIPTOR_TABLE_HEADER_BYTES: u32 = 12;
/// Descriptor row size in bytes, excluding its slots (six `u32` fields):
/// `type_id, kind, payload_alignment, fixed_payload_bytes, slot_count, array_stride`.
const DESCRIPTOR_ROW_HEADER_BYTES: u32 = 24;
/// Descriptor slot size in bytes: `offset, storage_tag, target_type_id, size`.
const DESCRIPTOR_SLOT_BYTES: u32 = 16;
/// `storage_tag` value that marks a scannable managed-reference slot
/// (`coregc_layout::storage_tag`'s `CoreGcStorage::ManagedRef` case).
const STORAGE_TAG_MANAGED_REF: u32 = 7;

/// Distinct trap identities (`docs/...` §10). Stored into `trap_code` as the
/// last action before every `Terminator::Unreachable` this module emits, so a
/// host catching a Wasmtime trap can read the global back out for
/// diagnostics (memory is not rolled back on trap; stores before it persist).
pub mod trap_code {
    pub const NONE: u32 = 0;
    pub const BAD_FAT_REF: u32 = 1;
    pub const BAD_TYPE_ID: u32 = 2;
    pub const OUT_OF_BOUNDS: u32 = 3;
    pub const ROOT_STACK_OVERFLOW: u32 = 4;
    pub const HEAP_OOM: u32 = 5;
    pub const WORKLIST_OOM: u32 = 6;
    pub const ALLOCATOR_CORRUPTION: u32 = 7;
}

/// Configuration for the generated coregc runtime and heap.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CoreGcOptions {
    pub initial_pages: usize,
    pub maximum_pages: Option<usize>,
    /// Start of the growable managed heap. Must be at least large enough to
    /// hold the descriptor table plus the fixed root-stack and worklist
    /// reservations (validated in [`build`]).
    pub heap_base: u32,
    /// Fixed reservation for shadow-root frames. v1 does not grow this
    /// region; exceeding it is a deterministic `ROOT_STACK_OVERFLOW` trap.
    pub root_stack_bytes: u32,
    /// Fixed reservation for the mark worklist (8 bytes per pending pair).
    /// v1 does not grow this region; exceeding it is a deterministic
    /// `WORKLIST_OOM` trap.
    pub worklist_bytes: u32,
    /// Bytes allocated since the last collection before a checkpoint call
    /// triggers a collection. `0` forces a collection at every checkpoint
    /// (used by the forced-collection acceptance fixtures).
    pub collect_threshold_bytes: u32,
}

impl Default for CoreGcOptions {
    fn default() -> Self {
        Self {
            initial_pages: 2,
            maximum_pages: None,
            heap_base: 0x1_0000,
            root_stack_bytes: 4096,
            worklist_bytes: 8192,
            collect_threshold_bytes: 1 << 20,
        }
    }
}

/// Handles to every generated runtime function/global that `coregc_lower`
/// and `coregc_emit` need. No field here exposes a byte offset; callers only
/// ever invoke these as ordinary Waffle `Call`s.
#[derive(Clone, Copy, Debug)]
pub(crate) struct CoreGcRuntime {
    pub(crate) memory: portal_pc_waffle::Memory,
    pub(crate) trap_code: Global,
    /// `(type_id: i32, payload_bytes: i32) -> address: i32`
    pub(crate) alloc: Func,
    /// `(address: i32, byte_len: i32) -> ()`
    pub(crate) zero_bytes: Func,
    /// `(address: i32, type_id: i32) -> address: i32` (or traps)
    pub(crate) validate_ref: Func,
    /// `(address: i32, type_id: i32, index: i32) -> index: i32` (or traps)
    pub(crate) array_bounds: Func,
    /// `(frame: i32) -> ()` — may collect once the allocation threshold is
    /// reached; `frame` is `0` when the calling function pushed no shadow
    /// frame of its own (checked against the live root-stack head only when
    /// nonzero).
    pub(crate) checkpoint: Func,
    /// `() -> ()` — unconditional full collection, used by tests and an
    /// optional debug export.
    pub(crate) collect: Func,
    pub(crate) roots: ShadowRootFunctions,
}

/// Generate the complete coregc runtime described in
/// `docs/plan-coregc-atomic-collector-and-lowering.md` §3 into `module`,
/// embedding `descriptors` as the memory's initial data segment.
pub(crate) fn build(
    module: &mut Module<'static>,
    options: &CoreGcOptions,
    descriptors: &CoreGcDescriptorTable,
) -> Result<CoreGcRuntime, CoreGcError> {
    if options.initial_pages == 0 {
        return Err(CoreGcError {
            message: "coregc requires at least one initial memory page".to_owned(),
        });
    }
    let root_stack_start = align8(
        u32::try_from(descriptors.bytes.len()).map_err(|_| CoreGcError {
            message: "coregc descriptor table exceeds u32 bytes".to_owned(),
        })?,
    )
    .ok_or_else(overflow_error)?;
    let worklist_start = root_stack_start
        .checked_add(options.root_stack_bytes)
        .and_then(align8)
        .ok_or_else(overflow_error)?;
    let min_heap_base = worklist_start
        .checked_add(options.worklist_bytes)
        .ok_or_else(overflow_error)?;
    if options.heap_base < min_heap_base {
        return Err(CoreGcError {
            message: format!(
                "coregc heap_base {} is smaller than the required reservation of {} bytes \
                 (descriptors + root stack + worklist)",
                options.heap_base, min_heap_base
            ),
        });
    }
    if options.worklist_bytes < 8 || options.worklist_bytes % 8 != 0 {
        return Err(CoreGcError {
            message: "coregc worklist_bytes must be a nonzero multiple of 8".to_owned(),
        });
    }
    let initial_bytes = options
        .initial_pages
        .checked_mul(0x1_0000)
        .ok_or_else(overflow_error)?;
    if usize::try_from(options.heap_base)
        .ok()
        .is_none_or(|base| base >= initial_bytes)
    {
        return Err(CoreGcError {
            message: "coregc heap base must lie inside initial memory".to_owned(),
        });
    }

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

    let bump = global_i32(module, u64::from(options.heap_base));
    let block_list_head = global_i32(module, 0);
    let free_list_head = global_i32(module, 0);
    let root_head = global_i32(module, 0);
    let root_bump = global_i32(module, u64::from(root_stack_start));
    let worklist_count = global_i32(module, 0);
    let collecting = global_i32(module, 0);
    let bytes_since_collect = global_i32(module, 0);
    let trap_code = global_i32(module, 0);

    let validate_ref = add_validate_ref(module, memory, trap_code);
    let array_bounds = add_array_bounds(module, memory, trap_code, validate_ref);
    let zero_bytes = add_zero_bytes(module, memory);
    let alloc = add_alloc(
        module,
        memory,
        trap_code,
        bump,
        block_list_head,
        free_list_head,
        bytes_since_collect,
        options.heap_base,
    );
    let mark_ref = add_mark_ref(module, memory, trap_code, validate_ref, worklist_start,
        options.worklist_bytes, worklist_count);
    let roots = coregc_roots::add_shadow_roots(
        module,
        memory,
        options.heap_base,
        root_head,
        root_bump,
        trap_code,
    );
    let root_walk = coregc_roots::add_root_walk(module, memory, root_head, mark_ref);
    let collect = add_collect(
        module,
        memory,
        trap_code,
        collecting,
        bytes_since_collect,
        block_list_head,
        free_list_head,
        worklist_start,
        worklist_count,
        root_walk,
        mark_ref,
    );
    let checkpoint = add_checkpoint(
        module,
        trap_code,
        root_head,
        bytes_since_collect,
        options.collect_threshold_bytes,
        collect,
    );

    Ok(CoreGcRuntime {
        memory,
        trap_code,
        alloc,
        zero_bytes,
        validate_ref,
        array_bounds,
        checkpoint,
        collect,
        roots,
    })
}

fn overflow_error() -> CoreGcError {
    CoreGcError {
        message: "coregc reserved-region layout overflows u32".to_owned(),
    }
}

fn align8(value: u32) -> Option<u32> {
    value.checked_add(7).map(|v| v & !7)
}

fn global_i32(module: &mut Module<'static>, initial: u64) -> Global {
    module.globals.push(GlobalData {
        ty: Type::I32,
        value: Some(initial),
        mutable: true,
    })
}

fn i32_const(body: &mut FunctionBody, block: portal_pc_waffle::Block, value: u32) -> portal_pc_waffle::Value {
    body.add_op(block, Operator::I32Const { value }, &[], &[Type::I32])
}

fn set_trap(body: &mut FunctionBody, block: portal_pc_waffle::Block, trap_code: Global, code: u32) {
    let value = i32_const(body, block, code);
    body.add_op(
        block,
        Operator::GlobalSet {
            global_index: trap_code,
        },
        &[value],
        &[],
    );
}

fn load32(
    body: &mut FunctionBody,
    block: portal_pc_waffle::Block,
    memory: portal_pc_waffle::Memory,
    addr: portal_pc_waffle::Value,
    offset: u64,
) -> portal_pc_waffle::Value {
    body.add_op(
        block,
        Operator::I32Load {
            memory: MemoryArg {
                align: 2,
                offset,
                memory,
            },
        },
        &[addr],
        &[Type::I32],
    )
}

fn store32(
    body: &mut FunctionBody,
    block: portal_pc_waffle::Block,
    memory: portal_pc_waffle::Memory,
    addr: portal_pc_waffle::Value,
    offset: u64,
    value: portal_pc_waffle::Value,
) {
    body.add_op(
        block,
        Operator::I32Store {
            memory: MemoryArg {
                align: 2,
                offset,
                memory,
            },
        },
        &[addr, value],
        &[],
    );
}

fn header_of(
    body: &mut FunctionBody,
    block: portal_pc_waffle::Block,
    payload: portal_pc_waffle::Value,
) -> portal_pc_waffle::Value {
    let header_bytes = i32_const(body, block, COREGC_HEADER_BYTES);
    body.add_op(block, Operator::I32Sub, &[payload, header_bytes], &[Type::I32])
}

fn push_func(
    module: &mut Module<'static>,
    params: Vec<Type>,
    returns: Vec<Type>,
    name: &str,
) -> (Func, SigCtx) {
    let signature = module.signatures.push(SignatureData::Func {
        params,
        returns,
        shared: false,
    });
    let body = FunctionBody::new(module, signature);
    (
        module.funcs.push(FuncDecl::None(std::marker::PhantomData)),
        SigCtx {
            signature,
            body,
            name: name.to_owned(),
        },
    )
}

struct SigCtx {
    signature: portal_pc_waffle::Signature,
    body: FunctionBody,
    name: String,
}

fn finish_func(module: &mut Module<'static>, placeholder: Func, ctx: SigCtx) -> Func {
    module.funcs[placeholder] = FuncDecl::Body(ctx.signature, ctx.name, ctx.body);
    placeholder
}

/// `validate_ref(address, type_id) -> address` (or traps `BAD_FAT_REF`/`BAD_TYPE_ID`).
/// Accepts a live allocated object regardless of its mark bit: outside a
/// collection (the only place a mark bit is ever set) this never matters,
/// and `mark_ref` reuses this same acceptance rule for objects discovered
/// mid-collection that were already marked by an earlier worklist entry.
fn add_validate_ref(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
    trap_code: Global,
) -> Func {
    let (placeholder, mut ctx) = push_func(
        module,
        vec![Type::I32, Type::I32],
        vec![Type::I32],
        "__coregc_validate_ref",
    );
    let body = &mut ctx.body;
    let entry = body.entry;
    let address = body.blocks[entry].params[0].1;
    let type_id = body.blocks[entry].params[1].1;
    let null_case = body.add_block();
    let non_null = body.add_block();
    let null_ok = body.add_block();
    let live = body.add_block();
    let fail = body.add_block();
    let address_is_null = body.add_op(entry, Operator::I32Eqz, &[address], &[Type::I32]);
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: address_is_null,
            if_true: BlockTarget { block: null_case, args: vec![] },
            if_false: BlockTarget { block: non_null, args: vec![] },
        },
    );
    let type_is_null = body.add_op(null_case, Operator::I32Eqz, &[type_id], &[Type::I32]);
    body.set_terminator(
        null_case,
        Terminator::CondBr {
            cond: type_is_null,
            if_true: BlockTarget { block: null_ok, args: vec![] },
            if_false: BlockTarget { block: fail, args: vec![] },
        },
    );
    body.set_terminator(null_ok, Terminator::Return { values: vec![address] });

    let type_is_zero = body.add_op(non_null, Operator::I32Eqz, &[type_id], &[Type::I32]);
    let header_bytes = i32_const(body, non_null, COREGC_HEADER_BYTES);
    let underflows = body.add_op(non_null, Operator::I32LtU, &[address, header_bytes], &[Type::I32]);
    let invalid = body.add_op(non_null, Operator::I32Or, &[type_is_zero, underflows], &[Type::I32]);
    body.set_terminator(
        non_null,
        Terminator::CondBr {
            cond: invalid,
            if_true: BlockTarget { block: fail, args: vec![] },
            if_false: BlockTarget { block: live, args: vec![] },
        },
    );

    let header = header_of(body, live, address);
    let flags = load32(body, live, memory, header, 0);
    let allocated_bit = i32_const(body, live, FLAG_ALLOCATED);
    let has_allocated = body.add_op(live, Operator::I32And, &[flags, allocated_bit], &[Type::I32]);
    let not_allocated = body.add_op(live, Operator::I32Eqz, &[has_allocated], &[Type::I32]);
    let free_bit = i32_const(body, live, FLAG_FREE);
    let has_free = body.add_op(live, Operator::I32And, &[flags, free_bit], &[Type::I32]);
    // "stray bits" = any flag bit outside {ALLOCATED, MARK}; a corrupt/foreign
    // header will generally set bits this runtime never writes.
    let known_mask = i32_const(body, live, FLAG_ALLOCATED | FLAG_MARK);
    let all_ones = i32_const(body, live, u32::MAX);
    let inverse_mask = body.add_op(live, Operator::I32Xor, &[known_mask, all_ones], &[Type::I32]);
    let stray_bits = body.add_op(live, Operator::I32And, &[flags, inverse_mask], &[Type::I32]);
    let zero = i32_const(body, live, 0);
    let has_stray_bits = body.add_op(live, Operator::I32Ne, &[stray_bits, zero], &[Type::I32]);
    let bad_flags = body.add_op(live, Operator::I32Or, &[not_allocated, has_free], &[Type::I32]);
    let bad_flags = body.add_op(live, Operator::I32Or, &[bad_flags, has_stray_bits], &[Type::I32]);
    let actual_type = load32(body, live, memory, header, 4);
    let bad_type = body.add_op(live, Operator::I32Ne, &[actual_type, type_id], &[Type::I32]);
    let bad_flags_block = body.add_block();
    let bad_type_check = body.add_block();
    let bad_type_block = body.add_block();
    let success = body.add_block();
    body.set_terminator(
        live,
        Terminator::CondBr {
            cond: bad_flags,
            if_true: BlockTarget { block: bad_flags_block, args: vec![] },
            if_false: BlockTarget {
                block: bad_type_check,
                args: vec![bad_type],
            },
        },
    );
    set_trap(body, bad_flags_block, trap_code, trap_code::BAD_FAT_REF);
    body.set_terminator(bad_flags_block, Terminator::Unreachable);
    let bad_type_p = body.add_blockparam(bad_type_check, Type::I32);
    body.set_terminator(
        bad_type_check,
        Terminator::CondBr {
            cond: bad_type_p,
            if_true: BlockTarget { block: bad_type_block, args: vec![] },
            if_false: BlockTarget { block: success, args: vec![] },
        },
    );
    set_trap(body, bad_type_block, trap_code, trap_code::BAD_TYPE_ID);
    body.set_terminator(bad_type_block, Terminator::Unreachable);
    body.set_terminator(success, Terminator::Return { values: vec![address] });
    set_trap(body, fail, trap_code, trap_code::BAD_FAT_REF);
    body.set_terminator(fail, Terminator::Unreachable);

    finish_func(module, placeholder, ctx)
}

/// `array_bounds(address, type_id, index) -> index` (validated receiver, or traps).
fn add_array_bounds(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
    trap_code: Global,
    validate_ref: Func,
) -> Func {
    let (placeholder, mut ctx) = push_func(
        module,
        vec![Type::I32, Type::I32, Type::I32],
        vec![Type::I32],
        "__coregc_array_bounds",
    );
    let body = &mut ctx.body;
    let entry = body.entry;
    let address = body.blocks[entry].params[0].1;
    let type_id = body.blocks[entry].params[1].1;
    let index = body.blocks[entry].params[2].1;
    let checked = body.add_op(
        entry,
        Operator::Call { function_index: validate_ref },
        &[address, type_id],
        &[Type::I32],
    );
    let length = load32(body, entry, memory, checked, 0);
    let in_bounds = body.add_op(entry, Operator::I32LtU, &[index, length], &[Type::I32]);
    let success = body.add_block();
    let fail = body.add_block();
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: in_bounds,
            if_true: BlockTarget { block: success, args: vec![] },
            if_false: BlockTarget { block: fail, args: vec![] },
        },
    );
    body.set_terminator(success, Terminator::Return { values: vec![index] });
    set_trap(body, fail, trap_code, trap_code::OUT_OF_BOUNDS);
    body.set_terminator(fail, Terminator::Unreachable);
    finish_func(module, placeholder, ctx)
}

/// `zero_bytes(address, byte_len) -> ()`, used by `array.new_default`.
fn add_zero_bytes(module: &mut Module<'static>, memory: portal_pc_waffle::Memory) -> Func {
    let (placeholder, mut ctx) = push_func(module, vec![Type::I32, Type::I32], vec![], "__coregc_zero_bytes");
    let body = &mut ctx.body;
    let entry = body.entry;
    let addr = body.blocks[entry].params[0].1;
    let len = body.blocks[entry].params[1].1;
    let loop_block = body.add_block();
    let step = body.add_block();
    let done = body.add_block();
    let zero_start = i32_const(body, entry, 0);
    body.set_terminator(
        entry,
        Terminator::Br { target: BlockTarget { block: loop_block, args: vec![zero_start] } },
    );
    let i = body.add_blockparam(loop_block, Type::I32);
    let finished = body.add_op(loop_block, Operator::I32GeU, &[i, len], &[Type::I32]);
    body.set_terminator(
        loop_block,
        Terminator::CondBr {
            cond: finished,
            if_true: BlockTarget { block: done, args: vec![] },
            if_false: BlockTarget { block: step, args: vec![i] },
        },
    );
    let step_i = body.add_blockparam(step, Type::I32);
    let target = body.add_op(step, Operator::I32Add, &[addr, step_i], &[Type::I32]);
    let zero = i32_const(body, step, 0);
    body.add_op(
        step,
        Operator::I32Store8 {
            memory: MemoryArg { align: 0, offset: 0, memory },
        },
        &[target, zero],
        &[],
    );
    let one = i32_const(body, step, 1);
    let next_i = body.add_op(step, Operator::I32Add, &[step_i, one], &[Type::I32]);
    body.set_terminator(step, Terminator::Br { target: BlockTarget { block: loop_block, args: vec![next_i] } });
    body.set_terminator(done, Terminator::Return { values: vec![] });
    finish_func(module, placeholder, ctx)
}

/// `alloc(type_id, payload_bytes) -> address`: first-fit free-list reuse,
/// falling back to bump allocation (growing memory as needed). §3.4.
fn add_alloc(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
    trap_code: Global,
    bump: Global,
    block_list_head: Global,
    free_list_head: Global,
    bytes_since_collect: Global,
    heap_base: u32,
) -> Func {
    let (placeholder, mut ctx) = push_func(
        module,
        vec![Type::I32, Type::I32],
        vec![Type::I32],
        "__coregc_alloc",
    );
    let body = &mut ctx.body;
    let entry = body.entry;
    let type_id = body.blocks[entry].params[0].1;
    let payload_bytes = body.blocks[entry].params[1].1;

    let fail = body.add_block();
    let sizing = body.add_block();
    let type_is_zero = body.add_op(entry, Operator::I32Eqz, &[type_id], &[Type::I32]);
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: type_is_zero,
            if_true: BlockTarget { block: fail, args: vec![] },
            if_false: BlockTarget { block: sizing, args: vec![] },
        },
    );

    let header_bytes = i32_const(body, sizing, COREGC_HEADER_BYTES);
    let total = body.add_op(sizing, Operator::I32Add, &[payload_bytes, header_bytes], &[Type::I32]);
    let seven = i32_const(body, sizing, 7);
    let rounded = body.add_op(sizing, Operator::I32Add, &[total, seven], &[Type::I32]);
    let mask = i32_const(body, sizing, !7u32);
    let block_bytes = body.add_op(sizing, Operator::I32And, &[rounded, mask], &[Type::I32]);
    let bad_size = body.add_op(sizing, Operator::I32LtU, &[block_bytes, total], &[Type::I32]);
    let search_init = body.add_block();
    body.set_terminator(
        sizing,
        Terminator::CondBr {
            cond: bad_size,
            if_true: BlockTarget { block: fail, args: vec![] },
            if_false: BlockTarget { block: search_init, args: vec![block_bytes] },
        },
    );
    set_trap(body, fail, trap_code, trap_code::BAD_FAT_REF);
    body.set_terminator(fail, Terminator::Unreachable);

    // First-fit free-list search.
    let block_bytes_p = body.add_blockparam(search_init, Type::I32);
    let free_head = body.add_op(
        search_init,
        Operator::GlobalGet { global_index: free_list_head },
        &[],
        &[Type::I32],
    );
    let zero = i32_const(body, search_init, 0);
    let search_loop = body.add_block();
    body.set_terminator(
        search_init,
        Terminator::Br {
            target: BlockTarget { block: search_loop, args: vec![block_bytes_p, zero, free_head] },
        },
    );
    let sl_block_bytes = body.add_blockparam(search_loop, Type::I32);
    let sl_prev = body.add_blockparam(search_loop, Type::I32);
    let sl_cur = body.add_blockparam(search_loop, Type::I32);
    let cur_is_null = body.add_op(search_loop, Operator::I32Eqz, &[sl_cur], &[Type::I32]);
    let bump_path = body.add_block();
    let check_size = body.add_block();
    body.set_terminator(
        search_loop,
        Terminator::CondBr {
            cond: cur_is_null,
            if_true: BlockTarget { block: bump_path, args: vec![sl_block_bytes] },
            if_false: BlockTarget { block: check_size, args: vec![sl_block_bytes, sl_prev, sl_cur] },
        },
    );
    let cs_block_bytes = body.add_blockparam(check_size, Type::I32);
    let cs_prev = body.add_blockparam(check_size, Type::I32);
    let cs_cur = body.add_blockparam(check_size, Type::I32);
    let cur_header = header_of(body, check_size, cs_cur);
    let cur_block_bytes = load32(body, check_size, memory, cur_header, 20);
    let big_enough = body.add_op(check_size, Operator::I32GeU, &[cur_block_bytes, cs_block_bytes], &[Type::I32]);
    let reuse = body.add_block();
    let advance = body.add_block();
    body.set_terminator(
        check_size,
        Terminator::CondBr {
            cond: big_enough,
            if_true: BlockTarget { block: reuse, args: vec![cs_block_bytes, cs_prev, cs_cur] },
            if_false: BlockTarget { block: advance, args: vec![cs_block_bytes, cs_prev, cs_cur] },
        },
    );
    let adv_block_bytes = body.add_blockparam(advance, Type::I32);
    let adv_prev = body.add_blockparam(advance, Type::I32);
    let adv_cur = body.add_blockparam(advance, Type::I32);
    let _ = adv_prev;
    let adv_header = header_of(body, advance, adv_cur);
    let adv_next = load32(body, advance, memory, adv_header, 16);
    body.set_terminator(
        advance,
        Terminator::Br {
            target: BlockTarget { block: search_loop, args: vec![adv_block_bytes, adv_cur, adv_next] },
        },
    );

    let re_block_bytes = body.add_blockparam(reuse, Type::I32);
    let re_prev = body.add_blockparam(reuse, Type::I32);
    let re_cur = body.add_blockparam(reuse, Type::I32);
    let re_header = header_of(body, reuse, re_cur);
    let re_next_free = load32(body, reuse, memory, re_header, 16);
    let prev_is_zero = body.add_op(reuse, Operator::I32Eqz, &[re_prev], &[Type::I32]);
    let unlink_head = body.add_block();
    let unlink_mid = body.add_block();
    let unlinked = body.add_block();
    body.set_terminator(
        reuse,
        Terminator::CondBr {
            cond: prev_is_zero,
            if_true: BlockTarget { block: unlink_head, args: vec![] },
            if_false: BlockTarget { block: unlink_mid, args: vec![] },
        },
    );
    body.add_op(
        unlink_head,
        Operator::GlobalSet { global_index: free_list_head },
        &[re_next_free],
        &[],
    );
    body.set_terminator(unlink_head, Terminator::Br { target: BlockTarget { block: unlinked, args: vec![] } });
    let prev_header = header_of(body, unlink_mid, re_prev);
    store32(body, unlink_mid, memory, prev_header, 16, re_next_free);
    body.set_terminator(unlink_mid, Terminator::Br { target: BlockTarget { block: unlinked, args: vec![] } });

    let allocated_flags = i32_const(body, unlinked, FLAG_ALLOCATED);
    store32(body, unlinked, memory, re_header, 0, allocated_flags);
    store32(body, unlinked, memory, re_header, 4, type_id);
    store32(body, unlinked, memory, re_header, 8, payload_bytes);
    let finish = body.add_block();
    body.set_terminator(
        unlinked,
        Terminator::Br { target: BlockTarget { block: finish, args: vec![re_cur, re_block_bytes] } },
    );

    // Bump-allocation fallback (grows memory as needed).
    let bp_block_bytes = body.add_blockparam(bump_path, Type::I32);
    let bump_value = body.add_op(bump_path, Operator::GlobalGet { global_index: bump }, &[], &[Type::I32]);
    let next = body.add_op(bump_path, Operator::I32Add, &[bump_value, bp_block_bytes], &[Type::I32]);
    let wrapped = body.add_op(bump_path, Operator::I32LtU, &[next, bump_value], &[Type::I32]);
    let pages = body.add_op(bump_path, Operator::MemorySize { mem: memory }, &[], &[Type::I32]);
    let page_shift = i32_const(body, bump_path, 16);
    let limit = body.add_op(bump_path, Operator::I32Shl, &[pages, page_shift], &[Type::I32]);
    let needs_grow = body.add_op(bump_path, Operator::I32GtU, &[next, limit], &[Type::I32]);
    let invalid = body.add_op(bump_path, Operator::I32Or, &[wrapped, needs_grow], &[Type::I32]);
    let grow = body.add_block();
    let grow_check_ok = body.add_block();
    let commit = body.add_block();
    body.set_terminator(
        bump_path,
        Terminator::CondBr {
            cond: wrapped,
            if_true: BlockTarget { block: fail, args: vec![] },
            if_false: BlockTarget { block: grow_check_ok, args: vec![bp_block_bytes, bump_value, next, limit] },
        },
    );
    let _ = invalid;
    let gc_block_bytes = body.add_blockparam(grow_check_ok, Type::I32);
    let gc_bump_value = body.add_blockparam(grow_check_ok, Type::I32);
    let gc_next = body.add_blockparam(grow_check_ok, Type::I32);
    let gc_limit = body.add_blockparam(grow_check_ok, Type::I32);
    let gc_needs_grow = body.add_op(grow_check_ok, Operator::I32GtU, &[gc_next, gc_limit], &[Type::I32]);
    body.set_terminator(
        grow_check_ok,
        Terminator::CondBr {
            cond: gc_needs_grow,
            if_true: BlockTarget { block: grow, args: vec![gc_block_bytes, gc_bump_value, gc_next, gc_limit] },
            if_false: BlockTarget { block: commit, args: vec![gc_block_bytes, gc_bump_value, gc_next] },
        },
    );
    let g_block_bytes = body.add_blockparam(grow, Type::I32);
    let g_bump_value = body.add_blockparam(grow, Type::I32);
    let g_next = body.add_blockparam(grow, Type::I32);
    let g_limit = body.add_blockparam(grow, Type::I32);
    let deficit = body.add_op(grow, Operator::I32Sub, &[g_next, g_limit], &[Type::I32]);
    let page_mask = i32_const(body, grow, 0xffff);
    let rounded_deficit = body.add_op(grow, Operator::I32Add, &[deficit, page_mask], &[Type::I32]);
    let grow_pages = body.add_op(grow, Operator::I32ShrU, &[rounded_deficit, page_shift], &[Type::I32]);
    let previous_pages = body.add_op(grow, Operator::MemoryGrow { mem: memory }, &[grow_pages], &[Type::I32]);
    let grow_failed_value = i32_const(body, grow, u32::MAX);
    let grow_failed = body.add_op(grow, Operator::I32Eq, &[previous_pages, grow_failed_value], &[Type::I32]);
    let grow_fail_block = body.add_block();
    body.set_terminator(
        grow,
        Terminator::CondBr {
            cond: grow_failed,
            if_true: BlockTarget { block: grow_fail_block, args: vec![] },
            if_false: BlockTarget { block: commit, args: vec![g_block_bytes, g_bump_value, g_next] },
        },
    );
    set_trap(body, grow_fail_block, trap_code, trap_code::HEAP_OOM);
    body.set_terminator(grow_fail_block, Terminator::Unreachable);

    let c_block_bytes = body.add_blockparam(commit, Type::I32);
    let c_bump_value = body.add_blockparam(commit, Type::I32);
    let c_next = body.add_blockparam(commit, Type::I32);
    let flags = i32_const(body, commit, FLAG_ALLOCATED);
    store32(body, commit, memory, c_bump_value, 0, flags);
    store32(body, commit, memory, c_bump_value, 4, type_id);
    store32(body, commit, memory, c_bump_value, 8, payload_bytes);
    let old_block_list_head = body.add_op(
        commit,
        Operator::GlobalGet { global_index: block_list_head },
        &[],
        &[Type::I32],
    );
    store32(body, commit, memory, c_bump_value, 12, old_block_list_head);
    let zero = i32_const(body, commit, 0);
    store32(body, commit, memory, c_bump_value, 16, zero);
    store32(body, commit, memory, c_bump_value, 20, c_block_bytes);
    body.add_op(commit, Operator::GlobalSet { global_index: bump }, &[c_next], &[]);
    let header_bytes2 = i32_const(body, commit, COREGC_HEADER_BYTES);
    let payload = body.add_op(commit, Operator::I32Add, &[c_bump_value, header_bytes2], &[Type::I32]);
    body.add_op(
        commit,
        Operator::GlobalSet { global_index: block_list_head },
        &[payload],
        &[],
    );
    body.set_terminator(
        commit,
        Terminator::Br { target: BlockTarget { block: finish, args: vec![payload, c_block_bytes] } },
    );

    let f_address = body.add_blockparam(finish, Type::I32);
    let f_block_bytes = body.add_blockparam(finish, Type::I32);
    let prior = body.add_op(
        finish,
        Operator::GlobalGet { global_index: bytes_since_collect },
        &[],
        &[Type::I32],
    );
    let updated = body.add_op(finish, Operator::I32Add, &[prior, f_block_bytes], &[Type::I32]);
    body.add_op(
        finish,
        Operator::GlobalSet { global_index: bytes_since_collect },
        &[updated],
        &[],
    );
    body.set_terminator(finish, Terminator::Return { values: vec![f_address] });
    let _ = heap_base;

    finish_func(module, placeholder, ctx)
}

/// `mark_ref(address, type_id) -> ()`: validate, set the mark bit if clear,
/// and push newly-marked pairs onto the fixed-capacity worklist. §3.5.
fn add_mark_ref(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
    trap_code: Global,
    validate_ref: Func,
    worklist_start: u32,
    worklist_bytes: u32,
    worklist_count: Global,
) -> Func {
    let (placeholder, mut ctx) = push_func(module, vec![Type::I32, Type::I32], vec![], "__coregc_mark_ref");
    let body = &mut ctx.body;
    let entry = body.entry;
    let address = body.blocks[entry].params[0].1;
    let type_id = body.blocks[entry].params[1].1;
    let checked = body.add_op(
        entry,
        Operator::Call { function_index: validate_ref },
        &[address, type_id],
        &[Type::I32],
    );
    let is_null = body.add_op(entry, Operator::I32Eqz, &[checked], &[Type::I32]);
    let non_null = body.add_block();
    let done = body.add_block();
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: is_null,
            if_true: BlockTarget { block: done, args: vec![] },
            if_false: BlockTarget { block: non_null, args: vec![] },
        },
    );
    let header = header_of(body, non_null, checked);
    let flags = load32(body, non_null, memory, header, 0);
    let mark_bit = i32_const(body, non_null, FLAG_MARK);
    let already_marked = body.add_op(non_null, Operator::I32And, &[flags, mark_bit], &[Type::I32]);
    let to_mark = body.add_block();
    body.set_terminator(
        non_null,
        Terminator::CondBr {
            cond: already_marked,
            if_true: BlockTarget { block: done, args: vec![] },
            if_false: BlockTarget { block: to_mark, args: vec![] },
        },
    );
    let new_flags = body.add_op(to_mark, Operator::I32Or, &[flags, mark_bit], &[Type::I32]);
    store32(body, to_mark, memory, header, 0, new_flags);
    let count = body.add_op(
        to_mark,
        Operator::GlobalGet { global_index: worklist_count },
        &[],
        &[Type::I32],
    );
    let capacity = i32_const(body, to_mark, worklist_bytes / 8);
    let full = body.add_op(to_mark, Operator::I32GeU, &[count, capacity], &[Type::I32]);
    let overflow = body.add_block();
    let push = body.add_block();
    body.set_terminator(
        to_mark,
        Terminator::CondBr {
            cond: full,
            if_true: BlockTarget { block: overflow, args: vec![] },
            if_false: BlockTarget { block: push, args: vec![] },
        },
    );
    set_trap(body, overflow, trap_code, trap_code::WORKLIST_OOM);
    body.set_terminator(overflow, Terminator::Unreachable);
    let eight = i32_const(body, push, 8);
    let slot_offset = body.add_op(push, Operator::I32Mul, &[count, eight], &[Type::I32]);
    let base = i32_const(body, push, worklist_start);
    let slot_addr = body.add_op(push, Operator::I32Add, &[base, slot_offset], &[Type::I32]);
    store32(body, push, memory, slot_addr, 0, checked);
    store32(body, push, memory, slot_addr, 4, type_id);
    let one = i32_const(body, push, 1);
    let new_count = body.add_op(push, Operator::I32Add, &[count, one], &[Type::I32]);
    body.add_op(
        push,
        Operator::GlobalSet { global_index: worklist_count },
        &[new_count],
        &[],
    );
    body.set_terminator(push, Terminator::Br { target: BlockTarget { block: done, args: vec![] } });
    body.set_terminator(done, Terminator::Return { values: vec![] });
    finish_func(module, placeholder, ctx)
}

/// `collect() -> ()`: mark from every shadow root, drain the worklist by
/// interpreting the embedded descriptor table, then sweep the block list.
/// §3.6. Either finishes fully or traps; no partial sweep is observable.
#[allow(clippy::too_many_arguments)]
fn add_collect(
    module: &mut Module<'static>,
    memory: portal_pc_waffle::Memory,
    trap_code: Global,
    collecting: Global,
    bytes_since_collect: Global,
    block_list_head: Global,
    free_list_head: Global,
    worklist_start: u32,
    worklist_count: Global,
    root_walk: Func,
    mark_ref: Func,
) -> Func {
    let (placeholder, mut ctx) = push_func(module, vec![], vec![], "__coregc_collect");
    let body = &mut ctx.body;
    let entry = body.entry;
    let already = body.add_op(
        entry,
        Operator::GlobalGet { global_index: collecting },
        &[],
        &[Type::I32],
    );
    let reentrant = body.add_block();
    let start = body.add_block();
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: already,
            if_true: BlockTarget { block: reentrant, args: vec![] },
            if_false: BlockTarget { block: start, args: vec![] },
        },
    );
    set_trap(body, reentrant, trap_code, trap_code::ALLOCATOR_CORRUPTION);
    body.set_terminator(reentrant, Terminator::Unreachable);

    let one = i32_const(body, start, 1);
    body.add_op(start, Operator::GlobalSet { global_index: collecting }, &[one], &[]);
    body.add_op(start, Operator::Call { function_index: root_walk }, &[], &[]);
    let drain_loop = body.add_block();
    body.set_terminator(start, Terminator::Br { target: BlockTarget { block: drain_loop, args: vec![] } });

    // --- drain worklist ---
    let count = body.add_op(
        drain_loop,
        Operator::GlobalGet { global_index: worklist_count },
        &[],
        &[Type::I32],
    );
    let empty = body.add_op(drain_loop, Operator::I32Eqz, &[count], &[Type::I32]);
    let pop = body.add_block();
    let sweep_start = body.add_block();
    body.set_terminator(
        drain_loop,
        Terminator::CondBr {
            cond: empty,
            if_true: BlockTarget { block: sweep_start, args: vec![] },
            if_false: BlockTarget { block: pop, args: vec![count] },
        },
    );
    let pop_count = body.add_blockparam(pop, Type::I32);
    let one_p = i32_const(body, pop, 1);
    let new_count = body.add_op(pop, Operator::I32Sub, &[pop_count, one_p], &[Type::I32]);
    body.add_op(pop, Operator::GlobalSet { global_index: worklist_count }, &[new_count], &[]);
    let eight = i32_const(body, pop, 8);
    let slot_offset = body.add_op(pop, Operator::I32Mul, &[new_count, eight], &[Type::I32]);
    let base = i32_const(body, pop, worklist_start);
    let slot_addr = body.add_op(pop, Operator::I32Add, &[base, slot_offset], &[Type::I32]);
    let addr = load32(body, pop, memory, slot_addr, 0);
    let type_id = load32(body, pop, memory, slot_addr, 4);
    let find_row = body.add_block();
    let zero_index = i32_const(body, pop, 0);
    let zero_row = i32_const(body, pop, DESCRIPTOR_TABLE_HEADER_BYTES);
    body.set_terminator(
        pop,
        Terminator::Br { target: BlockTarget { block: find_row, args: vec![addr, type_id, zero_index, zero_row] } },
    );

    // --- find descriptor row for `type_id` ---
    let fr_addr = body.add_blockparam(find_row, Type::I32);
    let fr_type = body.add_blockparam(find_row, Type::I32);
    let fr_index = body.add_blockparam(find_row, Type::I32);
    let fr_row = body.add_blockparam(find_row, Type::I32);
    let descriptor_table_base = i32_const(body, find_row, 0);
    let descriptor_count = load32(body, find_row, memory, descriptor_table_base, 8);
    let exhausted = body.add_op(find_row, Operator::I32GeU, &[fr_index, descriptor_count], &[Type::I32]);
    let row_missing = body.add_block();
    let row_check = body.add_block();
    body.set_terminator(
        find_row,
        Terminator::CondBr {
            cond: exhausted,
            if_true: BlockTarget { block: row_missing, args: vec![] },
            if_false: BlockTarget { block: row_check, args: vec![fr_addr, fr_type, fr_index, fr_row] },
        },
    );
    set_trap(body, row_missing, trap_code, trap_code::BAD_TYPE_ID);
    body.set_terminator(row_missing, Terminator::Unreachable);

    let rc_addr = body.add_blockparam(row_check, Type::I32);
    let rc_type = body.add_blockparam(row_check, Type::I32);
    let rc_index = body.add_blockparam(row_check, Type::I32);
    let rc_row = body.add_blockparam(row_check, Type::I32);
    let row_type_id = load32(body, row_check, memory, rc_row, 0);
    let matches = body.add_op(row_check, Operator::I32Eq, &[row_type_id, rc_type], &[Type::I32]);
    let dispatch = body.add_block();
    let advance_row = body.add_block();
    body.set_terminator(
        row_check,
        Terminator::CondBr {
            cond: matches,
            if_true: BlockTarget { block: dispatch, args: vec![rc_addr, rc_row] },
            if_false: BlockTarget { block: advance_row, args: vec![rc_addr, rc_type, rc_index, rc_row] },
        },
    );
    let ar_addr = body.add_blockparam(advance_row, Type::I32);
    let ar_type = body.add_blockparam(advance_row, Type::I32);
    let ar_index = body.add_blockparam(advance_row, Type::I32);
    let ar_row = body.add_blockparam(advance_row, Type::I32);
    let row_slot_count = load32(body, advance_row, memory, ar_row, 16);
    let slot_bytes_c = i32_const(body, advance_row, DESCRIPTOR_SLOT_BYTES);
    let slots_size = body.add_op(advance_row, Operator::I32Mul, &[row_slot_count, slot_bytes_c], &[Type::I32]);
    let row_header_c = i32_const(body, advance_row, DESCRIPTOR_ROW_HEADER_BYTES);
    let row_size = body.add_op(advance_row, Operator::I32Add, &[row_header_c, slots_size], &[Type::I32]);
    let next_row = body.add_op(advance_row, Operator::I32Add, &[ar_row, row_size], &[Type::I32]);
    let one_r = i32_const(body, advance_row, 1);
    let next_index = body.add_op(advance_row, Operator::I32Add, &[ar_index, one_r], &[Type::I32]);
    body.set_terminator(
        advance_row,
        Terminator::Br { target: BlockTarget { block: find_row, args: vec![ar_addr, ar_type, next_index, next_row] } },
    );

    // --- dispatch by descriptor kind ---
    let d_addr = body.add_blockparam(dispatch, Type::I32);
    let d_row = body.add_blockparam(dispatch, Type::I32);
    let kind = load32(body, dispatch, memory, d_row, 4);
    let struct_kind = i32_const(body, dispatch, COREGC_DESCRIPTOR_KIND_STRUCT);
    let is_struct = body.add_op(dispatch, Operator::I32Eq, &[kind, struct_kind], &[Type::I32]);
    let array_kind = i32_const(body, dispatch, COREGC_DESCRIPTOR_KIND_ARRAY);
    let is_array = body.add_op(dispatch, Operator::I32Eq, &[kind, array_kind], &[Type::I32]);
    let is_known = body.add_op(dispatch, Operator::I32Or, &[is_struct, is_array], &[Type::I32]);
    let unknown_kind = body.add_block();
    let struct_scan_init = body.add_block();
    let array_scan_init = body.add_block();
    let known_check = body.add_block();
    body.set_terminator(
        dispatch,
        Terminator::CondBr {
            cond: is_known,
            if_true: BlockTarget { block: known_check, args: vec![d_addr, d_row, is_struct] },
            if_false: BlockTarget { block: unknown_kind, args: vec![] },
        },
    );
    set_trap(body, unknown_kind, trap_code, trap_code::ALLOCATOR_CORRUPTION);
    body.set_terminator(unknown_kind, Terminator::Unreachable);
    let kc_addr = body.add_blockparam(known_check, Type::I32);
    let kc_row = body.add_blockparam(known_check, Type::I32);
    let kc_is_struct = body.add_blockparam(known_check, Type::I32);
    body.set_terminator(
        known_check,
        Terminator::CondBr {
            cond: kc_is_struct,
            if_true: BlockTarget { block: struct_scan_init, args: vec![kc_addr, kc_row] },
            if_false: BlockTarget { block: array_scan_init, args: vec![kc_addr, kc_row] },
        },
    );

    // struct: iterate slot_count slots, marking every ManagedRef slot.
    let ss_addr = body.add_blockparam(struct_scan_init, Type::I32);
    let ss_row = body.add_blockparam(struct_scan_init, Type::I32);
    let slot_count = load32(body, struct_scan_init, memory, ss_row, 16);
    let row_header_len = i32_const(body, struct_scan_init, DESCRIPTOR_ROW_HEADER_BYTES);
    let slots_base = body.add_op(
        struct_scan_init,
        Operator::I32Add,
        &[ss_row, row_header_len],
        &[Type::I32],
    );
    let struct_slot_loop = body.add_block();
    let zero_idx = i32_const(body, struct_scan_init, 0);
    body.set_terminator(
        struct_scan_init,
        Terminator::Br {
            target: BlockTarget { block: struct_slot_loop, args: vec![ss_addr, slots_base, slot_count, zero_idx] },
        },
    );
    let sl_addr = body.add_blockparam(struct_slot_loop, Type::I32);
    let sl_slots_base = body.add_blockparam(struct_slot_loop, Type::I32);
    let sl_slot_count = body.add_blockparam(struct_slot_loop, Type::I32);
    let sl_idx = body.add_blockparam(struct_slot_loop, Type::I32);
    let sl_done = body.add_op(struct_slot_loop, Operator::I32GeU, &[sl_idx, sl_slot_count], &[Type::I32]);
    let struct_slot_body = body.add_block();
    body.set_terminator(
        struct_slot_loop,
        Terminator::CondBr {
            cond: sl_done,
            if_true: BlockTarget { block: drain_loop, args: vec![] },
            if_false: BlockTarget { block: struct_slot_body, args: vec![sl_addr, sl_slots_base, sl_slot_count, sl_idx] },
        },
    );
    let sb_addr = body.add_blockparam(struct_slot_body, Type::I32);
    let sb_slots_base = body.add_blockparam(struct_slot_body, Type::I32);
    let sb_slot_count = body.add_blockparam(struct_slot_body, Type::I32);
    let sb_idx = body.add_blockparam(struct_slot_body, Type::I32);
    let slot_bytes_c2 = i32_const(body, struct_slot_body, DESCRIPTOR_SLOT_BYTES);
    let slot_off = body.add_op(struct_slot_body, Operator::I32Mul, &[sb_idx, slot_bytes_c2], &[Type::I32]);
    let slot_addr = body.add_op(struct_slot_body, Operator::I32Add, &[sb_slots_base, slot_off], &[Type::I32]);
    let field_offset = load32(body, struct_slot_body, memory, slot_addr, 0);
    let storage_tag = load32(body, struct_slot_body, memory, slot_addr, 4);
    let managed_ref_tag = i32_const(body, struct_slot_body, STORAGE_TAG_MANAGED_REF);
    let is_ref = body.add_op(
        struct_slot_body,
        Operator::I32Eq,
        &[storage_tag, managed_ref_tag],
        &[Type::I32],
    );
    let mark_struct_field = body.add_block();
    let next_struct_slot = body.add_block();
    body.set_terminator(
        struct_slot_body,
        Terminator::CondBr {
            cond: is_ref,
            if_true: BlockTarget { block: mark_struct_field, args: vec![sb_addr, sb_slots_base, sb_slot_count, sb_idx, field_offset] },
            if_false: BlockTarget { block: next_struct_slot, args: vec![sb_addr, sb_slots_base, sb_slot_count, sb_idx] },
        },
    );
    let mf_addr = body.add_blockparam(mark_struct_field, Type::I32);
    let mf_slots_base = body.add_blockparam(mark_struct_field, Type::I32);
    let mf_slot_count = body.add_blockparam(mark_struct_field, Type::I32);
    let mf_idx = body.add_blockparam(mark_struct_field, Type::I32);
    let mf_offset = body.add_blockparam(mark_struct_field, Type::I32);
    let field_addr = body.add_op(mark_struct_field, Operator::I32Add, &[mf_addr, mf_offset], &[Type::I32]);
    let child_addr = load32(body, mark_struct_field, memory, field_addr, 0);
    let child_type = load32(body, mark_struct_field, memory, field_addr, 4);
    body.add_op(
        mark_struct_field,
        Operator::Call { function_index: mark_ref },
        &[child_addr, child_type],
        &[],
    );
    body.set_terminator(
        mark_struct_field,
        Terminator::Br { target: BlockTarget { block: next_struct_slot, args: vec![mf_addr, mf_slots_base, mf_slot_count, mf_idx] } },
    );
    let ns_addr = body.add_blockparam(next_struct_slot, Type::I32);
    let ns_slots_base = body.add_blockparam(next_struct_slot, Type::I32);
    let ns_slot_count = body.add_blockparam(next_struct_slot, Type::I32);
    let ns_idx = body.add_blockparam(next_struct_slot, Type::I32);
    let one_s = i32_const(body, next_struct_slot, 1);
    let next_idx = body.add_op(next_struct_slot, Operator::I32Add, &[ns_idx, one_s], &[Type::I32]);
    body.set_terminator(
        next_struct_slot,
        Terminator::Br { target: BlockTarget { block: struct_slot_loop, args: vec![ns_addr, ns_slots_base, ns_slot_count, next_idx] } },
    );

    // array: one element slot at `slots_base`; scan iff it is a ManagedRef.
    let as_addr = body.add_blockparam(array_scan_init, Type::I32);
    let as_row = body.add_blockparam(array_scan_init, Type::I32);
    let row_header_c2 = i32_const(body, array_scan_init, DESCRIPTOR_ROW_HEADER_BYTES);
    let elem_slot_addr = body.add_op(array_scan_init, Operator::I32Add, &[as_row, row_header_c2], &[Type::I32]);
    let elem_storage_tag = load32(body, array_scan_init, memory, elem_slot_addr, 4);
    let elem_managed_ref_tag = i32_const(body, array_scan_init, STORAGE_TAG_MANAGED_REF);
    let elem_is_ref = body.add_op(
        array_scan_init,
        Operator::I32Eq,
        &[elem_storage_tag, elem_managed_ref_tag],
        &[Type::I32],
    );
    let array_scan_body_init = body.add_block();
    body.set_terminator(
        array_scan_init,
        Terminator::CondBr {
            cond: elem_is_ref,
            if_true: BlockTarget { block: array_scan_body_init, args: vec![as_addr, as_row] },
            if_false: BlockTarget { block: drain_loop, args: vec![] },
        },
    );
    let ab_addr = body.add_blockparam(array_scan_body_init, Type::I32);
    let ab_row = body.add_blockparam(array_scan_body_init, Type::I32);
    let stride = load32(body, array_scan_body_init, memory, ab_row, 20);
    let length = load32(body, array_scan_body_init, memory, ab_addr, 0);
    let array_elem_loop = body.add_block();
    let zero_i = i32_const(body, array_scan_body_init, 0);
    body.set_terminator(
        array_scan_body_init,
        Terminator::Br { target: BlockTarget { block: array_elem_loop, args: vec![ab_addr, stride, length, zero_i] } },
    );
    let ael_addr = body.add_blockparam(array_elem_loop, Type::I32);
    let ael_stride = body.add_blockparam(array_elem_loop, Type::I32);
    let ael_length = body.add_blockparam(array_elem_loop, Type::I32);
    let ael_idx = body.add_blockparam(array_elem_loop, Type::I32);
    let ael_done = body.add_op(array_elem_loop, Operator::I32GeU, &[ael_idx, ael_length], &[Type::I32]);
    let array_elem_body = body.add_block();
    body.set_terminator(
        array_elem_loop,
        Terminator::CondBr {
            cond: ael_done,
            if_true: BlockTarget { block: drain_loop, args: vec![] },
            if_false: BlockTarget { block: array_elem_body, args: vec![ael_addr, ael_stride, ael_length, ael_idx] },
        },
    );
    let aeb_addr = body.add_blockparam(array_elem_body, Type::I32);
    let aeb_stride = body.add_blockparam(array_elem_body, Type::I32);
    let aeb_length = body.add_blockparam(array_elem_body, Type::I32);
    let aeb_idx = body.add_blockparam(array_elem_body, Type::I32);
    let elem_off = body.add_op(array_elem_body, Operator::I32Mul, &[aeb_idx, aeb_stride], &[Type::I32]);
    let four = i32_const(body, array_elem_body, 4);
    let elem_off = body.add_op(array_elem_body, Operator::I32Add, &[elem_off, four], &[Type::I32]);
    let elem_addr = body.add_op(array_elem_body, Operator::I32Add, &[aeb_addr, elem_off], &[Type::I32]);
    let elem_child_addr = load32(body, array_elem_body, memory, elem_addr, 0);
    let elem_child_type = load32(body, array_elem_body, memory, elem_addr, 4);
    body.add_op(
        array_elem_body,
        Operator::Call { function_index: mark_ref },
        &[elem_child_addr, elem_child_type],
        &[],
    );
    let one_a = i32_const(body, array_elem_body, 1);
    let next_aidx = body.add_op(array_elem_body, Operator::I32Add, &[aeb_idx, one_a], &[Type::I32]);
    body.set_terminator(
        array_elem_body,
        Terminator::Br { target: BlockTarget { block: array_elem_loop, args: vec![aeb_addr, aeb_stride, aeb_length, next_aidx] } },
    );

    // --- sweep ---
    let head = body.add_op(
        sweep_start,
        Operator::GlobalGet { global_index: block_list_head },
        &[],
        &[Type::I32],
    );
    let sweep_loop = body.add_block();
    body.set_terminator(sweep_start, Terminator::Br { target: BlockTarget { block: sweep_loop, args: vec![head] } });
    let sw_cur = body.add_blockparam(sweep_loop, Type::I32);
    let sw_done = body.add_op(sweep_loop, Operator::I32Eqz, &[sw_cur], &[Type::I32]);
    let sweep_finish = body.add_block();
    let sweep_body = body.add_block();
    body.set_terminator(
        sweep_loop,
        Terminator::CondBr {
            cond: sw_done,
            if_true: BlockTarget { block: sweep_finish, args: vec![] },
            if_false: BlockTarget { block: sweep_body, args: vec![sw_cur] },
        },
    );
    let sb2_cur = body.add_blockparam(sweep_body, Type::I32);
    let sw_header = header_of(body, sweep_body, sb2_cur);
    let sw_flags = load32(body, sweep_body, memory, sw_header, 0);
    let sw_next = load32(body, sweep_body, memory, sw_header, 12);
    let allocated_bit2 = i32_const(body, sweep_body, FLAG_ALLOCATED);
    let has_allocated = body.add_op(sweep_body, Operator::I32And, &[sw_flags, allocated_bit2], &[Type::I32]);
    let zero_c = i32_const(body, sweep_body, 0);
    let is_allocated_base = body.add_op(sweep_body, Operator::I32Ne, &[has_allocated, zero_c], &[Type::I32]);
    let free_bit2 = i32_const(body, sweep_body, FLAG_FREE);
    let is_free = body.add_op(sweep_body, Operator::I32Eq, &[sw_flags, free_bit2], &[Type::I32]);
    let known_mask2 = i32_const(body, sweep_body, FLAG_ALLOCATED | FLAG_MARK);
    let all_ones2 = i32_const(body, sweep_body, u32::MAX);
    let inverse_mask2 = body.add_op(sweep_body, Operator::I32Xor, &[known_mask2, all_ones2], &[Type::I32]);
    let stray2 = body.add_op(sweep_body, Operator::I32And, &[sw_flags, inverse_mask2], &[Type::I32]);
    let clean_allocated = body.add_op(sweep_body, Operator::I32Eqz, &[stray2], &[Type::I32]);
    let is_allocated = body.add_op(sweep_body, Operator::I32And, &[is_allocated_base, clean_allocated], &[Type::I32]);
    let recognized = body.add_op(sweep_body, Operator::I32Or, &[is_allocated, is_free], &[Type::I32]);
    let corrupt = body.add_block();
    let recognized_ok = body.add_block();
    body.set_terminator(
        sweep_body,
        Terminator::CondBr {
            cond: recognized,
            if_true: BlockTarget { block: recognized_ok, args: vec![sb2_cur, sw_header, sw_flags, sw_next, is_allocated] },
            if_false: BlockTarget { block: corrupt, args: vec![] },
        },
    );
    set_trap(body, corrupt, trap_code, trap_code::ALLOCATOR_CORRUPTION);
    body.set_terminator(corrupt, Terminator::Unreachable);

    let ro_cur = body.add_blockparam(recognized_ok, Type::I32);
    let ro_header = body.add_blockparam(recognized_ok, Type::I32);
    let ro_flags = body.add_blockparam(recognized_ok, Type::I32);
    let ro_next = body.add_blockparam(recognized_ok, Type::I32);
    let ro_is_allocated = body.add_blockparam(recognized_ok, Type::I32);
    let handle_allocated = body.add_block();
    body.set_terminator(
        recognized_ok,
        Terminator::CondBr {
            cond: ro_is_allocated,
            if_true: BlockTarget { block: handle_allocated, args: vec![ro_cur, ro_header, ro_flags, ro_next] },
            if_false: BlockTarget { block: sweep_loop, args: vec![ro_next] },
        },
    );
    let ha_cur = body.add_blockparam(handle_allocated, Type::I32);
    let ha_header = body.add_blockparam(handle_allocated, Type::I32);
    let ha_flags = body.add_blockparam(handle_allocated, Type::I32);
    let ha_next = body.add_blockparam(handle_allocated, Type::I32);
    let mark_bit2 = i32_const(body, handle_allocated, FLAG_MARK);
    let was_marked = body.add_op(handle_allocated, Operator::I32And, &[ha_flags, mark_bit2], &[Type::I32]);
    let retain = body.add_block();
    let reclaim = body.add_block();
    body.set_terminator(
        handle_allocated,
        Terminator::CondBr {
            cond: was_marked,
            if_true: BlockTarget { block: retain, args: vec![ha_header, ha_next] },
            if_false: BlockTarget { block: reclaim, args: vec![ha_cur, ha_header, ha_next] },
        },
    );
    let rt_header = body.add_blockparam(retain, Type::I32);
    let rt_next = body.add_blockparam(retain, Type::I32);
    let cleared = i32_const(body, retain, FLAG_ALLOCATED);
    store32(body, retain, memory, rt_header, 0, cleared);
    body.set_terminator(retain, Terminator::Br { target: BlockTarget { block: sweep_loop, args: vec![rt_next] } });

    let rc2_cur = body.add_blockparam(reclaim, Type::I32);
    let rc2_header = body.add_blockparam(reclaim, Type::I32);
    let rc2_next = body.add_blockparam(reclaim, Type::I32);
    let free_flag = i32_const(body, reclaim, FLAG_FREE);
    store32(body, reclaim, memory, rc2_header, 0, free_flag);
    let zero_type = i32_const(body, reclaim, 0);
    store32(body, reclaim, memory, rc2_header, 4, zero_type);
    let old_free_head = body.add_op(
        reclaim,
        Operator::GlobalGet { global_index: free_list_head },
        &[],
        &[Type::I32],
    );
    store32(body, reclaim, memory, rc2_header, 16, old_free_head);
    body.add_op(
        reclaim,
        Operator::GlobalSet { global_index: free_list_head },
        &[rc2_cur],
        &[],
    );
    body.set_terminator(reclaim, Terminator::Br { target: BlockTarget { block: sweep_loop, args: vec![rc2_next] } });

    let zero_flag = i32_const(body, sweep_finish, 0);
    body.add_op(sweep_finish, Operator::GlobalSet { global_index: collecting }, &[zero_flag], &[]);
    body.add_op(
        sweep_finish,
        Operator::GlobalSet { global_index: bytes_since_collect },
        &[zero_flag],
        &[],
    );
    body.set_terminator(sweep_finish, Terminator::Return { values: vec![] });

    finish_func(module, placeholder, ctx)
}

/// `checkpoint(frame) -> ()`: verifies `frame` (when nonzero) is the current
/// shadow-root stack head, then collects once the allocation threshold is
/// reached.
fn add_checkpoint(
    module: &mut Module<'static>,
    trap_code: Global,
    root_head: Global,
    bytes_since_collect: Global,
    threshold: u32,
    collect: Func,
) -> Func {
    let (placeholder, mut ctx) = push_func(module, vec![Type::I32], vec![], "__coregc_checkpoint");
    let body = &mut ctx.body;
    let entry = body.entry;
    let frame = body.blocks[entry].params[0].1;
    let zero_frame = i32_const(body, entry, 0);
    let has_frame = body.add_op(entry, Operator::I32Ne, &[frame, zero_frame], &[Type::I32]);
    let verify = body.add_block();
    let check_threshold = body.add_block();
    body.set_terminator(
        entry,
        Terminator::CondBr {
            cond: has_frame,
            if_true: BlockTarget { block: verify, args: vec![] },
            if_false: BlockTarget { block: check_threshold, args: vec![] },
        },
    );
    let head = body.add_op(verify, Operator::GlobalGet { global_index: root_head }, &[], &[Type::I32]);
    let matches = body.add_op(verify, Operator::I32Eq, &[frame, head], &[Type::I32]);
    let mismatch = body.add_block();
    body.set_terminator(
        verify,
        Terminator::CondBr {
            cond: matches,
            if_true: BlockTarget { block: check_threshold, args: vec![] },
            if_false: BlockTarget { block: mismatch, args: vec![] },
        },
    );
    set_trap(body, mismatch, trap_code, trap_code::ALLOCATOR_CORRUPTION);
    body.set_terminator(mismatch, Terminator::Unreachable);

    let bytes = body.add_op(
        check_threshold,
        Operator::GlobalGet { global_index: bytes_since_collect },
        &[],
        &[Type::I32],
    );
    let threshold_c = i32_const(body, check_threshold, threshold);
    let due = body.add_op(check_threshold, Operator::I32GeU, &[bytes, threshold_c], &[Type::I32]);
    let do_collect = body.add_block();
    let done = body.add_block();
    body.set_terminator(
        check_threshold,
        Terminator::CondBr {
            cond: due,
            if_true: BlockTarget { block: do_collect, args: vec![] },
            if_false: BlockTarget { block: done, args: vec![] },
        },
    );
    body.add_op(do_collect, Operator::Call { function_index: collect }, &[], &[]);
    body.set_terminator(do_collect, Terminator::Br { target: BlockTarget { block: done, args: vec![] } });
    body.set_terminator(done, Terminator::Return { values: vec![] });
    finish_func(module, placeholder, ctx)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::coregc::CoreGcInventory;
    use portal_pc_waffle::{
        Export, ExportKind, HeapType, SignatureData, StorageType, WithMutablility, WithNullable,
    };
    use wasmtime::{Engine, Instance, Memory as WasmtimeMemory, Module as WasmtimeModule, Store};

    fn field(value: StorageType) -> WithMutablility<StorageType> {
        WithMutablility {
            mutable: true,
            value,
        }
    }

    /// A source module declaring:
    /// - `Leaf { value: i32 }` (pointer-free scalar struct)
    /// - `Node { value: i32, next: ref null Node }` (self-referential, for
    ///   cycle/graph retention tests)
    /// - `NodeArray` = array of `ref null Node` (reference array, for element
    ///   scanning tests)
    fn descriptor_source() -> Module<'static> {
        let mut module = Module::empty();
        // Forward-declare the recursive `Node` signature slot so the `next`
        // field below can reference it by `Signature` id.
        let node = module.signatures.push(SignatureData::Struct {
            fields: vec![],
            shared: false,
        });
        module.signatures[node] = SignatureData::Struct {
            fields: vec![
                field(StorageType::Val(Type::I32)),
                field(StorageType::Val(Type::Heap(WithNullable {
                    nullable: true,
                    value: HeapType::Sig { sig_index: node },
                }))),
            ],
            shared: false,
        };
        module.signatures.push(SignatureData::Array {
            ty: field(StorageType::Val(Type::Heap(WithNullable {
                nullable: true,
                value: HeapType::Sig { sig_index: node },
            }))),
            shared: false,
        });
        module
    }

    struct TestRig {
        instance: Instance,
        store: Store<()>,
        memory: WasmtimeMemory,
        node_type_id: i32,
        node_field_next_offset: i32,
        node_payload_bytes: i32,
        array_type_id: i32,
    }

    fn build_rig(options: CoreGcOptions) -> TestRig {
        let source = descriptor_source();
        let inventory = CoreGcInventory::build(&source).expect("inventory");
        let descriptors = CoreGcDescriptorTable::build(&inventory).expect("descriptors");
        let node_signature = source
            .signatures
            .entries()
            .find_map(|(sig, data)| match data {
                SignatureData::Struct { fields, .. } if fields.len() == 2 => Some(sig),
                _ => None,
            })
            .expect("node signature");
        let array_signature = source
            .signatures
            .entries()
            .find_map(|(sig, data)| match data {
                SignatureData::Array { .. } => Some(sig),
                _ => None,
            })
            .expect("array signature");
        let node_type_id = inventory.id_for(node_signature).expect("node id").get() as i32;
        let array_type_id = inventory.id_for(array_signature).expect("array id").get() as i32;
        let node_layout = descriptors
            .layouts
            .iter()
            .find(|layout| layout.id == inventory.id_for(node_signature).unwrap())
            .expect("node layout");
        let node_field_next_offset = node_layout.slots[1].offset as i32;
        let node_payload_bytes = node_layout.fixed_payload_bytes.expect("node is a struct") as i32;

        let mut module = Module::empty();
        let runtime = build(&mut module, &options, &descriptors).expect("runtime build");
        for (name, kind) in [
            ("alloc", ExportKind::Func(runtime.alloc)),
            ("zero_bytes", ExportKind::Func(runtime.zero_bytes)),
            ("validate_ref", ExportKind::Func(runtime.validate_ref)),
            ("array_bounds", ExportKind::Func(runtime.array_bounds)),
            ("checkpoint", ExportKind::Func(runtime.checkpoint)),
            ("collect", ExportKind::Func(runtime.collect)),
            ("push_frame", ExportKind::Func(runtime.roots.push)),
            ("pop_frame", ExportKind::Func(runtime.roots.pop)),
            ("root_store", ExportKind::Func(runtime.roots.store)),
            ("root_clear", ExportKind::Func(runtime.roots.clear)),
            ("memory", ExportKind::Memory(runtime.memory)),
        ] {
            module.exports.push(Export {
                name: name.to_owned(),
                kind,
            });
        }
        for (_, decl) in module.funcs.entries() {
            if let FuncDecl::Body(_, _, body) = decl {
                body.validate().expect("generated runtime function validates");
            }
        }
        let bytes = portal_pc_waffle::to_wasm_bytes(&module).expect("core wasm encodes");
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .expect("artifact validates with default core features");
        let engine = Engine::default();
        let wasmtime_module = WasmtimeModule::new(&engine, bytes).expect("engine compiles module");
        let mut store = Store::new(&engine, ());
        let instance =
            Instance::new(&mut store, &wasmtime_module, &[]).expect("module instantiates");
        let memory = instance
            .get_memory(&mut store, "memory")
            .expect("memory export");
        TestRig {
            instance,
            store,
            memory,
            node_type_id,
            node_field_next_offset,
            node_payload_bytes,
            array_type_id,
        }
    }

    impl TestRig {
        fn alloc(&mut self, type_id: i32, payload_bytes: i32) -> i32 {
            self.instance
                .get_typed_func::<(i32, i32), i32>(&mut self.store, "alloc")
                .unwrap()
                .call(&mut self.store, (type_id, payload_bytes))
                .expect("allocation")
        }
        fn validate_ok(&mut self, addr: i32, type_id: i32) -> bool {
            self.instance
                .get_typed_func::<(i32, i32), i32>(&mut self.store, "validate_ref")
                .unwrap()
                .call(&mut self.store, (addr, type_id))
                .is_ok()
        }
        fn collect(&mut self) {
            self.instance
                .get_typed_func::<(), ()>(&mut self.store, "collect")
                .unwrap()
                .call(&mut self.store, ())
                .expect("collection");
        }
        fn checkpoint(&mut self, frame: i32) -> Result<(), wasmtime::Error> {
            self.instance
                .get_typed_func::<i32, ()>(&mut self.store, "checkpoint")
                .unwrap()
                .call(&mut self.store, frame)
        }
        fn push_frame(&mut self, slots: i32) -> i32 {
            self.instance
                .get_typed_func::<i32, i32>(&mut self.store, "push_frame")
                .unwrap()
                .call(&mut self.store, slots)
                .expect("push frame")
        }
        fn pop_frame(&mut self, frame: i32) -> Result<(), wasmtime::Error> {
            self.instance
                .get_typed_func::<i32, ()>(&mut self.store, "pop_frame")
                .unwrap()
                .call(&mut self.store, frame)
        }
        fn root_store(&mut self, frame: i32, slot: i32, addr: i32, type_id: i32) {
            self.instance
                .get_typed_func::<(i32, i32, i32, i32), ()>(&mut self.store, "root_store")
                .unwrap()
                .call(&mut self.store, (frame, slot, addr, type_id))
                .expect("root store");
        }
        fn write_pair(&mut self, addr: i32, offset: i32, value_addr: i32, value_type: i32) {
            let at = (addr + offset) as usize;
            self.memory
                .write(&mut self.store, at, &value_addr.to_le_bytes())
                .unwrap();
            self.memory
                .write(&mut self.store, at + 4, &value_type.to_le_bytes())
                .unwrap();
        }
        fn write_i32(&mut self, addr: i32, offset: i32, value: i32) {
            self.memory
                .write(&mut self.store, (addr + offset) as usize, &value.to_le_bytes())
                .unwrap();
        }
    }

    #[test]
    fn alloc_bumps_then_reuses_a_freed_block_via_first_fit() {
        let mut rig = build_rig(CoreGcOptions {
            collect_threshold_bytes: u32::MAX,
            ..CoreGcOptions::default()
        });
        let type_id = rig.node_type_id;
        let first = rig.alloc(type_id, 8);
        let second = rig.alloc(type_id, 8);
        assert_ne!(first, second, "bump allocation never reuses live memory");
        // Nothing is rooted: a forced collection reclaims both.
        rig.collect();
        assert!(!rig.validate_ok(first, type_id));
        assert!(!rig.validate_ok(second, type_id));
        let reused = rig.alloc(type_id, 8);
        assert!(
            reused == first || reused == second,
            "first-fit reuse should return a previously freed block, got {reused}"
        );
    }

    #[test]
    fn collect_retains_a_rooted_two_cycle_and_clears_mark_bits() {
        let mut rig = build_rig(CoreGcOptions::default());
        let type_id = rig.node_type_id;
        let next_offset = rig.node_field_next_offset;
        let payload_bytes = rig.node_payload_bytes;
        let a = rig.alloc(type_id, payload_bytes);
        let b = rig.alloc(type_id, payload_bytes);
        rig.write_i32(a, 0, 1);
        rig.write_i32(b, 0, 2);
        rig.write_pair(a, next_offset, b, type_id);
        rig.write_pair(b, next_offset, a, type_id);
        let frame = rig.push_frame(1);
        rig.root_store(frame, 0, a, type_id);
        rig.collect();
        assert!(rig.validate_ok(a, type_id), "rooted node a survives");
        assert!(rig.validate_ok(b, type_id), "cycle-reachable node b survives");
        // Mark bits must be cleared after a successful sweep so the next
        // collection starts from a clean slate.
        let header_a = (a - COREGC_HEADER_BYTES as i32) as usize;
        let mut flags = [0u8; 4];
        rig.memory.read(&rig.store, header_a, &mut flags).unwrap();
        assert_eq!(u32::from_le_bytes(flags), FLAG_ALLOCATED);
        rig.pop_frame(frame).expect("pop frame");
    }

    #[test]
    fn collect_reclaims_unreachable_object() {
        let mut rig = build_rig(CoreGcOptions::default());
        let type_id = rig.node_type_id;
        let orphan = rig.alloc(type_id, 8);
        rig.collect();
        assert!(!rig.validate_ok(orphan, type_id));
    }

    #[test]
    fn collect_scans_reference_array_elements() {
        let mut rig = build_rig(CoreGcOptions::default());
        let node_type = rig.node_type_id;
        let array_type = rig.array_type_id;
        let child = rig.alloc(node_type, rig.node_payload_bytes);
        rig.write_i32(child, 0, 42);
        // Array payload: length word, then two 8-byte (addr,type) elements.
        let array = rig.alloc(array_type, 20);
        rig.write_i32(array, 0, 2);
        rig.write_pair(array, 4, child, node_type);
        rig.write_pair(array, 12, 0, 0);
        let frame = rig.push_frame(1);
        rig.root_store(frame, 0, array, array_type);
        rig.collect();
        assert!(rig.validate_ok(array, array_type));
        assert!(rig.validate_ok(child, node_type), "array element keeps child alive");
        rig.pop_frame(frame).expect("pop frame");
    }

    #[test]
    fn array_bounds_traps_out_of_range_index() {
        let mut rig = build_rig(CoreGcOptions::default());
        let array_type = rig.array_type_id;
        let array = rig.alloc(array_type, 12);
        rig.write_i32(array, 0, 1);
        let bounds = rig
            .instance
            .get_typed_func::<(i32, i32, i32), i32>(&mut rig.store, "array_bounds")
            .unwrap();
        assert_eq!(bounds.call(&mut rig.store, (array, array_type, 0)).unwrap(), 0);
        assert!(bounds.call(&mut rig.store, (array, array_type, 1)).is_err());
    }

    #[test]
    fn checkpoint_forces_collection_at_zero_threshold() {
        let mut rig = build_rig(CoreGcOptions {
            collect_threshold_bytes: 0,
            ..CoreGcOptions::default()
        });
        let type_id = rig.node_type_id;
        let orphan = rig.alloc(type_id, 8);
        rig.checkpoint(0).expect("checkpoint collects");
        assert!(!rig.validate_ok(orphan, type_id));
    }

    #[test]
    fn checkpoint_rejects_a_frame_that_is_not_the_current_root_head() {
        let mut rig = build_rig(CoreGcOptions {
            collect_threshold_bytes: 0,
            ..CoreGcOptions::default()
        });
        let frame = rig.push_frame(1);
        // A stale/forged frame value must not silently collect.
        assert!(rig.checkpoint(frame + 8).is_err());
    }

    #[test]
    fn worklist_oom_traps_deterministically_without_partial_sweep() {
        let mut rig = build_rig(CoreGcOptions {
            worklist_bytes: 8, // capacity for exactly one pending pair
            ..CoreGcOptions::default()
        });
        let type_id = rig.node_type_id;
        let a = rig.alloc(type_id, 8);
        let b = rig.alloc(type_id, 8);
        let frame = rig.push_frame(2);
        rig.root_store(frame, 0, a, type_id);
        rig.root_store(frame, 1, b, type_id);
        // Root-walk marks both `a` and `b` directly; only one worklist slot
        // exists, so the second root push must overflow deterministically.
        assert!(rig
            .instance
            .get_typed_func::<(), ()>(&mut rig.store, "collect")
            .unwrap()
            .call(&mut rig.store, ())
            .is_err());
    }

    #[test]
    fn push_pop_store_clear_round_trip_in_lifo_order() {
        let mut rig = build_rig(CoreGcOptions::default());
        let outer = rig.push_frame(1);
        rig.root_store(outer, 0, 123, rig.node_type_id);
        let inner = rig.push_frame(1);
        rig.root_store(inner, 0, 456, rig.node_type_id);
        rig.pop_frame(inner).expect("pop inner");
        rig.pop_frame(outer).expect("pop outer");
        assert!(rig.pop_frame(outer).is_err(), "double pop traps");
    }
}
