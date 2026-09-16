# Plan: coregc — a pure-core-Wasm backend for jsaw's typed WasmGC IR

**Status:** v1 **implemented and activated** (stages 1-6 complete: legacy
removal, generated runtime with free-list mark/sweep, general struct/array/
branch lowering with liveness-derived shadow-frame checkpoints, direct calls,
typed function references via `call_indirect`, and the public `emit_coregc`
orchestrator with its acceptance suite including a native-WasmGC-vs-coregc
differential test). §13 details the remaining v2 roadmap toward the dogfood
goal and §14 the cross-testing strategy.

Supersedes and absorbs the prior `docs/plan-wasmgc-linear-memory-fallback.md`
design (its research/prior-art sections remain cited from here) and the prior
`docs/plan-coregc-atomic-collector-and-lowering.md` "atomic activation" draft.
This revision exists because the first implementation attempt against the
prior draft failed for a specific, diagnosable reason (see §0) — this plan
fixed that reason structurally, not just by re-describing the same seam.

**Implementation rule:** the *public* activation (`emit_coregc` returning a
runnable artifact for real source programs) is one all-or-nothing gate — see
§9. Internal implementation work happens in the ordered stages of §8, and
**each stage's data layout and ABI must remain valid for every later stage**;
a later stage may only add to a stage's output, never contradict or replace
its committed invariants. This is the structural fix described in §0.

## 0. Why the previous attempt failed, and what changes here

The prior draft asked for one atomic commit implementing five requirements
(descriptor scanning, iterative mark worklist, sweep-with-retention, free-list
reuse, and liveness-derived root frames/checkpoints) on top of scaffolding
that was built in three *disjoint* proof phases:

- Phase 1 (`coregc_array.rs`, `coregc_lower.rs`) lowered exactly one
  hand-shaped function each (one struct access, one array access) by pattern
  matching the whole function body, not by a general transform.
- Phase 2 built an "empty-root" collector that resets the entire heap.
- Phase 3 built a root-frame ABI and a "marks roots but never sweeps"
  collector, exported side-by-side with Phase 2's collector because they were
  mutually incompatible: Phase 2's header-clearing reclaim strategy and Phase
  3's mark-bit strategy could not be combined without deciding sweep
  semantics first, which nobody had specified.

When implementation tried to add sweep on top of Phase 3, it broke Phase 3's
own root-survival test, because the header/allocation-list contract sweep
needs was never pinned down — it was reinvented ad hoc at the point sweep was
added. That is the actual failure: **the runtime data contract was decided
incrementally, once per phase, instead of once for the whole collector.**
Retrofitting requirements onto that scaffolding is not "atomic work," it is
"a rewrite disguised as an addition," which is exactly why the previous
attempt correctly refused to proceed silently.

This plan fixes the structural cause:

1. **One runtime data contract, decided now, in full** (§3): allocation
   header, free list, worklist, descriptor table, and root-frame layout are
   specified completely in this document before any code is written. No
   later stage is allowed to change a byte offset or field meaning that an
   earlier stage already committed.
2. **The existing legacy scaffolding is deleted, not layered on.** §7 lists
   the exact files removed. A clean module tree (§4) replaces them so the new
   contract has nowhere to silently drift back to the old one.
3. **Calls are in scope from the start**, not deferred. The prior draft
   treated direct calls as an optional "if ABI flattening is easy" add-on.
   jsaw's real compiled output (the dogfood target, `docs/plan-dogfood-self-hosting-java.md`)
   calls functions constantly; a collector that cannot checkpoint across a
   call is not a usable backend for that output. §6.4 folds calls into the
   same checkpoint/liveness machinery as allocation, uniformly, so there is
   no separate "calls are special" seam to get wrong later.
4. **Stages leave room for their successors instead of contradicting them**
   (§8). Each stage is real, tested, and permanently valid; later stages only
   add capabilities the runtime/ABI already has slots for. Nothing in stage N
   requires deleting or reinterpreting what stage N-1 committed.

## 1. Scope: what "coregc" is, and the honest current limit

coregc is a second backend for the same `portal_pc_waffle::Module` that the
native WasmGC backend already consumes. It emits **pure core Wasm** — no GC
proposal, no reference types beyond ordinary `funcref` tables, no typed
function references, no `anyref`/`i31ref`. Every managed value is a checked
`(address: i32, actual_type_id: i32)` pair into a compiler-generated,
non-moving mark-and-sweep heap that lives entirely in linear memory and is
collected by generated Wasm code — no host GC import.

**v1 target surface** (what this plan's implementation activates):

- concrete `struct`/`array` WasmGC types with scalar and concrete
  managed-reference (`ref $T` / `ref null $T`) fields/elements;
- scalar core arithmetic/comparison/conversion instructions;
- structured control flow: `Br`, `CondBr`, `Return`, with scalar and/or
  fat-reference block parameters;
- **direct calls** (`Operator::Call`) between functions in the same module,
  with fully flattened, checkpoint-safe ABI, including managed
  parameters/returns;
- **typed function references** (`Operator::RefFunc`, `Operator::CallRef`,
  and `Operator::CallIndirect` against a source-declared table) for concrete
  (non-`anyref`) call signatures — full design in §6.7, revised from the
  originally-drafted heap-allocated "function object" design after checking
  how `conv.rs` actually uses these operators (see §6.7's rationale);
- `RefNull`, `RefIsNull` for the accepted concrete reference types, including
  the concrete function-reference type.

**Explicitly out of v1, with a named fail-closed diagnostic, and a committed
non-conflicting design so v2 can add them without breaking v1**:

- `anyref`/`eqref`/`i31ref`/dynamic JS value representation (jsaw's `Repr`
  closure) — needed for full dogfood compilation of jsaw's own compiler
  output, tracked as a distinct, later plan once v1 is real and tested. This
  is why v1 function-reference support, while fully implemented, does not by
  itself make jsaw's actual emitted closures lower: `conv.rs` closures carry
  `anyref`-typed captured-environment/property-trie fields (confirmed by
  reading its closure-construction code, §6.7) that need the deferred `Repr`
  work regardless of call-mechanism support;
- `externref`, exceptions, threads/shared memory, `memory64`, SIMD-managed
  layouts, tail calls (including `ReturnCallRef`/`ReturnCall`/
  `ReturnCallIndirect`, which `conv.rs` also emits for proven-tail JS calls —
  rejected by name in v1's preflight, not silently miscompiled).

The dogfood goal (compiling jsaw's own compiler with coregc) needs the
deferred `Repr`/`anyref` items too. This plan does not claim v1 reaches that
goal by itself; it claims v1 is the correct, non-throwaway foundation for it
— see §11.

## 2. Prior art and design rationale (inherited, condensed)

Full citations and comparisons live in
`plan-wasmgc-linear-memory-fallback.md` §2 and its source notes; the
conclusions that drive this plan's decisions are:

- A core Wasm module cannot portably scan its own execution stack, so roots
  must be **compiler-maintained exact shadow frames**, not conservative stack
  scanning and not "collect only when the host stack is empty"
  (root-marking proposal discussion; V8's WasmGC porting notes;
  AssemblyScript's documented shadow-stack runtime).
- Conservative/raw-pointer scanning (Emgc) is rejected: it cannot distinguish
  a JS number from an address and cannot support the exact-type invariant
  this backend requires for casts and scanning.
- A non-moving collector is the correct first target: it avoids rewriting
  every root/field on every collection and keeps object identity address
  identity, at the cost of eventual fragmentation (deferred to a Phase 6
  performance pass, not blocking correctness).
- Fat references — `(address, actual_type_id)`, not a bare address — are
  required so a stale/forged/freed pointer is a deterministic trap rather
  than an arbitrary linear-memory access, and so casts/scans have the real
  dynamic type without a global address-to-type map.

## 3. The runtime data contract (frozen for all stages)

This section is normative. Every stage in §8 builds against exactly this
contract; none of it is renegotiated later.

### 3.1 Fat reference representation

```text
GcRef = (address: i32, actual_type_id: i32)
null  = (0, 0)          -- the only valid null encoding; a partial-null
                             pair (one zero, one nonzero) is always a trap
```

`address` is a **payload** address (never a header address). `actual_type_id`
is a dense nonzero `CoreGcTypeId` (§3.3), always the concrete runtime type,
never rewritten by a cast (a cast checks a subtype relation and traps or
passes the pair through unchanged — v1 has no subtyping between concrete
types, so casts are out of scope until v2's `anyref`/subtype work; this
reference representation already supports it without a format change).

### 3.2 Allocation header (24 bytes, unchanged from the existing scaffolding)

```text
offset  field            meaning
0       flags            bit0 ALLOCATED, bit1 MARK, bit2 FREE; 0 = corrupt/dead
4       type_id          concrete CoreGcTypeId; 0 while FREE
8       payload_bytes    requested payload size (not including header)
12      next_block       intrusive "every block ever carved" list (payload addr, 0 = end)
16      next_free        intrusive free-list link (payload addr, valid only while FREE)
20      block_bytes      header (24) + aligned payload capacity of this block
24      payload begins
```

Two independent intrusive singly-linked lists share this header:

- **block list** (`next_block`, global `block_list_head`): every block ever
  carved out of the bump area, allocated or free. Sweep walks this list
  exactly once per collection. A block is added to this list exactly once,
  when it is first bump-allocated; reuse from the free list never re-adds it.
- **free list** (`next_free`, global `free_list_head`): only blocks currently
  FREE. Allocation's first-fit search walks only this list.

`block_bytes` is required for first-fit sizing on reuse and is filled in at
first creation; it does not change across free/reuse cycles in v1 (no
splitting/coalescing — see §3.4). This is the field the original 24-byte
header reserved as "reserved" for exactly this purpose; no header resize is
needed.

### 3.3 Descriptor table (existing binary format retained, semantics now load-bearing)

The v1 descriptor bytes format (`coregc_layout.rs`, `COREGC_DESCRIPTOR_MAGIC
= CGD1`) already encodes everything the scanner needs — it was under-used,
not under-specified:

```text
Header: magic:u32, version:u32, count:u32
Per type (repeated `count` times):
  type_id:u32, kind:u32 (1=struct,2=array), payload_alignment:u32,
  fixed_payload_bytes:u32 (u32::MAX for array), slot_count:u32,
  array_stride:u32 (0 for struct)
  Per slot (repeated `slot_count` times):
    offset:u32, storage_tag:u32, target_type_id:u32 (0 if not a ref),
    size:u32
```

`storage_tag == 7` (`ManagedRef`) is the only scannable slot kind in v1;
`storage_tag == 8` (`DynamicRef`) is inventoried (for future `anyref` work)
but any lowering that would need to scan one is rejected in v1's preflight
(§6.1). For a struct, `slots` is every field in declaration order. For an
array, `slots` is exactly one entry describing the element layout at payload
offset 4 (offset 0 is reserved for the `i32` length word, matching the
existing bounds-check convention); the runtime scanner uses `array_stride`
and the live length to iterate elements.

This is the "descriptor-driven scanning" requirement: the generated
**scanner is one generic runtime function**, not N per-type generated
functions. It reads a type's row directly out of the descriptor bytes
embedded in linear memory (already emitted at module-build time) and, for
each `ManagedRef` slot, computes the child's address and validates+marks it.
For arrays it loops `length` times using `array_stride`. This is a
deliberate simplification versus the earlier fallback plan's aspiration of
per-type generated straight-line scanners (§7.5 of the superseded plan): a
data-driven interpreter over an immutable, already-validated descriptor table
is dramatically simpler to build correctly in this pass and is still fully
precise (it never inspects a byte outside a declared `ManagedRef` slot).
Generating specialized per-type scanners is a valid Phase 6 optimization
later; it must produce identical scan results to this interpreter, which
remains the semantic reference.

### 3.4 Allocator algorithm

```text
alloc(type_id, payload_bytes) -> GcRef:
  require type_id != 0
  block_bytes = align8(HEADER_BYTES + payload_bytes)   // traps on overflow
  // 1. first-fit free list
  walk free_list_head via next_free, tracking previous link:
    if block.block_bytes >= block_bytes:
      unlink from free list (no split; v1 accepts internal fragmentation,
        see §12 for the coalescing/splitting follow-up)
      block.flags = ALLOCATED; block.type_id = type_id;
      block.payload_bytes = payload_bytes  // block_bytes unchanged
      return payload address
  // 2. bump allocation, growing memory as needed (existing grow logic)
  reserve `block_bytes` at the bump frontier, growing memory a page at a time
    if the frontier would exceed the current memory size; trap HEAP_OOM if
    growth fails or would exceed `maximum_pages`
  initialize header fully (flags=ALLOCATED, type_id, payload_bytes,
    next_block = old block_list_head, next_free = 0, block_bytes)
  block_list_head = this block's payload address
  return payload address
```

Freshly bump-allocated payload bytes are zero (Wasm memory-grow zeroes new
pages; the bump region is never reused without going through the free list
first). Reused free-list payload bytes are **not** assumed zero; any operator
whose semantics requires zero-initialized payload (`array.new_default`) emits
an explicit zero-fill loop (§6.5), which also covers reused-block hygiene for
that case. `struct.new` and array element writes always assign every payload
word directly from source operands (WasmGC requires struct.new to supply all
fields), so no separate zero-fill is needed there — see §6.4 for why no
collection can observe a partially-initialized object.

### 3.5 Mark worklist

```text
worklist: PerEntity region in reserved (non-user-addressable) memory
  entries: (address: i32, type_id: i32) pairs
  globals: worklist_base, worklist_count, worklist_capacity
```

`mark_ref(addr, type_id)`: validates the pair (§3.1 invariants — null is a
no-op, partial-null traps, header/type mismatch traps); if the header's MARK
bit is clear, sets it and pushes the pair onto the worklist, growing the
reserved worklist region (a `memory.grow` of the same single memory, in a
region below the managed heap so it can never collide with heap addresses)
on overflow, or trapping `WORKLIST_OOM` if growth fails. It never recurses.

### 3.6 Collection algorithm

```text
collect(frame_head):
  trap if already collecting (reentrancy guard)
  collecting = 1
  for each frame in the linked shadow-frame chain from frame_head:
    validate frame (aligned, bounded slot_count, monotonically-descending
      `previous` links, no overlap with the heap region)
    for each slot: if non-null, mark_ref(slot.addr, slot.type_id)
  while worklist not empty:
    pop (addr, type_id)
    look up descriptor row for type_id (trap BAD_TYPE_ID if absent)
    if kind == struct: for each ManagedRef slot, load the pair at
      addr+slot.offset and mark_ref it
    if kind == array: load length at addr+0; for i in 0..length, load the
      pair at addr+4+i*array_stride and mark_ref it (skipped entirely if the
      element storage is not ManagedRef)
  sweep: walk block_list_head via next_block; for each block:
    validate header (trap ALLOCATOR_CORRUPTION on a block that is neither
      ALLOCATED nor FREE, or whose type_id/flags are inconsistent)
    if FREE: leave untouched (already on the free list)
    if ALLOCATED and MARK set: clear MARK only; stays allocated
    if ALLOCATED and MARK clear: flags = FREE, type_id = 0, push onto
      free_list_head via next_free
  collecting = 0
```

Collection either completes fully or traps; there is no code path that
returns having swept part of the block list. This directly satisfies the
outstanding requirements from the previous session: descriptor-driven
scanning (3.3/3.6), iterative worklist (3.5), sweep that retains marked and
reclaims unmarked objects (3.6), and free-list reuse (3.4) — specified once,
together, so no later addition can contradict an earlier one.

### 3.7 Shadow root frames (existing ABI, retained unchanged)

```text
Frame: previous:i32, slot_count:i32, slots: [ (addr:i32, type_id:i32) ] * slot_count
```

`push_frame(slot_count) -> frame_addr`, `pop_frame(frame_addr)` (traps unless
`frame_addr` is the current stack head — v1 keeps the existing fail-closed
"no nested-pop reordering" rule), `root_store(frame, slot, addr, type_id)`,
`root_clear(frame, slot)`. This is exactly the Phase 3 ABI already built and
tested; it moves file (§4) but its memory layout and function contracts do
not change.

## 4. Module layout (replaces the old file set — see §7 for deletions)

```text
crates/portal-jsc-waffle/src/
  coregc.rs           inventory: walk Module signatures -> CoreGcInventory,
                       deterministic CoreGcTypeId assignment, GC-operator scan.
                       (kept from the existing implementation; solid, reused.)
  coregc_layout.rs     descriptor byte encoding (kept; §3.3 already matches).
  coregc_runtime.rs    NEW: generates alloc/free-list/validator/worklist/
                       mark/sweep/collect — the complete §3.2-3.6 contract.
  coregc_roots.rs      NEW (moved + renamed from coregc_phase3.rs):
                       push/pop/store/clear shadow-frame ABI (§3.7).
  coregc_lower.rs      REWRITTEN: source Module -> core Module transform.
                       Owns: preflight/support matching, LowerValue value
                       plan, CFG/value rewrite, liveness, checkpoint/root
                       insertion, direct-call ABI flattening.
  coregc_emit.rs       REWRITTEN: thin orchestration only —
                       emit_coregc(source, options) -> inventory ->
                       descriptors -> runtime -> lower -> validate -> artifact.
```

Ownership is one invariant per module, matching `codebase-design` deep-module
guidance: `coregc_runtime` never interprets a source signature;
`coregc_lower` never encodes a header byte offset directly (it calls into
`coregc_runtime`-exposed `Func` handles); `coregc_emit` implements no
algorithm, only sequencing and the final validation gate.

## 5. Public interface

```rust
pub struct CoreGcOptions {
    pub initial_pages: usize,
    pub maximum_pages: Option<usize>,
    pub heap_base: u32,
    /// 0 forces a collection attempt at every checkpoint; used by the
    /// forced-collection acceptance tests in §9.
    pub collect_threshold_bytes: u32,
}

pub struct CoreGcArtifact {
    pub module: portal_pc_waffle::Module<'static>,
    pub inventory: CoreGcInventory,
    pub descriptors: CoreGcDescriptorTable,
}

pub fn emit_coregc(
    source: &portal_pc_waffle::Module<'_>,
    options: &CoreGcOptions,
) -> Result<CoreGcArtifact, CoreGcError>;
```

`emit_coregc` is the only public entry point once this plan's implementation
is activated (§9). It either returns a fully validated, fully rooted,
fully-scanned-and-swept artifact, or a `CoreGcError` naming the first
unsupported source function/value/operator/type. There is no partial-support
mode a caller can opt into.

## 6. Lowering: value plan, CFG rewrite, liveness, checkpoints

### 6.1 Preflight

Before any output is generated, `coregc_lower`:

1. builds `CoreGcInventory` and `CoreGcDescriptorTable` (fails closed on any
   `DynamicRef`/shared/unsupported storage per the existing inventory rules);
   `CoreGcStorage::FuncRef { nullable, target: Signature }` (§6.7) is treated
   as scalar-classified, not fat-ref, and never generates a descriptor scan
   entry;
2. for every function, classifies every value's `LowerValue` plan (§6.2) from
   its Waffle `Type`: a concrete `HeapType::Sig` reference into a
   `Struct`/`Array` signature is `FatRef`; a concrete `HeapType::Sig`
   reference into a `Func` signature is `Scalar(I32)` (§6.7); any other
   `Type::Heap` shape (`anyref`, `i31ref`, `externref`, `eqref`, `structref`,
   `arrayref`) is rejected by name;
3. walks every operator in every function against a **total** matcher: the
   v1 accepted set is exactly `StructNew`, `StructGet`, `StructSet`,
   `ArrayNewDefault`, `ArrayGet`, `ArraySet`, `ArrayLen`, `RefNull`,
   `RefIsNull`, `Call`, `CallRef`, `CallIndirect` (against a source table
   whose element type is a plain `funcref`, §6.7), `RefFunc`, ordinary
   scalar/comparison/conversion operators, and the terminators
   `Br`/`CondBr`/`Return`. Anything else (including `Select`, tail-call
   terminators `ReturnCall`/`ReturnCallIndirect`/`ReturnCallRef`, and
   `Unreachable`/`UB` used as a real reachable path) is rejected with the
   operator name, function index, and value index;
4. for every `Call`/`CallRef`/`CallIndirect` target (a specific callee for
   `Call`, a source `Signature` for `CallRef`, a source `Table` for
   `CallIndirect`), confirms the callee(s)/signature/table pass the same
   classification as every other function — an unsupported/unclassifiable
   callee is rejected before any output exists, not discovered mid-lowering;
5. collects the **function-reference table inventory** (§6.7): every `Func`
   ever taken by a `RefFunc` operator, plus every source `Table`/`func_elements`
   entry ever addressed by a `CallIndirect`, each assigned a deterministic
   output table slot;
6. only after all functions pass steps 2-5 does lowering emit anything.

### 6.2 Value plan

```rust
enum LowerValue {
    Scalar(Type),                                   // unchanged core scalar
    FatRef { nullable: bool, concrete: CoreGcTypeId }, // two i32 words
}
```

Every Waffle `Value` in an accepted function has exactly one `LowerValue`,
computed once in preflight from its declared type (block-param type,
operator result type, or function parameter/return type). A `FatRef` value
is *always* carried as two adjacent core `i32`s through locals, block
params, branch arguments, call arguments, call/function returns, and payload
storage — never as one raw address. This is enforced structurally: the
lowering functions that build core-block-params/branch-args/call-args/
returns take `&[LowerValue]` and expand each entry to 1 or 2 core values;
there is no code path that constructs a 1-word core value from a `FatRef`
plan.

### 6.3 Function and CFG flattening

For each accepted source function:

1. compute the flattened core signature: each source param/return type maps
   to 1 core `i32`/`i64`/`f32`/`f64` (scalar) or 2 core `i32`s (`FatRef`), in
   order;
2. create the core function's entry block with flattened params in the same
   order (a source `FatRef` param becomes two adjacent core params, joined
   back into one `LowerValue::FatRef` conceptually via a pair of Waffle
   `Value`s tracked in the lowering context — call this pairing a "fat pair"
   throughout);
3. for every other source block, in source order, create a matching core
   block whose params are the flattened form of the source block's params
   (preserves block identity 1:1; no block merging/splitting in v1);
4. translate each source block's instructions in program order, resolving
   operands from a `Value -> LowerValue-shaped core value(s)` map populated
   as each source value is lowered (a `FatRef` maps to a `(Value, Value)`
   pair of core values: address, type_id);
5. flatten `Br`/`CondBr` target argument lists the same way as block params;
6. flatten `Return` values the same way as function returns;
7. after all blocks are translated, call `FunctionBody::recompute_edges()`
   and `FunctionBody::validate()` on the lowered body before proceeding to
   the next function — a body that fails Waffle's own validator is an
   internal lowering bug, not a source-diagnostic condition, and is
   `panic!`/`debug_assert!`-checked accordingly rather than surfaced as a
   `CoreGcError`.

### 6.4 Liveness and checkpoint insertion (the core new algorithm)

**Checkpoint instructions** are every `StructNew`, `ArrayNewDefault`, and
`Call` in the accepted operator set — uniformly, so calls are not a special
case bolted on afterward (§0 point 3). Each has an associated *checkpoint
spill set*: the `FatRef` values that must already be stored in the current
function's shadow frame before that instruction executes, because the
instruction may itself trigger a collection (allocation) or call into code
that may (a callee that allocates).

Per function, after CFG flattening but before emitting any runtime calls:

1. **Per-block def/use sets**, restricted to `FatRef`-plan values only:
   - `defs(B)`: values whose Waffle definition (block param or operator
     result) is in `B`;
   - `uses(B)`: `FatRef` operands of every instruction and the terminator in
     `B` whose *defining* block is not `B` (an upward-exposed use — SSA
     program order means a same-block def always precedes its use, so it is
     never upward-exposed).
2. **Block-level backward dataflow to a fixed point:**
   `live_out(B) = ⋃ live_in(S)` for successors `S`;
   `live_in(B) = uses(B) ∪ (live_out(B) - defs(B))`.
   Iterate over all blocks until no set changes; this always converges
   (monotonically growing, bounded by the finite value set) but the
   implementation caps iterations at `4 * blocks.len() + 16` and treats a
   non-convergent result as an internal-invariant failure (not a silent best
   effort — a CFG that cannot converge indicates a malformed input graph).
3. **Per-instruction backward sweep within each block**, seeded from
   `live_out(B)`, walking the block's instructions (terminator first, then
   operators in reverse program order):
   - terminator: add every `FatRef` branch-target-argument / return value to
     `live`;
   - for each instruction, *in this order*:
     a. if the instruction has a `FatRef` result, remove it from `live`
        *first* — the checkpoint this instruction may lower to (e.g. the
        internal alloc call inside `struct.new`) runs before this result
        exists, so it can never itself be something that checkpoint needs
        already rooted, even though a *later* instruction's use of this same
        result (processed earlier in this reverse walk) may have
        provisionally added it to `live`;
     b. add every `FatRef` operand of the instruction to `live` (they must
        already be rooted to survive this instruction — for a checkpoint
        instruction specifically, they are consumed by payload stores /
        passed as call arguments only *after* its internal call returns, so
        they must be protected across it; see the worked example below);
     c. if the instruction is a checkpoint instruction (`StructNew`,
        `ArrayNewDefault`, `Call`, `CallRef`, `CallIndirect`):
        `checkpoint_live[instr] = live` (now correctly excluding the
        instruction's own not-yet-existing result and including its
        operands, from steps a and b above).
4. `spilled = ⋃ checkpoint_live[instr]` over every checkpoint instruction in
   the function. This is the exact, permanent set of `FatRef` values that
   need a shadow-frame slot in this function.
5. **Slot assignment**: one permanent slot per value in `spilled`, indexed
   deterministically (ascending Waffle `Value` index). v1 does **not** reuse
   slots across non-overlapping lifetimes — every spilled value keeps its
   slot for the rest of the function's execution once written. This is a
   documented, safe over-approximation (a value may be retained slightly
   longer than its true last use), not a correctness gap; slot-reuse
   liveness-interval coalescing is a Phase 6 optimization (§12) that must not
   change what stays reachable, only how long.
6. **Codegen**, once slots are assigned:
   - if `spilled` is non-empty: emit `push_frame(len(spilled))` as the first
     instruction of the lowered entry block (after any parameter fat-pair
     setup), and thread the resulting `frame` value through every block that
     can reach a checkpoint or a return (v1's function-level frame is
     visible everywhere in the function, not re-pushed per block);
   - immediately after **every** definition site of a spilled value —
     whether that is a checkpoint instruction's result, a non-checkpoint
     operator's result (e.g. `StructGet`/`ArrayGet` of a managed field), or a
     block's own entry (for a spilled block parameter, including a spilled
     function parameter, which is a parameter of the entry block) — emit
     `root_store(frame, slot, addr, type_id)`;
   - immediately before **every** checkpoint instruction, emit
     `checkpoint(frame)` (a call into the runtime, §6.5) — this is what may
     invoke `collect`;
   - at **every** `Return`, if `spilled` is non-empty, emit `pop_frame(frame)`
     immediately before the return values are produced (frame teardown is
     therefore guaranteed on every reachable exit; v1 accepts only
     `Return`/`Unreachable` as function exits, so this is exhaustive by
     construction, not a best-effort walk).
7. A **debug verifier** (`#[cfg(debug_assertions)]`, also run explicitly in
   the acceptance suite, §9) walks the lowered core body after generation and
   confirms: every checkpoint call is immediately preceded by a `root_store`
   for each of its `checkpoint_live` values (i.e. those values' most recent
   `root_store` in program order strictly precedes the checkpoint on every
   path reaching it), and every reachable `Return` is preceded by exactly one
   `pop_frame` when `spilled` is non-empty. A violation is a lowering bug
   (`panic!`), not a source diagnostic — this is the "negative fixture"
   requirement from the prior draft, implemented as an always-on internal
   proof rather than a hoped-for property.

**Worked example — why struct.new's operands must be captured *before*
removing its own result, step 3a before 3b:**

```text
%r = struct.new $Point(%a, %b)     ; %a, %b, %r are FatRef
return %r
```

Lowering `struct.new` is not one runtime call; it is:

```text
checkpoint(frame)                  ; may collect — must not free %a's or %b's targets
%addr = call __coregc_alloc(type_id, payload_bytes)
store_pair(%addr + off_a, %a.addr, %a.type)
store_pair(%addr + off_b, %b.addr, %b.type)
%r = (%addr, type_id)
```

The `checkpoint` happens *before* `%a`/`%b` are written into the new
payload. If `%a`/`%b` were not in `checkpoint_live[struct.new]`, a collection
inside `checkpoint` could reclaim whatever `%a`/`%b` point to (nothing else
roots them at that exact instant), and the subsequent `store_pair` would
write a dangling pair into `%r`. Because step (b) unions in `fatref_operands`
before step (c) captures `checkpoint_live`, and step (a) already removed `%r`
itself, the captured set is exactly `{%a, %b}` (plus anything else still live
from later in the block) — never `%r`. `%a` and `%b` are guaranteed to
already have a `root_store` from their own definition sites (step 6), so they
are valid roots at the moment `checkpoint` runs. The same reasoning is why
`Call`/`CallRef`/`CallIndirect` arguments are included identically — such a
checkpoint precedes the actual call operator, and the flattened fat-ref
arguments must survive it to be passed once the callee is actually invoked.

This exact ordering bug — capturing `checkpoint_live` before removing the
instruction's own provisionally-live result — is what the first
implementation attempt against this plan actually shipped and caught via the
`array_built_and_summed_survives_a_forced_collection_at_every_allocation`
test (a struct allocated then immediately stored into an array slot, so the
struct's own result value was already `live` from the store when the
struct.new checkpoint was reached): the corrected order above is what is
implemented.

### 6.5 Runtime call surface used by lowering

`coregc_lower` never re-encodes a header offset; it only calls generated
`Func`s exposed by `coregc_runtime`/`coregc_roots`:

```text
__coregc_checkpoint(frame: i32)                       -- may collect
__coregc_alloc(type_id: i32, payload_bytes: i32) -> i32
__coregc_zero_bytes(addr: i32, byte_len: i32)          -- array.new_default only
__coregc_validate_ref(addr: i32, type_id: i32) -> i32  -- returns addr or traps
__coregc_array_bounds(addr: i32, type_id: i32, index: i32) -> i32
__coregc_push_frame(slot_count: i32) -> i32
__coregc_pop_frame(frame: i32)
__coregc_root_store(frame: i32, slot: i32, addr: i32, type_id: i32)
__coregc_root_clear(frame: i32, slot: i32)
```

### 6.6 Struct/array operator lowering

| Source op | Core lowering |
| --- | --- |
| `struct.new $T(...)` | checkpoint(frame); `alloc(type_id(T), fixed_bytes(T))`; typed store per field at its descriptor offset (`ManagedRef` fields store both words, `FuncRef` fields store one word, §6.7); result is the fat pair |
| `struct.get $T.i` | `addr = validate_ref(recv.addr, recv.type)`; typed load at the field's offset; a `ManagedRef` field loads both words and the loaded pair is *not* re-validated at load time (validated lazily the next time it is dereferenced/scanned — validating on every load as well as every dereference would be redundant, not incorrect; the plan chooses load-time-cheap/dereference-time-checked) |
| `struct.set $T.i` | `validate_ref(recv)`; typed store (both words for `ManagedRef`, one word for `FuncRef`) |
| `array.new_default $A(len)` | checkpoint(frame); checked `4 + len*stride` (traps on overflow); `alloc`; store length at offset 0; `zero_bytes(addr+4, len*stride)` |
| `array.len` | `validate_ref`; load offset 0 |
| `array.get $A[i]` | `validate_ref`; `array_bounds(addr, type, i)`; typed load at `4 + i*stride` |
| `array.set $A[i] = v` | `validate_ref`; `array_bounds`; typed store at `4 + i*stride` |
| `ref.null $T` (struct/array `T`) | constant fat pair `(0, 0)` — no runtime call |
| `ref.null $T` (func `T`) | constant scalar `0` (§6.7) — no runtime call |
| `ref.is_null` | struct/array receiver: `addr == 0` (the invariant guarantees `type_id == 0` iff `addr == 0` for any value that ever passed validation); func receiver: `value == 0` |
| `call f(...)` | checkpoint(frame) with `checkpoint_live` per §6.4; flatten args; `Operator::Call { function_index: <lowered f> }`; flatten/re-pair the (possibly multi-value) result |
| `ref.func $f` | compile-time-constant scalar `table_slot_of(f)` — no runtime call, no allocation (§6.7) |
| `call_ref $sig(args..., callee)` | checkpoint(frame) with `checkpoint_live` (callee scalar plus flattened fat-ref args, §6.7); `Operator::CallIndirect { sig_index: <flattened sig>, table_index: <function-ref table> }` using the callee scalar as the index operand — no explicit null/arity check emitted; core Wasm's own `call_indirect` semantics (uninitialized-element trap, type-mismatch trap) already cover both |
| `call_indirect $sig $table(args..., index)` | same as `call_ref`, against the corresponding copied source table instead of the generated function-ref table |

No generic opcode interpreter exists; each row is a fixed generator, matching
the prior `coregc_array.rs`/`coregc_lower.rs` style but generalized to work
inside the CFG/liveness machinery instead of one hand-matched function shape.

### 6.7 Function references: `RefFunc`/`CallRef`/`CallIndirect`

This section was originally drafted (previous revision of this plan) as a
heap-allocated "function object" with its own descriptor kind, deferred to a
later activation. Before implementing that, `crates/portal-jsc-waffle/src/conv.rs`
was checked directly for how jsaw's own WasmGC backend actually uses these
operators (searched for `RefFunc`/`CallRef`/`CallIndirect`/`TableData`):

- `conv.rs` never emits `CallIndirect`; it uses typed `RefFunc`/`CallRef`
  exclusively for its own dispatch, and separately builds one ordinary
  `TableData { ty: Type::Heap(WithNullable { value: HeapType::FuncRef, .. }) }`
  (`declare_function_reference`) purely to expose function references to a
  host/import boundary — i.e. jsaw's own compiler already treats "a callable
  function" as a plain core-Wasm `funcref` table entry, not a heap object;
- a jsaw closure (e.g. around `conv.rs`'s `js_export_*`/adapter-construction
  code) is an ordinary `struct.new` whose fields are: a `RefFunc`-typed code
  pointer (`ref $adapter_sig`), an `i32` arrow flag, a boxed/`anyref`
  `captured_this`, an `anyref` property trie, and an `i32` tag. **The closure
  itself is already just a struct** — already fully covered by §6.6's struct
  lowering — whose "function object" nature comes entirely from one field
  being function-reference-typed, not from any special allocation shape.

This means the originally-drafted heap-allocated function-object design was
unnecessary complexity: a typed function reference never needs GC identity,
marking, or scanning, because the code it names is static module content
that is always alive, never collected. The corrected v1 design:

**Representation.** A concrete function-reference-typed value
(`Type::Heap(HeapType::Sig { sig_index })` where `sig_index` resolves to a
`SignatureData::Func`, not `Struct`/`Array`) is `LowerValue::Scalar(Type::I32)`
— a **1-based output table slot index**, not a fat pair. `0` is reserved as
the null function reference. No new `LowerValue` variant, no header, no
descriptor scan entry, and no runtime validator function are needed; a
function-reference-typed struct/array field costs exactly 4 bytes (one
scalar word), not 8.

**Inventory (§6.1 step 5).** `coregc.rs` gains
`CoreGcStorage::FuncRef { nullable: bool, target: Signature }` (the `target`
is the callee's *source* `Func` signature) as a sibling of `ManagedRef`,
chosen instead of `ManagedRef` whenever `storage()`'s `HeapType::Sig` case
resolves to a `SignatureData::Func`/`Import` rather than `Struct`/`Array` —
this requires threading `&Module` into `storage()`, which it does not
currently take. `coregc_layout.rs` assigns `FuncRef` slots 4-byte
alignment/size and a new `storage_tag` value (`9`, distinct from
`ManagedRef`'s `7` and `DynamicRef`'s `8`); `coregc_runtime`'s generic scanner
(§3.3/§3.6) simply has no case for tag `9`, so it is skipped during scanning
automatically, with no runtime change required beyond the new tag constant
being unrecognized-and-ignored (not unrecognized-and-erroring — the scanner's
"unknown storage tag" default is "not scannable," matching pointer-free
scalar fields; only an unknown descriptor *kind*, not an unknown *slot tag*,
is a corruption error).

**Function-reference table.** `coregc_lower` builds exactly one generated
core `TableData { ty: funcref, func_elements: Some([Func::invalid(), ...]) }`
whose slot `0` is deliberately left `Func::invalid()` (the null sentinel) and
whose slots `1..N` hold the *lowered* `Func` handles for every distinct
source `Func` ever named by a `RefFunc` operator anywhere in the module,
assigned in ascending source-`Func`-index order (deterministic). Any source
`Table`s actually addressed by a `CallIndirect` are copied over similarly
(their `func_elements` remapped from source to lowered `Func` handles) rather
than merged into the generated table, preserving the source's own table
identity/indices for that call site.

**Operator lowering** (added to §6.6's table): `RefFunc { func_index }`
lowers to a compile-time-constant scalar (the callee's assigned table slot;
no instruction is generated at all beyond the constant). `CallRef { sig_index }`
and `CallIndirect { sig_index, table_index }` both lower to
`Operator::CallIndirect` against the corresponding table, using the already-
lowered scalar index value with **no explicit null or arity check emitted by
coregc** — core Wasm's own `call_indirect` already traps deterministically on
an out-of-bounds index, an uninitialized (`Func::invalid()`) slot, or a
signature mismatch, which exactly covers "called a null function reference,"
"stale/corrupt index," and "mismatched signature" without any new runtime
helper or trap code.

**Liveness/checkpoints.** `CallRef`/`CallIndirect` join `Call` in §6.4's
checkpoint-instruction set with identical treatment: the callee's scalar
index is `LowerValue::Scalar`, so it is never spilled (scalars never need
root slots), while its flattened fat-ref *arguments* are captured into
`checkpoint_live` exactly like any other call's arguments. No new liveness
rule is needed — §6.4 already generalizes to "any instruction that may call
into code that may allocate," which a validated `call_indirect` is,
structurally, identically to a direct `Call`.

**What this does and does not unlock.** This makes `RefFunc`/`CallRef`/
`CallIndirect` over concrete (non-`anyref`) signatures a real, tested part of
the v1 activation — e.g. a function-pointer-table pattern over concrete
struct/array/scalar types works end-to-end. It does **not**, by itself, make
jsaw's actual emitted closures lower under coregc: those closures' other
fields (`captured_this`, the property trie) are `anyref`-typed, which §1/§11
already scope to the deferred `Repr`/`anyref` plan. The call *mechanism* is
nonetheless exactly what that later work will reuse unchanged — this section
exists so that v2 (`anyref`) is an addition on top of this call mechanism,
not a second redesign of it, matching this plan's §0 structural fix applied
one level down.

## 7. Legacy removal (done before any new code lands)

Delete outright (content fully superseded by §3-§6, not incrementally
migrated — keeping them around invites the exact "two incompatible collector
contracts" failure from §0):

- `crates/portal-jsc-waffle/src/coregc_array.rs`
- `crates/portal-jsc-waffle/src/coregc_lower.rs` (rewritten from scratch per
  §6, not edited in place)
- `crates/portal-jsc-waffle/src/coregc_phase3.rs` (content moves into
  `coregc_roots.rs`, §4, with the `_phase3` export-name suffixes dropped)
- the "Phase 1/2/3" proof-emitter surface of `coregc_emit.rs`
  (`emit_runtime_skeleton`, `emit_scalar_struct_subset`,
  `emit_scalar_array_subset`, `__coregc_collect_phase2`,
  `__coregc_alloc_phase1`, `__coregc_array_bounds_phase1`, the `_phase3`
  exports) is removed; `coregc_emit.rs` is rewritten as the thin orchestrator
  in §4/§5.

Retained and reused as-is (already match this plan's contract, confirmed in
§3.3 and inventory review):

- `coregc.rs` (`CoreGcInventory`, `CoreGcTypeId`, `CoreGcStorage`, ...);
- `coregc_layout.rs` (`CoreGcDescriptorTable`) — the encoding needs no format
  change, only a new `FunctionObject` `kind` value reserved for §6.6 (added
  as a no-op variant now, unused until v2, so the byte format never needs a
  second version bump).

`lib.rs`'s `pub mod`/`pub use` list for `coregc*` is rewritten to match the
new module set; no `_phase1`/`_phase2`/`_phase3` symbol survives.

## 8. Implementation stages (each stage permanently valid, not a throwaway phase)

Unlike the prior three-phase scaffolding, every stage below commits code
whose data contract is the frozen §3 contract from the start. A later stage
adds a capability; it never changes what an earlier stage already committed.
Each stage ends in a real, green `cargo test` state; `emit_coregc` is not
exported/wired until Stage 6 passes its full acceptance suite (§9) — earlier
stages export their pieces only as `pub(crate)` building blocks, so there is
no window where a partially-capable `emit_coregc` is reachable by a caller.

1. **Legacy removal + descriptor `FunctionObject` reservation** (§7). Compiles,
   existing `coregc`/`coregc_layout` unit tests still pass. *(Done. The
   function-object reservation was later superseded by §6.7's simpler
   table-slot design — no `kind == 3` is ever emitted; the reservation
   remains harmless.)*
2. **`coregc_runtime.rs`**: header v2 semantics (§3.2), first-fit allocator
   with free-list reuse (§3.4), validator, array-bounds check, worklist
   (§3.5), generic descriptor-driven `mark_ref`/scan dispatch, `sweep`,
   `collect` (§3.6), `zero_bytes`. Tested standalone by direct Waffle
   fixtures that call these `Func`s directly through Wasmtime, with no
   lowering involved — this proves retention/reclamation/reuse/worklist-
   exhaustion *before* the general lowerer exists, isolating runtime bugs
   from lowering bugs. *(Done: 9 tests in `coregc_runtime.rs`.)*
3. **`coregc_roots.rs`**: moved from `coregc_phase3.rs`, renamed exports,
   trap codes wired in. *(Done, folded into Stage 2's commit.)*
4. **`coregc_lower.rs`, part A — struct/array/branches, no calls**: value
   plan (§6.2), preflight (§6.1 steps 1-3 only), CFG/value flattening
   (§6.3), the full liveness/checkpoint algorithm (§6.4) restricted to
   `StructNew`/`ArrayNewDefault` as the only checkpoint instructions, and
   struct/array operator lowering (§6.6, excluding the call rows). Concrete
   deliverables:
   - a `LiveSet`/liveness module computing `defs`/`uses` per block, the
     block-level fixed-point dataflow, and the per-instruction backward
     sweep producing `checkpoint_live` (§6.4 items 1-4), independently unit
     tested against hand-built multi-block CFGs (including an irreducible
     back-edge loop) with obviously-correct expected live sets, *before*
     wiring it into codegen;
   - the function/block/value translation context (source `Value`/`Block` ->
     lowered `LowerValue`/`Block` maps) and the two-pass block-creation order
     from §6.3;
   - root-slot assignment and codegen (§6.4 items 5-6) plus the debug
     verifier (§6.4 item 7);
   - struct/array operator lowering table rows that do not involve a call.
   Tested with hand-built multi-block fixtures (loops constructing/
   traversing a linked list, wide/deep graphs, cycles, self-cycles) under
   forced collection (`collect_threshold_bytes = 0`), asserting both
   retention of reachable graphs and reclamation of unreachable ones, plus
   the root-verifier negative fixture from §9.
5. **`coregc_lower.rs`, part B — direct calls, function references**: extend
   part A's liveness (already generalized to a `is_checkpoint_instruction`
   predicate, §6.4) to also cover `Call`/`CallRef`/`CallIndirect`; add the
   two-pass whole-module lowering needed for (mutually) recursive/forward
   calls (preflight allocates every accepted function's lowered `Func`
   placeholder and flattened signature *before* any body is translated, §6.1
   step 4); add the function-reference table inventory and construction
   (§6.1 step 5, §6.7); add the `Call`/`RefFunc`/`CallRef`/`CallIndirect`
   lowering rows (§6.6, §6.7). *(Done, with three real bugs found by the
   tests: the call-site signature must be the callee's flattened signature
   without the selector operand folded in; waffle-backend stored multi-result
   operator values to locals in swapped order (fixed upstream in waffle-);
   and push_frame needed to zero its slots against stale memory reuse after
   pop.)* Tested with: a caller/callee pair where the callee allocates and
   the caller holds a live reference across the call under forced collection;
   a `RefFunc`/`CallRef` fixture over a concrete signature exercising a
   null-function-reference trap and successful indirect calls.
6. **`coregc_emit.rs` orchestrator + `emit_coregc` activation**: wires
   Stages 1-5 together behind the single public entry point (§5), runs the
   full acceptance suite (§9), and is the only stage that changes `lib.rs`'s
   public exports. *(Done — `emit_coregc` is public in `lib.rs`.)*

Stage boundaries are commit boundaries. A stage that cannot be completed
safely is reverted to the end of the previous stage — never left half-wired
into the next one, which is the specific mistake §0 diagnoses.

## 9. Acceptance suite (blocks Stage 6 activation)

- **Descriptor/codegen**: deterministic type-ID/descriptor bytes across
  insertion-order permutations (existing tests already cover this); every
  emitted function body passes `FunctionBody::validate()` and the encoded
  module passes `wasmparser::Validator::new().validate_all` with default core
  features; a byte/operator scan of the encoded module proves the absence of
  any GC/reference-type instruction or type section entry.
- **Runtime** (direct Wasmtime calls into `coregc_runtime`/`coregc_roots`
  exports, no lowering involved): leaf/array allocation; root in a shadow
  slot; self-cycle, two-cycle, wide graph, deep graph retention; nullable
  child; deliberate partial-null/mismatched/stale/freed-pair traps; repeated
  alloc/collect reuses a freed block (`address` reused, `wasmparser`-valid
  before and after); worklist and root-stack capacity exhaustion trap
  deterministically (no partial sweep); nested frame push/pop/clear LIFO
  discipline (existing tests already cover push/pop/store/clear — kept).
- **Lowering** (through `emit_coregc` end to end): struct/array
  new/get/set/len, direct calls, and `RefFunc`/`CallRef`/`CallIndirect` over
  concrete signatures (including a null-function-reference trap and a
  mismatched-table-slot trap), branches carrying fat refs, forced collection
  at every checkpoint (`collect_threshold_bytes = 0`) proves a local managed
  value used after later allocations/calls is still correct; the debug
  root-verifier (§6.4 item 7) runs and passes on every fixture; a
  deliberately hand-corrupted lowering (omitted `root_store` before a
  checkpoint, added directly to a test-only unchecked builder, never through
  the real lowerer) is asserted to be caught by the verifier, proving the
  verifier is not a no-op.
- **Differential**: for every accepted fixture, compare
  `source -> native WasmGC -> Wasmtime(GC enabled)` against
  `source -> emit_coregc -> Wasmtime(default features)` for return values and
  traps, at both `collect_threshold_bytes = 0` and a normal threshold (catches
  schedule-dependent rooting bugs the zero-threshold run alone would mask).

All of the above must pass in one clean checkout before `coregc_emit::emit_coregc`
is added to `lib.rs`'s public exports.

## 10. Security/failure posture (unchanged from the superseded plan, restated)

Checked arithmetic for every header/offset/length/stride computation;
alignment/magic/type-ID validation before every dereference; bounds checks on
every array access; distinct trap identities (`BAD_FAT_REF`, `BAD_TYPE_ID`,
`OUT_OF_BOUNDS`, `ROOT_STACK_OVERFLOW`, `HEAP_OOM`, `WORKLIST_OOM`,
`ALLOCATOR_CORRUPTION`) so a host can distinguish a memory-safety fault from
an ordinary trap; no partial sweep or partial root-frame teardown is ever
observable — every failure path traps before committing an inconsistent
state.

## 11. Relationship to the dogfood goal

`docs/plan-dogfood-self-hosting-java.md` (and the broader ambition of running
jsaw's own compiler through coregc) needs, beyond v1: the dynamic value model
(§13.2 — jsaw's `anyref` boundary), the remaining aggregate/control operators
(§13.3), and program-level activation with cross-testing (§13.4, §14). v1's
call mechanism (direct calls and typed function references, §6.7) is complete
and is not a blocker. §13 is grounded in the actual operator inventory of
`conv.rs`/`repr.rs`, and notably jsaw never uses `i31.get`, extern/any
conversion operators, or multi-value function returns — which is why the v2
roadmap is smaller than a full `anyref` implementation would suggest.

## 12. Explicit non-goals / later work

- **shadow-root stack on the managed heap instead of a fixed memory
  region.** v1/v2 reserves a fixed-size root region below the heap
  (configured by `root_stack_bytes`) that shares linear memory with the
  descriptor table and worklist. That design has already bitten twice: the
  frame-collision bound once pointed at the heap base instead of the
  worklist start (so deep frames silently overwrote worklist entries until
  the cross-test harness caught it), and the fixed default was too small
  for real jsaw-compiled programs (the primordial-context builder nests
  hundreds of frames). Moving the shadow stack onto the heap — frames
  allocated as ordinary GC blocks, linked through a root-of-roots pin, with
  `push_frame` calling the normal allocator/checkpoint machinery — removes
  the fixed reservation, eliminates the collision boundary entirely, and
  grows with the program instead of needing a tuned default. It needs care
  around the chicken-and-egg case (allocating a frame may itself want a
  collection, and the collection walks the frame chain being extended), so
  it is a self-contained redesign stage, not an incidental tweak — deferred
  until v2 stage 4 lands and the cross-test suite is the regression gate;
- allocator splitting/coalescing, TLSF or size-class allocation, generated
  per-type scanners as a performance upgrade over the generic interpreter
  (§3.3), and shadow-frame slot-interval reuse (§6.4 item 5) — all
  performance work that must reproduce the current interpreter/one-slot-per-
  value semantics exactly, never change reachability;
- moving/compacting GC, incremental/concurrent collection, weak
  references/finalizers, `memory64`, threads;
- publishing a stable cross-version ABI for fat references outside one
  module instance (they are process/instance-local addresses, never durable
  IDs — unchanged from the superseded plan's §13).

## 13. v2 roadmap: closing jsaw's `Repr` and remaining operator surface

v1 (stages 1-6) is complete and activated. This section details what remains
between v1 and compiling jsaw's own compiler output (the dogfood goal), with
each item grounded in the operators `conv.rs` actually emits (counts from a
direct read of `conv.rs`/`repr.rs`, September 2026).

### 13.1 What jsaw's compiled output actually uses

From a full inventory of `Operator::`/`Terminator::` sites in `conv.rs`,
`repr.rs`, and `repr/trie.rs`:

- **Already covered by v1:** `StructNew/Get/Set`, `ArrayNewDefault/Get/Set/
  Len`, `RefNull`, `RefIsNull`, `RefFunc`, `Call`, `CallRef`, `Br`,
  `CondBr`, `Return`, `Unreachable`, and most scalar ops.
- **Dynamic-value operations** (the dominant gap): `RefCast` (69 sites),
  `RefTest` (49), `RefEq`, `RefI31` — all operating on jsaw's `anyref`-typed
  `value` boundary.
- **Aggregate bulk/packed operations:** `ArrayNewFixed` (14), `ArrayGetU`
  (14), `ArrayCopy` (5), `StructNewDefault` (2), plus `ArrayGetS`/`ArrayFill`
  reachable through string/typed-array helpers.
- **Control flow:** `TypedSelect` (13), `ReturnCall` (11), `ReturnCallRef`
  (2).
- **Scalar ops v1's accept-list is missing** (all pure pass-throughs, no
  design needed — extend `validate_operator`'s whitelist): the remaining
  `I32`/`I64`/`F64` arithmetic/comparison/conversion operators jsaw emits,
  including `I32ShrS`, `I32DivS/RemS`, `I64Shl/ShrS/DivS/RemS`, `F64Neg/
  Trunc/Lt/Le/Gt/Ge/Eq/Ne`, `F64ConvertI32S/I64S`, `I32TruncF64S`,
  `I32TruncSatF64U`, and friends.
- **Never used by jsaw** (confirmed by the same inventory, so v2 need not
  support them): `i31.get_s`/`i31.get_u` (jsaw only *creates* the JS-null
  sentinel and *tests* for it; it never reads the payload back),
  `any.convert_extern`/`extern.convert_any`, and multi-value function
  returns (every function returns 0 or 1 values; jsaw's tagged-union
  `multi_*` types are ordinary structs returned as a single reference).

### 13.2 The dynamic value model (`anyref`/`eqref`/`i31ref`)

This is the one semantically deep addition; everything else in this section
is mechanical. The design constraint is to keep the existing two-word fat
reference instead of introducing a wider `GcValue` triple (the superseded
fallback plan's §4.5 sketch), so that shadow frames, field slots, block
parameters, and call ABIs do not change width.

**Representation.** A dynamic value is the same `(addr: i32, type_id: i32)`
pair as a v1 `FatRef`, with the type-id space extended by one sentinel:

- `(0, 0)` — null. At jsaw's `anyref` boundary this is JS `undefined`;
  against a nullable concrete type it is that type's null. The pair alone
  does not distinguish the two — the *source-level* static type does, and
  every v2 operator carries enough static type information to interpret it.
- `(payload, 0xFFFF_FFFF)` — an i31 immediate (`I31_TYPE_ID`). `payload` is
  the raw i31 bits. Used by jsaw exactly once: `JS_NULL_SENTINEL`. The
  sentinel id is `u32::MAX`, which the dense-from-1 heap type-id assignment
  can never reach (exhaustion is rejected long before), so no descriptor
  table change is needed.
- `(addr, heap_type_id)` — an ordinary managed allocation, unchanged from v1.

**Runtime changes** (small, additive, in `coregc_runtime.rs`):
`validate_ref` and `mark_ref` learn one rule — `type_id == I31_TYPE_ID`
accepts without touching the header (immediates have no header and nothing
to mark). The sweep never sees immediates (they are never in the block
list). `array_bounds`/`struct.get` callers never pass an immediate as a
receiver because the source `ref.cast`/`ref.test` discipline guards those
paths — and if a lowering bug ever let one through, the receiver's concrete
expected type id would mismatch `I31_TYPE_ID` and trap `BAD_TYPE_ID`, which
is the correct failure.

**Inventory/lowering changes:** `CoreGcStorage::DynamicRef { heap: Any | Eq }`
stops being a rejection and becomes a lowerable 8-byte slot kind (a fat pair,
exactly the `ManagedRef` layout) with descriptor `storage_tag` `8`; the
runtime scanner treats tag `8` as scannable, identically to tag `7` (the
only difference is that the pair may hold an immediate, which `mark_ref`
skips via the rule above). `DynamicRef { heap: I31 | Struct | Array | .. }`
arising from operator results (`RefI31`, tests) is a value-plan classification
in `coregc_lower`: `Type::Heap(HeapType::Any | Eq)` lowers to the dynamic fat
pair, `Type::Heap(HeapType::I31)` lowers to the same pair constrained to the
immediate form.

**Operator lowering:**

| Source op | Core lowering |
| --- | --- |
| `ref.i31` | `(payload, I31_TYPE_ID)` — two constants/values, no allocation |
| `ref.test (ref null? $concrete)` | `type_id == id(T)` after validating the pair; for the nullable form, `\|\| addr == 0` per the source type's nullability semantics (a nullable test accepts null, a non-null test rejects it) |
| `ref.test i31` / `ref.test (ref i31)` | `type_id == I31_TYPE_ID` (plus the null rule) |
| `ref.test any`/`eq` | constant true — everything this backend produces is in the `eq` hierarchy (matching jsaw's own comment that its eqref cast "cannot trap in practice") |
| `ref.cast` | the corresponding test, then trap `BAD_CAST` on failure and pass the pair through *unchanged* on success (the actual type id is preserved — a cast never rewrites it) |
| `ref.eq` | validate both; if both are null pairs → true; if both i31 → payload equality; if both heap → address equality, with `addr` equal but `type_id` unequal trapping as corruption (one address has exactly one type); heap-vs-immediate → false |

New trap code: `BAD_CAST = 8`.

### 13.3 Remaining aggregate/control operators

- `ArrayNewFixed(sig, n)`: checkpoint; checked `4 + n*stride`; `alloc`;
  store length; store each of the `n` flattened element arguments at
  `4 + i*stride`. (`ArrayNewFixed` is a checkpoint instruction with all `n`
  fat-ref element arguments in its spill set — the liveness algorithm already
  generalizes; the instruction walker just iterates all operands.)
- `ArrayGetS`/`ArrayGetU`: same address path as `ArrayGet`, loading with the
  signed/unsigned 8/16-bit variants from the element's storage tag.
- `ArrayCopy(dst, di, src, si, len)`: validate both arrays, bounds-check
  `di+len` and `si+len`, then a byte-wise copy loop using `array_stride` —
  handle overlap by copying backward when `dst > src` within the same array
  (the WasmGC semantics are `memmove`, not `memcpy`).
- `ArrayFill(arr, i, v, len)`: validate, bounds-check `i+len`, loop storing
  the (possibly fat) element value.
- `StructNewDefault(sig)`: checkpoint; `alloc`; `zero_bytes(addr,
  fixed_payload_bytes)`. (Struct payloads are fully covered by zeroing:
  scalar fields read as 0, reference slots read as the null pair.)
- `TypedSelect(cond, a, b)`: scalar-only in v1's accept set; extend to
  fat-pair select via two core `select`s (addr, type_id) once dynamic values
  exist. Plain `Select` (untyped) is scalar-only and can be accepted now.
- `ReturnCall`/`ReturnCallRef` (tail calls): lower as *checkpoint;
  root_store all live fat-ref arguments (already in the checkpoint's spill
  set); `pop_frame`; call; return the flattened results*. The
  safety argument: the checkpoint collects while the caller's frame (holding
  the spilled arguments) is still live; between `pop_frame` and the callee's
  own `push_frame` no allocation can occur (a call is not an allocation);
  the callee roots its own parameters on entry. The returned fat refs are
  never exposed to a collection before the caller returns them (no
  intervening checkpoint). This is the exact discipline the superseded plan
  §7.4 specified, restated against the v1 machinery.

### 13.4 Staging

v2 lands in the same stage discipline as v1 (each stage permanently valid,
public surface unchanged until the acceptance suite passes):

1. **Scalar/whitelist completion** — extend `validate_operator` with every
   pure scalar op jsaw emits (§13.1). Purely mechanical; verified by the
   preflight tests gaining one accepted-operator fixture per family.
2. **Aggregate completion** — §13.3's array/struct operators plus
   `Select`/`TypedSelect` and tail calls. Each gets a forced-collection
   end-to-end test in the style of the v1 fixtures.
3. **Dynamic values** — §13.2 (sentinel type id, runtime immediate rule,
   descriptor tag 8 scanning, ref.test/cast/eq lowering). This is the stage
   after which jsaw's `Repr` module itself becomes lowerable; it has its own
   acceptance battery (boxed values, null vs undefined, equality, casts).
4. **Program-level activation** — `conv.rs` output accepted by `emit_coregc`
   for a growing allowlist of e2e fixtures, gated by the cross-testing
   harness in §14.

## 14. Cross-testing: differential execution of jsaw-compiled programs

The goal is to run the *same* jsaw-compiled programs through both backends
and compare observable behavior:

```
JS source -> conv.rs -> portal_pc_waffle::Module (WasmGC IR)
  -> [native] to_wasm_bytes -> wasmtime (wasm_gc=true)      -> results
  -> [coregc] emit_coregc   -> wasmtime (default features)  -> results
```

### 14.1 Harness shape

A `#[cfg(test)]` harness (in `portal-jsc-waffle/tests/`) that, for each
fixture, compiles JS to the native module once, then:

- **native path:** execute exported functions under wasmtime with
  `wasm_gc(true)` + `wasm_function_references(true)` (the configuration the
  existing e2e suite already uses);
- **coregc path:** `emit_coregc` the same module, execute under default
  wasmtime — twice, once with `collect_threshold_bytes = 0` (forced
  collection at every checkpoint) and once with the default threshold
  (catches schedule-dependent rooting bugs the forced run alone would mask);
- compare every exported function's numeric results and trap behavior across
  all three runs;
- if `emit_coregc` returns an error, the fixture is *expected-rejected*:
  the harness asserts the diagnostic names a known-deferred feature (so the
  allowlist of unlowerable features is itself under test, and shrinks as v2
  stages land).

### 14.2 Speed discipline

The existing e2e suite runs every fixture in Node.js, the JVM, and Swift in
addition to wasmtime (~135 s per fixture on this host — that was the
"hang" observed during stage 5 validation; it is external-compiler latency,
not a deadlock). Cross-testing deliberately runs **wasmtime-only** (both
paths above) as the fast inner loop, so the differential suite scales with
fixture count rather than toolchain latency. Node/JVM/Swift remain the
native backend's own parity gate and are unchanged; coregc artifacts are
pure core Wasm, so any engine running the native suite's core subset (or
wasmtime alone) suffices to host them.

### 14.3 Rollout

- v1: the harness exists and runs the v1 hand-built fixtures (already
  covered by `coregc_emit::tests::differential_native_wasmgc_vs_coregc`),
  plus asserts every real `conv.rs` fixture fails closed with a named
  diagnostic (proving the rejection surface is honest, not silently partial).
- v2 stage 4: flip fixtures onto the accepted list as their required
  operators land, in increasing Repr complexity order: numeric cores →
  booleans/comparisons → strings → objects/properties → closures → typed
  arrays → the compiler itself (dogfood).
- The suite fails if a fixture is *silently* omitted: the harness enumerates
  the e2e fixture list, so adding a fixture without a cross-test entry is a
  test failure, matching the inventory-exhaustiveness stance elsewhere.
