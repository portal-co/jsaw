# Plan: coregc — a pure-core-Wasm backend for jsaw's typed WasmGC IR

**Status:** approved for implementation (this revision). Supersedes and
absorbs the prior `docs/plan-wasmgc-linear-memory-fallback.md` design (its
research/prior-art sections remain cited from here) and the prior
`docs/plan-coregc-atomic-collector-and-lowering.md` "atomic activation" draft.
This revision exists because the first implementation attempt against the
prior draft failed for a specific, diagnosable reason (see §0) — this plan
fixes that reason structurally, not just by re-describing the same seam.

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
- `RefNull`, `RefIsNull` for the accepted concrete reference types.

**Explicitly out of v1, with a named fail-closed diagnostic, and a committed
non-conflicting design so v2 can add them without breaking v1** (see §6.6):

- function references / `call_ref` / indirect calls through a table
  (`CallIndirect`, `RefFunc`) — the function-object descriptor kind and table
  layout are pinned in §6.6 now, but the lowering is not implemented in this
  activation;
- `anyref`/`eqref`/`i31ref`/dynamic JS value representation (jsaw's `Repr`
  closure) — needed for full dogfood compilation of jsaw's own compiler
  output, tracked as a distinct, later plan once v1 is real and tested;
- `externref`, exceptions, threads/shared memory, `memory64`, SIMD-managed
  layouts, tail calls.

The dogfood goal (compiling jsaw's own compiler with coregc) needs the
deferred items too. This plan does not claim v1 reaches that goal by itself;
it claims v1 is the correct, non-throwaway foundation for it — see §11.

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
2. for every function, classifies every value's `LowerValue` plan (§6.2) from
   its Waffle `Type`, rejecting `Type::Heap` shapes that are not a concrete
   `HeapType::Sig` reference into the inventory (this is where `anyref`,
   `i31ref`, `externref`, `eqref` are rejected by name);
3. walks every operator in every function against a **total** matcher: the
   v1 accepted set is exactly `StructNew`, `StructGet`, `StructSet`,
   `ArrayNewDefault`, `ArrayGet`, `ArraySet`, `ArrayLen`, `RefNull`,
   `RefIsNull`, `Call`, ordinary scalar/comparison/conversion operators, and
   the terminators `Br`/`CondBr`/`Return`. Anything else (including
   `CallIndirect`/`CallRef`/`RefFunc`/`Select`/`ReturnCall*`/`Unreachable`
   used as a real path/`UB`) is rejected with the operator name, function
   index, and value index;
4. for every `Call` target, confirms the callee is in the same source module
   and its own signature passes the same classification — a call to an
   unsupported/unclassifiable callee is rejected before any output exists,
   not discovered mid-lowering;
5. only after all functions pass steps 2-4 does lowering emit anything.

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
     a. if the instruction is a checkpoint instruction (`StructNew`,
        `ArrayNewDefault`, `Call`): `checkpoint_live[instr] = live ∪
        fatref_operands(instr)` — operands are included *before* removing
        the instruction's own result, because they must survive exactly the
        runtime call this instruction lowers to (they are consumed by
        payload stores / passed as call arguments only *after* that call
        returns; see the worked example below);
     b. if the instruction has a `FatRef` result, remove it from `live`
        (nothing before this point in program order needs to protect a
        value that does not exist yet);
     c. add every `FatRef` operand of the instruction to `live` (protect
        values needed to construct this instruction's inputs, propagating
        the requirement earlier in program order).
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
write a dangling pair into `%r`. Because step 3a unions in `fatref_operands`
before the def/use update, `%a` and `%b` are guaranteed to already have a
`root_store` from their own definition sites (step 6), so they are valid
roots at the moment `checkpoint` runs. The same reasoning is why `Call`
arguments are included identically — a call's checkpoint precedes the actual
`Operator::Call`, and the flattened fat-ref arguments must survive it to be
passed once the callee is actually invoked.

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

### 6.6 Struct/array operator lowering (unchanged in substance from the prior draft, now grounded in the above)

| Source op | Core lowering |
| --- | --- |
| `struct.new $T(...)` | checkpoint(frame); `alloc(type_id(T), fixed_bytes(T))`; typed store per field at its descriptor offset (`ManagedRef` fields store both words); result is the fat pair |
| `struct.get $T.i` | `addr = validate_ref(recv.addr, recv.type)`; typed load at the field's offset; a `ManagedRef` field loads both words and the loaded pair is *not* re-validated at load time (validated lazily the next time it is dereferenced/scanned — validating on every load as well as every dereference would be redundant, not incorrect; the plan chooses load-time-cheap/dereference-time-checked) |
| `struct.set $T.i` | `validate_ref(recv)`; typed store (both words for `ManagedRef`) |
| `array.new_default $A(len)` | checkpoint(frame); checked `4 + len*stride` (traps on overflow); `alloc`; store length at offset 0; `zero_bytes(addr+4, len*stride)` |
| `array.len` | `validate_ref`; load offset 0 |
| `array.get $A[i]` | `validate_ref`; `array_bounds(addr, type, i)`; typed load at `4 + i*stride` |
| `array.set $A[i] = v` | `validate_ref`; `array_bounds`; typed store at `4 + i*stride` |
| `ref.null $T` | constant fat pair `(0, 0)` — no runtime call |
| `ref.is_null` | `addr == 0` (the invariant guarantees `type_id == 0` iff `addr == 0` for any value that ever passed validation; constructing a value that violates this outside generated code is impossible in the accepted surface) |
| `call f(...)` | checkpoint(frame) with `checkpoint_live` per §6.4; flatten args; `Operator::Call { function_index: <lowered f> }`; flatten/re-pair the (possibly multi-value) result |

No generic opcode interpreter exists; each row is a fixed generator, matching
the existing `coregc_array.rs`/`coregc_lower.rs` style but generalized to
work inside the CFG/liveness machinery instead of one hand-matched function
shape.

### 6.7 Function objects and `call_ref` — pinned design, deferred implementation

This is the one v1-target item not implemented in this pass (§0, §1). The
design is pinned now so implementing it later cannot require reopening the
descriptor or header format:

- add descriptor `kind == 3` (`FunctionObject`) with a fixed payload layout
  `{ table_index: i32, arity: i32, tag: i32, env: GcRef }` (16 bytes, 8-byte
  aligned) — this fits the existing struct-descriptor encoding with `kind`
  extended from 2 values to 3 and needs no header/descriptor byte-format
  change, only a new `CoreGcTypeKind::FunctionObject` inventory variant and
  one more descriptor `kind` tag value;
- the module gets one generated `funcref` table (ordinary core Wasm — no
  reference-types-beyond-MVP feature is required for a `funcref`
  table + `call_indirect`, confirmed against `portal_pc_waffle`'s
  `TableData`/`Operator::CallIndirect`) whose entries are generated
  call-adapters with a single normalized flattened signature per arity class;
- `RefFunc` lowers to allocating a function object (checkpointed like any
  other allocation) whose `table_index` is a compile-time constant, capturing
  the closure environment as a `GcRef` through the same field-store lowering
  as `struct.new`;
- `CallRef`/indirect calls lower to: `validate_ref`; arity/tag check against
  the function object's descriptor fields; `Operator::CallIndirect` through
  the generated table; identical checkpoint/liveness treatment to `Call`
  (§6.4 already generalizes to "any instruction that may call into code that
  may allocate," which a `call_indirect` through a validated function object
  is, structurally, so no new liveness rule is needed — only the operator
  lowering itself).

No code for this section is written in this pass. It is here so v2 is an
addition, not a redesign.

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
exported/wired until Stage 5 passes its full acceptance suite (§9) — earlier
stages export their pieces only as `pub(crate)` building blocks, so there is
no window where a partially-capable `emit_coregc` is reachable by a caller.

1. **Legacy removal + descriptor `FunctionObject` reservation** (§7). Compiles,
   existing `coregc`/`coregc_layout` unit tests still pass.
2. **`coregc_runtime.rs`**: header v2 semantics (§3.2), first-fit allocator
   with free-list reuse (§3.4), validator, array-bounds check, worklist
   (§3.5), generic descriptor-driven `mark_ref`/scan dispatch, `sweep`,
   `collect` (§3.6), `zero_bytes`. Tested standalone by direct Waffle
   fixtures that call these `Func`s exactly like the existing
   `coregc_emit.rs` tests do (hand-built modules calling `__coregc_alloc`,
   `__coregc_root_store`, `__coregc_collect` etc. through Wasmtime) — this
   proves retention/reclamation/reuse/worklist-exhaustion *before* the
   general lowerer exists, isolating runtime bugs from lowering bugs.
3. **`coregc_roots.rs`**: moved from `coregc_phase3.rs`, renamed exports,
   otherwise unchanged (already tested).
4. **`coregc_lower.rs`**: value plan, CFG/value flattening (§6.2-6.3),
   liveness/checkpoint algorithm (§6.4), operator lowering (§6.6) for
   straight-line and branching single functions with no calls. Tested with
   hand-built multi-block fixtures (loops constructing/traversing a linked
   list, wide/deep graphs, cycles, self-cycles) under forced collection
   (`collect_threshold_bytes = 0`), asserting both retention of reachable
   graphs and reclamation of unreachable ones.
5. **Direct calls**: extend Stage 4's liveness (already generalized to treat
   `Call` as a checkpoint instruction, §6.4) to actually lower `Operator::Call`
   with the flattened multi-function ABI (§6.3 item 1, §6.4 item 4 of §6.1).
   Tested with a caller/callee pair where the callee allocates and the caller
   holds a live reference across the call under forced collection.
6. **`coregc_emit.rs` orchestrator + `emit_coregc` activation**: wires
   Stages 1-5 together behind the single public entry point (§5), runs the
   full acceptance suite (§9), and is the only stage that changes `lib.rs`'s
   public exports.

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
  new/get/set/len and direct calls, branches carrying fat refs, forced
  collection at every checkpoint (`collect_threshold_bytes = 0`) proves a
  local managed value used after later allocations/calls is still correct;
  the debug root-verifier (§6.4 item 7) runs and passes on every fixture; a
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
jsaw's own compiler through coregc) needs, beyond this plan: the full jsaw
`Repr` closure (boxed Number/Boolean/BigInt/String, property tries, dynamic
arrays, typed arrays, DataView, module contexts), `anyref`/dynamic dispatch,
and the function-object/`call_ref` work pinned in §6.7. This plan
deliberately stops short of those so that v1 is small enough to build
correctly and prove correct in one pass. The module boundaries in §4 and the
descriptor `kind` reservation in §6.7 exist specifically so that work is
additive on top of this plan rather than a second rewrite of it — the same
property §0 demanded of this plan's own internal stages, applied one level up
to the plan-of-plans.

## 12. Explicit non-goals / later work

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
