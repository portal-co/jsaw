# Plan: atomic CoreGC collector and lowering activation

**Status:** proposed for review. **Implementation rule:** this is one atomic
backend change. Development may use temporary local checkpoints, but no
intermediate change may expose a general `coregc` artifact, route user code to
CoreGC, or claim Phase 2/3 completion. The merged implementation is one feature
commit/PR whose acceptance suite proves all listed requirements together.

This plan completes the currently missing correctness unit from
`plan-wasmgc-linear-memory-fallback.md`:

1. descriptor-driven scanning of managed fields and arrays;
2. an iterative linear-memory mark worklist;
3. precise root traversal;
4. mark/sweep plus safe allocator reuse;
5. complete fat-reference lowering through the supported Waffle SSA closure;
6. liveness-derived shadow frames and checkpoints; and
7. tests that prove both retention and reclamation under generated user code.

It explicitly does **not** activate Phase 4 JS representation closure,
function-object dispatch, dynamic `anyref`/`i31ref`, imports, or arbitrary
native WasmGC operators. Those remain named fail-closed diagnostics.

## 1. Why this must be atomic

The current implementation contains useful, independently tested scaffolding:

- deterministic aggregate inventory and layouts (`coregc.rs`,
  `coregc_layout.rs`);
- a core-only runtime artifact, descriptor bytes, typed headers, allocation,
  growth, fat-reference validation, and scalar struct/array proof lowerers
  (`coregc_emit.rs`, `coregc_lower.rs`, `coregc_array.rs`);
- header marking and a root-frame ABI (`coregc_emit.rs`,
  `coregc_phase3.rs`).

They do **not** form an executable managed backend. In particular, the scalar
proof lowerers retain aggregate values as raw addresses, the runtime cannot
scan descriptor-declared edges, and generated user functions do not push/spill
shadow frames. Enabling sweep in that state can free a live object held in a
local or nested object field. Enabling generalized lowering without an exact
root protocol can create use-after-free bugs that ordinary core Wasm validation
will not detect.

Therefore the seam is deliberately a single public operation:

```rust
pub fn emit_coregc(
    source: &portal_pc_waffle::Module<'_>,
    options: &CoreGcOptions,
) -> Result<CoreGcArtifact, CoreGcError>;
```

`emit_coregc` is added and exposed **only** with this atomic change. Existing
`emit_runtime_skeleton`, `emit_scalar_struct_subset`, and
`emit_scalar_array_subset` become internal proof/test facilities; they are not
the backend selected by a caller.

The interface has one all-or-nothing result: either a validated core-Wasm
artifact whose compiler/runtime/root contracts match, or a fail-closed error
naming the first unsupported source operation/type/control shape. Callers do
not coordinate scanners, worklists, root slots, collection, or allocator
retries themselves. This is the deep-module seam: all correctness machinery
lives behind `emit_coregc`.

## 2. Frozen execution profile

The first activated profile is named **`coregc-multivalue-v1`**:

- core Wasm with multi-value, mutable globals, and bulk memory;
- one non-shared, 32-bit linear memory;
- single-threaded, stop-the-world, non-moving mark-and-sweep;
- no `memory64`, GC/reference types, typed function references, tail calls,
  exceptions, SIMD-specific managed layouts, or host `externref` values;
- a logical managed reference is exactly two core-Wasm `i32` values:
  `(address, actual_type_id)`;
- `(0, 0)` is the only null reference; partial null pairs trap;
- every managed edge and root contains both words; no raw address is emitted as
  a managed value in a function ABI, branch edge, call, return, field, or array
  element.

The activated source subset is deliberately smaller than all jsaw output:

| Accepted now | Rejected now, with diagnostic |
| --- | --- |
| concrete `struct`/`array` signatures with scalar and concrete managed-reference storage | `anyref`, `eqref`, `structref`, `arrayref`, `i31ref`, `externref`, exception refs, shared types |
| `struct.new`, `struct.get`, `struct.set`, `array.new_default`, `array.new_fixed`, `array.get`, `array.set`, `array.len` | data/element-segment array constructors, bulk array operations, casts/tests/equality until their subtype relation is emitted |
| scalar Wasm arithmetic/comparison/conversion operations already copyable to core Wasm | calls, indirect calls, imported functions, globals, tables, tail-call terminators in the first atomic activation |
| structured CFG with `Br`, `CondBr`, and scalar/fat-reference block arguments | unsupported terminators and non-core scalar instruction forms |

Direct calls and call checkpoints are added in the same atomic change only if
the Waffle source function ABI can be fully converted. If that conversion is
not complete at implementation time, **all `Call`/return-call operators stay
rejected**. It is unacceptable to lower calls while omitting live-root spills.

## 3. Module ownership and proposed file shape

No new public runtime knobs are scattered across `conv.rs` or callers. The
new module owns the translation and all its validation.

```text
crates/portal-jsc-waffle/src/
  coregc.rs           inventory + exact accepted-surface feature gate
  coregc_layout.rs    canonical payload/descriptors + scanner metadata
  coregc_runtime.rs   generated allocator, validator, worklist, mark, sweep
  coregc_roots.rs     root frame ABI + verifier + checkpoint helpers
  coregc_lower.rs     source Module -> core Module transform, ABI rewriting,
                       CFG remapping, liveness and checkpoint insertion
  coregc_emit.rs      thin public orchestration: emit_coregc + artifact checks
```

Existing experimental files may be renamed/moved into those modules in the
same change. There must be one owner for each invariant:

| Module | Owns | Must not own |
| --- | --- | --- |
| `coregc_layout` | deterministic physical layout, descriptor format, scan slot calculation | Waffle CFG rewriting or allocation policy |
| `coregc_runtime` | generated core-Wasm heap protocol, worklist, mark/sweep/free list | liveness or source signature interpretation |
| `coregc_roots` | exact shadow-frame memory ABI, frame validation, root iteration | descriptor scanning or source operator lowering |
| `coregc_lower` | all source-to-core ABI/value/CFG rewrite and checkpoint insertion | runtime heap policy |
| `coregc_emit` | validate → inventory → lower → append runtime → validate artifact | individual operator implementation |

The current scalar proof emitters are retained only under `#[cfg(test)]` or
renamed to clearly-private test builders. They must not be exported from
`lib.rs` after activation, preventing callers from accidentally receiving a
partially rooted artifact.

## 4. Data contracts

### 4.1 Logical value plan

`coregc_lower` has a private value-plan type:

```rust
#[derive(Clone, Debug)]
enum LowerValue {
    Scalar { values: SmallVec<[Value; 1]>, ty: Type },
    FatRef { address: Value, type_id: Value, nullable: bool },
}
```

The `FatRef` variant is the only representation of a source concrete managed
reference. It is prohibited to construct `Scalar { ty: I32 }` from a managed
source `Type::Heap`; a debug assertion plus an explicit error path enforce
this at every source value definition, block parameter, call edge, return, and
field/element storage conversion.

The lowerer has these exact transformations:

- source scalar value → one same-typed core scalar;
- source concrete `ref $T` / `ref null $T` → `(i32 address, i32 actual_type)`;
- source block parameter → one scalar parameter or two adjacent fat-ref
  parameters;
- source branch argument → the matching one/two lowered arguments;
- a source function signature → flattened core signature;
- a source public export with managed parameter/result → rejected in v1 rather
  than silently publishing an unpinned raw address.

### 4.2 Descriptor ABI v2

Bump `COREGC_DESCRIPTOR_VERSION` to 2. The encoded table is immutable runtime
data; all offsets are bytes and all type IDs are generated nonzero IDs.

```text
DescriptorHeader:
  magic, version, count, descriptor_bytes
Descriptor:
  type_id, kind, payload_alignment, fixed_payload_bytes_or_ARRAY,
  slot_count, array_stride, scan_count, scan_start
ScanEntry:
  byte_offset, expected_type_id, scan_kind
```

`scan_kind` is either `FatRef` or absent; only concrete managed-reference
storage generates a scan entry. Pointer-free slots generate none. Arrays with a
managed element generate one repeating scan entry with `byte_offset = 4`
(length occupies payload offset 0) and the descriptor's element stride.

`CoreGcDescriptorTable` exposes private, testable methods:

```rust
fn descriptor_for(id: CoreGcTypeId) -> &CoreGcPayloadLayout;
fn scan_slots(id: CoreGcTypeId) -> &[CoreGcScanSlot];
fn dynamic_scan_slot(id, payload_bytes) -> Iterator<CoreGcScanAddress>;
```

No runtime uses a Waffle `Signature` index. The emitted descriptor table and
manifest use only generated IDs.

### 4.3 Heap and allocation header ABI

Retain the v1 24-byte header, but define all fields precisely:

```text
0  flags/magic    ALLOCATED | MARK; invalid/zero means freed or corrupt
4  concrete type_id
8  payload bytes
12 next allocation payload address
16 next free header address (only when FREE)
20 block bytes (header + aligned payload; enables free-list reuse)
24 payload
```

The allocator maintains both `allocation_head` and `free_head`. Allocation:

1. validates nonzero known type ID and checked aligned requested block size;
2. first-fits a free block of adequate `block_bytes`, splitting only when the
   remainder can hold a header plus the minimum aligned payload;
3. otherwise attempts collection at a generated checkpoint, retries free list,
   grows memory if allowed, and only then advances bump;
4. initializes every header field and zeroes payload/reference storage;
5. appends/relinks allocation list exactly once.

A block is never reused before sweep clears every word required by validation.
Freed headers get a distinct poisoned non-live flag, not a valid type ID.

### 4.4 Worklist ABI

The mark worklist stores pairs in reserved runtime memory:

```text
worklist = [address: i32, actual_type_id: i32] * capacity
```

Runtime globals hold start, count, capacity, and maximum. `__coregc_mark_ref`
validates a pair; if unmarked, it sets MARK and pushes exactly one pair. It does
not recursively scan. Worklist capacity grows in page-bounded memory that is
not user-addressable; failure traps `WORKLIST_OOM` before sweep starts.

## 5. Generated runtime algorithm

### 5.1 `__coregc_collect(frame_head)`

1. Reject reentrant collection with `collecting` global.
2. Validate the root-frame linked list: aligned frame addresses, bounded slots,
   no root-region/heap overlap, and previous-frame links that monotonically
   descend.
3. Visit generated global roots, then every shadow-frame root slot.
4. For every non-null pair, call `mark_ref`.
5. Iteratively pop worklist entries. Validate the object's header and actual
   type ID. Dispatch by generated descriptor:
   - fixed struct: scan only `FatRef` slots;
   - reference array: read payload length from offset 0 and scan each
     `payload + 4 + index * stride` pair;
   - pointer-free struct/array: scan zero edges.
6. Sweep the allocation list. For each allocation:
   - corrupt header/type/payload size → `ALLOCATOR_CORRUPTION` trap;
   - marked → clear only MARK, retain allocation-list link;
   - unmarked → unlink from live allocation list, poison header, add its header
     to free list, and update counters.
7. Clear `collecting`, update thresholds/telemetry, and return.

An exception/trap in validation/worklist/scanning never starts or completes a
partial sweep. Collection either completes a full mark+sweep or traps.

### 5.2 Checkpoint behavior

A generated checkpoint is a direct call with the current frame head:

```text
__coregc_checkpoint(frame) {
  verify_frame_is_current_head(frame);
  if allocation_since_checkpoint >= threshold { __coregc_collect(); }
}
```

For deterministic tests, `collect_threshold_bytes = 0` means every allocation
and direct call executes a full collection checkpoint. It is a compiler option,
not a host callback.

## 6. Complete lowerer plan

### 6.1 Preflight

Before creating any output module, `coregc_lower` must:

1. inventory all signatures and source operations;
2. run a total operator/type/terminator support matcher;
3. reject unsupported source entities with function/value/operator detail;
4. reject a public ABI that contains managed references in v1;
5. calculate the flattened function signatures and confirm every direct call
   target has an accepted flattened ABI;
6. compute `CoreGcDescriptorTable` and verify it describes every allocation
   type and every managed storage slot mentioned by lowered operations.

This prevents producing a half-lowered module.

### 6.2 CFG and SSA rewrite

For every supported source body:

1. create each lowered block before translating values;
2. flatten block parameters with `LowerValue` arity;
3. translate instructions in source block order, resolving only previously
   available source values/block parameters;
4. flatten branch arguments and return values;
5. replace tail-call terminators with normal checkpointed call + return;
6. recompute Waffle CFG edges and call `FunctionBody::validate()`.

Supported direct aggregate operators lower as follows:

| Source op | Core lowering |
| --- | --- |
| `struct.new` | checkpoint; `alloc(type, fixed_bytes)`; typed stores at descriptor offsets; return fat pair |
| `struct.get` | validate pair/type; typed load; for managed field load both pair words and validate pair |
| `struct.set` | validate receiver; typed store; managed storage requires exact pair words |
| `array.new_default` / `array.new_fixed` | checkpoint; checked `4 + len*stride`; alloc; store length; initialize scalar/pair elements |
| `array.len` | validate pair/type; load payload offset 0 |
| `array.get` / `array.set` | validate pair/type; checked unsigned bounds; typed direct load/store at `4 + index*stride` |

No generic opcode interpreter is introduced. Unsupported native operations
remain unsupported until a direct lowering is designed and tested.

### 6.3 Liveness and root-slot allocation

The lowerer computes exact managed-reference liveness over source SSA before
flattening values:

- definitions: `struct.new`, `array.*new*`, managed field/element reads,
  managed block parameters, and flattened function parameters where accepted;
- uses: all direct aggregate/call/branch/return uses;
- edge-sensitive backward dataflow:
  `live_in = uses ∪ (live_out - defs)`;
- values live across an allocating/calling checkpoint get one deterministic
  frame slot, keyed by source `Value` (`Value`/`Ident` hygiene is irrelevant
  here; Waffle `Value` identity is the key);
- source `Value` aliases resolve before set membership; no stringified identity
  is used.

For each lowered function, allocate one frame whose slot count equals the
maximum live managed values at a checkpoint. Emit:

```text
entry:       frame = push_frame(slot_count)
before cp:   root_store(frame, slot(v), v.addr, v.type)
after cp:    root_clear(frame, slots not subsequently live)
all returns: pop_frame(frame); return flattened values
```

Structured branches preserve one function frame. The initial activation
rejects control flow that cannot prove a balanced frame lifecycle; it does not
attempt an unsound generic epilogue insertion. A core-IR verifier checks every
reachable return passes through exactly one pop and no call/allocation follows a
checkpoint without storing all live managed values.

## 7. Atomic implementation sequence

This is the order *inside one atomic implementation*, not independently
shippable phases:

1. **Replace experiments with one orchestrator.** Internalize narrow emitters;
   add `emit_coregc`; make all current unsupported source code fail before
   output generation.
2. **Upgrade descriptors and runtime data.** Implement descriptor v2 scan
   metadata, free-list/block-size header state, worklist globals, and debug
   counters.
3. **Implement complete runtime.** Generate validator, allocator retry path,
   mark worklist, scanner dispatch, sweep/free-list reuse, root-frame routines,
   checkpoint, and runtime integrity checks.
4. **Implement lowerer.** Add value plans, signature flattening, CFG rewrite,
   direct aggregate lowering, and liveness-derived root insertion as one
   transform.
5. **Wire allocation/call checkpoint protocol.** The only generated allocating
   instructions are through a helper that first spills roots/checkpoints;
   direct calls use the same helper. No bypass exists.
6. **Activate the one public function only after artifact verification.**
   Validate generated bodies, emit bytes, run core feature scan, then expose
   `CoreGcArtifact`.
7. **Delete obsolete public proof paths and activate tests.** The existing
   scalar emitters remain test fixtures only, avoiding two partially different
   runtime contracts.

## 8. Acceptance suite (single merge gate)

The atomic change is accepted only if all groups pass in one clean checkout:

### Static and code-generation checks

- descriptor v2 bytes/layout are deterministic across signature insertion
  permutations;
- scanner entries include every concrete managed field/array element and no
  scalar storage;
- all output function bodies validate;
- `wasmparser::Validator::new()` accepts output with default core features;
- encoded byte scan proves absence of GC/reference instructions/types;
- every accepted/rejected source op has a lowering/diagnostic test.

### Runtime collector tests

Run on default Wasmtime (no `wasm_gc(true)`):

- pointer-free leaf and array allocation;
- root field, root array element, self-cycle, two-cycle, wide graph, and deep
  graph;
- nullable child and deliberate partial-null/mismatched/stale/corrupt pairs;
- forced collection retains each reachable graph and reclaims each unreachable
  graph;
- mark bit is cleared after a retained object survives sweep;
- repeated allocation/collection uses a freed block and remains validator-safe;
- worklist and root-stack capacity failures trap deterministically;
- nested frames, every root slot, clear, pop, and return lifecycle;
- root verifier negative fixture: a deliberately omitted live spill must
  fail lowering with an invariant diagnostic, not produce runnable output.

### End-to-end lowering tests

Build Waffle fixtures with real `struct.new/get/set`, `array.new/get/set/len`,
branches carrying fat refs, and direct calls where implemented. With collection
forced at every checkpoint, prove a local managed value remains correct after
later allocations. Verify both correct observable values and expected traps.

### Differential harness

For the accepted fixture set only:

```text
source -> native WasmGC -> Wasmtime GC
       -> emit_coregc  -> default Wasmtime
```

Compare return values, traps, and deterministic heap statistics. The test
fails if a fixture is accepted by one path but silently omitted by the other.

## 9. Explicit non-goals of this atomic change

- Full jsaw `Repr` semantics and general JS e2e parity (Phase 4).
- `anyref`, `i31ref`, function objects, `call_ref`, casts/tests/subtyping,
  imports, globals, tables, closures, typed arrays, and DataView.
- Moving GC, incremental/concurrent collection, host pin/unpin, or memory64.
- Publishing an ABI for managed values; v1 public exports remain scalar-only.

Those omissions are enforced by preflight diagnostics. They are not hidden
fallback behavior.

## 10. Review decisions requested

Before implementation, confirm:

1. **Activation scope:** scalar-only public ABI and the listed concrete
   struct/array/direct-CFG subset are acceptable for the first activated
   `emit_coregc` target.
2. **Atomicity definition:** one merge-ready feature change, with no
   intermediate public backend activation, is the required unit.
3. **Allocator policy:** first-fit free-list reuse without coalescing is
   sufficient for this activation; coalescing/TLSF stays Phase 6.
4. **Call scope:** reject calls unless flattened ABI + liveness checkpoint
   tests land in the same change; do not broaden scope merely for nominal Phase
   3 coverage.
5. **Test policy:** default Wasmtime/core validation plus forced-collection
   differential fixtures are the blocking oracle.

Once approved, implementation will start from a clean worktree and create only
atomic feature commits that keep the public CoreGC target disabled until the
entire acceptance suite passes.
