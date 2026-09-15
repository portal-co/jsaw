# Plan: typed, pure-Wasm mark-and-sweep fallback for WasmGC

**Status:** design and research plan. This is a fallback backend for hosts that
cannot validate or execute WasmGC. It ingests the same typed WasmGC module that
jsaw already produces, inventories its GC types, and emits a **core Wasm**
module containing direct code plus a generated, typed, non-moving mark-and-sweep
runtime in linear memory.

The fallback deliberately does **not** attempt to make WasmGC itself optional
at runtime inside one binary. A WasmGC binary cannot be validated by an engine
without the GC feature. The compiler therefore emits a second artifact from the
same Waffle/WasmGC IR:

```text
JS module set
  -> portal-jsc-waffle Module (WasmGC IR, existing source of truth)
       -> normal WasmGC backend                  -> module.gc.wasm
       -> gc-fallback inventory + lowering       -> module.coregc.wasm
                                                     + type/root manifest
```

`module.coregc.wasm` uses core linear memory, integer loads/stores, tables and
ordinary calls only. The collector, allocator, root stack, type descriptors,
mark worklist, checks, and generated barriers all execute **inside the Wasm
module**. It has no required GC host import and no embedder heap callback.

## 1. Scope, goals, and non-goals

### Goals

1. Preserve jsaw's existing WasmGC semantics for the emitted feature closure
   on an engine without WasmGC: typed structs, arrays, nullable references,
   casts/tests, object identity, closures/function objects, and the existing
   JS value representation.
2. Make every managed reference a checked **fat pointer** carrying both the
   actual linear-memory payload address and the actual concrete managed type
   ID. A collector root and every object edge therefore identifies its target
   without guessing from an untyped machine word.
3. Use a precise, stop-the-world, non-moving mark-and-sweep collector with
   compiler-inserted checkpoints and a compiler-maintained shadow-root stack.
4. Retain typed direct field access where possible: a field access becomes
   address arithmetic plus a typed load/store, not a generic reflective lookup.
5. Generate the runtime and all per-module type scanning code as ordinary
   Wasm functions, globals, tables, data segments, and linear-memory regions.
6. Make unsupported source WasmGC constructs fail closed with a diagnostic
   naming the source operator/type. Never silently reinterpret a GC reference
   as an untyped `i32`.

### Non-goals

- A moving or compacting collector in the first implementation. Fat pointers
  and exact roots would permit it later, but non-moving sweep avoids rewriting
  every root/object edge and avoids interior-pointer policy in the first
  backend.
- Conservative pointer discovery. It would conflict with the required type
  identity, retain false positives, and be unsafe for arbitrary JS numbers.
- Scanning the native Wasm execution stack. Core Wasm provides no such portable
  operation; the compiler owns exact root materialization.
- Threading, shared memories, `memory64`, exceptions, host `externref`,
  WasmGC `exnref`, or arbitrary imported GC objects in phase 1.
- A universal binary patch format or a replacement for the separate HCR
  revision protocol. The fallback backend can be one target of a complete HCR
  revision, but HCR activation remains runtime-specific.
- Matching VM-managed WasmGC performance or browser DevTools object inspection.
  The native WasmGC artifact remains preferred whenever supported.

## 2. Research findings and design consequences

### 2.1 What WasmGC gives the existing backend

The normative WebAssembly type specification classifies aggregate heap types as
managed structures, arrays, and unboxed scalars; references are opaque values
whose bit pattern and size cannot be observed or stored in memories. Structures
have statically indexed heterogeneous fields, while arrays have homogeneous,
dynamically indexed elements.[^wasm-types] The GC overview likewise describes
struct/array allocation, typed field access, casts, `anyref`, `i31ref`, and
`call_ref` as low-level building blocks for source-language object models.[^gc-overview]

That is exactly what the existing Waffle module uses: `SignatureData::{Struct,
Array}`, `HeapType::Sig`, `StructNew/Get/Set`, `ArrayNew/Get/Set/Len`,
`RefTest/Cast`, `RefEq`, `RefI31`, and typed function references. The fallback
must inventory those instructions and types *before* core encoding removes the
information. It must not try to decompile a final WasmGC byte stream into a
collector after the fact.

### 2.2 Why a shadow-root stack is mandatory

A core Wasm module cannot portably inspect its own active execution stack. The
WebAssembly linear-memory root-marking proposal identifies inability to scan the
Wasm stack as the main obstacle for a linear-memory GC; it remains Phase 0, not
a baseline feature.[^root-marking] V8's WasmGC porting guide identifies the same
issue and lists shadow stacks or collection only when the stack is empty as the
current traditional-Wasm solutions.[^v8-porting]

Therefore this plan chooses **precise compiler-maintained shadow frames**, not
“collect only between host turns.” jsaw must collect during large compiler runs,
and a long Stage-B compiler invocation cannot wait for the JS/WASI host stack to
empty.

### 2.3 Prior art

This is not a novel claim that managed languages need a linear-memory GC:

- **AssemblyScript** ships an incremental runtime, a minimal runtime, and a
  bump-only stub runtime. Its own documentation says the incremental variant
  provides a shadow stack while the minimal variant requires collection when
  the Wasm stack is known to be unwound; it also publishes linear-memory RTTI
  and managed-object header layout.[^as-runtime] Its `itcms` source implements
  an incremental tri-color mark-and-sweep collector, visits globals and stack
  roots, uses per-type member visitors, and includes a write barrier.[^as-itcms]
  Its TLSF source is a two-level segregated-fit allocator with coalescing free
  blocks.[^as-tlsf]
- **Binaryen `SpillPointers`** instruments possible pointer locals around calls
  so a Boehm-style collector can see them in a C stack spill area. It is useful
  evidence that liveness-based pointer spilling is practical, but it is
  intentionally conservative: it treats address-sized locals as possible
  pointers.[^binaryen-spill]
- **Emgc** is a small research collector for Emscripten Wasm. Its README calls
  it a toy, describes mark-and-sweep over stack/global/explicit roots, and
  explains that raw-memory pointer identification is conservative and misses
  interior pointers.[^emgc] We take the opposite correctness choice: roots and
  fields have compiler-known exact types, no raw-memory guessing, and no
  interior pointers in phase 1.
- The Wasm root-marking discussion records AssemblyScript's precise shadow-stack
  strategy: generated global visitor, generated per-class member visitors,
  prologue/epilogue frame management, zeroed reference slots, and spills of
  only reference locals.[^root-marking] This is particularly close prior art
  for the checkpoint design below.

The differentiating project choice is not “a GC in Wasm.” It is a fallback
**transformation from jsaw's typed WasmGC IR** with (a) an explicit type-ID-plus-
address reference representation, (b) compiler-generated descriptors from the
same `SignatureData` inventory, (c) precise checkpoints compatible with jsaw's
control-flow lowering, and (d) no dependence on a host runtime GC API.

### 2.4 Chosen collector profile

| Decision | Phase-1 choice | Reason |
| --- | --- | --- |
| Heap | one managed region in core linear memory | pure Wasm, explicit bounds |
| Collector | stop-the-world, precise, non-moving mark-and-sweep | simplest correct root/object invariants |
| Allocation | bump frontier plus coalescing free list; grow memory as needed | small, predictable initial runtime; may adopt TLSF later |
| Mark worklist | linear-memory vector of validated fat references | avoids recursion/Wasm stack depth |
| Roots | shadow frames + generated global-root table + temporary checkpoint slots | exact, no native-stack scan |
| Pointer model | `(addr: i32, type_id: i32)` fat pair | preserves actual dynamic type at every edge |
| Type checks | descriptor parent-chain/bitset plus header cross-check | supports `ref.test`/`ref.cast` safely |
| Movement | none | no pointer rewriting in first backend |
| Threads | none | a stop-the-world protocol needs an explicit future design |

## 3. Public backend interface and target profiles

Add a new backend module, tentatively
`crates/portal-jsc-coregc-emit` (or `portal-jsc-waffle::coregc` until it earns
its own crate):

```rust
pub struct CoreGcOptions {
    pub initial_pages: u32,
    pub max_pages: Option<u32>,
    pub collect_threshold_bytes: u32,
    pub trap_on_type_mismatch: bool,
    pub export_runtime_debug: bool,
}

pub struct CoreGcArtifact {
    pub wasm: Vec<u8>,
    pub inventory: CoreGcInventory,
}

pub fn emit_coregc(
    wasmgc: &portal_pc_waffle::Module<'_>,
    options: &CoreGcOptions,
) -> Result<CoreGcArtifact, CoreGcError>;
```

`emit_coregc` consumes Waffle IR, never a serialized `.wasm` byte stream. The
ordinary `to_wasm_bytes` output and `emit_coregc` are sibling encoders.

Two core profiles are explicit in the artifact manifest:

1. **`coregc-multivalue`** — requires core Wasm, bulk memory, mutable globals,
   tables, and multi-value. Logical fat references expand to two `i32` values
   in internal signatures. This is the first implementation target because it
   keeps direct code simple and is widely supported even where WasmGC is not.
2. **`coregc-mvp-abi`** — no multi-value requirement. Internal calls lower a
   logical fat-reference result through a caller-provided return-slot pointer;
   reference arguments are still two `i32`s. This profile is a follow-up once
   the semantics are proven. It must not sneak in as a different ABI under the
   same artifact identifier.

Neither profile requires reference types or WasmGC. A function object becomes a
managed heap object containing a core-Wasm table index and an environment fat
pointer; invocation uses `call_indirect` over a generated homogeneous table.
This replaces WasmGC `ref.func`/`call_ref`, which are not available in the
strict core profile.

## 4. Type inventory and lowering contract

### 4.1 Inventory happens before code emission

`CoreGcInventory::build` walks the Waffle module in deterministic signature
index order and records every managed signature and every operation that
mentions it. It produces a self-contained table rather than relying on Waffle
entity indexes at runtime.

```rust
pub struct CoreGcInventory {
    pub managed_types: Vec<ManagedType>,       // sorted by assigned runtime ID
    pub signature_map: BTreeMap<Signature, TypeId>,
    pub function_abis: BTreeMap<Func, CoreFuncAbi>,
    pub global_roots: Vec<GlobalRoot>,
    pub required_features: CoreFeatureSet,
}

pub struct ManagedType {
    pub id: TypeId,
    pub kind: ManagedKind,                     // Struct | Array | FunctionObject
    pub payload_alignment: u32,
    pub fixed_payload_bytes: Option<u32>,
    pub fields: Vec<FieldLayout>,              // structs only
    pub element: Option<ElementLayout>,        // arrays only
    pub parent_ids: Vec<TypeId>,               // exact/subtype closure
    pub scan_kind: ScanKind,                   // NoRefs | FixedOffsets | ArrayStride
}

pub enum FieldLayout {
    I32 { offset: u32 }, I64 { offset: u32 }, F32 { offset: u32 }, F64 { offset: u32 },
    Ref { offset: u32, expected: ExpectedType },
    FatRef { offset: u32, expected: ExpectedType }, // two i32 words
}
```

The generated Wasm contains an encoded copy of this table in a read-only data
segment. Runtime descriptor indexes are **new deterministic IDs**; raw Waffle
`Signature` indexes are never exposed in a fat pointer, output ABI, cache key,
or HCR delta. The inventory's canonical input includes:

- signature kind and fields/elements;
- mutability and packed `i8`/`i16` storage;
- nullability and abstract/concrete heap type;
- declared subtype relationship if present in the source IR;
- the core fallback profile and compiler/runtime schema version.

A `TypeId` is a dense nonzero `u32`, assigned after sorting the canonical
layout key. `0` remains reserved for null. The descriptor table includes an
`expected` relation as either a direct ID, a parent-chain walk, or (if profiling
justifies it) a generated subtype bitmap. Phase 1 uses parent-chain walks;
types are few and casts are much rarer than direct field access.

### 4.2 Accepted and rejected WasmGC surface

| Waffle/WasmGC construct | CoreGC lowering |
| --- | --- |
| `SignatureData::Struct` | managed fixed-size payload + fixed descriptor offsets |
| `SignatureData::Array` | managed variable-size payload + element descriptor/stride |
| `StructNew`, `StructNewDefault` | `gc_alloc(type_id, payload_bytes)` + direct initialized stores |
| `StructGet/Set` | checked fat-ref validation + direct typed load/store |
| `ArrayNew*` | checked length arithmetic + `gc_alloc` + element initialization/copy |
| `ArrayGet/Set/Len/Copy/Fill` | checked fat ref, bounds check, direct linear-memory operation |
| `RefNull` | canonical pair `{ 0, 0 }` |
| `RefTest`, `RefCast` | null rule plus descriptor subtype predicate |
| `RefEq` on objects/arrays | address equality after validation; IDs must agree for one object |
| `RefI31`, `I31Get*` | fallback tagged-value helper, detailed in §4.5 |
| `ref.func`, `call_ref` | managed function-object `{table_index, env_fat_ref, ...}` + core table/call_indirect |
| primitive i32/i64/f32/f64 | unchanged direct core values |

Reject, with an error naming the type/operator and recommending native WasmGC:

- `externref`, host types, host-GC references, or type imports whose concrete
  layout cannot be inventoried;
- `exnref`, exceptions/tags, shared GC types/atomic GC fields, and threads;
- `memory64` in phase 1;
- GC operations that depend on an unmodeled recursive type relation;
- any unrecognized Waffle GC operator. `#[non_exhaustive]` matches must have a
  fail-closed default.

This rejection list is a product constraint, not an excuse to erase a feature.
A native WasmGC artifact remains available; `coregc` metadata says exactly why
the fallback cannot be emitted.

### 4.3 Layout rules

All payload fields use little-endian core-Wasm loads/stores and naturally
aligned offsets. The descriptor compiler computes offsets with explicit
alignment, never Rust host layout. The initial rules are:

| Source storage | Payload storage | Scan action |
| --- | --- | --- |
| `i8`, `i16` | 1/2 bytes | none |
| `i32`, `f32` | 4 bytes | none |
| `i64`, `f64` | 8 bytes | none |
| nullable/non-null managed `ref` | 8 bytes: `addr, type_id` | validate/mark pair |
| function reference | 8 bytes: managed function-object pair | validate/mark pair |
| `anyref` | 8-byte `GcValue` pair/tagged form | dynamic scan based on tag |

Structures use fixed offsets. Arrays use one element layout and a length in the
allocation header; `payload_bytes = length * stride` must be checked for `u32`
overflow before allocation. Packed source fields retain source sign-extension
behavior at get sites, so an `i8` load does not accidentally become an unsigned
JS number.

A type descriptor's scanner is generated straight-line code when it has a small
fixed set of reference offsets, not a general reflective loop. For arrays it is
one loop over `length`, using the recorded stride and element scan action.
Pointer-free descriptors skip scanning entirely. This follows the same broad
pattern as AssemblyScript's generated per-class member visitors, but with exact
fat-reference slots rather than a raw pointer-sized visitor API.[^root-marking]

### 4.4 Fat managed references

The logical managed-reference representation is:

```text
GcRef = { addr: i32, actual_type: i32 }

null = { 0, 0 }
```

`addr` is the address of the **payload**, not the header. The header begins a
fixed `HEADER_BYTES` before it. `actual_type` is the concrete runtime `TypeId`
read from the allocation header at creation and carried at every boundary.
Both words are present in:

- reference locals and parameters after core ABI lowering;
- shadow-root slots;
- structure fields and reference-element arrays;
- globals recorded as roots;
- object/function/closure environments;
- return slots in the MVP ABI profile;
- mark worklist entries and debugging exports.

The duplication is intentional. Before dereference or marking, generated code
checks all of the following:

1. `(addr == 0) == (actual_type == 0)`; a partial-null pair traps;
2. `addr` is aligned, lies within the managed heap, and does not underflow the
   header address;
3. the header magic/allocation-state is live;
4. header `type_id == actual_type`;
5. for an access/cast expecting `T`, `actual_type` is `T` or a registered
   subtype as required by the WasmGC operation.

The header cross-check makes a stale, forged, or mismatched pair a deterministic
trap rather than an arbitrary linear-memory access. It also catches use after
sweep in debug mode. Release builds retain checks required for memory safety and
may omit only redundant diagnostics after an optimizer proves a pair came from a
validated allocation.

A logical reference is **not** packed into one `i64` in phase 1. Pair lowering
keeps the representation obvious in Wasm, avoids signedness/endianness
ambiguity, works in 32-bit engines, and lets `coregc-mvp-abi` lower it through
ordinary two-word slots. A future internal `i64` optimization may exist only if
it preserves the public/profile ABI and exact null invariant.

### 4.5 JS values, `anyref`, and `i31ref`

The current jsaw WasmGC representation uses nullable `anyref` as a common
boundary and uses `i31` for its null/small-scalar conventions. Core Wasm has no
`anyref` or `i31ref`, so the fallback introduces a separate `GcValue` ABI:

```text
GcValue = { tag: i32, lo: i32, hi: i32 }

TAG_NULL       => lo = 0, hi = 0
TAG_BOOL       => lo = 0|1, hi = 0
TAG_I31        => lo = signed payload, hi = 0
TAG_GCREF      => lo = payload address, hi = concrete TypeId
TAG_F64_BOXED  => lo = GcRef.addr, hi = GcRef.type
TAG_STRING/... => lo = GcRef.addr, hi = GcRef.type
```

This is intentionally distinct from a `GcRef`: code which expects an object
cannot mistake a scalar for an address. Direct typed paths keep unboxed numeric
values; only the existing JS dynamic-value boundary uses `GcValue`. The fallback
must preserve observable distinctions currently carried by boxed Number,
Boolean, BigInt, String, object, function, typed-array, and null values.

The WasmGC proposal itself notes that uniform dynamic representations normally
use pointer tagging and that `i31ref` provides a bounded unboxed scalar option.
[^gc-overview] We preserve the *semantic role*, not the engine's opaque
reference bit pattern.

### 4.6 Function objects and calls

Function references cannot be represented as heap addresses to executable code.
The fallback allocates a managed function object whose descriptor includes:

```text
payload[0]  code_index: i32             // generated core table slot
payload[4]  arity: i32
payload[8]  tag: i32                    // jsaw provenance/identity tag
payload[12] flags: i32                  // arrow / metadata
payload[16] captured_this: GcValue
payload[28] context: GcRef
```

The exact layout is versioned in the generated type descriptor. The `code_index`
selects a core-Wasm table entry with a normalized adapter signature. The call
lowering validates the function object's fat reference, checks arity and its
function-object descriptor, retrieves the table index, and performs
`call_indirect`. The existing typed-function-reference optimization becomes a
static table-index call in this profile.

No raw `funcref` enters the managed heap; tables hold code, the heap holds only
integer indexes and managed environments. This makes collection, serialization,
and checkpointing uniform.

## 5. Pure-Wasm runtime layout

### 5.1 Linear memory partition

The fallback owns one non-shared core memory. The module emits a memory with an
initial page count sufficient for static data, descriptor tables, root-stack
bootstrap, allocator metadata, and the configured initial heap. It honors an
optional maximum; failure to grow is a deterministic allocation trap.

```text
0                                                    memory.size * 64 KiB
| static data | descriptor/data tables | runtime metadata | shadow roots | heap |
^             ^                        ^                ^              ^
0             static_end               runtime_base     heap_base      growable
```

- **Static data**: strings/data segments and generated immutable tables.
- **Descriptor/data tables**: fixed arrays of type descriptors, function-table
  metadata, global-root locations, and debug names when enabled.
- **Runtime metadata**: allocator heads, mark worklist pointers/capacity,
  collection counters, threshold, and root-stack head.
- **Shadow roots**: dynamically grown downward or separately allocated managed
  frames; phase 1 uses a fixed linear stack region that grows toward the heap
  and checks collision.
- **Heap**: managed allocations and allocator free blocks, grown only at page
  boundaries.

The emitter reserves all runtime regions before user allocation. User-generated
`memory.grow` is not supported in phase 1; if the source IR has explicit linear
memory behavior, it must use a separate, non-GC memory or fail closed. This
prevents a user write from corrupting allocator/root metadata.

### 5.2 Allocation header

Every managed allocation has this fixed 32-bit little-endian header, aligned to
8 bytes:

```text
offset  field
0       magic_and_flags: u32  // ALLOCATED, MARK, FREE, ARRAY, FINALIZABLE(reserved)
4       type_id: u32          // concrete TypeId; never zero for a live object
8       payload_bytes: u32    // excludes header; arrays include all elements
12      next_allocation: u32  // intrusive list for sweep; payload address or 0
16      next_free: u32        // allocator free list; valid only if FREE
20      reserved: u32         // generation/debug/checksum future use
24      payload begins
```

`HEADER_BYTES = 24` in v1. The generated descriptor determines whether bytes at
payload offsets are reference pairs. A mark bit belongs in the header, never in
the low bits of an address: references must retain a full concrete type ID and
may point to any aligned payload address.

The all-allocation list is a simple intrusive list. Sweep walks it once; a freed
block is returned to the allocator list and coalesced with adjacent free blocks
when boundary metadata is available. Phase 1 may use first-fit segregated free
lists rather than copying AssemblyScript's full TLSF implementation. The
allocator interface is deliberately isolated:

```text
gc_alloc(type_id, payload_bytes) -> GcRef
gc_free_block(header_addr)
gc_grow(required_bytes) -> success | trap
```

Start with a small coalescing first-fit allocator and benchmark it. Adopt TLSF
only if allocation fragmentation or scan time demands it; AssemblyScript's TLSF
is strong prior art but far larger than the initial collector needs.[^as-tlsf]

### 5.3 Runtime functions

All functions are generated/linked as internal core Wasm functions:

```text
__gc_init()
__gc_alloc(type_id, payload_bytes) -> (addr, type_id)
__gc_checkpoint(frame_ptr)                 // may collect
__gc_collect(frame_ptr)                    // full stop-the-world collection
__gc_mark_ref(addr, type_id)
__gc_scan_object(header_addr)
__gc_sweep()
__gc_push_frame(slot_count) -> frame_ptr
__gc_pop_frame(frame_ptr)
__gc_root_store(frame_ptr, slot, addr, type_id)
__gc_root_clear(frame_ptr, slot)
__gc_ref_validate(addr, type_id, expected_type) -> addr
__gc_is_subtype(actual_type, expected_type) -> i32
__gc_trap(code)
```

They are ordinary generated Wasm code, not imported host functions. Export
`__coregc_collect`, `__coregc_heap_stats`, and `__coregc_type_table` only under
`export_runtime_debug`; production artifacts expose only ordinary jsaw exports.

### 5.4 Mark algorithm

At a collection checkpoint:

1. Set `collecting = 1`; phase 1 is single-threaded, so no mutator races.
2. Mark all generated global roots, then walk the linked shadow-frame chain.
3. For each non-null root pair, validate it and call `__gc_mark_ref`.
4. `__gc_mark_ref` checks the header mark bit. If clear, sets it and pushes the
   payload/header onto the explicit linear-memory worklist.
5. Pop worklist entries until empty. Look up the descriptor by header type ID.
   Execute its generated scanner; each child fat-reference pair is validated
   and marked. Pointer-free fields are never inspected as addresses.
6. Sweep the allocation list. A marked live allocation has its mark bit cleared
   for the next cycle. An unmarked live allocation is unlinked/freed. Corrupt
   headers trap rather than causing allocator damage.
7. Set `collecting = 0`, update threshold/telemetry, and resume the mutator.

Marking is iterative, not recursive: a deeply nested JS array/object graph must
not overflow the Wasm value stack. Worklist growth uses reserved runtime memory;
if it cannot grow, the collector traps with a distinct `GC_WORKLIST_OOM`, rather
than treating incomplete marking as successful.

The collector is **precise**. It scans only descriptor-declared reference slots
and generated root slots. A JS f64 whose bits resemble an address is irrelevant;
it cannot retain an object accidentally. This directly avoids Emgc's documented
conservative false-positive tradeoff.[^emgc]

### 5.5 Sweep and finalization policy

Phase 1 has no user finalizers, weak references, ephemerons, or resurrection.
The current jsaw feature closure has no source-visible finalizer protocol; adding
one changes language/host semantics and requires an explicit plan.

Sweep may coalesce immediately. Object identity is address identity for the
lifetime of a live allocation because the collector never moves it. After a
sweep, stale references are invalid and fail header validation; user JS cannot
legitimately observe one without a compiler/runtime bug because every live
reference must have been rooted or reachable.

## 6. Root discipline and checkpoints

### 6.1 Shadow frame representation

The collector cannot see Wasm locals, operand-stack values, or call arguments
unless generated code materializes them. Every lowered function that can hold a
managed reference gets a shadow frame:

```text
Frame header:
  previous_frame: u32
  slot_count: u32
  flags: u32
  reserved: u32
Slots, 8 bytes each:
  slot[i].addr: u32
  slot[i].type_id: u32
```

The compiler's liveness analysis allocates a slot only for logical `GcRef` or
`GcValue` reference arms that are live across a possible collection checkpoint.
Numeric-only locals require no slot. `GcValue` roots use a tagged slot layout or
a pair of slots so the marker can ignore scalar tags.

At function entry, generated code pushes a zeroed frame. Before a return,
structured branch that exits the function, trap path, or tail-call transfer, it
clears/pops the frame exactly once. `try`/exception lowering is out of scope for
phase 1; it must not bypass cleanup silently.

This is a compiler-level shadow stack, not the host's C/Wasm stack. It uses
precise type pairs, unlike a conservative “scan all aligned words” scheme.
AssemblyScript's documented/generated shadow stack is the closest operational
prior art, including zeroing reference slots and spilling live refs.[^root-marking]

### 6.2 Checkpoints (safepoints)

A **checkpoint** is the only point at which `__gc_collect` may run. The emitter
must insert one where all live references have valid shadow slots and no
unrooted managed temporary exists only on the Wasm operand stack.

Mandatory checkpoint sites:

1. immediately before any `__gc_alloc` which may need collection;
2. immediately before a generated call that may allocate, including JS dynamic
   calls, property helpers, constructor adapters, array/string helpers, and
   indirect function calls;
3. loop back edges in lowering regions that can allocate without making an
   ordinary call, with a budget counter to avoid a collection every iteration;
4. explicit debug `__coregc_collect` calls;
5. memory-growth/allocator slow paths.

A checkpoint expansion is conceptually:

```text
materialize all live GcRef/GcValue-reference arms into frame slots
__gc_checkpoint(frame)
reload values only where the lowered SSA continuation needs them
```

Because the collector is non-moving, reload is not needed for address changes;
it is still useful to keep SSA/lowering ownership clear and becomes mandatory if
compaction is introduced later. The root spill occurs **after** evaluating call
arguments into temporary locals and **before** invoking the potentially
allocating call. This closes the classic unsafe interval where a returned or
argument reference is neither in the caller frame nor safely owned by the
callee.

Binaryen's `SpillPointers` pass demonstrates call-site liveness-based spilling,
but its conservative “address-sized local” policy is insufficient here. The
fallback emitter has typed Waffle values and must spill only actual GC-reference
arms, including both fat-pointer words.[^binaryen-spill]

### 6.3 Generated root inventory

`CoreGcInventory` emits three root sources:

- **shadow frames**: active function locals/temporaries;
- **managed globals**: generated function `__coregc_visit_globals` contains
  direct loads of each known global fat reference / dynamic-value reference arm;
- **pinned host roots**: optional explicit handles for an embedder that retains
  a value between exported calls. `pin`/`unpin` is an extension API, not an
  implicit host scan.

Static data is not blindly scanned. Any static managed object is represented as
a registered immutable allocation/root descriptor or initialized by
`__gc_init`; arbitrary data bytes cannot accidentally become a root.

### 6.4 Checkpoint correctness invariants

At every possible collection:

1. every live managed reference is in exactly one reachable root slot or a
   descriptor-scanned managed object;
2. every root/object reference has `{0,0}` or a valid matching pair;
3. no root slot contains an uninitialized non-null type ID;
4. no allocation occurs between creating a live managed value and storing it in
   a root/managed field, unless the creating callee itself holds it live;
5. root frame push/pop is balanced on all structured exits;
6. type descriptor scanning is total for every allocated type ID.

A debug verifier runs over generated core IR before serialization: it identifies
all coregc allocation/checkpoint calls and proves an emitted root-frame state is
present at each. It cannot prove arbitrary manually injected Wasm correct; coregc
artifacts do not admit arbitrary post-lowering transformations without rerunning
this verifier.

## 7. Lowering WasmGC operations to direct core code

### 7.1 Transformation order

The fallback is a typed backend pass over Waffle IR:

```text
validate source Module's supported GC closure
        -> CoreGcInventory
        -> assign TypeId/descriptors/function-table ABIs
        -> lower Waffle GC values to core scalar/fat-pair value plans
        -> insert root slots/checkpoints by SSA liveness
        -> append generated runtime functions/data
        -> validate resulting core-Wasm feature set
        -> encode module.coregc.wasm
```

Inventory must run before lowering because a descriptor scanner may be needed
for a type whose allocation occurs only in a nested function or helper body.
The normal WasmGC emitter remains unchanged; no fallback rule should weaken its
native GC typing.

### 7.2 Struct and array examples

For a `struct.new $Point(x: f64, next: ref null $Node)` with runtime TypeId 12:

```text
checkpoint(frame)
(addr, ty) = __gc_alloc(12, 16)
store_f64(addr + 0, x)
store_i32(addr + 8, next.addr)
store_i32(addr + 12, next.type_id)
result = {addr, ty}
```

For `struct.get $Point.next p`:

```text
p.addr = __gc_ref_validate(p.addr, p.type_id, TYPE_POINT)
child = { load_i32(p.addr + 8), load_i32(p.addr + 12) }
validate_pair_or_trap(child)
```

For `array.get $A a i` with reference elements:

```text
a.addr = __gc_ref_validate(a.addr, a.type_id, TYPE_A)
length = load_i32(header(a.addr) + ARRAY_LENGTH_OFFSET)
if i >= length: trap OOB
base = a.addr + ARRAY_PAYLOAD_OFFSET + i * 8
child = { load_i32(base), load_i32(base + 4) }
validate_pair_or_trap(child)
```

The compiler can omit repeated validation only within a dominating checked
region and only if no untrusted/raw pointer conversion is possible. It must not
remove the pair/header cross-check merely because a source WasmGC reference was
statically concrete: in core linear memory, data corruption must still trap
rather than become arbitrary access.

### 7.3 Casts, nullability, and subtyping

- `ref.test (ref null T) v`: true for null; otherwise validate pair and call
  `__gc_is_subtype(v.actual_type, T)`.
- `ref.test (ref T) v`: false for null; otherwise same subtype check.
- `ref.cast`: execute the corresponding test, trap `COREGC_BAD_CAST` on false,
  then retain the original actual type ID. A cast does **not** rewrite the fat
  pointer's concrete type to `T`; doing so would violate the actual-type
  invariant and lose subtype information needed by later checks/scanning.
- `ref.eq`: both null pairs compare equal; non-null pairs compare `addr` only
  after validation. Equal addresses with unequal actual IDs are a corruption
  trap, not false.

The descriptor relation must match Waffle's accepted subtype relation exactly.
The native Wasm type specification distinguishes concrete/abstract heap types
and nullability; the fallback translates only relations it inventories and
rejects the rest.[^wasm-types]

### 7.4 Function calls and checkpoints

Each source WasmGC function becomes a core function with a normalized ABI. In
`coregc-multivalue`, an internal function whose source signature is

```text
(context: ref Object, this: anyref, arguments: ref Arguments) -> anyref
```

becomes a core signature whose logical references/value representations expand
to their scalar components. Calls are direct when their source function is
known, or table-dispatched for function objects. Before either call, the caller
spills live roots and runs the checkpoint. The callee pushes its own frame before
any allocation.

Tail calls require special treatment: a tail edge cannot leave the caller's
shadow frame live after handing control to the callee. Phase 1 lowers all
eligible tail calls to a checkpointed normal call + frame pop + return, even if
native WasmGC used `return_call`. Reintroduce a core tail-call optimization only
when its generated root-frame transfer has a proven ownership protocol.

### 7.5 Helpers and directness

The fallback must not turn every source operation into one enormous reflective
`gc_op(opcode, ...)` interpreter. It emits:

- direct typed loads/stores for concrete fields/elements;
- one scanner function per descriptor (or a compact descriptor interpreter only
  for generated large/regular descriptor sets);
- small shared helpers for allocation, validation, subtype checks, bounds,
  root frames, and dynamic JS-value dispatch;
- direct Wasm function calls where the current converter proves a function;
- a generated table only for true function-object/indirect calls.

This mirrors the WasmGC design motivation: aggregate type information should
allow cheap direct accesses rather than a generic runtime lookup.[^gc-overview]

## 8. Type/GC runtime manifest and debugging

Each `module.coregc.wasm` has a companion JSON/CBOR manifest, versioned
independently from the source compilation manifest:

```json
{
  "schema": "jsaw.coregc.v1",
  "profile": "coregc-multivalue",
  "pointerWidth": 32,
  "collector": "precise-nonmoving-mark-sweep",
  "headerBytes": 24,
  "fatReference": {"address": "i32", "actualType": "u32", "null": [0, 0]},
  "types": [
    {"id": 12, "kind": "struct", "payloadBytes": 16,
     "referenceOffsets": [8], "parents": [12]}
  ],
  "checkpointProtocol": "shadow-frame-v1",
  "features": ["bulk-memory", "mutable-globals", "table", "multivalue"]
}
```

The manifest supports diagnostics and backend testing; the Wasm module's runtime
does not parse JSON. A debug artifact may export:

- `__coregc_collect()` — force a complete collection only at an exported-call
  boundary or an instrumented internal checkpoint;
- `__coregc_heap_stats()` — allocation/live/free/collection counters;
- `__coregc_validate_ref(addr, type)` — test-only validation trap helper;
- `__coregc_descriptors()` — descriptor table address/count.

No debug export grants arbitrary raw heap mutation in production builds.

## 9. Integration with hot-code revisions

CoreGC participates in the HCR system as an immutable artifact target:

- A `restart` or `reinstantiate` revision produces a new complete
  `module.coregc.wasm`, its descriptor manifest, and an artifact content ID.
- `ReinstantiatingActivator` semantics apply unchanged: calls already executing
  in the old instance retain its heap and root stack; subsequent calls select a
  new instance. State defaults to reset.
- The current dispatch-delta plan must **not** claim in-place replacement of a
  coregc function if its descriptor table, root-frame layout, function-object
  ABI, or checkpoint map changes. Those are compatibility contracts in the HCR
  component fingerprint.
- A future core-Wasm dispatch adapter may update function-table cells only after
  base revision, type-descriptor, and root-layout hashes match. It is a runtime
  feature, not a mutation of a standard Wasm function body.

The non-moving collector makes same-instance table-cell replacement less
complicated than a moving heap, but it does not make closure/context layout
changes safe. Existing function objects carry table indexes and captured fat
references from their originating revision. Phase 1 therefore uses complete
reinstantiation for CoreGC HCR.

## 10. Security and failure behavior

This backend changes GC safety from an engine guarantee into generated-code and
runtime invariants. Treat it as security-sensitive compiler/runtime code.

### Required checks

- checked addition/multiplication for header, field, array-stride, length, and
  allocation-size arithmetic;
- alignment/range/header magic/type-ID validation before every dereference;
- bounds checks for every dynamic array access and bulk copy;
- descriptor index bounds and nonzero live type IDs;
- allocator list integrity checks in debug mode and hard traps on corruption;
- worklist capacity checks;
- root frame chain bounds/cycle checks in debug mode;
- no host-controlled type ID may select arbitrary descriptor code;
- explicit maximum memory/page policy;
- checked table index and normalized indirect-call signature.

### Traps and OOM

Distinct trap codes include `BAD_FAT_REF`, `BAD_TYPE_ID`, `BAD_CAST`,
`OUT_OF_BOUNDS`, `ROOT_STACK_OVERFLOW`, `HEAP_OOM`, `WORKLIST_OOM`, and
`ALLOCATOR_CORRUPTION`. Hosts may map them to their existing jsaw trap/error
surface but must not resume after memory-integrity failure.

An allocation failure follows this sequence:

1. checkpoint and full collection;
2. retry allocator request;
3. grow memory if allowed;
4. retry;
5. trap `HEAP_OOM`.

It never drops a root, ignores a failed growth, or performs a partial sweep.

## 11. Implementation plan

Each phase has a narrow exit condition. Do not enable the backend for arbitrary
jsaw output until its preceding phase has a validation and execution oracle.

### Phase 0 — feature gate, inventory, and artifact plumbing

1. Add `Emit::coregc` / `emitCoreGc` as an opt-in target with a distinct file
   extension/path, e.g. `wasm/module.coregc.wasm` and
   `wasm/module.coregc.json`.
2. Add `CoreGcInventory` over `portal_pc_waffle::Module` and a total supported
   operator/type matcher.
3. Assign deterministic `TypeId`s and emit descriptor manifests without
   changing executable code.
4. Add feature validation asserting the core artifact contains no GC type
   section, GC instructions, `anyref`, `i31ref`, `structref`, `arrayref`, or
   `call_ref`.
5. Make unsupported Waffle entities fail with source operator/signature detail.

**Exit:** inventory is deterministic across equivalent module construction
orders; all current e2e modules either inventory successfully or reject with a
specific feature diagnostic.

### Phase 1 — runtime skeleton and allocation

1. Generate a core memory, allocator metadata, descriptor data segment, header
   constants, and runtime globals.
2. Implement `__gc_init`, bump/free-list allocation, memory growth, header
   validation, and debug heap accounting entirely in Waffle/core operations.
3. Lower pointer-free fixed structs and arrays first. No collection yet: use a
   bounded bump allocator and deterministic OOM to validate layout/access.
4. Add generated type-ID/header checks for allocation and field/element access.

**Exit:** struct/array allocation, primitive field access, packed field behavior,
and bounds traps execute in a core-Wasm engine without GC enabled.

### Phase 2 — precise descriptors and non-moving collection

1. Generate descriptors/scanners for fixed reference fields and reference
   arrays.
2. Implement header mark flags, linear worklist, mark traversal, sweep, and
   free-list reuse.
3. Add generated global-root visiting and allocation-triggered full collection.
4. Test cycles, self references, deep graphs, pointer-free leaves, nulls,
   subtype/cast checks, and deliberate stale/corrupt pair traps.

**Exit:** forcing a collection reclaims unreachable graphs and preserves every
reachable graph without conservative retention or host callback.

### Phase 3 — shadow frames and checkpoints

1. Extend core lowering with logical fat-reference values and root-slot
   allocation from SSA liveness.
2. Insert frame prologue/epilogue on all supported structured exits.
3. Insert checkpoints before allocations/calls and loop allocation budgets.
4. Add a core-IR root verifier and debug root-frame assertions.
5. Support direct calls, nested closures, function objects, and indirect calls
   under checkpoint discipline.

**Exit:** a function allocating repeatedly while retaining only a local managed
reference survives collections; the same test without a generated root slot is
detected by a negative compiler/runtime test, not silently accepted.

### Phase 4 — jsaw representation closure

1. Lower the existing `Repr`: boxed JS Number/Boolean/BigInt/String/object,
   property tries, arguments arrays, dynamic arrays, typed arrays, DataView,
   function objects, and module contexts into coregc layouts.
2. Implement `GcValue` dynamic dispatch and the exact current truthiness,
   equality, member, and call semantics.
3. Lower every current WasmGC operator emitted by `conv.rs`, `typed_arrays.rs`,
   `primordials.rs`, and `enumerate.rs`; inventory tests prevent a new native
   GC operation from being forgotten.
4. Keep the Java/Swift emitters independent: they already have their own host
   managed representations and are not targets of this core-Wasm collector.

**Exit:** the existing JS e2e corpus runs with equal observable numeric/string/
array/object results under WasmGC and coregc, excluding explicitly documented
unsupported host/interoperability features.

### Phase 5 — runtime integration and public protocol

1. Extend `jsaw-wasi-bin` manifest/result with `coregc` output and runtime
   profile metadata while preserving manifest v1 behavior for existing targets.
2. Add host feature selection: prefer WasmGC when available; select coregc only
   when the host requests it or WasmGC feature probing fails.
3. Provide optional `pin`/`unpin` host handles for values retained outside an
   exported call; document that hosts must not retain an unpinned fat reference
   across an allocation/return boundary.
4. Integrate HCR only as complete reinstantiate revisions first.

**Exit:** a Wasmtime/core-only harness validates and executes `module.coregc.wasm`
without enabling `WasmFeatures::GC`; a WasmGC-capable host still selects the
native artifact by default.

### Phase 6 — performance and optional extensions

Only after semantic parity and profiling:

- segregated-fit/TLSF allocator or size classes;
- allocation fast path + threshold heuristics;
- bounded incremental marking with a correctly implemented tri-color write
  barrier; do not make it default merely to reduce pause time;
- descriptor subtype bitsets;
- core tail-call/root-frame transfer if core tail calls are available;
- `memory64`, threads/shared-memory safepoint protocol, weak refs/finalizers,
  moving/compacting heap, and richer host FFI as separately specified projects.

## 12. Testing and verification

### 12.1 Inventory and code-generation tests

- deterministic TypeId assignment under source/module insertion permutations;
- descriptor offsets/alignment for every `StorageType`, including packed fields;
- source Waffle signatures with identical layout but distinct identities are
  retained as distinct descriptors unless the type system says they are the
  same/subtypes;
- each accepted GC operator has a core lowering test; each rejected operator
  checks a useful diagnostic;
- encoded core output validates with `wasmparser` with GC, reference types, and
  typed-function-reference features disabled as required by the chosen profile;
- scanning the encoded operator stream asserts absence of `struct.*`, `array.*`,
  `ref.cast/test`, `ref.i31`, and `call_ref`.

### 12.2 Runtime unit and property tests

- allocate/collect one leaf, one struct, one ref array, self-cycle, two-cycle,
  and wide/deep graph;
- root in local/shadow slot, global, managed field, array element, closure
  environment, and pinned host slot;
- collection at each checkpoint class, including immediately before/after an
  allocating indirect call;
- root-frame push/pop across return, conditional branch, loop break/continue,
  and supported tail-call lowering;
- null pair, partial-null pair, wrong type ID, freed header, bad descriptor,
  unaligned address, out-of-heap address, and out-of-bounds array index trap;
- allocator reuse/coalescing/growth and repeated full collection;
- mark worklist exhaustion is a clean trap, not a partial sweep;
- fuzzer/property test generates descriptor graphs and compares reachability to
  a small trusted host graph model.

### 12.3 Differential tests

For every fixture supported by both targets:

```text
jsaw source -> WasmGC artifact -> Wasmtime GC execution
             -> coregc artifact -> core-Wasm execution
```

Compare exports, return values, stdout/stderr side effects, traps, and selected
heap statistics. Use deterministic collection schedules in tests: checkpoint
threshold `0` forces collection at every safe allocation/call; a second run uses
a normal threshold to catch schedule-dependent rooting bugs.

### 12.4 Sanitizer/debug configuration

A debug coregc artifact adds:

- red zones/canaries around allocations;
- poisoned freed headers and payload prefix;
- validation before every reference access;
- descriptor scanner visits counted and logged;
- root-frame chain validation at each checkpoint;
- deterministic `collect_every_n_allocations` knob.

Run these artifacts in Wasmtime with bounded fuel/epoch timeout and memory
limits. A collector test that hangs or OOMs without an expected trap is a test
failure, not a performance result.

## 13. Compatibility and deployment policy

Artifact selection is explicit:

| Host capability | Default artifact |
| --- | --- |
| validates WasmGC feature closure | `module.gc.wasm` |
| core Wasm + required coregc profile features only | `module.coregc.wasm` |
| neither | compilation/host capability error |

The Gradle plugin and normal build cache include backend target/profile in their
cache keys. Never reuse a WasmGC artifact as a coregc artifact merely because
source hashes match. The artifact manifest records compiler version, fallback
runtime schema, type-table digest, checkpoint ABI, memory limits, and enabled
core features.

CoreGC has no automatic cross-artifact state migration. A host switching from
WasmGC to coregc, or vice versa, starts a new instance unless a future explicit
serialization/migration interface is designed. Fat pointers are process/module
instance addresses and must never cross an FFI boundary as durable IDs.

## 14. Risks, trade-offs, and rejected alternatives

### Risks

- **Rooting bugs are catastrophic.** One missed temporary can free a live
  object. Exact shadow frames, checkpoints, and forced-collection differential
  tests are non-negotiable.
- **Binary size/runtime overhead.** The fallback ships allocator, descriptors,
  scanners, root code, and checks that native WasmGC delegates to the engine.
  V8 explicitly notes this traditional linear-memory cost.[^v8-porting]
- **Fragmentation.** Non-moving sweep can fragment. The phase-1 allocator must
  expose fragmentation statistics before choosing TLSF or compaction.
- **Feature drift.** The native compiler will grow. An inventory exhaustiveness
  test must force each new WasmGC operation to get a fallback lowering or an
  explicit rejection.
- **Direct-code dependencies.** The HCR component graph must include generated
  descriptors/helpers/checkpoint ABI; otherwise a revision could reuse an
  incompatible core function body.

### Rejected alternatives

1. **Scan all `i32`s in linear memory/stack conservatively.** Rejected: false
   retention, accidental numbers, no exact type, inability to distinguish
   dynamic JS scalar values, and no safe interior-pointer story. Emgc documents
   these limitations for its intentionally conservative experiment.[^emgc]
2. **Collect only after exported calls return.** Rejected: long-running jsaw
   compiler work can allocate heavily before a host turn; it recreates the
   limitation AssemblyScript's minimal runtime documents.[^as-runtime]
3. **Use host `malloc`/GC imports.** Rejected: violates pure-Wasm runtime goal,
   complicates WASI/browser portability, and loses reproducible runtime
   semantics.
4. **Store only a pointer and recover type by address lookup.** Rejected: every
   edge/root would need a global address map, stale/forged pointer diagnosis
   weakens, and type checks/scanning lose the requested actual-type invariant.
5. **Start with a moving collector.** Rejected: every root slot/object field
   would need rewriting, FFI/pins become much harder, and correctness proof is
   materially larger. The initial collector is non-moving by design.
6. **Interpret WasmGC ops at runtime.** Rejected: it defeats direct typed code
   and makes the fallback a slow VM instead of a backend.

## 15. Source notes

[^wasm-types]: WebAssembly specification, *Types* — heap types classify managed
    aggregates; reference values are opaque and cannot be stored in memories;
    structures are heterogenous/static-indexed and arrays homogeneous/dynamic.
    <https://webassembly.github.io/spec/core/syntax/types.html>
[^gc-overview]: WebAssembly GC proposal overview — rationale and examples for
    structs, arrays, typed references, casts, `anyref`, `i31ref`, function
    references, and explicit layout-oriented runtime type information.
    <https://github.com/WebAssembly/gc/blob/main/proposals/gc/Overview.md>
[^root-marking]: WebAssembly design issue #1459 / root-scanning proposal —
    inability to scan the Wasm stack is identified as the linear-memory-GC
    obstacle; the proposal and comments describe marked locals and
    AssemblyScript's precise shadow-stack/global/per-class visitor strategy.
    It is explicitly Phase 0, so this plan cannot depend on it.
    <https://github.com/WebAssembly/design/issues/1459>
[^as-runtime]: AssemblyScript runtime documentation — incremental, minimal, and
    stub runtime variants; shadow-stack/minimal-stack-unwound behavior; managed
    header and RTTI description. <https://www.assemblyscript.org/runtime.html>
[^as-itcms]: AssemblyScript `itcms` runtime source — incremental tri-color
    mark-and-sweep, root visitation, type/member visitors, stack visitation,
    and write-barrier implementation. <https://github.com/AssemblyScript/assemblyscript/blob/main/std/assembly/rt/itcms.ts>
[^as-tlsf]: AssemblyScript TLSF allocator source — two-level segregated fit,
    block metadata, free-list coalescing, and memory growth implementation.
    <https://github.com/AssemblyScript/assemblyscript/blob/main/std/assembly/rt/tlsf.ts>
[^binaryen-spill]: Binaryen `SpillPointers` pass — spills possible pointer locals
    around calls to support Boehm-style collection; useful prior art for
    liveness-based spilling, but conservative by pointer-width local type.
    <https://github.com/WebAssembly/binaryen/blob/main/src/passes/SpillPointers.cpp>
[^emgc]: `juj/emgc` README — a deliberately non-production Emscripten research
    mark-and-sweep collector, documenting conservative raw-pointer discovery,
    global/stack/explicit roots, and interior-pointer limitations.
    <https://github.com/juj/emgc>
[^v8-porting]: V8, *A new way to bring garbage collected programming languages
    efficiently to WebAssembly* — compares traditional linear-memory runtimes
    with WasmGC, including stack-root limitations, shadow stacks, binary size,
    fragmentation, and VM-managed GC trade-offs.
    <https://v8.dev/blog/wasm-gc-porting>
