# Plan: source exceptions as explicit return structs

**Status:** Phase 1 committed (`e9e56fa`); Phase 2 implemented and validated
for native WasmGC; CoreGC parity and final semantic/preflight work remain.

## 1. Goal

Implement synchronous JavaScript `throw` / `try`-`catch` in the WasmGC
backend without using Wasm exception instructions. A thrown value is carried
through ordinary calls as a private, nominal exception-result `struct`.
When a source graph contains an explicit `throw`, source-callable functions
in that graph use an `anyref` return ABI: normal returns carry ordinary boxed
JavaScript values, while exceptional returns carry the private struct (upcast
to `anyref`). Each caller tests for that exact nominal type: it enters the
nearest active catch handler, or propagates the same exceptional result
through its own return. Exception-free graphs retain their existing optimized
return ABIs. An exported entry wrapper converts an uncaught exception to the
backend's existing trap behavior; it never exposes the private result struct
as a JavaScript value.

This representation is disjoint from jsaw's JavaScript object/value
representations. The result envelope has its own nominal Waffle struct type,
is never passed to `box_value` or treated as a source-level object, and is
only unpacked by exception-aware lowering. Its payload is an ordinary boxed
JavaScript value, preserving the identity of thrown objects and supporting
primitives such as `undefined`.

CoreGC should need no exception-specific runtime representation: the envelope
is an ordinary managed aggregate struct. Existing struct inventory, layout,
rooting, call-result flattening, and validation must lower it exactly as they
lower polymorphic-return and other aggregate structs. CoreGC output must
continue to contain no WasmGC references or exception instructions. Mobile
emitters do not support exception-bearing modules and must reject them in
their feature-closure audit.

## 2. Initial semantic boundary

### 2.1 Supported behavior

- `throw value` propagates that exact JavaScript value.
- `try { ... } catch (name) { ... }` catches explicit throws from the same
  function or from a called jsaw function. A catch without a binding is also
  supported when the source pipeline preserves it.
- Nested handlers catch at the nearest lexical catch. A rethrow of the catch
  binding preserves the original payload.
- A call which returns normally continues with its ordinary JS result. A call
  which returns exceptionally transfers its payload to the nearest active
  handler or propagates the exception result unchanged.
- Uncaught exceptions at a public Wasm export trap, matching today's
  externally visible behavior for an uncaught source `throw`.

The implementation must inspect the source TAC's catch representation
(current conversion comments refer to `SCatch` edges) and preserve its lexical
handler and catch-binding semantics. The current `TTerm::Throw` lowering to
`Unreachable` and the current lack of `SCatch` lowering are the behavior being
replaced, not constraints on the new design.

### 2.2 Explicit non-goals for this milestone

- Wasm EH tags, `throw`/`try` Wasm operators, or platform-specific exception
  unwinding. CoreGC continues to use ordinary core-Wasm control flow.
- Catching Wasm traps, `unreachable`, memory faults, or exceptions originating
  in foreign `wasm:` host imports. Host-import exceptions remain outside the
  jsaw result protocol; their current runtime behavior is unchanged.
- Publishing the private exception envelope through a host ABI. Export
  wrappers unwrap normal results and trap on an unhandled thrown result.
- `finally` and asynchronous exceptions. Reject unsupported `finally` forms
  before emission rather than silently skipping their semantics. Add them
  later using an explicit completion model that also handles return, break,
  and continue overriding a pending throw.
- Implementing exception semantics in the JVM, Swift, or mobile emitters.
  The mobile feature-closure audit rejects exception-result modules.

## 3. Internal result representation and call protocol

### 3.1 Private nominal envelope

Add one private managed struct type, owned by the representation layer (for
example `repr.exception_result_ty()`):

```text
ExceptionResult {
    state: i32,          // THROWN only; any other value is malformed
    payload: anyref,     // boxed thrown JavaScript value
}
```

The envelope's nominal WasmGC signature is the exception discriminator;
ordinary JS values are never encoded using this signature. Keep field metadata in `repr.rs` and centralize construction/decoding rather
than repeating field indexes in `conv.rs`. Validate the state before using the
payload. Do not add the envelope to the JS `Object` shape system,
object tags, or ordinary dynamic value conversion. A plain JS object, even
one with `state` and `payload` properties, is never a valid envelope.

Within an exception-enabled source graph, source-callable functions return
boxed JS values normally and the typed exception-result struct, upcast to
`anyref`, when unwinding. The mode is selected before signatures are allocated
by scanning source functions (including nested and linked functions) for
explicit throws; this avoids changing ordinary modules and their fast ABIs.
Within an enabled graph, callers test the nominal exception type rather than
guessing from callee identity. Generated non-source helpers and host-import
adapters retain their ordinary-value ABI.

Only thrown completions are wrapped; there is no `NORMAL` envelope tag.
Foreign import exceptions are not encoded as jsaw exception results. No source
function may return an uninitialized or malformed exception envelope.

### 3.2 Caller handling

For every direct, indirect, reference, and adapter-mediated source call in an
exception-enabled graph:

1. Receive the `anyref` result and `ref.test` it against the exact nominal
   exception-result type.
2. If it is not that type, continue with the ordinary JS result.
3. If it is that type, cast to the envelope, validate `state == THROWN`, and
   extract the payload.
4. Branch to the nearest active catch block, binding the value when requested.
   If no handler is active, return the same envelope (upcast to `anyref`)
   from the current function without wrapping it again.

The check is mandatory even for a statically-known callee; recursive and
indirect calls must use the same semantics. A tail call may forward the raw
`anyref` only when there is no local handler to bypass; a call in a protected
region must be lowered as an ordinary call and checked before returning or
continuing.

A direct `throw` in the current function enters its active lexical handler, or
constructs a `THROWN` result and returns it when no local handler exists.
Calls made inside a protected region use that region's handler as the call
site target. This keeps same-function throws and cross-function propagation
consistent without relying on Wasm unwinding.

### 3.3 Public boundaries

Keep public signatures unchanged. Generated numeric, `$gc`, and raw export
wrappers call the internal source function, test for the nominal envelope,
and:

- unwrap/coerce the normal payload using the existing export rules; or
- execute `unreachable` for an uncaught thrown result.

The private envelope must not be added to `CoreGcArtifact::handle_abi` or
exposed as an `i32` handle. It is internal to the module. Normal `wasm:` host
imports keep their existing adapter ABI; foreign host exceptions are not
invented or decoded as envelopes.

## 4. Ownership and implementation seams

- `conv.rs` owns source-function exception return values, wrapper
  construction, call-site checks, lexical handler routing, and export
  unwrapping. Centralize envelope operations behind small helpers so every
  call form shares the same behavior.
- `repr.rs` owns the unique envelope Waffle signature and field metadata. It
  must remain outside source JS object representations.
- The source TAC / CFG conversion owns mapping `SCatch` regions to emitted
  handler blocks and catch bindings. Preflight every construct that cannot be
  represented in this milestone.
- `coregc_layout.rs` / inventory owns ordinary field tracing and descriptors;
  do not add a special exception type ID protocol.
- `coregc_lower.rs` owns the ordinary struct allocation/get and typed-reference
  call lowering. A result reference is already a `FatRef` under the normal
  type classifier. Only fix generic lowering gaps exposed by this struct.
- `coregc_emit.rs` and the public artifact ABI remain unchanged unless a
  generic verifier needs to recognize the regular lowered signatures.

Avoid representing the exception envelope as a `String`, an object-shape
convention, a magic JS property, or an `anyref` sentinel. The dedicated
nominal struct and its validated state are the discrimination mechanism.

## 5. CoreGC requirements

The source exception-result signature is a concrete managed `struct` whose
payload field is the normal dynamic JS value reference. The existing CoreGC
classifier should classify the envelope reference as a concrete `FatRef`;
`StructNew` / `StructGet` and function-call results should use the same
flattening as any other aggregate. Its descriptor must mark the payload as a
reference so a thrown object remains live while the envelope is live.

In particular:

- Construction of a thrown result must root the payload across the
  `StructNew` checkpoint.
- A returned envelope must remain rooted until the caller tests it and either
  propagates it or transfers its payload to a catch block.
- A catch binding holding a thrown object must remain rooted across later
  allocations and calls in the handler.
- Propagating an uncaught result forwards the original pair; it must not
  allocate a replacement envelope or expose its linear-memory address.
- `verify_core_only` must still reject GC/reference types and Wasm exception
  operators from the final CoreGC artifact.

No handle-table entry is needed for internal exception propagation. Existing
CoreGC rooting/liveness is the ownership mechanism. A host-visible thrown
value protocol, if desired later, is a separate ABI design.

## 6. Implementation phases

### Phase 1 — pin source exception/control-flow representation

1. Trace source `try`/`catch`, `throw`, and catch bindings through SWC, TAC,
   continuation lowering, and CFG construction; record which `SCatch` or
   equivalent edges are available to `conv.rs`.
2. Add the private exception-result signature, field indexes, and state
   discriminants to `Repr`; test the tagged aggregate layout and nominal
   identity. Emitted constructors and checked state decoding land with the
   native call protocol in Phase 2.
3. Define preflight diagnostics for unsupported `finally` and any unsupported
   catch pattern before output mutation.
4. Keep current no-catch `throw` behavior until caller/result lowering lands;
   do not emit a partial protocol.

**Commit:** `exceptions: define internal completion result`.

**Gate:** representation-layout tests pass; source catch routing is pinned
(`SCatch::Just` carries the catch target and its live SSA args, with the
thrown value supplied as the first catch-shim parameter). Unsupported source
forms are precisely classified before lowering is added.

### Phase 2 — native WasmGC result and caller protocol

Exception-bearing source graphs are selected by a pre-scan and use the
existing `anyref` return ABI: normal values stay ordinary boxed JS values,
while throws return the private nominal aggregate. Exception-free graphs keep
their prior raw/union ABIs.

1. Emit thrown-result aggregates and validate their state at consumers.
2. Dispatch direct, indirect, adapter-mediated, and imported source-call
   results to the nearest lexical catch or propagate the original envelope.
3. Lower direct throws, nested catch/rethrow, public numeric/raw/GC export
   traps, and module-initializer traps.
4. Keep foreign host-import behavior unchanged and reject exception-result
   modules in the mobile feature-closure audit.

**Commit:** `exceptions: propagate results through native callers`.

**Gate:** direct/local catches, linked cross-function propagation, nested
catch/rethrow, primitive and object payload use, uncaught-export traps, and
normal returns in exception-enabled graphs pass on Wasmtime and Node.js.
Mobile emission rejects these modules; CoreGC parity is Phase 3.

### Phase 3 — CoreGC parity through generic aggregate lowering

1. Compile the same source fixtures through `emit_coregc`; first try the
   existing struct inventory/layout, call flattening, and root-spill logic
   without exception-specific code.
2. Fix only generic aggregate/reference lowering gaps needed by the envelope.
   Keep exception-state branching as ordinary scalar CFG and the envelope as
   an ordinary `FatRef`.
3. Add forced-collection tests for envelope construction, propagation across
   several calls, caught-payload use after allocation, and propagation of an
   uncaught result to the export wrapper.
4. Assert the final artifact has no WasmGC references, EH operators, or
   exception-result export/import ABI.

**Commit:** `coregc: lower exception results as ordinary structs`.

**Gate:** native/CoreGC outcomes match for all supported fixtures under normal
and forced collection schedules; existing CoreGC cross tests pass.

### Phase 4 — semantic regressions and consumer documentation

1. Add boundary and adversarial tests for direct/indirect calls, recursive
   calls, `undefined`, object identity, rethrow, shadowed catch names, and
   ordinary objects whose properties resemble the envelope fields.
2. Document that uncaught exceptions trap at exported boundaries and that
   foreign host throws, Wasm traps, `finally`, async exceptions, and mobile
   emission are not supported by this milestone.
3. Update the existing host-boundary plan when implementation starts:
   `docs/plan-wasm-handle-imports-exports.md` §8 currently lists “exception
   ABI” as a non-goal. Remove or narrow that exclusion so it describes only
   host-visible exception transport; this plan's internal return-struct
   protocol does not change the handle ABI.

**Commit:** `exceptions: verify native and CoreGC propagation parity`.

**Gate:** full `portal-jsc-waffle` native and CoreGC test suites pass; mobile
emitters reject unsupported exception-bearing artifacts loudly.

## 7. Test matrix

| Area | Test |
| --- | --- |
| Representation | Dedicated nominal type; valid state tags; ordinary JS objects cannot be decoded as envelopes. |
| Local control flow | Direct throw caught by nearest catch; nested catch; optional catch binding; rethrow preserves payload identity. |
| Calls | Throw from direct callee, transitive callee, recursive call, and dynamically-dispatched jsaw function is caught or propagated correctly. |
| Values | `undefined`, null, scalar, string, and object payloads; caught object is the identical object. |
| Return interaction | Normal return inside/outside protected region; tail-position calls do not skip exception dispatch. |
| Public boundary | Caught exception returns normally; uncaught exception traps and does not leak the envelope. |
| CoreGC roots | Forced collection during result construction and catch handling preserves envelope and payload; propagation does not allocate a replacement. |
| Core-only proof | No EH instructions or reference types in the encoded CoreGC module; no exception result in public ABI metadata. |
| Regression | Existing WasmGC e2e and CoreGC cross tests retain normal-path behavior; unsupported `finally` fails closed. |

## 8. Out of scope

- Wasm-native exception tags/instructions or host-visible exception handles.
- Catching traps, foreign import exceptions, or arbitrary backend runtime
  failures.
- `finally`, async/Promise rejection, generator unwinding, or cross-thread
  exceptions.
- Exception support in JVM, Swift, or mobile emitters.
- Per-function throw-effect inference to retain raw return ABIs inside a
  graph that contains any throw; the current exception mode is graph-wide.
