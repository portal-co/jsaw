# Plan: source exceptions as explicit return structs

**Status:** planned.

## 1. Goal

Implement synchronous JavaScript `throw` / `try`-`catch` in the WasmGC
backend without using Wasm exception instructions. A thrown value is carried
through ordinary calls as a private, nominal exception-result `struct`. Each
call site inspects the result: it enters the nearest active catch handler, or
propagates the same exceptional result through its own return. An exported
entry wrapper converts an uncaught exception to the backend's existing trap
behavior; it never exposes the private result struct as a JavaScript value.

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
continue to contain no WasmGC references or exception instructions.

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
- Adding exception support to the mobile emitters in this change. Their
  existing audits remain fail-closed for unsupported artifacts.

## 3. Internal result representation and call protocol

### 3.1 Private nominal envelope

Add one private managed struct type, owned by the representation layer (for
example `repr.exception_result_ty()`):

```text
ExceptionResult {
    state: i32,          // NORMAL or THROWN; no other values are valid
    payload: anyref,     // boxed normal JS result, or boxed thrown JS value
}
```

Use distinct constructors/accessors (`normal_result`, `thrown_result`,
`result_state`, `result_payload`) instead of repeating field indices in
`conv.rs`. Validate the state before interpreting the payload. Do not add the
envelope to the JS `Object` shape system, object tags, or ordinary dynamic
value conversion. A plain JS object, even one with `state` and `payload`
properties, is never a valid envelope.

For the first correct implementation, all generated source-callable
functions use the same internal `ExceptionResult` return ABI. On normal return,
box the source result in the payload; on an escaping throw, put the thrown
value there. This uniform ABI avoids unsound effect inference for recursive,
indirect, rebound, or dynamically-called functions. Retain existing
`ReturnKinds` information for normal-payload unboxing where convenient, but
correctness must not depend on callers guessing whether a callee can throw.
Restoring raw-return fast paths for proven no-throw functions is later
optimization work.

Generated non-source helpers and host-import adapters must explicitly adapt
to this protocol: ordinary completion becomes `NORMAL`; any existing trap
behavior remains a trap. No helper may return an uninitialized or malformed
envelope.

### 3.2 Caller handling

For every direct, indirect, reference, and adapter-mediated source call:

1. Receive the typed `ExceptionResult` reference.
2. Read and validate its state.
3. For `NORMAL`, extract the payload, convert it to the normal internal
   `LowerValue` expected by the continuation, and continue.
4. For `THROWN`, extract the payload and branch to the nearest active catch
   block, binding the value when requested. If no handler is active, return
   the same typed exception result from the current function without wrapping
   it again.

The check is mandatory even for a statically-known callee; recursive and
indirect calls must use the same semantics. Tail-call optimizations may not
skip the check or bypass a handler. Initially disable/fall back from tail-call
paths whose result could be exceptional; a later optimization may forward the
result only when it preserves the handler check.

A direct `throw` in the current function enters its active lexical handler, or
constructs a `THROWN` result and returns it when no local handler exists.
Calls made inside a protected region use that region's handler as the call
site target. This keeps same-function throws and cross-function propagation
consistent without relying on Wasm unwinding.

### 3.3 Public boundaries

Keep public signatures unchanged. Generated numeric, `$gc`, and raw export
wrappers call the internal source function, inspect its envelope, and:

- unwrap/coerce the normal payload using the existing export rules; or
- execute `unreachable` for an uncaught thrown result.

The private envelope must not be added to `CoreGcArtifact::handle_abi` or
exposed as an `i32` handle. It is internal to the module. Normal `wasm:` host
imports are adapted into `NORMAL` results; exception values returned by
foreign host code are not invented or decoded as envelopes.

## 4. Ownership and implementation seams

- `conv.rs` owns source-function exception return signatures, wrapper
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

1. Change generated source-callable function signatures to return the private
   envelope; pack normal returns and escaping throws.
2. Lower each call form through a shared result-dispatch helper, including
   direct native calls, `CallRef`/indirect calls, adapters, and recursive
   calls. Update return-kind analysis and tail-call downgrade/eligibility
   rules so throws are neither lost nor double-boxed.
3. Lower lexical catch handlers: local throws branch to the active handler;
   exceptional call results branch to the same handler; uncaught results
   propagate unchanged.
4. Adapt generated helpers/import adapters as normal completions and unwrap
   only at public export wrappers.

**Commit:** `exceptions: propagate results through native callers`.

**Gate:** validate and execute direct throw/catch, cross-function propagation,
rethrow, nested catches, primitive/object payload identity, uncaught-export
trap, and normal return regressions in native WasmGC.

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
- Optimizing away the uniform envelope for functions proven not to throw.
