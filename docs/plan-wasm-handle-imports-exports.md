# Plan: host Wasm imports and handle-based CoreGC boundaries

**Status:** planned.

## 1. Goal

Add one explicit host-function boundary to `portal-jsc-waffle` without
confusing it with ESM linking:

1. JavaScript modules can name a host Wasm function with a reserved module
   specifier and a signature encoded as a suffix of the imported binding's
   name.
2. The native WasmGC backend emits a real Wasm `func` import with that scalar
   / `anyref` signature, plus a generated JavaScript-value adapter so the
   imported function behaves like an ordinary callable in lowered JS.
3. A signature-marked exported JS function receives the corresponding raw
   Wasm export wrapper.
4. `emit_coregc` accepts those source imports and exports. At **every CoreGC
   host boundary**, a WasmGC reference becomes a validated `i32` handle,
   never a typed/reference-type value or an unvalidated heap address.

The result is a single source convention usable by native WasmGC embedders and
CoreGC embedders. Native WasmGC preserves the reference-valued Wasm ABI;
CoreGC exposes an equivalent ABI in which each `ref` position is an integer
handle. Internal calls remain typed WasmGC values in the native backend and
fat references in CoreGC; handles are strictly an import/export boundary
mechanism.

This plan intentionally does **not** make arbitrary bare ESM packages host
imports. Relative ESM imports remain closed-world links through `ModuleSet`.
The special specifier is a separate, narrow seam with a fail-closed ABI.

## 2. Frozen source convention

### 2.1 Reserved module specifier

A source import whose module specifier has the exact prefix `wasm:` is a host
Wasm import rather than an ESM module-set edge:

```js
import {
  add$wasm$i32_i32$i32 as add,
  log$wasm$ref$v as log,
} from "wasm:env";
```

- `wasm:<module>` maps to the emitted Wasm import module `<module>`.
  `"wasm:env"` therefore emits `(import "env" ...)`.
- `<module>` must be nonempty. It is otherwise copied byte-for-byte; there is
  no relative-path normalization, extension completion, filesystem lookup,
  import-map lookup, or JS module evaluation.
- Only **named** imports are valid. Default imports and namespace imports from
  `wasm:` are errors. A host function is not a JavaScript namespace object.
- A `wasm:` import is not added to `ModuleSet`; `ModuleSet` stays a closed
  set of relative ESM source modules. The existing bare-specifier rejection
  remains unchanged for every non-`wasm:` specifier.

`wasm:` is reserved. A future feature must not assign it ESM-linker semantics.

### 2.2 Function-name suffix grammar

The *imported/exported name*, before any local alias, carries the ABI:

```text
<field>$wasm$<param-tokens>$<result-token>
```

Examples:

```text
add$wasm$i32_i32$i32       host field "add": (i32, i32) -> i32
clock$wasm$$i64            host field "clock": () -> i64
log$wasm$ref$v             host field "log": (ref) -> ()
echo$wasm$ref$ref          host field "echo": (ref) -> ref
```

The parser splits at the **last** `$wasm$` marker, so a field base may itself
contain `$`; the base must be nonempty. The remainder must contain exactly one
`$`, separating an underscore-delimited parameter list from one result token.
An empty parameter segment denotes zero parameters. `v` denotes no result.
Only these tokens are accepted in this milestone:

| Token | Native WasmGC import/export type | CoreGC boundary type |
| --- | --- | --- |
| `i32` | `i32` | `i32` |
| `i64` | `i64` | `i64` |
| `f32` | `f32` | `f32` |
| `f64` | `f64` | `f64` |
| `ref` | nullable `anyref` | nullable `i32` handle (`0` is null) |
| `v` (result only) | no result | no result |

There is at most one result. This is deliberate: the current source/adapter
and CoreGC public boundary reject multi-value returns. A suffix such as
`f$wasm$i32$i32_i32`, an empty result token, `ref_null`, `funcref`,
`externref`, a typed concrete reference, or any unknown token is an error
naming the full binding name and malformed suffix.

The suffix is metadata, not part of the host field name. Thus the examples
above import/export fields `add`, `clock`, `log`, and `echo`; the suffix makes
the source declaration unambiguous and is stripped exactly once by the
boundary parser. A second signature marker, an empty base, or two declared
boundaries with the same direction/module/base but different parsed
signatures is rejected before any body is emitted.

### 2.3 Imports and aliases

The local JavaScript binding may be a normal alias:

```js
import { read$wasm$i32_i32_i32_i32_i32$i32 as wasiRead }
  from "wasm:wasi_snapshot_preview1";
```

`wasiRead(...)` is an ordinary JS call. The imported-name suffix controls the
raw host ABI; the local alias controls only source lookup and retains its SWC
`Ident`/`SyntaxContext`. The implementation must key source bindings by
`Ident`, never by rendered alias text.

### 2.4 Signature-marked exports

An entry module exposes a raw host export by giving its **exported** function
name the same suffix:

```js
export function sum$wasm$i32_i32$i32(a, b) {
  return a + b;
}

function implementation(x) { return x.label; }
export { implementation as readLabel$wasm$ref$i32 };
```

The emitted Wasm export names are `sum` and `readLabel`; their source names
include the suffix only to declare the ABI. A suffix-marked export is emitted
as its raw Wasm wrapper and is **not** additionally emitted through the
ordinary numeric-export or `$gc`-adapter paths. Unsuffixed exports retain the
current `ConvertOptions` behavior.

A stripped raw export name must not collide with:

- another stripped raw export name;
- an unsuffixed generated numeric export;
- a requested `gc_export_suffix` export;
- an existing module export or CoreGC runtime-reserved export.

Every collision is an error before output mutation.

## 3. Deep module and ownership

Introduce one private `wasm_boundary` module (for example
`crates/portal-jsc-waffle/src/wasm_boundary.rs`). It is the deep module at the
syntax/ABI seam; neither `linker.rs`, `conv.rs`, nor CoreGC lowering may parse
suffix strings independently.

Its small interface should expose data, not backend policy:

```rust
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum WasmBoundaryType { I32, I64, F32, F64, Ref }

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct WasmBoundarySignature {
    params: Vec<WasmBoundaryType>,
    result: Option<WasmBoundaryType>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct WasmImportSpec {
    module: String,
    field: String,
    signature: WasmBoundarySignature,
}

fn parse_wasm_import_specifier(specifier: &str) -> Option<Result<&str, ConvertError>>;
fn parse_wasm_boundary_name(name: &str) -> Result<(String, WasmBoundarySignature), ConvertError>;
```

The actual type names may vary, but these ownership boundaries must not:

- `wasm_boundary` owns the `wasm:` recognition, suffix grammar, deterministic
  ABI rendering, duplicate-key construction, and source-level diagnostics.
- `linker` owns only the decision that a `wasm:` declaration is not a relative
  ESM edge and the hygienic mapping from local imported `Ident` to parsed
  `WasmImportSpec`.
- `conv` owns native WasmGC adapters and import/export function construction.
- `coregc_runtime` owns validated handle-table state and lifetime operations.
- `coregc_lower` owns adaptation between fat pairs and the handle-table ABI.
- `coregc_emit` owns publication of generated CoreGC wrapper exports and
  artifact metadata.

This keeps parsing, native code generation, and collector ownership local.
Deleting `wasm_boundary` should make every caller reimplement grammar and
collision rules; that is evidence it is earning its depth rather than merely
passing strings through.

## 4. Native WasmGC backend

### 4.1 Import-table extension

Today `linker::build_import_table` resolves every referenced import through a
relative `ModuleSet` target. Replace its target value with an explicit sum:

```rust
enum ImportTarget<'a> {
    InternalFunction { module: String, function: &'a SFunc },
    WasmHost(WasmImportSpec),
}
```

The map remains `BTreeMap<Ident, ImportTarget>` and is built eagerly from
`refs()` so hygiene is preserved. A `wasm:` target must be validated even if
it is not called; malformed ABI declarations must not become latent runtime
failures.

`SValue::LoadId` retains this order:

1. a `WasmHost` table hit lazily creates/reuses a host import adapter and
   produces its `LowerValue::FunctionRef`;
2. an internal-function hit follows existing ESM linking;
3. existing literal/self/ordinary context lookup behavior applies.

Host imports therefore reuse the normal function-object and call machinery
rather than inventing a second JavaScript call representation.

### 4.2 Imported function construction

For each distinct `(host module, stripped field, parsed signature)`, create
exactly one raw Waffle function import:

```rust
FuncDecl::Import(raw_sig, field)
Import { module, name: field, kind: ImportKind::Func(raw_func) }
```

`raw_sig` maps the table in §2.2, using nullable `anyref` for `ref`. Reusing
one import prevents alias spelling and repeated `LoadId`s from changing the
Wasm import section.

Build a generated, ordinary jsaw callable around that raw import:

```text
js host-import adapter native body:
  (context:anyref, this:anyref, arguments:array, formals:anyref...) -> anyref
  read normal JS formals (existing adapter behavior)
  coerce each argument to its suffix type
  call raw imported Func
  box scalar result / pass ref result / produce undefined for void
```

The adapter must use the existing conversion helpers consistently:

| ABI token | JS → raw import | raw import → JS result |
| --- | --- | --- |
| `i32` | `as_i32` | `LowerValue::Wasm { Integer }` |
| `i64` | `as_i64` | `LowerValue::Wasm { BigInt }` |
| `f32` | `as_f64`, then `F32DemoteF64` | `F64PromoteF32`, then `Number` |
| `f64` | `as_f64` | `Number` |
| `ref` | `box_value` / nullable `anyref` | `Reference` |
| `v` | n/a | existing `undef` representation |

A generated host-import adapter uses a boxed `anyref` JS-function ABI even
when its raw import is scalar. That is the conservative correct choice;
return-kind fast-path specialization for host imports is follow-up work, not
a prerequisite for correct host ABI emission.

### 4.3 Raw export construction

When flattening the entry export surface, parse every exported function name
before the existing numeric/$gc export logic. For a marked export, create:

```text
raw Wasm wrapper signature from the suffix
  raw parameters -> LowerValue values of matching JS kind
  initialize the normal module context / this / arguments array
  call the function's existing js_adapter
  coerce its boxed result to the requested raw result token
  return it (or discard for v)
```

Parameter conversion must follow the same table as imports in reverse.
`ref` inputs are passed as `anyref` without a concrete `ref.cast`; a raw
export/import signature deliberately means the dynamic JavaScript-value
reference boundary, not a source-private `Repr` struct type.

The wrapper invokes `ensure_module_init` exactly as `export_numeric_function`
does, so top-level bindings and primordials have identical initialization
semantics. It must not duplicate a bespoke module context policy.

### 4.4 Native artifact rules

- Existing non-boundary exports are unchanged.
- A native artifact containing `wasm:` imports will no longer satisfy the
  current mobile emitter audit; that audit must continue to reject host
  imports loudly. JVM and Swift emitters are explicitly out of this phase.
- Wasmtime/Node native-backend tests instantiate the module with imports
  matching parsed raw signatures. No test may rely on an implicit undefined
  context lookup or an untyped dynamic host-function fallback.

## 5. CoreGC handle ABI

Core Wasm cannot expose WasmGC `anyref`/typed references. It also must never
let a host observe a raw linear-memory address, because an address could be
stale, forged, or become a use-after-free reference after collection.

For every CoreGC **function import or function export**, derive the boundary
signature from its source Waffle signature:

```text
source i32/i64/f32/f64            -> same core scalar
source concrete/abstract/dynamic ref -> i32 handle
source concrete funcref             -> i32 table slot (not a GC handle)
```

The special suffix determines which imports/exports `conv` emits, but CoreGC
applies this conversion to every source function import/export it accepts.
Thus a hand-built source Waffle module with a reference-valued export is safe
as well: its public CoreGC export gets an `i32` handle wrapper even if it was
not created from JavaScript suffix syntax.

A source `FuncDecl::Import` stops being rejected globally. Non-function
imports (memory/global/table/type/tag) remain rejected by name in this phase.
The CoreGC output import has the same module and field strings but its
reference positions are `i32` handles. All generated wrappers and runtime
handle functions remain ordinary core-Wasm bodies.

### 5.1 Handle representation

```text
handle = 0                         null
handle != 0                        (generation: u16, slot_plus_one: u16)
```

A fixed generated handle table has at most 65,534 usable slots; slot zero is
not issued. Its initial size is configured by a new
`CoreGcOptions::handle_table_bytes` and it resides in the static runtime
region below `heap_base`, after the descriptor/root/worklist reservations.
It is not a managed allocation and never moves.

Each 16-byte slot is:

```text
offset  field       meaning
0       address     fat-reference payload address (or i31 payload)
4       type_id     concrete type ID or I31_TYPE_ID
8       generation  nonzero u16 in low bits; incremented before reuse
12      refcount    live host/root count; zero means vacant
```

A handle resolution checks, in order:

1. `0` is accepted only as the null pair `(0, 0)`.
2. `slot_plus_one` is in range and its slot has `refcount != 0`.
3. the encoded generation equals the stored nonzero generation.
4. the stored pair is valid for the requested lowered value plan:
   - concrete reference: `validate_ref` plus exact expected type ID;
   - dynamic: `validate_ref` for heap values or the existing i31 rule;
   - abstract struct/array: `validate_ref` plus descriptor-kind check.

Failure traps deterministically with a new named CoreGC trap identity
(`BAD_HANDLE`; stale/generation mismatch and invalid slot) or
`HANDLE_TABLE_FULL` / `HANDLE_REFCOUNT_OVERFLOW` as appropriate. A generation
that would wrap to zero retires that slot permanently rather than making a
stale handle valid again.

### 5.2 Roots and ownership

Every live table entry is a strong root. `collect` marks each
`refcount != 0` pair before walking shadow frames; i31 entries are skipped by
the existing immediate rule. This is a required collector change, not a host
convention: a returned handle must survive arbitrary later allocations and
collections until it is released.

Generated CoreGC artifacts that expose any reference boundary also export:

```text
__coregc_handle_retain(handle: i32) -> i32
__coregc_handle_release(handle: i32) -> ()
```

- A reference-valued **CoreGC export result** creates a live handle with
  `refcount = 1`. Ownership transfers to the host, which must eventually call
  `release` once per owned/retained reference.
- A handle passed into a CoreGC export is borrowed for that invocation. The
  caller must keep it live for the call and retains ownership afterwards.
- A reference argument passed from CoreGC to a host import is represented by a
  temporary live handle for the duration of that import call. The imported
  function may inspect it and may return it, but must not retain/store it past
  the call in this phase.
- A reference result returned from a host import is a borrowed, already-live
  handle (or zero). The wrapper resolves it before temporary argument handles
  are released. A host cannot fabricate a managed object merely by inventing
  an integer; it can return only a currently valid handle it already owns or
  received in the same call.
- `retain` validates and increments a live handle; `release` validates and
  decrements it, clears `address`/`type_id` when it reaches zero, and makes
  that generation stale forever. Both reject zero: null needs neither
  ownership nor release.

No asynchronous, escaping host-import handle is supported initially. If an
embedder needs a callback to keep an import argument, it must be designed as a
separate reentrant/host-root protocol; silently allowing an imported function
to retain a temporary handle would leak or create a use-after-free hazard.

### 5.3 CoreGC lowering shape

Extend `ModulePlan` with two distinct identities for an imported function:

```text
source imported Func
  -> raw_core_import: (scalars, handles) -> (scalar | handle)
  -> lowered_adapter: (flattened scalars/fat pairs) -> flattened result
```

All source `Operator::Call` sites continue to target `lowered_adapter`; only
that adapter emits the raw core import call. It:

1. receives the source signature's flattened CoreGC values;
2. allocates temporary handles for each reference argument **after** the
   caller's existing checkpoint and while the caller's shadow frame still
   roots all reference operands;
3. invokes the raw core import with scalars, function-table slots, and handles;
4. resolves a returned handle back to a fat pair if required;
5. releases temporary argument handles; and
6. returns the flattened result to ordinary lowering.

The source call's existing checkpoint/liveness algorithm stays authoritative.
The handle-table operations must not allocate managed heap objects or trigger
collection; otherwise a new checkpoint rule would be needed. The fixed table
and deterministic overflow trap keep this invariant true.

For every source function export, CoreGC keeps the lowered implementation
private and emits a wrapper with the handle ABI. The wrapper resolves handle
parameters, calls the implementation, creates a retained handle for a
reference result, and publishes the wrapper under the original export name.
Scalar-only exports can be forwarded directly only after a test proves the
resulting Waffle and encoded core signatures are identical; initially generate
one uniform wrapper path for auditability.

`LoweredModule.exports` should carry enough boundary metadata for
`coregc_emit` to publish wrappers rather than implementations. Extend
`CoreGcArtifact` with a public deterministic `handle_abi` manifest containing
at least direction, module/field or export name, scalar/handle parameter
kinds, result kind, and the ownership version. Hosts must not reconstruct
this contract by reparsing source names or guessing CoreGC layouts.

### 5.4 Core-only proof and scope

`verify_core_only` continues to use default wasmparser validation and must
now explicitly allow ordinary **function imports** while rejecting all
non-function imports. Its GC/reference-instruction scan must assert that the
output import/export signatures contain only core scalar types. The only
reference-like external representation in the CoreGC artifact is `i32`.

Function references remain scalar table slots, not handles. This plan does
not add a `funcref` suffix token or host-provided function-table mutation.
Memory/global/table/tag imports, `externref`, multi-value boundary results,
threads, and asynchronous callbacks remain fail-closed non-goals.

## 6. Implementation phases and commits

### Phase 1 — parse and inventory the boundary contract

1. Add the private `wasm_boundary` module, grammar parser, deterministic
   renderer, and unit tests for valid/invalid suffixes and `wasm:` module
   parsing.
2. Change linker import classification to `InternalFunction | WasmHost` while
   preserving hygienic `Ident` keys.
3. Add export-surface classification before numeric/$gc emission, with all
   collision checks but no code generation changes yet.
4. Add explicit diagnostics for default/star `wasm:` imports and malformed
   or conflicting ABI declarations.

**Commit:** `wasm-boundary: parse host import and export signatures`.

**Gate:** parser/linker unit tests and existing `portal-jsc-waffle` lib/e2e
conversion tests pass unchanged.

### Phase 2 — native WasmGC imports and raw exports

1. Cache raw `FuncDecl::Import` records by `(module, field, signature)`.
2. Generate callable boxed-JS import adapters and wire `LoadId` host targets
   to `FunctionRef`.
3. Generate raw export wrappers for signature-marked entry exports; bypass
   numeric/$gc wrappers for those names.
4. Preserve deterministic import/export ordering and validate the emitted
   Waffle body plus Wasm bytes.

**Commit:** `wasm-boundary: emit typed host imports and exports`.

**Gate:** Wasmtime and Node tests instantiate a native WasmGC artifact with:

- scalar `(i32, i32) -> i32` import and export;
- no-argument / void import;
- `i64` BigInt round-trip;
- `f32` demote/promote observable rounding;
- nullable `ref` identity import/export;
- aliasing one imported binding under a different local source name;
- duplicate/conflicting ABI and stripped-name collision failures.

Mobile audit must explicitly reject the new import-bearing fixture, proving
it did not accidentally claim support.

### Phase 3 — standalone CoreGC handle runtime

1. Add handle-table layout validation to `CoreGcOptions` and
   `coregc_runtime::build`; fail before output generation if descriptors,
   roots, worklist, handles, and heap cannot fit below `heap_base`.
2. Generate allocate/resolve/retain/release helpers and named trap codes.
3. Make collection mark every live handle-table entry before shadow roots.
4. Test directly through a Wasmtime rig: null, retain/release, collection
   retention, release-driven reclamation, stale generation rejection,
   wrong-type rejection, table-full behavior, and i31 dynamic entries.

**Commit:** `coregc: add validated host handle table`.

**Gate:** runtime tests execute under forced collection, and all existing
CoreGC tests still pass with no handle table entries.

### Phase 4 — CoreGC imported-function adapters (Done)

1. Teach preflight to accept function imports and reject every other source
   import kind with a named diagnostic.
2. Build raw core import signatures by replacing every source reference plan
   with `i32`, then generate the flattened adapter described in §5.3.
3. Preserve direct-call checkpoints/liveness; add debug root-discipline
   coverage for reference arguments crossing imports.
4. Publish deterministic import entries with original module/field names.

**Commit:** `coregc: lower imported functions through handles`.

**Gate:** a hand-built Waffle fixture imports scalar and reference identity
functions. Wasmtime host functions receive handles, reject fabricated/stale
ones, and the CoreGC result matches native WasmGC behavior both with forced
and normal collection schedules. **Done:** scalar and reference identity
fixtures pass under forced collection; the standalone runtime rejects stale
or fabricated handles.

### Phase 5 — CoreGC exported-function wrappers and manifest (Done)

1. Lower source export implementations privately and create handle-ABI
   wrappers under source export names.
2. Emit `__coregc_handle_retain` / `__coregc_handle_release` only when an
   export/import signature contains `ref`; reserve and collision-check their
   names.
3. Add the public `CoreGcHandleAbi` manifest to `CoreGcArtifact`.
4. Extend whole-artifact verification to validate import/export scalar-only
   types and the manifest against emitted Waffle signatures.

**Commit:** `coregc: wrap public reference boundaries as handles`.

**Gate:** a creator → echo-import → consumer sequence proves that an exported
handle remains live across forced collections until release, becomes stale
after release, and cannot be replaced by a freshly allocated object in the
same slot. Scalar-only existing CoreGC exports retain their observed ABI.
**Done:** reference-valued public exports are wrapped with handle resolution
and ownership creation, retain/release are published when a handle boundary
exists, and `CoreGcArtifact::handle_abi` records deterministic boundary kinds.

### Phase 6 — program-level parity and documentation

1. Add native/CoreGC boundary differential fixtures driven by the same
   suffix-marked JavaScript modules wherever Wasmtime can represent the
   native reference values; use hand-built typed Waffle fixtures for cases
   that need direct host `anyref` construction.
2. Update `docs/plan-coregc-atomic-collector-and-lowering.md` §§1, 6, 9, and
   13 to record function imports and handle boundaries as implemented scope,
   not merely a test exception.
3. Add a short consumer-facing ABI guide to `README.md` or the relevant
   embedding documentation: suffix syntax, scalar coercions, handle
   ownership, release requirement, and unsupported async/escaping imports.

**Commit:** `wasm-boundary: validate native and coregc host ABI parity`.

**Gate:** the full portal-waffle lib/coregc-cross suites pass; no CoreGC
artifact contains Wasm GC/reference types; the M19 path remains unchanged
unless explicitly migrated from `wasi.js` to `wasm:wasi_snapshot_preview1` in
a separate, host-integration-approved change.

## 7. Test matrix

| Area | Test |
| --- | --- |
| Syntax | Valid suffixes, empty parameter list, void; malformed markers, unknown types, multiple results, empty field/module. |
| ESM separation | Relative imports still link internally; bare non-`wasm:` imports still reject; `wasm:` never looks in `ModuleSet`. |
| Native scalar ABI | `i32`, `i64`, `f32`, `f64`, void imports/exports run through Wasmtime and Node. |
| Native ref ABI | Null and object identity cross a nullable `anyref` host import/export without a concrete private-type cast. |
| Native errors | Default/star wasm imports, local alias hygiene, same field with incompatible signatures, and export-name collisions fail before encoding. |
| Handle runtime | Root retention, stale generation, wrong type/kind, null, capacity, refcount overflow, release reclamation, i31 dynamic reference. |
| CoreGC import | Imported scalar/ref calls preserve arguments across a forced checkpoint collection; fabricated `i32` fails with `BAD_HANDLE`. |
| CoreGC export | Returned handle can be passed into a later export/import; `release` invalidates it; slot reuse does not revive it. |
| Manifest | Every manifest entry exactly matches emitted import/export parameter and result types. |
| Regression | Existing native WasmGC e2e and CoreGC cross tests pass; mobile audit rejects imports rather than silently emitting unsupported code. |

## 8. Explicit non-goals

- Bare ESM packages, Node builtins, dynamic import, import maps, or changing
  the relative `ModuleSet` linker.
- Multi-value host ABI, typed concrete-ref suffixes, `funcref`/table host
  mutation, `externref`, memory/global/table/tag imports, or host-visible
  exception transport (internal source-call exception propagation is separate).
- Async host imports, callbacks retaining temporary import handles, host-side
  allocation of arbitrary CoreGC heap objects, or a raw-address escape hatch.
- Adding host-import support to JVM/Swift/mobile emitters in this work. The
  mobile audit remains fail-closed until a separate target-specific adapter
  design exists.
- Migrating existing `wasi.js` glue automatically. The source convention is
  available for a later deliberate WASI host integration; it must not bypass
  the current JavaScript glue semantics accidentally.
