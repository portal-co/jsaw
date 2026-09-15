# Plan: jsaw-managed JavaScript hot code reloading

**Status:** design plan. This is a development-mode compiler feature: when a
closed JavaScript module set changes, jsaw identifies the affected functions,
reuses cached ingestion/lowering results for the rest, and publishes an ordered
**module revision**. A runtime owns the mechanics of installing or selecting
Wasm code; jsaw owns source change detection, dependency invalidation, revision
construction, and (where supported) compact delta artifacts.

This plan does **not** claim that standard Wasm can mutate a function body in a
live module instance. It cannot. A standard Wasm runtime's valid options are to
instantiate a new immutable module, select a new instance at a runtime-defined
safe point, or expose a runtime-specific dispatch/update mechanism. jsaw must
produce artifacts whose contract makes one of those options possible.

## 1. Goals and terms

### Goals

1. Re-ingest only JS functions affected by a source edit; reuse cached parsed
   and SSA/lowered fragments for unchanged functions.
2. Recompile only the affected function closure and publish a deterministic
   revision manifest describing what changed and why.
3. Preserve normal ESM semantics as the correctness baseline: imports are live
   bindings, module top-level evaluation is observable, and a source change
   must never silently leave a stale function or stale initialization state.
4. Support two delivery levels:
   - **revision artifacts** — always possible: complete, immutable WasmGC
     modules (and corresponding Java/Swift outputs) plus a reload manifest;
   - **delta artifacts** — optional and runtime-profile-specific: replacement
     code and dispatch-table updates for runtimes intentionally built for it.
5. Keep the default production compiler deterministic and unchanged. HCR is an
   explicit development-mode option, not an optimization silently applied to
   release output.

### Non-goals

- Defining a universal Wasm runtime hot-reload protocol.
- Rewriting a running function's stack frame. A call that began in revision N
  finishes against revision N; a later call may enter N+1.
- Automatic state migration for arbitrary JS top-level state, closures, class
  instances, typed arrays, or host objects.
- Treating raw textual equality as semantic equality. Whitespace/comments must
  not force recompilation; changes to lexical binding identity or closure shape
  must.
- Making a live Java class patcher or a live Swift binary patcher. Java/Swift
  artifacts participate as revisions; their application is their host's job.

### Ubiquitous language

- **Source module** — one JS file keyed exactly like today’s `ModuleSet` key.
- **Function fragment** — jsaw’s cached representation of one hoisted or
  function-literal `SFunc`, its semantic fingerprint, direct dependencies,
  capture/context contract, and backend-independent lowered fragment.
- **Module fingerprint** — a hash for module-level syntax/metadata not owned by
  one function: imports/exports, top-level statements, declaration ordering,
  and source-mode/compiler configuration.
- **Revision** — one atomically publishable view of a closed module set and its
  generated code. A revision has a monotonic ID and a content-derived ID.
- **Reload plan** — jsaw’s machine-readable account of reuse, invalidation,
  replacement eligibility, and fallback reason.
- **Reload profile** — the runtime contract selected at compilation: `restart`,
  `reinstantiate`, or `dispatch-delta`.
- **Delta** — a profile-specific payload that updates the runtime’s indirection
  cells from one compatible revision to another. It is not a generic Wasm
  binary patch.

## 2. Current constraints that determine the design

Today `portal-jsc-waffle` parses each source module into `SModule`, constructs
one closed `ModuleSet`, resolves imports through `SModule` function metadata,
and lowers all source functions into one `portal_pc_waffle::Module`. The
converter memoizes functions by source `SFunc` address, mints user function tags
from a per-conversion counter, directly links imported functions, and emits one
module-init helper over top-level bodies.

Those facts make a naive “compile only the edited function and replace its Wasm
body” unsound:

- address-keyed source identity and counter-based function tags are not stable
  across compiler processes;
- direct native calls embed the callee’s function identity and ABI;
- a module-init body installs hoisted functions and executes top-level state;
- Wasm type indices, function indices, and Java/Swift generated names are
  revision-local implementation details;
- Wasm module instances are immutable under the core specification.

The recent `SModuleBuilder::take_functions` seam is useful but insufficient by
itself. It lets a dependent consume SSA functions as they are translated; HCR
needs stable fingerprints, dependency metadata, and an explicit runtime
interface before it can discard an unchanged function safely.

## 3. Deep modules and seams

The design deliberately separates three deep modules.

### 3.1 `jsaw-core`: incremental function inventory

`SModuleBuilder` remains responsible for parsing/lowering a `ModuleItem` and
making completed `SFunc`s available through `take_functions`. It must gain an
optional inventory output, not a dependency on Waffle or any runtime:

```rust
pub struct FunctionInventory {
    pub functions: BTreeMap<FunctionKey, FunctionSourceInfo>,
    pub module: ModuleSourceInfo,
}

pub struct FunctionSourceInfo {
    pub declared_name: Option<Atom>,
    pub semantic_hash: Digest,
    pub direct_refs: BTreeSet<FunctionRef>,
    pub captures: BTreeSet<Ident>,
    pub shape_hash: Digest,
}
```

`FunctionKey` is a **stable source-domain key**, not a pointer and not a flattened
SWC identifier. For declaration-backed functions it is `(module key, declared
export/local role, lexical ordinal)`; for nested/literal functions it is the
parent function key plus a structural path. SWC `Ident`/`Id` values continue to
be compared as hygienic values while computing the inventory. A textual name is
allowed only in the externally textual parts of a key (module path/export name),
never to equate lexical bindings.

The inventory interface should be independently useful to another dependent
that consumes `SModuleBuilder::take_functions` and compiles every `SFunc`
immediately. jsaw opts into it only in HCR mode. Normal whole-module conversion
retains its current interface.

### 3.2 `portal-jsc-waffle`: revision planner and fragment cache

A new optional `revision` module owns all HCR compiler policy:

```rust
pub struct RevisionCompiler<C: FragmentCache> { /* opaque */ }

pub struct RevisionRequest<'a> {
    pub entry: &'a str,
    pub sources: &'a BTreeMap<String, String>,
    pub options: ConvertOptions,
    pub profile: ReloadProfile,
    pub base: Option<RevisionId>,
}

pub struct RevisionOutput {
    pub revision: RevisionManifest,
    pub artifact: RevisionArtifact,
    pub delta: Option<ModuleDelta>,
}

impl<C: FragmentCache> RevisionCompiler<C> {
    pub fn compile(&mut self, request: RevisionRequest<'_>)
        -> Result<RevisionOutput, ConvertError>;
}
```

The interface is intentionally a single `compile` operation. The revision
planner hides source hashing, graph comparison, cache lookup, invalidation,
fragment lowering, deterministic assembly, and delta eligibility. Callers do
not stitch together partial Waffle modules or reason about function indices.

`FragmentCache` has at least two adapters:

- `MemoryFragmentCache` for editor/daemon sessions;
- `DirectoryFragmentCache` for the wasip1 compiler and CI, using content-
  addressed files and atomic rename. It is bounded by LRU/size policy and treats
  corruption as a cache miss, never as trusted compiler input.

### 3.3 Runtime: revision activation

The runtime implements one selected reload profile. It receives only the
manifest and immutable artifacts/delta; it never interprets JS dependency
rules.

```text
editor/host -> jsaw RevisionCompiler -> RevisionManifest + artifact/delta
                                       |
                                       v
                         runtime-specific revision activator
```

The activator reports the active `RevisionId`, whether it applied a delta or
fell back to re-instantiation, and any state-migration result. jsaw does not
claim that a runtime can apply a delta merely because it was emitted.

## 4. Fingerprints and cache keys

### 4.1 Function semantic fingerprint

A function cache hit requires equality of a canonical, versioned encoding of:

1. the resolved function AST/CFG/SSA semantics, excluding spans, comments, and
   source formatting;
2. hygienic binding structure and capture relationships (using `Ident`/`Id`
   equality during construction, never just identifier text);
3. literal values, operators, parameter/rest/default shape, async/generator
   flags, and nested function structural paths;
4. direct static dependencies by their stable `FunctionKey` and required ABI
   fingerprint;
5. the representation/layout assumptions that the fragment depends on:
   return-kind ABI, context/capture layout, object shapes, primordials,
   typed-array/DataView requirements, and feature closure;
6. compiler/frontend schema, jsaw-core IR schema, Waffle lowering schema,
   target profile, and relevant `ConvertOptions`.

Use a domain-separated cryptographic digest (for example BLAKE3):
`jsaw.function.v1\0 <canonical bytes>`. The canonical encoder must be unit-tested
with formatting-only equivalence and semantic-change inequality fixtures.

A hash is a cache locator, not the only correctness proof. Cache records carry
the full schema/version, source key, dependency fingerprints, and structural
contract; mismatches are misses.

### 4.2 Module fingerprint

The module-level fingerprint covers syntax outside independently cached
functions: static imports, export/re-export metadata, function declaration
ordering, top-level executable statements, source mode, and all module-level
binding/context layout. Its dependency graph includes imported module export
surface fingerprints.

A module whose function body is unchanged may still be invalidated when its
import/export surface, top-level statement, or context layout changes.

### 4.3 Revision identity

- `RevisionId`: monotonic session sequence, useful to hosts and logs.
- `ContentRevisionId`: digest of entry key, sorted module fingerprints, compiler
  schema/profile/options, and artifact ABI manifest. Equal content IDs must
  produce byte-identical revision artifacts.
- `BaseRevisionId`: the content ID against which a delta was planned. A runtime
  must reject a delta if its active content ID differs.

## 5. Dependency graph and invalidation

The planner maintains a typed graph:

```text
module metadata node
  -> top-level init node
  -> declared function nodes
  -> nested/literal function nodes
  -> static import/export surface nodes
```

Edges are labeled, because not all changes have the same blast radius:

| Edge | A change invalidates |
| --- | --- |
| body call/reference | direct callers only if compiled with static direct calls; not callers in dispatch mode when ABI is unchanged |
| capture/context layout | owner function, nested functions, all closures constructed by it |
| function ABI/return representation | every direct caller and dispatch cell signature |
| import/export surface | importing modules, re-exports, entry export wrappers |
| top-level execution | the entire module-init chain and normally the full revision |
| shared representation/object shape | every fragment whose layout contract includes it |

The invalidation algorithm is deliberately conservative:

1. Parse changed source modules and inventory their function/module
   fingerprints.
2. Reuse exact cache matches only after validating all recorded dependency
   fingerprints and layout contracts.
3. Seed the dirty set with changed/new/deleted functions, changed module
   metadata, and changed export/import surfaces.
4. Traverse reverse typed edges. Stop at a dispatch edge only when its stable
   signature and handle contract still match; otherwise continue.
5. Classify the result into `BodyOnly`, `CompatibleReload`, or
   `FullRevisionRequired`.
6. Compile dirty fragments, assemble a complete revision, and consider a delta
   only for `CompatibleReload`.

Deletion, duplicate declaration changes, resolver failures, unsupported syntax,
or missing cache records never result in a partial update. They produce an
explicit compile error or a full-revision requirement.

## 6. Reload profiles

### 6.1 `restart`

Compile a revision artifact. The host stops/restarts the application or test
fixture. This is the first profile and works for all output targets. It gives
cache reuse and a precise reload plan even before any runtime supports live
activation.

### 6.2 `reinstantiate`

Compile a new complete immutable WasmGC module. The runtime instantiates it and
atomically routes new entry calls to the new instance. Existing calls keep their
old instance; no live stack is rewritten.

Default state policy is **reset**. A host may provide an explicit,
versioned state-transfer adapter, but jsaw only enables it when the revision
manifest says the module context layout is compatible. If it is not compatible,
the runtime must reset/restart rather than guessing.

### 6.3 `dispatch-delta`

This profile requires a runtime designed around stable dispatch cells from the
first revision. HCR-mode lowering must intentionally avoid current direct
native-call assumptions at reloadable edges:

```text
JS call -> stable function handle/cell -> current revision implementation
```

Each cell has a stable key, a declared calling convention, and a revision slot.
A function-body-only update ships a replacement implementation plus operations
such as:

```json
{
  "baseContentRevision": "…",
  "targetContentRevision": "…",
  "operations": [
    {"replaceFunction", "key": "src/math.js#add@0", "abi": "…", "body": "…"},
    {"setEntry", "export": "run", "key": "src/main.js#run@0"}
  ]
}
```

The exact body encoding is runtime-specific:

- a runtime may accept a small Wasm module whose imports are stable cells;
- it may use engine APIs to compile functions and update an indirect table;
- it may reject deltas and request the complete revision artifact.

The delta format must **not** expose Waffle function indices, Wasm type indices,
or mutable edits to a standard `.wasm` binary. Those are revision-local. It
contains stable function keys, ABI fingerprints, content IDs, and opaque runtime
payload references only.

HCR-mode dispatch costs an indirect call and may disable some direct-call and
inlining opportunities. Release mode retains current direct dispatch. The
profile is therefore in every cache key and revision manifest.

## 7. Compatibility classification

A replacement may be emitted as a delta only when all of these hold:

- same stable `FunctionKey`;
- same externally callable ABI: arity/missing-argument behavior, native return
  representation, receiver/arguments convention, and WasmGC signature;
- same closure capture/context layout and same function-handle cell ABI;
- same required representation/object-shape contracts, or the runtime confirms
  compatible versioned adapters;
- no changed top-level executable statement, import/export surface, or module
  initialization ordering affecting the function;
- all affected edges are dispatch edges or have been recompiled into the delta.

Examples:

| Edit | Classification |
| --- | --- |
| `return x + 1` → `return x + 2`, same captures/ABI | body-only delta candidate |
| change a private helper body, callers use stable cells | helper replacement candidate |
| change helper arity or return representation | recompile dependents; possibly full revision |
| add/remove capture from a closure | full revision unless a state/layout migration adapter exists |
| change `const` at module top level | full revision required |
| modify export/re-export/import list | full revision required |
| change class/instance/object layout used across revisions | full revision required |

Top-level function declaration replacement deserves special care. The revision
manifest defines its visible switch point. Existing closures/function objects
continue to call their captured revision unless they were created through a
reloadable stable cell. This avoids violating in-flight-call behavior while
allowing new imports/entry calls to observe the new revision.

## 8. Artifact layout and CLI protocol

Keep manifest version 1 as the ordinary batch compile protocol. Add a separate,
explicit version-2 HCR request rather than overloading v1 semantics:

```json
{
  "version": 2,
  "mode": "revision",
  "entry": "src/main.js",
  "modules": ["src/main.js", "src/math.js"],
  "baseRevision": "optional-content-id",
  "profile": "dispatch-delta",
  "options": {"numericExports": true, "gcExportSuffix": null},
  "emit": {"wasm": "module.wasm", "revision": "revision.json", "delta": "delta/"}
}
```

The result records:

- content/base revision IDs;
- profile and activation requirements;
- changed/reused/recompiled function keys and invalidation reasons;
- artifact paths and SHA-256/BLAKE3 digests;
- whether a delta is present, rejected, or intentionally omitted;
- required reset/migration decision.

The cache root is supplied separately (CLI argument or host preopen), never
implicitly inferred from the source root. For WASI it is a dedicated writable
preopen. Cache entries are content-addressed under a schema-version directory;
write to a temporary sibling, fsync where supported, then atomically rename.

## 9. Implementation phases

### Phase 1 — inventory and stable hashing

- Add canonical function/module fingerprint encoding in jsaw-core.
- Give `SModuleBuilder` an optional inventory path that composes with its
  existing `take_functions` interface.
- Record hygienic lexical relationships without flattening `Id`/`Ident`.
- Add `MemoryFragmentCache` and cache-only tests; no runtime reload yet.

**Exit:** formatting-only edits reuse fragments; semantic/capture/import/export
edits invalidate exactly documented nodes.

### Phase 2 — revision planner and complete artifacts

- Add `portal-jsc-waffle::revision::RevisionCompiler`.
- Cache source ingestion and backend-independent fragment information, then
  deterministically assemble a complete Waffle module revision.
- Emit `RevisionManifest` and a complete WasmGC/Java/Swift revision.
- Add HCR manifest v2 to `jsaw-wasi-bin`; preserve v1 byte-for-byte behavior.

**Exit:** two successive revisions have deterministic IDs, a complete reload
plan, and a host can use `restart`/`reinstantiate` correctly.

### Phase 3 — state and activation contracts

- Define module-context layout fingerprints and conservative reset policy.
- Add host-facing reinstantiate reference adapter for Wasmtime tests, selecting
  a new instance only between exported calls.
- Specify optional state-transfer hooks with explicit old/new layout hashes.

**Exit:** body-only edits can switch instances safely; incompatible state
changes are rejected or restart-required, never silently migrated.

### Phase 4 — dispatch-mode lowering

- Add an explicit `ReloadProfile::DispatchDelta` lowering mode.
- Replace reloadable direct calls with stable handle/cell calls; retain current
  direct-call lowering for release mode.
- Freeze dispatch ABI and function-key-to-cell assignment in a revision family.
- Recompile only the reverse dependency closure required by ABI/layout changes.

**Exit:** the compiler can build two compatible revisions whose reload plan
contains only changed implementation cells.

### Phase 5 — runtime-specific deltas

- Define the opaque payload adapter trait and one reference runtime adapter.
- Produce/validate base-to-target `ModuleDelta` payloads.
- Require runtime base revision validation and atomic all-or-nothing updates.
- Fall back automatically to complete reinstantiate artifacts when the runtime
  cannot apply a delta.

**Exit:** a supported runtime applies a one-function compatible replacement and
new calls observe it without re-instantiating the whole application.

### Phase 6 — Gradle/editor integration

- Keep Gradle production tasks on whole-set cache keys by default.
- Add an opt-in development daemon/watch task using the directory cache and
  revision protocol.
- Publish revision events for IDE/browser/mobile hosts; never make build-cache
  correctness depend on a mutable HCR cache.

## 10. Verification matrix

### Compiler/cache tests

- whitespace/comment edits: same function and module fingerprints;
- changed literal/operator: only the relevant body fingerprint changes;
- hygienically distinct same-text bindings: distinct fingerprints and graph
  edges;
- changed captured variable/parameter/rest shape: owner/nested closure
  invalidation;
- renamed/exported/re-exported/imported bindings: module-surface invalidation;
- corrupted/outdated cache record: miss and clean rebuild;
- deterministic cache/revision artifact bytes across insertion orders.

### Revision tests

- private helper body edit produces a plan that identifies the helper and its
  needed callers under release/direct mode;
- same edit under dispatch mode produces a single compatible replacement;
- arity/return ABI change invalidates callers and rejects a body-only delta;
- top-level initialization edit requires full revision/reset;
- deletion and re-export ambiguity fail with explicit diagnostics;
- base revision mismatch makes the runtime reject the delta.

### Runtime tests

- Wasmtime reference reinstantiate adapter: in-flight exported call finishes in
  old revision; next exported call enters new revision;
- state-reset behavior is observable and deterministic;
- supported delta adapter swaps one stable cell atomically;
- unsupported delta adapter reports fallback and activates the complete
  revision;
- generated Java/Swift revision outputs compile independently; no claim of live
  patching is made without a host-specific test.

## 11. Risks and decisions

- **Fragment boundaries may be shallower than they look.** Context/object-shape
  and return-kind decisions can couple many functions. The planner records
  these dependencies and falls back rather than inventing unsafe independence.
- **HCR dispatch may regress performance.** It is profile-gated and excluded
  from release artifacts/cache keys.
- **State semantics are the hard part.** Default reset is honest. Migration is
  opt-in, explicitly versioned, and runtime-owned.
- **Cross-runtime deltas are not portable.** The portable unit is a revision
  manifest plus complete artifacts; deltas are a negotiated optimization.
- **Cache poisoning/staleness.** Content addressing, schema/versioned keys,
  dependency revalidation, bounded storage, and fail-closed misses prevent a
  stale fragment from becoming a silent correctness bug.
