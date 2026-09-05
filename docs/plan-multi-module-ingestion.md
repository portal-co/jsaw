# Plan: multi-module ESM ingestion in `portal-jsc-waffle`

Today the WasmGC backend ingests exactly one thing: a single script function
(`convert`) or a single ES module (`convert_module`). This plan adds ingestion
of a *set* of ES modules with three properties:

1. The caller selects a **main (entry) module** from the set.
2. The generated Wasm module **exports everything exported by the main
   module** — not just locally-declared function exports, but re-exports and
   star re-exports resolved through the set.
3. **Cross-module imports resolve via relative paths**: `import { f } from
   './lib.js'` in any module of the set links against `lib.js` from the same
   set. No host imports, no bundler.

All in `crates/portal-jsc-waffle`.

---

## Current state

### Entry points (`conv.rs`, re-exported by `lib.rs`)

- `convert(root: &SFunc)` — one script function, no Wasm exports.
- `convert_module(source: &SModule, module: &mut Module, options:
  &ConvertOptions)` — one ES module. Export surface is limited to what
  `source_function_exports` can materialize:
  - `ExportSpec::Local { local, exported }` where `local` resolves in
    `source.funcs` (hoisted function declarations),
  - `ExportSpec::DefaultFunc { func_name }` (exported as `default`).
  - `ExportSpec::DefaultExpr`, `ExportSpec::Reexport`, and
    `ExportSpec::ReexportAll` are *name-space reservations only*
    (`source_export_names` feeds collision checks) — no export is emitted and
    nothing errors.

### What is silently dropped today

- `SModule.body` (top-level executable statements) is never lowered by
  `convert_module`; only exported functions are.
- `SModule.imports` is never consulted. Every imported binding falls out of
  SSA as `SValue::LoadId`, which lowers to a **property get on the fresh
  per-call context** (`lower_statement`, the `LoadId` arm → `get_property`).
  An unresolved import therefore reads `undefined` (or a same-named global /
  primordial) at runtime — wrong, silently.

### Machinery that matters for linking

- `Converter` lowers a worklist (`pending: VecDeque<&'a SFunc>`) into one
  Waffle `Module`. Functions are memoized by *source address*
  (`Self::key(func) = func as *const SFunc as usize`), tags mint from a
  global `next_function_tag` counter, and generated names are counter-based
  (`js_body_{n}`, `js_adapter_{n}`) — none of this assumes a single source
  module.
- `LowerValue::FunctionRef { value, info, fresh }` already models "a value
  whose origin is a specific lowered `SFunc`". Call sites against it emit a
  tag check and dispatch the native body directly. Today it is produced only
  for single-assignment function literals (`function_literal_locals`), but
  the call-site machinery is exactly what a cross-module linked function
  needs.
- A hoisted function *within* one module is referenced as `SValue::Item {
  item: Item::Func { .. } }` — an inline `SFunc` reference, so intra-module
  calls never touch names. Only *cross-module* references survive as
  `LoadId` externals.
- `SModule::import_mapper()` → `mapper.import_of(&Id) -> Option<(specifier,
  ImportMap)>` with `ImportMap::{Default, Star, Named { name }}` exists in
  jsaw-core and is currently unused by this crate.
- `ConvertOptions { numeric_exports, gc_export_suffix }` with duplicate-name
  collision checks against both generated and declared names.
- Test harness (`tests/e2e.rs`): parse → `CfgModule` → `TModule` → `SModule`
  → convert → waffle `validate` + `wasmparser` validate → execute in
  wasmtime *and* node.

---

## Design

### 1. Module set and entry point

```rust
/// A closed set of ES modules keyed by canonical relative path
/// (e.g. `"src/main.js"`). Deterministic ordering (`BTreeMap`) keeps
/// generated Wasm reproducible.
pub struct ModuleSet<'s> {
    modules: BTreeMap<String, &'s SModule>,
}

impl<'s> ModuleSet<'s> {
    pub fn new() -> Self;
    /// Insert a module. Duplicate paths are a `ConvertError`.
    pub fn insert(&mut self, path: impl Into<String>, module: &'s SModule)
        -> Result<(), ConvertError>;
    /// Convenience for the existing single-module callers.
    pub fn single(path: impl Into<String>, module: &'s SModule) -> Self;
}

/// Lower a set of ES modules into one WasmGC module. Only `entry`'s
/// exports become Wasm exports.
pub fn convert_modules<'a, 'wasm>(
    entry: &str,
    set: &'a ModuleSet<'a>,
    module: &mut Module<'wasm>,
    options: &ConvertOptions,
) -> Result<(), ConvertError>;
```

- `convert_module(source, module, options)` becomes a thin wrapper:
  `convert_modules(path, &ModuleSet::single(path, source), module, options)`
  — no behavior change for existing callers, and the existing e2e tests keep
  exercising it.
- All `SFunc` borrows now come from `&'a ModuleSet<'a>`, so the existing
  `Converter<'a>` lifetime keeps working unchanged. The address-keyed
  function memoization remains sound: distinct modules hold distinct `SFunc`s,
  hence distinct addresses; two entries can never share a function.
- The entry path must exist in the set, else `ConvertError::invalid`.

### 2. Relative-path resolution

`resolve_specifier(importer: &str, specifier: &str) -> Result<String,
ConvertError>`:

- Only relative specifiers (`./…`, `../…`) are supported. Bare specifiers
  (`"lodash"`, node builtins) are rejected with an explicit error in this
  milestone — there is no host-module concept in a standalone WasmGC module.
- Resolution: join against the *importer's* directory, normalize `.`/`..`
  segments lexically, require the result to be a key in the set.
- Exact-key matching only for this milestone (extension optional):
  a fixture keyed `"lib.js"` is found by `"./lib.js"`, not `"./lib"`.
  Rationale: keeps the resolver a pure string function with no FS or
  extension-policy surprises; a `.js`-appending fallback can be added later
  behind an option if fixtures demand it.
- Escaping the set (`../../outside.js` that resolves to a missing key) is an
  error naming both importer and specifier, so the host can diagnose a
  missing module.

### 3. Import linking — the core change

Per module, build an **import table** before lowering:

- Walk the module's `externals()` / intercept `LoadId` ids and classify each
  via `import_mapper().import_of(&id)`:
  - `Named { name }` → resolve `name` in the target module's export surface:
    a hoisted function in `funcs`, a `DefaultFunc` for `name == "default"`,
    or follow a `Reexport` chain (cycle-guarded) into further modules.
  - `Default` → the target's `DefaultFunc` (or `default` re-export chain).
  - `Star` → phase 1: explicit `ConvertError` (namespace objects need
    module-instance state; see phase 2).
- A resolution succeeds only if the final target is a hoisted **function
  declaration** (`SModule::funcs`). Anything else (non-function local
  export, whose value only exists after top-level body evaluation) is a
  phase-1 error — deliberately not a silent fallback, so no module set
  produces silently-wrong code.

Lowering change, in `lower_statement`'s `SValue::LoadId` arm — consult the
*current module's* import table before the existing `function_literal_locals`
lookup:

```
LoadId(id):
  1. import table hit, function target → ensure_function(target_sfunc)
     → LowerValue::FunctionRef { fresh: false }
  2. function_literal_locals hit → (unchanged)
  3. otherwise → context property get (unchanged; globals/primordials)
```

Why this is cheap and sound:

- It reuses the single-assignment-literal machinery end to end: every call
  site against a `FunctionRef` already emits the tag check + guarded direct
  dispatch, and the property-read representation, boxing, and join behavior
  all match.
- Function-declaration exports are immutable bindings, so a linked import is
  *statically* the target function forever; the runtime tag check always
  passes. (A later optimization can skip the check — see work item 6.)
- Mutual recursion across modules just works: `ensure_function` registers
  before `lower_all` drains, which is how intra-module recursion already
  terminates.

Converter bookkeeping changes:

- Track the owning module of the function currently being lowered. Change
  `pending` to carry `(module_key, &SFunc)` (or keep a parallel
  `BTreeMap<usize, String>` keyed like `functions`), and expose the current
  module's import table to `lower_statement`. Nested closures inherit the
  module; the existing per-top-level-function resets
  (`shadowed_names`, `function_literal_locals`) are unaffected.
- `classify_return_call` / `analyze_return_kinds` keep treating cross-module
  callees conservatively (`Reference`) in this milestone — a linked call
  site is still `LoadId`-based, so it cannot hit the `Item::Func` fast path.
  Crossing the provenance/return-kind analysis over import edges is listed
  as follow-up work (it composes with
  `docs/plan-function-provenance-and-return-continuations.md`).

### 4. Export everything from the main module

Replace `source_function_exports(source)` with a resolver over the set that
flattens the main module's full export surface into `Vec<(name,
ExportTarget)>`:

| Export form | Phase 1 behavior |
|---|---|
| `Local` (function) | unchanged: numeric + optional GC-adapter exports |
| `DefaultFunc` | unchanged: exported as `default` |
| `Reexport { source, names }` | resolve `source` through the set; for each `(orig, exported)`, flatten the target's surface (following re-export chains, cycle-guarded) and export the resolved function under `exported`. `("default", y)` resolves the target's `DefaultFunc`. |
| `ReexportAll { source, ns: None }` (`export * from`) | enumerate the flattened surface of `source` minus `default`, minus any name the main module declares locally (local declarations shadow star exports). A name produced by two different star sources is an error (ESM ambiguity, conservative subset of the spec's "unbound" rule). Function targets only. |
| `ReexportAll { ns: Some(_) }` (`export * as ns`) | phase 2 — needs a namespace object at runtime. Phase 1: explicit `ConvertError`. |
| `DefaultExpr` | phase 2 — needs top-level body lowering. Phase 1: explicit `ConvertError` (today it is silently skipped; the milestone promise is "everything", so skipping must become loud). |

Non-main modules emit **zero** Wasm exports; their functions enter the Wasm
function pool only as link targets. Existing collision checks (duplicate
export names, GC-suffix collisions) already operate on the flattened list and
need no structural change — re-export flattening just feeds them more names.

### 5. Cycles

- Function-declaration-only import cycles are legal ESM (hoisting) and are
  supported: linking is by `SFunc` identity, and bodies are lowered from the
  worklist after registration, so mutual recursion across modules terminates
  exactly like intra-module recursion.
- A cycle encountered while *flattening* re-export chains, or a cycle whose
  resolution would require top-level evaluation (non-function binding), is a
  `ConvertError` with the cycle path in the message.

### 6. Phase 2 sketch (explicitly out of this milestone's scope)

Module instances: one context object per module built at init time in
post-order dependency order; imports of *values* become property reads on
the target module's context (live bindings); namespace objects from the
same surface; top-level bodies lowered as `js_module_init_*` functions with
an exported main-init entry. This is the natural extension point but needs
the context-builder to be parameterized per module and an init-order
algorithm; keeping it separate keeps this milestone reviewable.

---

## Work items

1. **`ModuleSet` + `convert_modules` skeleton.** New public API in
   `conv.rs`/`lib.rs`, `resolve_specifier`, entry validation, `convert_module`
   reimplemented as a wrapper. No behavior change for single-module input;
   existing e2e suite green.
2. **Import tables.** Per-module classification of externals via
   `import_mapper`, resolution through `funcs` / re-export chains, phase-1
   errors for `Star` and non-function targets. Unit-testable in isolation
   (table construction without Wasm emission).
3. **Static linking in `LoadId` lowering.** Owning-module tracking, import
   table consult before `function_literal_locals`, `FunctionRef` production.
   Test: main module calls a function imported from a sibling; executes
   correctly in wasmtime and node.
4. **Full main-module export surface.** Flattening resolver for `Reexport`
   and `ReexportAll` with shadowing/conflict rules; `DefaultExpr` /
   `export * as ns` become explicit errors; collision checks verified against
   flattened names.
5. **Error-surface pass.** Every failure mode names importer path,
   specifier, and (for exports) the export name; no silent skips remain on
   the main module's surface.
6. **(Follow-up, optional)** Skip the runtime tag check for linked imports
   (immutability is compile-time knowledge), and thread import edges through
   `analyze_return_kinds` / provenance so cross-module calls can hit the
   direct-call fast paths from
   `docs/plan-function-provenance-and-return-continuations.md`.

## Testing

Extend the e2e harness with a multi-file entry point —
`lower_modules(fixture: &[(path, source)], entry, options)` that lowers each
file through the existing CFG→TAC→SSA pipeline into a `ModuleSet` — then:

- **Linking:** `main.js` imports `{ add } from './lib.js'` and exports a
  wrapper calling it; numeric result verified in wasmtime and node.
- **Transitive chains:** `main → a → b → c`, all function imports.
- **Mutual recursion across modules:** `a.f` calls `b.g`, `b.g` calls `a.f`,
  with a base case.
- **Default imports:** `import d from './lib.js'` against
  `export default function`.
- **Re-exports:** `export { x as y } from './lib.js'`, chained re-exports,
  `export * from './lib.js'`, star shadowed by a local declaration.
- **Export surface:** assert the exact Wasm export name list (including
  `$gc` suffixed adapters) for a main module exercising every phase-1 form.
- **Errors:** missing module, bare specifier, `import * as`, non-function
  import, `DefaultExpr` on the main module, ambiguous star, re-export cycle,
  duplicate set path, missing entry — each asserting `ConvertError` with a
  diagnostic naming importer + specifier.
- **Regression:** the entire existing single-module suite passes unchanged
  through the `convert_module` wrapper.

## Non-goals

- Host imports / bare specifiers / node builtins; dynamic `import()`;
  top-level `await`.
- Live-binding mutation across modules (`export let x` reassigned by the
  exporter) — function declarations only until phase 2.
- Namespace objects as values (`import * as ns`, `export * as ns`).
- Lowering top-level statements of any module (phase 2 init work).
- Tree shaking, dead-code elimination, or emitting separate Wasm modules —
  the output remains one WasmGC module.
- Wasm-level `import` of host functions: linking is entirely internal.

---

## Implementation status (recorded at commit time)

Work items 1–5 are implemented (`src/linker.rs` + `convert_modules` in
`src/conv.rs`; item 6 remains open follow-up). Deviations from the design
above, all deliberate:

- **Referenced bindings only.** The import table is built from each
  module's `refs()` — not `externals()`, because jsaw-core records imported
  bindings in each function's `decls` set and `externals()` filters those
  out (this cost a debugging round-trip to discover). Unreferenced imports
  are still validated eagerly for every module in the set (closed-set
  guarantee), but never minted.
- **Per-load function-object minting.** A linked import mints the target's
  function object at each load site instead of reading a shared module
  instance, so two loads of the same import in one activation produce
  distinct function objects. Inconsequential while module top-level state
  (phase 2) does not exist; revisiting is part of phase 2.
- **Re-export resolution resolves at the re-exporting module.**
  `export { x as y } from './mod.js'` resolves `x` in the *target* module
  directly (equivalent semantics, sharper diagnostics than routing through
  the main module's own surface).
- **Star-shadowing semantics via `local_names`.** The ambiguity check
  compares star-provided names against the main module's local declaration
  names only (an earlier draft fed star names back into the same set and
  silently disabled the check — the tests caught it).
- The guarded direct-call machinery for `LowerValue::FunctionRef` proved
  to need no changes: linked imports reuse the single-assignment-literal
  path end to end, including the runtime tag check (always true for
  immutable function-declaration exports).
