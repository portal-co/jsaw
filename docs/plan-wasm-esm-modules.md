# Plan: Wasm ES modules as first-class module-set members

**Status:** accepted; tracked as portal-co/jsaw#14. Builds on the
closed-world [`ModuleSet`] linker (`crates/portal-jsc-waffle/src/linker.rs`),
the FFI manifest boundary (DreamComp #37, `crates/portal-jsc-waffle/src/ffi.rs`),
and the host-import machinery of
`docs/plan-wasm-handle-imports-exports.md`.

## 1. Goal

Today a [`ModuleSet`] is a closed world of **JavaScript** ES modules only:
`ModuleSet::insert(path, &SModule)` accepts SSA-lowered JS, and the only
non-JS imports are `wasm:` host functions whose signatures are declared
structurally in the FFI manifest, plus `~ffi` / `~weakmap` intrinsics.

This plan makes **Wasm binary modules** first-class members of that closed
world, with the semantics of the WebAssembly/ES-Module-Integration proposal
adapted to jsaw's closed set:

1. A JS module can import a sibling `.wasm` module with ordinary ESM syntax:
   `import { add } from "./math.wasm"`. The Wasm module's **export section**
   is the import surface; no FFI manifest entry, no name-suffix grammar, no
   hand-written declaration is needed for the Wasm module itself.
2. Imported Wasm modules are accepted at five explicit feature levels:
   **MVP**, **GC**, **multi-memory**, **multi-value**, and **memory64**.
   Every binary is validated and classified against a feature profile before
   linking; anything outside the accepted profile fails closed with a named
   diagnostic.
3. The Wasm module is **ingested into Waffle IR** (via `waffle-frontend`,
   already a workspace dependency through the `portal-pc-waffle` patch) and
   merged into jsaw's single output module. It is therefore not a sidecar
   import at runtime: it passes through jsaw's own Wasm output and is
   recompiled by every downstream target — WasmGC encode, CoreGC core-Wasm
   lowering, `nano_mvp`, `low_emit`, and (behind an audit) the mobile
   JVM/Swift emitters.

The result: a JS program can freely mix JS modules and precompiled Wasm
modules in one closed set, and the whole graph still compiles to one
artifact per target, with no host-side dynamic linking.

## 2. Non-goals

Each deferred item is tracked as its own GitHub issue:

- **Component Model / WASI p2** (portal-co/jsaw#5). No `.component`
  binaries, no WIT, no canonical ABI. Core modules only.
- **Dynamic linking** (portal-co/jsaw#6). No shared-memory dynamic linking
  conventions, no `dlopen`-style runtime loading. The set stays closed and
  statically resolved.
- **Satisfying Wasm imports from the JS host implicitly.** A Wasm module's
  own import section is resolved only through the FFI manifest
  (`wasm:`-equivalent structural declarations) or intra-set edges (§5.4).
  There is no ambient host namespace.
- **Threads, atomics, SIMD, exceptions, tail calls, JSPI, relaxed-SIMD**
  (portal-co/jsaw#7). Fail-closed at validation until a separate plan opts
  each in.
- **JS API fidelity** (portal-co/jsaw#8). `WebAssembly.Module` / `Instance`
  objects, `WebAssembly.instantiateStreaming`, and reflective inspection of
  the Wasm module from JS are not provided. Bindings behave like ordinary
  imported functions; there is no module namespace object in the MVP of
  this plan.
- Changing the semantics of existing `wasm:` FFI host imports, `~ffi`, or
  `~weakmap`.

## 3. Source convention

### 3.1 Specifier and ingestion

A relative specifier that resolves to a `.wasm` key in the module set names
a Wasm binary module:

```js
import { add, MEMORY } from "./math.wasm";
export function run(a, b) { return add(a, b); }
```

- The embedder supplies binaries alongside sources:
  `ModuleSet::insert_wasm(path, bytes)` (new; `insert` remains JS-only and
  rejects `.wasm` keys to keep the sum explicit). `jsaw-wasi-bin`'s JSON
  manifest gains a `wasm: [{ path, base64 }]` array next to its source
  array.
- Resolution reuses `resolve_specifier` unchanged: `./math.wasm` from
  `src/main.js` looks up the exact key `src/math.wasm`. No extension
  fallback, no import-map lookup.
- The closed-world rule is unchanged: every specifier of every module must
  resolve, and a `.wasm` key must be present at ingestion time — a missing
  binary is the same class of error as a missing JS module.
- Duplicate `insert_wasm` paths, and a path present as both JS and Wasm,
  are `ConvertError`s, matching the existing duplicate-path rule.

### 3.2 Import and export surface

Following the Wasm/ESM-integration proposal, restricted:

- **Named imports** bind Wasm **function exports**. `import { add } from
  "./math.wasm"` binds the export named `add`; the local alias is an
  ordinary hygienic `Ident` and the import table stays keyed by `Ident`,
  never by rendered text.
- **Memory/global/table exports are not importable values in this plan.**
  A named import resolving to a non-function Wasm export is a named error.
  (Sharing linear memory with JS is a deliberate later design, not an
  accident of ingestion.)
- **Default imports are rejected.** A Wasm module has no default export.
- **Namespace imports (`import * as m`) are rejected initially**, matching
  the existing closed-set rejection of star imports for JS modules. A later
  phase may build a frozen namespace object of the function exports.
- **Re-exporting a Wasm function through a JS module works** by the ordinary
  re-export resolution once `resolve_export` can return a Wasm-backed
  target (§5.2); `export { add } from "./math.wasm"` is the canonical
  passthrough form.

### 3.3 Feature profiles

Validation uses `wasmparser` (already a workspace dependency,
`features = ["validate"]`) with an explicit `WasmFeatures` set per profile —
the same pattern as `crates/jsaw-wasi-bin/tests/wasi_run.rs:164` and
`crates/portal-jsc-waffle/src/nano_mvp/tests.rs:24`. Profiles are
**cumulative and named**, so diagnostics and manifests can say exactly what
a module required:

| Profile | wasmparser features | Notes |
| --- | --- | --- |
| `mvp` | MVP + `mutable-global` | Baseline. Sign-extension, bulk-memory, reference-types, and SIMD are each opt-in later; MVP here means exactly the 1.0 core plus mutable globals. |
| `multi-value` | + `MULTI_VALUE` | Block/func multi-results. Waffle already round-trips multi-result operators (see the `Cargo.toml` patch comment about the multi-result `lower_inst` fix). |
| `memory64` | + `MEMORY64` | 64-bit linear memory. Waffle IR carries `memory64` on `MemoryData` (waffle-frontend `frontend.rs:147,244`). |
| `multi-memory` | + `MULTI_MEMORY` | Multiple named memories. Waffle IR is `EntityVec<Memory, MemoryData>` with `memory_index` operands (`frontend.rs:264`), so this is representable; the audit phase must confirm every backend lowers it. |
| `gc` | + `GC` (implies function-references, tail-call off) | WasmGC types. jsaw's own output is WasmGC, so the type universe exists; the hard part is type interning (§5.3). |

`ConvertOptions` gains `wasm_esm: WasmEsmOptions { max_profile: WasmProfile, … }`
defaulting to `mvp`-only so the feature is opt-in per embedder. A binary
requiring a feature above the configured ceiling fails at ingestion with a
diagnostic naming the feature, the module path, and the ceiling. There is no
silent downgrade and no silent feature stripping.

## 4. Architecture and ownership

Introduce one private deep module, `crates/portal-jsc-waffle/src/wasm_esm.rs`,
at the binary/IR seam. Like `wasm_boundary`, it owns a grammar (here: the
binary format acceptance rules) so that no caller re-derives them:

```rust
pub(crate) struct WasmModulePlan {
    pub path: String,                    // module-set key, for diagnostics
    pub profile: WasmProfile,            // exact feature requirement
    pub module: waffle::Module<'static>, // ingested, not yet merged
    pub exports: BTreeMap<String, WasmExport>,  // name -> Func | Memory | Global | Table
    pub imports: Vec<(String /*module*/, String /*field*/, WasmImportKind)>,
}

pub(crate) fn parse_wasm_module(
    path: &str,
    bytes: &[u8],
    ceiling: WasmProfile,
) -> Result<WasmModulePlan, ConvertError>;
```

Ownership boundaries (deleting `wasm_esm` must force every caller to
reimplement validation, profiling, and interning — that is the depth test):

- `wasm_esm` owns: binary validation, feature classification, Waffle
  ingestion, export-surface extraction, and deterministic diagnostics. It
  does **not** know about JS values, adapters, or backend policy.
- `linker` owns: the `.wasm` key distinction in `ModuleSet`, specifier
  resolution, and a new `ImportTarget::WasmModule { path, func }` variant
  (alongside `InternalFunction`, `WasmHost`, `Intrinsic`), still keyed by
  hygienic `Ident`.
- `conv` owns: JS↔Wasm value adapters at the call boundary and merging the
  ingested Waffle module into the output module (entity remapping).
- `coregc_lower` / `coregc_emit` own: passing imported Wasm functions
  through core lowering, and recording feature requirements in
  `CoreGcArtifact`.
- `nano_mvp`, `low_emit`, and the mobile emitters own: their own capability
  audits, which remain fail-closed until each target explicitly accepts a
  profile.
- `RuntimeRequirements` / the backend-plan manifest record the union of
  profiles actually used, so hosts learn "this artifact needs memory64"
  from metadata, not by re-parsing output bytes.

## 5. Design

### 5.1 Ingestion and merge

`waffle-frontend` already translates a validated Wasm binary into a Waffle
`Module` (the workspace patch pins a portal-co fork with multi-result and
GC support; Phase 0 audits the exact operator coverage). Merging is a
mechanical entity remap into the output module being built by `conv`:

1. **Types**: append the Wasm module's rec groups; intern structurally so a
   GC module importing the same shape twice shares types (matters for
   `ref.cast` across two imported modules and for §5.3).
2. **Functions**: append code bodies; rewrite type, func, memory, table,
   and global indices by the remap tables. Imported functions of the Wasm
   module become output-module imports only after §5.4 resolution; an
   unresolved Wasm import is an error at merge time, not a runtime trap.
3. **Memories/tables/globals**: append with fresh indices. Data/elem
   segments are rebased onto the remapped indices verbatim — this is what
   "passes through" means: the bytes, segments, and code bodies reach the
   output artifact unaltered in semantics, sharing one module.
4. **Exports**: the Wasm module's exports become **internal** entities of
   the combined module. They are re-exported from the final artifact only
   when reachable from a JS export surface (§5.2); otherwise they are
   private and may be dead-code eliminated by existing passes.
5. Determinism: Wasm modules merge in `BTreeMap` key order, matching the
   existing reproducibility rule for JS modules.

Name identity: the Wasm module's **name section is preserved**. Merged
functions, locals, memories, tables, and globals keep their binary names
(falling back to `<path>:<export>` for exports and `<path>:<index>` for
anonymous entities) as Waffle debug names, so they survive into the
artifact's name section and into diagnostics. Preservation is
deterministic: names are copied verbatim from the input binary, never
invented from host state, and two same-named functions from different
modules are disambiguated by the `<path>:` prefix only when no binary name
exists. No `$`-grammar is involved and no source-visible renaming occurs.

### 5.2 Linker integration

`ModuleSet` becomes a sum over two member kinds (JS `SModule` vs ingested
`WasmModulePlan`); `get`, `contains`, `keys` keep working, and
`resolve_specifier_target` returns an enum. `build_import_table` gains:

```rust
ImportTarget::WasmModule {
    path: String,            // module-set key of the .wasm member
    export: String,          // Wasm export name
    func: MergedFunc,        // handle into the merged module
    signature: WasmBoundarySignature, // derived from the binary, not the FFI manifest
}
```

Key consequences:

- The Wasm module's **own type section is the ABI declaration**. This is
  strictly more precise than the FFI manifest and needs no embedder
  bookkeeping. `WasmBoundarySignature` (from `wasm_boundary`) is reused and
  extended: `v` disappears (a real Wasm signature has real results),
  multi-result signatures appear under the `multi-value` profile, and
  ref tokens carry the actual heap type when `gc` is on.
- Eager validation extends to Wasm members: every named import from a
  `.wasm` specifier must name a real export of the binary; a dangling name
  fails at import-table construction even in dead source, matching the
  existing closed-world discipline.
- `resolve_export` and `flatten_module_export_surface` learn to return
  Wasm-backed targets so `export { add } from "./math.wasm"` and
  star-re-export through a JS shim both work. A Wasm function re-exported
  from the entry module gets a raw Wasm export wrapper with the **binary's
  own signature** — this is the natural generalization of the
  signature-marked raw exports from the host-ABI plan.

### 5.3 Calling convention: JS values across the Wasm boundary

`conv` generates a boxed-JS adapter per `(path, export)` exactly once,
reusing the coercion table of the host-import plan (§4.2 there):

| Wasm param/result type | JS → Wasm | Wasm → JS |
| --- | --- | --- |
| `i32` | `as_i32` | `Number` (Wasm `Integer` fast path) |
| `i64` | `as_i64` | `BigInt` |
| `f32` | `as_f64` + `F32DemoteF64` | `F64PromoteF32` + `Number` |
| `f64` | `as_f64` | `Number` |
| `funcref` / `externref` / nullable abstract refs | boxed value / host handle at CoreGC | `Reference` |
| concrete GC refs (`(ref $t)` etc.) | `ref.cast` after interning; mismatch traps, not silently coerce | `Reference` |
| multi-result | n/a | JS `Array` of the coerced results, in order |
| v128 / SIMD | rejected | rejected until SIMD is planned |

Two deliberate rules:

- **Multi-value returns become arrays.** JS has one result; the adapter
  packs multiple Wasm results into a fresh JS array. This keeps the JS-side
  calling convention total and is observable in tests.
- **GC type identity is by interning, not by name.** An imported GC module's
  `$point` and a jsaw `Repr` struct are different types even when
  shape-identical. Passing a JS object where `(ref $point)` is required
  traps on the adapter's `ref.cast` unless the value is an abstract
  supertype position (`anyref`, `eqref`, …). This is fail-closed and
  honest; structural interop of GC objects is a later plan, tracked as
  portal-co/jsaw#12.

Calls between two imported Wasm modules, and JS calls that stay in the Wasm
world, never cross adapters — only JS↔Wasm edges pay for boxing.

### 5.4 The Wasm module's own imports

A Wasm binary may itself import. Resolution order, all fail-closed:

1. **FFI manifest**: `(module, field)` matches a `ConvertOptions::ffi`
  entry → the existing host-import path, with the binary's declared import
  type checked against the manifest signature (mismatch is an error naming
  both).
2. **Intra-set edge**: `field` matches a named function export of another
  `.wasm` member whose import module name equals that member's path →
  direct internal call after merge (Wasm-to-Wasm linking, no adapter).
3. Otherwise: `ConvertError` listing the unresolved import. No implicit
  environment.

Non-function imports (memory, table, global) of a Wasm module are rejected
in this plan except through explicit future design; they interact with
multi-memory identity and CoreGC's static layout and must not be
half-supported.

### 5.5 Pass-through per backend

| Target | MVP | multi-value | memory64 | multi-memory | GC |
| --- | --- | --- | --- | --- | --- |
| Native WasmGC encode | ✅ direct | ✅ direct | ✅ direct (encoder feature) | ✅ direct (encoder feature) | ✅ direct |
| CoreGC core-Wasm | scalar-only funcs; ref params/results fail closed (the JS-facing handle ABI does not apply — these are internal calls) | ✅ (core feature) | ✅ emit memory64 output | ✅ emit multi-memory output | fail closed: GC module in, core out is a contradiction — reject the combination at `emit_coregc` |
| `nano_mvp` | ✅ (its whole purpose) | ✅ (`MULTI_VALUE` already enabled in its validator) | audit | audit | reject |
| `low_emit` | audit per profile | audit | audit | audit | reject |
| Mobile JVM/Swift | audit stays fail-closed this plan; translation of the MVP subset (i32/i64/f32/f64, one memory → `ByteBuffer`) is portal-co/jsaw#13 | reject | reject | reject | reject |

The audit pattern is the one the host-import plan established for mobile:
an unsupported feature is a loud rejection naming the profile and the
module path, never silent miscompilation. `CoreGcArtifact` and the
backend-plan manifest gain a `wasm_features` field recording the union
profile so downstream hosts can check instantiation capability.

### 5.6 Interaction with existing seams

- **`wasm:` host imports**: unchanged. A Wasm member may resolve its own
  imports through the FFI manifest; JS continues to use `wasm:` directly.
  The two grammars do not overlap (`.wasm` keys are paths; `wasm:` is a
  reserved scheme).
- **Source exception boundary**: a JS `throw` propagating through a call
  into Wasm member code traps at that boundary, same as a host import
  today — Wasm modules cannot participate in jsaw's internal
  exception-result protocol. Documented, tested, fail-closed.
- **SWC hygiene**: Wasm members have no `Ident`s; the JS side of every edge
  keeps `Ident`-keyed bookkeeping. The Wasm export *name* is a genuinely
  textual domain (an export name string), so `String` keys are correct
  there and only there.
- **`jsaw-wasi-bin`**: the JSON manifest grows `wasm: [{path, base64}]`;
  the Gradle plugin passes it through unchanged (byte blobs, no parsing in
  Kotlin).

## 6. Phases and commits

### Phase 0 — Waffle capability audit (Done)

Audit of the pinned `../waffle-` fork (portal-co/waffle- @ `0.6.0-alpha.1`,
workspace `wasmparser 0.240`):

| Feature | Frontend (parse) | IR | Backend (encode) | Verdict |
| --- | --- | --- | --- | --- |
| MVP operators | ~436 arms in `frontend.rs` plus 450-arm generic `From<&wasmparser::Operator>` in `waffle-ir/src/ops.rs` | yes | yes | green |
| Multi-value | `BlockType::FuncType` handled (`frontend.rs:1805`); multi-result `lower_inst` fix already relied upon by jsaw's `Cargo.toml` patch comment | yes | yes | green |
| memory64 | `MemoryData.memory64` carried (`frontend.rs:147,244`) | yes | encoded (`backend/mod.rs:1527,1590`) | green; note `initial_pages as usize` cast is fine on 64-bit hosts |
| Multi-memory | `MemoryArg { align, offset, memory: Memory }` carries the index (`ops.rs:24`); data segments read `memory_index` (`frontend.rs:264`) | yes | full memory section + per-memory data (`backend/mod.rs:1585,1732`) | green |
| GC types | rec groups iterated; `From<&wasmparser::SubType> for SignatureData` covers Func/Struct/Array (`waffle-ir/src/ir/module.rs:161`) | yes | yes | green |
| GC operators | generic conversion covers `StructNew`/`StructNewDefault`/`ArrayNew*`/`RefCast*` (`ops.rs:1890`–`1946`) | yes | encoded (`backend/mod.rs:1312`–`1326`) | green |
| Name section | `KnownCustom::Name` parsed (`frontend.rs:280`) | yes | — | green ("keep WASM names" feasible) |

Caveats recorded for later phases:

- **Passive data segments and passive/declared element segments are
  silently dropped** by the frontend (`DataKind::Passive => {}`,
  `frontend.rs:341` region). Strict MVP-profile validation rejects the
  binaries that need them (bulk-memory), so the profile validator is the
  guard; enabling bulk-memory later REQUIRES a waffle-frontend fix first.
  The `wasm_esm` validator must also reject passive segments explicitly so
  a future profile bump cannot silently corrupt.
- Active data/element offsets accept only constant `i32/i64/f32/f64`
  operators (`parse_init_expr`, `frontend.rs:55`–`75`); `global.get` bases
  are unsupported. Reject at validation.
- Table element ingestion assumes funcref tables
  (`func_elements.as_mut().unwrap()`); externref/typed tables panic.
  Reject non-funcref tables at validation until the GC phase fixes this.
- ~~Backend data-segment offsets hardcode `i32.const`~~ — fixed in
  `../waffle-` `1b55a86` during Phase 3 (memory64 requires `i64.const`).
  Element-segment offsets have the same hardcode for 64-bit tables
  (`table64`); funcref64 tables are out of profile scope for now and noted
  here for the tail-calls/function-references work (portal-co/jsaw#7).

Gate result: Phases 1–4 (MVP, multi-value, memory64, multi-memory) have no
waffle-side blockers. Phase 5 (GC) is green on coverage but must add the
table-kind validation above.

**Commit:** `docs: audit waffle wasm-feature coverage for wasm esm ingestion`.

### Phase 1 — ModuleSet ingestion and linker classification (Done)

1. `ModuleSet::insert_wasm` / `insert_wasm_with_profile`, `ModuleMember`
   sum-type members, `.wasm` specifier classification, `insert` rejecting
   `.wasm` keys.
2. `wasm_esm::parse_wasm_module` with all five profile validators
   (cumulative `wasmparser` feature sets; exact-requirement classification
   by validating lowest-up), export surface extraction, and rejection of
   the Phase 0 audit caveats (passive segments, non-const offsets,
   non-funcref tables) plus all Wasm-side imports.
3. `ImportTarget::WasmModule` for function exports; dangling-import,
   default/namespace-import, and non-function-export diagnostics (naming
   portal-co/jsaw#9 and portal-co/jsaw#10). Calls into Wasm members fail
   closed with a "not yet lowered" diagnostic until Phase 2.
4. `jsaw-wasi-bin` manifest `wasm: [{path, base64}]` array plumbing.

**Commit:** `wasm-esm: ingest MVP wasm modules into the closed module set`.
**Gate:** existing lib/e2e suites pass unchanged; new unit tests cover
specifier resolution, duplicate paths, profile ceiling, and import-table
construction for a `.wasm` member. **Done:** 146 lib tests (incl. 8
`wasm_esm` + 6 linker tests), 3 `tests/wasm_esm.rs` integration tests, and
3 `jsaw-wasi-bin` manifest tests pass; profile classification covers all
five profiles (multi-value, memory64, multi-memory, GC each gated and
classified).

### Phase 2 — JS↔Wasm adapters and native pass-through (Done)

1. Merge the ingested Waffle module into the output module (entity remap,
   segment rebase, deterministic order): ingestion (`from_wasm_bytes` +
   `expand_all_funcs` + a safe field-by-field strip to `Module<'static>`)
   happens at `insert_wasm` time; `merge_wasm_members` appends signatures,
   globals, tables (funcref elements rebased), memories (segments
   verbatim), and function bodies with all MVP-reachable operator indices
   remapped. Binary name-section names are kept as Waffle debug names, with
   `<path>:<index>` fallbacks. Members at GC profile fail closed at merge
   until Phase 5.
2. `wasm_module_adapter` generates the boxed-JS call adapters per §5.3
   (scalar types only; multi-result, reference, and v128 signatures fail
   closed with phase/SIMD-issue diagnostics). The callee is the merged
   internal function — no host import, no FFI manifest entry.
3. Re-export passthrough: `resolve_export` returns `ExportTarget::{Js, Wasm}`,
   so `export { f } from "./m.wasm"` and shim/star re-exports resolve; the
   artifact re-exports the merged function raw under the binary's own
   signature, bypassing numeric/`$gc` wrappers, with collision checks; an
   FFI boundary export naming a Wasm re-export fails closed.
4. Wasmtime + Node e2e: JS calls into Wasm through the numeric ABI,
   aliased imports reusing one adapter, no-arg/f32 rounding, memory +
   data-segment pass-through, raw i32/i64 re-export signatures, and shim
   re-exports — `crates/portal-jsc-waffle/tests/wasm_esm.rs` (10 tests).

**Commit:** `wasm-esm: call MVP wasm modules from JS and re-export them`.
**Gate:** differential fixtures (pure-JS implementation vs imported Wasm
implementation of the same function) produce identical results through
both the numeric and `$gc` export ABIs. **Done:** all wasm_esm integration
tests execute identically under Wasmtime and Node.

### Phase 3 — multi-value and memory64 (Done)

1. Multi-result adapters: `wasm_module_adapter` packs multiple Wasm results
   into a fresh JS array via `ValueDef::PickOutput` extraction (each
   PickOutput must be appended to the block — `add_value` alone leaves SSA
   illegal) and `ArrayNewFixed` over `repr.arguments`, preserving order.
2. memory64: merge already carried `memory64` flags and remapped `MemoryArg`
   indices; the one real bug was in waffle-backend, which hardcoded
   `i32.const` data-segment offsets — fixed in `../waffle-` commit
   `1b55a86` (`i64.const` for memory64 memories). Recorded as a Phase 0
   audit follow-up below.
3. Tests: multi-result round-trip through JS array indexing and `.length`;
   a memory64 module with a >4 GiB declared maximum (65537 pages) merges,
   keeps its `memory64` flag in the artifact, and its data segment survives
   the rebase — verified under Wasmtime and Node.

**Commit:** `wasm-esm: multi-value returns and memory64 memories`.
**Gate:** output artifacts validate with the corresponding wasmparser
features on; host manifest records the requirement. **Done:**
`tests/wasm_esm.rs` `multi_result_adapters_pack_into_a_js_array` and
`memory64_members_merge_and_execute`.

### Phase 4 — multi-memory (Done)

1. Ingestion and merge remap were already in place from Phase 2
   (`MemoryArg.memory` carried end-to-end; backend emits the full memory
   section and per-memory data). `verify_core_only` needed no change:
   multi-memory and memory64 are default-on in wasmparser 0.240.
2. CoreGC carry-through: `emit_coregc` appends source memories after the
   runtime memory (`source_memory_base`); `is_scalar_passthrough` now
   accepts loads/stores/`memory.size`/`memory.grow`, and the passthrough
   arm remaps memory indices by that base. The runtime memory layout is
   undisturbed.
3. Test: a two-memory member whose data segments land in distinct memories
   (`mem0[4]='h'`, `mem1[1]='o'`) executes identically through native
   WasmGC (Wasmtime + Node) and CoreGC (Wasmtime, core features). Encoding
   lesson recorded in the test: a memarg with a memory index encodes
   **memidx before offset** (bit 6 of the align flags).

**Commit:** `wasm-esm: multi-memory modules`.
**Gate:** CoreGC output validates with multi-memory on; memory contents do
not alias across indices in differential tests. **Done:**
`multi_memory_members_merge_and_execute_through_coregc`.

### Phase 5 — GC Wasm modules

1. `gc` profile: type-section interning into the output rec groups,
   abstract-ref adapters, concrete-ref `ref.cast` boundary rule.
2. FFI-manifest satisfaction of the GC module's own imports (§5.4) with
   signature checking.
3. Tests: a GC Wasm module exporting struct constructors/accessors called
   from JS; abstract `anyref` identity round-trips; concrete-type mismatch
   traps; two GC modules sharing an interned type.

**Commit:** `wasm-esm: ingest and link wasmgc modules`.
**Gate:** merged module validates with GC on; `emit_coregc` rejects the
combination loudly; mobile audit rejects GC-bearing sets.

### Phase 6 — CoreGC / nano_mvp / low_emit pass-through and manifests

1. CoreGC lowering of imported Wasm funcs (scalar-only) with the §5.5
  matrix enforced; `wasm_features` in `CoreGcArtifact` and the backend-plan
   manifest.
2. `nano_mvp` acceptance of MVP+multi-value imported modules behind its
   capability audit.
3. Mobile emitter audit: explicit rejection diagnostics per profile (no
   translation yet).

**Commit:** `wasm-esm: pass imported wasm through coregc and nano targets`.
**Gate:** the full cross-target suite runs a mixed JS+Wasm program to
identical results on WasmGC, CoreGC, and nano_mvp where the matrix allows.

### Phase 7 — documentation and consumer surface

1. README / embedding docs: `.wasm` module-set members, profiles, the
   adapter coercions, multi-value array results, GC cast rule, and the
   fail-closed import-resolution order.
2. `jsaw-wasi-bin` and Gradle plugin manifest docs for the `wasm` array.

**Commit:** `docs: wasm esm module consumer guide`.
**Gate:** docs examples are executable fixtures in the test suite.

## 7. Test matrix

| Area | Tests |
| --- | --- |
| Ingestion | Valid MVP binary; truncated/corrupt binary; feature above ceiling; `.wasm` key via `insert` (JS) rejected; duplicate path; JS+Wasm same path. |
| Linking | Named function import; missing export name; non-function export imported; default/star import rejected; `export { f } from "./m.wasm"`; star re-export through JS shim; hygiene of aliased bindings. |
| Adapters | i32/i64/f32/f64 round-trips incl. f32 rounding and i64 BigInt; multi-result array order; null `anyref`; concrete `ref.cast` trap on wrong type. |
| Pass-through | Data/elem segments preserved (observable via exported functions); two Wasm members with disjoint memories; Wasm→Wasm intra-set call without adapter; FFI-satisfied Wasm import with signature mismatch rejected. |
| Profiles | multi-value blocks; memory64 declared-max validation; multi-memory non-aliasing; GC interning shared types. |
| Backends | WasmGC encode validates per profile; CoreGC scalar-only acceptance and GC rejection; nano_mvp MVP acceptance; mobile audit rejects each profile loudly. |
| Regression | All existing e2e/coregc-cross suites unchanged; no `wasm:`/`~ffi` behavior change; artifacts without Wasm members are byte-identical to before. |

## 8. Open questions

- **Namespace objects** (`import * as m from "./m.wasm"`): worth a frozen
  namespace of function objects later, or keep named-only? Tracked as
  portal-co/jsaw#9.
- **Shared memory between a Wasm member and JS** (importing a memory
  export, or JS `Memory` objects): needs a linear-memory interop design
  that CoreGC's static layout can survive; deliberately deferred. Tracked
  as portal-co/jsaw#10.
- **WASI p1 modules** as Wasm members: resolvable today only through the
  FFI manifest per import; a blanket `wasi_snapshot_preview1` provider is
  host integration and out of scope (same stance as the existing `wasi.js`
  note in the host-import plan). Tracked as portal-co/jsaw#11.
- ~~**Source maps / names**~~: resolved — the Wasm name section **is**
  preserved into artifact debug names (§5.1); deterministic by
  construction.
