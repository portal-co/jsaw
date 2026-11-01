# @portal-solutions/jsaw

## Description
Compiler workspace (`jsaw`) related to `jsaw-core`, containing the WasmGC backend (`portal-jsc-waffle`) and the mobile emitters that lower the same IR to Java and Swift (see `docs/plan-wasmgc-mobile-backends.md`).

## Goals
- [ ] Implement specific backends for `jsaw`
- [ ] WasmGC → Java/Swift mobile emitters (docs/plan-wasmgc-mobile-backends.md)
- [ ] Integrate with `jsaw-core` IR

## Layout
- `crates/portal-jsc-waffle` — the WasmGC backend (`convert_modules` lowers a linked ES module set to a Waffle IR module; `to_wasm_bytes` encodes WasmGC).
- `crates/mobile/portal-jsc-{mob,jvm,swift}-emit` — emit the same IR as Java / Swift for mobile.
- `crates/jsaw-wasi-bin` — the whole compiler as a `wasm32-wasip1` binary (JSON manifest on stdin, one JSON result line on stdout), so any host that can run Wasm/WASI can compile JS → WasmGC / Java / Swift.
- `gradle/jsaw-gradle-plugin` — the `dev.portal.jsaw` Gradle plugin, which runs that compiler wasm inside the build via Chicory (see `docs/gradle-plugin.md`).

## Wasm module-set members (wasm-esm)

Besides JavaScript sources, the closed module set accepts **Wasm binary
modules** under `.wasm` keys (portal-co/jsaw#14;
`docs/plan-wasm-esm-modules.md`). JavaScript imports their function
exports with ordinary named ESM imports, and the binary's own type section
is the ABI declaration — no FFI manifest entry is needed:

```js
import { add } from "./math.wasm";
export function run(a, b) { return add(a, b); }
```

The binary is ingested into the same Waffle IR as the lowered JS and
merged into jsaw's single output module, so it passes through to every
backend: WasmGC encode, CoreGC core-Wasm, nano_mvp, and the mobile audit.

Embedder API:

- `ModuleSet::insert_wasm(path, bytes)` validates at the **MVP** profile;
  `insert_wasm_with_profile(path, bytes, WasmProfile::…)` raises the
  ceiling through the cumulative profiles `MultiValue`, `Memory64`,
  `MultiMemory`, `Gc`. A module is classified at its exact requirement and
  rejected above the ceiling with a diagnostic naming the feature.
- `jsaw-wasi-bin` manifests carry binaries as
  `wasm: [{ "path": "lib/math.wasm", "base64": "…" }]`.

Semantics at the JS boundary (plan §5.3):

| Wasm type | JS → Wasm | Wasm → JS |
| --- | --- | --- |
| `i32` | `ToInt32` | `Number` |
| `i64` | `ToBigInt64` | `BigInt` |
| `f32` | number, demoted | number, promoted (f32 rounding is observable) |
| `f64` | number | number |
| multi-result | — | fresh JS `Array`, in order |
| concrete `(ref (null) $t)` | `ref.cast` to the interned type (mismatch **traps**, no structural coercion) | JS value |
| abstract internal refs (`anyref`, `eqref`, …) | cast to the target | JS value |
| `funcref` / `externref` | rejected | rejected |
| `v128` (SIMD) | rejected (portal-co/jsaw#7) | rejected |

Member types are interned structurally across Wasm members, so a value one
member produced passes another member's `ref.cast` when the shapes match;
recursive groups and subtyped (`sub`/`extends`) members are excluded (the
latter rejected, since Waffle drops subtyping). jsaw `Repr` types never
intern with member types.

A Wasm member's own **function imports** are satisfied from the FFI
manifest (`ConvertOptions::ffi`) with a signature check; undeclared or
mismatched imports, and all memory/global/table/tag imports, fail closed
(portal-co/jsaw#10). Re-exporting works:
`export { add } from "./math.wasm"` emits the merged function raw under
the binary's own signature, bypassing the numeric/`$gc` wrappers.

Feature requirements of produced artifacts are recorded in
`CoreGcArtifact.wasm_features` and the backend-plan manifest
(`uses_multi_memory` / `uses_memory64`).

## Source exception boundary

The WasmGC backend supports synchronous JavaScript `throw`/`try`-`catch`
across jsaw source calls. Uncaught throws trap at exported boundaries; the
internal exception-result value is never exposed. CoreGC uses the same
internal propagation model. Foreign Wasm-import exceptions, Wasm traps,
`finally`, and asynchronous exceptions are not caught by this protocol.
Mobile emitters reject exception-bearing modules.

## Progress
- [ ] Workspace setup with backend crates

---
*AI assisted*
