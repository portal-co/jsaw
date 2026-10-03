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
