# Dogfooding: the jsaw compiler, compiled by itself, to Java — no Chicory

**Status:** plan. Goal: make jsaw **self-hosting** — run the whole
jsaw compiler (today a Rust `wasm32-wasip1` binary) inside a JVM build
*as plain Java*, so the Gradle plugin no longer needs Chicory (or any
embedded Wasm runtime). The compiler compiles itself.

## 1. The goal and the pipeline

Today the Gradle plugin (`docs/plan-gradle-plugin-wasm-compiler.md`,
milestones 12–14) compiles `jsaw-wasi-bin` to `wasm32-wasip1` and runs it
under **Chicory**, a pure-Java Wasm interpreter. That works but carries a
runtime dependency, an interpreter's speed, and a foreign execution model.

Dogfooding replaces Chicory with a **compiled** path through our own two
compilers:

```
jsaw-wasi-bin (Rust source)
   │  rustc --target wasm32-wasip1        (existing, Milestone 12)
   ▼
compiler.wasm  ── core wasm32-wasip1, linear memory, i32/i64/f32/f64, WASI imports
   │  wasm-blitz  (blitz-js backend)      (../wasm-blitz-spectests)
   ▼
compiler.js    ── a core-wasm-to-JS translation: stack machine, $mem
   │              Uint8Array linear memory, WASI import glue, BigInt i64
   │  jsaw  (portal-jsc-waffle, JS → WasmGC)   (this repo)
   ▼
compiler.module.wasm  ── the compiler, as a WasmGC module
   │  jsaw  (portal-jsc-jvm-emit, WasmGC → Java)  (this repo)
   ▼
Mod.java + S*.java + W.java   ── the compiler, as plain Java
   │  javac (already in the Gradle build)
   ▼
the compiler running on the JVM, no Wasm runtime
```

The Gradle plugin then instantiates this generated Java `Mod` instead of
interpreting `compiler.wasm` under Chicory. The bootstrap chain is:

1. **Stage 0** — the current native/Chicory-compiled compiler exists
   (Milestone 12) and is the trusted bootstrap.
2. **Stage 1** — Stage 0 (running however) drives the pipe above to
   produce the Java `Mod`.
3. **Stage 2 (dogfood proof)** — the Stage 1 Java `Mod`, running on the
   JVM, compiles the *same fixture inputs* and must produce **byte-identical
   outputs** to Stage 0. That is the self-hosting check.

## 2. What each stage actually is

### 2.1 Stage A: `compiler.wasm` (already done — Milestone 12)
`cargo build -p jsaw-wasi-bin --target wasm32-wasip1 --release`. Core
wasm, one linear memory, WASI imports for stdin/stdout/stderr and
preopened `/src` + `/out`. **No change needed.**

### 2.2 Stage B: wasm → JS via wasm-blitz
`blitz-js` (`../wasm-blitz-spectests/crates/blitz-js`) translates the
module's `MachOperator` stream to a JS program. From the e2e harness
(`crates/blitz-tests/tests/e2e.rs`) the emitted shape is:

- a **stack-machine** model: each WASM function `$N` becomes a JS function
  manipulating a JS-array `stack`, with `__sig = {params, rets}` runtime
  metadata and a per-call signature check;
- **linear memory** as a module-scope `let $mem = new Uint8Array(0)` +
  `$mem_dv = new DataView(...)` (ESM variant `js_module_preamble_esm`),
  grown by `memory.grow`, with data segments applied via
  `js_apply_data_segments` / passive `js_emit_passive_data_segment`;
- **imports** via `js_emit_imports_esm` → `import { name as _import_N }
  from 'module'; let $N = _import_N;` — this is the **WASI glue seam**;
- **exports** via `js_emit_exports_esm` → `export { $N as name };`;
- **i64 as BigInt** (`1n`, `BigInt.asUintN(bits, …)`), i32/f32/f64 as JS
  numbers.

So Stage B emits a **small ES module set**: the translated compiler plus a
hand-written **WASI glue module** (`wasi.js`) that implements the WASI
imports the binary actually uses (`fd_read`/`fd_write`/`path_open`/
`fd_close`/`proc_exit`/clock/random, as applicable) against an in-memory
or host-backed FS abstraction — exactly what Chicory's `WasiOptions`
provides today, but as JS.

**Owner: this repo calls blitz-js; blitz-js needs no changes** for core
wasm + WASI (its feature set — bulk memory, multi-memory, call_indirect /
return_call, i64 BigInt — covers a rustc wasip1 output). Any gap found is
filed against blitz-js, not patched here.

### 2.3 Stage C: JS → WasmGC via jsaw
`convert_modules` ingests the Stage B ES module set (entry = the
translated compiler, plus `wasi.js`) and lowers it to a WasmGC module.
**This is where the work is** — see §3.

### 2.4 Stage D: WasmGC → Java via jsaw
`emit_java` (Milestone 8) renders the Stage C module as `pc.portal.mob.*`
Java. **This is the payoff** and must also handle whatever Stage C
produces — see §3.

### 2.5 Stage E: the Gradle plugin switches hosts
`JsawRunner` (today: Chicory + `WasiOptions`) gains a backend that instead
loads the generated Java `Mod` and drives it through the same WASI
contract (feed manifest JSON on stdin, read `/out`). The task surface
(manifest, inputs/outputs, `@Cacheable`) is unchanged; only the host
changes.

## 3. The hard part: jsaw must compile *this* JS

The jsaw frontend is a **subset** of JavaScript, proven against
hand-written e2e fixtures. blitz-js's output is a *different, mechanical*
JS dialect. The two must meet. This is the substance of the plan and where
milestones land. The mismatches, in rough order of risk:

### 3.1 BigInt (the dominant risk)
blitz-js lowers **every i64 to a JS BigInt** (`1n`,
`BigInt.asUintN(64, x)`, `x & 0xffffffffffffffffn`, `…n`). A rustc
`wasm32-wasip1` build is **i64-heavy** (pointers are i32, but WASI return
values, file sizes, clocks, and all of swc/serde's `u64`/`usize` math ride
i64). jsaw today has **no BigInt value representation** — its `Repr` has
`number`/`boolean`/`string`/object structs and the f64 numeric export
boundary, nothing for BigInt.

**This must be resolved or the pipe dies at Stage C.** Options, in
preference order:

- **(a) Teach jsaw a BigInt representation.** A `bigint` boxed struct in
  `Repr`, BigInt literals parsed by swc, `asUintN`/`asIntN` and the `& | +
  - * / % < ==` BigInt operator family lowered to Waffle i64 ops (BigInt
  is, semantically, an i64 here — blitz-js only ever uses the fixed-width
  `asUintN/asIntN` forms). The numeric export boundary stays f64; BigInt
  never crosses it. **This is the honest path** and is what "this repo
  getting updates" means. Large but well-scoped: one new repr, one
  operator family, no GC design.
- **(b) Have blitz-js emit i64 as a `{lo,hi}` pair or two i32s.** Avoids
  BigInt in the JS but pushes a calling-convention change into blitz-js
  (violating "blitz-js needs no changes") and makes the JS far harder for
  jsaw to type — every i64 op becomes multi-value. **Rejected.**

**Decision: (a).** Add a BigInt repr + the fixed-width BigInt operator
family to jsaw.

### 3.2 The stack-machine JS shape
blitz-js does not emit idiomatic JS; it emits a value-stack interpreter in
JS (`stack=[...stack,tmp]`, `tmp=stack.pop()`, `__sig` checks, comma
operators, spread). jsaw's frontend must lower:
- array **spread in array literals** (`[...stack, tmp]`) and array
  `.pop()`/`.push()`/`.length` mutation — jsaw's array/TypedArray support
  is currently *typed-array* oriented (`typed_i8`…`typed_f64` reprs), and
  `$mem` is a real `Uint8Array`, but `stack` is a **plain JS Array of
  mixed values**, which jsaw may not model as a growable heterogeneous
  array;
- the **comma operator** in expression position (jsaw's CFG lowering must
  sequence it);
- `__sig` property assignment on function objects (`$N.__sig = {...}`).

Each is a concrete frontend gap to close. The plan assumes **array spread
+ dynamic-array push/pop + comma operator + function-object property
assignment** all need first-class jsaw support.

### 3.3 TypedArray / DataView linear memory
`$mem = new Uint8Array(0)`, `$mem_dv = new DataView($mem.buffer)`,
`$mem.set([...], off)`, and `memory.grow` → reallocating the Uint8Array.
jsaw has typed-array reprs (`typed_i8` etc.) and `length`/index member
reads, but **DataView get/set intrinsics, `Uint8Array` growth, `.set`,
and `.buffer`** are host intrinsics jsaw must gain (they are the linear
memory the whole compiler reads/writes through). This is a bounded set of
primordials to add.

### 3.4 WASI as a JS module (the glue)
`js_emit_imports_esm` emits `import { fd_read as _import_0 } from
'wasi_snapshot_preview1'`. The glue is a hand-written `wasi.js` in the
module set implementing each import as a JS function over the linear
memory — mirroring Chicory's `WasiOptions` (stdin pipe, stdout/stderr
sinks, preopened dirs). Because jsaw links a **closed module set**, the
import specifier `wasi_snapshot_preview1` must resolve to a module in the
set — so either blitz-js's import emission is pointed at a local `./wasi.js`
(it takes the module string verbatim, so `js_emit_imports_esm` can emit
`from './wasi.js'`), or a linker alias maps it. The glue functions read/write
the compiler's `$mem` via the DataView intrinsics of §3.3.

### 3.5 Control flow & the rest
blitz-js emits blocks/loops/branches and `return_call` — all within
jsaw's existing structured-control + tail-call support (Milestones 2–5).
`call_indirect` becomes a `$table_N[idx]` funcref-table index + `.__sig`
check — jsaw already models function values and indirect calls. Expected
to need little new work.

## 4. What "avoid Chicory" does and does not mean

- The **Gradle plugin's runtime** drops Chicory entirely — it loads
  generated Java `Mod` and drives it through WASI-shaped calls. Chicory
  leaves the plugin's dependencies.
- The **bootstrap** (Stage 0) still needs *some* way to run the original
  `compiler.wasm` once to produce Stage 1. That can be the existing
  Chicory path, or `wasmtime` on a maintainer machine — it is a build-time
  bootstrap, not a runtime dependency, and its output (the Java `Mod`) is
  checked in or cached so end-user builds never run a Wasm runtime.

## 5. Milestones

Continue cross-doc numbering from Milestone 14.

### Milestone 15 — characterize the Stage B output (feasibility gate)
Compile the real `compiler.wasm` with blitz-js and **inventory exactly
what JS it emits** for this module: which BigInt ops, which array/stack
forms, which DataView intrinsics, which WASI imports. Produce a concrete
gap list against jsaw's current frontend. *Exit: a written inventory +
a minimal blitz-js→JS→jsaw smoke (a tiny hand-written core-wasm add/mul
module round-tripping through Stages B→C→D to Java) proving the pipe's
shape end-to-end on a trivial input.*

### Milestone 16 — BigInt in jsaw (risk §3.1)
Add the `bigint` boxed repr, swc BigInt literal parsing, and the
fixed-width BigInt operator family (`asUintN`/`asIntN`, arithmetic,
bitwise, comparison) lowered to Waffle i64 ops. Tests: e2e fixtures
exercising BigInt arithmetic/comparison with known i64 results, on all
backends jsaw supports (WasmGC + Java + Swift where the repr maps).

### Milestone 17 — the mechanical-JS surface (§3.2, §3.3)
Array spread in literals, dynamic-array `push`/`pop`/`length`, the comma
operator, function-object property assignment, and the DataView /
`Uint8Array` `.set` / `.buffer` / growth intrinsics. Tests: e2e fixtures
mirroring blitz-js's exact emission idioms (a hand-written JS mimic of a
stack-machine function + a Uint8Array memory), asserting correct values.

### Milestone 18 — the WASI glue module (§3.4)
Hand-write `wasi.js` implementing the imports the compiler binary uses,
over the DataView linear-memory intrinsics, with an in-memory FS +
stdin/stdout contract matching `JsawRunner`'s manifest protocol. Tests: a
JS fixture that does `fd_write`/`fd_read` against the glue and round-trips
bytes, compiled through Stages C→D and run on the JVM.

### Milestone 19 — end-to-end dogfood (Stage 1 → Stage 2)
Drive the real `compiler.wasm` through Stages B→C→D to Java, compile it
with `javac`, and run it on the JVM against the standard fixtures. **Stage
2 proof: its outputs are byte-identical to Stage 0's.** Tests: a dogfood
e2e that runs the generated Java compiler on a fixture module set and
diffs its wasm/java/swift outputs against the Milestone 12 binary's
(golden). This is the milestone that *proves* self-hosting.

### Milestone 20 — the Gradle plugin switches hosts (§2.5, §4)
`JsawRunner` gains the generated-Java backend; Chicory is removed from
the plugin's dependencies; the functional tests run the compiler as Java.
The build-cache / up-to-date surface is unchanged. Tests: the existing
TestKit functional tests, now exercising the Chicory-free host, plus a
dependency assertion that Chicory is gone.

## 6. Alternatives considered

- **blitz-jvm direct (wasm → JVM bytecode).** `blitz-jvm` exists but is a
  stub (empty `#![no_std]` lib). Going through JS keeps **one** blitz
  backend (blitz-js, which is real and spec-tested) and exercises jsaw's
  full JS→WasmGC→Java chain — the actual dogfood. A future, separate
  effort could mature blitz-jvm and bypass JS, but that is not this plan
  and would dogfood blitz, not jsaw.
- **Keep Chicory.** Works today (Milestone 14) but is an interpreter +
  foreign runtime dependency; the point of dogfooding is to eat our own
  cooking and remove it.
- **Compile the compiler to JS once and check it in, skipping Stage B in
  the build.** Tempting for build speed, but the dogfood value is the
  *whole pipe* being exercised; Stage B stays in the loop (its output can
  still be cached).

## 7. Risks

- **BigInt scope (§3.1).** The single largest jsaw change. Mitigated by
  blitz-js using only fixed-width BigInt (no arbitrary-precision corner
  cases) and by the numeric boundary staying f64 so BigInt never escapes.
- **blitz-js emission idioms broader than expected (§3.2).** Milestone 15
  exists precisely to bound this before committing to 16–18; the trivial
  round-trip smoke de-risks the pipe shape early.
- **Performance of the stack-machine JS after jsaw lowers it.** jsaw's
  optimizations (tail calls, multi-return) target idiomatic JS; a
  value-stack interpreter may lower poorly. Correctness first (Milestone
  19); performance is a follow-up and not a dogfood blocker.
- **WASI surface creep (§3.4).** The compiler binary may use more WASI
  than fd_read/write (preopens, clocks, random). Milestone 15 inventories
  the exact import list so 18 is scoped.
