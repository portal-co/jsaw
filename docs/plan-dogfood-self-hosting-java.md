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

### Milestone 18 — as-built foundation

`wasi.js` now provides the hand-written `wasi_snapshot_preview1` module with
all 16 imports inventoried in Milestone 15 plus host hooks for binding the
owner's `Uint8Array` memory, feeding stdin/argv, reading stdout/stderr, and
observing `proc_exit`. It uses `DataView` for preview1 ABI structures and
returns the blitz-js-compatible one-element BigInt errno array (or `[]` for
`proc_exit`). The module intentionally keeps mutable state in top-level
arrays and primitive bindings, matching jsaw's cross-function state model.

The source is compiled as a closed JS module set by
`m18_wasi_glue_module_lowers`. This verifies its full surface can be parsed,
linked, and lowered through jsaw after the shared module-top-level context
work. The JVM emitter now has a conservative method-splitting path for large,
straight-line function prefixes: a generated mutable frame carries every
Wasm local across small private helper methods. A terminal structured suffix
(including conditionals whose arms both return) is also emitted in a helper:
source returns store the result in the frame and signal the public wrapper to
return it. `break`-terminated bodies and tail-call protocol bodies still need
a continuation-aware splitter before the entire glue module compiles on the
JVM. The full 64 KiB blocker therefore remains open, but the splitting seam
now covers terminal conditional CFG exits and has JVM execution regressions.

The observed WasmGC cast failure was rooted in computed keys that crossed a
function/property boundary: a numeric key became a boxed `Number` `anyref`,
yet member lowering treated every reference key as a string and attempted a
string cast. Reference-valued keys now refine a boxed Number back to a numeric
array index; raw growable arrays also take their own checked read path before
the string/object fallback. The imported `fd_read` memory round-trip now
executes under Wasmtime.

Current filesystem calls expose the correct preview1 import ABI and errno
behavior but return `ENOENT` until the M19 manifest-backed `/src` and `/out`
file table is attached. This keeps the artifact honest: it is suitable for
link/lowering integration today but not yet the compiler-execution proof.

### Milestone 19 — end-to-end dogfood (Stage 1 → Stage 2)
Drive the real `compiler.wasm` through Stages B→C→D to Java, compile it
with `javac`, and run it on the JVM against the standard fixtures. **Stage
2 proof: its outputs are byte-identical to Stage 0's.** Tests: a dogfood
e2e that runs the generated Java compiler on a fixture module set and
diffs its wasm/java/swift outputs against the Milestone 12 binary's
(golden). This is the milestone that *proves* self-hosting.

#### M19 Stage-C ingestion blockers (as diagnosed, September 2026)

The bounded lazy-ingestion gate
(`jsaw-wasi-bin/tests/dogfood_m19.rs`, ignored; requires the real
`target/dogfood-m15/compiler.js`) exposed three frontend-scale defects, in
order:

1. **TAC rewriter gap (fixed).** The TAC→AST rewriter lacked an
   `Item::Tpl` inverse even though TAC conversion produces it; the 164 MiB
   artifact panicked immediately at `swc-tac/src/rew.rs`. Fixed in
   jsaw-core `cac93f8` (render template literals when rewriting TAC).
2. **Unconditional HCR inventory fingerprinting (fixed).**
   `SModuleBuilder::append` computed a canonical SSA→TAC→AST fingerprint
   for every completed function even when no caller ever consumed the
   inventory; at 8,096 generated functions that pass dominated ingestion.
   Fixed in jsaw-core `ce15489` by making inventory opt-in (`for_module`
   computes fingerprints; `new` does not). This moved 15-minute ingestion
   progress from byte 78 KB to byte 4.47 MB.
3. **SSA conversion is super-linear in `|decls| × |blocks|` (open).**
   The remaining wall is `swc-ssa`'s `TFunc → SFunc` conversion: every
   block allocates one block-param per declared identifier and every
   jump threads the full `all` set (`convert_block`'s `state`/`params`
   over `self.all`), while `load()` additionally runs `TCfg::def`'s
   whole-body scan per non-inlinable-miss load. The first multi-hundred-KB
   generated stack-machine function (`$272`, ~1.6 MB source, ~520 labeled
   loop blocks) does not finish conversion within 15 minutes, and a
   whole-module release run was killed at 50.2 GB peak RSS before
   ingestion completed. Two separable remedies, in increasing invasiveness:
   (a) bound `all` per block to variables actually live across edges
   (dominance/liveness-driven instead of the current conservative
   full-set threading), and (b) stream Stage C per function: finish the
   `IncrementalConverter` seam so completed `SFunc`s lower to WasmGC as
   they are produced and the module-wide SSA map is never materialized.
   Until at least (a) lands, M19 cannot run on realistic hardware; the
   CoreGC parity suite (`portal-jsc-waffle/tests/coregc_cross.rs`) remains
   the operative correctness gate in the meantime.

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

## 8. As-built

### Milestone 15 — characterization done, blitz-js opt mode fixed (prerequisite)

`crates/jsaw-wasi-bin/tests/dogfood_m15.rs` characterizes the real
`jsaw-wasi-bin.wasm` through blitz-js and executes the result. Findings:

- **16 WASI imports** scope the Milestone 18 glue (args/environ get+sizes,
  fd_close/fd_fdstat_get/fd_filestat_get/fd_prestat_dir_name/fd_prestat_get/
  fd_read/fd_write, path_create_directory/path_filestat_get/path_open,
  proc_exit, random_get). Exports: `_start`, `__main_void`.
- **~6042 `call_indirect` sites, zero `return_call`, zero `memory.grow`** —
  so Milestone 17 needs no tail-call or growth handling from this corpus.
- blitz-js models **every** wasm value as BigInt (i32 included — `mask32`
  masking everywhere), so Milestone 16 BigInt support is load-bearing for all
  arithmetic, not just i64.

**Deviation discovered here:** blitz-js's non-optimized output was 452 MB
(dominated by the stack weave), so opt mode was required — but opt mode
*crashed with a stack underflow* on the real compiler wasm. The plan said
"blitz-js needs no changes," but opt mode was genuinely broken (not a
scope choice), so it was fixed in the blitz repo (`wasm-blitz-spectests`
commit `9c52005`): per-frame base tracking in `OptState`, and codegen fixes
for `Drop` (physical pop never emitted), `Select` (popped 5 instead of 3),
and conditional branches (`br_if`/`br_table` committed depth mutations the
fallthrough path didn't take). With the fix, opt mode compiles the compiler
wasm cleanly and emits **167 MB** (2.7× smaller), and five opt-mode modules
in `crates/blitz-js/tests/opt_repro.rs` execute correctly in Node.

`trivial_core_wasm_round_trips_to_js` now executes **both** non-opt and opt
compiled output in Node (add(40,2)=42), proving the pipe shape end-to-end on
a trivial input.

### Milestone 16 — BigInt in jsaw (as-built)

BigInt landed as a **fixed-width, `i64`-backed** value representation —
exactly the form blitz-js uses (it only ever applies `asUintN`/`asIntN`,
never arbitrary-precision), so a raw `i64` payload is exact and never
allocates in straight-line code.

- **`repr.rs`**: a single-field `bigint` struct (`{ i64 }`) minted in
  `Repr::new`, plus `bigint_ty`/`bigint_non_null_ty` accessors. A BigInt
  boxes into this struct only when it escapes to a reference boundary
  (the adapter, a context store, a property slot).
- **`ValueKind::BigInt`** (raw `i64`) added; every kind-driven match updated.
  `as_i64` (identity for BigInt, `ref.cast`+`struct.get` unbox for a boxed
  BigInt reached through a reference boundary, compile error for a number),
  `box_value` (StructNew bigint), `as_condition` (`!= 0n`), `wasm_type`
  (`I64`).
- **Return ABI**: a pure-BigInt core returns a raw `i64`
  (`native_return_type`/`ReturnType::I64`); any kind mix containing BigInt
  keeps the boxed `anyref` ABI. **The M5 union layouts are untouched** —
  BigInt never rides a union (it joins no `r`/`i`/`f` group), so the
  tail-call/union machinery needed no change. The adapter gained an `I64`
  return arm that StructNews the bigint box.
- **Literals**: `parse_bigint_i64` reads the swc `raw` source text (decimal,
  `0x`/`0o`/`0b`, with `_` separators) into raw `i64` bits — no `num-bigint`
  dependency. Out-of-range literals are rejected. (swc parses `-1n` as
  `UnaryOp::Minus(1n)`, so the literal is always unsigned.)
- **Operators**: `binary`/`unary` route BigInt operands to the raw `i64`
  family (`+ - * / % & | ^ << >>`, comparisons `=== !== < <= > >=`, unary
  `- ~ !`). JS BigInt `/`/`%` truncate toward zero, matching Wasm
  `i64.div_s`/`rem_s` exactly; `>>` is arithmetic; `>>>` on a BigInt errors
  (JS throws). Mixing a BigInt with a plain number is a compile error (JS
  throws TypeError; the corpus never does it).
- **`BigInt.asUintN(width, x)` / `asIntN(width, x)`** are provable primordial
  intrinsics (new `BigInt` namespace + tags). The width is **not** required
  to be a literal: blitz-js routes it through trivial forwarder arrows
  (`const toUint = (a,b) => BigInt.asUintN(b,a)`), so the masking computes a
  runtime shift count `64 - width` from the boxed Number arg. `asUintN` uses
  a logical right shift, `asIntN` arithmetic.
- **Tail-position primordial calls** (`return BigInt.asUintN(…)`, the exact
  form of the forwarder arrows) now route through the provable primordial
  lowering in `try_tail_dispatch`, converting the result to the caller's ABI
  — without this, the `BigInt` member callee (which has no context object)
  fell through to the generic adapter path and trapped on a null receiver.

Tests: 12 e2e fixtures (`bigint_*`) covering literals, arithmetic, bitwise,
shifts, division/remainder truncation, comparisons, unary ops, both
intrinsics, truthiness, i64-range arithmetic beyond f64 precision, and the
exact `mask32`/`toUint` blitz-js idiom — all green on **all four runtimes**
(Wasmtime, Node.js, JVM, Swift), full suite 93 passed / 0 failed.

**Known boundary (out of scope for 16):** module top-level binding evaluation
(`let mask32 = …` at module scope) is the pre-existing jsaw-core shim gap —
top-level non-function stores don't run before a function body reads them.
The corpus's per-function aliases are local, so this doesn't block BigInt;
it belongs to Milestone 17/18 (module top-level evaluation).

### Milestone 17 — the mechanical-JS surface (as-built)

Implements every syntactic and intrinsic form the blitz-js opt-mode
`compiler.js` emits, validated against fixtures mirroring the exact emission
idioms (a stack-machine function plus a `Uint8Array` linear memory). Two
jsaw-core bugs blocked this milestone and were fixed first (jsaw-core
`81ccb25`): labeled `break`/`continue` (the `labelled` map keyed on
`Ident` including its span, so declaration and use never hashed equal) and
array-rest binding (an off-by-one that silently dropped the first rest
element, plus a bind that was never emitted).

Surface implemented (all new tests are `m17_*`, run on all four runtimes):

- **Labeled control flow** (`lN: for(;;)` / `break lN` / `continue lN`) —
  the corpus's 85,844 labeled loops.
- **Rest parameters and array destructuring rest** (`function(...locals)`,
  `let [...rest] = arr`), including the spread-call path (`$N(...args)`)
  through both the generic adapter and direct native dispatch.
- **Array spread literals** (`[...rest, x]`), folded member-by-member.
- **Dynamic arrays** as the operand stack: `stack.length++`/`--`,
  `stack[i]` reads/writes, `push`-free slot management.
- **The comma operator** (used ~204k times by blitz-js for sequencing).
- **Function-object properties** (`f.__sig = {...}` read back), plus
  `Object.defineProperty`/`Object.freeze` as no-op-ish metadata carriers.
- **The `typeof` operator** over all kinds (compile-time constant for
  statically-known kinds; a runtime ref-test chain for boxed values).
- **`Number(x)` / `BigInt(x)`** global conversion intrinsics.
- **Template literals** (constant-concatenated; the corpus's only template
  is the constant `` `wasm sig mismatch` `` sig-guard message).
- **`throw` / `new Error(msg)`** — `throw` lowers to value evaluation plus a
  wasm `unreachable` (the corpus never catches); `new Error` is a provable
  constructor producing a plain object.
- **Truthy/`||` correctness** for all falsy values (null/0/NaN/''/0n), used
  by the corpus's `cur || default` guards.
- **`Uint8Array` `.set`/`.length`/`.byteLength`** and **`.buffer`** (returns
  an `ArrayBuffer` object sharing the backing bytes).
- **`DataView`** (constructor over `.buffer`, and the full accessor surface
  `getUint8/Int8/Uint16/Int16/Uint32/Int32/Float64/BigUint64` plus the
  `set` family) over a **shared** i8 backing array so DataView writes alias
  Uint8Array reads — the exact `__wasm_dv`/`__wasm_mb` linear-memory
  intrinsic. Accessors assemble/split bytes little-endian from the shared
  array; `getBigUint64`/`setBigUint64` round-trip the raw i64 as a BigInt.

Tests: 38 e2e fixtures (`m17_*`), each asserting a known result on all four
runtimes (Wasmtime, Node.js, JVM, Swift).

**Deviations / boundaries:**
- The DataView adapter methods are minted per access *site* (~14 KB of wasm
  per distinct-argument site in the current design, since `build_native_adapter`
  specializes on nothing but is invoked per site). Module size grows linearly
  with site count — fine for fixtures, a scaling concern for the 250k-access
  corpus (addressed in Milestone 18/19 by sharing or caching method bodies).
- `setUint32` with the full u32 range (`4294967295`) hits the f64→i32
  saturating-truncate boundary; offsets beyond `2^32` are out of range. The
  corpus's offsets and values stay within f64/i32-exact range.
- Module top-level binding evaluation remains the pre-existing jsaw-core
  shim gap (a top-level non-function store doesn't run before a function
  body reads it); fixtures bind such values inside the exported function.
  Addressed in Milestone 18.
