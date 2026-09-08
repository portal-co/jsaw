# Plan: WasmGC → Java/Swift mobile backends (replacing the legacy mobile crates)

Follow-up to `plan-merge-tailcalls-and-multi-return.md` (Milestones 1–3) and
`plan-tail-union-and-dedicated-multi-ret.md` (Milestones 4–5), all landed.
Milestone numbers here continue the sequence (6–11).

Two goals, one design:

- **Delete the legacy mobile crates.** `crates/mobile/jacon`,
  `crates/mobile/portal-jsc-mob-backend-common`, and
  `crates/mobile/portal-jsc-swift-backend` are 213 lines of
  display-combinator stubs (plus a 25-line Java `Blocks` trampoline runtime
  and generated Gradle wiring). Nothing references them — not
  `portal-jsc-waffle`, not `swc-batch`, not the tests. Git history preserves
  them; the tree should not carry them.
- **Replace them with a real WasmGC → Java/Swift compiler** that shares the
  existing frontend and every optimization by construction: the emitters
  consume the same `portal-pc-waffle` IR module that `waffle-backend`
  encodes to WasmGC bytes, so return-kind analysis, tail dispatch, dedicated
  multi-return layouts, direct/provable call dispatch, primordial fast
  cores, shape analysis, and ESM linking all apply unchanged to the mobile
  targets.

---

## 6.0 Current shape

### The pipeline today

```
swc AST → swc-tac → swc-ssa (SModule)
        → portal-jsc-waffle::convert_modules   (all analyses/optimizations)
        → portal_pc_waffle::Module             (WasmGC 1:1 IR)
        → waffle-backend::to_encoded_module    (validate, Reducifier,
                                                CFGInfo, Trees/Stackify,
                                                Localify, opcode encode)
        → .wasm bytes
```

Every optimization this project has built lives **before** the final encode
step and is expressed *in the IR itself*: multi-return cores are struct
types; tail dispatch is `ReturnCall` operators; direct dispatch is `Call`;
primordials and shapes are ordinary in-module functions and struct types.

### The emitted-module feature closure (the emitter contract)

Audited from `conv.rs` operator/terminator usage — this is the entire
surface the mobile emitters must cover, and an assertion pass (Milestone 7)
will pin it:

- **Types**: GC structs (repr layouts, shapes, multi-return unions), GC
  arrays (`i8` utf8, `i16` utf16, `anyref` argument arrays, typed-array
  element arrays), `i31ref` (only the JS-null sentinel — `repr.rs`
  guarantees no other i31 ever materializes), `funcref` (adapter sig only),
  no `externref` inside the module, **no linear memory, no globals, no
  tables-with-`table.get`, no exceptions** (`TTerm::Throw` is rejected
  upstream).
- **Operators**: `I32*`/`I64*`/`F64*` arithmetic/comparison/conversion,
  `TypedSelect`, `Call`, `CallRef` (always the adapter sig), `StructNew`/
  `StructNewDefault`/`StructGet`/`StructSet`, `ArrayNewFixed`/
  `ArrayNewDefault`/`ArrayGet(U)`/`ArraySet`/`ArrayLen`/`ArrayCopy`,
  `RefNull`/`RefIsNull`/`RefTest`/`RefCast`/`RefFunc`/`RefEq`/`RefI31`.
- **Terminators**: `Br`, `CondBr`, `Return`, `ReturnCall`,
  `ReturnCallRef`, `Unreachable`.
- **Host imports**: none. Modules are self-contained — primordials and the
  lexical context are generated in-module (`primordials.rs`), ESM imports
  are resolved by the in-module linker. The only host surface is the export
  shims (numeric `f64 × arity → f64` today, GC exports behind the suffix
  option).
- **The `funcref` table** exists only to declare `RefFunc` elements; no
  `CallIndirect` is ever emitted.

Anything outside this closure is a bug in the audit pass, not a silent
fallback.

### Why emit from IR, not from `.wasm` bytes

The `portal-pc-waffle` module *is* the WasmGC module (1:1). Emitting from
it means the mobile backends share the frontend and optimizations
literally, with no byte round-trip and no second validation story. Lifting
arbitrary prebuilt `.wasm` via `waffle-frontend` into the same emitters is
a possible future use but a **non-goal** here.

### The key emission insight

`waffle-backend`'s encode pipeline ends with **Localify**: values are
assigned to typed locals and the CFG is a stackified tree of
`Block`/`Loop`/`If`/`Br` with local gets/sets — i.e. exactly the shape a
source-language emitter wants. The mobile emitters consume the **same
post-Localify tree** the opcode encoder consumes; only the final "encode
opcodes" step is replaced with "print Java" / "print Swift". Block params
are already locals; irreducible control flow is already treeified; the
wasm operand stack is already gone.

Milestone 7 exposes that tree from the local `waffle-` checkout
(`../waffle-`) if it isn't already public.

---

## 6.1 Shared design: value model

One design, two syntaxes. The audit-driven mapping:

| WasmGC | Java | Swift |
|---|---|---|
| `anyref` (nullable) | `Object` (`null`) | `Any?` (`nil`) |
| struct sig | `final class` per sig, public fields | `final class` per sig |
| array sig | plain Java arrays (`byte[]`, `char[]`, `Object[]`, `double[]`, …) — every array op is sig-typed so the concrete type is known at each site | `ContiguousArray`-backed wrapper classes per sig (Swift arrays are generic structs; wrappers keep identity semantics) |
| `i31ref` | `JsNull.INSTANCE` singleton (sentinel-only; audit asserts no other i31 use) | `JsNull.shared` singleton |
| `funcref` | one `@FunctionalInterface` per func sig actually referenced via `RefFunc` (the adapter sig dominates); `RefFunc f` → method reference | closures / `final class` with `invoke` |
| `RefEq` | `==` | `===` |
| `RefTest`/`RefCast` | `instanceof` / cast (audit asserts every test target maps to a generated class or the sentinel) | `is` / `as!` with a trapping helper |
| trap sites (`Unreachable`, div-by-zero, `F64ConvertI32S` range) | `throw new WasmTrap(...)` (unchecked) | `fatalError`/thrown `WasmTrap` |

**Per-operator numeric semantics** get an explicit mapping table in the
shared core (Milestone 7): the traps above, wasm shift-count masking
(matches Java/Swift), `f64.min/max` NaN and ±0 behavior (`Math.min/max`
match; Swift needs checked wrappers), `nearest` (`Math.rint`;
`FloatingPointRoundingRule.toNearestOrEven`), reinterpret as raw bit
moves (`Double.doubleToRawLongBits` / `bitPattern`).

**Strings**: the repr `string` struct is `{ utf8: array i8, utf16: array
i16 }`; the emitters generate the matching class with lazy
`java.lang.String` / `Swift.String` conversion for host interop. The
numeric export boundary is `f64`-only, so no string marshalling is needed
for the default host surface.

**Runtime library per target is thin by design**: struct/array classes
are generated *from the module's own signatures*; primordials and the
context are in-module code and come along for free. Hand-written per
target: the `JsNull` sentinel, `WasmTrap`, numeric helper functions, the
tail-call trampoline (`§6.2`), export entry shims, and the string bridge.

## 6.2 Shared design: control flow and tail calls

- **Structured control flow**: the post-Localify tree maps mechanically —
  wasm `block` → Java labeled block / Swift labeled `do`, `loop` → labeled
  loop, `br` to a block → `break label`, `br` to a loop → `continue
  label`, `if` → `if/else`. (Swift labels attach to `do`/loops/`if`, which
  covers the tree shapes stackify produces.) Any function the tree
  consumer cannot handle falls back to a per-function state machine
  (`while (true) switch (state)`); the fallback must exist but is
  expected to fire never.
- **Self-`ReturnCall`** (the dominant case; our depth-100000 tests):
  assign arguments to parameter locals and `continue` an outer trampoline
  loop — O(1) stack, zero allocation.
- **Cross-function `ReturnCall` and `ReturnCallRef`**: a thunk protocol
  over the *tail-callable set* = static `ReturnCall` targets ∪ all
  adapters (the dynamic `ReturnCallRef` targets). Members get a wrapper:
  ```
  Object f(args) { Step s = f_body(args);
                   while (s is Tail t) s = t.invoke();
                   return s.value; }
  ```
  A `ReturnCall` inside the body returns `Tail(target, args)` instead of
  calling. Callers of the set see an ordinary method; non-members call
  members normally (the wrapper absorbs the loop). This preserves the
  Milestone 2/4/5 O(1)-stack guarantees on JVM/ART and Swift, which have
  no native tail calls. Swift mirrors with an `indirect enum Step`.
- Non-tail recursion depth is engine-bounded exactly as it is in Wasmtime
  today; no special handling.

## 6.3 Crate layout

```
crates/mobile/portal-jsc-mob-emit/      shared: audit pass, tree consumer,
                                        value-model/name mangling, numeric
                                        mapping tables, trampoline planner
crates/mobile/portal-jsc-jvm-emit/      IR → Java source (+ runtime .java)
crates/mobile/portal-jsc-swift-emit/    IR → Swift source (+ runtime .swift)
```

Emitters depend on `portal-pc-waffle` only — never on swc/jsaw internals —
keeping the layering `jsaw → IR → {wasm, java, swift}` clean. Public API:
`emit_java(&Module) -> JavaSources`, `emit_swift(&Module) -> SwiftSources`.

Testing harness invokes `javac`/`java` and `swiftc` directly (no Gradle in
tests); the legacy Gradle skeleton is deleted in Milestone 6.

---

## Milestone 6 — delete the legacy mobile crates

Remove `crates/mobile/{jacon,portal-jsc-mob-backend-common,
portal-jsc-swift-backend}`, the workspace members, the Gradle skeleton
(`settings.gradle.kts`, `build.gradle.kts`, `gradlew*`, `gradle/`) that
existed only for `jacon`, and the README backend list; update `goals.md`.
Git history preserves the code. Gate: workspace builds, all 81 e2e tests
stay green.

## Milestone 7 — audit pass + shared emission core

- `portal-jsc-mob-emit` crate: the feature-closure **audit pass** walks a
  module and errors on anything outside §6.0 (run it over every e2e
  fixture — it becomes the living contract).
- Expose/consume the post-Localify tree from `portal-pc-waffle`
  (patch the local `../waffle-` checkout if the API isn't public).
- Shared: name mangling (Java/Swift keywords, sig → class names), the
  per-operator numeric mapping tables, the tail-callable-set computation,
  i31-sentinel assertion.
- Skeleton emitters produce: per-sig classes, per-func method stubs with
  correct signatures, runtime files. Gate: audit passes on all fixtures;
  skeletons compile (`javac`/`swiftc`) for the smallest fixture.

## Milestone 8 — Java emitter core (no tail calls yet)

Full §6.0 operator/terminator coverage; structured control flow with the
state-machine fallback; numeric export entry points; `ReturnCall`/
`ReturnCallRef` temporarily lowered as plain calls (documented stack
caveat). New e2e runner compiling emitted Java with `javac` and executing
fixtures on the JVM alongside Wasmtime/Node. Gate: every non-tail
execution fixture passes on the JVM; suite stays green.

## Milestone 9 — Java tail-call trampolining

Implement §6.2: self-tail loops plus the thunk protocol over the
tail-callable set. Gate: the depth-100000 raw/union tail-recursion
fixtures pass on the JVM; full corpus green on all three runtimes.

## Milestone 10 — Swift emitter + runtime

Mirror Milestones 8–9 for Swift (same shared core, same fixtures, `swiftc`
runner). Gate: full corpus on Wasmtime, Node, JVM, and Swift; depth
fixtures pass on Swift.

## Milestone 11 — packaging and docs

Public `emit_java`/`emit_swift` API polish, generated-code organization
(split classes/files for large modules; JVM 64 KiB method limit and Swift
type-checker scaling are the known hazards), `README.md`/`TESTING.md`
updates, and a consumer guide (how to embed the emitted runtime in an
Android/Xcode project).

---

## Non-goals

- Lifting arbitrary prebuilt `.wasm` (via `waffle-frontend`) — the
  emitters target the jsaw-emitted closure; the audit rejects the rest.
- Kotlin-idiomatic or SwiftUI-idiomatic output — generated code is
  compiler-grade, not hand-style.
- Exception handling, linear memory, threads, SIMD — unused by the
  frontend (throws are rejected upstream).
- Gradle/SwiftPM *build-system* codegen for consumers (Milestone 11
  documents embedding only).
- Host string/JSON interop beyond the numeric export boundary (GC exports
  already expose internals to embedders who opt in).

## Risks

- **Tree API exposure**: `Trees`/`Stackify`/`Localify` may be private to
  `waffle-backend`; the local `../waffle-` checkout makes this patchable,
  but upstream-shape changes ripple. Milestone 7 resolves this first.
- **JVM stack depth**: non-tail recursion depth differs from Wasmtime;
  only tail depth is pinned by tests (as today).
- **NaN bit patterns**: payloads through `f64` boxing are preserved via
  raw-bit helpers; sign/NaN-canon differences in `min/max` are handled in
  the mapping table, but host-visible NaN prints may differ cosmetically.
- **Swift toolchain availability**: `swiftc` on this macOS host via Xcode
  CLT; if missing, Milestone 10 reports the environment prerequisite
  rather than substituting another host (per global QEMU/tooling rules —
  Linux Swift coverage, if ever needed, goes through QEMU software
  emulation, not host substitution).

---

## Milestone 7 — as built (implementation notes)

- Crates: `crates/mobile/portal-jsc-mob-emit` (shared core),
  `portal-jsc-jvm-emit`, `portal-jsc-swift-emit`.
- **No waffle patch was needed**: `waffle-backend` exposes `Reducifier`,
  `Trees::compute`, `StackifyContext::compute`, and `Localifier::compute`
  publicly, so `lower::lower_body` consumes exactly the post-Localify
  artifacts the opcode encoder consumes (`portal_pc_waffle::backend::
  backend::{reducify,treeify,stackify,localify}`).
- `lower::lower_body` mirrors `WasmFuncBackend` step for step (validate →
  Reducifier → CFGInfo → Trees → Stackify → Localify → walk), producing
  the target-neutral SIR (`sir.rs`): labeled `Block`/`Loop`/`If`,
  `Break`/`Continue`, `Assign`/`Effect`, `Return`, `TailCall`,
  `TailCallRef`, `Unreachable`, and expression trees with owned/remat
  values inlined exactly like the encoder places them on the Wasm stack.
  Block-param transfers are parallel assignments through fresh temps;
  `If` occupies one label depth (branching to it is a break to its end);
  the trailing-`Unreachable` rule is mirrored.
- `audit::audit_module` is wired into the e2e harness's `validate()`
  (plus `lower_body` for every body), so the feature closure and the
  walker are exercised by all 81 fixtures on every run — the living
  contract.
- `tail::tail_callable_set` = static `ReturnCall` targets ∪ `RefFunc`
  targets ∪ declared table elements (conservative superset of dynamic
  `return_call_ref` targets).
- Java skeleton: `final class S{sig}` with public fields + all-args
  constructor (only ≤255 fields — Java's method param limit; the
  257-field global-context struct uses the no-arg constructor and
  field-by-field population), `I{sig}` funcref interfaces, `Mod` with
  static method stubs + export delegates, `W` runtime (WasmTrap, JsNull
  sentinel, IFun marker, non-constant `T` to defeat `javac` reachability
  errors on Wasm-shaped loops).
- Swift skeleton: `final class S{sig}`, `A{sig}` array wrappers
  (reference semantics — Swift arrays are value types), `Fn{sig}`
  funcref closure boxes, `Mod` enum namespace with stubs + export
  delegates, `Runtime.swift` (WasmTrap error, JsNull singleton, IFun
  protocol).
- Gates: `javac` compiles and `swiftc -typecheck` passes the skeleton
  emission of the smallest module fixture (`export function run(a) {
  return a + 1; }`); tool discovery via JAVAC/JAVA_HOME/PATH/Homebrew and
  SWIFTC/PATH, missing toolchains reported as prerequisites (skip), never
  substituted. All 81 e2e fixtures pass the audit + SIR lowering.

---

## Milestone 8 — as built (implementation notes)

- `portal-jsc-jvm-emit` now renders full method bodies from the SIR
  (`render.rs`): all closure operators, structured control flow
  (`Block` → labeled block, `Loop` → one-trip `while (W.T)` with a
  trailing break, `If` occupying one label depth), and expression trees.
- **Tail calls are framed plain calls** (documented stack caveat); the
  e2e JVM runner passes `-Xss256m`, so even the depth-100000 tail
  fixtures pass on the JVM. Milestone 9 replaces this with the
  trampoline.
- **No state-machine fallback was needed**: Stackify's output for
  closure-valid bodies is always label-reducible; `Select`/
  `ReturnCallIndirect` are audit-rejected, so the walker errors (never
  silently falls back) — the plan's "must exist, fires never" fallback
  is the audit itself.
- Java semantic details pinned down:
  - Wasm comparisons/`ref.test`/`ref.eq` produce Java `boolean`, and the
    renderer tracks expression Java-ness (`JTy`) to materialize
    `? 1 : 0` wherever Wasm wants an `i32`.
  - `javac` rejects `instanceof`/casts between unrelated *final* classes
    (e.g. a `ref.test $S19` on an `$S11` value, valid and false in
    Wasm). All tests/casts/field accesses route through an `(Object)`
    detour: `((Object)x) instanceof S19`, `(S19)((Object)x)` — compile-
    time acceptance, runtime ClassCastException as the trap.
  - `javac` reachability: statements after an unconditional terminator
    within a sequence are elided; loops use the non-constant `W.T`
    condition so `javac` always considers their fall-through reachable
    (and the mirrored trailing `Unreachable` keeps semantics).
  - Trapping arithmetic/conversions are `W` helpers (`divS`, `remS`,
    `truncF64I32`, …); saturating unsigned conversions and unsigned
    i64→float use exact helper implementations; `f64.min/max` map to
    `Math.min/max` (NaN and ±0 semantics match Wasm).
  - Struct classes implement the marker `IStruct` so abstract
    `ref.test struct` is one `instanceof`; >255-field structs
    (the global context) use `W.build` + field-wise population.
  - `F32Const`/`F64Const` emit via `intBitsToFloat`/`longBitsToDouble`
    for bit-exact NaN payloads; `i32::MIN`/`i64::MIN` avoid the literal
    pitfall.
- The e2e harness gained a JVM leg in `assert_executes_in_all_runtimes`:
  emit → `javac -proc:none -nowarn -g:none` → `java -Xss256m` with a
  generated `Main` printing `Double.doubleToRawLongBits`, compared
  bit-exactly against Wasmtime/Node. Full suite: **81 passed** (every
  execution fixture on all three runtimes).
- Gate note: JDK discovery via JAVA_HOME/PATH/Homebrew; a missing JDK
  fails loudly (execution) or reports-and-skips (skeleton gate).

---

## Milestone 9 — as built (implementation notes)

- **Protocol.** `W.Step` = `Value(Object) | Tail(Supplier<Step>)`.
  Functions in the tail-callable set (or containing cross-function tail
  calls) get a `fN$step` body returning `W.Step` plus a public `fN`
  trampoline wrapper (`while (s instanceof W.Tail t) s = t.invoke();`).
  Plain functions keep plain bodies.
- **Self `ReturnCall`** renders as final-temp argument assignment +
  `continue selfTail` inside a `selfTail: while (W.T)` wrap of the whole
  body (locals re-declare per iteration, matching frame replacement);
  zero allocation per hop.
- **Cross `ReturnCall`** renders as `return W.Step.tail(() ->
  g$step(args))` (args through `final` temps — Java lambdas capture
  only effectively-final locals).
- **`ReturnCallRef`** renders as `return fn.apply$step(args)`; funcref
  interfaces declare `apply` abstract plus a default value-wrapping
  `apply$step`. `RefFunc` sites create an anonymous class overriding
  both (`apply` → public `fN`, `apply$step` → `fN$step`), because every
  `RefFunc` target is in the tail-callable set by construction — this
  keeps funcref chains O(1): hops are `Step`s consumed by the outermost
  loop instead of trampolines nested per hop (the default `apply$step`
  alone would nest one trampoline per funcref boundary).
- **O(1) evidence**: depth-100000 self and mutual tail fixtures (plus
  raw/union/cross-module/forwarding chains) pass on the JVM at
  `-Xss2m`; the e2e runner now uses `-Xss8m` headroom (a framed
  lowering would need ~256 MB at that depth).
- **Latent M8 bug fixed**: `Return` now coerces the value to the
  declared return type (`? 1 : 0` materialization for Wasm-i32
  booleans) — previously a raw comparison return would not have
  compiled (no fixture happened to hit it).
- **Renderer robustness**: the structured statement walker recurses
  with deliberately thin frames (Block/Loop/If inline) and does all
  leaf work in an `#[inline(never)]` renderer — real CFGs nest
  arbitrarily deeply (`js_property_trie_set_9` nests 257 levels) and
  the fat match frame overflowed the 2 MB test-thread stack before the
  split. Indentation is applied per line at emission time.

---

## Milestone 10 — as built (implementation notes)

- `portal-jsc-swift-emit` renders full bodies from the shared SIR,
  mirroring the Java renderer: labeled `do`/`while true`/`if` for
  structured control flow (Swift labels attach to `do`, loops, and
  `if`; no unreachable-statement errors exist, so no elision pass —
  instead every non-Void body ends with a defensive `fatalError`).
- **The state-machine fallback exists and lives in Swift**: swiftc
  rejects structure nesting beyond 256 levels, and real CFGs (the
  property-trie walkers) nest 257+. Bodies deeper than 220 emit as a
  flat `switch`-in-`while` state machine (one state per statement
  sequence, branches as `pc` assignments, label entry/exit states
  computed by a reverse linearization pass) — the plan's promised
  fallback, scoped to where the structured form cannot compile.
- Swift semantics details:
  - wrapping `&+`/`&-`/`&*`; Swift `/`, `%`, `Int32(Double)` trap
    exactly where Wasm traps; masking shifts `&<<`/`&>>` match Wasm;
    `W` helpers for rotl/rotr, NaN/signed-zero-correct min/max, and
    saturating conversions (Swift has none).
  - all casts/tests route through `Any` (`(x as Any) is S19`,
    `(x as Any) as! S19`) since Swift rejects statically-impossible
    casts; array member access uses `as!`-typed unwrap (non-optional
    inline constructions and optional locals both work).
  - arrays are one generic `AArr<E>` reference-semantics wrapper with
    per-signature typealiases (Swift arrays are value types);
    `array.copy` uses `replaceSubrange`.
  - the trampoline mirrors the JVM: `W.Step` = `W.Value`/`W.Tail`
    classes, `fN_step` bodies, `fN` wrappers, and funcref boxes carry
    both `body` and `step` closures (`$` is not a Swift identifier,
    hence `_step`).
- The e2e harness gained a Swift leg: emit →
  `swiftc -emit-library -emit-module-path` into a content-addressed
  cache dir (dylibs are reused across runs; the math fixture's ~20
  asserts share one compile) → per-call `main.swift` linked against
  the dylib → raw `bitPattern` comparison. Full suite: **81 passed**
  on Wasmtime, Node.js, JVM, and Swift (depth-100000 fixtures
  included).
- Known cost: swiftc dominates suite time (~45 unique module compiles
  at ~40-80s each; repeated runs reuse the on-disk cache).
