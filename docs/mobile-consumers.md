# Embedding the mobile emitters' output (Java / Swift)

The mobile backends compile the same `portal-pc-waffle` IR module the
WasmGC backend emits — same frontend, same optimizations — into Java or
Swift source. This guide covers taking that output into a JVM/Android or
Swift/Xcode project. See `docs/plan-wasmgc-mobile-backends.md` for the
architecture.

## Producing the sources

```rust
// Rust, in a build step:
let module: portal_pc_waffle::Module = /* convert_modules(...) */;
let java = portal_jsc_jvm_emit::emit_java(&module)?;
java.write_to("generated/java")?;
let swift = portal_jsc_swift_emit::emit_swift(&module)?;
swift.write_to("generated/swift")?;
```

The emitters first run the feature-closure audit
(`portal_jsc_mob_emit::audit::audit_module`); modules outside the closure
the jsaw compiler emits are hard errors, never silent fallbacks.

## What gets emitted

Java (package `pc.portal.mob`):

- `W.java` — runtime: `WasmTrap`, the `JsNull` sentinel, `IFun` funcref
  marker, the `Step`/`Value`/`Tail` trampoline, and the exact numeric
  helpers (trapping arithmetic/conversions, `min`/`max`).
- `S{i}.java` — one `final class` per WasmGC struct signature, public
  fields, all-args constructor (structs over Java's 255-parameter limit
  are built field-wise via `W.build`).
- `I{sig}.java` — functional interfaces for typed funcrefs (`apply` plus
  the trampoline-protocol `apply$step`).
- `Mod.java` — one `public static` method per module function, plus one
  public delegate per export. Numeric exports keep the
  `double x arity -> double` host ABI.

Swift (single module, one file per type):

- `Runtime.swift` — `W` (trap type, `Step`, numeric helpers, array
  ops), the generic `AArr<E>` array wrapper, `JsNull`, `IFun`,
  `IStruct`.
- `S{i}.swift` — one `final class` per struct signature.
- `A{i}.swift` — per-signature `AArr` typealiases.
- `Fn{sig}.swift` — funcref boxes (`body` + trampoline `step`).
- `Mod.swift` — one `public static func` per module function, plus
  export delegates (`Double x arity -> Double`).

## Embedding

**Android / JVM.** Add `generated/java` as a source directory (or copy
the files into your package tree). Call the numeric export delegates:
`double y = pc.portal.mob.Mod.run(x);`. No third-party dependencies; the
generated code targets any JDK 17+ (it uses `instanceof` pattern
binding only in `Mod` wrappers — remove if you need older).

**Xcode / SwiftPM.** Add the `generated/swift` files to a target.
`Mod.run(x)` etc. are `public`. Very large modules compile slowly; the
state-machine fallback (bodies nested deeper than 220 levels) is
emitted automatically because `swiftc` rejects nesting beyond 256.

## Semantics notes

- **Traps.** Wasm traps become `W.WasmTrap` (Java) / `fatalError` or a
  Swift runtime trap. Failed `ref.cast`s surface as
  `ClassCastException`/failed `as!` casts. Null `struct.get`/`array.get`
  trap via NPE / force-unwrap.
- **Tail calls.** `return_call`/`return_call_ref` are O(1) stack through
  the trampoline: self-calls loop in place; cross calls flow
  `W.Step` thunks through the public wrapper's loop. Depth-100000 tail
  recursion passes at `-Xss2m` on the JVM in the e2e suite.
- **Identity.** `ref.eq` is Java `==` / Swift `===`; the i31 JS-null
  sentinel is the `JsNull` singleton.
- **Numbers.** Java booleans materialize to Wasm `i32` as `? 1 : 0`;
  float constants are bit-exact (`intBitsToFloat`/`bitPattern`);
  `f64.min/max` match Wasm NaN and ±0 behavior via `W` helpers.

## Limits and hazards

- **JVM 64 KiB method-code limit.** The largest method emitted for the
  current corpus is ~30 KB of bytecode (measured via `javap`), ~2×
  headroom. Very large single source functions could exceed it; the fix
  would be method splitting (not implemented — file an issue if your
  module trips a `code too large` error).
- **Swift compiler scaling.** A full module compiles in ~40-80 s; the
  e2e cache (`swift_e2e_<content-hash>`) reuses on-disk dylibs across
  runs.
- **Threading.** Generated code is plain static methods and instance
  state — thread-safety is the embedder's concern, exactly as with the
  Wasm module.
