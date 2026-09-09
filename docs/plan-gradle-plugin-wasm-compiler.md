# Plan: Gradle plugin driving the wasm-compiled jsaw compiler

Status: proposed. This is the packaging half of
`plan-wasmgc-mobile-backends.md`: the mobile emitters turn a waffle IR
module into Java/Swift source; this plan makes a Gradle task produce that
source (and optionally the WasmGC `.wasm`) from JS, on any machine with a
JDK, by running the **whole compiler compiled to WebAssembly** inside the
build.

## 1. Decision summary (and alternatives considered)

Run the compiler as a **Wasm module inside the Gradle daemon**, not as a
native binary and not as Node/JS:

- Native binary per (OS × arch): six+ artifacts to build/publish, JNI or
  `Process` spawning, macOS notarization pain, and Android-Gradle-Plugin
  machines that can't exec arbitrary binaries. Rejected.
- Compile the compiler to JS and run on Node: needs a Node toolchain in
  the build (the thing this repo removes), and swc-in-JS via wasm-bindgen
  is itself a wasm-in-wasm problem. Rejected.
- **Wasm inside the JVM (chosen)**: one platform-independent artifact
  (`jsaw-compiler.wasm`), runs anywhere the Gradle daemon runs, sandboxed,
  no external toolchain. This is also the dogfood: the whole pipeline is
  "JS → (wasm-compiled compiler) → Java/Swift/WasmGC".

Runtime: **Chicory** (pure-Java Wasm runtime, no JNI — the safe default
for arbitrary CI/agents). Wasmtime-on-JVM exists (kawamuray) and is faster
(AOT) but needs per-OS native libs and unsafe memory access; keep Chicory
as the default and leave a plugin property to opt into a native runtime
later. The plan does not hard-block a swap: the runtime lives behind a
small interface.

Target: **wasm32-wasip1** (already installed on this machine). WASI gives
the compiler `stdin/stdout` + preopened directories as the entire I/O
surface — exactly the sandboxing and input/output model a compiler wants,
with zero host shims. wasm32-unknown-unknown would require hand-rolled
memory/string imports; wasip1 is the right target.

## 2. Architecture

```
┌──────────────────────────── Gradle daemon (JVM) ───────────────────────────┐
│  JsawCompileTask (Kotlin)                                                   │
│    ├── collects module set: source dir + files, entry, options              │
│    ├── computes cache key: compiler wasm hash + source hashes + options     │
│    ├── launches Chicory: jsaw-compiler.wasm (wasip1)                        │
│    │     args: --entry <path> --out-dir <dir> --emit <targets> [flags]      │
│    │     preopens:  <source root> → /src  (read-only)                       │
│    │                 <staging dir> → /out  (read-write)                     │
│    │     stdin: JSON manifest (module set + options)                        │
│    └── moves staged outputs into build/generated/..., wires source sets     │
└─────────────────────────────────────────────────────────────────────────────┘
┌── jsaw-compiler.wasm (Rust → wasm32-wasip1) ────────────────────────────────┐
│  crates/jsaw-wasi-bin: reads manifest, walks /src for the module set,       │
│  parses each .js (swc) → CFG → TAC → SSA, convert_modules → waffle Module,  │
│  then: to_wasm_bytes → /out/*.wasm, and/or emit_java → /out/java/...,       │
│  and/or emit_swift → /out/swift/...  ; writes JSON result to stdout.        │
└─────────────────────────────────────────────────────────────────────────────┘
```

The Gradle task never parses JS, never resolves modules, never touches
wasm internals — the wasm binary does all of it against its WASI view of
the source tree. That keeps the plugin thin and the compiler the single
source of truth for resolution rules.

## 3. New crate: `crates/jsaw-wasi-bin`

A thin `main.rs` (binary, `edition = "2024"`). Dependencies: the existing
crates only — `portal-jsc-waffle` (parsing helpers + `convert_modules` +
`to_wasm_bytes`), `portal-jsc-mob-emit`, `portal-jsc-jvm-emit`,
`portal-jsc-swift-emit`, plus `swc_common`/`swc_ecma_parser` and a
JSON crate (`serde`/`serde_json`, wasm-clean). **No new compiler logic.**

### 3.1 Interface (stable, versioned)

`stdin` manifest:
```json
{
  "version": 1,
  "entry": "index.js",
  "modules": ["index.js", "lib/a.js"],
  "options": { "numericExports": true, "gcExportSuffix": null },
  "emit": {
    "wasm":  "out/module.wasm",
    "java":  "out/java",
    "swift": "out/swift"
  }
}
```
- `modules` are paths **relative to the preopened `/src` root**, matching
  the linker keys (`resolve_specifier` joins on `/`); the binary reads each
  file, parses to `SModule`, and inserts into `ModuleSet` under that
  relative path. Resolution of `./a.js` imports is then already correct.
- `emit.*`: null or a destination path (relative to the preopened `/out`
  root). Any combination of targets.

`stdout` result: `{ "ok": true, "exports": ["run", ...], "outputs":
["out/module.wasm", ...] }` or `{ "ok": false, "error": "..." }`. All
`ConvertError` / parse diagnostics funnel into `error` (structured enough
for the task to fail the build with a useful message). Panics are caught
(`catch_unwind` is unavailable on wasm — instead set a panic hook writing
to stderr and return nonzero) so a compiler bug fails the task cleanly
rather than hanging the daemon.

### 3.2 wasm32-wasip1 build requirements

- `RUSTFLAGS = "-C target-feature=+simd128"` is NOT needed (audit forbids
  SIMD); a plain `cargo build --target wasm32-wasip1 --release` suffices.
- swc compiles to wasip1 today (swc itself ships wasi builds); the
  `GLOBALS.set(&Globals::default(), …)` thread-local pattern from the e2e
  harness is used verbatim — wasip1 is single-threaded, which is fine.
- The local `[patch]` of `portal-pc-waffle` to `../waffle-` applies to the
  bin crate the same way; the treeify fix is on the compile path.
- **WASM GC is not involved here**: the compiler wasm is ordinary
  MVP+reference-types wasm (Chicory supports what wasip1 needs). Only the
  *output* `.wasm` uses GC — and Chicory never executes it; it is copied
  out as data.
- Size: expect 30–60 MB unoptimized; `wasm-opt -O2` (already a dev tool)
  trims it. A `.wasm` of that size is fine to ship in the plugin jar.

### 3.3 Determinism

Same sources + same options ⇒ byte-identical outputs. The ModuleSet is a
`BTreeMap` (deterministic), and both emitters are deterministic. The task
relies on this for Gradle up-to-date checks and the build cache: the cache
key is `hash(compiler.wasm) + hash(each source file) + options`.

## 4. The Gradle plugin

New directory `gradle/jsaw-gradle-plugin/` (Kotlin, `java-gradle-plugin`
+ `kotlin("jvm")`), published as `dev.portal.jsaw` (coordinates TBD;
the repo previously used `maven.local()` + `Portal-Solutions` group).
The deleted M6 Gradle skeleton was Android-app-focused and native-based;
this plugin is a fresh, library-style code-generation plugin.

### 4.1 Extension

```kotlin
jsaw {
    modules.register("main") {
        entry.set("index.js")
        sourceDir.set(layout.projectDirectory.dir("src/main/js"))
        emitWasm.set(false)                 // → build/generated/jsaw/main/wasm
        emitJava.set(true)                  // → build/generated/jsaw/main/java
        emitSwift.set(false)                // → build/generated/jsaw/main/swift
        numericExports.set(true)
        gcExportSuffix.set(null as String?)
    }
}
```

### 4.2 Task (`JsawCompileTask`, one per registered module set)

- **Inputs** (for up-to-date + build cache): `sourceDir` (`@InputDirectory`
  + `PathSensitive(RELATIVE)`), `entry`/`emit*`/`options` (`@Input`), the
  compiler wasm (`@InputFile`, from a `Configuration` — see §4.3).
- **Outputs**: `build/generated/jsaw/<name>/{wasm,java,swift}`
  (`@OutputDirectory` per enabled target).
- **Action**: build the JSON manifest from the module-set file listing +
  options; run Chicory with `/src` = `sourceDir` (read-only) and `/out` =
  a per-task staging dir (read-write); parse stdout JSON; on `ok:false`
  throw `GradleException(error)`; copy staged outputs to the output dirs.
- **Worker API**: run the compile in a `WorkerExecutor` with classloader
  isolation so Chicory and its deps don't leak into the daemon's
  classloader; keeps memory bounded and lets the task be a build-cache
  hit on other machines.
- **Wiring**: if `emitJava`, register the java output dir as a generated
  source dir on the consuming source set (and `compileJava.dependsOn`
  the task); if the `com.android.library` plugin is present, add to the
  Android source set instead. `emitWasm` output is exposed as a task
  output property for consumers (e.g. packaging, or the swift half is
  consumed by an Xcode build outside Gradle — documented, not wired).

### 4.3 Compiler artifact resolution

The plugin needs `jsaw-compiler.wasm`. Three modes, first wins:

1. `jsaw.compilerWasm.set(file(…))` — explicit local override (dev loop).
2. A detached `Configuration` resolving the published artifact
   `dev.portal.jsaw:jsaw-compiler-wasm:<version>@wasm` from the repo's
   Maven (published by CI from `jsaw-wasi-bin` release builds).
3. `jsaw.compilerProject.set(":jsaw-wasi-bin")` — a composite/ included
   build that runs the Rust build via a small Exec task (documented; for
   hacking on compiler + plugin together).

Default to (2) with the plugin's own version, so consumers get a matching
compiler and the deterministic cache key includes the wasm bytes hash.

### 4.4 Plugin tests

- Gradle TestKit `functionalTest`: a fixture project with 2–3 JS modules
  (including a relative import and a deep tail-recursion function),
  `apply plugin: "dev.portal.jsaw"`, run `:compileJsawMain`, assert the
  generated `.wasm` validates (reuse the wasmparser GC check via a tiny
  helper) and the generated Java compiles (javac task wired by the
  plugin). One test with `emitSwift` asserts the Swift files exist and
  contain the expected export delegate signature.
- A "cache" test: run the task twice, assert `UP-TO-DATE` / `FROM-CACHE`
  the second time.

## 5. Repository changes

- New workspace member `crates/jsaw-wasi-bin` (bin + small lib for the
  manifest/marshalling so it is unit-testable).
- `Cargo.toml` workspace: add the member. No changes to existing crates
  except optionally re-exporting the multi-module parse helper currently
  living in `tests/e2e.rs` (parse → CFG → TAC → SSA → ModuleSet) as a
  `portal-jsc-waffle` util so the bin and tests share it. That helper is
  the ONLY logic moved out of tests.
- `gradle/jsaw-gradle-plugin/`: plugin project (settings, build script,
  `JsawPlugin`, `JsawExtension`, `JsawCompileTask`, functional tests).
- CI (follow-up, noted not built here): release workflow building
  `jsaw-wasi-bin` for `wasm32-wasip1`, `wasm-opt`, publish the `.wasm` to
  Maven alongside the plugin.

## 6. Milestones

- **Milestone 12 — `jsaw-wasi-bin` core.** New crate; manifest parsing;
  module-set walk + parse (sharing the extracted util); `convert_modules`
  → `to_wasm_bytes`; JSON result; smoke test: build for `wasm32-wasip1`,
  run under `wasmtime run` (dev dependency, already in tree) on a fixture
  module set, assert the output `.wasm` validates with the GC feature.
- **Milestone 13 — emitters in the bin.** Wire `emit_java` / `emit_swift`
  targets writing into `/out`; golden tests comparing the emitted Java /
  Swift against `emit_java`/`emit_swift` run natively on the same fixture
  (must be byte-identical — pins determinism of the wasm build).
- **Milestone 14 — Gradle plugin.** The extension/task/worker wiring of
  §4, chicory dependency, staging + generated-source registration, modes
  (1) and (2) of §4.3. TestKit functional tests incl. cache test. Docs:
  `docs/gradle-plugin.md` (usage, cache behavior, modes) + README section.

Milestone ordering keeps the risky part (does the whole swc+jsaw stack
compile and run correctly on wasip1) first, before any Gradle code exists.

## 7. Risks and mitigations

1. **A transitive dependency doesn't compile for wasip1** (a proc-macro
   free crate using `std::net`, `getrandom`, threads, etc.). Mitigation:
   Milestone 12 is the first thing built; if a dep fails, gate it out or
   substitute (e.g. `getrandom` needs the `js` feature off; swc is known
   wasi-clean). This is THE risk and it is front-loaded.
2. **Chicory wasip1 coverage gaps** (e.g. `path_open` flags, `fd_readdir`
   the module-set walk needs). Mitigation: the bin uses the simplest WASI
   calls (`fd_read`/`fd_write` on preopened dirs + `path_open` read-only);
   the functional test on the actual fixture surfaces gaps immediately;
   fallback is a tiny hand-rolled WASI import shim in the plugin.
3. **Performance**: Chicory interprets (its AOT compiles to JVM bytecode
   and is the default in 1.x — acceptable). Compiling a module set is a
   cold, cacheable, per-build-step cost; the build cache makes it a
   once-per-input-change cost. No per-test or per-class invocation.
4. **Memory**: swc on a big module set can use hundreds of MB; the worker
   gets a bounded `--max-heap`, and the manifest batches all modules in
   one run (one wasm instance per task execution, not per file).
5. **Local waffle `[patch]`**: the wasip1 build uses the same patched
   treeify fix; if the local checkout moves, CI must build the wasm from
   the pinned rev — the published artifact pins this for consumers.

## 8. What is deliberately out of scope

- Executing the **output** WasmGC `.wasm` on the JVM (would need a GC-
  capable runtime; Chicory does not support the GC proposal). The wasm
  output is for Node/Wasmtime embedders; the JVM/Android consumer uses
  `emitJava`.
- A Maven-publish CI workflow (sketched in §5, not built in these
  milestones).
- Incremental per-module compilation (the manifest already batches; the
  cache key is the whole set).
- Any change to the compiler's semantics, resolution rules, or emitted
  code — this plan only packages what exists.

## Milestone 12 — as built

`crates/jsaw-wasi-bin` compiles the whole swc+jsaw stack to
`wasm32-wasip1 --release` with **zero source changes** (5.3 MB wasm,
~25 s build) — the plan's highest-risk item cleared on the first attempt.
The crate is a lib + thin bin:

- `src/manifest.rs` — the versioned schema (`Manifest`/`Options`/`Emit`,
  `Result` as a `#[serde(tag = "status")]` enum). One as-built deviation:
  `Options` implements `Default` by hand (not derived) so
  `numeric_exports` defaults to `true` in both the serde path and the
  direct-construction path — a derived `Default` had set it `false` and
  silently dropped every numeric export.
- `src/lib.rs` — `compile_to_outputs(&Manifest, &sources)` is the
  target-independent core (manifest check → `module_set_from_sources` →
  `convert_modules` → per-target emission into an in-memory
  path→bytes map), plus `read_sources`/`write_outputs` (with
  path-escape rejection) for the binary.
- `src/main.rs` — parses `--src`/`--out`, reads the manifest from stdin,
  prints one result JSON line to stdout, sets a panic hook to stderr, and
  exits 0/1 by status.
- The multi-module ingestion pipeline (parse → CFG → TAC → SSA →
  `ModuleSet`, owning the swc `GLOBALS` scope) moved out of
  `tests/e2e.rs` into `portal-jsc-waffle::ingest` (new public module);
  the e2e `lower_modules` now calls it, so the harness and the CLI share
  exactly one front half.

Tests (`tests/wasi_run.rs`, 3 passing) build the wasip1 wasm (cargo
unconditionally, so it is never stale), run it under `wasmtime` with
`wasmtime-wasi` p1 (`--src`/`--out` as preopened dirs, manifest on a
memory-pipe stdin), and assert: a multi-module fixture (entry with a
relative import + a deep tail-recursive function) compiles to a
`module.wasm` that validates with the GC feature and reports `run` /
`count` in `exports`; the same driver natively sees the exports (an
isolation test for wasip1-only divergence); and a bad entry yields a
structured `{"status":"error"}` with exit code 1. Full e2e stays green
(81 passed).

## Milestone 13 — as built

`emit_java` and `emit_swift` were already reachable in the M12 driver, so
this milestone's substance is the determinism guarantee. The new golden
test `java_and_swift_outputs_match_native_emission_byte_for_byte` runs
the wasip1 binary on the fixture with `emit.java` and `emit.swift` set,
collects every written file into a path→bytes map, and asserts it equals
`compile_to_outputs` run natively on the same module — **byte-for-byte
equal on the first attempt**, confirming the wasm build is fully
deterministic (matching the `BTreeMap`-keyed `ModuleSet` and the
deterministic emitters) and that both emitters behave identically inside
the WASI sandbox. This is the property the Gradle build cache relies on:
the cache key `hash(compiler.wasm) + source hashes + options` is sound
because compilation is a pure function of those inputs.

## Milestone 14 — as built

`gradle/jsaw-gradle-plugin/` is a Kotlin `java-gradle-plugin` +
`maven-publish` project publishing `dev.portal.jsaw`. It compiles and its
`check` (unit + TestKit functional) is green against Gradle 8.10.2 on JDK
21.

Structure:
- `JsawExtension` / `JsawModuleSet` — the `jsaw { modules { register("main") { ... } } }`
  DSL (entry, sourceDir, emitWasm/Java/Swift, numericExports,
  gcExportSuffix), plus `compilerWasm` / `compilerProject` for the three
  compiler-resolution modes.
- `Manifest` — hand-rolled JSON build/parse, in lockstep with
  `crates/jsaw-wasi-bin/src/manifest.rs`.
- `JsawRunner` — the Chicory invocation: `WasiOptions` with
  stdin/stdout/stderr pipes, `/src` (read-only) and `/out` (read-write)
  preopens, `WasiPreview1` host functions into an `Instance` with
  `withStart(false)` and an explicit `_start` call, catching
  `WasiExitException` for the exit code.
- `JsawCompileTask` — `@Cacheable`, inputs = compiler wasm + source dir +
  scalar options, outputs = the enabled target dirs; enumerates the
  module set, builds the manifest, runs the compiler into a staging dir,
  moves outputs to `build/generated/jsaw/<name>/{wasm,java,swift}`, and
  fails with the compiler's structured error. When `emitJava` is on, the
  output dir is wired into the `main` source set and `compileJava`
  depends on the task.
- `JsawPlugin` — registers the `jsawCompiler` configuration (default
  dependency `dev.portal.jsaw:jsaw-compiler-wasm:<version>@wasm`) and one
  `compileJsaw<Name>` task per module set.

As-built deviations / notes:
- **Chicory API (1.7.5)**: WASI imports come from
  `WasiPreview1.toHostFunctions()` into `ImportValues`, and `_start` is a
  plain export invoked via `instance.export("_start").apply()` with
  `withStart(false)` (a wasip1 command module has no start *section*);
  `proc_exit(code)` throws `WasiExitException`, whose `exitCode()` is the
  status. This differs from the `wasmtime-wasi` p1 shape used in the M12
  Rust test but is the same WASI contract.
- **Gradle 8.10.2 requires a JDK it recognizes** — it runs on JDK 21 but
  rejects JDK 26 (`26.0.1` as an opaque error). Build the plugin with
  `JAVA_HOME` pointing at a supported JDK (21 here).
- A task bug found by the functional test: the staging dir was itself
  under the output root and the manifest paths were `out/...`-prefixed,
  double-nesting outputs; fixed by staging into `<root>/staging` with
  root-relative emit paths (`wasm/module.wasm`, `java`, `swift`).

Tests: `JsawRunnerTest` drives Chicory directly (asserts the wasm and
`Mod.java` are written and the result is `ok`); four TestKit functional
tests assert valid-WasmGC + Java generation, generated-Java compilation
via the source-set wiring, `UP-TO-DATE` on a second run, and a structured
compiler error failing the build. `docs/gradle-plugin.md` is the consumer
guide; the README gained a Layout section.
