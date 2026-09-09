# The jsaw Gradle plugin

The `dev.portal.jsaw` plugin compiles a JavaScript module set to WasmGC,
Java, and/or Swift inside a Gradle build, on any machine with a JDK — no
Node, no native compiler, no other toolchain. It runs the **whole jsaw
compiler, itself compiled to a `wasm32-wasip1` binary**, inside the
Gradle daemon via [Chicory](https://github.com/dylibso/chicory) (a
pure-Java Wasm runtime with WASI support). See
`docs/plan-gradle-plugin-wasm-compiler.md` for the design.

## Quick start

```kotlin
// build.gradle.kts
plugins {
    java                      // or `com.android.library` etc.
    id("dev.portal.jsaw") version "0.1.0"
}

jsaw {
    modules {
        register("main") {
            entry.set("index.js")
            sourceDir.set("src/main/js")
            emitWasm.set(true)     // -> build/generated/jsaw/main/wasm/module.wasm
            emitJava.set(true)     // -> build/generated/jsaw/main/java/...
            emitSwift.set(false)   // -> build/generated/jsaw/main/swift/...
        }
    }
}
```

Then `./gradlew build` runs `compileJsawMain`, which compiles
`src/main/js/index.js` (and every `.js`/`.mjs` it transitively imports)
and writes the enabled outputs. When `emitJava` is on, the generated Java
is wired into the `main` source set and `compileJava` depends on the
task, so the generated code is compiled with the rest of the project.

## How it works

1. The task enumerates every `.js`/`.mjs` under `sourceDir`, keyed by its
   root-relative path (those keys are how the compiler resolves
   `./sibling.js` imports).
2. It builds a versioned JSON manifest (entry, module list, options,
   enabled targets) and runs the compiler wasm once via Chicory, with
   `sourceDir` preopened read-only at `/src` and a staging dir preopened
   read-write at `/out`.
3. The compiler wasm parses each module (swc), lowers CFG → TAC → SSA,
   links the closed module set, and emits WasmGC bytes and/or Java/Swift
   sources into `/out`, printing a single result JSON line on stdout.
4. The task moves the staged outputs to `build/generated/jsaw/<name>/`
   and, on a compiler error, fails the build with the compiler's message.

Compilation is a **pure function** of the compiler wasm, the sources, and
the options (proven byte-identical by the wasip1↔native golden tests), so
the task is `@Cacheable` and participates in Gradle up-to-date checks and
the build cache: re-running with unchanged inputs is `UP-TO-DATE`.

## Where the compiler wasm comes from

The task needs `jsaw-compiler.wasm` (built from `crates/jsaw-wasi-bin`
with `cargo build --target wasm32-wasip1 --release`). Three modes, first
wins:

1. **Explicit override** (the dev loop):
   ```kotlin
   jsaw { compilerWasm.set("/path/to/jsaw-wasi-bin.wasm") }
   ```
2. **Published artifact** (default): the plugin resolves
   `dev.portal.jsaw:jsaw-compiler-wasm:<pluginVersion>@wasm` from the
   configured repositories. (CI publishes this alongside the plugin.)
3. **Subproject** (`jsaw.compilerProject.set(":jsaw-wasi-bin")`): an
   included build that runs the Rust build, for hacking on compiler and
   plugin together.

## Module-set rules

- **Closed world**: every module the entry transitively imports must be a
  `.js`/`.mjs` file under `sourceDir`. Only relative `./` (and `../`)
  specifiers resolve; bare/npm specifiers are a compile error.
- **Function exports only** cross modules in this milestone (matching the
  compiler's `convert_modules`): named/renamed/default function exports.
- The numeric export boundary is `f64`-only, so generated Java/Swift
  functions take and return `double`/`Double` — no string marshalling.

## Outputs

| Target | Path | Consumer |
|---|---|---|
| `emitWasm` | `…/wasm/module.wasm` | Node / Wasmtime embedders (WasmGC) |
| `emitJava` | `…/java/pc/portal/mob/*.java` | compiled into the project |
| `emitSwift` | `…/swift/*.swift` | an Xcode build (outside Gradle) |

## Caching and performance

- The task is `@Cacheable` with the compiler wasm (`@InputFile`), the
  source dir (`@InputDirectory`, relative path sensitivity), and the
  scalar options as inputs; the enabled output dirs as outputs.
- Compiling is a cold, once-per-input-change cost; the build cache makes
  it a no-op on other machines and branches. Chicory interprets (its AOT
  compiles to JVM bytecode), which is acceptable for a per-build-step
  compile.

## Requirements

- A JDK on the build machine (the plugin runs in the Gradle daemon; no
  other runtime). Gradle 8.10+.
- The compiler wasm (see above). Building it from source additionally
  needs Rust with the `wasm32-wasip1` target.

## Tests

- `src/test` — `JsawRunnerTest` drives Chicory directly against the real
  compiler wasm (no Gradle machinery).
- `src/functionalTest` — TestKit tests applying the plugin to a fixture
  project: valid WasmGC + Java generation, generated-Java compilation via
  the source-set wiring, up-to-date behavior, and a structured compiler
  error failing the build.
