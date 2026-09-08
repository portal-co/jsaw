TESTING and Harness Guide

Purpose

This document explains how to write and run tests for this repository and how to integrate runtime/harness tooling (assemblers, compilers, or web runtimes) into the harness/ directory.

Writing tests

- JS projects: use Vitest. Add tests under tests/ with .test.ts/.test.js suffixes and run via `npm test` (which executes `vitest`).
- Rust projects: use cargo test; place integration tests in tests/ as *.rs files.

Harness

- The harness/ directory is the place to add tooling (scripts, container configs, or small runtimes) used by tests.
- Recommend: harness/build.sh to prepare tools (install compilers, build helper binaries), harness/run.sh to execute the harness, harness/docker/ for docker images.

How to run

- JS: npm install && npm test
- Rust: cargo test

## Cross-runtime e2e matrix

`crates/portal-jsc-waffle/tests/e2e.rs` runs every execution fixture on
four runtimes and compares raw f64 results bit-for-bit:

- **Wasmtime** (in-process) and **Node.js** (required) run the emitted
  WasmGC module.
- **JVM**: the module is emitted as Java by `portal-jsc-jvm-emit`,
  compiled with `javac`, and run with `java -Xss8m`. JDK discovery:
  `$JAVA_HOME/bin`, `java`/`javac` on `PATH`, then the Homebrew
  `/opt/homebrew/opt/openjdk` keg. A missing JDK is a hard failure for
  execution tests (the skeleton gate in `portal-jsc-jvm-emit` reports
  the prerequisite and skips instead).
- **Swift**: the module is emitted as Swift by `portal-jsc-swift-emit`,
  compiled with `swiftc -emit-library` into a content-addressed cache
  dir (`/tmp/swift_e2e_<hash>` — reused across runs), and executed via a
  per-call `main.swift` linked against the dylib. Requires a Swift
  toolchain (`$SWIFTC` or `swiftc` on `PATH`).

Note: the first full run pays ~40-80 s of `swiftc` time per unique
module; subsequent runs reuse the on-disk cache.

Specific functionality to add (placeholder)

- TODO: List specific functions, modules, or behavior to test in this repo (manually update this section).
