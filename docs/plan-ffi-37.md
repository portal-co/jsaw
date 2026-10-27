# Plan: structured FFI and the `~ffi` virtual module (DreamComp #37)

**Status:** implemented (jsaw, uncommitted-pending-push at writing).

Two halves, both required by the low-level backend stage (#38–#40):

## 1. Structured import declarations

The `$wasm$` name-suffix grammar is removed. Host ABIs are declared in the
structured FFI manifest (`portal_jsc_waffle::FfiManifest`, carried on
`ConvertOptions::ffi` and the `jsaw-wasi-bin` manifest's `ffi` field):

- `wasm:<module>` named imports resolve against `manifest.modules`; any
  undeclared import fails closed, even when unused.
- `manifest.exports` gives a named source export a raw host-ABI wrapper
  under its plain name (the generic numeric export is suppressed for it).
- `wasm_boundary.rs` keeps only the `wasm:` specifier grammar and the
  legacy-suffix migration diagnostic.

## 2. `~ffi` virtual module (DreamCompat-style factories)

`import { ffi, struct, opaque } from "~ffi"` — call-only compiler intrinsics
(value use is rejected), optional (inactive unless imported) and stackable
with `wasm:` and ordinary ESM imports. `callback`/`contains` fail closed
with named diagnostics pointing at DreamComp #42.

- `ffi(sym, sig)` parses dreamcomp's hybrid signature grammar with the real
  `dreamcompiler-ffi-sig` crate (byte-identical sigs), which **moved to this
  repo** (`crates/dreamcompiler-ffi-sig`). A prescan over the module set
  finds the literal-arg calls (plain and tail position) so raw imports
  `ffi.<sym>` / `ffi.<sym>_<method>` can be declared import-first.
- Boundary mapping: `slot`/`P<rid>` → `ref`, `i32` → `i32`, `ptr` → `i64`,
  `void` → no result. Thunk results with `P<…>` type are wrapped in an
  object carrying the sig's method table; method natives receive the
  wrapper's hidden `\u{1}ffi_handle` as the first host argument.
- `struct('extern x: i32, …')` returns a constructor building plain objects
  with the declared fields (jsaw is extern-only; it never computes host
  layouts). `opaque(name)` returns a non-callable marker object.
- All factory arguments must be compile-time string literals; template
  splices fail closed (#42).

## Latent bug fixed

`dreamcompiler-ffi-sig`'s type-word lexer accepted only alphabetic
characters, so `i32` never parsed (`UnknownAbiType("i")`) — DreamComp's own
sigs only use `slot`/`ptr`/`P<…>`, so it never fired. Fixed with a
regression test (`parse_i32_roundtrip`).

## Tests

- `tests/e2e.rs`: `ffi_factory_executes_against_host_imports` (wasmtime
  host imports, method-table wrapper, i31 handle round-trip),
  `struct_factory_builds_plain_objects` and `opaque_factory_makes_marker_objects`
  (all runtimes), `ffi_factories_fail_closed` (callback, non-literal args,
  non-extern struct, unknown member), plus the rewritten manifest-form
  `wasm:` host tests.
- `tests/low_emit_unit.rs`: FFI imports flow into the low-level module's
  boundary records (`ffi_imports_flow_through_boundary_records_only`).
