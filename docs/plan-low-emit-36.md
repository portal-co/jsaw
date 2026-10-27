# Plan: DreamComp #36 — low-level emitter skeleton and representation trait

Design and operator decisions:
[`docs/jsaw-low-level-backends-scout.md`](https://github.com/portal-co/dreamcomp/blob/main/docs/jsaw-low-level-backends-scout.md)
(DreamComp repo) §8. This module is the shared machinery for jsaw's C, Rust,
and MiniBC backends (#38/#39/#40).

## Layout

- `crates/portal-jsc-waffle/src/low_emit.rs` — the target-neutral IR
  (`LeModule`/`LeFunc`/`LeStmt`/`LeExpr`), the `Representation` trait, the
  emission context (`InstCx`/`Tables`/`FnState`), scalar operator tables,
  `TypeIdTable`, `heap_test_mode`, `RuntimeSink`.
- `crates/portal-jsc-waffle/src/low_emit/skeleton.rs` — the plan walk:
  scalar lowering, flat-CFG translation with temp-mediated parallel-move
  edges, call plumbing (direct/ref/indirect), the step-protocol tail-call
  trampoline, boundary adaptation, CoreGC root discipline, fail-closed
  diagnostics.
- `crates/portal-jsc-waffle/src/low_emit/nano.rs` — arena-offset references
  (8-byte words, `{type_id, len}` headers, bump allocator through the
  `js_nano_arena_ensure` runtime entry point with a zero-fill contract).
- `crates/portal-jsc-waffle/src/low_emit/coregc.rs` — thin header'd
  pointers, native-struct layouts, descriptor static, root-stack runtime
  contract (`js_coregc_alloc`/`js_coregc_root_*`), GC-allocated trampoline
  frames with a no-reference descriptor.
- `crates/portal-jsc-waffle/src/low_emit/debug.rs` — the smoke consumer:
  stable text rendering plus structural validation (labels resolve, locals
  in range, calls reference declared functions).

## Key contracts (module header of `low_emit.rs` is authoritative)

- References are one word: 0 = null, odd = i31, else lane storage.
- Tail calls: step functions `F$step(frame) -> frame` + plain wrappers;
  slot 0 of the frame is the next callee word; tables 0/1 are the
  funcref-value and step-dispatch tables.
- Traps are explicit `LeStmt::Trap`; mid-block traps live in helpers.
- Runtime entry points are name-bound (`LeCallee::Runtime`), declared in
  each representation's `emit_runtime` manifest, implemented per target.

## Tests

- `tests/low_emit_smoke.rs` — every corpus family emits on both lanes,
  deterministically, with zero structural violations.
- `tests/low_emit_unit.rs` — minimal known-shape emission, CoreGC
  header/descriptor/runtime declarations, nano runtime declaration, and the
  trampoline on a mutual-recursion case.
