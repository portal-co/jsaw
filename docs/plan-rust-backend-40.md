# Plan: Rust backend over the low_emit skeleton (DreamComp #40)

**Status:** implemented (this commit).

`BackendPlan` → stable Rust source via the #36 skeleton, both lanes. Mirrors
the C backend's structure (`low_emit/c_render.rs`); see
`docs/plan-c-backend-38.md` for the shared design.

## Files

- `src/low_emit/rust_render.rs` — `LeModule` → Rust (`render_rust_file`),
  plus `low_emit::emit_rust(plan, rep)`.
- `src/low_emit/rust_runtime.rs` — fixed per-lane runtime texts (prelude,
  nano arena, CoreGC collector), same contracts as the C runtime.

## Rust-specific rendering decisions

- `unsafe` is confined to documented boundaries: raw-pointer heap access,
  function-table transmutes, `static mut` arena/root state.
- Wrapping arithmetic via `wrapping_*`; `wrapping_shl/shr` mask the shift
  count exactly per wasm. Division/remainder via helpers with wasm trap
  semantics. Float→int `as` casts saturate natively (exactly `TruncSat`;
  the trapping conversions are in-range by skeleton guard).
- f32 arithmetic is true single precision; constants are bit-exact
  (`f64::from_bits`); float globals initialize via const `from_bits`.
- Rust has no `goto`: block CFGs render as a state machine —
  `pc = match pc { <id> => { ...; <next id> }, ... _ => js_trap() }` —
  with `If` in arm-tail position yielding per-arm target ids, and diverging
  terminators (`return`, `js_trap()`) unifying via `!`.
- Function tables are match-dispatch fns returning `usize` addresses
  (`fn as usize` is not const-evaluable, so a static array cannot hold
  them); indirect calls `transmute` back through the call's signature.
- Value words are `usize` (`JsRef`) in both lanes; typed coercions
  (`as`-reinterpretation between `JsRef`/`i64`/`u64`, narrowing for i8/i16
  stores) happen at assignment/call/store/return boundaries — C's implicit
  integer conversions hid this class entirely in #38.
- Imports bind as `extern "C"` declarations (`js_imp_<module>_<name>`),
  exports as `#[no_mangle] pub extern "C"` wrappers with boundary resets.
- Multi-result calls fail closed; lane-inconsistent IR fails closed.

## Tests

- `tests/low_rust_runner.rs`: corpus cases × both lanes → `rustc` → run →
  bit-exact compare. rustc costs minutes per artifact (the corpus embeds
  the full primordial library), so the default sweep is a six-case diverse
  subset; `JSAW_RUST_RUNNER=full` runs the whole corpus in parallel
  (concurrency capped at 8; ~7 GB RSS per rustc). Artifact bytes recorded
  via `JSAW_RUST_ARTIFACT_REPORT=<path>` (jsaw.rust-artifact-report.v1).
