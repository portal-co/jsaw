# Plan: C backend over the low_emit skeleton (DreamComp #38)

**Status:** implemented (this commit).

`BackendPlan` → C99 source for both lanes, via the #36 skeleton
(`low_emit::emit_low`) plus a target renderer. No linear-memory emulation:
CoreGC refs are native pointers, nano refs are arena byte offsets into a
runtime-owned buffer.

## Files

- `src/low_emit/c_render.rs` — `LeModule` → C99 text (`render_c_file`),
  plus `emit_c(plan, rep) -> Result<String, Vec<LeDiag>>` producing the
  complete translation unit (prelude + lane runtime + module glue).
- `src/low_emit/c_runtime.rs` — the hand-written per-target runtime support
  the #36 design mandates: a shared prelude (types, traps, bit/reinterpret
  and guarded-arithmetic helpers), the nano runtime (arena buffer +
  `js_nano_arena_ensure` + alias-safe `js_ld_*`/`js_st_*` memcpy accessors),
  and the CoreGC runtime (shadow root stack, precise mark/sweep collector
  driven by module-emitted layout info, `js_coregc_alloc`/`array_copy`).

## Rendering decisions

- `LeType::Ref` renders as `js_ref` (`uintptr_t`) in both lanes: nano = arena
  byte offset, CoreGC = object pointer; i31 tagging and null-0 are shared.
- CoreGC layouts become real C structs (`js_hd` header + fields, C99
  flexible array member for arrays). The renderer also emits
  `js_layout_for(type_id)` — `{size, is_array, elem_size, elem_is_ref,
  ref_offsets…}` via `offsetof` — which the fixed collector runtime consumes.
- All i32/i64 arithmetic renders on unsigned types and casts back (C signed
  overflow is UB; wasm wraps). Division/remainder go through helpers that
  trap deterministically on division by zero (and `INT_MIN / -1` for div).
  Shifts mask the count. Trapping float→int conversions appear only behind
  skeleton guard helpers, so native casts are in-range by construction;
  `TruncSat` renders as explicit saturating helpers.
- Float constants and reinterpret ops are bit-exact (`memcpy` helpers); no
  decimal float formatting anywhere. F32 arithmetic uses out-of-line helpers
  so the ABI forces single precision regardless of `FLT_EVAL_METHOD`.
- CFG blocks render as C labels + `goto`; all locals are declared
  zero-initialized at function entry, so arbitrary edge shapes and traps are
  total. `Trap`/`Unreachable` call `js_trap()` (prints and `abort()`s —
  never UB).
- Function tables render as `js_fnptr_t` arrays (index 0 = trap stub);
  indirect calls cast back at the call site. Casting between function
  pointer types is the one documented extension beyond strict C99
  (universal on every hosted target; POSIX `dlsym` precedent).
- Imports render as `extern` prototypes named `js_imp_<module>_<name>`
  (sanitized); the host links those symbols. Exports render as
  `js_export_<name>` wrappers that reset the CoreGC root stack at the
  boundary (trap-safety contract) around the call.
- Multi-result calls fail closed with a named diagnostic (none exist in the
  current corpus ABI).
- Skeleton fix in passing: import functions now occupy their `LeFuncId` with
  a placeholder body so `module.funcs` stays dense-as-documented (previously
  `debug::validate`'s id==index assumption was unsound under imports).

## Skeleton/representation bugs found by the first semantic runs

The #36 smoke tests were structural only; compiling and *executing* corpus
cases surfaced four latent defects, all fixed here:

1. `skeleton.rs emit_inst` keyed call result arity off `inst.results`, but
   waffle gives even void-producing operators a placeholder `Value` — a call
   to a void callee recorded one bogus result. Now keyed off
   `inst.result_types`.
2. `TypeIdTable::build` was single-pass, so an array signature appearing
   before later struct signatures got a type id a later struct reused.
   Now two passes (structs 1..=S, arrays S+1..=S+A).
3. `nano.rs array_new` computed `PAYLOAD_OFFSET + (i as u32) << elem_log2`
   — `+` binds tighter than `<<`, so element 0 stored at offset 64 instead
   of 8. Parenthesized.
4. Funcref-typed values (bare `funcref` *and* typed `(ref $funcsig)`) flowed
   as `LeType::Ref`, so the CoreGC shadow stack rooted them and the collector
   scanned table-slot indices as pointers (crash), and `array_get` typed its
   raw temp `I32` unconditionally, truncating ref elements in the C renderer
   (invisible in wasm where a ref local is a ref). Funcref words now lower
   to `I64` in both lanes (`plan_heap_is_func_word`), `array_get` types its
   temp from the element type, and packed `get_u` zero-extends (`I32Extend8U/
   16U` arms added — coregc previously had none).

## Tests

- `tests/low_c_runner.rs`: every non-`~Natives_` corpus family × both lanes
  → emit C → compile with `$CC`/`cc -std=c99 -O1 -lm` → run a generated
  driver calling the numeric export bit-exactly → compare against the
  expected value (same rule as `corpus_parity`). Also asserts deterministic
  re-emission at the C text level. Optional `JSAW_C_ARTIFACT_REPORT=<path>`
  writes per-case/lane artifact bytes (acceptance #3). Fails closed with a
  prerequisite error when no C compiler exists.
