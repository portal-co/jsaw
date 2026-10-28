# Plan: MiniBC backend over the low_emit skeleton (DreamComp #39)

**Status:** in progress. Landed: the DreamComp format/interp foundations
(`AluRaw`/`AluUn` raw-word sub-op opcodes `0x74`/`0x75`, `Trap` `0x76`, the
`0x9B` stateful host-op hook, `MiniInterp`/`StdExtOp` moved into
`dreamcompiler-minibc` under the `interp` feature) and the jsaw emitter +
host runtime skeleton (`low_emit/minibc_render.rs`, `minibc_host.rs`,
`tests/low_minibc_smoke.rs`). The emitter compiles and runs artifacts under
the stock interpreter, but a miscompilation in the call/window protocol
remains (smoke test `#[ignore]`d) — see the issue for the current evidence
and next steps.

`BackendPlan` → MiniBC container via the #36 skeleton, with a jsaw-native
value model on the shared bytecode format (no NaN-box object model).

## Value model

Every `LeType` lives in an R4 (64-bit) register run: i32/f32 zero-extended,
i64/f64 raw bits, refs as the lane word (nano: arena byte offset; coregc:
heap handle), i31 as odd words. `Move` copies R4 runs verbatim; reinterprets
are free.

## Control flow

Stock `StdExtOp` (the `0x80–0x85` reserved-range set): `Jump`/`Cond`/`Table`
for block edges, `Call`/`TailCall` for calls with the interpreter's call-stack
return-address protocol, `Throw` unused (the plan surface has no exceptions).
The trampoline from the skeleton maps onto `Table` dispatch over step
segments.

## Opcode coverage matrix (from the #36 corpus operator inventory)

Covered by stock `MiniOp`: i32 add/sub/mul/and/or/xor/not/shifts/eq; i64
add/sub/mul/and/or/xor/not/shifts; raw-bit eq/ne (refs, i64); f64
add/sub/mul/div/rem/floor/trunc/min; width `Convert`.

Missing (gap families):
- i32/i64 division and remainder (signed + unsigned, with wasm trap
  semantics);
- i32/i64 ordered comparisons (signed + unsigned) producing **raw 0/1**
  (stock `Lt`/`Gt`/… produce NaN-boxed booleans — unusable for jsaw);
- f32 arithmetic/comparisons; f64 max; f64 ceil/sqrt/abs/neg/nearest;
  conversions: int↔float (incl. saturating + trapping), extends, i31
  get_s/get_u;
- memory: nano arena load/store; coregc alloc/field/element access;
- runtime fns and FFI imports.

Decision, refined after studying the interpreter surfaces:

- **Scalar gaps become two new `MiniBc` opcodes with sub-op tables** —
  `AluRaw { sub_op, a, b, dst }` and `AluUn { sub_op, src, dst }` in the
  reserved range (`0x74`/`0x75`), each with a documented sub-op enum under
  stable-tag discipline. Raw-word semantics (slots carry bit patterns, no
  f64 round-trips — the stock 32-bit ops are f64-mediated and unusable for
  jsaw's model). Two opcode tags keep the footprint compact while covering
  the ~40-op gap; both interpreters implement the sub-op tables natively
  (the Rust interp's run loop, and stock `COpcodeGen` impls for the C
  header generator), so scalar semantics never depend on the host.
- **Memory, runtime, and FFI gaps become a jsaw ext-op family** (`0xA0+`
  tags, after the `0xFF` segment exit): nano arena ops operate on the
  interpreter's byte space, coregc allocation/field ops on a jsaw-native
  handle heap, FFI imports dispatch to host callbacks. The interpreter
  gains a generic **stateful host-op hook** (handlers receive `&mut`
  access to the documented interp state surface — store, virtual_memory,
  heap) so jsaw ships its runtime as a host table without DreamComp-side
  per-op changes; jsaw ships matching `COpcodeGen` implementations so a
  generated C interpreter header runs jsaw artifacts unmodified
  (acceptance #1).

## Cross-compatibility (acceptance #4)

A load test: one container carries a jsaw artifact segment group and a
DreamComp artifact segment group; both parse and enter under one interpreter
instance. Semantic NaN-box interop stays #16 scope.

## Tests

- Local runner mirroring `low_c_runner.rs`: corpus cases × both lanes →
  container → `dreamcompiler-minibc-interp` → bit-exact compare; artifact
  bytes recorded (acceptance #3) for comparison against DreamComp's
  `minibc`/`cps_minibc` artifact-report records.
- C-interpreter execution via a generated header including the jsaw opcode
  and ext-op sets (acceptance #1).
