# Plan: MiniBC backend over the low_emit skeleton (DreamComp #39)

**Status:** complete (jsaw `master`; DreamComp foundations `0ef8b50`/`08d5337`/`3682bbc` + the `AluUn` C-codegen fix). All four acceptance criteria met:

1. **Cross-interpreter**: every corpus case × both lanes runs under the stock Rust `MiniInterp` (`tests/low_minibc_runner.rs`, 88/88) and under the generated C interpreter header via a C host implementing the jsaw `0x9B` host-op family (`low_emit/minibc_c_host.rs`, `tests/low_minibc_c_interp.rs`, 88/88).
2. **Corpus parity** in both lanes (same runners).
3. **Artifact bytes** recorded per case/lane (`JSAW_MINIBC_ARTIFACT_REPORT`, schema `jsaw.minibc-artifact-report.v1`).
4. **Cross-compat**: `tests/low_minibc_crosscompat.rs` runs a jsaw artifact and a DreamComp-emitted program (round-tripped through the DreamComp `Container` format) in one `MiniInterp` instance.

Defects found during bring-up (all fixed):

- **jsaw emitter**: `If`/`Select` lowering overwrote the arm's *first* segment's exit instead of the arm-*end* segment's (clobbering nested `If` Conds and `Ret`/`Trap` exits); Table arm shims recorded the segment *before* the `FuncAddrLoad` (off-by-one through the whole table); the Table default fixup was 4 bytes late (clobbering arm 0's shim); the Table discriminant needed the raw funcref word converted to f64 (the stock `Table` ext-op reads its index as an f64 JS number); the CoreGC host keyed layouts by type-id where the IR passes layout *indices*, and field/index accesses needed per-size read/write (8-byte unconditional writes clobbered packed neighbours).
- **Allocation**: primordial-init-scale functions exceed the 64-run R4 file; the linear-scan allocator now spills to the stock spill frame (64 slots, jsaw-side scratch runs above the call window), restricted to locals never live across a call (the spill frame is shared across calls).
- **DreamComp c-interp**: `AluUn`'s C execution wrote to the stale shared `c` operand variable (wire dst is `b`) — caught only by cross-running jsaw artifacts, locked by a regression case; `mn_spill` moved to file scope (spills cross segment dispatches, exactly like `mn_call_stack`).
- **C host**: root frames stored pointers into a realloc'able buffer (dangling after growth → GC read garbage roots → divergent frees); now offset-based.

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
