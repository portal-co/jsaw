# Plan: DreamComp #25/#26/#27 staged chain (BackendPlan, specialization, nano native mode)

Worktree branch `ai/backend-plan-25-27`. One commit minimum per issue, `[AI]`-prefixed.
Do NOT edit `conv.rs`, `crates/jsaw-wasi-bin/*`, `../jsaw-core`, `../waffle-`, `../dreamcomp`.
All new logic in new modules + additive `pub mod` wiring in `portal-jsc-waffle/src/lib.rs`.

## Stage A (#25) — `crates/portal-jsc-waffle/src/backend_plan.rs`

Immutable, target-independent `BackendPlan` built from a Waffle `Module`:

- Canonical ids: `PlanFuncId` (traversal = `funcs.entries()` order), `PlanBlockId`
  (reverse-postorder from entry, unreachable blocks after in entity order),
  `PlanValueId` (block params then instruction order, per function),
  interned `TypeId`/`PlanSigId` tables (signatures deduped structurally via
  `BTreeMap`, so ids never leak Waffle arena allocation order).
- Flat block-parameter CFG: `PlanBlock { params, insts, terminator }`; terminators
  cover Br/CondBr/Select/Return/TailCall(direct|indirect|ref)/Unreachable/UB.
- `PlanInst`: stable operator name (`Debug` prefix), args, result arity/types,
  optional `PlanCall` (direct/indirect/ref), optional `PlanArenaOp` (alloc/read/
  write/grow/len classification for WasmGC ops).
- Per-value `NanoValueRepresentation` + `NanoLifetime` + `PlanLayout` records.
- `CompletionShape` (Return/Throw/Trap kinds + result arity), runtime
  requirements, import/export boundary records, structured diagnostics.
- `BackendPlan::from_module(&Module) -> Result<BackendPlan, Vec<BackendPlanDiagnostic>>`.
- Validation (fail-closed, aggregated diagnostics): block-param arity/types vs
  predecessor args; value ownership (`value_blocks[inst] == containing block`);
  dominance-based def-before-use; terminator validity; call shape (arity, table
  index arg, sig match, tail-call returns == func rets); boundary ABI
  (duplicate export names, import kind/entity consistency, start func shape).
- Deterministic JSON via serde (Vec/struct fields only, no maps) + SHA3-256
  fingerprint over domain-separated length-delimited canonical bytes,
  lowercase hex.
- Does NOT rewire `emit_coregc`; inspection/adapter seam only.

Tests: scalars, aggregates, funcrefs, direct/tail calls, multi-results,
returns, exceptions (trap completion), host/export boundaries, determinism
(plan twice / clone / permuted signature interning), malformed-input failures.

## Stage B (#26) — `crates/portal-jsc-waffle/src/specialize.rs`

- `SpecializationFacts`: primitive kind/range/bounded constants, nullability,
  object shape + alias/mutation class, this/context/arguments liveness,
  completion behavior, opaque arena refs, flattened fields, Waffle session ids,
  proven function identity.
- `SpecializationKey`: unused-parameter elision first; assumption digest
  (portal-jsc-common `AssumptionSet::digest()`) participates; deterministic
  serialization + SHA3-256 fingerprint.
- Bounded constant evaluator over `BackendPlan` blocks (64-step walk,
  transparent nodes, predecessor agreement at joins, nested call-site param
  envs, wrapping arithmetic only under explicit flag), fail-closed.
- `VariantBudget` (per-func, global) + deterministic coarsening to generic
  fallback with observable diagnostics/manifest records.
- CoreGC keeps facts only (`CoreGcFacts` adapter, no forced variants).

Tests: key determinism across insertion orders, unused params elided, budgets
coarsen, evaluator (nested envs, wrapping gate, cycles, join disagreement,
unsupported values), CoreGC facts-only, dynamic refs stay generic.

## Stage C (#27) — `crates/portal-jsc-waffle/src/nano_native.rs`

- `NanoNativeConfig`: opt-in `native_functions` + `full_inlining` flags;
  defaults off; CoreGC untouched.
- Fixed adapter ABI `(context, this, arguments) -> value` preserved as a
  declared record; source arity/spread preserved.
- `NanoCallDecision`: direct (proven identity only), tail (frame replacement
  only when return ABI/completion subsets match), guarded generic adapter
  (uncertain/rebound), or inline.
- Bounded inlining: depth/block/code-size budgets; recursion proof; required
  inline failure = fail-closed structured diagnostic; opportunistic failure =
  recorded fallback to framed/direct call.
- Emits an annotated `NanoPlan`-sidecar + validated `BackendPlan` after
  (param elision before specialization keys).
- Capability gating: target must support direct/tail/multi-result ops the
  plan uses (MiniBC-relevant ops representable, never rejected); unsupported
  combos are diagnostics, not invalid plans.

Tests: defaults unchanged, uncertain refs keep adapter, required-inline
fail-closed on recursion/budget, tail-call ABI gating, exceptions + object
writebacks preserved, capability gating reports unsupported targets.

## Validation after each stage

`cargo test -p portal-jsc-waffle --lib` and `cargo check --workspace`
(workspace `[patch]`es jsaw-core and waffle- to sibling checkouts).
