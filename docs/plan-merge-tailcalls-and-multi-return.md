# Plan: merge jsaw-extra-branch, then tail calls, then tagged-union multi-return functions

Three sequential milestones on `feat/esm-import`:

1. **Merge** the `jsaw-extra-branch` worktree branch (multi-module ESM
   ingestion) into this branch.
2. **Tail calls**: correct and tested `TTerm::Tail` lowering.
3. **Tagged-union multi-return functions**: cores whose return statements
   produce *multiple* value representations (`f64` / `i32` / boxed
   `anyref`) return a tagged union; every caller propagates the tag up its
   own return path so the topmost caller branches on the tag into a
   per-case continuation.

Milestone 3 builds directly on the implemented `ReturnKinds` /
continuation-split machinery (`docs/plan-function-provenance-and-return-continuations.md`,
all four commits landed) and on whatever the merge brings in (the
multi-module linker's import-linking reuses `FunctionRef` direct dispatch,
so merge ordering matters).

---

## Milestone 1 — merge `jsaw-extra-branch`

### 1.0 Repository layout

- This checkout: `/Users/g/Code-local/portal-hot/jsaw`, branch
  `feat/esm-import` at `530372d` (3 commits ahead of `2c8a939`:
  `0ffb4a4`, `62e6c2e`, `530372d`).
- Other checkout: `../jsaw-extra-branch`, branch `master` at `7d26381`
  ("[AI] Multi-module ESM ingestion with relative-path import linking"),
  whose parent is `2c8a939` — an ancestor of our HEAD. Merge-base is
  `2c8a939`; the branch carries exactly one commit on top of it.
- The two worktrees share one repository; from here the branch is
  reachable directly (e.g. `git merge 7d26381` or by branch name).

### 1.1 What the commit touches

- `crates/portal-jsc-waffle/src/linker.rs` — **new file** (~440 lines):
  `ModuleSet`, lexical relative-path import resolution, export resolution
  through re-export chains, per-module import tables, main-module
  export-surface flattening (`Local`, `DefaultFunc`, `Reexport`,
  `export * from`), `convert_modules(entry, set, ...)`.
- `crates/portal-jsc-waffle/src/lib.rs` — module declaration wiring.
- `crates/portal-jsc-waffle/src/conv.rs` — `convert_module` becomes a
  single-module wrapper over `convert_modules`; `LoadId` import-linking arm
  mints the target's function object directly so cross-module calls reuse
  the guarded `FunctionRef` direct-dispatch path.
- `crates/portal-jsc-waffle/tests/e2e.rs` — +262 lines of e2e tests.
- `docs/plan-multi-module-ingestion.md` — the plan behind it (items 1–5
  done; item 6 = open follow-up).

### 1.2 Conflict surface

Our 3 commits since the merge-base touch `conv.rs` (+570/-55 in the
diff range), `primordials.rs`, `Cargo.toml`, `Cargo.lock`, and
`tests/e2e.rs`. Overlap risk is concentrated in:

- `conv.rs`: our `LoadId`-adjacent provenance changes vs. their new
  import-linking arm in the same `lower_statement` region.
- `tests/e2e.rs`: both sides appended tests at the end — likely a trivial
  adjacency conflict; keep both blocks.
- `Cargo.lock`: regenerate after resolution (`cargo update -w --offline`
  or just accept either side and rebuild).

### 1.3 Steps

1. Pre-flight: commit or stash local dirt on `feat/esm-import`
   (`Cargo.lock` is currently modified). Confirm
   `git merge-base HEAD 7d26381` = `2c8a939`.
2. `git merge 7d26381 -m "[AI] Merge multi-module ESM ingestion from jsaw-extra-branch"`.
3. Resolve per §1.2. Semantic check, not just textual: after resolving
   `conv.rs`, the import-linking arm must sit *after* our provenance
   tagging logic or explicitly bypass it (a linked import is
   `FunctionRef { origin }` by construction — it should not be
   re-looked-up from the context).
4. `cargo build -p portal-jsc-waffle` and run the e2e suite
   (`cargo test -p portal-jsc-waffle` / the vitest harness per TESTING.md).
   Baseline: 43 passing + 2 pre-existing utf16 string failures; the merge
   adds the branch's new tests, all expected green.
5. Sanity-check the merged surface: a two-module fixture (entry imports a
   function from `./dep.js`) still exercises the guarded direct-dispatch
   path in the merged tree (the branch ships this as an e2e test; keep it).
6. Record the open follow-up (plan item 6 of
   `plan-multi-module-ingestion.md`) in `goals.md` if it isn't already.

---

## Milestone 2 — support and test tail calls

### 2.0 Current shape

- `TTerm::Tail { callee, args }` lowers via `lower_call_parts` per
  continuation and unconditionally emits
  `Terminator::ReturnCallRef { sig: self.repr.adapter }` with
  `(context, receiver, arguments, code)` (`conv.rs` ~line 1457).
- `scan_return_kinds` treats every `Tail` block as returning
  `Reference` (~line 721) — sound today because the adapter's ABI is
  boxed.
- No *validity* analysis exists: nothing checks that the tail call's
  callee/args are actually in tail position (no `TTerm::Default`
  fallthrough, no live temporaries needing epilogue work, no `try`/loop
  constraints). Tail calls currently "work" only where the generic
  adapter happens to be semantically adequate.

### 2.1 What "support" means here

Tail calls must be *correct*, not merely emitted:

1. **Position validity.** A `TTerm::Tail` is a real JS tail position only
   when the enclosing function has no enclosing `try` block (no
   stack-frame-stealing observer) and no pending work the epilogue must do
   (no live `finally`, no return-kind conversion at the boundary that the
   callee can't perform). Add a per-function flag computed in the same
   pre-pass family: `tail_call_ok: bool` (false if any `Tail` block is
   inside a `try` region; conservative false when in doubt).
2. **ABI honesty.** `ReturnCallRef` through `repr.adapter` requires the
   *caller's* native return type to equal the adapter's boxed return.
   Today that holds because everything returns `anyref`. Once Milestone 3
   introduces raw-return natives, a tail call out of an `f64`-returning
   function through the boxed adapter is a type error. This milestone
   keeps the invariant explicitly: tail-call-eligible functions must
   return `Reference` (assert in lowering; downgrade `TTerm::Tail` to
   `Call` + `Return` when the analysis says otherwise), so Milestone 3 can
   extend instead of unwinding.
3. **Milestone-3 interlock (state now, build later).** A tail call that
   jumps through the boxed adapter *reboxes* the callee's return — for
   raw-return callees the adapter boundary is where unboxing/refinement
   would be lost. Milestone 3's tagged-union return ABI is designed so a
   tail call forwards the tagged union *as-is* (§3.4); nothing in this
   milestone may bake in "tail = boxed adapter" harder than one match arm.

### 2.2 Implementation steps

1. Add the `tail_call_ok` pre-pass (conservative; `try` region detection
   via the existing control-flow scan used by the return-kind pass).
2. Guard `TTerm::Tail` lowering: if `!tail_call_ok`, lower as
   `lower_call_parts` + `Terminator::Return { box_value(result) }`
   (same blocks, non-tail terminator).
3. Assert the boxed-return invariant (2.1.2) at `ensure_function` time.
4. Keep `scan_return_kinds`' `Tail → Reference` rule; add a comment
   pointing at Milestone 3 (the tagged union makes this rule *more* true,
   not less).

### 2.3 Tests

- **Direct tail recursion**: `function f(n) { return n <= 0 ? 0 :
  f(n - 1); }` deep (e.g. 100k) — must not overflow where a non-tail
  version would (this is the observable payoff; if the WasmGC backend's
  host stack still grows, the test pins current behavior with an explicit
  comment and a shallower bound rather than failing).
- **Mutual tail recursion**: even/odd pair.
- **Tail through a method / `FunctionRef` local**: guarded dispatch path
  feeding `TTerm::Tail` — verify the tag-check fallback arm does *not*
  tail-call (it can't; the fallback re-enters generic dispatch), i.e. the
  tail emission only happens on the fast arm or via the generic adapter.
- **Non-tail downgrade**: `function f(n) { try { return g(n); } finally
  {} }` style (adapted to the backend's supported syntax) → inspect: no
  `ReturnCallRef` in the parent body; plain `Call` + `Return`.
- **Tail into the JS runtime surface**: `return arr.map(f)` last-call
  forms where the callee is a primordial core — must fall back to the
  boxed call + return, not `ReturnCall` into a core with a different ABI.

---

## Milestone 3 — tagged-union multi-return functions

### 3.0 Problem

Post Part B, a function with a *single* return kind gets a raw native ABI
(`-> f64` / `-> i32`), and provable call sites consume the raw value with
zero splits. A function with *mixed* return kinds (`return a ? 1.5 : {};`)
still uses the boxed ABI, so every caller pays box + re-coerce, and fast
callers can't stay raw across the call.

Goal: mixed-kind cores return a small **tagged union** (tag + payload, one
word per slot, all unboxed), callers *propagate the union up through their
own returns* when they too are multi-kind, and the topmost caller branches
on the tag into a **per-case continuation** — the existing continuation
machinery, now driven by a runtime tag instead of a static kind set.

### 3.1 Representation

```rust
/// One concrete representation a multi-kind core may return.
/// Derived from the ReturnKinds set; tag order is a fixed global
/// total order (Reference < BooleanOrInteger < Number) for stable tags.
```

- Wasm: a `waffle` struct type `$multi_ret { tag: i32, r: anyref, i: i32,
  f: f64 }` (payload slots always present; only the tagged slot is valid).
  Simple, matches, and `ref.test`-friendly; a packed variant-per-tag
  family is an optimization, not a requirement. Mint lazily in `repr`,
  cached alongside the adapter/unboxed-adapter maps in `FunctionInfo`.
- Tag constants: `0 = Reference`, `1 = BooleanOrInteger`, `2 = Number` —
  the same coarse `ValueKind` enum from the return-kind analysis, no
  refinement (per the non-goals of the provenance plan).

### 3.2 Callee side: `make_multi_return_core`

When a user function's `return_kinds` has ≥ 2 members (and the function is
not a tail-call-eligible boxed returner per §2.1.2 — multi-return cores
have no boxed return at all):

- Native signature stays `(context, this, arguments, T...) ->
  $multi_ret` — one extra struct allocation at each return.
- `TTerm::Return` lowering, per block: classify the returned value's kind
  (already available — the analysis runs per-block kind refinement), then
  emit `StructNew $multi_ret` with the tag for that kind and the value in
  its slot. Fast cores (Math tags etc.) never produce multi-ret; they
  remain singleton-kind.
- No adapter changes: `make_adapter` (boxed universal fallback) and
  `make_unboxed_adapter` (singleton raw) stay exactly as they are.

### 3.3 Caller side: propagate or split

At a call site whose callee's `return_kinds` is multi:

- **Callers that are themselves multi-kind cores**: no split. The call
  result (`$multi_ret` value) flows straight into the caller's own
  `StructNew` at return — forwarding is one `local.get`-shaped move. This
  is the "propagate up to callers" half: multi-ret results stay packed
  through arbitrarily deep chains of multi-kind frames.
- **Callers returning a single kind**: at the caller's return, the union
  is refined once (see below) and the single kind is extracted; or the
  caller emits `StructGet` per known-tag assumption guarded by the
  analysis. Prefer the simplest sound form: refine-then-extract at the
  return.
- **Ordinary callers (the top of the chain, e.g. a Wasm export or a
  generic frame)**: the call site becomes a **tag-branching
  continuation**, the multi-kind generalization of B4's static split:

  ```
  call → struct.get $multi_ret 0 (tag)
       → br_table / if-chain over the callee's ReturnKinds members
         ├── tag=Number  → continuation block, values[get 2] as raw f64
         ├── tag=B/I     → continuation block, values[get 1] as raw i32
         └── tag=Ref     → continuation block, values[get 0] as anyref
  ```

  Each arm lowers the remainder of the SSA block once, exactly like
  `guarded_primordial_call`'s fast/slow split and
  `resolve_property_read_paths`' ordinary/absent/getter split; `BlkSet`
  deduplicates arms whose lowered forms agree. The split must compose with
  the *existing* static continuation splits: if the call site already
  carries a primordial fast/slow split, each arm of that split
  independently decides "singleton (raw) vs multi (tag-branch)". Emission
  order: outer static split first, tag-branch nested inside each arm.

- **Provable singleton call sites are untouched** — the entire payoff
  structure of Part B remains; tagged unions only appear where the
  analysis genuinely cannot pick one kind.

### 3.4 Tail calls through multi-return cores

Per §2.1.3: a `TTerm::Tail` into a multi-ret callee in a caller whose
native ABI is also `-> $multi_ret` emits `ReturnCallRef` against the
multi-ret signature with the callee's returned union forwarded untouched —
no rebox, no refinement at the boundary. When the caller's ABI is boxed
(or raw singleton), downgrade to call + per-§3.3 propagation + `Return`,
since `ReturnCall` into the multi-ret signature from a different return
type is illegal. This is the only new interaction between Milestones 2
and 3, and it's one match arm in the `TTerm::Tail` lowering.

### 3.5 Implementation order

1. Mint `$multi_ret` struct type + tag constants in `repr.rs` (pure).
2. `make_multi_return_core` return-side emission (§3.2), gated to ≥2-kind
   functions. Single-kind behavior byte-identical (assert via e2e
   inspection).
3. Topmost-caller tag-branch continuations (§3.3 ordinary case) — the
   core milestone; reuse `Continuation`/`select_continuations_*` shape.
4. Multi-kind caller propagation (§3.3 first bullet) + tail-call arm
   (§3.4).
5. Interlock pass with Part A/B call-site matrix: extend the
   implementation-status matrix in
   `plan-function-provenance-and-return-continuations.md` with a
   multi-kind column rather than re-deriving dispatch.

### 3.6 Tests

- Mixed-kind core: `function pick(n) { return n > 0 ? n * 1.5 : null; }`
  consumed by arithmetic (Number arm), by strict-equality-with-null (Ref
  arm), and by a branch on truthiness (both arms) — inspect: caller body
  contains a tag branch; each continuation consumes the correct payload
  slot with no intervening box.
- Multi-kind chain: `middle()` tail-calls `pick()` and is itself
  multi-kind → inspect: `middle`'s body contains **no** tag branch and
  its return is `StructNew $multi_ret` forwarding the callee's result.
- Tail call through multi-ret (§3.4): deepest frame's tag is observed by
  the topmost caller's continuation, uncorrupted.
- Singleton regression: every existing e2e (43 + merged set) — no
  `$multi_ret` may appear in any module whose functions are all
  single-kind (global inspection assert).
- Guarded dispatch × multi-kind: a guarded `FunctionRef` call whose
  fast arm is multi-ret and fallback arm is generic-boxed — the nested
  split (static then tag) must produce correct values on both arms.
- `try`-adjacent multi-kind call: split interacts with the non-tail
  downgrade path from §2.2 (call lands inside a continuation after the
  tag branch; exceptional edge still routes to the catch).

---

## Non-goals

- Finer tags than the three coarse kinds (unchanged from the provenance
  plan's non-goals).
- Packed/partial union variants per tag (possible later; the fat struct
  is correct and small enough to start).
- Unboxed *parameter* ABIs (still a non-goal; arguments stay boxed/formal).
- Making the universal boxed adapter multi-ret-aware (it remains the
  generic fallback with the pinned table signature).
- Guaranteed O(1) host-stack tail calls if the WasmGC toolchain doesn't
  lower `return_call` as a frame replacement — we emit the correct
  terminator and document observed behavior in tests.

## Sequencing summary

1. Merge (`7d26381`) → green e2e including the branch's module tests.
2. Tail-call validity pass + guarded lowering + tests (self-contained).
3. Tagged-union multi-return: repr → callee emission → topmost split →
   propagation + tail arm → tests.
