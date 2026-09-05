# Plan: test262 compatibility tester, wired into CI

Goal: measure `portal-jsc-waffle`'s conformance against the ECMAScript test262
suite, with a runner that compiles test262 tests through the existing
swc → jsaw-core → waffle pipeline, executes the emitted WasmGC, classifies
results (including negative tests), and enforces a baseline so CI fails on
new regressions and passes on improvements.

---

## Current state

- `crates/portal-jsc-waffle` compiles script/module sources to WasmGC and
  `tests/e2e.rs` already has the two execution backends we need:
  - `execute_in_wasmtime` — in-process Wasmtime with `wasm_gc` +
    `wasm_function_references` enabled (dev-dependency, default-features off,
    features `cranelift, gc-null, runtime`).
  - `execute_in_node` — shells out to Node with a temp `.wasm` fixture.
- Both paths are currently numeric-only (f64 exports, f64 args). test262 needs
  richer interaction: observing globals, `$262` hooks, string/boolean results,
  and error behavior.
- `TESTING.md` designates `harness/` for test tooling; `vitest` covers JS-side
  tests; `cargo test` covers Rust. There is no CI configuration in the repo
  yet (`jsaw-test262` is a worktree of `portal-co/jsaw`; workflows added here
  apply to the shared repo — coordinate before pushing).
- The workspace members list in the root `Cargo.toml` is explicit; a new
  runner crate must be added there.
- test262 tests are plain `.js` files with a YAML frontmatter block
  (`/*--- ... ---*/`) describing flags (`module`, `onlyStrict`, `noStrict`,
  `raw`, `non-deterministic`), negative expectations
  (`negative: phase: parse|resolution|runtime, type: SomeError`), and
  `includes:` of harness support files. `harness.js` itself defines the
  `$262` host object contract (`createRealm`, `detachArrayBuffer`,
  `evalScript`, `gc`, `global`, `agent`).

## Design decisions (up front)

1. **Runner is a Rust crate**, `crates/test262-runner`, reusing the same
   lowering entry points `tests/e2e.rs` uses (`CfgModule → TModule →
   SModule/SFunc → portal_jsc_waffle::convert`). A Rust crate keeps one
   language for pipeline + report + baseline diffing, and wasmtime is already
   a dev-dep pattern here.
2. **Two-tier execution**: in-process Wasmtime as the primary engine
   (fast, hermetic, no process churn), Node as an optional `--engine node`
   cross-check so engine-specific bugs don't masquerade as conformance bugs.
3. **`$262` is implemented as an exported Wasm object** built by the compiler
   from a small built-in shim module (not injected from the host), so the
   compiled code observes it like any other global. Host-only capabilities
   (reading a temp file for `evalScript`, resolving realm exports) are
   implemented as *imported* Wasm functions the runner provides.
4. **Baseline-as-data**: expected failures live in a checked-in JSON manifest
   (`harness/test262/baseline.json`), keyed by test path + variant. CI diffing
   means: any test newly failing that isn't in the baseline fails the build;
   any baseline entry that now passes must be removed (CI fails until the
   baseline is pruned, keeping it honest in both directions).
5. **Subset-first rollout**: start with the test262 suites corresponding to
   implemented features (`built-ins/TypedArray*`, `built-ins/Math`,
   `built-ins/Array`, `language/expressions/*`, `language/statements/*`
   slices we already support), not the whole suite.

---

## Phase 1 — compilation adaptations for test262 tests

Work items (all in `crates/portal-jsc-waffle`, plus a thin layer in the
runner):

1. **Frontmatter stripping / metadata.** test262 test files embed YAML inside
   a leading `/*--- ... ---*/` comment. Extract and parse it in the runner
   (`serde_yaml` + a tolerant scanner for `includes:`/`flags:`/`negative:`
   shapes), and feed the *remaining* source to the existing parser path.
   Reuse the script-vs-module selection from e2e's `lower_module` /
   `script_ssa` split based on the `module` flag.
2. **Harness includes.** Prepend `harness.js` and each `includes:` file's
   contents ahead of the test body and compile as one script (test262's own
   browser/node runners do the same concatenation). Runner owns locating
   `harness/` inside the test262 checkout; attribution (BSD license text of
   test262) must be preserved when harness files are stored/cached — do *not*
   vendor harness files into this repo, always read from the test262 clone.
3. **Strict-mode variants.** Expand one test file into multiple *variants*:
   default, `"use strict"`-prefixed (`onlyStrict`), sloppy forced
   (`noStrict`), and raw. Each variant is a separate compile+run unit with a
   variant-suffixed baseline key.
4. **Richer result observation.** Today's exports are numeric-only. For the
   runner we need the compiled module to additionally export:
   - a mutable global or getter for each declared top-level binding the test
     expects the runner to inspect (test262 harness helpers such as
     `assert.sameValue` live *inside* the compiled code, so most assertions
     self-report via thrown errors — observation needs are limited);
   - a way to surface a thrown error's *constructor name / message* to the
     host (negative `runtime`-phase tests need to verify the error *type*).
     Cheapest correct approach: catch at the exported entry wrapper inside
     the module, store a tag string into an exported global, return an
     `i32` status code (0 = clean, 1 = threw).
   - `createRealm`, `detachArrayBuffer`, `gc`, `agent` shims:
     - `gc` → no-op (wasmtime GC is exact; document as acceptable for
       leak-sensitive tests only via flag `--gc-required` exclusion).
     - `detachArrayBuffer` → imported host function taking the buffer
       object ref; the compiled shim walks the object layout and flips the
       detached bit (extends the typed-array work in `typed_arrays.rs` /
       `repr.rs`).
     - `createRealm` → import that asks the runner to compile the
       `realm.js`-style source into a *fresh* wasmtime `Instance` and return
       an opaque realm id; subsequent `$262.global` style access routes
       through a registry. Phase 4 item — stub with a `Test262Error` throw
       ("createRealm unsupported") initially and exclude dependent tests.
     - `agent` → stub-throw initially (multi-agent tests excluded).
5. **Timeouts.** Wrap each execution in a bounded budget (wasmtime fuel or
   epoch interruption — prefer epoch, it doesn't perturb semantics) so
   infinite loops in a bad test can't hang the runner.

## Phase 2 — runner crate

New crate `crates/test262-runner` (added to workspace `members`), a CLI:

1. **Inputs**: `--test262-root <dir>` (a checkout of
   tc39/test262), `--suite <glob>` (repeatable, e.g.
   `built-ins/TypedArray/from`), `--engine wasmtime|node`,
   `--update-baseline`, `--jobs N`, `--report <file>`.
2. **Per test**:
   - parse frontmatter; skip suites by flags we can't honor yet
     (`raw`+`module` combos, `non-deterministic`, `async` until Phase 4);
   - assemble variant sources; compile (failure = parse/resolution-phase
     negative pass if `negative.phase` matches, else failure);
   - execute with timeout; a thrown error matching `negative.type`
     (`Test262Error` from the harness = assertion failure) is classified;
   - record `{ path, variant, status: pass|fail|skip|compile-error, detail }`.
3. **Report + baseline.**
   - `--report` writes JSON plus a human-readable summary (counts per suite,
     worst offenders).
   - Baseline diff mode (used by CI): load `harness/test262/baseline.json`,
     compare, exit non-zero on new failures or stale baseline entries;
     `--update-baseline` rewrites the file with an explanatory header.
4. **Caching.** Key compile cache on (source hash, backend revision) to keep
   re-runs fast locally; runner may persist to `target/test262-cache/`
   (gitignored).

## Phase 3 — CI workflow

Add `.github/workflows/test262.yml` (GitHub Actions; the repo's remote is
`portal-co/jsaw`):

1. **Triggers**: `pull_request` (paths: `crates/portal-jsc-waffle/**`,
   `crates/test262-runner/**`, `harness/test262/**`) and `push` on the main
   branch. Concurrency group to cancel superseded runs.
2. **Steps**:
   - checkout; install stable Rust with `Swatinem/rust-cache` scoped to the
     two crates (wasmtime/cranelift compile time is the dominant cost);
   - clone test262 at a *pinned commit* stored in
     `harness/test262/test262-rev` (bump explicitly; avoids suite drift
     breaking baselines), with an action cache keyed on that rev;
   - install Node 22 (already required by `execute_in_node`);
   - `cargo run -p test262-runner -- --test262-root ... --suite ...`
     (subset list from Phase 2 scope) in baseline-diff mode;
   - upload JSON report as a workflow artifact; emit a job summary table
     (pass/fail/skip counts per suite, delta vs baseline).
3. **Nightly full-ish run** as a separate scheduled (cron) job running the
   broader suite with `continue-on-error`, writing the report artifact only —
   used to discover newly feasible suites without gating PRs.
4. **Baseline updates** are PRs touching only
   `harness/test262/baseline.json`, reviewed explicitly (each entry = a known
   compiler limitation, ideally referencing a tracking issue in the message).

## Phase 4 — expansion (follow-ups)

- `async` flag support (needs the return-kind continuation work from
  `plan-function-provenance-and-return-continuations.md` plus an event-loop
  shim: `$262` drain-microtasks hook implemented as an exported function the
  runner pumps).
- `createRealm`/`agent` support (realm registry as in Phase 1.4).
- Module-graph (`import`/`export`) tests once module linking exists.
- Node cross-engine run in CI (nightly job) to separate waffle-emission bugs
  from wasmtime-behavior surprises.

## Risks / open questions

- **Compilation coverage**: most failures will be compile-phase (unsupported
  syntax/semantics). The runner must classify these distinctly from runtime
  failures so baselines stay meaningful when coverage grows.
- **Error identity across the boundary**: mapping a wasm trap/catch to a JS
  error constructor name requires the Phase 1.4 tag-string scheme; until then
  negative `runtime` tests report as skipped, not failed.
- **Strict/sloppy variant doubling**: runtime cost is ~2× per enabled suite;
  the suite globs and CI subset list keep this bounded.
- **test262 pinning drift**: harness `includes` change upstream; pinning the
  rev (Phase 3.2) is mandatory, not optional.
- **WasmGC wasmtime version**: e2e tests pin wasmtime 47 with GC support;
  CI must use a runner image where that engine builds and runs (it is
  self-contained via cranelift, so no extra system deps expected).
- **Workspace/pipeline coupling**: the root `Cargo.toml` currently patches
  `jsaw-core` and `waffle-` to local checkouts (`../jsaw-core`, `../waffle-`).
  CI checkout must clone those sibling repos at compatible revs, or the patch
  sections must be dropped/conditioned for CI. Resolve before Phase 3;
  likely a `ci` feature or a workspace split.

## Milestone order

1. Frontmatter parsing + variant expansion + single-test CLI (one known-pass
   test, e.g. `built-ins/Math/sqrt`-adjacent) — the "spike".
2. `$262` shim + thrown-error reporting + timeouts.
3. Baseline manifest + suite runs + report.
4. CI workflow (subset, gated) + pinned test262 rev + nightly broad run.
5. Remove skips as coverage grows; prune baseline each time.
