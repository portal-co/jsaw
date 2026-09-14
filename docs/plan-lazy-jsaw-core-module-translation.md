# Lazy jsaw-core module translation

**Status:** in progress.  This plan makes generated JavaScript modules—especially
Milestone 19's ~167 MiB `compiler.js`—practical to ingest without retaining the
same program as a SWC AST, CFG, TAC, and SSA graph simultaneously. `jsaw-core`
has several downstream users, so the change is additive, preserves the existing
whole-module interface, and is rolled out behind an explicit ingestion choice.

## Problem

The ordinary pipeline is deliberately clear but memory-hungry:

```text
SWC Module -> CfgModule -> TModule -> SModule -> portal-jsc-waffle
```

For a whole module, parsing retains all AST nodes. `CfgModule` retains every
hoisted function's CFG, then `TModule` and `SModule` each construct another
module-wide function map before prior representations can be released. Mechanical
output from `blitz-js` has thousands of functions and very large expression
trees; this overlap was enough for the M19 attempt to exhaust host memory.

The desired property is **bounded transient representation**: at any time, the
translator retains the final SSA module, the module top-level body, and at most
one currently parsed hoisted function's AST/CFG/TAC chain. The final SSA is
necessarily retained because existing linking and lowering inspect it across the
closed module set.

## Interface and seam

`portal-jsc-swc-ssa::module::SModuleBuilder` is the deep module that owns the
incremental translation policy:

```rust
let mut builder = SModuleBuilder::new();
for item in parsed_module_items {
    builder.append(item)?;
}
let module: SModule = builder.finish()?;
```

Its interface has only `new`, `append`, and `finish`:

- `append` accepts one parsed `ModuleItem`, lets the CFG builder classify its
  imports/exports/top-level statements, recognizes both exported and ordinary
  module-scope `function` declarations as hoisted functions, drains them, and
  immediately lowers each through TAC to SSA.
- `finish` lowers the remaining top-level body and reconstructs the ordinary
  module-init stores which install hoisted functions in the shared context.
- The resulting `SModule` has the existing imports, exports, functions, and
  body representation. `ModuleSet`, linker, and WasmGC lowering callers do not
  need a second linking interface.

`CfgModuleBuilder::take_functions` is an intentionally narrow internal seam:
it transfers just-completed CFG functions out of the CFG builder while retaining
metadata and top-level statements. It avoids widening the public `CfgModule`
representation or forcing every dependent to become incremental.

At the portal-waffle edge, the policy is explicit:

- `module_set_from_sources` remains the stable whole-module API for ordinary
  inputs and diagnostics.
- `module_set_from_sources_lazy` parses one SWC item at a time and delegates to
  `SModuleBuilder`. Its return type and conversion behavior are identical.

This is deliberately not a global behavior change: downstream projects can
adopt the lazy path independently, measure it, and retain the familiar path
until they do.

## Compatibility rules

1. **The final `SModule` is the compatibility contract.** Imports, exports,
   resolved SWC `Id`s (including `SyntaxContext`), hoisted function names, and
   top-level initialization semantics must match the whole-module pipeline.
2. **No textual identifier shortcuts.** All compiler bookkeeping continues to
   use SWC `Id`/`Ident`; module specifiers and export names are the only textual
   domains.
3. **No eager fallback inside the lazy path.** A lazy caller must not first
   build an entire `swc_ecma_ast::Module` merely to select the incremental
   route.
4. **Parsing diagnostics remain fail-closed.** Recovered parser errors are
   collected after item parsing and reject the module just as the existing
   whole-module helper does.
5. **Existing `TryFrom<Module>` / `TryFrom<CfgModule>` /
   `TryFrom<TModule>` conversions remain supported.** This avoids a flag-day
   migration across jsaw-core's test harness, generator, simplifier, and
   external dependents.

## Rollout

### Phase 1 — incremental hoisted-function lowering (implemented)

- Add `CfgModuleBuilder::take_functions`.
- Add `SModuleBuilder`, which lowers drained functions immediately.
- Add `parse_module_source_lazy` and `module_set_from_sources_lazy` in
  `portal-jsc-waffle`.
- Keep `parse_module_source` and `module_set_from_sources` unchanged.
- Verify metadata parity and cross-module exported-function execution on all
  existing runtime backends.

This removes the largest avoidable overlap for the current `blitz-js` output:
thousands of function AST/CFG/TAC copies need not coexist.

**Retry observation:** the real 164,687,048-byte Stage-B artifact was rebuilt
and the M19 lazy test was run with a 256 MiB dedicated thread stack. It did not
finish within bounded 20-, 10-, and 5-minute runs, but it did not reproduce the
prior host OOM. Progress logging shows it remains in **lazy ingestion**, before
Stage C begins. The integration test is therefore explicitly ignored by default
until Phase 3 identifies and fixes the remaining cost; it is still runnable on
demand with `--ignored` and fails closed if its Stage-B artifact is absent.

### Phase 2 — make M19 use the seam (next)

- Generate Stage B `compiler.js` from the real wasip1 compiler.
- In the M19 integration test, ingest `compiler.js` with
  `module_set_from_sources_lazy`; ingest the small WASI glue through the same
  method for a uniform closed-module set.
- Record elapsed time and peak resident memory around parse/lower separately.
- Keep the test's stack size explicit and bounded. A stack overflow is reported
  as a compiler limitation, never hidden by unbounded process resources.

**Exit:** Stage B -> lazy Stage C lowers the real compiler without host OOM.
The Java emission/JVM execution gates remain separate: this plan addresses
translation retention, not Java's 64 KiB method limit.

### Phase 3 — profile residual retention

If M19 still exceeds the memory budget, measure retained allocations by phase:

1. SWC source and parser allocations,
2. final SSA function graphs,
3. portal-waffle shape/return analysis maps,
4. WasmGC module construction and generated native adapters.

Only introduce another seam where measurements identify a second representation
that can be released. Likely candidates are lazy portal-waffle function lowering
and adapter-body caching; neither should be coupled to this first jsaw-core
interface change.

### Phase 4 — dependent adoption

Publish the interface and migrate consumers one at a time:

| Dependent class | Default action |
| --- | --- |
| Existing jsaw-core unit tests and `swc-test-harness` | Keep whole-module conversion; add parity fixtures where useful. |
| `portal-jsc-waffle` / `jsaw-wasi-bin` | Use lazy ingestion for generated or manifest-sized sources. |
| Generator/simplifier tools | Opt in only when they parse sources item-by-item. |
| External crates | Continue using existing conversions; document `SModuleBuilder` as the bounded-memory option. |

A downstream migration is complete only after its eager and lazy paths agree on
exports, import resolution, and runtime behavior for its fixtures.

## Verification matrix

- Unit: lazy/eager metadata parity for imports, exports, default functions,
  hoisted functions, and top-level statements.
- E2E: a lazy-ingested, cross-module exported function executes correctly on
  Wasmtime, Node, JVM, and Swift.
- jsaw-core: existing CFG/TAC/SSA whole-module tests remain green.
- M19: real Stage-B compiler plus WASI glue lowers as a closed module set under
  the lazy path; report peak RSS and timing.
- Regression: run the ordinary whole-module portal-waffle suite to prove the
  additive interface did not alter existing callers.

## Non-goals

- Streaming the final `SModule` directly into portal-waffle. That would require
a new linker/lowering interface and should be justified by Phase 3 data.
- Changing ESM evaluation/linking semantics.
- Relaxing source parsing errors to continue after malformed generated code.
- Solving JVM method-size splitting or full M19 execution; those have separate
plans and failure modes.
