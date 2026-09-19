# Plan: move-oriented continuation environments

**Status:** implemented (2026-09-19).

## 1. Purpose

`Converter::lower_function` in
`crates/portal-jsc-waffle/src/conv.rs` lowers a source SSA block through a
set of Wasm control-flow continuations. A continuation currently owns:

```rust
struct Continuation {
    block: Block,
    values: BTreeMap<SValueId, LowerValue>,
}
```

For every source statement, the driver calls `lower_statement` for every
continuation, then clones that continuation's complete `values` map once for
every returned `(Block, LowerValue)` result before inserting the current SSA
value. This is correct: each split can refine the same source value to a
different `LowerValue` representation. It is catastrophically expensive for
a long straight-line block, however. A 299,633-value module-initializer body
can copy an approximately 300,000-entry `BTreeMap` approximately 300,000
times: roughly 45 billion map-entry copies before considering allocations and
`LowerValue` cloning.

This plan replaces that copy-on-every-statement protocol with a small,
private continuation-environment module. It has two complementary rules:

1. **Move a continuation when a statement has exactly one successor.** The
   common straight-line case mutates that continuation's private writable
   bindings in place. It neither clones nor allocates a historical
   environment.
2. **Freeze only at a real fan-out.** When a statement produces multiple
   continuations, move the shared history into an immutable sidecar owned by
   `Arc`; each child gets its own tiny writable overlay containing only the
   newly-produced source value. The children share all earlier
   `LowerValue`s through the sidecar rather than cloning the environment.

This is a lowering-internal redesign. It must preserve source semantics,
Waffle CFG shape, representation splitting, diagnostics, and the public
`convert`/`convert_module`/`IncrementalConverter` interfaces.

## 2. Current seam and constraints

### 2.1 Where environments are used

`lower_function` creates one initial continuation per source `BlkSet`, then
feeds every statement in `sfunc.cfg.blocks[sblock].stmts` through
`lower_statement`. A statement can return:

- **zero** paths (the path is consumed or rejected);
- **one** path (ordinary expressions, most loads/stores/calls, and a select
  whose arms can be joined as a `TypedSelect`); or
- **several** paths (raw/reference representation splits, guarded dispatch,
  accessor/property paths, object-rest paths, and multi-kind return
  unpacking).

The callee chain beginning at `lower_statement` is intentionally read-only
with respect to the environment. It only calls `values.get(&SValueId)`;
there is no required iteration, removal, mutation, or map-order observable
outside the `lower_function` driver. The affected helper parameters include
`lower_item`, template/object-literal lowering, call argument construction,
fast/tail dispatch, and property-path lowering.

Terminator lowering likewise only looks values up. Its ordinary
`LowerValue::clone()` calls are semantic value duplication at the Wasm IR
layer and are not the full-environment cloning problem.

### 2.2 Required invariants

The new module must make these facts explicit and test them:

1. A lookup returns the binding nearest to the continuation's writable
   overlay, then searches immutable sidecars from newest to oldest.
2. Every continuation produced for source statement `stmt` contains exactly
   the parent bindings plus its own `(stmt, produced_lower_value)` binding.
   Different children may bind `stmt` to different representations.
3. A zero-result statement leaves no continuation to lower later statements.
4. A one-result statement preserves the original continuation's environment
   identity and moves its `LowerValue` into that environment; it does not
   freeze or clone the old environment.
5. A fan-out never permits later mutation of bindings shared by sibling
   continuations.
6. Environment objects never cross source-function / `FunctionBody`
   boundaries: their embedded Waffle `Value`s belong only to the body being
   lowered.
7. `SValueId` remains the key. It is an arena identity, not a source-name
   substitute; this design introduces no textual identifier bookkeeping.

The existing source SSA convention gives every statement definition a unique
`SValueId`, but lookup still follows normal overlay precedence rather than
assuming unique keys. That keeps the module correct for entry/block parameter
setup and robust against future lowering changes.

## 3. Chosen design: immutable Arc sidecars plus one owned overlay

### 3.1 Private types

Add private types adjacent to `Continuation` in `conv.rs` (or move them to a
private `continuation_env` submodule once their tests need more room):

```rust
use std::sync::Arc;

#[derive(Debug)]
struct ValueSidecar {
    /// Older bindings shared by this whole suffix of continuations.
    parent: Option<Arc<ValueSidecar>>,
    /// Bindings frozen together at one real split boundary.
    bindings: BTreeMap<SValueId, LowerValue>,
    depth: u16,
}

#[derive(Debug)]
struct ContinuationValues {
    /// Immutable history, shared only through `Arc`.
    sidecar: Arc<ValueSidecar>,
    /// Bindings owned exclusively by this continuation since its last split.
    writable: BTreeMap<SValueId, LowerValue>,
}

#[derive(Debug)]
struct Continuation {
    block: Block,
    values: ContinuationValues,
}
```

`ValueSidecar`, not an `Arc` around every individual `LowerValue`, is the
sharing unit. Consequently a fan-out shares the actual historical
`LowerValue` allocations and their strings/Rc metadata without one atomic
allocation per SSA value. It also keeps the straight-line working set as one
ordinary `BTreeMap`.

`Continuation` and `ContinuationValues` deliberately do **not** implement
`Clone`. Forking must use the named operation below, so an accidental future
`.clone()` cannot recreate the original performance bug.

### 3.2 Small interface

`ContinuationValues` is the deep module at this seam. Its caller-facing
interface is intentionally small:

```rust
impl ContinuationValues {
    fn new(base: Arc<ValueSidecar>) -> Self;
    fn get(&self, id: &SValueId) -> Option<&LowerValue>;
    fn insert(&mut self, id: SValueId, value: LowerValue);
    fn freeze(self) -> Arc<ValueSidecar>;
    fn child(base: Arc<ValueSidecar>, id: SValueId, value: LowerValue) -> Self;
}
```

Implementation rules:

- `get` checks `writable`, then `sidecar.bindings`, then recursively (or
  iteratively) walks `parent`. It returns `&LowerValue`; existing lowering
  helpers retain their read-only use pattern.
- `insert` inserts into `writable`. It is legal only while this environment
  remains uniquely owned by one `Continuation`.
- `freeze(self)` **moves** `writable` into a new
  `Arc<ValueSidecar { parent: Some(self.sidecar), ... }>`. If `writable` is
  empty, it returns the existing sidecar. It must not clone either map.
- `child` starts an empty private overlay on a supplied frozen base and inserts
  the single produced value into that overlay.

The root sidecar owns the old `entry_values` map. It is built once per
`lower_function` and contains the entry-shim values that may be referenced
from every source block. For each `BlkSet`, block-parameter bindings go into
that block's fresh `writable` overlay. This removes the existing
`entry_values.clone()` per lowered source block as well as the per-statement
clone.

### 3.3 Driver algorithm

The statement loop becomes move-oriented. In pseudocode:

```rust
for stmt in source_block.stmts {
    let mut next = Vec::new();

    for mut continuation in continuations {
        let results = lower_statement(
            body,
            continuation.block,
            context,
            this.clone(),
            arguments.clone(),
            &continuation.values,
            source_value(stmt),
        )?;

        match results.len() {
            0 => {}
            1 => {
                let (block, value) = results.into_iter().next().unwrap();
                continuation.block = block;
                continuation.values.insert(stmt, value);
                next.push(continuation); // move; no map clone
            }
            _ => {
                let base = continuation.values.freeze(); // move; no map clone
                next.extend(results.into_iter().map(|(block, value)| {
                    Continuation {
                        block,
                        values: ContinuationValues::child(base.clone(), stmt, value),
                    }
                }));
            }
        }
    }

    continuations = next;
}
```

The read-only borrow passed to `lower_statement` ends before either `insert`
or `freeze`, so this structure is safe Rust without `unsafe`, interior
mutability, raw pointers, or aliasing assumptions.

This chooses the single-successor move path before any `Arc` operation. A
straight-line source block therefore retains the same owned `BTreeMap` from
its first statement to its terminator and does one `insert` per statement.

### 3.4 Sidecar-chain bound

A nested sequence of representation splits can make lookups traverse several
sidecars. Unbounded history is unacceptable as a replacement for map-copying,
so `ValueSidecar` records `depth`.

- Start with a root sidecar at depth zero.
- Set a conservative `MAX_SIDECAR_DEPTH` (initially 16, private and measured;
  it is not part of an external contract).
- Before freezing onto a sidecar at the limit, compact the chain into one
  new root-sidecar map. Walk oldest to newest so newer bindings win.
- Compaction is the *only* intentional whole-environment copy. It is bounded
  to one copy per sixteen nested fan-outs, never per source statement. It
  should be counted in test/diagnostic statistics.

Do not preemptively compact at every source block, join, or single-successor
statement. The point of the sidecar is precisely to preserve long shared
history without materializing it for each child.

If measurement shows the depth limit is not reached on realistic code, retain
it as a safety valve. If it is reached often, profile lookup and allocation
before changing the threshold or considering a persistent-map dependency.

## 4. Alternatives rejected

### 4.1 Only mutate the singleton continuation

A singleton fast path alone fixes the module initializer until its first
split, but after a split the old implementation still clones the complete
history for every child and for future fork points. It has no principled way
to share values between siblings. It is necessary but not sufficient.

### 4.2 `Arc<BTreeMap<...>>` with `Arc::make_mut`

This looks simpler, but the first insert in each child after a split invokes
copy-on-write on the entire map. It merely moves the catastrophic clone from
before the split to immediately after it. It also makes performance sensitive
to reference-count timing rather than the semantic fan-out boundary.

### 4.3 `BTreeMap<SValueId, Arc<LowerValue>>`

Making each value reference-counted still requires cloning every map node and
key/value entry on every environment copy. It adds one allocation and atomic
reference counting per source SSA value without solving the cardinality
problem.

### 4.4 A new persistent-map crate

A HAMT/RRB-style map could work, but adds a dependency and a second map model
to a very localized lowering problem. The sidecar interface has exactly the
operations this caller needs—lookup, private insert, and explicit
fan-out—and uses only `std::sync::Arc` plus the existing `BTreeMap`.
Reconsider a persistent map only if measured nested-split behavior defeats
the bounded-sidecar design.

### 4.5 Share one mutable map between continuations

This is unsound. Select/guard/accessor paths deliberately bind the same
source `SValueId` to distinct raw/reference `LowerValue`s. Shared mutability
would let one branch rewrite another branch's representation and silently
miscompile the generated Wasm CFG.

## 5. Migration plan

### Phase 1 — introduce and prove the environment module

1. Add `ValueSidecar` and `ContinuationValues` privately, with no converter
   behavior changed yet.
2. Add focused unit tests for:
   - root and overlay lookup;
   - a freeze followed by two children that share every parent binding but
     resolve their own same-`SValueId` result differently;
   - a moved singleton continuation accepting later inserts without changing
     earlier bindings;
   - empty-overlay freeze reusing its sidecar;
   - 16+ nested freezes compacting correctly with newest binding precedence.
3. Use `SValueId::new(...)` in these tests; construct minimal `LowerValue`
   payloads with distinct Waffle `Value` ids/kinds so tests verify identity,
   not merely presence.

**Gate:** `cargo test --offline -p portal-jsc-waffle --lib` passes before the
driver is migrated.

### Phase 2 — migrate the `lower_function` driver

1. Replace `Continuation.values: BTreeMap<...>` with `ContinuationValues`.
2. Move `entry_values` into one root `Arc<ValueSidecar>` and construct each
   source-block continuation from that base plus its block parameters.
3. Replace the per-statement nested `for` clone loop with the explicit
   zero/one/many algorithm in §3.3.
4. Retain `lower_statement`'s result type, `(Block, LowerValue)`, unchanged.
   It expresses the path-local result cleanly and avoids widening all split
   helpers merely to optimize the driver.
5. Remove `Clone` from `Continuation` and make environment forking impossible
   except through `freeze`/`child`.

**Gate:** generated `FunctionBody::validate()` and the existing
portal-waffle unit suite pass with no source or emitted-Wasm behavior changes.

### Phase 3 — replace the read-only map parameter at the seam

1. Change the `values: &BTreeMap<SValueId, LowerValue>` parameters reached
   from `lower_statement` to `values: &ContinuationValues` (or a private
   read-only `ValueLookup` wrapper if that produces clearer signatures).
2. Keep the exposed interface to one lookup method. Do not leak sidecar,
   `Arc`, overlay, or compaction details into expression/property/call
   helpers.
3. Mechanically update every lookup. Preserve each current missing-value
   `ConvertError` message exactly; failed lookup is still a lowering error,
   not an `undefined` JavaScript value.
4. Search for remaining environment clones (`continuation.values.clone()`,
   `entry_values.clone()`) and reject any use outside intentionally isolated
   test setup.

**Gate:** run, at minimum:

```bash
cargo test --release --offline -p portal-jsc-waffle --lib
cargo test --release --offline -p portal-jsc-waffle --test e2e
cargo test --release --offline -p jsaw-wasi-bin --test dogfood_m19 -- --ignored --nocapture
```

The last command is an explicit heavyweight M19 gate, using its existing
256 MiB lowering-thread stack. It must be run with a bounded external timeout
and report its elapsed time/RSS; a timeout is a result to investigate, not a
passing inference.

### Phase 4 — measure, document, and land

1. Add temporary, opt-in aggregate diagnostics (for example
   `JSAW_CONTINUATION_STATS`) recording per lowering run:
   - statements lowered;
   - zero/single/fan-out result counts;
   - sidecars frozen;
   - maximum sidecar depth;
   - compactions and bindings copied by compaction.
2. Use it on the M19 artifact to demonstrate that the approximately 300k-value
   module initializer takes the single-successor move path rather than cloning
   its full environment per statement.
3. Compare elapsed time and peak RSS against the current reproducible M19
   run. Record measurements and the remaining Stage-C blocker, if any, in
   `docs/plan-dogfood-self-hosting-java.md`.
4. Remove or leave diagnostics only if they are explicitly opt-in and useful
   for future scale regressions; do not make normal compilation noisy.

**Gate:** all ordinary regressions remain green, the M19 run either reaches
its next real diagnostic/limit or completes Stage C, and no new unbounded
memory behavior is introduced. This work removes the continuation-copying
wall only; it does not claim to solve the separate final-SSA-retention or JVM
method-size limits.

## 6. Regression coverage

In addition to the environment-unit tests, retain/add behavior tests that
exercise both the move and fork paths:

| Scenario | Required property |
| --- | --- |
| Long straight-line arithmetic/assignment chain | One continuation stays live; generated behavior remains correct; stats show no fan-out-induced environment copy. |
| `cond ? number : object` followed by use | The two continuations retain distinct `LowerValue` kinds for the same source statement and both reach valid terminators. |
| Guarded local-function or object-method call | Fast and generic fallback branches each resolve earlier arguments/receiver bindings correctly. |
| Accessor/property read with multiple paths | Every path sees the same pre-split values and its own current result. |
| Nested split depth beyond the compact threshold | Compacting preserves lookup precedence and generated body validation. |
| Existing M19 Stage-C fixture | The real generated compiler progresses past the former 45-billion-entry-copy shape without weakening diagnostics. |

Run the normal CoreGC cross suite as a downstream conversion-regression gate
as well. It has previously caught invalid Waffle CFG shapes from unrelated
staged diagnostics, so it is useful evidence that this representation-only
refactor did not perturb generated control flow.

## 7. Success criteria

The redesign is complete only when all of the following are true:

- `lower_function` has no full continuation-environment clone in its
  statement loop.
- A singleton result moves and extends the existing continuation.
- A fan-out moves historical bindings once into an `Arc<ValueSidecar>` and
  siblings share that sidecar while keeping independent writable overlays.
- Lookup depth is bounded by the tested compaction policy.
- No public converter interface or JavaScript/Wasm semantic contract changes.
- Existing unit/e2e/CoreGC tests pass, and the bounded M19 stress run records
  its actual next-stage outcome with elapsed time and peak memory.

## 8. As-built result (2026-09-19)

Implemented in these commits:

1. `f250a85` — introduced `ValueSidecar` / `ContinuationValues`, bounded
   compaction, and five focused environment tests before changing the driver.
2. `7554c36` — migrated `lower_function` and all read-only expression/call
   helper seams to the move-oriented environment. A singleton result mutates
   and moves its continuation; a fan-out freezes the old writable overlay once
   and gives each child a private one-binding overlay.
3. `31bf36c` — added opt-in `JSAW_CONTINUATION_STATS` accounting for path
   shapes, freezes, depth, and compaction.

The real 168,968,420-byte Stage-B M19 artifact was run with the statistics
flag. Lazy ingestion completed in **640.35 s**. During Stage C, the large
straight-line function recorded **202,454 statements, 202,454 singleton
paths, zero fan-outs, zero sidecars, and zero compactions**, demonstrating
that the former per-statement whole-map clone is absent. The run then reached
a distinct, fail-closed frontend diagnostic after about 33 more seconds:
`coercing Integer to BigInt (mixing BigInt and number) at "BigInt operand"`.
It no longer stalls in continuation-environment copying. The bounded run's
reported maximum resident set size was 20.9 GiB.

The focused environment tests and `portal-jsc-waffle` lib suite pass; the
release CoreGC differential suite passes 14/14. A targeted e2e JVM test
(`executes_arrays_as_objects_with_named_fields`) still fails with `split
function fell through`, but it reproduces on the Phase-1/baseline converter
and is therefore not attributed to this redesign. It remains a pre-existing
JVM-emitter correctness bug to diagnose separately.
