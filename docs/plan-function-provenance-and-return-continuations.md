# Plan: retain-and-check call fast paths, and return-kind continuations

Follow-up to the implemented fast-call work (see git history for the
`ref.eq` strict-equality + provable primordial implementation). Two
extensions:

1. **Part A — provenance retention for local function variables and object
   methods.** The provable/guarded fast paths currently fire only for
   primordial *namespace* identifiers. This plan extends identity retention
   to `let f = function() {...}` locals and `{ method() {...} }` members, so
   calls through them skip `callable_parts` + `CallRef` while a cheap check
   proves the binding still holds the original function literal.

2. **Part B — continuations on concrete return types.** Every generic call
   boxes its return to `anyref`; the caller then re-coerces. This plan makes
   `Item::Call` a *continuation-producing* statement, split per statically
   possible return representation, and adds unboxed-return adapters so fast
   cores feed raw `f64`/`i32` into the parent function.

Both parts reuse the existing representation-splitting machinery
(`Continuation`, `BlkSet`, `select_continuations_for_condition`), which
already lowers one SSA block several times for refined value kinds.

---

## Part A — provenance retention

### A0. Current shape (what exists today)

- `LowerValue` (`conv.rs` ~line 310) carries static key metadata for
  *identifiers* (`ReferenceKey { value, key }`) — this is what the primordial
  provable path matches on. Function literals themselves lower through
  `Item::Func` → `function_object_from_info` to a plain
  `ValueKind::Reference`, and all static identity is lost immediately.
- `function_object_from_info` (~line 5668) has the `FunctionInfo`
  (`native` + `adapter` + `arity`) in hand at construction time and throws it
  away.
- `SValue::StoreId` lowering (`lower_statement`, ~line 1149) writes any
  variable through `set_property` onto the context object; the value being
  stored keeps only its boxed `LowerValue`.
- `object_literal` (~line 5734) places `PropVal::Method`/`Getter`/`Setter`
  functions via the generic property-write path; the shape field index the
  method lands in is known statically but never recorded.
- A call site goes through `lower_call_parts` → `callable_parts`
  (~line 8482): `RefCast` to the function struct, four `StructGet`s, a
  `TypedSelect` for `this`, then `CallRef` through the adapter signature.

### A1. New `LowerValue` variant: `FunctionRef`

```rust
/// A function value whose *origin* is statically known: a specific lowered
/// source function (and therefore a specific `FunctionInfo`). The value is
/// still an ordinary function object at runtime; the origin enables
/// direct-dispatch checks at call sites.
FunctionRef {
    value: Value,           // the boxed function object (anyref)
    info: FunctionInfo,     // native + adapter + arity of the literal
    origin: FunctionOrigin,
},
enum FunctionOrigin {
    /// A specific object-literal member: which shape owns the slot.
    Method { shape: usize, field: usize },
    /// A context variable slot (identifier name), written exactly once
    /// during the current scan window.
    Local { name: String },
}
```

Construction sites:

- `function_object_from_info` already receives `info`; wrap the result in
  `FunctionRef { origin }` when the caller knows one (`Item::Func` knows it
  is a literal; object-literal methods know their shape + field).
- `Item::Just { id }` / `LoadId` alias propagation: when the aliased value is
  a `FunctionRef`, keep the variant (like `String` retention across aliases,
  which is already sound for the same immutability-ish reason). The
  `runtime()` boundary keeps it — a function object crossing a branch is
  still the same object.
- `LoadId(key)` (`lower_statement` ~line 1135): after the generic
  `get_property`, if a per-function scan (A2) recorded that `key` was
  initialized from a single function literal and never reassigned, tag the
  result `FunctionRef::Local`.

### A2. Per-function scan: `collect_function_literals`

A sibling of `collect_shadowed_names`, run in the same pre-lowering pass
(`convert` / `convert_module` already call `collect_shapes` +
`collect_shadowed_names` per top-level function):

1. Walk `SValue::StoreId { target, val }` statements. If `val` resolves (via
   a cheap `SValue` lookup, not full lowering) to an `Item::Func` — record
   `target` in `single_assignment_functions: BTreeMap<String, FunctionKey>`.
2. Any *second* `StoreId` to the same name, any `SValue::Assign { LId::Id }`,
   any parameter with the same name, or a nested closure assigning it
   removes the entry (conservative: exactly the `collect_shadowed_names`
   taint pattern, but initialized from the literal instead of banned
   outright).
3. For object literals: during the existing `collect_shapes` walk, note
   members whose `PropVal::Method(f)`/`Item(f)` is an `Item::Func`; the
   shape index + sorted field position is already determined by
   `register_shape`.

Output consumed by A1 tagging and by the call-site check selection (A3).

### A3. Call-site dispatch

Extend the `Item::Call` pre-hook (currently only `TCallee::Member` over a
`ReferenceKey` namespace) and `lower_call_parts`:

- **`TCallee::Val(v)` where `values[v]` is `FunctionRef`**:
  - *Provable* (origin `Local`, name proven single-assignment): emit the
    direct call to `info.native` with the ABI
    `(context, this, arguments, args...) -> value`, passing the *current*
    context and `undefined` receiver. Skips `callable_parts`, the box of the
    receiver, and the adapter's formal re-read (the adapter's
    missing-argument blocks are exactly what the direct native call
    avoids — the caller statically knows `arity` and rejects arity mismatch
    the way `make_arguments` + `ArrayLen` does today, or simply relies on
    the native body's own `read_arg` semantics for missing formals).
  - *Guarded* (retention could not be proven): read the current binding via
    the ordinary lookup, then `ref.test` that it is a function struct and
    compare the tag field (A4 option 2) against the literal's minted tag.
    Match → direct native call; mismatch → generic `CallRef` path (the
    fallback blocks are the ordinary continuation, so the check costs one
    branch, not a duplicated body).

- **`TCallee::Member { func: receiver, member }` where the receiver's
  `LowerValue` is an object-literal result with a known shape and the member
  is a `Method` field**:
  - *Provable*: the receiver `Value` is the literal allocation in the same
    block scope — if the literal was allocated in the current block (tracked
    by the scan) the method slot provably holds the literal; call
    `info.native` directly with `receiver` as `this`/context per ordinary
    method semantics.
  - *Guarded*: `ref.test` the receiver against the shape's concrete struct
    type (`ShapeInfo.sig`), `StructGet` the method field, then the same
    code-pointer check as above. No trie walk, no string keys.

### A4. Callee-identity check mechanics (risk item — resolved: use tags)

Wasm has no funcref equality: `ref.eq` rejects `funcref`, and `ref.test`
against the adapter's concrete signature type is not identity (any function
sharing the signature passes it). Two candidate mechanisms:

1. **Immutable `funcref` globals + `ref.eq`**: still fails — a `funcref`
   loaded from a global is a `funcref`, and no equality instruction exists
   for it in the current Wasm GC toolchain the backend emits against.
   Rejected.
2. **Reuse the function struct's tag field** (added for primordials in the
   previous milestone): `ensure_function` mints every user function a fresh
   nonzero tag from a converter counter (primordial tags start at
   `PRIMORDIAL_TAG_BASE`, so user tags start above, e.g. `1 << 20`), stored
   via `function_object_from_info`. A guarded call site performs the same
   `StructGet field 7` + `i32.eq` the primordial path already uses.
   Strictly correct: the tag lives inside the function object, travels with
   it, and only `ensure_function` ever mints one — an equal tag means the
   same literal.

**Chosen: option 2.** ~10 lines in `ensure_function` +
`function_object_from_info`; the check mirrors the proven primordial
mechanism exactly.

### A5. What stays generic

- Function values that escape through property writes onto *other* objects,
  arrays, or closures' captured environments: the scan only vouches for the
  original slot; the check (guarded or provable) is what keeps this sound.
- `arguments`-dependent calls keep the raw array: the direct native call
  still passes `make_arguments`'s output as argument 3, unchanged.
- Recursive/self-calls inside the function: `Item::Func` for the function's
  own declaration is available at lowering time via `ensure_function`, so
  self-recursion gets the provable path for free (worth a test).

---

## Part B — continuations on concrete return types

### B0. Current shape

- `ensure_function` (~line 523) gives every user function the native
  signature `(context, this, arguments, T...) -> T` where every `T` is the
  boxed `anyref` (`repr.value`). The native body's `TTerm::Return` lowering
  (~line 905) unconditionally `box_value`s the returned `LowerValue`.
- Every call site (`Item::Call`, `lower_call_parts`) receives a
  `ValueKind::Reference` result and re-derives primitives with casts at each
  use (`as_f64`, `as_condition`, `RefTest`s).
- Fast cores already return unboxed `f64`/`i32`; the primordial provable
  path boxes their result immediately (`box_value` in
  `try_provable_primordial_call`) — throwing away exactly the information
  Part B wants to keep.

### B1. Static return-kind analysis: `ReturnKinds`

Per `SFunc`, compute the set of `ValueKind`s its return statements can
produce. A new pre-pass in the same family as `collect_shapes` /
`collect_shadowed_names`:

```
enum ReturnKind { Reference, Number, BooleanOrInteger }
```

- Coarse is deliberate: `Boolean` and `Integer` share the `i32` Wasm type
  (`wasm_type` maps both to `I32`), and distinguishing them would split
  blocks without enabling different code — a boxed `true` and a raw `1`
  still need the same consumer paths. `Number` is `f64`. Everything else
  (objects, strings, functions, arrays) is `Reference`.
- Walk `TTerm::Return`/`TTerm::Tail` terminators per block of the `SCfg` and
  classify the returned `SValueId` lexically:
  - `Item::Lit { Num }`, `Item::Bin` with arithmetic ops, `Item::Un` with
    `-`/numeric coercion targets → `Number`
  - `Item::Lit { Bool }`, comparison/`Bin` with `Eq`-family or relational
    ops → `BooleanOrInteger`
  - `Item::Call` → recurse into the callee's `ReturnKinds` when it is a
    local/hoisted function (or a primordial fast-core tag → `Number` for the
    Math surface, `BooleanOrInteger` for `Array.isArray`)
  - Anything else (`Item::Obj`, `Item::Func`, `LoadId`, `Item::Call` with
    unknown callee) → `Reference`
- `SValue::EdgeBlocker` and `Item::Just` chains are transparent (follow the
  alias, same as `LowerValue::runtime`).
- Blocks with phis: if incoming edges disagree, the join is the union —
  matching how `BlkSet` already re-lowers blocks per parameter-kind tuple.
- Store the result per `SFunc` key next to `FunctionInfo`
  (`FunctionInfo { native, adapter, arity, return_kinds: ReturnKinds }`),
  filled before `lower_all` runs so call sites can read it even when the
  callee body has not been lowered yet.

Fast cores: their kinds are constants (`TAG_MATH_*` → `Number`;
`TAG_ARRAY_IS_ARRAY` → `BooleanOrInteger`), recorded in the same table keyed
by tag.

### B2. Return-kind-split native signatures

For each user function, when `return_kinds` is a *single* kind:

- `Reference` (the overwhelmingly common case today): unchanged — current
  ABI already returns boxed `anyref`.
- `Number`: emit the native body with return type `f64` and drop the
  `box_value` in the `TTerm::Return` lowering when the returned
  `LowerValue` is already `ValueKind::Number` (it always is, by the
  analysis). `as_f64`-style numeric returns of `Math`-wrapped locals become
  raw.
- `BooleanOrInteger`: same with `i32`.

For *multiple* kinds, keep the boxed ABI (the boxed return is the join) —
splitting the function's exit per kind would duplicate the CFG for little
gain and fight `BlkSet`'s existing per-kind re-lowering. The unboxed
**adapter** (B3) still helps: it refines the boxed result into per-kind
continuations on the caller side.

The adapter (`make_adapter`, ~line 559) always keeps the boxed ABI — it is
the generic-dispatch fallback and its signature is pinned by `ref.func`
table placement. Only *direct* call sites consult `return_kinds`.

### B3. Unboxed-return adapters: `make_unboxed_adapter`

New sibling of `make_adapter`, generated lazily per `(native, kind)`:

```
sig: (context, this, arguments, T...) -> <raw kind>   // same param layout
body: call native (boxed ABI, unchanged)
      RefTest against the kind's type:
        Number  -> RefTest (ref null $number)  / RefCast / StructGet 0 -> f64
        Boolean -> RefTest (ref null $boolean) / RefCast / StructGet 0 -> i32
      trap on RefTest failure (a return-kind bug, not a JS condition —
      the analysis is trusted, so the failure mode is a loud cast)
```

This is the "adapters adapt the unboxed returns to a single value" seam
inverted: the boxed result is *destructured* once, in one tiny function,
instead of re-coerced at every use site. Generation is cached in
`FunctionInfo` alongside `native`/`adapter` (add
`unboxed: BTreeMap<ReturnKind, Func>`).

For fast cores no adapter is needed — they already return raw; the
primordial call path stops boxing and forwards the raw result directly.

### B4. Caller-side continuations

`Item::Call` becomes a continuation-producing statement (like `Item::Select`
and `Item::Mem` already are):

- Compute the callee's possible kinds from B1:
  - provable direct calls (primordial or Part A `FunctionRef`): the kind is
    a singleton → **no continuation split needed at all**; produce a
    `LowerValue::Wasm { kind }` directly and keep the current single-block
    flow. This is the main win: `let x = Math.sqrt(9); x * 2` lowers `x` as
    raw `f64` end-to-end.
  - guarded/direct-but-multi-kind calls: emit the call into a short split —
    `RefTest` per possible kind, one continuation block per kind carrying
    the refined `LowerValue`, exactly following
    `resolve_property_read_paths`'s shape (it already splits
    ordinary/absent/getter and rejoins through a block parameter). The
    existing `Continuation` machinery then lowers the rest of the SSA block
    once per kind, and `target_block`/`BlkSet` deduplicate when both arms
    agree.
- `TTerm::Tail` (`ReturnCallRef`, ~line 1024) keeps the boxed adapter ABI —
  tail calls cross an activation boundary, so the return refinement happens
  in the *caller's* epilogue, not the tail position.

### B5. Interaction matrix (what combines with what)

| Call site form | Part A path | Return kinds | Emission |
|---|---|---|---|
| Primordial, provable | direct core call | singleton | raw result, no split, no box |
| Primordial, guarded | tag check + core | singleton | raw on fast arm, boxed→unbox on fallback arm |
| Local fn, provable (A) | direct native call | from B1 | unboxed adapter when kind ≠ Reference, else plain |
| Local fn, guarded (A) | tag check + native | from B1 | same split as above |
| Method, provable (A) | direct native call | from B1 | as above |
| Unknown callee | generic `CallRef` | unknown | current behavior, optionally the B4 split with kinds {Reference} ∪ known |

The primordial guarded arm (fallback through the generic adapter) and the
fast arm return *different* representations — the B4 continuation split is
what makes that legal without a join-box. This is the one place where Parts
A and B structurally interlock; implement A3's guarded emission on top of
B4's split, not before it.

---

## Implementation order

1. **A4** (tag minting for user functions) — tiny, unblocks everything.
2. **B1** (`ReturnKinds` pass) — pure analysis, unit-testable via e2e
   inspection like `provable_math_call_reaches_fast_core_directly`.
3. **B3 + B4** (unboxed adapters + caller splits) — the core Part B work;
   test with `let x = localNumberReturningFn(); x * 10` style e2e cases and
   a module-inspection assert that no `StructGet $number 0` re-coercion
   appears in the parent body after the call.
4. **A1 + A2** (`FunctionRef` + literal scan) — representation groundwork.
5. **A3** (call-site dispatch for locals + methods) — final wiring; the
   guard/fallback tests from the primordial work (shadowing wins) translate
   directly.

## Tests to add

- `let f = () => 7; f()` and `function outer() { function inner() {...}
  inner(); }` → direct native call (inspect: no `CallRef` in the parent
  body, `js_body_*` reached via `Call`).
- `let f = ...; f = other; f()` → guarded path still correct (fallback
  executes; inspect: tag check present).
- `let o = { m() { return this.v; } }; o.m()` → method fast path with
  `this` wired from the receiver.
- `let o = { m() {...} }; let g = o.m; g()` → detached method falls back
  (receiver becomes undefined, not the literal's `this`).
- Number-returning local fn: parent arithmetic consumes raw `f64` (inspect:
  no boxed-number `StructGet` between the call and the use).
- Mixed-kind returns (`return a ? 1 : {}`) keep the boxed ABI and still
  validate.
- `Math.sqrt` result feeding a phi with a boxed arm (join kind union).

## Implementation status (2026-09-04)

All four commits are implemented; the full e2e suite is 43 passed with
only the 2 pre-existing utf16 string failures:

1. `ed35047` — A4: user function tags (`USER_TAG_BASE = 1 << 20`),
   minted per literal in `ensure_function`, stamped by
   `function_object_from_info`.
2. `d026d4d` — B1–B2: `ReturnKinds` analysis + raw native return types
   with `make_adapter` re-boxing unboxed returns at the adapter
   boundary.
3. `dc0ace6` — B3–B4: primordial provable paths return raw f64/i32
   results; `guarded_primordial_call` splits fast/slow arms into
   separate continuations (also fixes the latent guarded-`Array.isArray`
   conversion error).
4. `f892f14` — Part A: `FunctionRef`/`ObjectLiteral` provenance,
   single-assignment literal scan, `direct_native_call`, guarded tag
   dispatch for locals and methods, e2e + inspection tests.

Deviations discovered during implementation:

- The analysis must classify *only* `TTerm::Return` values (an earlier
  draft folded every statement value into the set, collapsing every
  function to the boxed join).
- `ReturnKinds::native_return_type` takes `&Repr` (the boxed type is
  `repr.value`, not a constant).
- Pre-existing backend bugs surfaced by the new tests (both confirmed
  on the clean tree, out of scope here): `y === x` between two context
  reads traps with a cast failure, and some ternary/call-result
  coercions trap with a null reference. The e2e fixtures avoid those
  patterns.

## Explicit non-goals

- Whole-program escape analysis of function objects (the checks are
  per-call-site, cheap, and sound without it).
- Splitting *primitive* return kinds finer than Number/i32 (no consumer
  distinguishes `Boolean` from `Integer` today).
- Unboxed *parameter* ABI for user functions (arguments can be missing /
  `arguments`-observable; the adapter's formal-reread blocks are the
  correctness mechanism — revisit only with static arity proof).
- Making the generic adapter itself return unboxed (it is pinned by
  `ref.func` table placement and is the universal fallback).
