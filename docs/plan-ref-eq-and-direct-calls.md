# Plan: `ref.eq` for `===`, and direct calls for provable/guarded primordials

Follow-up to the typed-array work (commit `4cb0120`). Two independent work
items, both in `crates/portal-jsc-waffle`.

---

## Part 1 — Use `ref.eq` to implement JS `===`

### Current state

`Converter::equality` (`conv.rs`, ~line 1788) handles `EqEq | EqEqEq | NotEq |
NotEqEq` with one shared path:

- `(Number, Number)` → `F64Eq` / `F64Ne`
- `(Boolean | Integer, Boolean | Integer)` → `I32Eq` / `I32Ne`
- everything else → `box_value` both sides, then a single `Operator::RefEq`

`RefEq` is already emitted for the fallback, so the operator itself is proven
to work in this waffle build. The problem is that the fallback is *semantically
wrong* for several primitive combinations, because a `ValueKind::Reference` at
this point may hold a *boxed primitive* that crossed a call/property boundary
and lost its unboxed kind:

| Comparison | Current behavior | Correct `===` behavior |
|---|---|---|
| `"abc" === "abc"` (both `LowerValue::String`) | `RefEq` on two distinct string structs → **false** | content equality → true |
| boxed number `5` (from a call result) `=== 5` | fresh box vs `RefEq` → **false** | numeric → true |
| boxed boolean crossing a boundary `=== true` | `RefEq` → **false** | value equality → true |
| `null === undefined` | both lower to a null `anyref` (`Lit::Null` → `undef`) → `RefEq` **true** | **false** |
| `{} === {}` | `RefEq` → false | false (correct) |
| `a === a` (same object) | `RefEq` → true | true (correct) |

WasmGC `ref.eq` is genuinely correct only for genuine reference types:
objects, arrays, functions-as-structs, typed arrays. Primitives need value
comparison; `null` vs `undefined` need distinct representations.

### Work items

1. **Separate strict from loose.** Split `equality` into
   `strict_equality` (used by `EqEqEq`/`NotEqEq`) and keep the existing
   `equality` for `EqEq`/`NotEq` (loose equality with coercions is out of
   scope for this milestone; it keeps today's behavior).

2. **Static fast paths in `strict_equality`:**
   - `(String, String)` → call `ensure_string_equal` (already generated,
     byte-wise compare). Negation wraps with `I32Eqz`.
   - `(String, other)` / `(other, String)` → constant `false` fast path (no
     helper call; only valid for strict equality — a string is never
     strict-equal to a number/boolean/object).
   - `(Number, Number)`, `(Boolean|Integer, Boolean|Integer)` → unchanged
     from today.
   - `(Boolean, Number)` and similar cross-primitive statics → constant
     `false` (strict equality never coerces).

3. **Runtime guarded path for `(Reference, Reference)`.** Emit one shared
   helper `ensure_strict_equality_helper(value, value) -> i32` implementing
   full strict equality on boxed values:
   - `ref.test` number on either side → unbox both via `as_f64`, `F64Eq`
     (correct for `NaN !== NaN` and `+0 === -0`).
   - `ref.test` string → `ensure_string_equal` on both sides cast to string.
   - `ref.test` boolean → unbox and `I32Eq`.
   - null vs null → need a decision (see item 4) before returning true.
   - otherwise → `ref.eq` (both sides cast to the common object/function
     hierarchy; all our heap values are struct refs, so `ref.eq` is legal).
   
   Emit the helper call only when at least one side has an unknown kind;
   when both sides are statically known references that can only be
   objects/functions (e.g. results of `new`, object literals, method
   lookups), keep the direct inline `RefEq` — that is the hot path this
   milestone is about.

4. **Distinguish JS `null` from `undefined`.** Both currently lower to a
   null `anyref`, so `ref.eq` cannot tell them apart. Pick one:
   - *Option A (recommended):* reserve null `anyref` for `undefined` and
     represent JS `null` as a singleton empty struct (a per-module
     `js_null` struct, one instance stored like other singletons, handed
     out by a tiny helper). `ref.eq` against the singleton then works
     naturally, and `x === null` becomes a `ref.test` against the singleton
     type. `Lit::Null` and property-miss results switch to the singleton;
     `undef` stays null.
   - *Option B:* keep both as null and accept `null === undefined → true`
     as a documented deviation until a tag bit exists in the object header.
   
   Option A touches `undef` callers (`Lit::Null`, missing-arg synthesis,
   property-miss synthesis) — audit each call site when doing this.

5. **Optional: string interning.** If string literals were interned at
   creation (module-level dedup table — note the backend has no
   reference-typed globals, so this needs an internal registry array plus a
   lookup helper, or interning only *literals* at compile time by
   canonicalizing identical literal contents to one shared string
   constant), then literal-vs-literal `===` could stay on inline `RefEq`.
   Compile-time literal canonicalization is cheap and catches the common
   case; defer runtime interning.

6. **Tests** (`tests/e2e.rs`):
   - object identity: `let a = {}; a === a`, `{} === {} → false`
   - `a !== b` negated forms
   - string strict equality: two separately-built equal strings, unequal
     strings, string vs number → false
   - boxed-primitive round trip: `f() === 5` where `f` returns `5` through
     a call boundary
   - `null === undefined → false`, `null == undefined` unaffected
   - typed array identity: same view twice → true; two `subarray`s → false
   - NaN: `NaN === NaN → false` via a helper-returned NaN

---

## Part 2 — Direct calls for globals / primordials (lookup, compare, call body)

### Current state

Every call goes through the generic dispatch path:

1. Callee resolution (`TCallee::Id` / `TCallee::Member`, `conv.rs` ~line
   7840) does a property lookup on the context object → any `anyref`.
2. `callable_parts` (`conv.rs` ~line 7902) ref-casts to `function` struct,
   extracts `code` (adapter `ref.func`), `context`, `this`, `arrow`.
3. Arguments are boxed into a fresh `arguments` array (`make_arguments`).
4. `CallRef` through the generic adapter signature
   `(object, anyref, arguments) -> anyref`.
5. Inside the adapter, `read_arg_raw` re-coerces each argument, results get
   re-boxed.

The same applies to `Item::New` (~line 1290), which additionally
pre-allocates a receiver object and branches on whether the constructor
returned an object (the "override" check) — work native constructors like
the typed-array ones never need.

Native primordials are `function` structs whose captured context (field 4
of the *adapter's* context object; for typed-array constructors the kind
tag rides in field 4 of that private context object, set in
`primordials.rs`) parameterizes the shared adapter body. Adapter bodies are
cached by label in `native_function_cache`.

### Goal

When a call site's callee can be *proven* or cheaply *verified* to be a
known primordial, skip steps 1–4 (and the receiver/override work for
`new`) and call a **specialized body with a typed Wasm signature** — the
body split out from the generic adapter interface.

### Work items

1. **Split native bodies into fast core + thin adapter.** For each
   primordial, generate:
   - a *fast core* `Func` with a concrete signature, e.g.
     `Math.sqrt` → `(f64) -> f64`, `typed-array subarray` →
     `(object, i32, i32) -> object`, typed-array constructor →
     `(i32 kind, arguments-or-typed-args) -> object`;
   - the existing generic adapter, reduced to: unpack the `arguments`
     array via `read_arg_raw`, coerce with the existing `as_f64` etc.,
     `call` the fast core, box the result.
   
   Cache both under the existing `native_function_cache` label scheme
   (e.g. `"math.sqrt"` / `"math.sqrt#fast"`).

2. **Provable-callee analysis (no runtime guard).** Add a per-function
   scan, modeled on `collect_shapes`, that records whether any
   `StoreId`/`Assign` writes a name that collides with a primordial global
   (`Math`, `Array`, `Reflect`, `Object`, the nine typed-array names) or a
   primordial namespace member (scan member writes on those ids). If a
   name is never shadowed in the function, then:
   - `LoadId("Math")` / `TCallee::Id("Math")` resolves to the primordial
     namespace *by construction* — represent this as a new
     `LowerValue::Primordial { namespace }` (or carry the info in a
     side map keyed by `SValueId`) so the *call site* can short-circuit
     without materializing the namespace object at all;
   - static member calls on that id (`Math.sqrt(x)`) resolve to the
     fast core directly: unboxed args in, unboxed result out, no
     arguments array, no `CallRef`.
   
   Non-call uses of the same id (`let m = Math;`) still lower through the
   ordinary property path — only call sites take the shortcut, so the
   namespace object must still be constructible when actually needed.

3. **Guarded-callee path (runtime check, general fallback).** For call
   sites where shadowing cannot be ruled out statically, emit:
   - the ordinary lookup as today;
   - `ref.test function` + `StructGet` of the code field + `RefEq` (or
     `I32Eq` on func indices if waffle exposes them) against the expected
     `ref.func` of the fast-core-dispatching adapter;
   - on match → direct `call` to the fast core with unboxed arguments;
   - on mismatch → the existing generic `CallRef` path unchanged.
   
   This keeps user shadowing (`let Math = { sqrt: () => 4 };`) correct
   with one predictable branch as the only cost.

4. **Typed-array constructors / `Item::New`.** Add the same two-tier
   scheme to `Item::New`:
   - provable case: skip the pre-allocated receiver and the
     object/function override branch entirely; call a per-kind
     constructor core (or the shared one with the kind tag as a constant)
     that returns the typed-array object directly.
   - guarded case: `ref.test` the callee's code against the typed-array
     constructor adapter; on match call the core, on mismatch fall back to
     today's path (including the arrow-reject branch, which the fast path
     can also skip because native primordials are never arrows).
   
   The per-kind tag is already available at the call site when the
   constructor name is statically known (`new Uint8Array(...)`), so the
   guarded path can even compare the *context's* tag field rather than the
   code pointer.

5. **Scope the first batch.** Land the machinery on a small, high-value
   set first, then extend:
   - `Math.*` numeric methods (pure `(f64...) -> f64` cores — simplest
     unboxing story);
   - typed-array constructors + `length`/`byteLength`/static-index reads
     (the receiver is already the typed object; the guard is the header
     kind tag);
   - `Array.isArray` (single `(anyref) -> i32` core).
   Leave variadic/coercion-heavy methods (`set`, `push`) on the generic
   path until the arguments-array elision story is settled.

6. **Tests** (`tests/e2e.rs`):
   - provable path: a function that calls `Math.sqrt` with no shadowing
     still returns correct values; validated + executed in all runtimes
   - guarded fallback: `function f(Math) { return Math.sqrt(4); }`-style
     shadowing (and `let Math = ...`) returns the shadow's result
   - direct-constructor: `new Int8Array(n)` in a loop produces correct
     results (exercises the skip-receiver/override path)
   - existing typed-array e2e suite must pass unchanged — it is the
     regression net for the `new` path changes
   - a numeric micro-loop (e.g. sum of `Math.sqrt(i)` over 1000 elements)
     to confirm the fast path is actually emitted (assert on module
     inspection: the fast core is called directly, no `CallRef` on that
     path)

### Sequencing

1. Part 1 items 1–3 (strict equality fast paths + helper) — independent,
   no representation changes.
2. Part 1 item 4 (null singleton) — small but touches many `undef` sites;
   do it as its own commit.
3. Part 2 items 1–2 (fast cores + provable analysis) for `Math.*`.
4. Part 2 item 4 (constructor direct path).
5. Part 2 item 3 (guarded path) — most general, lands last so the
   provable path's tests establish the baseline first.

### Risks / notes

- `ref.eq` requires both operands in the `eqref` hierarchy; all our heap
  values are structs so a cast to the common struct hierarchy is enough —
  but verify waffle's `Operator::RefEq` accepts the `value` (anyref) type
  or whether an intermediate eqref type must be registered in `Repr`.
- The `native_function_cache` keys must stay stable per module; adding
  `#fast` variants must not collide with the existing label set.
- Loose `==` deliberately keeps today's (incomplete) behavior; note it in
  the module comment so the deviation is discoverable.
- The provable-callee scan must run before lowering (like
  `collect_shapes`) and be per-function, not per-module, since shadowing
  is function-local.
