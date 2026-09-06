# Plan: tail recursion via callee-kind unioning, then dedicated multi-value return types

Follow-up to `plan-merge-tailcalls-and-multi-return.md` (Milestones 1–3 all
landed: module ingestion, tail calls with static dispatch, tagged-union
multi-return cores). Two further milestones:

- **Milestone 4** — let tail-carrying functions keep a precise return ABI by
  *unioning the analyzed return kinds of their tail callees* (falling back to
  `Reference` for unknown callees), which unlocks direct `ReturnCall` tail
  dispatch for raw and union ABIs: true O(1)-stack tail recursion without
  boxing.
- **Milestone 5** — replace the single fat `$multi_ret { tag, r, i, f }`
  union with *dedicated per-kind-set return types* carrying only the payload
  slots the core actually needs (a `{Boolean, Number}` core returns
  `{ tag, i: i32, f: f64 }`, no reference slot), plus union forwarding
  through tail chains and continuation splitting on known calls to those
  cores.

The two interact deliberately: Milestone 4's `ReturnCall` eligibility check
is "same native return type + callee kinds ⊆ caller kinds". With Milestone
5's per-set types, "same return type" *is* "same kind set" — the structural
check subsumes the subset check, and different-set cores can never be
tail-forwarded into each other by accident.

---

## Milestone 4 — tail recursion with union-of-callees return kinds

### 4.0 Current shape

- `analyze_return_kinds` (`conv.rs` ~814) ends with a **blanket tail
  interlock**: if any block's terminator is `TTerm::Tail`, the whole kind
  set is discarded and replaced with `{Reference}` — every tail-carrying
  function gets the boxed ABI.
- `scan_return_kinds`' Tail arm (`conv.rs` ~882) is
  `kinds.insert(ValueKind::Reference)`, contributing nothing precise.
- `try_tail_dispatch` (`conv.rs` ~10125):
  - fresh callee with raw returns (`single() != Some(Reference)`) **bails
    to the generic adapter tail** (`Ok(None)`) — including multi callees;
  - fresh callee with `Reference` returns emits
    `ReturnCall { func: info.native }` directly;
  - non-fresh callees get the tag check whose fast arm emits `ReturnCall`
    (Reference callees) or downgrades to `direct_native_call` + box +
    `Return` (raw callees), slow arm keeps the generic adapter tail;
  - the generic fallback (`lower_call_parts` + `ReturnCallRef{adapter}`)
    is only type-correct from a boxed-ABI caller; multi/raw callers already
    downgrade to call + convert + `Return` (Milestone 3).
- Consequence today: a raw self-recursive core like
  `function f(n) { if (n <= 0) { return 0; } return f(n - 1); }` is pinned
  boxed. It still gets the O(1)-stack direct `ReturnCall` fast arm (boxed
  callee ABI == boxed caller ABI), but *pays a box per hop* at every
  return site and at the call boundary. And a *union* tail chain
  (`return pick(n - 1);` inside a multi-kind core) can never tail-dispatch:
  the pin forces the caller boxed, and the tail into the union-ABI callee
  downgrades to a framed call + unpack + repack.

### 4.1 Analysis: union the tail callees' kinds

Replace the blanket interlock with a per-callee union:

- Delete the `any(block.term is Tail) => kinds = {Reference}` reset in
  `analyze_return_kinds`.
- Rewrite `scan_return_kinds`' Tail arm: classify the tail callee with the
  same literal-resolution logic `classify_return_call` already uses
  (`TCallee::Val` pointing at an `Item::Func` literal reachable through
  `function_literal_locals` / self names / import tables — factor that
  resolution into one helper both call sites share):
  - **Provable callee**: recursively `analyze_return_kinds(callee_sfunc)`
    and insert **all** of its kinds (not `single()` — a tail edge
    contributes the callee's full set, since whatever the callee can
    return, the caller now returns).
  - **Unknown callee** (`LoadId` of an untracked name, `ReferenceKey`
    property read, `TCallee::Member`, spread args): insert
    `ValueKind::Reference`. Unknown ⇒ the callee may return anything ⇒ the
    boxed join is the only safe claim.
- Cycle safety is inherited from the existing analysis: the callee's
  `analyze_return_kinds` re-enters the in-progress placeholder (empty set)
  and inserts nothing for the back-edge. A self-recursive core's kinds are
  therefore computed from its non-tail returns alone — and that is exactly
  right, because the callee *is* the caller: whatever the tail forwards,
  the caller's own ABI already accepts.

### 4.2 Why this stays sound

The invariant chain, end to end:

1. **Tags never escape their set.** A union core packs only kinds from its
   analyzed set (Milestone 3's packing coercion rule; multi sets always
   contain `Reference`, so every off-set value coerces to the `REF` tag).
   So at runtime, every tag inside a core `X`'s union value ∈
   `analyze_return_kinds(X)`.
2. **Tail forwarding preserves that.** `ReturnCall { callee }` is emitted
   only when (a) the callee's native return type equals the caller's and
   (b) the callee's ReturnKinds ⊆ the caller's ReturnKinds. Condition (b)
   is what makes the forwarded tags a subset of what every downstream
   consumer of the caller's union branches on.
3. **Cycles under-approximate, never over-approximate.** The back-edge
   inserts nothing, so a cyclic tail chain computes the union of the
   *non-tail* returns around the cycle — an under-approximation of the true
   fixpoint. This is still sound because of (1) + (2): each core's runtime
   tags are confined to its own computed set, and forwarding only happens
   between sets where the callee's is contained in the caller's. Mismatched
   or unknown cases downgrade (below) and never forward raw.
4. **Every mismatch downgrades.** When the type check or subset check
   fails at the tail site, the existing machinery applies unchanged:
   direct call through `direct_native_call` (which unpacks a multi result
   per kind) + convert to the caller's ABI + `Return` — framed but
   correct. The generic `ReturnCallRef{adapter}` fallback remains reserved
   for boxed-ABI callers only.

Residual imprecision (cycles, untracked callees) costs frames or boxes,
never correctness.

### 4.3 Tail dispatch: ABI-matched `ReturnCall`

Extend `try_tail_dispatch`:

- **Fresh callees**: replace the `raw_returns => bail` rule with the
  general eligibility check. Let `caller_ty` be the native signature's
  declared return type and `callee_ty` the callee `info`'s:
  - `callee_ty == caller_ty && info.returns ⊆ current kinds` →
    `ReturnCall { func: info.native }` with the built arguments. Covers
    `f64 → f64`, `i32 → i32`, boxed → boxed, and union → union.
  - otherwise (raw/union callee, mismatched caller): `direct_native_call`
    (multi callees split into per-kind continuations) then per continuation
    convert to the caller's ABI (`box_value` / `as_f64` / `as_i32` /
    `pack_multi_return` via the same match the `Return` arm uses) +
    `Terminator::Return`. Framed; reuse the return-packing helper so the
    conversion logic lives in exactly one place.
- **Guarded (non-fresh) callees**: same eligibility check on the fast arm;
    the slow arm (unknown identity) keeps its current shape — generic
    adapter tail from a boxed caller, or the framed downgrade otherwise.
- The generic `ReturnCallRef{adapter}` path keeps its boxed-caller-only
  precondition; with the interlock gone, a boxed caller is now the case
  where the unioned kinds landed on exactly `{Reference}` (e.g. an unknown
  callee forced `Reference` in and nothing else appeared), plus explicit
  boxed sets. Multi/raw callers keep the Milestone 3 downgrade.

### 4.4 Expected outcomes

- Raw self-recursion stays raw *and* O(1): `return f(n - 1);` compiles to
  `ReturnCall` into the same `-> f64` native. No boxes anywhere on the
  recursion path.
- Union self-recursion forwards the packed union untouched:
  `function f(n) { if (n <= 0) { return null; } if (n === 1) { return 1.5; }
  return f(n - 2); }` — kinds `{Number, Reference}`, union ABI, tail hop is
  `ReturnCall` union → union. Zero unpacks inside the recursion.
- Mutual recursion: each tail edge whose callee set is provable unions
  precisely; edges between different-ABI cores downgrade per-hop (frames
  kept, values correct). True mutual O(1) requires identical sets on both
  sides (e.g. even/odd returning booleans: both sets `{Boolean}` → i32 →
  i32 `ReturnCall` — the existing depth-bounded mutual test can move its
  bound up).
- Unknown callees (`return mystery(n);` where `mystery` is untracked)
  behave exactly as today: `Reference` fallback, boxed or multi ABI,
  generic/dispatched lowering — no regression.

### 4.5 Implementation steps

1. Factor the provable-literal resolution out of `classify_return_call`
   into `tail_callee_kinds(root, value) -> Option<ReturnKinds>`; reuse it
   in the new Tail arm.
2. Delete the interlock; rewrite the Tail arm per §4.1.
3. Generalize `try_tail_dispatch`'s fresh and fast arms per §4.3; extract
   the "convert value to caller ABI + Return" sequence into a helper shared
   with the `TTerm::Return` arm.
4. Sweep the audit asserts: wherever code assumed "tail ⇒ boxed ABI"
   (comments, the inspection test that pins `ReturnCallRef` presence),
   update to the new rule set.
5. Tests (§4.6); full suite; commit.

### 4.6 Tests

- `raw_tail_recursion_stays_raw_and_constant_stack` —
  `function f(n) { if (n <= 0) { return 0; } return f(n - 1); }` at depth
  100000 expecting `0.0`; inspection: the f's native returns `F64` and
  contains `ReturnCall` to itself; no `StructNew` on the tail path.
- `union_tail_recursion_forwards_untouched` — the §4.4 union self-recursive
  core at depth 100000 (alternating `null` / `1.5` outcomes by parity);
  inspection: native returns the union type; the tail block's terminator is
  `ReturnCall` to the same native; the body contains no `CondBr` tag chain
  (no unpack on the recursion path).
- `mutual_tail_recursion_matching_sets_dispatches_directly` — boolean
  even/odd, both sets `{Boolean}`; depth raised well past the old
  adapter-path bound (e.g. 20000) to observe the O(1) hops; inspection:
  at least one direction emits `ReturnCall` with an `I32` native.
- `unknown_tail_callee_falls_back_to_reference` — tail into an untracked
  name (e.g. through a parameter of the enclosing function); execution
  correct, and the caller's ABI is boxed or multi (never raw).
- `mismatched_tail_abis_downgrade_and_stay_correct` — caller
  `{Number}`-only core tail-calling a `{Number, Reference}` callee:
  executes correctly on both arms; inspection shows a framed
  call + pack + `Return` (no `ReturnCall`).
- Full e2e suite green, including the Milestone 3 test
  `tail_call_out_of_multi_kind_caller_downgrades_to_call_and_pack`, which
  after this milestone should flip to direct forwarding (update that test's
  comment/assert to the new expectation rather than deleting it).

---

## Milestone 5 — dedicated multi-value return types, union forwarding, known-call splitting

### 5.0 Current shape

Every multi-kind core returns the single fat union
`$multi_ret { tag: i32, r: anyref, i: i32, f: f64 }`
(`Repr::multi`, `repr.rs` ~54), regardless of which kinds it can produce.
A `{Boolean, Number}` core allocates a struct whose reference slot is
always dead; `{Number, Reference}` cores carry a dead i32 slot. Packing
zero-fills dead slots on every return; the shape of the union is also the
*only* thing making union → union `ReturnCall` legal between cores with
*different* kind sets (same fat type) — which Milestone 4's subset check
currently has to guard against explicitly.

### 5.1 Representation: per-slot-set dedicated types

There are only three payload-slot groups (boxed `r`, i32-slot `i`, f64
`f`); multi sets use two or three of them. That is exactly **four**
distinct multi layouts. Mint all of them eagerly in `Repr::new` (no
backrefs — each references only `anyref`/primitives):

- `$multi_rf { tag: i32, r: anyref, f: f64 }` — sets `{Number, Reference}`
- `$multi_ri { tag: i32, r: anyref, i: i32 }` — sets with `Reference` +
  Boolean/Integer
- `$multi_if { tag: i32, i: i32, f: f64 }` — sets with Number +
  Boolean/Integer (e.g. `{Boolean, Number}` — "booleans and floats return
  just the values needed")
- `$multi_rif { tag: i32, r: anyref, i: i32, f: f64 }` — all three groups
  (today's fat type, kept as-is)

Tag *values* stay the global constants (`0=REF, 1=BOOL, 2=INT, 3=F64`) —
every core interprets tags identically, only the slot layout varies.
`ReturnKinds` grows `slot_groups()` / `multi_type(&Repr) -> Signature`
mapping a kind set to its layout (memoization unnecessary: four eager
types, a pure match).

Two properties fall out for free:

- **Slot elision**: a core's returns touch only slots in its layout — no
  dead fields, no wasted zero-fills.
- **Structural ABI identity**: equal kind sets ⇒ equal struct type ⇒
  Milestone 4's "same native return type" check *is* the subset check.
  Different-set cores get different types, so a bad union → union
  `ReturnCall` is now a validation error caught at compile time instead of
  a runtime trap guarded only by analysis. (Keep the explicit subset check
  anyway — it makes the invariant local and documents intent.)

### 5.2 Emission sites that learn the layout

All union sites switch from `repr.multi` to `kinds.multi_type(&repr)`,
threading the relevant `ReturnKinds` (already available at every site):

- `ReturnKinds::native_return_type` — the ABI decision.
- `pack_multi_return` — emit `StructNew` with only the layout's fields
  (tag + live slots; no dead-slot defaults).
- `unpack_multi_return` — `CondBr` chain over the set's kinds, each arm
  `StructGet` at its layout's field index (per-type index tables: derive
  from the kind set at emission time, keep `MULTI_FIELD_*` as rif-only
  constants or replace with a layout fn).
- `make_adapter`'s multi prologue and `multi_kinds_of` — the producer's
  set picks the type it unpacks.
- `direct_native_call` / guarded fast arms / tail downgrades — callee's
  set picks the type they unpack; the caller's set picks the repack type.

### 5.3 Union forwarding through tail chains

The dominant chain form `function middle(w) { return pick(w); }` is a
`TTerm::Tail` in TAC — so with Milestone 4, forwarding is *already* the
`ReturnCall` union → union fast arm: the packed union flows through
`middle`'s frame untouched, and only the topmost consumer splits. After
Milestone 5 this works between any same-set cores (same dedicated type).
Milestone 5 adds the inspection tests and makes the downgrade path
(different sets) do unpack-with-callee-layout + repack-with-caller-layout.

**Non-tail forwarding stays split + repack.** `let y = pick(w); return y;`
flows the call result through the statement/values machinery, which erases
`LowerValue` provenance (Milestone 3's finding); a raw union marker cannot
safely cross that boundary, and a boxed-opaque-union escape would corrupt
JS-level value identity. One tag check + one repack per hop is the accepted
cost. (If it ever shows in profiles, the fix is a
`LowerValue::Union { value, kinds }` marker *proven not to cross a
statement boundary* — a stretch goal, not in this milestone.)

### 5.4 Continuation splitting on known calls to those cores

Already landed in Milestone 3 (`direct_native_call` →
`unpack_multi_return` → one continuation per analyzed kind; composes with
the static fast/slow splits). Milestone 5 keeps the behavior and updates
the mechanics:

- each split site reads the *callee's* dedicated type and field indices;
- the per-kind continuations stay raw-payload (`LowerValue::Wasm` with the
  exact kind), so downstream consumers keep zero re-boxing;
- the universal adapter's multi prologue unpacks the producer's layout
  before boxing, so generic `CallRef` sites are unchanged;
- the primordial guarded split (tag check → fast/slow arms) composes: each
  arm's multi callee unpacks with that arm's known set.

### 5.5 Implementation steps

1. `Repr`: mint the four dedicated types; add `ReturnKinds::multi_type`.
   Keep `Repr::multi` as the rif alias or remove it (sweep all uses).
2. Thread `ReturnKinds` into `pack_multi_return` / `unpack_multi_return` /
   the adapter prologue; replace `repr.multi` at every site (compiler
   guides — all sites were enumerated in §5.2).
3. Per-type field-index resolution (layout fn keyed by kind set) replacing
   the fixed `MULTI_FIELD_*` constants where the set isn't rif.
4. Update the Milestone 4 subset check comment: with dedicated types, the
   type identity and the subset check agree; keep both.
5. Tests (§5.6); full suite; commit.

### 5.6 Tests

- `boolean_float_core_returns_dedicated_layout` —
  `let f = function(n) { if (n === 0) { return false; } return n * 1.5; };`
  inspection: its native's return struct has exactly 3 fields
  `(i32, i32, f64)` and no `anyref` field; execution correct on both arms.
- `ref_float_core_returns_dedicated_layout` — the `{Number, Reference}`
  pick core from Milestone 3 now returns `(i32, anyref, f64)`; the
  Milestone 3 union inspection test updates to assert the 3-field layout.
- `forwarding_chain_has_no_interior_unpacks` —
  `a → b → c → export`, all same set, each link `return next(x);`:
  inspection: `a` and `b` bodies contain `ReturnCall` and **no** `CondBr`
  tag chains; only the export's adapter prologue unpacks; depth-100000
  execution for O(1).
- `different_set_cores_repack_across_the_boundary` — `{Boolean, Number}`
  core returning into a `{Number, Reference}` core: correct values both
  arms; inspection: the boundary emits unpack + `StructNew` of the
  caller's layout (no `ReturnCall`).
- `splitting_stays_raw_on_known_calls` — a known multi callee consumed
  arithmetically: per-kind continuations consume raw payloads (existing
  Milestone 3 behavior preserved; update its union-type inspection to the
  dedicated layout).
- `singleton_regression_no_union_in_all_single_module` — unchanged
  (checks function return types, not type existence; the four eager types
  may exist unused in an all-single module, which that test already
  tolerates).
- Full e2e suite green; no existing test may regress its *execution*
  expectations, only its ABI/inspection asserts may tighten.

---

## Non-goals

- Packed variant-per-tag unions (list-of-alternatives encoding); struct +
  tag stays the representation.
- Unboxed *parameter* ABIs — arguments remain boxed formals.
- Non-tail raw union forwarding across statement boundaries
  (`LowerValue::Union` marker) — split + repack remains; revisit only on
  profile evidence.
- Making tag values per-type (they stay global so every core's unpack
  logic is one implementation).
- Fixed-point convergence for cyclic tail-kind analysis — under-
  approximation + ABI checks are sound and single-pass (§4.2).
- O(1) guarantees for mutual recursion through *unknown* callees — those
  keep framed downgrades by design.

## Sequencing

1. **Milestone 4** (self-contained in `conv.rs`): literal-resolution
   helper → Tail arm → `try_tail_dispatch` generalization → tests →
   commit. Milestone 3's downgrade test flips to forwarding as part of
   this.
2. **Milestone 5** (repr + all union emission sites): four eager types →
   layout threading → inspection updates → tests → commit. Depends on 4
   only for the forwarding tests; could be built first, but the dedicated
   types make 4's eligibility check structural, so 4-then-5 minimizes
   churn.

## Risks

- **Analysis interlock removal** touches every tail-carrying function's
  ABI decision. Mitigation: the soundness argument is per-site and local
  (§4.2); the full suite's inspection tests pin ABI shapes; any surprise
  can be re-tightened by re-adding the interlock for the offending shape
  without unwinding the dispatch work.
- **Field-index bugs** in per-type pack/unpack would misread payloads.
  Mitigation: layout fn is total over the four types and unit-covered by
  the dedicated-layout tests on both execution and inspection axes;
  WasmGC validation catches out-of-range indices at compile time.
- **Wasmtime/Node parity** on `return_call` with struct returns — both
  runtimes already execute struct-returning calls in the suite; the
  harness compares f64 results in both engines as before.
