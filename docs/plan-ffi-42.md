# Plan: `~ffi` follow-up factories (DreamComp #42)

**Status:** implemented (jsaw `master`). All five scope items land:
`callback` (constructor + `js_ffi_cb_<ordinal>` reverse-boundary export
adapter, wasmtime host round-trip e2e), `contains` (marker value; fields
behave as ordinary own properties per the jsaw-native reading), template
splices (alias-chasing structural resolution; factory results carry an own
`toString` yielding the same token the compile-time resolution substitutes),
non-`extern` structs (plain-object constructors), and method-level `P<…>`
results (the PIT rid→method-table catalog). Corpus cases live in
`jsaw_corpus_ext.json` (merged after the fingerprint-pinned vendored
corpus) and pass all low-level lanes; e2e coverage is in
`portal-jsc-waffle/tests/e2e.rs`.

Two latent bugs surfaced and were fixed: the JVM emitter's split-function
path never populated `broken_to` (a block/if whose body exits through its
own label was misclassified as sequence-terminating, truncating live code —
fixed in `portal-jsc-jvm-emit`), and `ffi_convert_out` wrapped non-`P<>` thunk
results in a method-table object (fixed — wrap only `P<…>` results). The
MiniBC backend gained const rematerialization (register-file pressure from
per-byte string locals) and spill-slot save/restore across calls; a
remat/spill scratch-id collision was found and fixed by allocating spill
scratches from the complement of remat-assigned ones.

Follow-up to #37. The `~ffi` virtual module currently supports `ffi`,
`struct` (extern-only), and `opaque`; `callback`, `contains`,
template-literal splices, non-`extern` structs, and method-level `P<…>`
results fail closed. This issue implements them.

## jsaw-native readings of the DreamComp semantics

DreamComp's spec (`docs/public/dreamcompat-api.md`) is written against its
NaN-box/CPS machinery. jsaw's value model differs (every JS value is one
ref word; there is no host-visible raw memory), so several spec constructs
have a *simpler* jsaw-native reading that is behaviorally identical from
JS:

- **`contains(value)`**: DreamComp embeds "N raw NaN-box slots, N resolved
  at CPS time" by value into a struct field. In jsaw every value's
  representation is exactly one word, so by-value embedding *is* an
  ordinary own property. `contains(v)` evaluates to a marker object
  (identity + the source value for diagnostics); a struct field whose type
  is a `contains` splice holds the value as an ordinary field.
- **Template-literal splices** (`struct(\`extern key: ${Bytes}, kind: u32\`)`):
  the sig must resolve at compile time. The prescan/factory accepts a
  template literal when every splice expression resolves — per the spec's
  same-function-body, executed-earlier rules — to a tracked factory result
  (a `struct`/`callback`/`opaque`/`contains` value). The field becomes an
  ordinary struct field; the splice's type only affects validation (the
  spliced value must be a factory result) and, for `callback` splices in
  `ffi(...)` signatures, the boundary ABI (see below).
- **Non-`extern` structs**: DreamComp computes and pins a host layout.
  jsaw never shares raw memory with the host, so a non-`extern` struct is
  the same plain-object constructor as an `extern` one; the `extern`
  keyword becomes a documentation marker with no behavioral difference.
- **Method-level `P<…>` results**: the prescan builds a catalog
  `rid → method table` from every `ffi(sym, sig)` whose top-level result is
  `P<rid>`; a method whose result is `P<rid2>` wraps with rid2's table.
  Fails closed when rid2 has no registered table.

## `callback(sig)` — the reverse boundary

`callback('(f64) -> f64')` returns a constructor; `C(handler)` binds the
JS function and produces the callback *value*:

- The value crossing to the host is a **wrapper object** (a `ref` at the
  boundary) carrying two hidden slots: `\u{1}ffi_callback` (the callback
  declaration identity) and the bound handler function.
- For the host to call the callback, jsaw emits an **exported adapter** per
  callback declaration, `js_ffi_cb_<ordinal>`, with boundary signature
  `(ref wrapper, args…) -> result`: it loads the handler from the wrapper,
  converts arguments per the sig, calls the handler through the ordinary
  function-call path, and converts the result back. Arity mismatch is a
  compile-time error (the spec requires an exact match).
- A host that wants a bare function pointer stores the wrapper and invokes
  the export. (DreamComp's native backends emit a real function pointer;
  the wasm/low-level model's equivalent is handle-plus-export.)
- **Exceptions**: a non-`Return` exit from the handler traps the adapter
  (jsaw matches DreamComp's first cut: no cross-boundary propagation).

`callback` values may also be *spliced* into `ffi(...)` signatures as an
argument type: the host import then receives the wrapper `ref`.

## Corpus families

The parity harness has no host-import provision (proven fail-closed by
`low_exec_isolation`), so corpus cases exercise the host-free surface:
factory construction, identity/typeof behavior, constructor arity checks,
and splice/contains field plumbing — in both lanes. Real host round-trips
(including a host actually *calling* a callback through the export) are
covered by wasmtime e2e tests in `portal-jsc-waffle`.

## Acceptance mapping

1. `callback`/`contains` through `~ffi` with dreamcomp-compatible sigs →
   the prescan accepts the same literal grammar; boundary ABI documented
   above.
2. Corpus families for both, both lanes → construction-level cases in
   `jsaw_corpus.json` + wasmtime e2e for the host round-trip.
3. Docs → this file, `jsaw-corpus-bridge.md` if the corpus grows, and the
   factory header comment.
