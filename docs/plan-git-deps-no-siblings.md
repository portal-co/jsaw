# Plan: replace sibling path-patches with Git deps, recursively

Follow-up to `plan-test262-compatibility.md` (its "Workspace/pipeline coupling"
risk). Goal: make `jsaw-test262` (and every workspace in the portal-co tree)
buildable from a fresh clone with **no sibling checkouts present**, by
consuming `jsaw-core` and `waffle-` as plain Git dependencies, and moving any
local-development override into a `.cargo/config.toml` patch (checked in or
local-only) so external users never need the sibling repos.

---

## Current state (audited 2026-09-05)

### Dependency graph of the workspace

`jsaw-test262` consumes via **Git** today (already external-safe):

- `codegen-utils.git` (`master`, `dev/0.3` branches) — via `ssa-traits`,
  `cfg-traits`, `ssa-impls`, `ssa-reloop` (+ `0.3.0-alpha.2` aliases)
- `llvm-codegen-utils.git`, `asm-common.git` / `asm-arch.git` (rev-tagged
  versions), `rayoff.git`, `swibb.git`

`jsaw-test262` consumes via **local path-patch** (NOT external-safe):

```toml
# jsaw-test262/Cargo.toml (bottom of file)
[patch."https://github.com/portal-co/jsaw-core.git"]   # 6 crates → ../jsaw-core/crates/*
[patch."https://github.com/portal-co/waffle-.git"]     # portal-pc-waffle → ../waffle-
```

Without these patches, the manifests reference
`git = "https://github.com/portal-co/jsaw-core.git"` (default branch) and
`git = "https://github.com/portal-co/waffle-.git"` with version specs
(`^0.8.0-pre.11`, `^0.6.0-alpha.1`) — so de-patching is correct the moment
upstream contains the needed commits.

### Upstream state of the two patched repos (the actual blocker)

**`jsaw-core`** (branch `main`) is **13 commits ahead of `origin/main`** and
none are pushed:

- `95bab25` incremental CfgModuleBuilder + with-clause on re-exports
- `08f3360` fix ternary real-branch merge dropping else-arm value
- `783da5b` land terminator on post-expr block in CFG→TAC
- `0f9e42f` fix closure-captured variables incorrectly SSA-promoted
- `52e0a6c` wire trap_catch into new_block + catch-param decl tracking
- `1304954` extend Primordial enum with Array + Math/Object/Reflect surface
- `726f005` fix infinite loop in bind_array_contents for empty array patterns
- `dd2253c` wire up Pat::Array in bind()'s dispatch
- `de6295e` register uninitialized let/var bindings in decls (phi bug)
- `2b3c013` add Expr::Update (i++/++i/i--/--i) support in swc-tac
- `00cc843` add Item::Tpl for template literals
- `b73836e` erase TS type-assertion expressions in TAC conversion
- `4292809` swc-ssa: SSA-track reassigned function parameters (loop-hang fix)

**`waffle-`** (branch `portal`) is **2 commits ahead of `origin/portal`**:

- `bb126c5` fix treeify inlining values across block boundaries — **this is
  the exact fix the "Development-only" comment in `jsaw-test262/Cargo.toml`
  cites as required by e2e test
  `call_then_prop_read_combined_in_one_expression`**
- `b5cd16c` repair 0.2 SSA trait terminator adapter

Additionally `waffle-` has **uncommitted working-tree changes** that are
*required for external consumption*:

- `Cargo.toml`: `wax-meta` source moved `wax.git` → `portal-container.git`
  (matches the portal-container ownership note in portal-hot's config)
- untracked `tests/ssa_traits_02.rs` (referenced by a new `[[test]]` stanza
  in that same Cargo.toml diff — must be committed together or the
  `required-features` build breaks)

### Verification baseline

- `cargo check --workspace` in `jsaw-test262` currently passes in ~18s
  (warm; with the patches active).
- `Cargo.lock` **is committed** and is the intended rev-pinning mechanism for
  git deps — keep committing it after every resolution change.

---

## Design decisions

1. **Do not pin `rev` in `Cargo.toml`.** Git-dep rev pinning belongs in
   `Cargo.lock` (already committed). Manifests keep `branch`/version specs so
   `cargo update -p <pkg>` remains the upgrade path.
2. **De-patch in manifest, re-patch in config.** The workspace manifest gets
   *zero* `[patch]` sections; local development against sibling checkouts is
   restored by a `.cargo/config.toml` `[patch]` layer — either repo-committed
   with a documented "requires sibling checkouts" contract, or kept
   local-only (see Phase 2.3).
3. **Recursion stops at upstream push, not at config.** "Recursively safe"
   means every repo in the chain must be buildable from Git alone. That
   imposes the ordering: push `waffle-` first (it depends only on
   `wax.git`/`portal-container.git`/`codegen-utils.git`), then `jsaw-core`
   (depends on `codegen-utils.git` only), then de-patch `jsaw-test262`.
   `jsaw-core` has no `[patch]` sections and no outside-sibling path deps, so
   it is git-clean once pushed; `waffle-` becomes git-clean once the wax-meta
   edit is committed.
4. **Verify with a sandbox consumer.** De-patching is only proven done when a
   *separate* throwaway workspace, depending on both repos purely via Git
   with a fresh `CARGO_HOME`, resolves and builds. Local sibling presence
   must not affect the sandbox (use a temp dir outside `portal-hot/`, since
   `.cargo/config.toml` discovery walks upward).

---

## Phase 1 — Upstream the local-only commits (blocking)

### 1.1 `waffle-` (branch `portal`)

1. Decide on the untracked `tests/ssa_traits_02.rs` + the matching `[[test]]`
   stanza: commit both together (they are one logical change) or drop both.
2. Commit the `wax-meta` → `portal-container.git` change separately with a
   message explaining it removes the dependency on the portal-hot local
   patch, enabling external Git consumption.
3. Push `portal` to origin; note the new head rev for `Cargo.lock` refresh.
4. Sanity-check: fresh clone of `waffle-` + `cargo check --workspace` with
   `CARGO_HOME` pointed at a temp dir (proves no hidden sibling deps).

### 1.2 `jsaw-core` (branch `main`)

1. Review the 13 commits; they are logically independent fixes/features —
   push as-is to `main` (repo convention: direct `[AI]` commits on `main`)
   or batch into a PR if that's the team norm.
2. After push, verify `git ls-remote origin main` contains `95bab25`'s tree.
3. Sanity-check like 1.1.4: fresh clone + `cargo check --workspace`.

## Phase 2 — De-patch `jsaw-test262`

1. **Remove** from `crates/../Cargo.toml` (workspace root):
   - `[patch."https://github.com/portal-co/jsaw-core.git"]` + its 6 entries
   - `[patch."https://github.com/portal-co/waffle-.git"]` + its 1 entry
   - both "Development-only" comment blocks.
2. Confirm the workspace dependency specs still name Git + compatible
   versions (`^0.8.0-pre.11`, `^0.6.0-alpha.1`). Since `jsaw-core` workspace
   version is `0.8.0-pre.11` and the new commits are compatible fixes,
   *consider* bumping `jsaw-core`'s `[workspace.package] version` to
   `0.8.0-pre.12` in the same push (Phase 1.2) and updating the spec here to
   `^0.8.0-pre.12` so the version number documents "needs the new commits".
   Optional but recommended — the caret spec alone cannot express "not the
   published pre.11".
3. **Local-dev override file.** Create `jsaw-test262/.cargo/config.toml`:
   ```toml
   # Local development override: build against sibling checkouts instead of
   # the pinned Git revs. SAFE TO DELETE — CI and fresh clones must build
   # without this file (do not commit it; or commit it only if the team
   # accepts that a missing sibling breaks local builds).
   [patch."https://github.com/portal-co/jsaw-core.git"]
   portal-jsc-common      = { path = "../../jsaw-core/crates/portal-jsc-common" }
   portal-jsc-swc-cfg     = { path = "../../jsaw-core/crates/swc-cfg" }
   portal-jsc-swc-tac     = { path = "../../jsaw-core/crates/swc-tac" }
   portal-jsc-swc-ssa     = { path = "../../jsaw-core/crates/swc-ssa" }
   portal-jsc-swc-opt-ssa = { path = "../../jsaw-core/crates/swc-opt-ssa" }
   portal-jsc-swc-util    = { path = "../../jsaw-core/crates/swc-util" }

   [patch."https://github.com/portal-co/waffle-.git"]
   portal-pc-waffle = { path = "../../waffle-" }
   ```
   and add `.cargo/config.toml` handling to `.gitignore` (or commit a
   `.cargo/config.toml.example`). Choose "not committed" — the sandbox test
   in Phase 3 and CI must both be proven with the file absent, and committing
   it would defeat that. **Caveat:** this config file will be picked up by
   any workspace nested *under* `jsaw-test262/` too — acceptable; the
   reverse problem (portal-hot's config leaking into this workspace) already
   exists and is worse.
   - **Path correction note**: patches inside `.cargo/config.toml` resolve
     relative to the *config file's directory's parent* (i.e. the workspace
     root), so `../../jsaw-core` is wrong — use `../jsaw-core/...` exactly as
     the old workspace-manifest patches did. Cargo resolves relative paths in
     config-file patches relative to the directory containing the
     `.cargo` directory's parent... **Action: verify empirically in Phase 3**
     with `cargo tree -p portal-jsc-common` / a deliberate build break to
     confirm which spelling Cargo accepts; start with `../jsaw-core/...`.
4. `cargo update -p portal-jsc-common` (and siblings) — or simply
   `cargo check` and let resolution re-lock against origin. Commit the
   refreshed `Cargo.lock` with the de-patch commit.

## Phase 3 — Recursive external-consumption verification

1. **Sandbox consumer workspace** (in `/tmp`, deliberately outside
   `portal-hot/` so no upward `.cargo/config.toml` applies):
   ```toml
   # /tmp/consumer/Cargo.toml
   [workspace]
   members = ["consumer"]
   resolver = "2"

   [workspace.dependencies]
   portal-jsc-common = { version = "^0.8.0-pre.11", git = "https://github.com/portal-co/jsaw-core.git" }
   portal-pc-waffle  = { version = "^0.6.0-alpha.1", git = "https://github.com/portal-co/waffle-.git", features = ["backend"] }

   [patch."https://github.com/portal-co/codegen-utils.git"]  # only if dev/0.3 branch work is unpushed
   ```
   with `consumer/Cargo.toml` depending on `portal-jsc-common` +
   `portal-pc-waffle` and a `main.rs` touching their APIs. Run with
   `CARGO_HOME=/tmp/consumer-cargo-home cargo check` to force true Git
   fetches (no sibling checkouts, no inherited config). **This is the
   pass/fail gate for the whole plan.**
2. **De-patched workspace, sandboxed**: `cd jsaw-test262 && mv .cargo
   /tmp/held-config && CARGO_HOME=/tmp/... cargo check --workspace && cargo
   test -p portal-jsc-waffle` (e2e tests hit Node — needs node on PATH; fine
   on CI). Restore `.cargo` afterwards. Proves `jsaw-test262` itself is now
   sibling-free buildable.
3. **Recursion check for other portal-hot workspaces** (out of scope to fix
   here, but audit): portal-hot's own `.cargo/config.toml` patches many repos
   — the fix pattern from Phase 2.3 (manifest git deps + optional local
   config patch) should be replicated repo-by-repo; file follow-ups rather
   than expanding this plan.

## Phase 4 — CI implications (updates `plan-test262-compatibility.md`)

Replace that plan's Phase 3 risk bullet ("the root Cargo.toml currently
patches jsaw-core and waffle- to local checkouts…") with:

- After this plan lands, CI needs **no sibling checkouts** — plain
  `actions/checkout` + Rust toolchain suffices; the "resolve patch sections
  for CI" risk is retired.
- Until it lands (and if the local `.cargo/config.toml` approach is chosen),
  CI must either (a) checkout jsaw-core + waffle- at the pinned revs next to
  the workspace, or (b) run with the local config file absent — (b) is only
  possible after Phase 1+2.
- Add a CI guard job: build the sandbox consumer of Phase 3.1 (or run it as
   a dedicated workflow step) so a future accidental re-introduction of a
   sibling path-patch into the manifest fails CI immediately.

## Risks / open questions

- **Upstream acceptance**: 13 jsaw-core commits + 2 waffle- commits must land
  before anything else. If review demands changes, `jsaw-test262` stays on
  path-patches (or on a temporary branch-pinned git dep, e.g.
  `branch = "ai/local-fixes"`, as an intermediate state that is still
  externally safe).
- **Version pin semantics**: `^0.8.0-pre.11` cannot distinguish the published
  pre.11 from the improved one; the optional `0.8.0-pre.12` bump (Phase 2.2)
  is the only honest encoding. Without it, a stale `Cargo.lock`-less build
  could silently resolve the old upstream.
- **`.cargo/config.toml` leak direction**: portal-hot's config patches
  *everything* below it — including this workspace — so locally the git deps
  may be shadowed by portal-hot's own patches for the same URLs. That's
  benign (same sources, local paths), but must be remembered when
  interpreting local vs CI resolution differences.
- **codegen-utils `dev/0.3`**: verify the `dev/0.3` branch is actually pushed
  (`git ls-remote https://github.com/portal-co/codegen-utils dev/0.3`) during
  Phase 1; the `0.3.0-alpha.2` aliases depend on it.
- **Intermediate branch-dep state**: if pushing to `main` takes time, pin
  `jsaw-test262`'s deps to a pushed feature branch temporarily. This keeps
  external safety while review completes; remove when upstream merges.

## Milestone order

1. Push `waffle-` `portal` (incl. wax-meta commit); verify fresh-clone build.
2. Push `jsaw-core` `main` (13 commits; optional pre.12 version bump).
3. De-patch `jsaw-test262/Cargo.toml`, refresh + commit `Cargo.lock`.
4. Sandbox consumer + de-patched-workspace verification (Phase 3).
5. Update CI plan / add sandbox-consumer CI guard.
6. File follow-ups for the remaining portal-hot workspaces using the same
   pattern.
