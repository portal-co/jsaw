# Domain Docs

## Layout

This is a single-context repository:

- `CONTEXT.md` at the repository root
- `docs/adr/` for architecture decision records

## Consumption rules

Before exploring code:

- Read root `CONTEXT.md` when it exists.
- Read ADRs in `docs/adr/` that touch the area being changed.
- Proceed silently when these files do not exist.
- Use terminology from `CONTEXT.md` in issue titles, proposals, tests, and refactors.
- If a proposed change conflicts with an ADR, call out the conflict explicitly rather than silently overriding it.
