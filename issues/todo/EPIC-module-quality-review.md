# EPIC: Module-by-module quality & vision-consistency review

**Status:** todo
**Type:** epic
**Owner:** team-lead

## Goal

Review **every** Haskell module in `hypha` for general quality and consistency
with the project's overall vision (the master spec at
`docs/superpowers/specs/2026-05-18-hypha-design.md` and the ethos in `CLAUDE.md`).
Each module gets a verdict and, where needed, a remediation ticket.

This is the read/assess pass plus targeted fixes — NOT a rewrite. Prefer many
small, surgical tickets over sweeping refactors. Anything large enough to be its
own design effort gets logged as a follow-up epic, not done inline.

## Scope

All modules under `src/Hypha/**` and `app/**` (~70 modules + 2 executables).
The lead slices these into tickets **by subsystem** so they parallelise cleanly
and rarely touch the same files:

- `BuildEnv/*`
- `Cabal/*` + top-level `Cache.hs`
- `Cli/*`
- `Command/*`
- `Hackage/*`
- `Haddock/*`
- `Hoogle/*`
- `Mcp/*`
- `Output/*`
- `Package/*` + `Project/*`
- `Search/*`
- `Server/*` (largest — may split into `Server/Ui/*` vs the rest)
- `Source/*`
- `Types/*`
- top-level: `Error.hs`, `Exit.hs`, `Logging.hs`, `Prelude.hs`

One subsystem → one ticket → one senior dev → one worktree slot. The lead assigns
no more concurrent tickets than there are worktree slots in the pool.

## Review Rubric (per module)

Each module is graded against the `CLAUDE.md` "Well-Typed Ethos" + Ousterhout:

1. **Impossible states unrepresentable** — no `error "unreachable"`; refine types.
2. **Types over strings** — domain values are newtypes/sum types, not `Text`.
3. **`mtl`/`transformers` over zig-zag `case` cascades** on `Either`/`Maybe`.
4. **No duplication** — shared bodies factored into helpers.
5. **Errors first-class** — funnel through `Hypha.Error.HyphaError`, no re-stringifying.
6. **Render at the edge** — domain types carried as themselves; stringify only at the wire.
7. **Never round-trip own output** — no decoding bytes we just encoded.
8. **No silent error swallowing** — no `Left _ -> fallback` that hides the cause.
9. **Strict bangs** on strict fields; lazy fields justified by a comment.
10. **Deep modules / information hiding** — simple interface, hidden internals, no leakage.
11. **Interface comments present** and accurate.
12. **Consistency with the vision** — does this module still serve the agent-first,
    compact-JSON, build-plan-scoped design? Flag drift.

## Per-module deliverable

For each module the dev produces a short verdict line: `PASS` (meets the bar) or
`FIX` + the specific rubric items violated, with file:line. `FIX` modules get the
remediation applied in the same ticket where it is small; larger ones are logged
as new tickets in `issues/todo/`.

## Acceptance Criteria (per subsystem ticket)

- [ ] Every module in the subsystem has a recorded PASS/FIX verdict against the rubric.
- [ ] All small fixes applied; `cabal build all && cabal test all` green.
- [ ] Larger remediations logged as separate `issues/todo/` tickets, not left implicit.
- [ ] No behavioral change beyond what the verdict justifies (QA confirms no regressions).
- [ ] Conventional-commit history, one feature branch per subsystem.

## Done when

- Every subsystem ticket is in `issues/done/`.
- A closing artifact `docs/closing/module-quality-review.md` summarises the
  per-subsystem verdicts, fixes applied, follow-up tickets logged, and verify evidence.
