# Task 10.1: `package` Command

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 9 (dispatcher scaffolding)  
**PR:** Part of Task 10 mega-PR (individual commit)  
**Commit:** `feat(cmd): package command + cli emit helper + golden`

## Goal
Implement the `hypha package <PKG>` command showing metadata scoped to the build plan.

## Files to Create
- `src/Hypha/Command/Package.hs`

## Files to Modify
- `src/Hypha/Cli/Run.hs` — add `CmdPackage` arm using `withPlan` helper, add `emit` if not present
- `test/Golden/Commands.hs` (create if not exists; add `package-async` golden)
- `test/Golden/golden/package-async.compact.json`
- `hypha.cabal` — expose `Hypha.Command.Package`
- `test/Main.hs`

## Acceptance Criteria
- [ ] `runPackage` returns `Outcome Value` with name, version, in_plan, is_local, deps_count
- [ ] Golden test `package-async` passes
- [ ] Dispatcher rejects packages not in plan
