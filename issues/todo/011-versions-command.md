# Task 10.2: `versions` Command

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 10.1  
**Commit:** `feat(cmd): versions command + withPlan helper + golden`

## Goal
Implement `hypha versions <PKG>` showing pinned version from plan (available versions deferred to Task 13).

## Files to Create
- `src/Hypha/Command/Versions.hs`

## Files to Modify
- `src/Hypha/Cli/Run.hs` — refactor `CmdSearch`/`CmdPackage` to use `withPlan` helper
- `test/Golden/Commands.hs` — add `versions-async` golden
- `test/Golden/golden/versions-async.compact.json`
- `hypha.cabal` — expose `Hypha.Command.Versions`
- `test/Main.hs`

## Acceptance Criteria
- [ ] `withPlan` helper abstracts plan-root discovery + error handling
- [ ] `runVersions` returns pinned version from build plan
- [ ] Golden test passes
