# Task 12: `doctor` Command

**Status:** todo  
**Priority:** P2  
**PR:** One PR  
**Commit:** `feat(cmd): doctor — checks ghc, haddock, plan.json presence`

## Goal
Implement `hypha doctor` diagnosing the environment: checks for GHC, Haddock, and plan.json.

## Files to Create
- `src/Hypha/Command/Doctor.hs`
- `test/Unit/Doctor.hs`

## Files to Modify
- `src/Hypha/Cli/Run.hs` — wire `CmdDoctor`
- `hypha.cabal` — add `typed-process`; expose `Hypha.Command.Doctor`
- `test/Main.hs`

## Acceptance Criteria
- [ ] Checks `ghc` on PATH
- [ ] Checks `haddock` on PATH
- [ ] Checks `dist-newstyle/cache/plan.json` exists
- [ ] Returns JSON envelope with check statuses (pass/warn/fail)
- [ ] Unit test verifies `all_pass=false` for missing plan
