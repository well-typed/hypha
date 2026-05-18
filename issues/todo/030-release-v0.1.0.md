# Task 30: v0.1.0 Release

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 29  
**PR:** One PR  
**Commit:** `release: v0.1.0`

## Goal
Bump version to 0.1.0, run full test suite, tag release.

## Files to Modify
- `hypha.cabal` — `version: 0.1.0`
- `src/Hypha/Prelude.hs` — `version = "0.1.0"`
- `CHANGELOG.md` — replace "TBD" with release date

## Steps
1. Bump version in `hypha.cabal` and `src/Hypha/Prelude.hs`
2. Update CHANGELOG date
3. Run `cabal build all && cabal test`
4. Run `cabal run hypha -- doctor` and `cabal run hypha -- --human search Map.insert`
5. Commit: `release: v0.1.0`
6. Tag: `git tag -a v0.1.0 -m "hypha v0.1.0 — initial release"`
7. (Optional) `cabal sdist` for Hackage candidate

## Acceptance Criteria
- [ ] Version is 0.1.0 in cabal, Prelude, and CHANGELOG
- [ ] Full test suite passes
- [ ] `doctor` reports working environment
- [ ] `search --human` produces coloured output
- [ ] Git tag `v0.1.0` created
- [ ] No push without user approval
