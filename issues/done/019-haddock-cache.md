# Task 13: Haddock.Generate — Lazy On-Demand Build Pipeline + Cache Layout

**Status:** todo  
**Priority:** P2  
**Blocked by:** Task 12 (doctor checks)  
**PR:** One PR  
**Commit:** `feat(haddock): on-demand cache layout + ensureHaddockFor + doctor check`

## Goal
Create the Haddock HTML cache layout for Plan B's server mode, and integrate a Haddock-cache check into `doctor`.

## Files to Create
- `src/Hypha/Haddock/Generate.hs` (ensureHaddockFor, cache/symlink logic)
- `test/Unit/Haddock.hs`

## Files to Modify
- `src/Hypha/Cache.hs` — add `haddockCacheRoot`
- `src/Hypha/Command/Doctor.hs` — add Haddock-cache check (Warn if absent)
- `hypha.cabal` — expose `Hypha.Haddock.Generate`
- `test/Main.hs`

## Acceptance Criteria
- [ ] `haddockDirFor` returns `~/.cache/hypha/haddock/<pkg>-<ver>/`
- [ ] `ensureHaddockFor` checks cache, then store, then best-effort build
- [ ] `doctor` includes `haddock cache` check (Warn if missing, not Fail)
- [ ] Unit test verifies cache directory naming
- [ ] No new CLI subcommand; this is plumbing for Plan B
