# Task 7: Hoogle — Per-Project DB + Query

**Status:** todo  
**Priority:** P1  
**PR:** One PR  
**Commit:** `feat(hoogle): per-project Hoogle database with plan-hash staleness; global fallback`

## Goal
Integrate `hoogle` library: per-project `.hoo` database, staleness check via plan hash, global fallback.

## Files to Create
- `src/Hypha/Hoogle/Type.hs`
- `src/Hypha/Hoogle/Database.hs` (plan-hash based staleness, `withProjectDb`)
- `src/Hypha/Hoogle/Query.hs` (`mkProjectHoogle`, `mkGlobalHoogle`)
- `test/Unit/Hoogle.hs`

## Files to Modify
- `hypha.cabal` — add `hoogle`, `cryptohash-sha256`; expose new modules
- `test/Main.hs`

## Acceptance Criteria
- [ ] `planHashFile` path layout correct
- [ ] Hash file round-trip works
- [ ] Staleness predicate detects changed plan hash
- [ ] Does NOT invoke real Hoogle generation in unit tests (integration deferred to Task 9)
