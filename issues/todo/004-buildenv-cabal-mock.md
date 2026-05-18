# Task 4: BuildEnv Interface + Cabal Implementation + Mock

**Status:** todo  
**Priority:** P0  
**PR:** One PR  
**Commit:** `feat(buildenv): records-of-functions interface, Cabal store impl, in-memory mock`

## Goal
Create the `BuildEnv` record-of-functions interface, a Cabal-store implementation, and an in-memory mock for testing.

## Files to Create
- `src/Hypha/BuildEnv/Type.hs` (record of functions over `m`)
- `src/Hypha/BuildEnv/Cabal.hs` (scans `~/.cabal/store/ghc-X.Y.Z/`)
- `src/Hypha/BuildEnv/Mock.hs`
- `test/Unit/BuildEnv.hs`
- `test/fixtures/fake-cabal-store/` (stub store layout)

## Files to Modify
- `hypha.cabal` — add `transformers`, `mtl`; expose new modules
- `test/Main.hs`

## Acceptance Criteria
- [ ] Mock returns configured source paths
- [ ] Cabal impl finds `async-2.2.5` in fake store fixture
- [ ] Cabal `locateHaddockHtml` hits stub `index.html`
- [ ] All tests pass
