# Task 3: Project Resolution — BuildPlan, Discovery, Overrides

**Status:** todo  
**Priority:** P0  
**PR:** One PR  
**Commit:** `feat(project): plan.json ingestion, project-root discovery, package overrides`

## Goal
Parse `dist-newstyle/cache/plan.json`, discover project roots, and support package version overrides.

## Files to Create
- `src/Hypha/Types/BuildPlan.hs`
- `src/Hypha/Project/Discovery.hs` (walk up to find `cabal.project`)
- `src/Hypha/Project/Plan.hs` (`cabal-plan` integration)
- `src/Hypha/Project/Overrides.hs` (`PKG=VER` CLI overrides)
- `test/Unit/Project.hs`
- `test/fixtures/tiny-project/` (minimal cabal project with hand-built `plan.json`)

## Files to Modify
- `hypha.cabal` — add `cabal-plan`, `cabal-install-parsers`, `Cabal-syntax`, `directory`, `filepath`
- `test/Main.hs`

## Acceptance Criteria
- [ ] `discoverProjectRoot` finds tiny-project fixture
- [ ] `loadBuildPlan` parses fixture `plan.json`
- [ ] `parsePackageOverride "async=2.2.6"` works
- [ ] `applyOverrides` changes pinned version in plan
- [ ] All tests pass
