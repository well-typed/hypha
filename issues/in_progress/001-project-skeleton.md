# Task 1: Project Skeleton

**Status:** todo  
**Priority:** P0 (blocks everything)  
**PR:** One PR  
**Commit:** `feat: project skeleton — cabal + ghc matrix + devshell + hello-world hypha exe`

## Goal
Create the project scaffolding: cabal package, CI, devshell, and a working hello-world executable.

## Files to Create
- `LICENSE` (BSD-3-Clause)
- `README.md` (stub with etymology, mantra, status)
- `hypha.cabal`
- `cabal.project`
- `cabal.project.freeze` (generated via `cabal freeze`)
- `flake.nix` (minimal devshell)
- `.github/workflows/ci.yml` (GHC 9.6 / 9.8 / 9.10 matrix)
- `src/Hypha/Prelude.hs` (exports `version = "0.0.0"`)
- `app/hypha/Main.hs` (prints `hypha 0.0.0`)
- `test/Main.hs` (empty tasty suite)

## Acceptance Criteria
- [ ] `cabal build all` succeeds
- [ ] `cabal run hypha` outputs `hypha 0.0.0`
- [ ] `cabal test` exits 0 with 0 tests
- [ ] CI workflow file is present
- [ ] All files committed with conventional commit message

## Spec Reference
`docs/superpowers/specs/2026-05-18-hypha-design.md`
