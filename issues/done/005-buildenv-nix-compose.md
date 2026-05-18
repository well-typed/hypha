# Task 5: Nix BuildEnv + Composition

**Status:** todo  
**Priority:** P1  
**PR:** One PR  
**Commit:** `feat(buildenv): Nix store impl + composition (primary wins, secondary fills gaps)`

## Goal
Add a Nix store BuildEnv and a composition operator that falls back from primary to secondary.

## Files to Create
- `src/Hypha/BuildEnv/Nix.hs` (walks `result` symlink, `package.conf.d`)
- `src/Hypha/BuildEnv/Compose.hs` (`composeBuildEnv`)
- `test/Unit/BuildEnvCompose.hs`

## Files to Modify
- `hypha.cabal`
- `test/Main.hs`

## Acceptance Criteria
- [ ] Composition: primary wins for shared keys
- [ ] Composition: fallthrough for missing keys
- [ ] `discoverInstalledPackages` is union of both sets
- [ ] All three composition tests pass
