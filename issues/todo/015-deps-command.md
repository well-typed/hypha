# Task 10.6: `deps` Command

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 10.5  
**Commit:** `feat(cmd): deps command — forward and reverse, depth-aware; goldens`

## Goal
Implement `hypha deps <PKG> [--reverse] [--depth N]` showing dependency tree.

## Files to Create
- `src/Hypha/Command/Deps.hs`

## Files to Modify
- `src/Hypha/Cli/Run.hs` — add `CmdDeps` arm
- `test/Golden/Commands.hs` — add `deps-async-forward` and `deps-async-reverse` goldens
- `test/Golden/golden/deps-async-forward.compact.json`
- `test/Golden/golden/deps-async-reverse.compact.json`
- `hypha.cabal` — expose `Hypha.Command.Deps`
- `test/Main.hs`

## Acceptance Criteria
- [ ] Forward deps from plan.json
- [ ] Reverse deps computed across plan
- [ ] `--depth` bound respected
- [ ] Two golden tests pass
