# Task 10.5: `source` Command

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 10.4  
**Commit:** `feat(cmd): source command — symbol snippet with path/line; golden`

## Goal
Implement `hypha source <PKG/MOD[/SYM]>` returning a source snippet around the symbol definition.

## Files to Create
- `src/Hypha/Command/Source.hs`

## Files to Modify
- `src/Hypha/Cli/Run.hs` — add `CmdSource` arm
- `test/Golden/Commands.hs` — add `source-async-concurrently` golden
- `test/Golden/golden/source-async-concurrently.compact.json`
- `hypha.cabal` — expose `Hypha.Command.Source`
- `test/Main.hs`

## Acceptance Criteria
- [ ] Returns 30-line snippet around definition
- [ ] Includes path, line, symbol name
- [ ] Golden test passes
