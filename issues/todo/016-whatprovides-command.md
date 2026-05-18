# Task 10.7: `whatprovides` Command

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 10.6  
**Commit:** `feat(cmd): whatprovides — Hoogle-backed lookup by symbol name; golden`

## Goal
Implement `hypha whatprovides <SYM>` using Hoogle to find packages/modules that export the symbol.

## Files to Create
- `src/Hypha/Command/WhatProvides.hs`

## Files to Modify
- `src/Hypha/Cli/Run.hs` — add `CmdWhatProvides` arm
- `test/Golden/Commands.hs` — add `whatprovides-concurrently` golden
- `test/Golden/golden/whatprovides-concurrently.compact.json`
- `hypha.cabal` — expose `Hypha.Command.WhatProvides`
- `test/Main.hs`

## Acceptance Criteria
- [ ] Uses Hoogle `is:exact` query
- [ ] Returns list of providers with fetch links
- [ ] Golden test passes
