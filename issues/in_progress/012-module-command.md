# Task 10.3: `module` Command

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 10.2  
**Commit:** `feat(cmd): module command + naive export scraper + golden via fixture store`

## Goal
Implement `hypha module <PKG/MOD>` listing exported symbols by scraping module headers.

## Files to Create
- `src/Hypha/Command/Module.hs`
- `src/Hypha/Source/Locate.hs` (naive export parser, symbol definition locator)
- `test/fixtures/fake-cabal-store/.../Control/Concurrent/Async.hs`

## Files to Modify
- `src/Hypha/Cli/Run.hs` — add `CmdModule` arm
- `test/Golden/Commands.hs` — add `module-async` golden
- `test/Golden/golden/module-async.compact.json`
- `hypha.cabal` — expose new modules
- `test/Main.hs`

## Acceptance Criteria
- [ ] Scrapes `module ... ( ... ) where` export list
- [ ] Returns exports with related links to `symbol` command
- [ ] Golden test passes using fixture store
