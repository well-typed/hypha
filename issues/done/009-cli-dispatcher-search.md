# Task 9: Exit Codes, Errors, CLI Dispatcher, `search` Command End-to-End, First Golden

**Status:** todo  
**Priority:** P1  
**PR:** One PR  
**Commit:** `feat(cli): exit codes, error sum, dispatcher, search command, first golden test`

## Goal
Wire the CLI parser, dispatcher, exit codes, error sum, and the first end-to-end command (`search`) with a golden test.

## Files to Create
- `src/Hypha/Exit.hs` (typed exit codes: 0,2,3,4,5,7)
- `src/Hypha/Error.hs` (HyphaError constructors + messages)
- `src/Hypha/Logging.hs` (contra-tracer based)
- `src/Hypha/Cli/Parser.hs` (optparse-applicative, all global flags + subcommands)
- `src/Hypha/Cli/Run.hs` (dispatcher, `emit` helper)
- `src/Hypha/Command/Search.hs` (first command: Hoogle search, JSON output)
- `test/Golden/Search.hs`
- `test/Golden/golden/search-map-insert.compact.json`

## Files to Modify
- `app/hypha/Main.hs` — thin wrapper over `Hypha.Cli.Run`
- `hypha.cabal` — add `contra-tracer`, `optparse-applicative`, `prettyprinter`, `prettyprinter-ansi-terminal`, `tasty-golden`, `aeson`, `bytestring`
- `test/Main.hs`

## Acceptance Criteria
- [ ] `cabal run hypha -- search 'Map.insert'` produces JSON envelope
- [ ] Golden test `search-map-insert` passes
- [ ] Exit codes are correct per error type
- [ ] Typed errors have human-readable messages
- [ ] `--human` flag parsed but not yet honoured (deferred to Task 11)
