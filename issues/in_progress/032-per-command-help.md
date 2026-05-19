# Issue 032: Per-subcommand `--help` with argument examples

**Status:** todo
**Priority:** P2
**Blocked by:** —
**Commit:** `feat(cli): per-subcommand --help with argument format examples`

## Goal
`hypha --help` lists subcommands but doesn't show argument shapes (e.g. `symbol PKG/MOD/SYM`). An agent's first instinct is to guess the format, which fails. Per-subcommand `--help` (`hypha symbol --help`) should show the argument format + a concrete example.

## Fix
- Update `progDesc` for each subcommand parser in `src/Hypha/Cli/Parser.hs` to include argument template + example
- E.g. `symbol PKG/MOD/SYM` with example: `hypha symbol async/Control.Concurrent.Async/race`

## Files to Modify
- `src/Hypha/Cli/Parser.hs` — enrich `progDesc` strings

## Acceptance Criteria
- [ ] `hypha symbol --help` shows `PKG/MOD/SYM` format with example
- [ ] `hypha package --help` shows `PKG[@VER]` format
- [ ] `hypha module --help`, `hypha source --help`, etc. follow same pattern
