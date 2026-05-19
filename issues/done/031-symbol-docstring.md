# Issue 031: Add docstring and signature to `symbol` output

**Status:** todo
**Priority:** P1
**Blocked by:** —
**Commit:** `feat(cmd): include haddock docstring + signature in symbol compact output`

## Goal
`hypha symbol PKG/MOD/SYM` currently returns metadata (kind, module, name, package, version) but NOT the Haddock prose docstring or the type signature. An agent's first question is "what does this symbol do?" — which means signature + docstring. This one change eliminates 5+ unnecessary tool calls.

Currently only `source` includes the docstring (as `haddock_raw`), but `source` also returns the full implementation body — overkill for a quick lookup.

## Fix
- Add `signature` and `haddock_raw` fields to the default compact output of `symbol`
- `--human symbol` already shows signature + docstring; the JSON compact output just needs to include them
- Make `symbol` the canonical "tell me about this symbol" command

## Files to Modify
- `src/Hypha/Command/Symbol.hs` — include `signature` and `haddock_raw` in compact key set, update `symbolResultToJSON`
- `test/Golden/golden/symbol-concurrently.compact.json` — update golden

## Acceptance Criteria
- [ ] `hypha symbol async/Control.Concurrent.Async/concurrently` returns JSON with `signature` and `haddock_raw` in compact output
- [ ] No regressions in `source` or `module` commands
- [ ] Golden test updated
