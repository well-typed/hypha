# Issue 034: `--full symbol` expands payload

**Status:** todo
**Priority:** P3
**Blocked by:** —
**Commit:** `feat(output): --full symbol expands to include source coordinates and module tree`

## Goal
Currently `compactKeys == fullKeys` for `symbol`, which means `--full` is a no-op. An agent reaching for "give me more information" has no lever. `--full` should add fields that aren't in the compact set: source coordinates, the module's full export list, and cross-package references.

## Fix
- Make `fullKeys` a strict superset of `compactKeys` for `symbol`
- Add `source` (path + line) and `module_exports` (the module's full export list) to the full set
- Update `symbolResultToJSON` to include these fields conditionally

## Files to Modify
- `src/Hypha/Command/Symbol.hs` — define `fullKeys ≠ compactKeys`, add extra fields to `symbolResultToJSON`
- `test/Golden/golden/symbol-concurrently.compact.json` — already correct (compact set)
- `test/Golden/golden/symbol-concurrently.full.json` — create full version golden (if golden test uses `--full`)

## Acceptance Criteria
- [ ] `hypha symbol async/Control.Concurrent.Async/concurrently --full` returns more fields than compact
- [ ] `compactKeys` is a strict subset of `fullKeys` (property test enforces this)
