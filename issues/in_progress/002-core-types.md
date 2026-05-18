# Task 2: Core Types — PackageId and SymbolPath

**Status:** todo  
**Priority:** P0  
**PR:** One PR  
**Commit:** `feat(types): add PackageId and SymbolPath with parser, pretty, and falsify roundtrip`

## Goal
Define the two fundamental types: `PackageId` and `SymbolPath`, with parsing, rendering, and property tests.

## Files to Create
- `src/Hypha/Types/PackageId.hs`
- `src/Hypha/Types/SymbolPath.hs`
- `test/Property/SymbolPath.hs`

## Files to Modify
- `hypha.cabal` — add `text`, `bytestring`, `containers`, `falsify`, `tasty-falsify`, `tasty-hunit` deps; expose new modules
- `test/Main.hs` — register property tests

## Acceptance Criteria
- [ ] Property test `parse . render = Right` passes (TDD: start with failing test)
- [ ] Concrete unit tests for: pkg only, pkg+ver+mod+sym, rejects symbol without module
- [ ] All tests pass via `cabal test`
- [ ] Strict bangs on all strict record fields per project convention

## Related
Blocks Task 3 (BuildPlan types depend on PackageId).
