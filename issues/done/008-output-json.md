# Task 8: Output — Outcome, Actions, Json (Compact / Full), --select

**Status:** todo  
**Priority:** P1  
**PR:** One PR  
**Commit:** `feat(output): Outcome envelope with compact/full fieldsets and --select projection`

## Goal
Define the output envelope schema, compact/full fieldset projection, and `--select` key filtering.

## Files to Create
- `src/Hypha/Output/Outcome.hs` (Outcome, OutcomeEnvelope, Action, Related)
- `src/Hypha/Output/Actions.hs` (standard cross-reference actions)
- `src/Hypha/Output/Json.hs` (encodeEnvelope, filterSelect, restrictBody)
- `test/Property/OutputJson.hs` (compact ⊆ full property)

## Files to Modify
- `hypha.cabal`
- `test/Main.hs`

## Acceptance Criteria
- [ ] Property `compact ⊆ full` for symbol-card key sets passes
- [ ] `encodeEnvelope` produces valid JSON with schema `hypha/v0`
- [ ] `--select` filters top-level keys correctly
- [ ] `actions` and `related` are encoded as objects/lists
