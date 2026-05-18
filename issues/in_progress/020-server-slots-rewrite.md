# Task 20: Slot Management + Haddock Rewrite

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 19 (haddock-cache)  
**PR:** One PR  
**Commit:** `feat(server): per-package build slots + Haddock URL rewriter`

## Goal
Per-package MVar build slots for de-duplicated lazy Haddock builds, and a tagsoup-based Haddock HTML URL rewriter.

## Files to Create
- `src/Hypha/Server/Slots.hs`
- `src/Hypha/Server/Haddock/Rewrite.hs`
- `test/Unit/ServerSlots.hs`
- `test/Property/HaddockRewrite.hs`

## Files to Modify
- `hypha.cabal` — add `async`, `stm`, `tagsoup`; expose new modules
- `test/Main.hs`

## Acceptance Criteria
- [ ] `BuildSlots` is an immutable `Map PackageId (MVar BuildState)`
- [ ] `withSlot` deduplicates: two concurrent calls for same `pid` invoke build at most once
- [ ] `rewriteHaddockHtml` rewrites `../pkg-ver/...` to `/haddock/pkg-ver/...`
- [ ] Rewrite is idempotent (property test)
- [ ] Rewrite preserves non-pkg hrefs (property test)
- [ ] Slot dedup unit test passes
