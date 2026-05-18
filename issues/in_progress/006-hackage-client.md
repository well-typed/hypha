# Task 6: HackageClient + ETag/Last-Modified Cache

**Status:** todo  
**Priority:** P1  
**PR:** One PR  
**Commit:** `feat(hackage): JSON API client with ETag-aware filesystem cache; offline cache-miss errors`

## Goal
HTTP client for Hackage JSON API with filesystem cache, ETag revalidation, and offline mode.

## Files to Create
- `src/Hypha/Hackage/Types.hs` (CacheKind, CachedResponse)
- `src/Hypha/Hackage/Cache.hs` (FS cache with SHA256 keys, JSON persistence)
- `src/Hypha/Hackage/Api.hs` (HackageClient, fetchPackageJson)
- `src/Hypha/Cache.hs` (shared `cacheRoot` helper)
- `test/Property/HackageCache.hs` (JSON roundtrip property)
- `test/Unit/Hackage.hs` (offline miss test)

## Files to Modify
- `hypha.cabal` — add `http-client`, `http-client-tls`, `aeson`, `time`, `cryptohash-sha256`, `http-types`, `temporary`
- `test/Main.hs`

## Acceptance Criteria
- [ ] Cache JSON encode/decode roundtrip property passes
- [ ] Offline mode with empty cache returns `OfflineCacheMiss`
- [ ] No network touched in test suite
- [ ] Freshness predicate works for Immutable vs TTL
