# Design: `hypha lookup` — single tiered symbol-resolution command

**Status:** Approved (brainstorming)
**Date:** 2026-05-20
**Supersedes:** `hypha search`, `hypha whatprovides`, `--global` flag

## Motivation

Today, agents and humans face three overlapping entry points to answer
"which package/module provides this thing?":

- `hypha search <free-text>` — Hoogle-backed fuzzy/type-sig search.
- `hypha whatprovides <symbol>` — Hoogle exact-name lookup.
- `--global` flag toggling between project-local and global Hoogle DBs.

In practice the project-local Hoogle path is **non-functional**: it is
stubbed with placeholder inputs (`HoogleConfig _ [] "alpha-stub"`), so
queries silently return `[]`.  Agents waste tool calls discovering this
empirically and fall back to `grep`.

This design collapses the three entry points into a single command,
`hypha lookup`, and makes the underlying machinery actually work.

## Goals

- One command for "where does this live?" — `hypha lookup <query>`.
- Tiered, short-circuiting cascade: cheapest source first.
- Local-package symbols resolve **without** an internet connection.
- Type-signature queries (`a -> Maybe a`) work against local code.
- Remote Hoogle is a fallback for discovery beyond the user's plan.
- Structured failure outcomes — never `[]` masking an error.
- No 1 GB Stackage tarball, no daemon, no consent prompt.

## Non-goals

- Pre-fetching package source on remote hits (`hypha package` handles
  that explicitly).
- Cache-invalidation CLI commands (manual `rm -rf` documented in
  README; we deliberately do not expose a footgun for agents).
- Backwards compatibility shims for `search` / `whatprovides` /
  `--global` (clean break, version bump to 0.2.0).
- Background warm-up of the local Hoogle DB at server startup (the
  `hypha server` keeps using the in-memory exports index it already
  builds; `lookup` only needs the DB when invoked).

## Architecture

### Cascade

```
hypha lookup <query>  [--offline]
        │
        ▼
┌──────────────────────────────────────────────────────────┐
│ Tier 1: PackageCache (SQLite, project + global)          │
│   - exact-name match in pkg_index                        │
│   - qualified-name split ("Data.Map.lookup")             │
│   - lazy fingerprint refresh for local-origin rows       │
└──────────────────────────────────────────────────────────┘
        │ non-empty → emit, stop
        ▼
┌──────────────────────────────────────────────────────────┐
│ Tier 2: Local Hoogle (project-scoped .hoo DB)            │
│   - regenerate iff plan-hash or aggregate fingerprint    │
│     differs from stored                                   │
│   - serialised behind an MVar                            │
└──────────────────────────────────────────────────────────┘
        │ non-empty → emit, stop
        ▼
┌──────────────────────────────────────────────────────────┐
│ Tier 3: Remote Hoogle (hoogle.haskell.org)               │
│   - skipped when --offline / HYPHA_OFFLINE=1             │
│   - 24h KV cache on (query → hits)                       │
│   - 10s default timeout, configurable via env            │
└──────────────────────────────────────────────────────────┘
        │
        ▼
OutcomeEnvelope
```

Short-circuit semantics: the first tier that yields ≥1 hit wins; later
tiers are not consulted.  Rationale: agents need a fast, predictable
"where do I start looking" answer; merging tiers re-introduces the
ambiguity we are removing.

### Modules

**Added:**

- `Hypha.Command.Lookup` — replaces `Search` and `WhatProvides`.
  Owns `LookupResult`, `Provider`, outcome assembly.
- `Hypha.Hoogle.Local` — opaque `HyphaHoogle`, generation lifecycle,
  `searchLocal`, MVar-guarded regen.
- `Hypha.Hoogle.Remote` — HTTP client, KV-cached, 10s timeout.

**Extended:**

- `Hypha.Search.PackageCache` — adds `lookupByName`,
  `componentFingerprint`, `staleFingerprint`.  Schema gains a
  `fingerprint` column on `pkg_index_meta`.
- `Hypha.Project.Plan` — exposes `planHash :: BuildPlan -> Text`
  (sha256 over canonicalised `plan.json`).
- `Hypha.Cli.Parser` / `Hypha.Cli.Run` — wires `LookupCommand`,
  removes `SearchCommand` + `WhatProvidesCommand`, removes `--global`,
  adds `--offline`.

**Removed:**

- `Hypha.Command.Search`
- `Hypha.Command.WhatProvides`
- `Hypha.Hoogle.Query`
- `Hypha.Hoogle.Database.withGlobalDb` (and supporting code)

### Local Hoogle DB generation

The Hoogle library only consumes `.txt` files produced in the format
`haddock --hoogle` emits.  We cannot bypass this, but we minimise the
work:

1. Walk every unit in the build plan.
2. For each **non-local** unit, look for an existing
   `~/.cabal/store/ghc-X.Y.Z/<pkg-hash>/share/doc/<pkg>-<ver>/html/<pkg>.txt`.
   If found, **scavenge** it (no haddock invocation).
3. For **local** units, or non-local units whose `.txt` is missing
   (user disabled docs), invoke `haddock --hoogle -o <tmp>/<pkg>.txt
   <module files>` with GHC db flags derived from `plan.json`.
4. Pass the assembled list of `.txt` files to
   `Hoogle.hoogle ["generate", "--local=<dir>", "--database=<path>"]`.
5. Stamp the DB with `(plan-hash, aggregate-fingerprint)` in the `kv`
   table.

The whole pipeline runs under a single `MVar ()` to prevent concurrent
regen (Hoogle's library is known to deadlock under that).

### Fingerprint invalidation

Local-package cache rows go stale aggressively (editor edits in-flight).
Mitigation: a per-component fingerprint stored alongside `pkg_index_meta`.

```
fingerprint = sha256(sort([file <> show mtime <> show size
                          | file <- recursively hsFiles hs_source_dirs]))
```

On `lookupByName`:

- Compute fingerprint for each local-origin component touched.
- Compare against stored fingerprint.
- Mismatch → re-index that component synchronously (typically <500ms),
  replace its rows in `pkg_index`, update fingerprint.

Aggregate fingerprint (hash of per-component fingerprints sorted by
component key) drives Hoogle-DB staleness too.

### Remote Hoogle

- Endpoint: `https://hoogle.haskell.org/?hoogle=<query>&mode=json&count=20`.
- Response shape: JSON array of `{package, module, item, type, docs}`.
- `Hypha.Hoogle.Remote.searchRemote` returns `Either RemoteError [HoogleHit]`.
- Cached in `PackageCache.kv` under
  `hoogle:remote:<sha256(query)>` with `cached_at` timestamp.
- TTL = 24h; cached value used iff `now - cached_at < ttl`.
- Timeout 10s default, override via `HYPHA_HOOGLE_TIMEOUT=<seconds>`.
- `--offline` / `HYPHA_OFFLINE=1` skips Tier 3 entirely.

### Outcome envelopes

```jsonc
// Cache or local-hoogle hit (Tier 1 example)
{ "ok": true,
  "result": {
    "query": "lookup",
    "providers": [
      {"pkg":"containers","mod":"Data.Map","name":"lookup",
       "sig":"Ord k => k -> Map k a -> Maybe a",
       "origin":{"kind":"hackage"},"tier":"cache"}
    ],
    "tiers_consulted": ["cache"]
  },
  "related": [
    {"label":"containers/Data.Map",
     "fetch":"hypha symbol containers/Data.Map/lookup"}
  ] }

// No tier produced hits
{ "ok": false, "code": "NOT_FOUND",
  "result": { "query":"foo", "providers": [],
              "tiers_consulted":["cache","local-hoogle","remote-hoogle"] },
  "actions": { "retry_with_prefix": "hypha lookup foo*" } }

// Remote tier failed (network)
{ "ok": false, "code": "HOOGLE_REMOTE_ERROR",
  "result": { "query":"...", "providers": [],
              "tiers_consulted":["cache","local-hoogle","remote-hoogle"],
              "error_detail":"timeout after 10s" },
  "actions": { "retry_offline":"hypha lookup ... --offline",
               "raise_timeout":"HYPHA_HOOGLE_TIMEOUT=30 hypha lookup ..." } }

// --offline AND local tiers missed
{ "ok": false, "code": "HOOGLE_OFFLINE",
  "result": { "query":"...", "providers": [],
              "tiers_consulted":["cache","local-hoogle"] },
  "actions": { "retry_online":"hypha lookup ..." } }

// Local Hoogle DB generation failed mid-cascade
// (haddock missing, parse error, ...).  Cascade continues; surfaced as
// a warning rather than a top-level error.
{ "ok": <whatever later tier produced>,
  "warnings": [
    "local hoogle db generation failed: <detail>; consulted remote only"
  ],
  ... }
```

`Provider`:

```jsonc
{ "pkg":    "containers",
  "mod":    "Data.Map.Strict",
  "name":   "lookup",
  "sig":    "Ord k => k -> Map k a -> Maybe a",
  "origin": { "kind": "hackage" },
  "tier":   "cache" }
```

`origin` reuses the `PackageOrigin` discriminator already shipped by
issue 036.  `tier` is `cache | local-hoogle | remote-hoogle`.

### Concurrency

| Resource              | Guard                                   |
|-----------------------|-----------------------------------------|
| Local Hoogle DB regen | New `MVar ()` in `HyphaHoogle`          |
| `PackageCache` writes | Existing `IndexCache.icLock` MVar       |
| KV cache writes       | Same `icLock`                           |
| Remote HTTP           | Stateless — no guard needed             |

## Data flow detail

### Tier 1 — `lookupByName`

```haskell
lookupByName :: HyphaPackageCache -> Text -> IO [Provider]
lookupByName cache rawQuery = do
  let (mModName, name) = splitQualified rawQuery
      -- "Data.Map.lookup" -> (Just "Data.Map", "lookup")
      -- "lookup"          -> (Nothing,         "lookup")
  rows <- queryRows cache name mModName
  -- project shadows global is handled inside the cache wrapper.
  freshRows <- mapM refreshIfStale rows
  pure (map toProvider freshRows)
  where
    refreshIfStale row
      | localOrigin row = do
          fpNow <- componentFingerprint (rowComponentDirs row)
          if fpNow == rowFingerprint row
            then pure row
            else reindexComponent cache row >> readRow cache row
      | otherwise = pure row
```

### Tier 2 — `searchLocal`

```haskell
searchLocal :: HyphaHoogle -> HoogleQuery -> IO [HoogleHit]
searchLocal hh q = withMVar (hhLock hh) $ \_ -> do
  ensureFresh hh        -- regen iff plan-hash or fingerprint stale
  Hoogle.withDatabase (hhPath hh) $ \db ->
    pure (map toHit (Hoogle.searchDatabase db (Text.unpack (unHoogleQuery q))))
```

`ensureFresh`:

1. Read stored `(plan-hash, aggregate-fingerprint)` from `kv`.
2. Compute current values.
3. If differ → regenerate DB, write new values.

### Tier 3 — `searchRemote`

```haskell
searchRemote :: HyphaPackageCache       -- ^ for KV cache
              -> HoogleQuery
              -> IO (Either RemoteError [HoogleHit])
searchRemote cache q = do
  mCached <- readKv cache (cacheKey q)
  case mCached of
    Just (cachedAt, hits) | fresh cachedAt -> pure (Right hits)
    _ -> do
      r <- httpJsonGet (endpoint q) timeoutSecs
      case r of
        Left err   -> pure (Left err)
        Right hits -> do
          writeKv cache (cacheKey q) (now, hits)
          pure (Right hits)
```

## Migration

| Surface           | Action                                       |
|-------------------|----------------------------------------------|
| `hypha search`    | Removed.  Optparse emits "unknown command".  |
| `hypha whatprovides` | Removed.  Optparse emits "unknown command". |
| `--global`        | Removed.  Optparse emits "unknown flag".     |
| `--offline`       | New flag on `lookup`.                        |
| `hypha-mcp`       | `search` + `whatprovides` tools removed; `lookup` tool added. |
| Version           | 0.1.x → 0.2.0 (breaking).                    |

CHANGELOG entry + README sections rewritten:

- "Looking up symbols" — built around `lookup` and the cascade.
- "Cache layout" — explains `~/.cache/hypha/` and `<project>/.hypha/`,
  with `rm -rf` recipe for invalidation.

Schema migration: on `openIndexCache`, run
`ALTER TABLE pkg_index_meta ADD COLUMN fingerprint TEXT`, idempotent,
guarded by `PRAGMA user_version`.  Existing rows get `NULL` and are
treated as stale on first lookup.

## Testing

### Unit (`Test.Tasty.HUnit`)

- `Unit.PackageCache.Fingerprint` — determinism, sensitivity, sort
  stability, empty-dir case.
- `Unit.PackageCache.LookupByName` — exact, qualified, cross-package
  collisions.
- `Unit.Hoogle.Remote` — timeout, offline, KV hit, parse failure.
- `Unit.Hoogle.LocalGen` — `.txt` scavenger, haddock fallback,
  hash-based skip, MVar serialisation.

### Property (`Test.Tasty.Falsify`)

- `Property.Lookup.Cascade` — for every `(hit?, hit?, hit?, offline?)`
  combination, `tiers_consulted` is exactly the prefix up to the
  hitting tier (or all tiers when none hit).
- `Property.Lookup.OutcomeShape` — envelope invariants:
  `ok ⇔ providers ≠ []`, `code` set iff failure, `actions` non-empty
  on failure.

### Golden (`Test.Tasty.Golden`)

`test/Golden/golden/`:

- `lookup-cache-hit.json`
- `lookup-local-hoogle-hit.json`
- `lookup-remote-hit.json`
- `lookup-all-miss.json`
- `lookup-remote-error.json`
- `lookup-offline.json`

### Mocking discipline

- `Hoogle IO` stays a record-of-functions.
- New `RemoteHoogleTransport` record wraps HTTP — tests inject
  deterministic transports.
- New `HaddockRunner` record wraps `Process` — tests do not shell out.

CI runs the full suite without network, haddock, or GHC subprocesses;
manual integration smoke lives in `docs/manual-testing.md`.

## Open issues (deferred)

- **Cross-project SRP collisions at differing commits.**  If two
  projects pin the same `(pkg, ver)` from an SRP at different commits,
  the global cache row for that version is shared.  Acknowledged in
  the package-cache work (issue 035); not addressed here.
- **Result ranking.**  Tier-1 hits are returned in `pkg_index` insertion
  order; Hoogle tiers in upstream order.  No relevance scoring across
  tiers; deferred until we have user feedback on what's confusing.
- **MCP integration tests.**  The MCP-side rewiring of `lookup` tool
  is in scope but tested only manually; structured test harness for
  the MCP layer is its own follow-up.
