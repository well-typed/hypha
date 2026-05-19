# hypha — local package browsing

| Field    | Value                                              |
|----------|----------------------------------------------------|
| Status   | Draft v0 (post-brainstorm)                         |
| Date     | 2026-05-19                                         |
| Authors  | Alfredo (Well-Typed) + Pi (brainstorming pair)     |
| Audience | WT engineers, AI agents                            |
| License  | BSD-3-Clause                                       |
| Spec     | Implements Plan A (source path in `PlannedUnit`)   |

## 1. Motivation

When a developer works on a Haskell project, the code they care about most is their own — the `source-repository-package` checkouts, the local packages in the cabal project, the one package they are actively editing. Yet `hypha` today can only browse **dependencies**: packages from the Cabal store or Hackage. The local package appears in the build plan (with `puIsLocal = True`) but its source is unreachable because `locatePackageSource` only looks in the cabal store, and `resolveSrc` falls through to a Hackage tarball download that does not exist for unpublished local packages.

This is a fundamental gap: the whole point of a project-aware tool is to probe the project itself.

## 2. Goals

1. **CLI commands** (`hypha module`, `hypha symbol`, `hypha source`, `hypha package`) work against local (inplace) packages, resolving source from the project tree.
2. **`hypha server`** serves the local package's source, module exports, symbol cards, and — when Haddock HTML is available — its rendered docs.
3. **Search index** includes the local package's symbols and modules.
4. **No network** required for any of the above — local source is on disk.

### 2.1 Non-goals

- **Auto-building Haddock for local packages** is deferred. The spec opts for a best-effort check of existing `dist-newstyle/` Haddock output; triggering a `cabal haddock` build is future work.
- **Full `.cabal`-file grammar parser.** The existing text-based `parseExposedModules` heuristic from `Hypha.Source.Modules` is sufficient.

## 3. Architecture

The key insight: `plan.json` already contains the source path for local packages via `uPkgSrc :: Maybe PkgLoc` and the build directory via `uDistDir :: Maybe FilePath`. We simply stop discarding this information.

```
plan.json ──→ cabal-plan ──→ cabalPlanToBuildPlan ──→ PlannedUnit
                                                          │
                                            puSrcDir ─────┤
                                            puDistDir ────┘
                                                          │
                                                          ▼
                                           PackageResolver.resolveSrc
                                                          │
                                           (1) puSrcDir? ──┤
                                           (2) Store? ─────┤
                                           (3) Hackage? ───┘
                                                          │
                                                          ▼
                                              Source found ✓
```

## 4. Change set

### 4.1 Enrich `PlannedUnit`

**File: `src/Hypha/Types/BuildPlan.hs`**

Add two `Maybe FilePath` fields:

```haskell
data PlannedUnit = PlannedUnit
  { puId       :: !PackageId
  , puDeps     :: ![PackageId]
  , puIsLocal  :: !Bool
  , puSrcDir   :: !(Maybe FilePath)   -- ^ Source root from plan.json (pkg-src.path).
                                     -- 'Just p' for inplace/local packages, 'Nothing'
                                     -- for store/Hackage packages.
  , puDistDir  :: !(Maybe FilePath)   -- ^ Build directory from plan.json (dist-dir).
                                     -- Used to locate pre-built Haddock HTML.
  }
```

All existing pattern matches on `PlannedUnit` update to include the new fields.

### 4.2 Extract from plan.json

**File: `src/Hypha/Project/Plan.hs`**

In `toPlannedUnit`, map cabal-plan's `Unit` fields:

| cabal-plan field | Our field | Mapping |
|---|---|---|
| `uPkgSrc = Just (LocalUnpackedPackage p)` | `puSrcDir = Just p` | Source root of local package |
| `uPkgSrc = _` | `puSrcDir = Nothing` | Not a local package |
| `uDistDir = Just d` | `puDistDir = Just d` | Build directory |
| `uDistDir = Nothing` | `puDistDir = Nothing` | No build info |

The `PkgLoc` constructor `LocalUnpackedPackage` is the one cabal-install uses for local source packages. Other constructors (`RemoteTarballPackage`, `RepoTarballPackage`, etc.) map to `Nothing`.

```haskell
toPlannedUnit :: Map CP.UnitId CP.PkgId -> CP.Unit -> PlannedUnit
toPlannedUnit unitIdToPkgId u =
  let … -- existing code unchanged
  in PlannedUnit
    { puId      = pkgId
    , puDeps    = deps
    , puIsLocal = (CP.uType u == CP.UnitTypeLocal)
    , puSrcDir  = extractSrcDir (CP.uPkgSrc u)
    , puDistDir = CP.uDistDir u
    }

extractSrcDir :: Maybe CP.PkgLoc -> Maybe FilePath
extractSrcDir (Just (CP.LocalUnpackedPackage p)) = Just p
extractSrcDir _                                   = Nothing
```

### 4.3 Plan-aware source resolution

**File: `src/Hypha/Package/Resolver.hs`**

Modify `resolvePackageSourceWith` to check the plan's `puSrcDir` before falling through:

```haskell
resolvePackageSourceWith
  :: BuildEnv IO -> HackageClient IO -> FilePath -> BuildPlan
  -> PackageId -> IO (Either HyphaError FilePath)
resolvePackageSourceWith env hclient sourceCache plan pid = do
  -- Step 0: Local package source from plan (fast, no I/O beyond stat).
  case planSrcDir plan (pkgName pid) of
    Just dir | dirExists dir -> pure (Right dir)
    _                        -> fallbackToEnv
  where
    fallbackToEnv = do
      mSrc <- locatePackageSource env pid
      case mSrc of
        Just dir -> pure (Right dir)
        Nothing  -> downloadFromHackage pid  -- existing code unchanged

planSrcDir :: BuildPlan -> PackageName -> Maybe FilePath
planSrcDir plan name =
  case lookupUnit name plan of
    Just pu | Just dir <- puSrcDir pu -> Just dir
    _                                 -> Nothing
```

The `Path` argument to `mkPackageResolver` already carries the source-cache directory; we add `BuildPlan` as a parameter so the plan is available to queries. The top-level dispatch in `Hypha.Cli.Run` already passes `plan` to `mkPackageResolver` — we thread it through.

### 4.4 Haddock resolution for local packages

**File: `src/Hypha/Haddock/Generate.hs`**

Add a dist-dir check between the cache and the store fallback:

```haskell
ensureHaddockFor :: BuildPlan -> BuildEnv IO -> PackageId -> IO (Maybe FilePath)
ensureHaddockFor plan env pid = do
  -- 1. hypha cache (existing)
  inCache <- haddockCacheExists pid
  if inCache
    then … -- existing code
    else do
      -- 2. local package dist-dir (NEW)
      mDist <- distDirHaddock plan pid
      case mDist of
        Just idx -> pure (Just idx)
        Nothing  -> locateHaddockHtml env pid  -- 3. store (existing)

distDirHaddock :: BuildPlan -> PackageId -> IO (Maybe FilePath)
distDirHaddock plan pid =
  case lookupUnit (pkgName pid) plan of
    Just pu | Just d <- puDistDir pu -> do
      let pkgName = Text.unpack (unPackageName (pkgName pid))
          idx = d </> "doc" </> "html" </> pkgName </> "index.html"
      ok <- doesFileExist idx
      pure (if ok then Just idx else Nothing)
    _ -> pure Nothing
```

**File: `src/Hypha/Command/Server.hs`**

Thread the `BuildPlan` into `ensureHaddockFor` in `buildServerConfig` and `prebuildAll`. The signature changes are confined to these two call sites.

### 4.5 Server search index

No changes needed. `buildAndCacheIndex` iterates all packages from `planPackageIds` (which includes local ones) and calls `resolveSrc` for each. Once step 4.3 returns the local source dir, the indexer walks the tree and indexes symbols.

## 5. Testing

| Area | What | How |
|---|---|---|
| Unit | `PlannedUnit` extraction from `Unit` with `LocalUnpackedPackage` | Extend `cabalPlanToBuildPlan` test fixture with a local unit |
| Unit | Source dir extraction: `Just "/project"` → `puSrcDir`, others → `Nothing` | HUnit on `extractSrcDir` |
| Unit | Resolver fallback: local dir returned before store | Mock `BuildPlan` with a `puSrcDir`, verify `resolveSrc` returns it |
| Golden | Local package JSON output includes `"is_local": true` and populated `"exposed_modules"` | Add a `package-local` golden test with fixture plan containing an inplace package |
| Server | Local package appears in module list and search index | Update `Server` golden test fixture to include a local source tree |

## 6. Files touched

| File | Change |
|---|---|
| `src/Hypha/Types/BuildPlan.hs` | Add `puSrcDir`, `puDistDir` fields |
| `src/Hypha/Project/Plan.hs` | Extract `uPkgSrc` / `uDistDir` in `cabalPlanToBuildPlan` |
| `src/Hypha/Package/Resolver.hs` | Add plan-sourced step in `resolvePackageSourceWith`; pass `BuildPlan` through |
| `src/Hypha/Haddock/Generate.hs` | Add dist-dir check in `ensureHaddockFor` |
| `src/Hypha/Command/Server.hs` | Thread `BuildPlan` into `ensureHaddockFor` callers |
| `test/fixtures/tiny-project/dist-newstyle/cache/plan.json` | Add an inplace (local) package entry |
| `test/Golden/golden/` | New golden for local package output |
| `test/Unit/Modules.hs` | Unit tests for new fields and fallback logic |

## 7. Open questions

- **`distDirHaddock` path discovery.** The exact path `<distDir>/doc/html/<pkg>/index.html` is the convention used by cabal-install for GHC ≥9.2. If it differs, we can adjust by globbing or making it configurable. Future-proof by checking both `<distDir>/doc/html/<pkg>/` and the legacy `share/doc/` layout inside the dist directory.
- **Symlink vs copy for the source cache.** Currently `resolveSrc` downloads Hackage tarballs into `~/.cache/hypha/source/`. For local packages we return the real project source dir directly — no caching needed. This is fine: the project dir is stable for the duration of the tool's lifetime.

## 8. Future work (not in this spec)

- **Lazy `cabal haddock` for local packages.** Invoke `typed-process` to build Haddock on first request, cache result.
- **`hypha doctor` check** that warns when the local package source is listed in the plan but the directory is missing.
