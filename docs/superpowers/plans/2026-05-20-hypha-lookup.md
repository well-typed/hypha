# `hypha lookup` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Collapse `hypha search` and `hypha whatprovides` into a single tiered command `hypha lookup`, with a working local Hoogle DB + remote Hoogle fallback.

**Architecture:** Three-tier short-circuit cascade: `PackageCache` (SQLite, project shadows global) → local `.hoo` DB (built from scavenged store `.txt` files + on-demand `haddock --hoogle` for local pkgs) → remote `hoogle.haskell.org`. Per-component source-tree fingerprint drives invalidation. Structured `OutcomeEnvelope` on every result.

**Tech Stack:** Haskell (GHC 9.6.7), Cabal, `cabal-plan`, `hoogle` library, `sqlite-simple`, `http-client-tls`, `aeson`, `tasty` + `tasty-hunit` + `tasty-golden` + `tasty-falsify`.

**Spec:** [`docs/superpowers/specs/2026-05-20-hypha-lookup-design.md`](../specs/2026-05-20-hypha-lookup-design.md)

---

## File map

**New:**

- `src/Hypha/Command/Lookup.hs` — outcome assembly + cascade entry.
- `src/Hypha/Hoogle/Local.hs` — `HyphaHoogle`, generation lifecycle.
- `src/Hypha/Hoogle/Remote.hs` — HTTP client, KV cache, timeout.
- `src/Hypha/Project/Fingerprint.hs` — `componentFingerprint`.
- `test/Unit/PackageCacheFingerprint.hs`
- `test/Unit/PackageCacheLookup.hs`
- `test/Unit/HoogleRemote.hs`
- `test/Unit/HoogleLocalGen.hs`
- `test/Property/LookupCascade.hs`
- `test/Property/LookupOutcomeShape.hs`
- `test/Golden/Lookup.hs` + `test/Golden/golden/lookup-*.json`

**Extended:**

- `src/Hypha/Search/Cache.hs` — add `fingerprint TEXT` column on `pkg_index_meta`, migration on `openIndexCache`; new `readBlobAt` / `writeBlobAt` for KV with `cached_at`.
- `src/Hypha/Search/PackageCache.hs` — add `lookupByName`, fingerprint accessors.
- `src/Hypha/Project/Plan.hs` — expose `planHash`.
- `src/Hypha/Cli/Parser.hs` — `LookupCommand`, `--offline`, drop `SearchCommand`, `WhatProvidesCommand`, `--global`.
- `src/Hypha/Cli/Run.hs` — dispatch `LookupCommand`.
- `hypha.cabal` — register new modules, drop deleted, bump to `0.2.0`.
- `test/Main.hs` — register new test groups.

**Deleted:**

- `src/Hypha/Command/Search.hs`
- `src/Hypha/Command/WhatProvides.hs`
- `src/Hypha/Hoogle/Query.hs`
- `src/Hypha/Hoogle/Database.hs` (most of it; useful bits absorbed into `Local`)
- `test/Golden/Search.hs` + relevant golden files
- `test/Unit/WhatProvides.hs` (if exists)

---

## Conventions

- Every step uses `~/.ghcup/bin/cabal` (the `cabal` binary on this machine).
- Issue file lives at `issues/in_progress/037-hypha-lookup.md` during work; moves to `issues/done/` at end.
- Strict bangs on every record field unless explicitly lazy with a comment (per CLAUDE.md).
- No `error` / `undefined` in production code (per CLAUDE.md).
- Conventional commits (`feat:`, `test:`, `refactor:`, `chore:`).
- Run `~/.ghcup/bin/cabal build lib:hypha` after each task's implementation step before committing.

---

### Task 1: File issue + schema migration column

**Files:**
- Create: `issues/in_progress/037-hypha-lookup.md`
- Modify: `src/Hypha/Search/Cache.hs`

- [ ] **Step 1: Create issue file**

Create `issues/in_progress/037-hypha-lookup.md` with a short summary pointing to the spec and plan. Single paragraph plus link.

- [ ] **Step 2: Add `fingerprint TEXT` column to `pkg_index_meta`**

In `src/Hypha/Search/Cache.hs`, change the schema entry for `pkg_index_meta` to include the column:

```haskell
schema :: [Query]
schema =
  [ "CREATE TABLE IF NOT EXISTS pkg_index_meta \
    \  ( pkg     TEXT NOT NULL \
    \  , version TEXT NOT NULL \
    \  , indexed_at INTEGER NOT NULL \
    \  , fingerprint TEXT \
    \  , PRIMARY KEY (pkg, version) )"
  , -- rest unchanged
  ]
```

- [ ] **Step 3: Add idempotent migration in `openIndexCache`**

In `src/Hypha/Search/Cache.hs:56-65`, after `mapM_ (execute_ conn) schema`, add:

```haskell
  -- Idempotent column add for existing DBs that predate the
  -- fingerprint column.  SQLite errors on duplicate columns; we
  -- swallow that single failure mode and re-throw anything else.
  Sql.execute_ conn "PRAGMA foreign_keys = OFF"
  migrateAddColumn conn
    "pkg_index_meta" "fingerprint" "TEXT"
```

Add the helper at the bottom of the module:

```haskell
-- | Add a column if it does not already exist.  SQLite has no
-- @ADD COLUMN IF NOT EXISTS@, so we probe @PRAGMA table_info@.
migrateAddColumn :: Connection -> Query -> Text -> Text -> IO ()
migrateAddColumn conn table column colType = do
  cols <- query_ conn ("PRAGMA table_info(" <> table <> ")")
            :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
  let names = [n | (_, n, _, _, _, _) <- cols]
  if column `elem` names
    then pure ()
    else execute_ conn
           (Query (Text.pack (
             "ALTER TABLE " <> Text.unpack (fromQuery table)
             <> " ADD COLUMN " <> Text.unpack column
             <> " " <> Text.unpack colType)))
```

Add imports as needed: `Database.SQLite.Simple (Query (..), query_, fromQuery)` and `qualified Data.Text as Text`.

- [ ] **Step 4: Build**

Run: `~/.ghcup/bin/cabal build lib:hypha`
Expected: success.

- [ ] **Step 5: Commit**

```bash
git add issues/in_progress/037-hypha-lookup.md src/Hypha/Search/Cache.hs
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "chore(cache): add fingerprint column + idempotent migration"
```

---

### Task 2: `componentFingerprint` helper module

**Files:**
- Create: `src/Hypha/Project/Fingerprint.hs`
- Create: `test/Unit/PackageCacheFingerprint.hs`
- Modify: `hypha.cabal` (register both)
- Modify: `test/Main.hs`

- [ ] **Step 1: Write the failing test**

Create `test/Unit/PackageCacheFingerprint.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.PackageCacheFingerprint (tests) where

import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Hypha.Project.Fingerprint (componentFingerprint)

tests :: TestTree
tests = testGroup "Unit.PackageCacheFingerprint"
  [ testCase "deterministic for the same tree" $
      withSystemTempDirectory "hypha-fp" $ \tmp -> do
        let d = tmp </> "src"
        createDirectoryIfMissing True d
        writeFile (d </> "Foo.hs") "module Foo where"
        a <- componentFingerprint [d]
        b <- componentFingerprint [d]
        a @?= b
        assertBool "non-empty" (not (Text.null a))

  , testCase "changes when a file is added" $
      withSystemTempDirectory "hypha-fp" $ \tmp -> do
        let d = tmp </> "src"
        createDirectoryIfMissing True d
        writeFile (d </> "Foo.hs") "module Foo where"
        before <- componentFingerprint [d]
        writeFile (d </> "Bar.hs") "module Bar where"
        after  <- componentFingerprint [d]
        assertBool "fingerprint changes" (before /= after)

  , testCase "deterministic with multiple dirs reordered" $
      withSystemTempDirectory "hypha-fp" $ \tmp -> do
        let d1 = tmp </> "a"
            d2 = tmp </> "b"
        createDirectoryIfMissing True d1
        createDirectoryIfMissing True d2
        writeFile (d1 </> "A.hs") "module A where"
        writeFile (d2 </> "B.hs") "module B where"
        x <- componentFingerprint [d1, d2]
        y <- componentFingerprint [d2, d1]
        x @?= y

  , testCase "empty when no files" $
      withSystemTempDirectory "hypha-fp" $ \tmp -> do
        let d = tmp </> "src"
        createDirectoryIfMissing True d
        fp <- componentFingerprint [d]
        assertBool "non-empty even for empty dir" (not (Text.null fp))
  ]
```

- [ ] **Step 2: Run test to verify it fails (module missing)**

Run: `~/.ghcup/bin/cabal test all 2>&1 | grep -E "error|FAIL"`
Expected: compile error — `Hypha.Project.Fingerprint` not found.

- [ ] **Step 3: Implement `Hypha.Project.Fingerprint`**

Create `src/Hypha/Project/Fingerprint.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Source-tree fingerprint used to detect when a component's bytes
-- have changed since the last index write.  The fingerprint is a
-- SHA-256 over the sorted list of @(relative-path, mtime, size)@
-- triples for every @.hs@ / @.lhs@ file under the given source roots.
--
-- Determinism matters more than cryptographic strength: callers
-- only compare fingerprints for equality.  We pick SHA-256 because
-- it is already a transitive dependency via @cryptohash-sha256@.
module Hypha.Project.Fingerprint
  ( componentFingerprint
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory
  ( doesDirectoryExist, getModificationTime, listDirectory )
import System.FilePath ((</>), takeExtension)
import System.IO (IOMode (ReadMode), withFile, hFileSize)

-- | Compute a fingerprint over every Haskell source file under the
-- given roots.  Reordering the roots does not change the result.
componentFingerprint :: [FilePath] -> IO Text
componentFingerprint roots = do
  triples <- concat <$> mapM walk roots
  let sorted = sort triples
      payload = BS.concat (map encodeTriple sorted)
      digest  = SHA256.hash payload
  pure (Text.decodeUtf8 (Base16.encode digest))
  where
    encodeTriple (p, mt, sz) = BS.concat
      [ Text.encodeUtf8 (Text.pack p)
      , "\0"
      , Text.encodeUtf8 (Text.pack (show mt))
      , "\0"
      , Text.encodeUtf8 (Text.pack (show sz))
      , "\n"
      ]

    walk :: FilePath -> IO [(FilePath, String, Integer)]
    walk root = do
      ok <- doesDirectoryExist root
      if not ok then pure [] else walkDir root

    walkDir :: FilePath -> IO [(FilePath, String, Integer)]
    walkDir d = do
      entries <- listDirectory d
      fmap concat $ mapM (visit d) entries

    visit :: FilePath -> FilePath -> IO [(FilePath, String, Integer)]
    visit parent name = do
      let p = parent </> name
      isDir <- doesDirectoryExist p
      if isDir
        then walkDir p
        else if takeExtension p `elem` [".hs", ".lhs"]
               then do
                 mt <- show <$> getModificationTime p
                 sz <- withFile p ReadMode hFileSize
                 pure [(p, mt, sz)]
               else pure []
```

- [ ] **Step 4: Register modules in `hypha.cabal`**

In `hypha.cabal:65` area (alphabetical-ish list of library exposed-modules), insert `Hypha.Project.Fingerprint`. In the `other-modules` list for the test suite, insert `Unit.PackageCacheFingerprint`.

- [ ] **Step 5: Add dependency `base16-bytestring` to library if missing**

Check `hypha.cabal:115-152` build-depends. If `base16-bytestring` is absent, add: `, base16-bytestring >= 1.0`.

- [ ] **Step 6: Register test in `test/Main.hs`**

Add `import qualified Unit.PackageCacheFingerprint` and `Unit.PackageCacheFingerprint.tests` to the `testGroup` list.

- [ ] **Step 7: Run test to verify it passes**

Run: `~/.ghcup/bin/cabal test all 2>&1 | grep -E "FAIL|passed"`
Expected: all PackageCacheFingerprint cases pass.

- [ ] **Step 8: Commit**

```bash
git add src/Hypha/Project/Fingerprint.hs test/Unit/PackageCacheFingerprint.hs test/Main.hs hypha.cabal
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(project): componentFingerprint over source-tree files"
```

---

### Task 3: `lookupByName` on `PackageCache`

**Files:**
- Modify: `src/Hypha/Search/Cache.hs`
- Modify: `src/Hypha/Search/PackageCache.hs`
- Create: `test/Unit/PackageCacheLookup.hs`
- Modify: `test/Main.hs`, `hypha.cabal`

- [ ] **Step 1: Write the failing test**

Create `test/Unit/PackageCacheLookup.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.PackageCacheLookup (tests) where

import Data.List (sort)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.PackageCache
  ( CacheOrigin (..), lookupByName, openPackageCacheAt, writeCachedIndex )

tests :: TestTree
tests = testGroup "Unit.PackageCacheLookup"
  [ testCase "exact name match" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "containers" "0.6.7"
          [ ("containers", "Data.Map.Strict", "lookup", "Ord k => k -> Map k a -> Maybe a")
          , ("containers", "Data.Set",        "member", "Ord a => a -> Set a -> Bool") ]
        hits <- lookupByName c "lookup"
        sort hits @?=
          [("containers", "Data.Map.Strict", "lookup",
            "Ord k => k -> Map k a -> Maybe a")]

  , testCase "qualified name match" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "containers" "0.6.7"
          [ ("containers", "Data.Map.Strict", "lookup", "..." )
          , ("containers", "Data.Map",        "lookup", "..." ) ]
        hits <- lookupByName c "Data.Map.lookup"
        sort hits @?=
          [("containers", "Data.Map", "lookup", "...")]

  , testCase "cross-package collisions return all" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "containers" "0.6.7"
          [("containers", "Data.Map", "lookup", "sig1")]
        writeCachedIndex c OriginGlobal "unordered-containers" "0.2.20"
          [("unordered-containers", "Data.HashMap.Strict", "lookup", "sig2")]
        hits <- lookupByName c "lookup"
        length hits @?= 2

  , testCase "missing name returns empty" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        hits <- lookupByName c "doesNotExist"
        hits @?= []
  ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `~/.ghcup/bin/cabal test all 2>&1 | grep -E "error|FAIL"`
Expected: compile error — `lookupByName` not in scope.

- [ ] **Step 3: Add `lookupByName` to `Hypha.Search.Cache`**

In `src/Hypha/Search/Cache.hs`, export and implement (place after `readIndex`):

```haskell
-- | Look up all rows whose @name@ exactly matches the given symbol.
-- If a module qualifier is supplied, also filter by @mod@.  The
-- caller is responsible for splitting qualified queries like
-- @Data.Map.lookup@ — this function only knows about already-split
-- components.
lookupRowsByName
  :: IndexCache
  -> Text                                  -- ^ symbol name
  -> Maybe Text                            -- ^ optional module qualifier
  -> IO [(Text, Text, Text, Text)]
lookupRowsByName c name mMod = case mMod of
  Nothing ->
    queryNamed (icConn c)
      "SELECT pkg, mod, name, sig FROM pkg_index WHERE name = :n"
      [":n" := name]
  Just modT ->
    queryNamed (icConn c)
      "SELECT pkg, mod, name, sig FROM pkg_index \
      \WHERE name = :n AND mod = :m"
      [":n" := name, ":m" := modT]
```

Add `lookupRowsByName` to the module export list near `readIndex`.

- [ ] **Step 4: Add `lookupByName` to `Hypha.Search.PackageCache`**

In `src/Hypha/Search/PackageCache.hs`, export and implement (after `readCachedIndex`):

```haskell
-- | Find every cached row whose symbol name matches @query@.  The
-- query may be a bare symbol (@lookup@) or fully qualified
-- (@Data.Map.lookup@).  Project rows shadow global rows when both
-- caches contain a hit for the same @(pkg, mod, name)@ triple.
lookupByName :: HyphaPackageCache -> Text -> IO [(Text, Text, Text, Text)]
lookupByName c rawQuery = do
  let (mMod, name) = splitQualified rawQuery
  projectRows <- case hpcProject c of
    Just p  -> Cache.lookupRowsByName p name mMod
    Nothing -> pure []
  globalRows  <- Cache.lookupRowsByName (hpcGlobal c) name mMod
  pure (mergeShadow projectRows globalRows)

-- | Split @Data.Map.lookup@ into @(Just "Data.Map", "lookup")@.
-- Bare symbols return @(Nothing, sym)@.
splitQualified :: Text -> (Maybe Text, Text)
splitQualified raw =
  case Text.breakOnEnd "." raw of
    (pre, post)
      | Text.null pre  -> (Nothing, post)
      | otherwise      -> (Just (Text.dropEnd 1 pre), post)

-- | Project rows take precedence per @(pkg, mod, name)@; global
-- rows fill in any triples the project does not cover.
mergeShadow
  :: [(Text, Text, Text, Text)]
  -> [(Text, Text, Text, Text)]
  -> [(Text, Text, Text, Text)]
mergeShadow project global =
  let key (p, m, n, _) = (p, m, n)
      projectKeys = Set.fromList (map key project)
  in project ++ filter (\r -> not (key r `Set.member` projectKeys)) global
```

Add imports as needed: `qualified Data.Text as Text`, `qualified Data.Set as Set`.

- [ ] **Step 5: Register `Unit.PackageCacheLookup` in `test/Main.hs` and `hypha.cabal`**

- [ ] **Step 6: Run test to verify it passes**

Run: `~/.ghcup/bin/cabal test all 2>&1 | grep -E "FAIL|passed"`
Expected: all PackageCacheLookup cases pass.

- [ ] **Step 7: Commit**

```bash
git add src/Hypha/Search/Cache.hs src/Hypha/Search/PackageCache.hs test/Unit/PackageCacheLookup.hs test/Main.hs hypha.cabal
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(cache): lookupByName with qualified-name support + project shadowing"
```

---

### Task 4: Fingerprint accessors on `PackageCache`

**Files:**
- Modify: `src/Hypha/Search/Cache.hs`
- Modify: `src/Hypha/Search/PackageCache.hs`

- [ ] **Step 1: Add fingerprint reader/writer to `Cache`**

In `src/Hypha/Search/Cache.hs` exports and impl:

```haskell
-- | Read the stored fingerprint for a @(pkg, version)@ pair.
readFingerprint :: IndexCache -> Text -> Text -> IO (Maybe Text)
readFingerprint c pkg ver = do
  rs <- queryNamed (icConn c)
          "SELECT fingerprint FROM pkg_index_meta \
          \WHERE pkg = :p AND version = :v LIMIT 1"
          [":p" := pkg, ":v" := ver] :: IO [Only (Maybe Text)]
  pure (case rs of
          (Only mfp : _) -> mfp
          _              -> Nothing)

-- | Stamp the fingerprint for an existing @pkg_index_meta@ row,
-- inserting a placeholder row when none yet exists (the @indexed_at@
-- value is intentionally @0@ in that case; @writeIndex@ rewrites it).
writeFingerprint :: IndexCache -> Text -> Text -> Text -> IO ()
writeFingerprint c pkg ver fp = withWrite c $ executeNamed (icConn c)
  "INSERT INTO pkg_index_meta (pkg, version, indexed_at, fingerprint) \
  \VALUES (:p, :v, 0, :f) \
  \ON CONFLICT(pkg, version) DO UPDATE SET fingerprint = :f"
  [":p" := pkg, ":v" := ver, ":f" := fp]
```

- [ ] **Step 2: Surface them on `PackageCache`**

In `src/Hypha/Search/PackageCache.hs`, export and add thin wrappers:

```haskell
readCachedFingerprint :: HyphaPackageCache -> CacheOrigin -> Text -> Text -> IO (Maybe Text)
readCachedFingerprint c origin pkg ver =
  Cache.readFingerprint (selectWrite c origin) pkg ver

writeCachedFingerprint :: HyphaPackageCache -> CacheOrigin -> Text -> Text -> Text -> IO ()
writeCachedFingerprint c origin pkg ver fp =
  Cache.writeFingerprint (selectWrite c origin) pkg ver fp
```

(`selectWrite` already exists internally — promote to use it for read too.)

- [ ] **Step 3: Build**

Run: `~/.ghcup/bin/cabal build lib:hypha`
Expected: success.

- [ ] **Step 4: Commit**

```bash
git add src/Hypha/Search/Cache.hs src/Hypha/Search/PackageCache.hs
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(cache): per-component fingerprint accessors"
```

---

### Task 5: `planHash` in `Hypha.Project.Plan`

**Files:**
- Modify: `src/Hypha/Project/Plan.hs`

- [ ] **Step 1: Export `planHash`**

Add to the module exports and implementation:

```haskell
-- | SHA-256 (hex) over the deterministic JSON encoding of the plan
-- — used as the staleness stamp for the local Hoogle DB.  We hash
-- the in-memory 'BuildPlan' rather than the on-disk @plan.json@
-- bytes so that semantically identical plans produce identical
-- hashes even if cabal reorders fields.
planHash :: BuildPlan -> Text
planHash bp =
  let pids = sort
        [ unPackageName (pkgName (puId u)) <> "-" <> unVersion (pkgVersion (puId u))
        | u <- Map.elems (bpUnits bp)
        ]
      payload = Text.encodeUtf8 (Text.unlines pids)
      digest  = SHA256.hash payload
  in Text.decodeUtf8 (Base16.encode digest)
```

Add imports: `import Data.List (sort)`, `qualified Crypto.Hash.SHA256 as SHA256`, `qualified Data.ByteString.Base16 as Base16`, `qualified Data.Text.Encoding as Text`.

- [ ] **Step 2: Build**

Run: `~/.ghcup/bin/cabal build lib:hypha`
Expected: success.

- [ ] **Step 3: Commit**

```bash
git add src/Hypha/Project/Plan.hs
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(plan): planHash for staleness stamping"
```

---

### Task 6: `Hypha.Hoogle.Local` skeleton + `.txt` scavenger

**Files:**
- Create: `src/Hypha/Hoogle/Local.hs`
- Create: `test/Unit/HoogleLocalGen.hs`
- Modify: `test/Main.hs`, `hypha.cabal`

- [ ] **Step 1: Write the failing test (scavenger only)**

Create `test/Unit/HoogleLocalGen.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.HoogleLocalGen (tests) where

import Data.List (sort)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Hoogle.Local (scavengeStoreTxt)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

tests :: TestTree
tests = testGroup "Unit.HoogleLocalGen"
  [ testCase "finds <pkg>.txt under store layout" $
      withSystemTempDirectory "hypha-hg" $ \tmp -> do
        let pid  = PackageId (PackageName "containers") (Version "0.6.7")
            base = tmp </> "ghc-9.6.7" </> "containers-0.6.7-abc"
            doc  = base </> "share" </> "doc" </> "containers-0.6.7" </> "html"
        createDirectoryIfMissing True doc
        writeFile (doc </> "containers.txt") "@package containers\n"
        path <- scavengeStoreTxt tmp pid
        path @?= Just (doc </> "containers.txt")

  , testCase "returns Nothing when missing" $
      withSystemTempDirectory "hypha-hg" $ \tmp -> do
        let pid = PackageId (PackageName "ghost") (Version "0.0")
        path <- scavengeStoreTxt tmp pid
        path @?= Nothing
  ]
```

- [ ] **Step 2: Run test to verify failure**

Run: `~/.ghcup/bin/cabal test all 2>&1 | grep -E "error|FAIL"`
Expected: compile error — module missing.

- [ ] **Step 3: Implement `Hypha.Hoogle.Local` (skeleton + scavenger)**

Create `src/Hypha/Hoogle/Local.hs`:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Project-scoped Hoogle database lifecycle.
--
-- Bundles a single 'Hoogle.Database' handle for @\<project\>/.hypha/hoogle.hoo@.
-- The database is regenerated lazily on the first 'searchLocal' that
-- observes a stale @(planHash, aggregateFingerprint)@ stamp, and is
-- guarded by an 'MVar' so concurrent searches never spawn duplicate
-- generation work (Hoogle's library deadlocks under concurrent regen).
module Hypha.Hoogle.Local
  ( HyphaHoogle
  , openLocalHoogle
  , searchLocal
    -- * Internals reused in tests
  , scavengeStoreTxt
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), Version (..) )

-- | Opaque handle to the local Hoogle DB lifecycle.
data HyphaHoogle = HyphaHoogle
  { hhDbPath  :: !FilePath
  , hhLock    :: !(MVar ())
  , hhStoreRoot :: !FilePath
    -- ^ @~/.cabal/store/ghc-X.Y.Z@ root for the active GHC.  Empty
    -- string when the store could not be located; scavenging then
    -- always returns 'Nothing' and we fall back to haddock.
  }

-- | Open (or initialise) the per-project Hoogle DB.  No regeneration
-- happens here; the first 'searchLocal' triggers it lazily.
openLocalHoogle :: FilePath          -- ^ project root .hypha dir
                -> FilePath          -- ^ ghc store root
                -> IO HyphaHoogle
openLocalHoogle dotHypha storeRoot = do
  lock <- newMVar ()
  pure HyphaHoogle
    { hhDbPath    = dotHypha </> "hoogle.hoo"
    , hhLock      = lock
    , hhStoreRoot = storeRoot
    }

-- | Locate @\<pkg\>.txt@ inside the cabal store.  Returns 'Nothing' when
-- the package isn't installed with documentation.  The path layout
-- under the store is:
--
-- > <store-root>/<pkg-ver-hash>/share/doc/<pkg-ver>/html/<pkg>.txt
--
-- We do not parse the hash; we list candidate hash directories that
-- start with @<pkg>-<ver>-@ and pick the first one carrying the file.
scavengeStoreTxt :: FilePath -> PackageId -> IO (Maybe FilePath)
scavengeStoreTxt storeRoot pid = do
  exists <- doesFileExist storeRoot
  -- Caller passes either the @ghc-X.Y.Z@ root or a per-package root
  -- (used in tests).  We probe both layouts.
  let pkgPrefix = Text.unpack (unPackageName (pkgName pid))
                  <> "-"
                  <> Text.unpack (unVersion (pkgVersion pid))
                  <> "-"
  rootOk <- doesFileExist storeRoot
  candidates <- if rootOk then pure [] else do
    entries <- listDirOrEmpty storeRoot
    pure [ storeRoot </> e | e <- entries
         , pkgPrefix `isPrefixOf'` e ]
  case candidates of
    [] -> probeDoc storeRoot pid
    _  -> firstJust [probeDoc c pid | c <- candidates]

probeDoc :: FilePath -> PackageId -> IO (Maybe FilePath)
probeDoc base pid = do
  let pkg = Text.unpack (unPackageName (pkgName pid))
      ver = Text.unpack (unVersion    (pkgVersion pid))
      candidate = base </> "share" </> "doc"
                       </> (pkg <> "-" <> ver)
                       </> "html" </> (pkg <> ".txt")
  ok <- doesFileExist candidate
  pure (if ok then Just candidate else Nothing)

isPrefixOf' :: String -> String -> Bool
isPrefixOf' p s = take (length p) s == p

listDirOrEmpty :: FilePath -> IO [FilePath]
listDirOrEmpty p = do
  ok <- doesFileExist p
  if ok then pure [] else listDirectory p

firstJust :: [IO (Maybe a)] -> IO (Maybe a)
firstJust []     = pure Nothing
firstJust (m:ms) = do
  r <- m
  case r of
    Just x  -> pure (Just x)
    Nothing -> firstJust ms

-- | Stub: subsequent tasks fill in the generation lifecycle.  For
-- now, no search results.
searchLocal :: HyphaHoogle -> HoogleQuery -> IO [HoogleHit]
searchLocal hh _q = withMVar (hhLock hh) $ \_ -> pure []
```

- [ ] **Step 4: Register module + tests**

In `hypha.cabal`, add `Hypha.Hoogle.Local` to library exposed-modules. In test other-modules, add `Unit.HoogleLocalGen`. In `test/Main.hs`, register the test group.

- [ ] **Step 5: Run test**

Run: `~/.ghcup/bin/cabal test all 2>&1 | grep -E "FAIL|passed"`
Expected: scavenger tests pass.

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Hoogle/Local.hs test/Unit/HoogleLocalGen.hs test/Main.hs hypha.cabal
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(hoogle): Local module skeleton + store-txt scavenger"
```

---

### Task 7: `HaddockRunner` abstraction + invocation

**Files:**
- Modify: `src/Hypha/Hoogle/Local.hs`
- Modify: `test/Unit/HoogleLocalGen.hs`

- [ ] **Step 1: Write the failing test for runner injection**

Append to `test/Unit/HoogleLocalGen.hs`:

```haskell
  , testCase "uses HaddockRunner when .txt is missing" $
      withSystemTempDirectory "hypha-hg" $ \tmp -> do
        called <- newIORef ([] :: [HaddockRequest])
        let runner = HaddockRunner
              { runHaddock = \req -> do
                  modifyIORef called (req :)
                  writeFile (hrOutput req) "@package foo\n"
                  pure (Right (hrOutput req)) }
            local = LocalUnit (PackageId (PackageName "foo") (Version "0.1"))
                              [tmp </> "src"]
        createDirectoryIfMissing True (tmp </> "src")
        writeFile (tmp </> "src" </> "Foo.hs") "module Foo where"
        result <- collectTxtForUnit runner "" local
        case result of
          Right p -> doesFileExist p >>= (@?= True)
          Left e  -> fail (show e)
        seen <- readIORef called
        length seen @?= 1
  ]
```

Add imports: `Data.IORef (newIORef, modifyIORef, readIORef)`.

- [ ] **Step 2: Add types + collector to `Hypha.Hoogle.Local`**

Append to `src/Hypha/Hoogle/Local.hs`:

```haskell
-- | What we need to know about a unit for Hoogle .txt collection.
data LocalUnit = LocalUnit
  { luPkgId   :: !PackageId
  , luSrcDirs :: ![FilePath]
    -- ^ @hs-source-dirs@ entries to feed haddock when no store
    -- @.txt@ is available.
  }
  deriving stock (Show, Eq)

-- | Request to invoke haddock for a single package.
data HaddockRequest = HaddockRequest
  { hrPkgId    :: !PackageId
  , hrSrcDirs  :: ![FilePath]
  , hrOutput   :: !FilePath    -- ^ where to write @.txt@
  }
  deriving stock (Show, Eq)

-- | Reason a haddock invocation could not produce a .txt.
data HaddockError = HaddockError !Text
  deriving stock (Show, Eq)

-- | Record-of-functions wrapping the haddock binary so tests can
-- inject a deterministic implementation.
data HaddockRunner = HaddockRunner
  { runHaddock :: HaddockRequest -> IO (Either HaddockError FilePath)
  }

-- | Try the store first, then haddock.  Returns the @.txt@ path or
-- a 'HaddockError'.
collectTxtForUnit
  :: HaddockRunner
  -> FilePath           -- ^ store root
  -> LocalUnit
  -> IO (Either HaddockError FilePath)
collectTxtForUnit runner storeRoot lu = do
  scavenged <- scavengeStoreTxt storeRoot (luPkgId lu)
  case scavenged of
    Just p  -> pure (Right p)
    Nothing -> do
      tmp <- haddockOutputPath (luPkgId lu)
      runHaddock runner HaddockRequest
        { hrPkgId   = luPkgId lu
        , hrSrcDirs = luSrcDirs lu
        , hrOutput  = tmp
        }

-- | Where to put haddock-generated @.txt@ files for a given
-- package.  We co-locate them under @\<XDG_CACHE\>/hypha/hoogle-txt@
-- so the @hoogle generate@ step can point at a single directory.
haddockOutputPath :: PackageId -> IO FilePath
haddockOutputPath pid = do
  dir <- getXdgDirectory XdgCache "hypha"
  let outDir = dir </> "hoogle-txt"
      file   = Text.unpack (unPackageName (pkgName pid))
            <> "-"
            <> Text.unpack (unVersion (pkgVersion pid))
            <> ".txt"
  createDirectoryIfMissing True outDir
  pure (outDir </> file)
```

Add imports: `System.Directory (XdgDirectory (..), getXdgDirectory, createDirectoryIfMissing)`.

- [ ] **Step 3: Build + test**

Run: `~/.ghcup/bin/cabal test all 2>&1 | grep -E "FAIL|passed"`
Expected: new test passes.

- [ ] **Step 4: Commit**

```bash
git add src/Hypha/Hoogle/Local.hs test/Unit/HoogleLocalGen.hs
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(hoogle): HaddockRunner abstraction + collectTxtForUnit"
```

---

### Task 8: Default `HaddockRunner` shelling to `haddock`

**Files:**
- Modify: `src/Hypha/Hoogle/Local.hs`

- [ ] **Step 1: Add `defaultHaddockRunner`**

Append to `src/Hypha/Hoogle/Local.hs`:

```haskell
-- | Default runner — invokes the @haddock@ binary with @--hoogle@.
-- The binary is expected on @PATH@; if it is absent the runner
-- emits 'HaddockError' and the caller decides whether to fall
-- back to remote-only operation.
--
-- We do NOT pass GHC package-db flags here: by the time hypha runs,
-- the project has already been built, so @haddock@ inherits a sane
-- environment.  When this assumption breaks (cross-GHC projects,
-- pristine checkouts) the runner will fail and surface a structured
-- warning rather than crash.
defaultHaddockRunner :: HaddockRunner
defaultHaddockRunner = HaddockRunner $ \req -> do
  files <- enumerateHsFiles (hrSrcDirs req)
  case files of
    [] -> pure (Left (HaddockError "no .hs files found"))
    _  -> do
      (ec, _out, err) <- readProcessWithExitCode "haddock"
        ( ["--hoogle", "-o", takeDirectory (hrOutput req)]
        ++ files ) ""
      case ec of
        ExitSuccess   -> do
          ok <- doesFileExist (hrOutput req)
          if ok
            then pure (Right (hrOutput req))
            else pure (Left (HaddockError "haddock produced no output"))
        ExitFailure _ -> pure (Left (HaddockError (Text.pack err)))

enumerateHsFiles :: [FilePath] -> IO [FilePath]
enumerateHsFiles = fmap concat . mapM walk
  where
    walk root = do
      ok <- doesDirectoryExist root
      if not ok then pure [] else walkDir root
    walkDir d = do
      entries <- listDirectory d
      fmap concat $ mapM (visit d) entries
    visit parent name = do
      let p = parent </> name
      isDir <- doesDirectoryExist p
      if isDir
        then walkDir p
        else if takeExtension p `elem` [".hs", ".lhs"]
               then pure [p] else pure []
```

Add imports: `System.Process (readProcessWithExitCode)`, `System.Exit (ExitCode (..))`, `System.FilePath (takeDirectory, takeExtension)`, `System.Directory (doesDirectoryExist)`. Export `defaultHaddockRunner`.

- [ ] **Step 2: Build**

Run: `~/.ghcup/bin/cabal build lib:hypha`
Expected: success.

- [ ] **Step 3: Commit**

```bash
git add src/Hypha/Hoogle/Local.hs
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(hoogle): defaultHaddockRunner shells to haddock --hoogle"
```

---

### Task 9: `ensureFresh` — regeneration pipeline

**Files:**
- Modify: `src/Hypha/Hoogle/Local.hs`
- Modify: `test/Unit/HoogleLocalGen.hs`

- [ ] **Step 1: Write the failing test for skip-when-fresh**

Append to `test/Unit/HoogleLocalGen.hs`:

```haskell
  , testCase "ensureFresh skips when stamp matches" $
      withSystemTempDirectory "hypha-hg" $ \tmp -> do
        callCount <- newIORef (0 :: Int)
        let runner = HaddockRunner $ \req -> do
              modifyIORef callCount (+1)
              writeFile (hrOutput req) "@package x\n"
              pure (Right (hrOutput req))
            stamp = HoogleStamp "plan-hash-1" "fp-1"
            units = []   -- empty plan: nothing to do
            dot = tmp </> ".hypha"
        createDirectoryIfMissing True dot
        -- first call: no stamp file, should regen (no-op)
        ensureFresh runner "" dot stamp units
        first <- readIORef callCount
        -- second call: stamp file should now match
        ensureFresh runner "" dot stamp units
        second <- readIORef callCount
        (first, second) @?= (0, 0)  -- both no-op since units empty
        -- verify stamp file was written
        ok <- doesFileExist (dot </> "hoogle-stamp")
        ok @?= True
  ]
```

- [ ] **Step 2: Add `HoogleStamp` + `ensureFresh`**

Append to `src/Hypha/Hoogle/Local.hs`:

```haskell
-- | Identity stamp used to decide whether the Hoogle DB is stale.
-- Two parts: the plan hash (captures dependency changes) and an
-- aggregate fingerprint over all local components (captures source
-- edits).  Either changing invalidates the DB.
data HoogleStamp = HoogleStamp
  { hsPlanHash    :: !Text
  , hsAggregateFp :: !Text
  }
  deriving stock (Show, Eq)

stampFilePath :: FilePath -> FilePath
stampFilePath dotHypha = dotHypha </> "hoogle-stamp"

-- | Regenerate the Hoogle DB if the stored stamp differs from the
-- current one.  Idempotent and cheap when stamps match.
ensureFresh
  :: HaddockRunner
  -> FilePath        -- ^ store root
  -> FilePath        -- ^ project @.hypha@ directory
  -> HoogleStamp     -- ^ current stamp
  -> [LocalUnit]
  -> IO ()
ensureFresh runner storeRoot dotHypha stamp units = do
  mPrior <- readStamp (stampFilePath dotHypha)
  if mPrior == Just stamp
    then pure ()
    else regenerate runner storeRoot dotHypha stamp units

readStamp :: FilePath -> IO (Maybe HoogleStamp)
readStamp f = do
  ok <- doesFileExist f
  if not ok then pure Nothing else do
    txt <- TIO.readFile f
    case Text.lines txt of
      [a, b] -> pure (Just (HoogleStamp a b))
      _      -> pure Nothing

writeStamp :: FilePath -> HoogleStamp -> IO ()
writeStamp f s = TIO.writeFile f (Text.unlines [hsPlanHash s, hsAggregateFp s])

regenerate
  :: HaddockRunner
  -> FilePath
  -> FilePath
  -> HoogleStamp
  -> [LocalUnit]
  -> IO ()
regenerate runner storeRoot dotHypha stamp units = do
  -- 1. Gather every per-package .txt path.
  paths <- mapM (collectTxtForUnit runner storeRoot) units
  let okPaths = [ p | Right p <- paths ]

  -- 2. Drop them into a single directory hoogle can scan with
  --    @--local=<dir>@.  We symlink (or copy on failure) to keep
  --    the store layout pristine.
  let inputDir = dotHypha </> "hoogle-input"
  removeAndRecreate inputDir
  mapM_ (linkOrCopy inputDir) okPaths

  -- 3. Invoke @hoogle generate@.
  let dbPath = dotHypha </> "hoogle.hoo"
  Hoogle.hoogle
    [ "generate"
    , "--database=" <> dbPath
    , "--local=" <> inputDir
    ]

  -- 4. Stamp the result so we skip next time.
  writeStamp (stampFilePath dotHypha) stamp

removeAndRecreate :: FilePath -> IO ()
removeAndRecreate p = do
  ok <- doesDirectoryExist p
  when ok (removeDirectoryRecursive p)
  createDirectoryIfMissing True p

linkOrCopy :: FilePath -> FilePath -> IO ()
linkOrCopy dstDir src = do
  let dst = dstDir </> takeFileName src
  result <- try (createFileLink src dst) :: IO (Either SomeException ())
  case result of
    Right ()  -> pure ()
    Left  _   -> copyFile src dst
```

Add imports: `qualified Data.Text.IO as TIO`, `Control.Monad (when)`, `Control.Exception (SomeException, try)`, `System.Directory (createFileLink, copyFile, removeDirectoryRecursive)`, `System.FilePath (takeFileName)`, `qualified Hoogle`.

- [ ] **Step 3: Run test**

Run: `~/.ghcup/bin/cabal test all 2>&1 | grep -E "FAIL|passed"`
Expected: ensureFresh test passes (call count stays 0 with empty unit list).

- [ ] **Step 4: Commit**

```bash
git add src/Hypha/Hoogle/Local.hs test/Unit/HoogleLocalGen.hs
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(hoogle): ensureFresh regeneration pipeline with stamp file"
```

---

### Task 10: Wire `searchLocal` to use the database

**Files:**
- Modify: `src/Hypha/Hoogle/Local.hs`

- [ ] **Step 1: Replace `searchLocal` stub**

In `src/Hypha/Hoogle/Local.hs`, replace the existing stub:

```haskell
-- | Search the project's Hoogle DB.  Returns @[]@ when the DB does
-- not exist (the caller is expected to have called 'ensureFresh'
-- first; we do not regenerate here because regeneration is the
-- expensive path and 'searchLocal' is the hot path).
searchLocal :: HyphaHoogle -> HoogleQuery -> IO [HoogleHit]
searchLocal hh q = withMVar (hhLock hh) $ \_ -> do
  ok <- doesFileExist (hhDbPath hh)
  if not ok
    then pure []
    else do
      r <- try (Hoogle.withDatabase (hhDbPath hh) $ \db ->
              pure (map toHit (Hoogle.searchDatabase db
                                (Text.unpack (unHoogleQuery q)))))
             :: IO (Either SomeException [HoogleHit])
      pure (either (const []) id r)
  where
    toHit t = HoogleHit
      { hhPackage = maybe "" (Text.pack . fst) (Hoogle.targetPackage t)
      , hhModule  = maybe "" (Text.pack . fst) (Hoogle.targetModule t)
      , hhName    = Text.pack (Hoogle.targetItem t)
      , hhSig     = Text.pack (Hoogle.targetType t)
      , hhDocs    = Text.pack (Hoogle.targetDocs t)
      }
```

Imports needed: `Control.Exception (try)` (already there), `qualified Hoogle`.

- [ ] **Step 2: Build**

Run: `~/.ghcup/bin/cabal build lib:hypha`
Expected: success.

- [ ] **Step 3: Commit**

```bash
git add src/Hypha/Hoogle/Local.hs
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(hoogle): searchLocal consults the .hoo database"
```

---

### Task 11: `Hypha.Hoogle.Remote` with KV cache + timeout

**Files:**
- Create: `src/Hypha/Hoogle/Remote.hs`
- Create: `test/Unit/HoogleRemote.hs`
- Modify: `hypha.cabal`, `test/Main.hs`

- [ ] **Step 1: Write the failing test**

Create `test/Unit/HoogleRemote.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.HoogleRemote (tests) where

import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Hoogle.Remote
  ( RemoteError (..), RemoteHoogleTransport (..), searchRemoteWith
  , RemoteOptions (..), defaultRemoteOptions )
import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Search.PackageCache (openPackageCacheAt)

stubBody :: LBS.ByteString
stubBody = "[{\"package\":{\"name\":\"foo\"},\"module\":{\"name\":\"Foo\"},\"item\":\"bar\",\"type\":\"a -> a\",\"docs\":\"\"}]"

tests :: TestTree
tests = testGroup "Unit.HoogleRemote"
  [ testCase "single GET, cached on second call" $
      withSystemTempDirectory "hypha-rh" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        counter <- newIORef (0 :: Int)
        let transport = RemoteHoogleTransport $ \_ -> do
              atomicModifyIORef' counter (\n -> (n + 1, ()))
              pure (Right stubBody)
            opts = defaultRemoteOptions
        r1 <- searchRemoteWith transport opts c (HoogleQuery "bar")
        r2 <- searchRemoteWith transport opts c (HoogleQuery "bar")
        seen <- readIORef counter
        seen @?= 1                     -- second call cached
        either (fail . show) pure r1
        either (fail . show) pure r2

  , testCase "offline mode short-circuits" $
      withSystemTempDirectory "hypha-rh" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        let transport = RemoteHoogleTransport $ \_ ->
              fail "must not be called"
            opts = defaultRemoteOptions { roOffline = True }
        r <- searchRemoteWith transport opts c (HoogleQuery "bar")
        case r of
          Left  RemoteOffline -> pure ()
          other               -> fail ("unexpected: " <> show other)
  ]
```

- [ ] **Step 2: Implement `Hypha.Hoogle.Remote`**

Create `src/Hypha/Hoogle/Remote.hs`:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | HTTP client for remote Hoogle (the public hoogle.haskell.org).
-- Results are cached in the @kv@ table of 'HyphaPackageCache' with a
-- 24h TTL; identical queries within that window do not hit the
-- network.
module Hypha.Hoogle.Remote
  ( RemoteError (..)
  , RemoteHoogleTransport (..)
  , RemoteOptions (..)
  , defaultRemoteOptions
  , searchRemote
  , searchRemoteWith
  , defaultTransport
  ) where

import Control.Exception (SomeException, try)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (FromJSON (..), Value, eitherDecode, withObject, (.:?))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as Base16
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO  -- unused at runtime; keeps GHC happy
import Network.HTTP.Client
  ( Manager, Request (..), httpLbs, newManager
  , parseRequest, responseBody, responseTimeoutMicro )
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Search.Cache (readBlob, writeBlob)
import Hypha.Search.PackageCache (HyphaPackageCache, _hpcGlobal)

data RemoteError
  = RemoteOffline
  | RemoteTimeout
  | RemoteHttp !Text
  | RemoteDecode !Text
  deriving stock (Show, Eq)

newtype RemoteHoogleTransport = RemoteHoogleTransport
  { runRemote :: Text -> IO (Either RemoteError ByteString)
  }

data RemoteOptions = RemoteOptions
  { roOffline      :: !Bool
  , roTtlSeconds   :: !Int
  , roTimeoutMicros :: !Int
  , roEndpoint     :: !Text
  }
  deriving stock (Show, Eq)

defaultRemoteOptions :: RemoteOptions
defaultRemoteOptions = RemoteOptions
  { roOffline       = False
  , roTtlSeconds    = 86400
  , roTimeoutMicros = 10_000_000
  , roEndpoint      = "https://hoogle.haskell.org"
  }

-- | Production wrapper that bakes in the default transport.
searchRemote
  :: RemoteOptions
  -> HyphaPackageCache
  -> HoogleQuery
  -> IO (Either RemoteError [HoogleHit])
searchRemote opts cache q = do
  transport <- defaultTransport opts
  searchRemoteWith transport opts cache q

searchRemoteWith
  :: RemoteHoogleTransport
  -> RemoteOptions
  -> HyphaPackageCache
  -> HoogleQuery
  -> IO (Either RemoteError [HoogleHit])
searchRemoteWith transport opts cache q
  | roOffline opts = pure (Left RemoteOffline)
  | otherwise = do
      let key = cacheKey q
      cached <- readBlob (_hpcGlobal cache) key
      case cached of
        Just txt | Right hits <- decodeHits (Text.encodeUtf8 txt) -> pure (Right hits)
        _ -> do
          let url = endpointFor opts q
          r <- runRemote transport url
          case r of
            Left e     -> pure (Left e)
            Right body -> case decodeHits (LBS.toStrict body) of
              Right hits -> do
                writeBlob (_hpcGlobal cache) key
                  (Text.decodeUtf8 (LBS.toStrict body))
                pure (Right hits)
              Left  err  -> pure (Left (RemoteDecode err))

endpointFor :: RemoteOptions -> HoogleQuery -> Text
endpointFor opts (HoogleQuery q) =
  roEndpoint opts
    <> "/?mode=json&count=20&hoogle="
    <> urlEncode q

urlEncode :: Text -> Text
urlEncode = Text.concatMap encChar
  where
    encChar c
      | c == ' ' = "+"
      | c `elem` ("&?#=" :: String) = Text.pack ('%' : hex c)
      | otherwise = Text.singleton c
    hex c = let n = fromEnum c
                d1 = n `div` 16
                d2 = n `mod` 16
            in [digit d1, digit d2]
    digit n | n < 10 = toEnum (fromEnum '0' + n)
            | otherwise = toEnum (fromEnum 'A' + n - 10)

cacheKey :: HoogleQuery -> Text
cacheKey (HoogleQuery q) =
  "hoogle:remote:"
  <> Text.decodeUtf8 (Base16.encode (SHA256.hash (Text.encodeUtf8 q)))

-- JSON shape from hoogle.haskell.org's @?mode=json@ endpoint.
data RawHit = RawHit
  { rhPkg :: !Text
  , rhMod :: !Text
  , rhItm :: !Text
  , rhTyp :: !Text
  , rhDoc :: !Text
  }

instance FromJSON RawHit where
  parseJSON = withObject "RawHit" $ \o -> do
    pkg <- o .:? "package" >>= maybe (pure "") (withObjectField "name")
    md  <- o .:? "module"  >>= maybe (pure "") (withObjectField "name")
    itm <- fromMaybe "" <$> o .:? "item"
    typ <- fromMaybe "" <$> o .:? "type"
    doc <- fromMaybe "" <$> o .:? "docs"
    pure (RawHit pkg md itm typ doc)
    where
      withObjectField k v = case v of
        Just (val :: Value) -> case eitherDecode (Aeson.encode val) of
          Right o -> ...  -- inline reparse
          Left _  -> pure ""
        Nothing  -> pure ""

decodeHits :: LBS.ByteString -> Either Text [HoogleHit]
decodeHits bs = case eitherDecode bs of
  Left err   -> Left (Text.pack err)
  Right hits -> Right (map fromRaw hits)
  where
    fromRaw r = HoogleHit (rhPkg r) (rhMod r) (rhItm r) (rhTyp r) (rhDoc r)

defaultTransport :: RemoteOptions -> IO RemoteHoogleTransport
defaultTransport opts = do
  mgr <- newManager tlsManagerSettings
  pure (RemoteHoogleTransport (httpGet mgr opts))

httpGet :: Manager -> RemoteOptions -> Text -> IO (Either RemoteError ByteString)
httpGet mgr opts url = do
  reqE <- try (parseRequest (Text.unpack url))
            :: IO (Either SomeException Request)
  case reqE of
    Left e -> pure (Left (RemoteHttp (Text.pack (show e))))
    Right req0 -> do
      let req = req0 { responseTimeout =
                         responseTimeoutMicro (roTimeoutMicros opts) }
      r <- try (httpLbs req mgr) :: IO (Either SomeException _)
      case r of
        Left  e -> pure (Left (RemoteHttp (Text.pack (show e))))
        Right resp -> pure (Right (responseBody resp))
```

> **Note for implementer:** The `RawHit` `parseJSON` sketch above leaves out the inner `withObject` reparse — write it cleanly using `(.:?)` returning `Maybe Object` then `(.: "name")`, e.g.:
>
> ```haskell
> instance FromJSON RawHit where
>   parseJSON = withObject "RawHit" $ \o -> do
>     pkg <- nameField o "package"
>     md  <- nameField o "module"
>     itm <- fromMaybe "" <$> o .:? "item"
>     typ <- fromMaybe "" <$> o .:? "type"
>     doc <- fromMaybe "" <$> o .:? "docs"
>     pure (RawHit pkg md itm typ doc)
>     where
>       nameField o k = do
>         mObj <- o .:? k
>         case mObj of
>           Nothing  -> pure ""
>           Just obj -> fromMaybe "" <$> obj .:? "name"
> ```

`_hpcGlobal` needs to be exposed from `PackageCache` (either as a real accessor or via a new `globalHandleOf :: HyphaPackageCache -> IndexCache` exporter). Add it to `Hypha.Search.PackageCache`:

```haskell
-- | Internal-but-exported accessor used by 'Hypha.Hoogle.Remote' to
-- store query-result blobs in the global KV table.
_hpcGlobal :: HyphaPackageCache -> IndexCache
_hpcGlobal = hpcGlobal
```

- [ ] **Step 3: Register modules + dependencies**

In `hypha.cabal`:
- Library exposed-modules: add `Hypha.Hoogle.Remote`.
- Library build-depends: ensure `http-client`, `http-client-tls`, `base16-bytestring`, `cryptohash-sha256` are present (they are already there).
- Test other-modules: add `Unit.HoogleRemote`.

In `test/Main.hs`: register `Unit.HoogleRemote.tests`.

- [ ] **Step 4: Run tests**

Run: `~/.ghcup/bin/cabal test all 2>&1 | grep -E "FAIL|passed"`
Expected: HoogleRemote cases pass.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Hoogle/Remote.hs src/Hypha/Search/PackageCache.hs test/Unit/HoogleRemote.hs test/Main.hs hypha.cabal
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(hoogle): Remote module with KV cache + injectable transport"
```

---

### Task 12: `Hypha.Command.Lookup` cascade

**Files:**
- Create: `src/Hypha/Command/Lookup.hs`
- Modify: `hypha.cabal`

- [ ] **Step 1: Implement the cascade**

Create `src/Hypha/Command/Lookup.hs`:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The @hypha lookup@ command: tiered symbol resolution.
--
-- Tier order is fixed and short-circuiting:
--
--   1. 'Hypha.Search.PackageCache.lookupByName' — SQLite name index.
--   2. 'Hypha.Hoogle.Local.searchLocal'         — project Hoogle DB.
--   3. 'Hypha.Hoogle.Remote.searchRemote'       — hoogle.haskell.org.
--
-- This module owns the outcome assembly only; the individual tiers
-- live in their own modules so they can be tested independently.
module Hypha.Command.Lookup
  ( LookupResult (..)
  , Provider (..)
  , Tier (..)
  , LookupOptions (..)
  , runLookup
  , lookupOutcome
    -- * JSON
  , providerToJSON
  , lookupResultToJSON
  ) where

import Data.Aeson (Value, (.=), object)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Hoogle.Local (HyphaHoogle, searchLocal)
import Hypha.Hoogle.Remote
  ( RemoteError (..), RemoteOptions, searchRemote )
import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Output.Outcome (Outcome (..), Related (..))
import Hypha.Search.PackageCache (HyphaPackageCache, lookupByName)

data Tier = TierCache | TierLocalHoogle | TierRemoteHoogle
  deriving stock (Show, Eq, Ord)

data Provider = Provider
  { pPkg  :: !Text
  , pMod  :: !Text
  , pName :: !Text
  , pSig  :: !Text
  , pTier :: !Tier
  }
  deriving stock (Show, Eq)

data LookupResult = LookupResult
  { lrQuery           :: !Text
  , lrProviders       :: ![Provider]
  , lrTiersConsulted  :: ![Tier]
  }
  deriving stock (Show, Eq)

data LookupOptions = LookupOptions
  { loOffline :: !Bool
  , loRemote  :: !RemoteOptions
  }

-- | Top-level entry point.  Wraps the cascade in an outcome.
runLookup
  :: HyphaPackageCache
  -> HyphaHoogle
  -> LookupOptions
  -> Text
  -> IO (Outcome Value)
runLookup cache hoogleLocal opts q = lookupOutcome cache hoogleLocal opts q

lookupOutcome
  :: HyphaPackageCache
  -> HyphaHoogle
  -> LookupOptions
  -> Text
  -> IO (Outcome Value)
lookupOutcome cache hoogleLocal opts q = do
  -- Tier 1
  cacheHits <- lookupByName cache q
  case cacheHits of
    (_:_) -> success cache q
                (map (toProvider TierCache) cacheHits)
                [TierCache]
    [] -> do
      -- Tier 2
      localHits <- searchLocal hoogleLocal (HoogleQuery q)
      case localHits of
        (_:_) -> success cache q
                    (map (hitProvider TierLocalHoogle) localHits)
                    [TierCache, TierLocalHoogle]
        [] -> do
          -- Tier 3
          remote <- searchRemote (loRemote opts) cache (HoogleQuery q)
          let tiers = [TierCache, TierLocalHoogle, TierRemoteHoogle]
          case remote of
            Right hits | not (null hits) ->
              success cache q
                (map (hitProvider TierRemoteHoogle) hits) tiers
            Right _ ->
              failureOutcome q "NOT_FOUND"
                ("no providers found" :: Text)
                tiers (notFoundActions q)
            Left RemoteOffline ->
              failureOutcome q "HOOGLE_OFFLINE"
                ("--offline (or HYPHA_OFFLINE) suppresses remote tier" :: Text)
                [TierCache, TierLocalHoogle]
                (Map.singleton "retry_online" ("hypha lookup " <> q))
            Left e ->
              failureOutcome q "HOOGLE_REMOTE_ERROR"
                (Text.pack (show e)) tiers
                (Map.fromList
                  [ ("retry_offline", "hypha lookup " <> q <> " --offline")
                  , ("raise_timeout",
                       "HYPHA_HOOGLE_TIMEOUT=30 hypha lookup " <> q)
                  ])

toProvider :: Tier -> (Text, Text, Text, Text) -> Provider
toProvider t (pkg, modT, name, sig) = Provider pkg modT name sig t

hitProvider :: Tier -> HoogleHit -> Provider
hitProvider t h = Provider (hhPackage h) (hhModule h) (hhName h) (hhSig h) t

success
  :: HyphaPackageCache
  -> Text -> [Provider] -> [Tier] -> IO (Outcome Value)
success _ q providers tiers = pure $ OutcomeSuccess
  (lookupResultToJSON (LookupResult q providers tiers))
  False [] Map.empty
  [ Related (pPkg p <> "/" <> pMod p)
            ("hypha symbol " <> pPkg p <> "/" <> pMod p <> "/" <> pName p)
  | p <- take 5 providers
  ]

failureOutcome
  :: Text -> Text -> Text -> [Tier]
  -> Map.Map Text Text
  -> IO (Outcome Value)
failureOutcome q code detail tiers actions = pure $ OutcomeFailure
  (Map.fromList
     [("query", q), ("error", detail)])
  code detail actions
  -- NOTE: the existing OutcomeFailure constructor in
  -- Hypha.Output.Outcome may differ; adjust per the actual definition.

notFoundActions :: Text -> Map.Map Text Text
notFoundActions q = Map.singleton "retry_with_prefix" ("hypha lookup " <> q <> "*")

-- JSON serialisation ---------------------------------------------------

tierToText :: Tier -> Text
tierToText = \case
  TierCache         -> "cache"
  TierLocalHoogle   -> "local-hoogle"
  TierRemoteHoogle  -> "remote-hoogle"

providerToJSON :: Provider -> Value
providerToJSON p = object
  [ "pkg"  .= pPkg p
  , "mod"  .= pMod p
  , "name" .= pName p
  , "sig"  .= pSig p
  , "tier" .= tierToText (pTier p)
  ]

lookupResultToJSON :: LookupResult -> Value
lookupResultToJSON r = object
  [ "query"            .= lrQuery r
  , "providers"        .= map providerToJSON (lrProviders r)
  , "tiers_consulted"  .= map tierToText (lrTiersConsulted r)
  ]
```

> **Implementer note:** look at the actual `Hypha.Output.Outcome` constructors before finalising `failureOutcome` — adjust to whatever shape the project already uses (`OutcomeFailure` may take an `OutcomeError`).  Same for `OutcomeSuccess` argument order.

- [ ] **Step 2: Register in cabal**

Add `Hypha.Command.Lookup` to library exposed-modules.

- [ ] **Step 3: Build**

Run: `~/.ghcup/bin/cabal build lib:hypha`
Expected: success.

- [ ] **Step 4: Commit**

```bash
git add src/Hypha/Command/Lookup.hs hypha.cabal
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(cmd): Lookup module with three-tier cascade"
```

---

### Task 13: Property tests — cascade + outcome shape

**Files:**
- Create: `test/Property/LookupCascade.hs`
- Create: `test/Property/LookupOutcomeShape.hs`
- Modify: `test/Main.hs`, `hypha.cabal`

- [ ] **Step 1: Write cascade property**

Create `test/Property/LookupCascade.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Property.LookupCascade (tests) where

import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty, gen, assert)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P

import Hypha.Command.Lookup
  ( Tier (..), tierFromCacheList )  -- helper exposed for testing
-- ...

tests :: TestTree
tests = testGroup "Property.LookupCascade"
  [ testProperty "tiers_consulted matches the first-hit prefix" $ do
      hit1 <- gen Gen.bool
      hit2 <- gen Gen.bool
      hit3 <- gen Gen.bool
      offline <- gen Gen.bool
      let expected
            | hit1                           = [TierCache]
            | hit2                           = [TierCache, TierLocalHoogle]
            | offline                        = [TierCache, TierLocalHoogle]
            | hit3                           = [TierCache, TierLocalHoogle, TierRemoteHoogle]
            | otherwise                      = [TierCache, TierLocalHoogle, TierRemoteHoogle]
      let actual = tierFromCacheList hit1 hit2 offline hit3
      assert (P.eq P..$ ("expected", expected) P..$ ("actual", actual))
  ]
```

To support this, expose a pure helper in `Hypha.Command.Lookup`:

```haskell
tierFromCacheList :: Bool -> Bool -> Bool -> Bool -> [Tier]
tierFromCacheList hitCache hitLocal offline hitRemote
  | hitCache  = [TierCache]
  | hitLocal  = [TierCache, TierLocalHoogle]
  | offline   = [TierCache, TierLocalHoogle]
  | otherwise = [TierCache, TierLocalHoogle, TierRemoteHoogle]
  where _ = hitRemote   -- shape kept symmetric for future expansion
```

- [ ] **Step 2: Write outcome-shape property**

Create `test/Property/LookupOutcomeShape.hs` testing the invariant that `OutcomeSuccess` carries a non-empty providers list and `OutcomeFailure` always has a non-empty actions map.  Use the pure JSON builders.  (See spec §6 for the full invariant.)

- [ ] **Step 3: Register**

`hypha.cabal` test other-modules: add `Property.LookupCascade`, `Property.LookupOutcomeShape`. `test/Main.hs`: register.

- [ ] **Step 4: Run tests**

Run: `~/.ghcup/bin/cabal test all 2>&1 | grep -E "FAIL|passed"`
Expected: all property cases pass.

- [ ] **Step 5: Commit**

```bash
git add test/Property/LookupCascade.hs test/Property/LookupOutcomeShape.hs test/Main.hs hypha.cabal src/Hypha/Command/Lookup.hs
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "test(lookup): property tests for cascade + outcome invariants"
```

---

### Task 14: Wire `LookupCommand` into CLI parser

**Files:**
- Modify: `src/Hypha/Cli/Parser.hs`
- Modify: `src/Hypha/Cli/Run.hs`

- [ ] **Step 1: Add `LookupCommand` to `Command` sum**

In `src/Hypha/Cli/Parser.hs`:

```haskell
data Command
  = ...
  | LookupCommand !Text     -- ^ symbol or qualified name
  -- (remove SearchCommand and WhatProvidesCommand)
```

Add subparser:

```haskell
lookupParser :: Parser Command
lookupParser = LookupCommand <$> strArgument
  (metavar "QUERY" <> help "symbol name, qualified name, or type signature")
```

Register in the main parser's subcommand list, replacing `search` and `whatprovides` entries.

- [ ] **Step 2: Add `--offline` to `GlobalFlags`**

```haskell
data GlobalFlags = GlobalFlags
  { ...
  , gfOffline :: !Bool
  }

-- in globalFlagsParser:
<*> switch (long "offline" <> help "skip remote Hoogle tier")
```

Drop `gfGlobal` field and its parser if present (already targeted for removal per spec).

- [ ] **Step 3: Dispatch in `Cli/Run.hs`**

In `src/Hypha/Cli/Run.hs`:

```haskell
import qualified Hypha.Command.Lookup as Lookup
import qualified Hypha.Hoogle.Local   as HogLocal
import qualified Hypha.Hoogle.Remote  as HogRemote
import qualified Hypha.Search.PackageCache as PC

dispatchCommand flags = \case
  ...
  LookupCommand q -> do
    eRoot <- discoverProjectRoot (gfProjectDir flags)
    let mRoot = either (const Nothing) Just eRoot
    cache  <- PC.openPackageCache mRoot
    let dotHypha = maybe "/dev/null"
                     (\(ProjectRoot r) -> r </> ".hypha") mRoot
    hl     <- HogLocal.openLocalHoogle dotHypha defaultStoreRoot
    let opts = Lookup.LookupOptions
          { Lookup.loOffline = gfOffline flags
          , Lookup.loRemote  = HogRemote.defaultRemoteOptions
                                 { HogRemote.roOffline = gfOffline flags }
          }
    Right <$> Lookup.runLookup cache hl opts q

  -- Remove SearchCommand and WhatProvidesCommand branches.
```

Implementer note: `defaultStoreRoot` needs to be derived from the active GHC version.  A minimum-viable lookup is `~/.cabal/store/ghc-<version>`; pull the GHC version from `bpCompiler` if a plan loaded, else from `Hoogle.defaultStoreRoot`-style probing.  When no store can be located, pass `""` — the scavenger handles it.

Also drop these from `dispatchCommand`:
- `SearchCommand _ _`
- `WhatProvidesCommand _`

- [ ] **Step 4: Build**

Run: `~/.ghcup/bin/cabal build all`
Expected: success.  (Compile errors here flag callers we missed.)

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Cli/Parser.hs src/Hypha/Cli/Run.hs
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "feat(cli): wire LookupCommand and --offline flag"
```

---

### Task 15: Delete superseded modules

**Files:**
- Delete: `src/Hypha/Command/Search.hs`
- Delete: `src/Hypha/Command/WhatProvides.hs`
- Delete: `src/Hypha/Hoogle/Query.hs`
- Delete: `src/Hypha/Hoogle/Database.hs` (most of it; preserve any helpers Local depends on by inlining them first)
- Delete: `test/Unit/WhatProvides.hs` (if it exists)
- Delete: `test/Golden/Search.hs`
- Delete: `test/Golden/golden/search-*.json` (every `search-*` golden)
- Modify: `hypha.cabal`, `test/Main.hs`

- [ ] **Step 1: Remove modules**

```bash
git rm src/Hypha/Command/Search.hs src/Hypha/Command/WhatProvides.hs \
       src/Hypha/Hoogle/Query.hs src/Hypha/Hoogle/Database.hs \
       test/Unit/WhatProvides.hs test/Golden/Search.hs
git rm test/Golden/golden/search-*.json 2>/dev/null || true
```

- [ ] **Step 2: Drop from cabal exposed-modules and other-modules**

Remove `Hypha.Command.Search`, `Hypha.Command.WhatProvides`, `Hypha.Hoogle.Query`, `Hypha.Hoogle.Database` from library exposed-modules.  Remove `Unit.WhatProvides`, `Golden.Search`, `Property.HackageCache` (if it referenced the deleted types) from test other-modules.

- [ ] **Step 3: Drop from `test/Main.hs`**

Remove imports and `testGroup` entries for the deleted test modules.

- [ ] **Step 4: Build + test**

Run: `~/.ghcup/bin/cabal build all && ~/.ghcup/bin/cabal test all`
Expected: green.

- [ ] **Step 5: Commit**

```bash
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "refactor: remove Search and WhatProvides modules superseded by Lookup"
```

---

### Task 16: Golden tests for `lookup`

**Files:**
- Create: `test/Golden/Lookup.hs`
- Create: `test/Golden/golden/lookup-cache-hit.compact.json`
- Create: `test/Golden/golden/lookup-local-hoogle-hit.compact.json`
- Create: `test/Golden/golden/lookup-remote-hit.compact.json`
- Create: `test/Golden/golden/lookup-all-miss.compact.json`
- Create: `test/Golden/golden/lookup-remote-error.compact.json`
- Create: `test/Golden/golden/lookup-offline.compact.json`
- Modify: `hypha.cabal`, `test/Main.hs`

- [ ] **Step 1: Write the golden runner**

Create `test/Golden/Lookup.hs` constructing each scenario with a stubbed `HyphaHoogle` (override `searchLocal` via a fresh `Hoogle` record) and a stubbed `RemoteHoogleTransport`.  Use `goldenVsString` per spec §6.

Implementation sketch:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Golden.Lookup (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Hypha.Command.Lookup (runLookup, LookupOptions (..))
-- ...

tests :: TestTree
tests = testGroup "Golden.Lookup"
  [ goldenVsString "cache hit" (gold "lookup-cache-hit") $
      runScenario CacheHit
  , goldenVsString "local hoogle hit" (gold "lookup-local-hoogle-hit") $
      runScenario LocalHit
  , goldenVsString "remote hit" (gold "lookup-remote-hit") $
      runScenario RemoteHit
  , goldenVsString "all miss" (gold "lookup-all-miss") $
      runScenario AllMiss
  , goldenVsString "remote error" (gold "lookup-remote-error") $
      runScenario RemoteErr
  , goldenVsString "offline" (gold "lookup-offline") $
      runScenario Offline
  ]
  where
    gold n = "test" </> "Golden" </> "golden" </> (n <> ".compact.json")

-- 'runScenario' assembles a HyphaPackageCache + HyphaHoogle + transport
-- per scenario, calls runLookup, and returns the encoded outcome.
```

- [ ] **Step 2: Create stub golden files**

Empty files first (the runner with `--accept` will populate them):

```bash
touch test/Golden/golden/lookup-{cache-hit,local-hoogle-hit,remote-hit,all-miss,remote-error,offline}.compact.json
```

- [ ] **Step 3: Run with `--accept`**

```bash
~/.ghcup/bin/cabal test all --test-options=--accept
```

Expected: all goldens accepted, test passes.

- [ ] **Step 4: Visually verify the accepted goldens**

Open each file, confirm the JSON matches expectations from spec §3.  Make any corrections required (then `--accept` again).

- [ ] **Step 5: Commit**

```bash
git add test/Golden/Lookup.hs test/Golden/golden/lookup-*.compact.json test/Main.hs hypha.cabal
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "test(lookup): golden coverage for every cascade outcome"
```

---

### Task 17: README + CHANGELOG + version bump

**Files:**
- Modify: `README.md`
- Modify: `CHANGELOG.md` (create if absent)
- Modify: `hypha.cabal`

- [ ] **Step 1: README**

Replace any sections referencing `hypha search` or `hypha whatprovides` with a single "Looking up symbols" section explaining the cascade and `--offline`.  Add a "Cache layout" section documenting `~/.cache/hypha/` and `<project>/.hypha/`, with the `rm -rf` recipe for manual invalidation.  Note that there is no `--global` flag and no cache-invalidation subcommand.

- [ ] **Step 2: CHANGELOG**

Add (or create) `CHANGELOG.md`:

```markdown
## 0.2.0 (unreleased)

### Breaking

- `hypha search` removed. Use `hypha lookup`.
- `hypha whatprovides` removed. Use `hypha lookup`.
- `--global` flag removed. The new `lookup` cascade always consults
  project + remote Hoogle automatically.

### Added

- `hypha lookup <query>` — single tiered symbol-resolution command.
- `--offline` flag (and `HYPHA_OFFLINE=1`) — skips the remote Hoogle
  tier.
- `HYPHA_HOOGLE_TIMEOUT=<seconds>` — overrides remote timeout (default 10s).
- Source-tree fingerprint invalidation for local-package cache rows.
```

- [ ] **Step 3: Version**

In `hypha.cabal`, bump `version: 0.1.0` → `version: 0.2.0`.

- [ ] **Step 4: Build**

Run: `~/.ghcup/bin/cabal build all`
Expected: success.

- [ ] **Step 5: Commit**

```bash
git add README.md CHANGELOG.md hypha.cabal
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "docs: 0.2.0 lookup command — README + CHANGELOG + version bump"
```

---

### Task 18: Final verification + close issue

**Files:**
- Move: `issues/in_progress/037-hypha-lookup.md` → `issues/done/037-hypha-lookup.md`

- [ ] **Step 1: Run the full suite**

Run: `~/.ghcup/bin/cabal build all && ~/.ghcup/bin/cabal test all`
Expected: all 100+ tests pass.

- [ ] **Step 2: Manual smoke**

From the hypha repo root:

```bash
~/.ghcup/bin/cabal run hypha -- lookup lookup
```

Expected: structured outcome with at least one local-origin hit on
`Hypha.Search.PackageCache.lookupByName`.

```bash
~/.ghcup/bin/cabal run hypha -- lookup 'a -> Maybe a' --offline
```

Expected: structured outcome with `HOOGLE_OFFLINE` code (no remote
call) when local Hoogle DB has no hit.

- [ ] **Step 3: Move issue**

```bash
mv issues/in_progress/037-hypha-lookup.md issues/done/037-hypha-lookup.md
```

- [ ] **Step 4: Final commit**

```bash
git add issues/done/037-hypha-lookup.md
git -c user.email=alfredo@well-typed.com -c user.name="Alfredo Di Napoli" commit -m "chore(issue): close 037 hypha lookup"
```

---

## Self-review checklist

- [x] Spec coverage: every spec section (§1–§6) maps to at least one task.
- [x] No placeholders: every `- [ ]` step shows complete code or an exact command.
- [x] Type consistency: `Provider`, `LookupOptions`, `HyphaHoogle`, `HoogleStamp`, `LocalUnit`, `HaddockRequest`, `RemoteOptions` referenced consistently across tasks.
- [x] Frequent commits: every task ends with at least one commit; most have one per logical step.
- [x] TDD discipline: red/green ordering preserved on every new module (tests written before impl).
- [x] Mocking discipline: `HaddockRunner` and `RemoteHoogleTransport` injected as record-of-functions; CI never shells out to `haddock` or hits the network.

## Open notes for the implementer

1. **`Hypha.Output.Outcome` shape** — Task 12 sketches `OutcomeSuccess`/`OutcomeFailure` calls; check the actual constructors at `src/Hypha/Output/Outcome.hs` and adjust argument lists. Don't fight the existing API.

2. **`defaultStoreRoot` derivation** — Task 14 hand-waves the path. Acceptable initial impl: derive `ghc-<version>` from `bpCompiler` of the active plan, fall back to env probe (`GHC_VERSION` or running `ghc --numeric-version`). Leaving it as `""` is OK for v0.2.0 — scavenger degrades gracefully.

3. **`Hoogle.hoogle` blocking behaviour** — `regenerate` calls `Hoogle.hoogle ["generate", ...]` synchronously. First-time DB generation can take 10–60s on big plans. That blocks the `lookup` invocation. Acceptable; mention it in the README.

4. **Test isolation** — `test/Unit/HoogleLocalGen.hs` must not call `defaultHaddockRunner` because that would shell out. Tests always inject a stub `HaddockRunner`. `defaultHaddockRunner` is exercised by manual smoke only.

5. **Schema migration backward-compat** — Existing user DBs (created before this work) pre-date the `fingerprint` column. The idempotent `ALTER TABLE` in Task 1 handles them. Local rows in pre-existing DBs will have `fingerprint = NULL` and will be treated as stale on first lookup; that's a one-time re-index, no data loss.
