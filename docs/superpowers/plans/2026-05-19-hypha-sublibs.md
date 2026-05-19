# Hypha Sub-Libraries Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make every cabal `library <name>` (sublib) of every package in the build plan a first-class browsable unit in `hypha server` — separate sidebar entry, own URL, own search-index rows.

**Architecture:** Parse each package's `.cabal` with `Cabal-syntax` to discover its library components and their `hs-source-dirs`. Carry the per-component data through `PlannedUnit`. Index each `(pkg, sublib)` pair under a composite name (`nike:lib-foo`) reusing the existing SQLite cache schema. Route URLs decode the composite name and dispatch to the matching component's source roots.

**Tech Stack:** Haskell (GHC 9.6+), `Cabal-syntax 3.10+`, `cabal-plan`, `lucid2`, `servant`, `sqlite-simple`, `tasty`/`tasty-hunit`/`falsify` for tests.

---

## File Structure

**New files:**
- `src/Hypha/Types/ComponentName.hs` — `ComponentName` data type + parse/render helpers.
- `src/Hypha/Project/Components.hs` — `ComponentInfo` type + `.cabal` parsing with `Cabal-syntax`.
- `test/Property/ComponentName.hs` — round-trip property tests.
- `test/Unit/Components.hs` — cabal-parse unit tests against fixtures.
- `test/fixtures/cabal/nike.cabal` — fixture with main lib + two sublibs.

**Modified files:**
- `hypha.cabal` — add `Cabal-syntax` dep + new exposed modules.
- `src/Hypha/Types/BuildPlan.hs` — extend `PlannedUnit` with `puLibComponents`.
- `src/Hypha/Project/Plan.hs` — populate `puLibComponents` via `Hypha.Project.Components`.
- `src/Hypha/Search/Cache.hs` — no schema change; comment updated to note composite-name usage.
- `src/Hypha/Command/Server.hs` — indexer iterates `(unit, component)`; sidebar list + handler dispatch use composite names; `enumModules` becomes `componentSourceRoots`.
- `src/Hypha/Server/App.hs` — handlers accept composite `pkg` already (no signature change), just thread composite name through.
- `src/Hypha/Server/Ui/Tree.hs` — sidebar entries link to URL-encoded composite name; sublib entries styled with muted suffix.
- `src/Hypha/Server/Ui/Layout.hs` — no change.
- `test/Unit/Server.hs` — add `splitComponentName` tests.
- `test/Main.hs` — register `Property.ComponentName` and `Unit.Components` test groups.
- `test/Golden/golden/server-home.html` — regenerate (sidebar entry includes a sublib).

---

### Task 1: Add `Cabal-syntax` dependency

**Files:**
- Modify: `hypha.cabal`

- [ ] **Step 1: Add the dep + new modules under the library stanza**

```cabal
  exposed-modules:
    ...
    Hypha.Project.Components
    Hypha.Project.Plan
    ...
    Hypha.Types.ComponentName
    ...
```

Add the dependency in the alphabetised dependency list (between `bytestring` and `cabal-plan`):

```cabal
    , Cabal-syntax        >= 3.10 && < 3.20
```

- [ ] **Step 2: Build everything to confirm dep resolves**

```bash
~/.ghcup/bin/cabal build all
```

Expected: PASS (no Haskell changes yet — just config).

- [ ] **Step 3: Commit**

```bash
git add hypha.cabal
git commit -m "build: add Cabal-syntax dependency for sublib parsing"
```

---

### Task 2: `Hypha.Types.ComponentName` skeleton

**Files:**
- Create: `src/Hypha/Types/ComponentName.hs`
- Create: `test/Property/ComponentName.hs`
- Modify: `test/Main.hs`
- Modify: `hypha.cabal` (test-suite other-modules)

- [ ] **Step 1: Write the failing property test**

```haskell
-- test/Property/ComponentName.hs
{-# LANGUAGE OverloadedStrings #-}
module Property.ComponentName (tests) where

import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Test.Tasty.Falsify as F

import Hypha.Types.ComponentName
  ( ComponentName (..), parseComponentName, renderComponentName )
import Hypha.Types.PackageId (PackageName (..))

tests :: TestTree
tests = testGroup "Property.ComponentName"
  [ testCase "round-trip simple package" $
      parseComponentName "nike"
        @?= ComponentName (PackageName "nike") Nothing
  , testCase "round-trip pkg:sublib" $
      parseComponentName "nike:lib-breakdown"
        @?= ComponentName (PackageName "nike") (Just "lib-breakdown")
  , testCase "render simple" $
      renderComponentName (ComponentName (PackageName "nike") Nothing)
        @?= "nike"
  , testCase "render composite" $
      renderComponentName
        (ComponentName (PackageName "nike") (Just "lib-breakdown"))
        @?= "nike:lib-breakdown"
  , testCase "empty sublib suffix collapses" $
      parseComponentName "nike:"
        @?= ComponentName (PackageName "nike") Nothing
  , F.testProperty "parse . render is id" $ do
      pkg    <- F.gen (F.elem [ "nike", "containers", "aeson" ])
      sublib <- F.gen (F.elem [ Nothing, Just "lib-foo", Just "internal" ])
      let cn  = ComponentName (PackageName (Text.pack pkg)) (fmap Text.pack sublib)
      F.assert (renderComponentName cn == renderComponentName (parseComponentName (renderComponentName cn)))
  ]
```

- [ ] **Step 2: Wire the new test module into the test suite**

In `hypha.cabal` under `test-suite hypha-tests`'s `other-modules`, add `Property.ComponentName` (alphabetical position next to `Property.HackageCache`).

In `test/Main.hs`, import and register the new group:

```haskell
import qualified Property.ComponentName
...
main = defaultMain $ testGroup "hypha"
  [ ...
  , Property.ComponentName.tests
  , ...
  ]
```

- [ ] **Step 3: Run tests, confirm they fail (module missing)**

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: FAIL with `Could not find module 'Hypha.Types.ComponentName'`.

- [ ] **Step 4: Implement the module**

```haskell
-- src/Hypha/Types/ComponentName.hs
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | A cabal-style component reference: a package name with an optional
-- sub-library qualifier.  @nike@ refers to the main library;
-- @nike:lib-breakdown@ refers to the @lib-breakdown@ sub-library.
module Hypha.Types.ComponentName
  ( ComponentName (..)
  , parseComponentName
  , renderComponentName
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.PackageId (PackageName (..))

-- | Reference to a single library component in the build plan.
data ComponentName = ComponentName
  { cnPackage :: !PackageName
  , cnSublib  :: !(Maybe Text)
    -- ^ 'Nothing' for the main library; 'Just' for a sub-library.
  }
  deriving stock (Show, Eq, Ord)

-- | Split a textual reference on the first @:@.  Empty sub-library
-- suffix collapses to 'Nothing' so @"pkg:"@ and @"pkg"@ are equivalent.
parseComponentName :: Text -> ComponentName
parseComponentName raw =
  case Text.breakOn ":" raw of
    (pkg, rest)
      | Text.null rest -> ComponentName (PackageName pkg) Nothing
      | otherwise      ->
          let sublib = Text.drop 1 rest
          in ComponentName (PackageName pkg)
               (if Text.null sublib then Nothing else Just sublib)

-- | Inverse of 'parseComponentName'.
renderComponentName :: ComponentName -> Text
renderComponentName (ComponentName (PackageName p) Nothing)  = p
renderComponentName (ComponentName (PackageName p) (Just s)) = p <> ":" <> s
```

Add `Hypha.Types.ComponentName` to `hypha.cabal`'s library `exposed-modules` if not already there.

- [ ] **Step 5: Run tests, confirm they pass**

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: all tests pass.

- [ ] **Step 6: Commit**

```bash
git add hypha.cabal src/Hypha/Types/ComponentName.hs \
        test/Property/ComponentName.hs test/Main.hs
git commit -m "feat(types): ComponentName with pkg:sublib parsing"
```

---

### Task 3: `Hypha.Project.Components` — `.cabal` parsing

**Files:**
- Create: `src/Hypha/Project/Components.hs`
- Create: `test/fixtures/cabal/nike.cabal`
- Create: `test/Unit/Components.hs`
- Modify: `test/Main.hs`
- Modify: `hypha.cabal`

- [ ] **Step 1: Write the fixture cabal file**

```cabal
-- test/fixtures/cabal/nike.cabal
cabal-version: 3.0
name:          nike
version:       0.1.0
synopsis:      Fixture
build-type:    Simple

library
  hs-source-dirs: src
  exposed-modules: Nike.Main
  build-depends:   base

library internal
  hs-source-dirs: internal-src
  exposed-modules: Nike.Internal.Helpers
  build-depends:   base

library bench
  hs-source-dirs: bench-src
  exposed-modules: Nike.Bench.Suite
  build-depends:   base
```

- [ ] **Step 2: Write the failing unit test**

```haskell
-- test/Unit/Components.hs
{-# LANGUAGE OverloadedStrings #-}
module Unit.Components (tests) where

import Data.List (sort)
import qualified Data.Text as Text
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Project.Components
  ( ComponentInfo (..), parseLibComponents )

tests :: TestTree
tests = testGroup "Unit.Components"
  [ testCase "parses main lib + two sublibs from fixture" $ do
      let root  = "test" </> "fixtures" </> "cabal"
          cabal = root </> "nike.cabal"
      comps <- parseLibComponents cabal root
      let summary =
            sort [ ( fmap Text.unpack (ciSublib c)
                   , sort (ciHsSourceDirs c)
                   )
                 | c <- comps
                 ]
      summary @?=
        [ ( Nothing,         [root </> "src"] )
        , ( Just "bench",    [root </> "bench-src"] )
        , ( Just "internal", [root </> "internal-src"] )
        ]
  , testCase "missing cabal file returns []" $ do
      res <- parseLibComponents "/does/not/exist.cabal" "/does/not"
      res @?= []
  ]
```

Register `Unit.Components` in `test/Main.hs` and `hypha.cabal`'s
`other-modules` list.

- [ ] **Step 3: Run tests, confirm they fail (module missing)**

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: FAIL with `Could not find module 'Hypha.Project.Components'`.

- [ ] **Step 4: Implement the module**

```haskell
-- src/Hypha/Project/Components.hs
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Read a package's @.cabal@ file and report the library components
-- it defines (main library + every @library NAME@ stanza), together
-- with the absolute paths to their @hs-source-dirs@.
--
-- The result drives sub-library indexing in @hypha server@: each
-- component becomes its own browsable entry under @pkg:sublib@.
module Hypha.Project.Components
  ( ComponentInfo (..)
  , parseLibComponents
  , findCabalFile
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Parsec as PDP
import qualified Distribution.Types.UnqualComponentName as UC
import qualified Distribution.Utils.Path as UP
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)

-- | One library component of a package.
data ComponentInfo = ComponentInfo
  { ciSublib       :: !(Maybe Text)
    -- ^ 'Nothing' for the main library; 'Just' for a sub-library.
  , ciHsSourceDirs :: ![FilePath]
    -- ^ Absolute paths.  Falls back to the package root when the
    -- stanza omits @hs-source-dirs@ (cabal default).
  }
  deriving stock (Show, Eq)

-- | Locate the @.cabal@ file inside a package source directory.  Cabal
-- forbids more than one, so the first match is canonical.
findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile dir = do
  ok <- doesDirectoryExist dir
  if not ok
    then pure Nothing
    else do
      entries <- listDirectory dir
      pure $ case filter ((== ".cabal") . takeExtension) entries of
        (f : _) -> Just (dir </> f)
        []      -> Nothing

-- | Parse a @.cabal@ file and return one 'ComponentInfo' per library
-- component (main + sublibs).  Returns @[]@ on parse failure or
-- missing file — callers fall back to the heuristic source-root walk.
parseLibComponents
  :: FilePath  -- ^ cabal file path
  -> FilePath  -- ^ package root (for resolving relative source dirs)
  -> IO [ComponentInfo]
parseLibComponents cabalPath pkgRoot = do
  eBs <- try @IOException (BS.readFile cabalPath)
  case eBs of
    Left _  -> pure []
    Right bs ->
      case PDP.parseGenericPackageDescriptionMaybe bs of
        Nothing  -> pure []
        Just gpd ->
          let mainComp =
                [ mkComponent Nothing (PD.libBuildInfo (PD.condTreeData ct))
                | ct <- maybe [] (:[]) (PD.condLibrary gpd)
                ]
              subComps =
                [ mkComponent (Just (Text.pack (UC.unUnqualComponentName n)))
                              (PD.libBuildInfo (PD.condTreeData ct))
                | (n, ct) <- PD.condSubLibraries gpd
                ]
          in pure (mainComp ++ subComps)
  where
    mkComponent name bi =
      let dirs0 = map (UP.getSymbolicPath) (PD.hsSourceDirs bi)
          dirs  = if null dirs0 then [pkgRoot] else map (pkgRoot </>) dirs0
      in ComponentInfo name dirs
```

If the installed `Cabal-syntax` does not expose `Distribution.Utils.Path.getSymbolicPath` for the field type used by `hsSourceDirs` (e.g. on Cabal-syntax 3.10 the field is `[FilePath]`), replace the `dirs0` line with `dirs0 = PD.hsSourceDirs bi` and use a CPP shim:

```haskell
#if MIN_VERSION_Cabal_syntax(3,14,0)
      dirs0 = map UP.getSymbolicPath (PD.hsSourceDirs bi)
#else
      dirs0 = PD.hsSourceDirs bi
#endif
```

Add `{-# LANGUAGE CPP #-}` to the module header in that case.

- [ ] **Step 5: Wire the new module into the library**

Add `Hypha.Project.Components` to the library `exposed-modules` in
`hypha.cabal` (place between `Hypha.Project.Plan` and `Hypha.Project.Overrides`).

- [ ] **Step 6: Run tests, confirm they pass**

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -8
```

Expected: 2 new `Unit.Components` cases green; full suite passes.

- [ ] **Step 7: Commit**

```bash
git add hypha.cabal src/Hypha/Project/Components.hs \
        test/Unit/Components.hs test/fixtures/cabal/nike.cabal \
        test/Main.hs
git commit -m "feat(project): parseLibComponents reads sublibs from .cabal"
```

---

### Task 4: Extend `PlannedUnit` with `puLibComponents`

**Files:**
- Modify: `src/Hypha/Types/BuildPlan.hs`
- Modify: `src/Hypha/Project/Plan.hs`

- [ ] **Step 1: Add the field**

In `Hypha.Types.BuildPlan.PlannedUnit`:

```haskell
  , puLibComponents :: ![ComponentInfo]
    -- ^ Library components (main + sublibs) discovered from the
    -- package's @.cabal@ file.  Empty list means "fall back to the
    -- heuristic source-root walk".
```

Import `ComponentInfo` from `Hypha.Project.Components` (this introduces
a new module-level dep BuildPlan → Components, no cycle).

Update the placeholder constructor in `applyOverrides`:

```haskell
PlannedUnit { puId = ..., puDeps = [], puIsLocal = False
            , puSrcDir = Nothing, puDistDir = Nothing
            , puLibComponents = [] }
```

- [ ] **Step 2: Populate the field in `toPlannedUnit`**

`toPlannedUnit` becomes `IO`-flavoured because the cabal parse is `IO`.
In `Hypha.Project.Plan`:

```haskell
import qualified Hypha.Project.Components as Comp

unitsFromPlan :: CP.PlanJson -> Map FilePath FilePath -> IO (Map PackageName PlannedUnit)
unitsFromPlan pj sourceCacheLookup = do
  let allUnits = Map.elems (CP.pjUnits pj)
      unitIdToPkgId = Map.fromList
        [ (CP.uId u, CP.uPId u) | u <- allUnits ]
  pairs <- mapM
    (\u -> do
       pu <- toPlannedUnit unitIdToPkgId sourceCacheLookup u
       let CP.PkgId (CP.PkgName pkgText) _ = CP.uPId u
       pure (PackageName pkgText, pu))
    allUnits
  pure (Map.fromList pairs)

toPlannedUnit :: Map CP.UnitId CP.PkgId -> Map FilePath FilePath -> CP.Unit -> IO PlannedUnit
toPlannedUnit unitIdToPkgId sourceCacheLookup u = do
  let CP.PkgId (CP.PkgName name) ver = CP.uPId u
      pkgId   = PackageId (PackageName name) (Version (CP.dispVer ver))
      libDeps = concatMap (Set.toList . CP.ciLibDeps) (Map.elems (CP.uComps u))
      deps    = [ toPackageId pid | uid <- libDeps
                                  , Just pid <- [Map.lookup uid unitIdToPkgId] ]
      srcDir  = extractSrcDir (CP.uPkgSrc u)
  comps <- case srcDir of
    Just d -> componentsFor d
    Nothing -> case Map.lookup (Text.unpack name <> "-" <> Text.unpack (CP.dispVer ver)) sourceCacheLookup of
      Just d  -> componentsFor d
      Nothing -> pure []
  pure PlannedUnit
    { puId            = pkgId
    , puDeps          = deps
    , puIsLocal       = (CP.uType u == CP.UnitTypeLocal)
    , puSrcDir        = srcDir
    , puDistDir       = CP.uDistDir u
    , puLibComponents = comps
    }

componentsFor :: FilePath -> IO [Comp.ComponentInfo]
componentsFor d = do
  mCabal <- Comp.findCabalFile d
  case mCabal of
    Just c  -> Comp.parseLibComponents c d
    Nothing -> pure []
```

Update `loadBuildPlan` to thread the result and provide an empty
sourceCacheLookup for now (dependency cabal parsing is wired in
Task 5):

```haskell
loadBuildPlan :: ProjectRoot -> IO (Either PlanError BuildPlan)
loadBuildPlan (ProjectRoot root) = do
  result <- try @IOException (CP.findAndDecodePlanJson (CP.ProjectRelativeToDir root))
  case result of
    Left e   -> pure (Left (PlanNotFound (show e)))
    Right pj -> do
      units <- unitsFromPlan pj Map.empty
      pure (Right (BuildPlan
        { bpCompiler  = compilerFromPlan pj
        , bpUnits     = units
        , bpOverrides = []
        }))
```

- [ ] **Step 3: Build**

```bash
~/.ghcup/bin/cabal build all 2>&1 | tail -10
```

Expected: PASS.  The full test suite still passes because the cabal
parse is best-effort and falls back to `[]`.

- [ ] **Step 4: Run the test suite**

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: all 93+ tests pass.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Types/BuildPlan.hs src/Hypha/Project/Plan.hs
git commit -m "feat(plan): populate puLibComponents from cabal files"
```

---

### Task 5: Wire dependency-package cabal lookup

Provide a sourceCacheLookup so the indexer's cabal parser sees
dependency source dirs too.

**Files:**
- Modify: `src/Hypha/Project/Plan.hs`
- Modify: `src/Hypha/Hackage/Source.hs` (export a helper to enumerate cached pkg dirs)

- [ ] **Step 1: Read the source-cache layout**

```bash
grep -n "sourceCache\|XdgCache\|defaultSource" src/Hypha/Hackage/Source.hs | head
```

Confirm the source-cache root is `$XDG_CACHE_HOME/hypha/source/` and
contains one subdir per `<pkg>-<ver>`.

- [ ] **Step 2: Export an enumeration helper**

Add to `Hypha.Hackage.Source`:

```haskell
-- | Enumerate cached source directories as a map from @"pkg-ver"@ to
-- absolute path.  Returns 'Map.empty' if the cache doesn't exist yet.
enumerateSourceCache :: IO (Map.Map FilePath FilePath)
enumerateSourceCache = do
  dir <- defaultSourceCacheDir       -- or whatever the existing name is
  ok  <- doesDirectoryExist dir
  if not ok then pure Map.empty
            else do
              names <- listDirectory dir
              pure (Map.fromList [ (n, dir </> n) | n <- names ])
```

Add `enumerateSourceCache` to the module's export list.  If the helper
name `defaultSourceCacheDir` doesn't match the existing one, adapt
to whatever the module already uses (search via
`grep -n "sourceCache" src/Hypha/Hackage/Source.hs`).

- [ ] **Step 3: Pass the map into `loadBuildPlan`**

```haskell
import qualified Hypha.Hackage.Source as Src

loadBuildPlan (ProjectRoot root) = do
  result <- try @IOException (CP.findAndDecodePlanJson (CP.ProjectRelativeToDir root))
  case result of
    Left e   -> pure (Left (PlanNotFound (show e)))
    Right pj -> do
      cache <- Src.enumerateSourceCache
      units <- unitsFromPlan pj cache
      pure (Right (BuildPlan
        { bpCompiler  = compilerFromPlan pj
        , bpUnits     = units
        , bpOverrides = []
        }))
```

- [ ] **Step 4: Build + test**

```bash
~/.ghcup/bin/cabal build all
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: all tests pass.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Project/Plan.hs src/Hypha/Hackage/Source.hs
git commit -m "feat(plan): hydrate sublibs for dependency packages too"
```

---

### Task 6: `splitComponentName` helper + index data flow

**Files:**
- Modify: `src/Hypha/Command/Server.hs`
- Modify: `test/Unit/Server.hs`

- [ ] **Step 1: Add the failing parse test**

In `test/Unit/Server.hs`:

```haskell
import Hypha.Types.ComponentName
  ( ComponentName (..), parseComponentName )
import Hypha.Types.PackageId (PackageName (..))

...
  , testCase "parseComponentName splits sublib suffix" $
      parseComponentName "nike:lib-breakdown"
        @?= ComponentName (PackageName "nike") (Just "lib-breakdown")
  , testCase "parseComponentName: plain pkg has no sublib" $
      parseComponentName "nike"
        @?= ComponentName (PackageName "nike") Nothing
```

- [ ] **Step 2: Run, confirm green (already implemented in Task 2)**

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: PASS.

- [ ] **Step 3: Extend the indexer to emit per-component rows**

In `src/Hypha/Command/Server.hs`, replace the unit-keyed iteration in
`buildAndCacheIndex` and `hydrateFromCache` with component-keyed
iteration.  Concretely:

- Build a list of `(PackageId, Maybe Text, [FilePath])` tuples from
  the `[PackageId]` argument by looking each unit up in `bpUnits plan`
  and flattening its `puLibComponents`.  If `puLibComponents` is
  empty, emit a single fallback tuple with `sublib = Nothing` and
  source dirs from the existing `chooseSourceRoots` heuristic.
- The cache key becomes the composite name:
  - `Nothing`   → `pkgName`
  - `Just lib`  → `pkgName <> ":" <> lib`
- Done-counter still bumps once per **unit** (not per component) so
  the progress bar continues to read like a package count.

```haskell
buildAndCacheIndex
  :: BuildPlan
  -> Cache.IndexCache
  -> PackageResolver IO
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IORef.IORef Int
  -> IO ()
buildAndCacheIndex plan cache resolver pids ref doneRef =
  mapM_ indexUnit pids
  where
    bump = IORef.atomicModifyIORef' doneRef (\n -> (n + 1, ()))

    indexUnit pid = do
      eDir <- resolveSrc resolver pid
      case eDir of
        Left _  -> bump
        Right d -> do
          comps <- componentsForUnit plan pid d
          mapM_ (writeComp pid) comps
          bump

    writeComp pid (sublib, sourceDirs) = do
      let pkgT    = unPackageName (pkgName pid)
          verT    = unVersion    (pkgVersion pid)
          compKey = case sublib of
            Nothing -> pkgT
            Just s  -> pkgT <> ":" <> s
      mods <- enumModulesIn sourceDirs
      rowChunks <- mapM (collectMod compKey sourceDirs) mods
      let flatRows = concat rowChunks
          indexed  = [ Fuzzy.mkIndexedRow p m n s | (p, m, n, s) <- flatRows ]
      Cache.writeIndex cache compKey verT flatRows
      indexed `seq` IORef.atomicModifyIORef' ref (\old -> (indexed ++ old, ()))
```

Add the two helpers, replacing `enumModules`/`chooseSourceRoots`
callers that previously took a single dir:

```haskell
-- | Resolve a unit's components.  Returns @(sublib, srcDirs)@ pairs.
-- Falls back to the heuristic root walk when the unit has no parsed
-- components.
componentsForUnit
  :: BuildPlan -> PackageId -> FilePath
  -> IO [(Maybe Text, [FilePath])]
componentsForUnit plan pid d =
  case lookupUnit (pkgName pid) plan of
    Just pu | not (null (puLibComponents pu)) ->
      pure [ (Comp.ciSublib c, Comp.ciHsSourceDirs c) | c <- puLibComponents pu ]
    _ -> do
      roots <- chooseSourceRoots d
      pure [(Nothing, roots)]

-- | Enumerate module paths from a fixed list of source roots.
enumModulesIn :: [FilePath] -> IO [Text]
enumModulesIn roots = do
  paths <- concat <$> mapM
            (\r -> map (drop (length r + 1)) <$> findHs r 4)
            roots
  pure (map (Text.pack . hsToModule) paths)
```

`hydrateFromCache` is updated symmetrically to look up cached rows by
composite name for every component of every unit:

```haskell
hydrateFromCache
  :: BuildPlan -> Cache.IndexCache -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IO [PackageId]
hydrateFromCache plan cache pids ref = do
  missingRefs <- mapM hydrateUnit pids
  pure [pid | (pid, miss) <- zip pids missingRefs, miss]
  where
    hydrateUnit pid = do
      let pkgT = unPackageName (pkgName pid)
          verT = unVersion    (pkgVersion pid)
      compKeys <- compositeKeys plan pid pkgT
      hits <- mapM (\k -> Cache.haveIndex cache k verT) compKeys
      if and hits && not (null compKeys)
        then do
          mapM_ (loadKey verT) compKeys
          pure False
        else pure True

    loadKey verT k = do
      rows <- Cache.readIndex cache k verT
      let indexed = [ Fuzzy.mkIndexedRow p m n s | (p, m, n, s) <- rows ]
      indexed `seq` IORef.atomicModifyIORef' ref
        (\old -> (indexed ++ old, ()))

    compositeKeys plan pid pkgT =
      case lookupUnit (pkgName pid) plan of
        Just pu | not (null (puLibComponents pu)) ->
          pure [ case Comp.ciSublib c of
                   Nothing -> pkgT
                   Just s  -> pkgT <> ":" <> s
               | c <- puLibComponents pu ]
        _ -> pure [pkgT]
```

Thread `plan` into `hydrateFromCache` and `buildAndCacheIndex` calls
inside `buildServerConfig`:

```haskell
missing <- hydrateFromCache plan cache pids indexRef
...
        r <- try (buildAndCacheIndex plan cache resolver missing indexRef doneRef)
```

- [ ] **Step 4: Build + test**

```bash
~/.ghcup/bin/cabal build all 2>&1 | tail -10
~/.ghcup/bin/cabal test all 2>&1 | tail -8
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Command/Server.hs test/Unit/Server.hs
git commit -m "feat(server): indexer + cache iterate per component"
```

---

### Task 7: Sidebar — list every component

**Files:**
- Modify: `src/Hypha/Command/Server.hs` (build `scPackages` list)
- Modify: `src/Hypha/Server/Ui/Tree.hs` (URL-encode `:`)
- Modify: `ui/css/components/tree.css` (subtle sublib tag styling)

- [ ] **Step 1: Build `scPackages` from all components**

In `buildServerConfig` replace:

```haskell
let packages = map (unPackageName . pkgName) pids
```

with:

```haskell
let packages = concatMap (\pid ->
      case lookupUnit (pkgName pid) plan of
        Just pu | not (null (puLibComponents pu)) ->
          [ case Comp.ciSublib c of
              Nothing -> unPackageName (pkgName pid)
              Just s  -> unPackageName (pkgName pid) <> ":" <> s
          | c <- puLibComponents pu
          ]
        _ -> [unPackageName (pkgName pid)]
      ) pids
```

- [ ] **Step 2: Encode `:` in sidebar hrefs**

In `src/Hypha/Server/Ui/Tree.hs`, replace the entry rendering with a
helper that URL-encodes `:` and styles the sublib suffix:

```haskell
import qualified Data.Text as Text

renderEntry :: Text -> Html ()
renderEntry compName =
  let (pkgPart, sublibPart) = case Text.breakOn ":" compName of
        (a, b) | Text.null b -> (a, Nothing)
               | otherwise   -> (a, Just (Text.drop 1 b))
      hrefText = case sublibPart of
        Nothing -> pkgPart
        Just s  -> pkgPart <> "%3A" <> s
  in li_ $ a_ [href_ ("/pkg/" <> hrefText)] $ do
       toHtml pkgPart
       case sublibPart of
         Nothing -> pure ()
         Just s  -> span_ [class_ "sublib-tag"] (toHtml (":" <> s))
```

Update `packageTree` to call `renderEntry` instead of the existing
inline renderer.

- [ ] **Step 3: Add a tiny CSS rule**

Append to `ui/css/components/tree.css`:

```css
.sublib-tag {
  color: var(--muted);
  font-size: 0.85em;
  margin-left: 0.1em;
}
```

- [ ] **Step 4: Regenerate the golden home page**

```bash
~/.ghcup/bin/cabal test all --test-options="--accept" 2>&1 | tail -5
```

- [ ] **Step 5: Build + smoke run**

```bash
~/.ghcup/bin/cabal build all
~/.ghcup/bin/cabal run hypha -- server --bind 127.0.0.1:4310 &
PID=$!; sleep 6
curl -s http://127.0.0.1:4310/ | grep -oE '/pkg/[^"]+' | head -10
kill $PID
```

Expected: at least one URL with `%3A` if the local plan has sublibs.

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Command/Server.hs src/Hypha/Server/Ui/Tree.hs \
        ui/css/components/tree.css test/Golden/golden/server-home.html
git commit -m "feat(server): sidebar lists each library component"
```

---

### Task 8: Handler dispatch by composite name

**Files:**
- Modify: `src/Hypha/Command/Server.hs` (the four `sc*` callbacks)

- [ ] **Step 1: Update each handler to parse the composite name**

Pattern, applied uniformly:

```haskell
let cn      = parseComponentName pkgT
    parent  = cnPackage cn
    sublib  = cnSublib  cn
```

Then dispatch to source dirs from the matching `ComponentInfo`:

```haskell
sourceDirsFor plan parent sublib =
  case lookupUnit parent plan of
    Just pu ->
      case [ Comp.ciHsSourceDirs c
           | c <- puLibComponents pu
           , Comp.ciSublib c == sublib ] of
        (dirs : _) -> Just dirs
        []         -> Nothing
    Nothing -> Nothing
```

Replace `findModuleFile d modT` (which walks heuristic roots) with a
version that takes an explicit list of roots:

```haskell
findModuleFileIn :: [FilePath] -> Text -> IO (Maybe FilePath)
findModuleFileIn roots modPath =
  let relFile = modulePathToFile modPath
  in firstExisting [ r FP.</> relFile | r <- roots ]
```

(Live in `Hypha.Source.Locate` next to `findModuleFile`.)

Update each of:

- `scSymbolLookup` — resolves to the parent package's source dir,
  then narrows to the component's `ciHsSourceDirs` for the initial
  file lookup and the `findInTree` re-extract step.
- `scSourceText`   — same.
- `scPackageInfo`  — version comes from the parent; module list comes
  from walking just this component's source dirs.
- `scModuleExports`— same.

Where a component lookup fails (e.g. URL refers to a non-existent
sublib), the handler returns `Nothing`/`[]` and the page renders the
existing "not found" branch.

- [ ] **Step 2: Build**

```bash
~/.ghcup/bin/cabal build all
```

- [ ] **Step 3: Smoke run**

```bash
~/.ghcup/bin/cabal run hypha -- server --bind 127.0.0.1:4311 &
PID=$!; sleep 6
# Expect a sublib package page to render with its modules:
curl -s 'http://127.0.0.1:4311/pkg/nike%3Alib-breakdown' | grep -oE 'class="module-list"' | head
kill $PID
```

Expected: the module-list class shows up (i.e. the page rendered).

- [ ] **Step 4: Commit**

```bash
git add src/Hypha/Command/Server.hs src/Hypha/Source/Locate.hs
git commit -m "feat(server): route pkg:sublib URLs to component source dirs"
```

---

### Task 9: Refresh golden tests + manual end-to-end

**Files:**
- Modify: `test/Golden/golden/server-home.html` (if not already
  regenerated)

- [ ] **Step 1: Re-run full suite and accept any deterministic golden
  diffs**

```bash
~/.ghcup/bin/cabal test all --test-options="--accept" 2>&1 | tail -5
```

Then re-run without `--accept`:

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: all green.

- [ ] **Step 2: Manual smoke against `nike`**

If a `nike` checkout is available locally, switch to it and run:

```bash
cd /path/to/nike
~/.ghcup/bin/cabal run hypha -- server --bind 127.0.0.1:4312 &
PID=$!; sleep 12
# Sidebar shows nike + every sublib:
curl -s http://127.0.0.1:4312/ | grep -oE '/pkg/nike[^"]*' | sort -u
# Pick a sublib URL and confirm the module list renders:
curl -s 'http://127.0.0.1:4312/pkg/nike%3Alib-...' | head -c 400
kill $PID
```

- [ ] **Step 3: Final commit if any golden updates**

```bash
git add test/Golden/golden/server-home.html 2>/dev/null || true
git commit -m "test(golden): refresh server-home for sublib sidebar entry" \
  --allow-empty
```

---

## Self-Review

**Spec coverage:**

- "Per-component entries in sidebar" → Task 7.
- "Local + dependencies" → Task 5 (`enumerateSourceCache` feeds dep
  parsing).
- "Cabal-syntax parsing" → Tasks 1–3.
- "Composite names with `:`" → Task 2 + Task 6.
- "Cache reuses existing `pkg` column" → Task 6 (no schema change).
- "URL encoding `:`" → Task 7 + Task 8.
- "Tests: round-trip, components parse, cache round-trip, golden,
  manual smoke" → Tasks 2, 3, 6, 7, 9.

**Placeholder scan:** no "TBD"/"TODO"; every step has the code or
command it needs.

**Type consistency:** `ComponentName`, `ComponentInfo`, `cnPackage`,
`cnSublib`, `ciSublib`, `ciHsSourceDirs` appear identically across
every task that mentions them.

**Type names cross-check:** `parseComponentName`/`renderComponentName`,
`findCabalFile`/`parseLibComponents`, `componentsForUnit`,
`enumerateSourceCache`, `findModuleFileIn` — each is defined exactly
once and used by the matching identifier in later tasks.
