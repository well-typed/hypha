# Local Package Browsing Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `hypha` CLI commands and `hypha server` work against local (inplace) packages — the user's own source code — not just dependencies.

**Architecture:** Enrich `PlannedUnit` with `puSrcDir`/`puDistDir` from plan.json's `uPkgSrc`/`uDistDir`, then use those paths as the first fallback in the source resolution and Haddock resolution chains. Downstream callers (server search index, module views, symbol cards) work automatically via `resolveSrc`.

**Tech Stack:** Haskell, cabal-plan library, existing hypha types/effects

---

### Task 1: Add `puSrcDir` and `puDistDir` to `PlannedUnit`

**Files:**
- Modify: `src/Hypha/Types/BuildPlan.hs` — add fields to `PlannedUnit`
- Modify: `src/Hypha/Project/Plan.hs` — extract new fields from cabal-plan Unit
- Test: `test/Unit/Project.hs` — verify extraction for local units

- [ ] **Step 1: Add fields to `PlannedUnit`**

Add two `Maybe FilePath` fields after `puIsLocal`:

```haskell
data PlannedUnit = PlannedUnit
  { puId       :: !PackageId
  , puDeps     :: ![PackageId]
  , puIsLocal  :: !Bool
  , puSrcDir   :: !(Maybe FilePath)
    -- ^ Source root from plan.json (pkg-src.path).
    -- @Just p@ for inplace\/local packages, @Nothing@ otherwise.
  , puDistDir  :: !(Maybe FilePath)
    -- ^ Build directory from plan.json (dist-dir).  Used to locate
    -- pre-built Haddock HTML for local packages.
  }
  deriving stock (Show, Eq)
```

- [ ] **Step 2: Update all sites that construct `PlannedUnit`**

In `src/Hypha/Types/BuildPlan.hs`, update `applyOverrides`:

```haskell
applyOverride (PackageOverride n v) =
  Map.insertWith (\_ old -> old { puId = (puId old) { pkgVersion = v } }) n
    PlannedUnit { puId = PackageId n v, puDeps = [], puIsLocal = False
                , puSrcDir = Nothing, puDistDir = Nothing }
```

- [ ] **Step 3: Extract from plan.json in `toPlannedUnit`**

In `src/Hypha/Project/Plan.hs`, import `CP.PkgLoc` and add `extractSrcDir` helper:

```haskell
import qualified Cabal.Plan as CP
-- CP.PkgLoc(..) imported via qualified Cabal.Plan

-- In toPlannedUnit, set new fields:
    , puSrcDir  = extractSrcDir (CP.uPkgSrc u)
    , puDistDir = CP.uDistDir u

extractSrcDir :: Maybe CP.PkgLoc -> Maybe FilePath
extractSrcDir (Just (CP.LocalUnpackedPackage p)) = Just p
extractSrcDir _                                   = Nothing
```

- [ ] **Step 4: Update unit tests in `test/Unit/Project.hs`**

Add a test that loads the plan from the tiny-project fixture and verifies that a local (inplace) package has `puSrcDir = Just path`. Since the fixture currently has no local package, add a simplified local unit entry to the test expectations or create a small fixture extension.

```haskell
testCase "local package gets srcDir from plan" $ do
  let fixtureDir = "test" </> "fixtures" </> "tiny-project"
  rootResult <- discoverProjectRoot (Just fixtureDir)
  case rootResult of
    Left err -> assertFailure ("discovery: " <> show err)
    Right root -> do
      planResult <- loadBuildPlan root
      case planResult of
        Left err -> assertFailure ("plan: " <> show err)
        Right plan -> do
          -- The fixture plan has no inplace packages, so puSrcDir should be Nothing:
          case lookupUnit (PackageName "async") plan of
            Nothing -> assertFailure "async not in plan"
            Just pu -> assertBool "async is not local" (not (puIsLocal pu))
                       >> assertBool "async has no srcDir" (isNothing (puSrcDir pu))
```

- [ ] **Step 5: Build and run tests**

```bash
cabal build all && cabal test all --test-option='-p Unit.Project'
```
Expected: all existing tests pass + new test passes.

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Types/BuildPlan.hs src/Hypha/Project/Plan.hs test/Unit/Project.hs
git commit -m "feat(types): add puSrcDir and puDistDir to PlannedUnit"
```

---

### Task 2: Plan-aware source resolution in `PackageResolver`

**Files:**
- Modify: `src/Hypha/Package/Resolver.hs` — add plan path as first fallback in `resolvePackageSourceWith`
- Test: `src/Hypha/Cli/Run.hs` — no change needed (resolver capture handles it)
- Test: `test/Unit/BuildEnv.hs` — add test for local source resolution

- [ ] **Step 1: Modify `resolvePackageSourceWith`**

The function already receives `BuildPlan` indirectly via `mkPackageResolver`'s closure. Add a plan-sourced step before the store lookup:

```haskell
resolvePackageSourceWith
  :: BuildEnv IO
  -> HackageClient IO
  -> FilePath          -- ^ source cache directory
  -> BuildPlan
  -> PackageId
  -> IO (Either HyphaError FilePath)
resolvePackageSourceWith env hclient sourceCache plan pid = do
  -- Step 0: Local package source from plan (fast, no I/O beyond stat).
  case planSrcDir plan (pkgName pid) of
    Just dir -> do
      exists <- doesDirectoryExist dir
      if exists then pure (Right dir) else fallbackToEnv
    Nothing -> fallbackToEnv
  where
    fallbackToEnv = do
      mSrc <- locatePackageSource env pid
      case mSrc of
        Just dir -> pure (Right dir)
        Nothing  -> do
          -- existing download-from-Hackage code unchanged
          ...

-- | Look up the package source dir from the plan's uPkgSrc field.
planSrcDir :: BuildPlan -> PackageName -> Maybe FilePath
planSrcDir plan name =
  case lookupUnit name plan of
    Just pu | Just dir <- puSrcDir pu -> Just dir
    _                                 -> Nothing
```

Add `System.Directory.doesDirectoryExist` to imports.

- [ ] **Step 2: Update `mkPackageResolver` to pass plan**

The function already takes `BuildPlan` as a parameter. Update the `resolveSrc` field to capture and use it:

```haskell
mkPackageResolver env hclient plan = do
  sourceCache <- (</> "source") <$> cacheRoot
  createDirectoryIfMissing True sourceCache
  pure PackageResolver
    { resolvePkg = resolvePackageWith env hclient plan
    , resolveSrc = resolvePackageSourceWith env hclient sourceCache plan
    , fetchVrs   = ...
    }
```

- [ ] **Step 3: Add a unit test for the local source path**

In `test/Unit/BuildEnv.hs`, add:

```haskell
import System.FilePath ((</>))
import Hypha.Types.BuildPlan (BuildPlan(..), PlannedUnit(..), lookupUnit)
import Hypha.Types.PackageId (PackageName(..), PackageId(..), Version(..))
import qualified Data.Map.Strict as Map

testCase "resolveSrc returns local package source dir from plan" $ do
  let pid = PackageId (PackageName "mylib") (Version "0.1.0")
      plan = BuildPlan
        { bpCompiler = CompilerId "ghc-9.6.7"
        , bpUnits = Map.singleton (PackageName "mylib")
            PlannedUnit
              { puId = pid
              , puDeps = []
              , puIsLocal = True
              , puSrcDir = Just "/tmp/fake-src"
              , puDistDir = Nothing
              }
        , bpOverrides = []
        }
      result = planSrcDir plan (PackageName "mylib")
  result @?= Just "/tmp/fake-src"
```

- [ ] **Step 4: Build and run tests**

```bash
cabal build all && cabal test all --test-option='-p Unit.BuildEnv'
```
Expected: all existing tests pass + new test passes.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Package/Resolver.hs test/Unit/BuildEnv.hs
git commit -m "feat(resolver): plan-aware source resolution for local packages"
```

---

### Task 3: Haddock resolution for local packages in `ensureHaddockFor`

**Files:**
- Modify: `src/Hypha/Haddock/Generate.hs` — add dist-dir check
- Modify: `src/Hypha/Command/Server.hs` — thread `BuildPlan` into `ensureHaddockFor` callers

- [ ] **Step 1: Add `BuildPlan` parameter to `ensureHaddockFor`**

Update signature and add dist-dir resolution:

```haskell
module Hypha.Haddock.Generate
  ( haddockDirFor
  , haddockCacheExists
  , ensureHaddockFor
  ) where

import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type (BuildEnv(..))
import Hypha.Cache (haddockCacheRoot)
import Hypha.Types.BuildPlan (BuildPlan, PlannedUnit(..), lookupUnit)
import Hypha.Types.PackageId (PackageId(..), PackageName(..), renderPackageId)

-- | Ensure rendered Haddock HTML is available for a package.
--
-- Resolution order:
-- 1. hypha Haddock cache (@~/.cache/hypha/haddock/...@).
-- 2. Local package dist-dir from the build plan (NEW).
-- 3. Build environment's store location (cabal store / nix store).
ensureHaddockFor :: BuildPlan -> BuildEnv IO -> PackageId -> IO (Maybe FilePath)
ensureHaddockFor plan env pid = do
  inCache <- haddockCacheExists pid
  if inCache
    then do
      dir  <- haddockDirFor pid
      let idx = dir </> "index.html"
      ok <- doesFileExist idx
      pure (if ok then Just idx else Nothing)
    else do
      mDist <- distDirHaddock plan pid
      case mDist of
        Just idx -> pure (Just idx)
        Nothing  -> locateHaddockHtml env pid

-- | Check the plan's dist-dir for pre-built Haddock HTML.
distDirHaddock :: BuildPlan -> PackageId -> IO (Maybe FilePath)
distDirHaddock plan pid =
  case lookupUnit (pkgName pid) plan of
    Just pu | Just d <- puDistDir pu -> do
      let pkgNameStr = Text.unpack (unPackageName (pkgName pid))
          idx = d </> "doc" </> "html" </> pkgNameStr </> "index.html"
      ok <- doesFileExist idx
      if ok then pure (Just idx) else pure Nothing
    _ -> pure Nothing
```

- [ ] **Step 2: Update `Server.hs` call sites**

In `src/Hypha/Command/Server.hs`, `buildServerConfig` calls `ensureHaddockFor` indirectly through the prebuild function and the Haddock callback. Update:

```haskell
-- In prebuildAll, change signature to accept BuildPlan:
prebuildAll :: BuildPlan -> BuildEnv IO -> Int -> [PackageId] -> IO ()
prebuildAll plan env jobs pids = do
  sem <- newQSem (max 1 jobs)
  mapConcurrently_ (withSem sem . ensureOne) pids
  where
    ensureOne pid = do
      r <- try (ensureHaddockFor plan env pid) :: IO (Either SomeException (Maybe FilePath))
      ...
```

In `buildServerConfig`, pass `plan` to the prebuild call and Haddock callbacks. The `scHaddockHtml` closure already captures `resolveSrc` — now also capture `plan` and pass it to `ensureHaddockFor`.

Actually, looking at `buildServerConfig` more carefully, `ensureHaddockFor` is called in the prebuild path (`prebuildAll`) and potentially in the Haddock serving path. Let me check where exactly.

In the current `buildServerConfig`:
```haskell
buildServerConfig plan _env _hclient resolver _hoogle = do
  ...
  , scHaddockHtml  = \pkgVer segments -> do
      let pidM = parsePkgVer pkgVer
      case pidM of
        Nothing  -> pure Nothing
        Just pid -> do
          dir <- haddockDirFor pid  -- this is the cache dir
          ...
```

The `scHaddockHtml` callback doesn't call `ensureHaddocFor` — it directly checks the cache. So the main place where `ensureHaddocFor` is used is in `prebuildAll`. Let me update that.

- [ ] **Step 3: Update `prebuildAll` and `buildServerConfig`**

```haskell
-- In buildServerConfig, pass plan to prebuildAll:
  case soPrebuild opts of
    False -> pure ()
    True  -> prebuildAll plan env (soPrebuildJobs opts) (planPackageIds plan)
```

And `prebuildAll` signature changes as shown above.

- [ ] **Step 4: Build and run tests**

```bash
cabal build all && cabal test all
```
Expected: builds clean, all tests pass.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Haddock/Generate.hs src/Hypha/Command/Server.hs
git commit -m "feat(haddock): resolve pre-built Haddock for local packages via dist-dir"
```

---

### Task 4: Update test fixture with a local package

**Files:**
- Modify: `test/fixtures/tiny-project/dist-newstyle/cache/plan.json` — add a local (inplace) package entry
- Create: `test/fixtures/tiny-project/mylib.cabal` — minimal cabal file for local package
- Create: `test/fixtures/tiny-project/src/MyLib.hs` — source with one module

- [ ] **Step 1: Add a local package entry to the fixture plan.json**

Edit the plan.json to add a `type: "configured"` (inplace) entry for a package called `mylib`:

```json
{
  "type": "configured",
  "id": "mylib-0.1.0-inplace",
  "pkg-name": "mylib",
  "pkg-version": "0.1.0",
  "style": "local",
  "pkg-src": {
    "type": "local",
    "path": "test/fixtures/tiny-project"
  },
  "dist-dir": "test/fixtures/tiny-project/dist-newstyle/build/x86_64-linux/ghc-9.6.7/mylib-0.1.0",
  "component-name": "lib",
  "components": {
    "lib": {
      "depends": [
        "base-4.18.3.0"
      ]
    }
  }
}
```

The path is relative to CWD when tests run (project root).

- [ ] **Step 2: Create a minimal `.cabal` file**

`test/fixtures/tiny-project/mylib.cabal`:

```cabal
cabal-version: 3.0
name: mylib
version: 0.1.0
build-type: Simple

library
  exposed-modules: MyLib
  build-depends: base >=4.18
  hs-source-dirs: src
  default-language: Haskell2010
```

- [ ] **Step 3: Create a minimal source file**

`test/fixtures/tiny-project/src/MyLib.hs`:

```haskell
module MyLib (hello) where

hello :: String
hello = "hello"
```

- [ ] **Step 4: Commit**

```bash
git add test/fixtures/tiny-project/
git commit -m "test: add local (inplace) package to test fixture"
```

---

### Task 5: Golden test — local package JSON output

**Files:**
- Create: `test/Golden/golden/package-local.compact.json` — expected JSON for a local package
- Modify: `test/Golden/Package.hs` — add the golden test case

- [ ] **Step 1: Write the golden test**

In `test/Golden/Package.hs`, add:

```haskell
  , goldenVsString
      "package-local produces expected JSON"
      ("test" </> "Golden" </> "golden" </> "package-local.compact.json")
      runPackageLocalCommand
```

Add `runPackageLocalCommand`:

```haskell
runPackageLocalCommand :: IO LBS.ByteString
runPackageLocalCommand = do
  let fixtureDir = "test" </> "fixtures" </> "tiny-project"
  rootResult <- discoverProjectRoot (Just fixtureDir)
  case rootResult of
    Left err -> error $ "Could not discover project root: " ++ show err
    Right root -> do
      planResult <- loadBuildPlan root
      case planResult of
        Left err -> error $ "Could not load plan: " ++ show err
        Right plan -> do
          -- Now that the fixture includes 'mylib', resolve via the
          -- package resolver to also get exposed modules.
          storeDir <- canonicalizePath ("test" </> "fixtures" </> "fake-cabal-store" </> "ghc-9.6.7")
          eEnv <- mkCabalBuildEnv storeDir
          env <- case eEnv of
            Right be -> pure be
            Left _   -> pure offlineNullBuildEnv
          hclient <- mkOfflineHackageClient
          resolver <- mkPackageResolver env hclient plan
          result <- resolvePkg resolver (PackageName "mylib")
          case result of
            Left err -> error $ "resolve: " ++ show err
            Right rp -> do
              modules <- resolveExposedModules resolver env (rpPkgId rp)
              pure (Aeson.encode (encodeEnvelope "package"
                (Package.mkSuccessOutcome
                  "mylib"
                  (pkgVersion (rpPkgId rp))
                  (rpIsLocal rp)
                  (rpDepsCount rp)
                  modules)))
```

- [ ] **Step 2: Run with `--accept` to generate golden file**

```bash
cabal test all --test-option='--accept' --test-option='-p package-local'
```

Expected: golden file is generated.

- [ ] **Step 3: Verify the golden file content**

Check `test/Golden/golden/package-local.compact.json` shows:

```json
{
  "result": {
    "name": "mylib",
    "version": "0.1.0",
    "in_plan": true,
    "is_local": true,
    "deps_count": 0,
    "exposed_modules": ["MyLib"]
  },
  ...
}
```

- [ ] **Step 4: Commit**

```bash
git add test/Golden/Package.hs test/Golden/golden/package-local.compact.json
git commit -m "test(golden): local package JSON output"
```

---

### Task 6: Final integration pass

- [ ] **Step 1: Full build and test**

```bash
cabal build all && cabal test all
```

Expected: all builds clean, all tests pass (≥90 tests).

- [ ] **Step 2: Quick manual smoke test**

```bash
# Inside a real Haskell project (or hypha itself), run:
cabal build all --dry-run
cabal run -- hypha package hypha --human
```

Expected: see the local package with `is_local: true` and its exposed modules listed.

- [ ] **Step 3: Final commit if any fixes were needed**

```bash
git add -A
git commit -m "chore: fix review issues"
```
