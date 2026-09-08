{-# LANGUAGE OverloadedStrings #-}
module Unit.Project (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import qualified Data.Map.Strict as Map

import Hypha.Project.Discovery (DiscoveryError (..), cabalStoreBase)

import Hypha.Types.BuildPlan
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Project.Discovery (discoverProjectRoot)
import Hypha.Project.Plan (PlanPin (..), loadBuildPlan, loadPlanVersions)
import Hypha.Project.Overrides (parsePackageOverride)

tests :: TestTree
tests = testGroup "Unit.Project"
  [ testDiscovery
  , testDiscoveryIgnoresDotCabalDir
  , testStoreDirFromProjectFiles
  , testLoadBuildPlan
  , testLoadPlanVersionsAgrees
  , testParseOverride
  , testApplyOverrides
  ]

testDiscovery :: TestTree
testDiscovery = testCase "discoverProjectRoot finds tiny-project fixture" $ do
  -- Get the fixture path relative to the test directory
  let fixtureDir = "test" </> "fixtures" </> "tiny-project"
  result <- discoverProjectRoot (Just fixtureDir)
  case result of
    Left err -> error ("Expected to find project root, got: " ++ show err)
    Right (ProjectRoot root) -> do
      -- The root should end with the tiny-project directory
      assertBool "root should contain tiny-project" ("tiny-project" `elem` pathSegments root)
  where
    pathSegments = words . map (\c -> if c == '/' then ' ' else c)

-- | Regression: walking up from a non-project subdir must NOT
-- stop at a directory whose only \"cabal-ish\" entry is a @.cabal@
-- /directory/ (the per-user cabal config dir, common at @~/@).
-- A real cabal project requires either @cabal.project@ or a
-- @<pkg>.cabal@ /file/ with a non-empty stem.
testDiscoveryIgnoresDotCabalDir :: TestTree
testDiscoveryIgnoresDotCabalDir =
  testCase "discoverProjectRoot ignores a `.cabal` config directory" $
    withSystemTempDirectory "hypha-discovery" $ \fakeHome -> do
      -- Mimic ~/ shape: only a `.cabal` directory, no real .cabal file.
      createDirectory (fakeHome </> ".cabal")
      createDirectory (fakeHome </> "subdir")
      result <- discoverProjectRoot (Just (fakeHome </> "subdir"))
      case result of
        Right (ProjectRoot r) ->
          error ("Expected NoProjectFound, found " <> r)
        Left NoProjectFound{} -> pure ()

-- | @store-dir@ is read the way cabal reads it: @cabal.project.local@
-- over @cabal.project@, a relative path against the project root.
-- A project built into a non-default store is otherwise served from
-- whatever @~/.cabal/store@ happens to hold, silently.
testStoreDirFromProjectFiles :: TestTree
testStoreDirFromProjectFiles =
  testCase "cabalStoreBase honours store-dir from the project files" $
    withSystemTempDirectory "hypha-store" $ \root -> do
      writeFile (root </> "cabal.project") $ unlines
        [ "packages: .", "store-dir: /stores/from-project" ]
      fromProject <- cabalStoreBase (Just (ProjectRoot root))
      fromProject @?= "/stores/from-project"

      writeFile (root </> "cabal.project.local") $ unlines
        [ "tests: true", "-- the store this checkout builds into"
        , "store-dir: /stores/from-local", "", "package foo"
        , "  ghc-options: -Wall", "  store-dir: /stores/not-a-thing-here" ]
      fromLocal <- cabalStoreBase (Just (ProjectRoot root))
      fromLocal @?= "/stores/from-local"

      writeFile (root </> "cabal.project.local") "store-dir: .store\n"
      relative <- cabalStoreBase (Just (ProjectRoot root))
      relative @?= root </> ".store"

testLoadBuildPlan :: TestTree
testLoadBuildPlan = testCase "loadBuildPlan parses fixture plan.json" $
  withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
    let fixtureDir = "test" </> "fixtures" </> "tiny-project"
    result <- loadBuildPlan cacheDir (ProjectRoot fixtureDir)
    case result of
      Left err -> error ("Expected to parse plan, got: " ++ show err)
      Right bp -> do
        -- Check compiler
        bpCompiler bp @?= CompilerId "ghc-9.6.7"
        -- Check packages
        lookupPackage (PackageName "async") bp @?= Just (Version "2.2.5")
        lookupPackage (PackageName "base") bp @?= Just (Version "4.18.3.0")
        lookupPackage (PackageName "text") bp @?= Just (Version "2.0.2")
        lookupPackage (PackageName "nonexistent") bp @?= Nothing
        -- mylib contributes two units (lib + exe:myexe).  The map is
        -- keyed by package name, so the library-carrying unit must be
        -- the one retained: its dist-dir holds the rendered Haddock.
        case lookupUnit (PackageName "mylib") bp of
          Nothing -> error "Expected a unit for mylib"
          Just pu -> puDistDir pu @?= Just
            ("test/fixtures/tiny-project/dist-newstyle/build/x86_64-linux/ghc-9.6.7/mylib-0.1.0")

-- | The cheap reader must agree with the full one, or pinning a lookup
-- to the plan would pin it to a different plan than the one every other
-- command resolves against.
testLoadPlanVersionsAgrees :: TestTree
testLoadPlanVersionsAgrees =
  testCase "loadPlanVersions agrees with the full plan's pins" $
    withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
      let fixtureDir = "test" </> "fixtures" </> "tiny-project"
      full  <- loadBuildPlan     cacheDir (ProjectRoot fixtureDir)
      light <-                   loadPlanVersions (ProjectRoot fixtureDir)
      case (full, light) of
        (Right bp, Right pins) -> do
          Map.map ppVersion pins @?= planVersions bp
          -- The unit-id too: it is what the index cache keys on, so the
          -- cheap reader disagreeing would scope a lookup to
          -- configurations the rest of hypha never writes.
          Map.map ppUnitId  pins @?= planUnitIds bp
        _ -> error "Expected both plan readers to parse the fixture"

testParseOverride :: TestTree
testParseOverride = testCase "parsePackageOverride parses async=2.2.6" $ do
  let result = parsePackageOverride "async=2.2.6"
  case result of
    Left err -> error ("Expected to parse override, got: " ++ show err)
    Right (PackageOverride name ver) -> do
      name @?= PackageName "async"
      ver @?= Version "2.2.6"

testApplyOverrides :: TestTree
testApplyOverrides = testCase "applyOverrides changes pinned version in plan" $
  withSystemTempDirectory "hypha-plan" $ \cacheDir -> do
    let fixtureDir = "test" </> "fixtures" </> "tiny-project"
    result <- loadBuildPlan cacheDir (ProjectRoot fixtureDir)
    case result of
      Left err -> error ("Expected to parse plan, got: " ++ show err)
      Right bp -> do
        -- Override async from 2.2.5 to 2.2.6
        let override = PackageOverride (PackageName "async") (Version "2.2.6")
            bp' = applyOverrides [override] bp
        -- Check that the override took effect
        lookupPackage (PackageName "async") bp' @?= Just (Version "2.2.6")
        -- Check that other packages are unchanged
        lookupPackage (PackageName "base") bp' @?= Just (Version "4.18.3.0")
        -- Check that overrides are recorded
        bpOverrides bp' @?= [override]
