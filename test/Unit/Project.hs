{-# LANGUAGE OverloadedStrings #-}
module Unit.Project (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import qualified Data.Map.Strict as Map
import System.FilePath ((</>))

import Hypha.Types.BuildPlan
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Project.Discovery (DiscoveryError (..), discoverProjectRoot)
import Hypha.Project.Plan (PlanError (..), loadBuildPlan)
import Hypha.Project.Overrides (OverrideError (..), parsePackageOverride)

tests :: TestTree
tests = testGroup "Unit.Project"
  [ testDiscovery
  , testLoadBuildPlan
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

testLoadBuildPlan :: TestTree
testLoadBuildPlan = testCase "loadBuildPlan parses fixture plan.json" $ do
  let fixtureDir = "test" </> "fixtures" </> "tiny-project"
  result <- loadBuildPlan (ProjectRoot fixtureDir)
  case result of
    Left err -> error ("Expected to parse plan, got: " ++ show err)
    Right bp -> do
      -- Check compiler
      bpCompiler bp @?= CompilerId "ghc-9.6.7"
      -- Check packages
      let pkgs = bpPackages bp
      Map.lookup (PackageName "async") pkgs @?= Just (Version "2.2.5")
      Map.lookup (PackageName "base") pkgs @?= Just (Version "4.18.3.0")
      Map.lookup (PackageName "text") pkgs @?= Just (Version "2.0.2")
      Map.lookup (PackageName "nonexistent") pkgs @?= Nothing

testParseOverride :: TestTree
testParseOverride = testCase "parsePackageOverride parses async=2.2.6" $ do
  let result = parsePackageOverride "async=2.2.6"
  case result of
    Left err -> error ("Expected to parse override, got: " ++ show err)
    Right (PackageOverride name ver) -> do
      name @?= PackageName "async"
      ver @?= Version "2.2.6"

testApplyOverrides :: TestTree
testApplyOverrides = testCase "applyOverrides changes pinned version in plan" $ do
  let fixtureDir = "test" </> "fixtures" </> "tiny-project"
  result <- loadBuildPlan (ProjectRoot fixtureDir)
  case result of
    Left err -> error ("Expected to parse plan, got: " ++ show err)
    Right bp -> do
      -- Override async from 2.2.5 to 2.2.6
      let override = PackageOverride (PackageName "async") (Version "2.2.6")
          bp' = applyOverrides [override] bp
      -- Check that the override took effect
      Map.lookup (PackageName "async") (bpPackages bp') @?= Just (Version "2.2.6")
      -- Check that other packages are unchanged
      Map.lookup (PackageName "base") (bpPackages bp') @?= Just (Version "4.18.3.0")
      -- Check that overrides are recorded
      bpOverrides bp' @?= [override]
