{-# LANGUAGE OverloadedStrings #-}
module Unit.Deps (tests) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Command.Deps (runDeps)
import Hypha.Output.Outcome (outcomeResult, outcomeActions)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

-- | Build a test plan with known dependency structure:
--   async depends on stm, hashable
--   stm depends on array
--   hashable has no deps
--   array has no deps
mkTestPlan :: BuildPlan
mkTestPlan = emptyBuildPlan
  { bpUnits = Map.fromList
      [ (PackageName "async", PlannedUnit
          { puId = PackageId (PackageName "async") (Version "2.2.5")
          , puDeps =
              [ PackageId (PackageName "stm") (Version "2.5.1")
              , PackageId (PackageName "hashable") (Version "1.4.4")
              ]
          , puIsLocal = False
          , puOrigin = OriginHackage
          , puSrcDir  = Nothing
          , puDistDir = Nothing
          , puLibComponents = []
          })
      , (PackageName "stm", PlannedUnit
          { puId = PackageId (PackageName "stm") (Version "2.5.1")
          , puDeps =
              [ PackageId (PackageName "array") (Version "0.5.6")
              ]
          , puIsLocal = False
          , puOrigin = OriginHackage
          , puSrcDir  = Nothing
          , puDistDir = Nothing
          , puLibComponents = []
          })
      , (PackageName "hashable", PlannedUnit
          { puId = PackageId (PackageName "hashable") (Version "1.4.4")
          , puDeps = []
          , puIsLocal = False
          , puOrigin = OriginHackage
          , puSrcDir  = Nothing
          , puDistDir = Nothing
          , puLibComponents = []
          })
      , (PackageName "array", PlannedUnit
          { puId = PackageId (PackageName "array") (Version "0.5.6")
          , puDeps = []
          , puIsLocal = False
          , puOrigin = OriginHackage
          , puSrcDir  = Nothing
          , puDistDir = Nothing
          , puLibComponents = []
          })
      ]
  }

tests :: TestTree
tests = testGroup "Deps"
  [ testCase "forward deps of async" $ do
      oc <- runDeps mkTestPlan (PackageName "async") False Nothing
      extractDeps (outcomeResult oc) @?=
        [ ("stm", "2.5.1")
        , ("hashable", "1.4.4")
        ]

  , testCase "reverse deps of stm" $ do
      oc <- runDeps mkTestPlan (PackageName "stm") True Nothing
      extractDeps (outcomeResult oc) @?= [("async", "2.2.5")]

  , testCase "reverse deps of array" $ do
      oc <- runDeps mkTestPlan (PackageName "array") True Nothing
      extractDeps (outcomeResult oc) @?= [("stm", "2.5.1")]

  , testCase "forward deps of leaf package" $ do
      oc <- runDeps mkTestPlan (PackageName "hashable") False Nothing
      extractDeps (outcomeResult oc) @?= []

  , testCase "forward deps with depth bound" $ do
      oc <- runDeps mkTestPlan (PackageName "async") False (Just 1)
      extractDeps (outcomeResult oc) @?= [("stm", "2.5.1")]

  , testCase "action hints for forward deps" $ do
      oc <- runDeps mkTestPlan (PackageName "async") False Nothing
      let acts = outcomeActions oc
      Map.lookup "stm"      acts @?= Just "hypha package stm"
      Map.lookup "hashable" acts @?= Just "hypha package hashable"
      Map.size acts @?= 2
  , testCase "deps of unknown package" $ do
      oc <- runDeps mkTestPlan (PackageName "nonexistent") False Nothing
      extractDeps (outcomeResult oc) @?= []
  ]

-- | Extract dependency list from the outcome value.
-- Returns list of (package name, version) pairs.
extractDeps :: Value -> [(String, String)]
extractDeps v = case v of
  Aeson.Object o ->
    case KM.lookup (Key.fromString "deps") o of
      Just (Aeson.Array arr) -> concatMap extractPair (V.toList arr)
      _ -> []
  _ -> []
  where
    extractPair (Aeson.Object dep) =
      case (KM.lookup (Key.fromString "package") dep, KM.lookup (Key.fromString "version") dep) of
        (Just (Aeson.String pkg), Just (Aeson.String ver)) -> [(Text.unpack pkg, Text.unpack ver)]
        _ -> []
    extractPair _ = []
