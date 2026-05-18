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
import Hypha.Output.Outcome (outcomeResult, outcomeRelated, Related (..))
import Hypha.Types.BuildPlan (BuildPlan (..), PlannedUnit (..), emptyBuildPlan)
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
          })
      , (PackageName "stm", PlannedUnit
          { puId = PackageId (PackageName "stm") (Version "2.5.1")
          , puDeps =
              [ PackageId (PackageName "array") (Version "0.5.6")
              ]
          })
      , (PackageName "hashable", PlannedUnit
          { puId = PackageId (PackageName "hashable") (Version "1.4.4")
          , puDeps = []
          })
      , (PackageName "array", PlannedUnit
          { puId = PackageId (PackageName "array") (Version "0.5.6")
          , puDeps = []
          })
      ]
  }

tests :: TestTree
tests = testGroup "Deps"
  [ testCase "forward deps of async" $ do
      oc <- runDeps mkTestPlan (PackageName "async") False Nothing
      case outcomeResult oc of
        Nothing -> error "expected success"
        Just v -> extractDeps v @?=
          [ ("stm", "2.5.1")
          , ("hashable", "1.4.4")
          ]

  , testCase "reverse deps of stm" $ do
      oc <- runDeps mkTestPlan (PackageName "stm") True Nothing
      case outcomeResult oc of
        Nothing -> error "expected success"
        Just v -> extractDeps v @?= [("async", "2.2.5")]

  , testCase "reverse deps of array" $ do
      oc <- runDeps mkTestPlan (PackageName "array") True Nothing
      case outcomeResult oc of
        Nothing -> error "expected success"
        Just v -> extractDeps v @?= [("stm", "2.5.1")]

  , testCase "forward deps of leaf package" $ do
      oc <- runDeps mkTestPlan (PackageName "hashable") False Nothing
      case outcomeResult oc of
        Nothing -> error "expected success"
        Just v -> extractDeps v @?= []

  , testCase "forward deps with depth bound" $ do
      oc <- runDeps mkTestPlan (PackageName "async") False (Just 1)
      case outcomeResult oc of
        Nothing -> error "expected success"
        Just v -> extractDeps v @?= [("stm", "2.5.1")]

  , testCase "related links for forward deps" $ do
      oc <- runDeps mkTestPlan (PackageName "async") False Nothing
      let rels = outcomeRelated oc
      length rels @?= 2
      relatedLabel (head rels) @?= "stm"
      relatedFetch (head rels) @?= "hypha package stm"

  , testCase "deps of unknown package" $ do
      oc <- runDeps mkTestPlan (PackageName "nonexistent") False Nothing
      case outcomeResult oc of
        Nothing -> error "expected success"
        Just v -> extractDeps v @?= []
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
