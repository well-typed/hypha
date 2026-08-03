{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for the order the indexer walks units in.
--
-- A component's cross-package re-exports resolve against the rows its
-- dependencies already produced, so "dependencies first" is a correctness
-- requirement here, not a performance one.
module Unit.BuildPlanOrder (tests) where

import qualified Data.Map.Strict as Map
import           Data.Text (Text)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan
  , topologicalOrder )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

pid :: Text -> PackageId
pid n = PackageId (PackageName n) (Version "1.0")

unit :: Text -> [Text] -> (PackageName, PlannedUnit)
unit n deps =
  ( PackageName n
  , PlannedUnit
      { puId            = pid n
      , puDeps          = map pid deps
      , puIsLocal       = False
      , puOrigin        = OriginDistribution
      , puSrcDir        = Nothing
      , puDistDir       = Nothing
      , puLibComponents = []
      }
  )

-- | base -> ghc-internal -> ghc-prim, the shape this work exists for.
plan :: BuildPlan
plan = emptyBuildPlan
  { bpUnits = Map.fromList
      [ unit "base"         ["ghc-internal", "ghc-prim"]
      , unit "ghc-internal" ["ghc-prim"]
      , unit "ghc-prim"     []
      ]
  }

names :: [PackageId] -> [Text]
names = map (unPackageName . pkgName)

tests :: TestTree
tests = testGroup "Unit.BuildPlanOrder"
  [ testCase "dependencies come before dependents" $
      names (topologicalOrder plan [pid "base", pid "ghc-internal", pid "ghc-prim"])
        @?= ["ghc-prim", "ghc-internal", "base"]

  , testCase "the input order does not decide the result" $
      names (topologicalOrder plan [pid "ghc-prim", pid "base", pid "ghc-internal"])
        @?= ["ghc-prim", "ghc-internal", "base"]

  , testCase "a dependency absent from the input does not constrain the order" $
      -- ghc-internal is already cached, so it is not in the list.  base
      -- must still come out, and ghc-prim before it.
      names (topologicalOrder plan [pid "base", pid "ghc-prim"])
        @?= ["ghc-prim", "base"]

  , testCase "a unit the plan does not know is kept" $
      assertBool "unknown unit present"
        (pid "mystery" `elem` topologicalOrder plan [pid "base", pid "mystery"])

  , testCase "a cycle yields every unit exactly once" $ do
      let cyclic = emptyBuildPlan
            { bpUnits = Map.fromList [ unit "a" ["b"], unit "b" ["a"] ] }
          out = topologicalOrder cyclic [pid "a", pid "b"]
      length out @?= 2
      assertBool "a present" (pid "a" `elem` out)
      assertBool "b present" (pid "b" `elem` out)
  ]
