{-# LANGUAGE OverloadedStrings #-}
-- | The ordering must never lose or invent a unit.  A silently dropped
-- unit is a package that simply never gets indexed, with nothing in the
-- output to say so.
module Property.BuildPlanOrder (tests) where

import           Data.Containers.ListUtils (nubOrd)
import           Data.List (sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as Range
import Test.Falsify.Property (assert, gen)

import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan
  , topologicalOrder )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

pid :: Text -> PackageId
pid n = PackageId (PackageName n) (Version "1.0")

-- | Names drawn from a small alphabet, so generated dependency edges
-- actually land on generated units instead of dangling.
genName :: Gen.Gen Text
genName = Gen.elem (NE.fromList ["p0", "p1", "p2", "p3", "p4", "p5"])

genUnit :: Gen.Gen (PackageName, PlannedUnit)
genUnit = do
  n    <- genName
  deps <- Gen.list (Range.between (0, 4)) genName
  pure
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

tests :: TestTree
tests = testGroup "Property.BuildPlanOrder"
  [ testProperty "the order is a permutation of the distinct input" $ do
      units  <- gen (Gen.list (Range.between (0, 8)) genUnit)
      inputs <- gen (Gen.list (Range.between (0, 8)) genName)
      let bp  = emptyBuildPlan { bpUnits = Map.fromList units }
          ins = map pid inputs
      assert (P.eq P..$ ("ordered", sort (topologicalOrder bp ins))
                   P..$ ("distinct input", sort (nubOrd ins)))
  ]
