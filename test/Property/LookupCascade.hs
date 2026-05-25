{-# LANGUAGE OverloadedStrings #-}
module Property.LookupCascade (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import Test.Falsify.Property (gen, assert)

import Hypha.Command.Lookup (chooseTiers)
import Hypha.Hoogle.Tier (Tier (..))

tests :: TestTree
tests = testGroup "Property.LookupCascade"
  [ testProperty "tier prefix matches the short-circuit model" $ do
      t1 <- gen (Gen.bool False)
      t2 <- gen (Gen.bool False)
      offline <- gen (Gen.bool False)
      t3 <- gen (Gen.bool False)
      let expected
            | t1        = [TierCache]
            | t2        = [TierCache, TierLocalHoogle]
            | offline   = [TierCache, TierLocalHoogle]
            | otherwise = [TierCache, TierLocalHoogle, TierRemoteHoogle]
          actual = chooseTiers t1 t2 offline t3
      assert (P.eq P..$ ("expected", expected)
                   P..$ ("actual", actual))
  ]
