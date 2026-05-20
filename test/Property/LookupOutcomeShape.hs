{-# LANGUAGE OverloadedStrings #-}
module Property.LookupOutcomeShape (tests) where

import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Command.Lookup
  ( Provider (..), Tier (..), buildOutcome )
import Hypha.Hoogle.Remote (RemoteError (..))
import Hypha.Output.Outcome (Outcome (..), OutcomeError (..))

mkProvider :: Provider
mkProvider = Provider "containers" "Data.Map" "lookup" "sig" TierCache

tests :: TestTree
tests = testGroup "Property.LookupOutcomeShape"
  [ testCase "non-empty providers => Success with related links" $
      case buildOutcome "lookup" [mkProvider] [TierCache] Nothing of
        OutcomeSuccess _ _ _ _ rel -> assertBool "has related" (not (null rel))
        OutcomeFailure _ _         -> fail "expected success"

  , testCase "empty providers + offline => HOOGLE_OFFLINE failure" $
      case buildOutcome "x" [] [TierCache, TierLocalHoogle]
              (Just RemoteOffline) of
        OutcomeFailure (OutcomeError code _ _) actions -> do
          code @?= "HOOGLE_OFFLINE"
          assertBool "has actions" (not (Map.null actions))
        OutcomeSuccess {} -> fail "expected failure"

  , testCase "empty providers + remote http error => HOOGLE_REMOTE_ERROR" $
      case buildOutcome "x" []
              [TierCache, TierLocalHoogle, TierRemoteHoogle]
              (Just (RemoteHttp "boom")) of
        OutcomeFailure (OutcomeError code _ _) actions -> do
          code @?= "HOOGLE_REMOTE_ERROR"
          assertBool "carries retry_offline"
            (Map.member "retry_offline" actions)
        OutcomeSuccess {} -> fail "expected failure"

  , testCase "empty providers, no remote error => NOT_FOUND" $
      case buildOutcome "x" []
              [TierCache, TierLocalHoogle, TierRemoteHoogle] Nothing of
        OutcomeFailure (OutcomeError code _ _) _ -> code @?= "NOT_FOUND"
        OutcomeSuccess {} -> fail "expected failure"

  , testCase "remote sentinel NOT_FOUND maps to NOT_FOUND" $
      case buildOutcome "x" []
              [TierCache, TierLocalHoogle, TierRemoteHoogle]
              (Just (RemoteHttp "NOT_FOUND")) of
        OutcomeFailure (OutcomeError code _ _) _ -> code @?= "NOT_FOUND"
        OutcomeSuccess {} -> fail "expected failure"
  ]
