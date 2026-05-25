{-# LANGUAGE OverloadedStrings #-}
-- | Golden coverage of the @hypha lookup@ outcome envelope.  Each
-- golden is produced by feeding a canned 'buildOutcome' through the
-- standard encoder; no real cache or Hoogle DB is touched.  This
-- means the goldens are stable regardless of network / filesystem
-- state, while still snapshotting the JSON shape an agent would
-- observe.
module Golden.Lookup (tests) where

import Data.Aeson (Value)
import qualified Data.Set as Set
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Hypha.Cli.Types (ClientCommandTag (..))
import Hypha.Command.Lookup
  ( Provider (..), Tier (..), buildOutcome )
import Hypha.Hoogle.Remote (RemoteError (..))
import Hypha.Output.Json (EnvelopeOpts (..), encodeOutcomeBytes)
import Hypha.Output.Outcome (Outcome)

tests :: TestTree
tests = testGroup "Golden.Lookup"
  [ goldenCase "lookup-cache-hit"         cacheHitOutcome
  , goldenCase "lookup-local-hoogle-hit"  localHitOutcome
  , goldenCase "lookup-remote-hit"        remoteHitOutcome
  , goldenCase "lookup-all-miss"          missOutcome
  , goldenCase "lookup-remote-error"      remoteErrorOutcome
  , goldenCase "lookup-offline"           offlineOutcome
  ]
  where
    goldenCase name outcome = goldenVsString name (goldPath name)
                                                  (pure (encode outcome))
    goldPath n = "test" </> "Golden" </> "golden" </> (n <> ".compact.json")
    encode outcome = encodeOutcomeBytes
      (EnvelopeOpts False [] False)
      LookupCmd
      (Set.fromList ["query", "providers", "tiers_consulted"])
      (Set.fromList ["query", "providers", "tiers_consulted"])
      outcome

mkProvider :: Tier -> Provider
mkProvider t = Provider "containers" "Data.Map" "lookup"
                 "Ord k => k -> Map k a -> Maybe a" t

cacheHitOutcome :: Outcome Value
cacheHitOutcome =
  buildOutcome "lookup" [mkProvider TierCache] [TierCache] Nothing

localHitOutcome :: Outcome Value
localHitOutcome =
  buildOutcome "lookup"
    [mkProvider TierLocalHoogle]
    [TierCache, TierLocalHoogle] Nothing

remoteHitOutcome :: Outcome Value
remoteHitOutcome =
  buildOutcome "lookup"
    [mkProvider TierRemoteHoogle]
    [TierCache, TierLocalHoogle, TierRemoteHoogle] Nothing

missOutcome :: Outcome Value
missOutcome = buildOutcome "doesNotExist" []
  [TierCache, TierLocalHoogle, TierRemoteHoogle] Nothing

remoteErrorOutcome :: Outcome Value
remoteErrorOutcome = buildOutcome "x" []
  [TierCache, TierLocalHoogle, TierRemoteHoogle]
  (Just (RemoteHttp "timeout after 10s"))

offlineOutcome :: Outcome Value
offlineOutcome = buildOutcome "x" []
  [TierCache, TierLocalHoogle] (Just RemoteOffline)
