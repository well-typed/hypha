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

import Hypha.Command.Lookup
  ( Provider (..), RemoteTierOutcome (..), buildOutcome, compactKeys )
import Hypha.Hoogle.Remote (RemoteSource (..))
import Hypha.Error (HyphaError)
import Hypha.Hoogle.Remote (RemoteError (..))
import Hypha.Hoogle.Tier (Tier (..))
import Hypha.Hoogle.Type (HoogleQuery (..))
import Hypha.Output.Json
  ( EnvelopeOpts (..), encodeEnvelopeValue, encodeErrorEnvelope
  , encodeOutcomeBytes )
import Hypha.Types.PackageId (Version (..))
import Hypha.Output.Outcome (Outcome)

tests :: TestTree
tests = testGroup "Golden.Lookup"
  [ goldenCase "lookup-cache-hit"         cacheHitOutcome
  , goldenCase "lookup-local-hoogle-hit"  localHitOutcome
  , goldenCase "lookup-remote-hit"        remoteHitOutcome
  , goldenCase "lookup-remote-cached"     remoteCachedOutcome
  , goldenCase "lookup-all-miss"          missOutcome
  , goldenCase "lookup-remote-error"      remoteErrorOutcome
  , goldenCase "lookup-offline"           offlineOutcome
  ]
  where
    goldenCase name outcome = goldenVsString name (goldPath name)
                                                  (pure (encode outcome))
    goldPath n = "test" </> "Golden" </> "golden" </> (n <> ".compact.json")
    encode = either
      (encodeEnvelopeValue envOpts . encodeErrorEnvelope)
      -- The command's own compact set, not a copy of it: a golden that
      -- pinned its own key list would keep passing while the CLI stopped
      -- emitting the field.
      (encodeOutcomeBytes envOpts compactKeys
        (Set.fromList ["query", "providers", "tiers_consulted"]))
    envOpts = EnvelopeOpts False [] False

mkProvider :: Tier -> Provider
mkProvider t = Provider "containers" "Data.Map" "lookup"
                 "Ord k => k -> Map k a -> Maybe a" t (versionFor t)
  where
    -- Only the cache tier reads a @(pkg, version)@ entry and so has a
    -- version to report; a Hoogle hit carries a package name alone.
    versionFor TierCache = Just (Version "0.6.7")
    versionFor _         = Nothing

cacheHitOutcome :: Either HyphaError (Outcome Value)
cacheHitOutcome =
  buildOutcome (HoogleQuery "lookup") [mkProvider TierCache]
               [TierCache] Nothing RemoteNotConsulted

localHitOutcome :: Either HyphaError (Outcome Value)
localHitOutcome =
  buildOutcome (HoogleQuery "lookup")
    [mkProvider TierLocalHoogle]
    [TierCache, TierLocalHoogle] Nothing RemoteNotConsulted

remoteHitOutcome :: Either HyphaError (Outcome Value)
remoteHitOutcome =
  buildOutcome (HoogleQuery "lookup")
    [mkProvider TierRemoteHoogle]
    [TierCache, TierLocalHoogle, TierRemoteHoogle] (Just FromNetwork)
    RemoteNotConsulted

-- | The shape issue #41 was about: the remote tier answered, but from
-- its blob cache.  Indistinguishable from 'remoteHitOutcome' before
-- @resolved_from@ existed -- both printed @tier: remote-hoogle@ and
-- nothing else.
remoteCachedOutcome :: Either HyphaError (Outcome Value)
remoteCachedOutcome =
  buildOutcome (HoogleQuery "lookup")
    [mkProvider TierRemoteHoogle]
    [TierCache, TierLocalHoogle, TierRemoteHoogle] (Just FromCache)
    RemoteNotConsulted

missOutcome :: Either HyphaError (Outcome Value)
missOutcome = buildOutcome (HoogleQuery "doesNotExist") []
  [TierCache, TierLocalHoogle, TierRemoteHoogle] Nothing RemoteEmpty

remoteErrorOutcome :: Either HyphaError (Outcome Value)
remoteErrorOutcome = buildOutcome (HoogleQuery "x") []
  [TierCache, TierLocalHoogle, TierRemoteHoogle] Nothing
  (RemoteFailed (RemoteHttp "timeout after 10s"))

offlineOutcome :: Either HyphaError (Outcome Value)
offlineOutcome = buildOutcome (HoogleQuery "x") []
  [TierCache, TierLocalHoogle] Nothing RemoteSkippedOffline
