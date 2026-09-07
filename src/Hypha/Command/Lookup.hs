{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The @hypha lookup@ command: tiered symbol resolution.
--
-- Tier order is fixed and short-circuiting:
--
--   1. 'Hypha.Search.PackageCache.lookupByName' — SQLite name index.
--   2. 'Hypha.Hoogle.Local.searchLocal'         — project Hoogle DB.
--   3. 'Hypha.Hoogle.Remote.searchRemote'       — hoogle.haskell.org.
--
-- This module owns the outcome assembly only; the individual tiers
-- live in their own modules so they can be tested independently.
module Hypha.Command.Lookup
  ( -- * Types
    LookupResult (..)
  , Provider (..)
  , LookupOptions (..)
  , RemoteTierOutcome (..)
    -- * Cascade
  , runLookup
    -- * Pure helpers used by tests
  , chooseTiers
  , buildOutcome
    -- * JSON
  , providerToJSON
  , lookupResultToJSON
    -- * Field sets
  , compactKeys
  , fullKeys
  ) where

import Data.Aeson (Value, (.=), object)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Hypha.Cli.Types
import Hypha.Error (HyphaError (..))
import Hypha.Hoogle.Local (HyphaHoogle, searchLocal)
import Hypha.Hoogle.Remote
  ( RemoteAnswer (..), RemoteError (..), RemoteOptions, RemoteSource
  , remoteSourceLabel, searchRemote )
import Hypha.Hoogle.Tier (Tier (..), tierLabel)
import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Output.Outcome (Outcome (..))
import Hypha.Search.Cache (CacheScope, VersionedRow (..))
import Hypha.Search.Index (IndexRow (..))
import Hypha.Search.PackageCache (HyphaPackageCache, lookupByName)
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (Version (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))

-- | A single hit, tagged with its origin tier.
data Provider = Provider
  { pPkg     :: !Text
  , pMod     :: !Text
  , pName    :: !Text
  , pSig     :: !Text
  , pTier    :: !Tier
  , pVersion :: !(Maybe Version)
    -- ^ The version the row was indexed under.  'Just' on the cache
    -- tier, which reads a @(pkg, version)@ entry and so always knows;
    -- 'Nothing' on the Hoogle tiers, which answer with a package name
    -- and no version.  Without it two rows for the same symbol in the
    -- same module of the same package — the shape a cache holding two
    -- versions produces — render identically and read as a duplicate.
  }
  deriving stock (Show, Eq)

-- | Result body of a successful 'runLookup'.
data LookupResult = LookupResult
  { lrQuery          :: !Text
  , lrProviders      :: ![Provider]
  , lrTiersConsulted :: ![Tier]
  , lrResolvedFrom   :: !(Maybe RemoteSource)
    -- ^ How the remote tier's answer was obtained, when the remote tier
    -- is the one that answered.
    --
    -- 'Nothing' for tiers 1 and 2: they never touch the network, and
    -- reporting @cache@ for them would name the wrong cache — 'TierCache'
    -- is hypha's index of the project's own build plan, not the blob
    -- cache of Hoogle responses this field is about.
  }
  deriving stock (Show, Eq)

data LookupOptions = LookupOptions
  { loOffline      :: !Bool
  , loCacheScope   :: !CacheScope
    -- ^ Which of the shared cache's versions tier 1 may answer from.
    -- The DB is keyed on @(package, version)@ and shared across every
    -- project on the host, so left unscoped it answers from the host's
    -- indexing history rather than from this project's plan.
  , loRemote       :: !RemoteOptions
  , loPrepareLocal :: !(IO ())
    -- ^ Action invoked /just-in-time/ before tier 2 ('searchLocal').
    -- Used to bring the project's local Hoogle DB up to date.  Hoisted
    -- into 'LookupOptions' so the cascade can skip the prep work
    -- entirely when tier 1 hits: the package cache lookup is a single
    -- SQLite read, but ensuring the Hoogle DB freshness involves
    -- enumerating units and re-indexing stale ones, which is
    -- expensive and noisy.  Defaulting to @pure ()@ keeps the cascade
    -- usable from test harnesses that don't care about tier 2.
  }

-- | What happened on the remote tier, when the earlier tiers were empty
-- and the cascade actually reached it.  Used by 'buildOutcome' so we
-- never have to overload a 'RemoteError' constructor with a sentinel
-- value (the prior @RemoteHttp \"NOT_FOUND\"@ trick was banished).
data RemoteTierOutcome
    -- | Earlier tier hit; remote tier never consulted.
  = RemoteNotConsulted
    -- | @--offline@ and no cached answer on disk (the network call
    -- was never attempted).
  | RemoteSkippedOffline
    -- | Remote returned with zero hits.
  | RemoteEmpty
    -- | Remote call failed with the embedded structured error.
  | RemoteFailed !RemoteError
  deriving stock (Show, Eq)

-- | Top-level entry point.  Runs the cascade and assembles an
-- 'Outcome' for the CLI envelope encoder.
runLookup
  :: HyphaPackageCache
  -> HyphaHoogle
  -> LookupOptions
  -> HoogleQuery
  -> IO (Either HyphaError (Outcome Value))
runLookup cache hoogleLocal opts q@(HoogleQuery qText) = do
  -- Tier 1
  cacheHits <- lookupByName cache (loCacheScope opts) qText
  case cacheHits of
    (_:_) -> pure (buildOutcome q
                    (map (toProvider TierCache) cacheHits)
                    [TierCache] Nothing RemoteNotConsulted)
    [] -> do
      -- Tier 2: bring the local Hoogle DB up to date only now that we
      -- actually need it (tier 1 missed).
      loPrepareLocal opts
      localHits <- searchLocal hoogleLocal q
      case localHits of
        (_:_) -> pure (buildOutcome q
                        (map (hitProvider TierLocalHoogle) localHits)
                        [TierCache, TierLocalHoogle] Nothing
                        RemoteNotConsulted)
        [] -> do
          -- Tier 3
          remote <- searchRemote (loRemote opts) cache q
          let tiersWithRemote =
                [TierCache, TierLocalHoogle, TierRemoteHoogle]
          case remote of
            Right answer | not (null (raHits answer)) ->
              pure (buildOutcome q
                     (map (hitProvider TierRemoteHoogle) (raHits answer))
                     tiersWithRemote (Just (raResolvedFrom answer))
                     RemoteNotConsulted)
            -- An empty answer is a failure the caller is told about, and
            -- no provider carries the provenance, so it goes unreported
            -- here rather than being attached to nothing.
            Right _ ->
              pure (buildOutcome q [] tiersWithRemote Nothing RemoteEmpty)
            Left RemoteOffline ->
              pure (buildOutcome q []
                     [TierCache, TierLocalHoogle] Nothing
                     RemoteSkippedOffline)
            Left e ->
              pure (buildOutcome q [] tiersWithRemote Nothing
                     (RemoteFailed e))

-- | Pure outcome assembly.  When providers are present we build a
-- success 'Outcome'; otherwise the failure is surfaced as a typed
-- 'HyphaError' carrying the original 'HoogleQuery', the @['Tier']@
-- consulted, and the structured 'RemoteError' (when relevant).  Wire
-- rendering (comma-joined tier list, etc.) belongs to
-- "Hypha.Error.errorActions" — not here.
buildOutcome
  :: HoogleQuery           -- ^ query (carried as a domain type)
  -> [Provider]            -- ^ providers (may be empty on failure)
  -> [Tier]                -- ^ tiers actually consulted
  -> Maybe RemoteSource    -- ^ how the remote tier answered, if it did
  -> RemoteTierOutcome     -- ^ what happened on the remote tier
  -> Either HyphaError (Outcome Value)
buildOutcome q providers tiers resolvedFrom remoteOutcome =
  case (providers, remoteOutcome) of
    (_:_, _) -> Right $ Outcome
      { outcomeResult      = lookupResultToJSON
                               (LookupResult (unHoogleQuery q) providers
                                             tiers resolvedFrom)
      , outcomeTag         = LookupCmd
      , outcomeOutsidePlan = False
      , outcomeOverrides   = []
      , outcomeActions     = Map.fromList
          [ ( pPkg p <> "/" <> pMod p <> "/" <> pName p
            , "hypha symbol "
                <> pPkg p <> "/" <> pMod p <> "/" <> pName p )
          | p <- take 5 providers
          ]
      }

    ([], RemoteSkippedOffline) -> Left (HoogleOffline      q tiers)
    ([], RemoteFailed e)       -> Left (HoogleRemoteError  q tiers e)
    ([], RemoteEmpty)          -> Left (HoogleNotFound     q tiers)
    -- Unreachable in practice (earlier tiers produced no providers
    -- so the cascade always consults remote), but kept total.
    ([], RemoteNotConsulted)   -> Left (HoogleNotFound     q tiers)

-- | Pure tier-prefix model used by property tests.  Mirrors the
-- short-circuit logic of 'runLookup' without performing IO: an
-- offline warm hit still reaches tier 3 (served from its cache), so
-- only "offline and tier 3 found nothing" stops the prefix short.
chooseTiers
  :: Bool   -- ^ Tier 1 hit?
  -> Bool   -- ^ Tier 2 hit?
  -> Bool   -- ^ offline? (gates only the network call, not the cache)
  -> Bool   -- ^ Tier 3 answered? (from disk under @--offline@, else the network)
  -> [Tier]
chooseTiers t1 t2 offline t3
  | t1                 = [TierCache]
  | t2                 = [TierCache, TierLocalHoogle]
  | offline && not t3  = [TierCache, TierLocalHoogle]
  | otherwise          = [TierCache, TierLocalHoogle, TierRemoteHoogle]

-- Adapters --------------------------------------------------------------

-- | A cached index row as a lookup provider.  The row's definition
-- module and visibility do not appear in @hypha lookup@'s output shape,
-- which reports where a symbol is /available/, not where it is declared.
-- Built with record syntax rather than positionally: 'Provider' now has
-- two adjacent fields the type checker cannot tell apart by shape, and
-- the same positional habit is what filed a doctor's @all_pass@ as its
-- @outside_plan@.
toProvider :: Tier -> VersionedRow -> Provider
toProvider t vr = Provider
  { pPkg     = unComponentKey (rowComponent r)
  , pMod     = unModulePath   (rowModule r)
  , pName    = unSymbolName   (rowName r)
  , pSig     = unSignature    (rowSignature r)
  , pTier    = t
  , pVersion = Just (vrVersion vr)
  }
  where
    r = vrRow vr

hitProvider :: Tier -> HoogleHit -> Provider
hitProvider t h = Provider
  { pPkg     = hhPackage h
  , pMod     = hhModule h
  , pName    = hhName h
  , pSig     = hhSig h
  , pTier    = t
  , pVersion = Nothing
  }

-- JSON ------------------------------------------------------------------

providerToJSON :: Provider -> Value
providerToJSON p = object $
  [ "pkg"  .= pPkg p
  , "mod"  .= pMod p
  , "name" .= pName p
  , "sig"  .= pSig p
  , "tier" .= tierLabel (pTier p)
  ]
  -- Emitted only when known: a Hoogle hit carries a package name and no
  -- version, and a @version: null@ would read as "no version exists".
  <> [ "version" .= unVersion v | Just v <- [pVersion p] ]

-- | Compact field set: what a consumer reads by default.
--
-- @tiers_consulted@ is deliberately absent — it is redundant with the
-- per-provider @tier@.  @resolved_from@ is deliberately present, for the
-- opposite reason: it is the one thing @tier@ cannot say, since a row
-- from the remote tier's blob cache and one fetched a moment ago carry
-- the same tier (issue #41).  A provenance field only visible under
-- @--select full@ would not have prevented the debugging session that
-- motivated it.
compactKeys :: Set Text
compactKeys = Set.fromList ["query", "providers", "resolved_from"]

-- | Full field set.
fullKeys :: Set Text
fullKeys =
  Set.fromList ["query", "providers", "tiers_consulted", "resolved_from"]

lookupResultToJSON :: LookupResult -> Value
lookupResultToJSON r = object $
  [ "query"           .= lrQuery r
  , "providers"       .= map providerToJSON (lrProviders r)
  , "tiers_consulted" .= map tierLabel (lrTiersConsulted r)
  ]
  -- Emitted only when the remote tier answered.  Absent is the honest
  -- answer for the local tiers rather than a "n/a" a consumer has to
  -- special-case.
  <> [ "resolved_from" .= remoteSourceLabel src
     | Just src <- [lrResolvedFrom r] ]
