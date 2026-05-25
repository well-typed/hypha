{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Error
  ( -- * Umbrella error
    HyphaError (..)
    -- * Classification
  , errorCode
  , errorMessage
  , errorExitCode
  , errorActions
    -- * ExceptT helpers
  , discoverProjectRootE
  , loadBuildPlanE
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Exit
  ( ExitCode, exitUserError, exitNotFound, exitNetworkError, exitCacheError
  , exitEnvironmentError, exitToolMissing )
import Hypha.Hoogle.Remote (RemoteError, renderRemoteError)
import Hypha.Hoogle.Tier (Tier, renderTierList)
import Hypha.Hoogle.Type (HoogleQuery (..))
import Hypha.Project.Discovery (DiscoveryError (..), discoverProjectRoot)
import Hypha.Project.Plan (PlanError (..), loadBuildPlan)
import Hypha.Types.BuildPlan (BuildPlan, ProjectRoot (..))

-- | Umbrella error type produced by hypha.  Every fallible boundary of the
-- CLI funnels through this ADT.  Constructors embed precise sub-errors
-- and any envelope context (query, tiers consulted, ...) needed to
-- render the wire-format error/actions without an auxiliary type.
--
-- Each constructor maps to exactly one 'ExitCode'; totality is checked
-- by the unit tests in @test/Unit/Errors.hs@.
--
-- Per the project ethos (CLAUDE.md, \"Render at the edge\"): the
-- @Hoogle*@ constructors carry domain types — 'HoogleQuery', @['Tier']@,
-- 'RemoteError' — never pre-rendered 'Text'.  Stringification happens
-- in 'errorMessage' \/ 'errorActions', at the wire boundary.
data HyphaError
  = UserError         !Text             -- ^ bad CLI args, malformed path
  | NotFound          !Text             -- ^ pkg/symbol absent from fallback chain
  | NetworkError      !Text             -- ^ --offline cache miss, 429, 503
  | Corruption        !Text             -- ^ cache / parse / on-disk corruption
  | EnvError          !Text             -- ^ store unreachable, generic env failure
  | ToolMissing       !Text             -- ^ haddock/cabal/ghc not on PATH
  | DiscoveryFailure  !DiscoveryError   -- ^ project root discovery failed
  | PlanFailure       !ProjectRoot !PlanError -- ^ plan.json missing/unparseable
    -- | @hypha lookup@: @--offline@ suppressed the remote tier.  Carries
    --   the original query and the tiers actually consulted so the
    --   failure envelope can suggest a retry.
  | HoogleOffline      !HoogleQuery ![Tier]
    -- | @hypha lookup@: no providers found across every tier consulted.
  | HoogleNotFound     !HoogleQuery ![Tier]
    -- | @hypha lookup@: the remote Hoogle tier failed.  The embedded
    --   'RemoteError' is kept structured (not flattened to 'Text') so
    --   downstream consumers can still pattern-match on the cause.
  | HoogleRemoteError  !HoogleQuery ![Tier] !RemoteError
  deriving stock (Show, Eq)

errorCode :: HyphaError -> Text
errorCode = \case
  UserError{}
    -> "USER_ERROR"
  NotFound{}
    -> "NOT_FOUND"
  NetworkError{}
    -> "NETWORK_ERROR"
  Corruption{}
    -> "CORRUPTION"
  EnvError{}
    -> "ENV_ERROR"
  ToolMissing{}
    -> "TOOL_MISSING"
  DiscoveryFailure{}
    -> "ENV_ERROR"
  PlanFailure _ e
    -> case e of
         PlanNotFound{}     -> "ENV_ERROR"
         PlanParseFailure{} -> "CORRUPTION"
  HoogleOffline{}
    -> "HOOGLE_OFFLINE"
  HoogleNotFound{}
    -> "NOT_FOUND"
  HoogleRemoteError{}
    -> "HOOGLE_REMOTE_ERROR"

errorMessage :: HyphaError -> Text
errorMessage = \case
  UserError         msg -> msg
  NotFound          msg -> msg
  NetworkError      msg -> msg
  Corruption        msg -> msg
  EnvError          msg -> msg
  ToolMissing       msg -> msg
  DiscoveryFailure  (NoProjectFound location)
    -> "no cabal project found (searched up from " <> Text.pack location <> ")"
  PlanFailure (ProjectRoot r) e -> case e of
    PlanNotFound pth ->
         "plan.json missing under " <> Text.pack r
      <> " (cabal-plan: " <> Text.pack pth <> ")"
      <> "; run `cabal build --dry-run`"
    PlanParseFailure m -> "plan.json parse failure: " <> Text.pack m
  HoogleOffline      _ _ ->
    "--offline (or HYPHA_OFFLINE) suppresses remote tier"
  HoogleNotFound     _ _ ->
    "no providers found"
  HoogleRemoteError  _ _ remoteErr -> renderRemoteError remoteErr

errorExitCode :: HyphaError -> ExitCode
errorExitCode = \case
  UserError{}
    -> exitUserError
  NotFound{}
    -> exitNotFound
  NetworkError{}
    -> exitNetworkError
  Corruption{}
    -> exitCacheError
  EnvError{}
    -> exitEnvironmentError
  ToolMissing{}
    -> exitToolMissing
  DiscoveryFailure{}
    -> exitEnvironmentError
  PlanFailure _ e
    -> case e of
         PlanNotFound{}     -> exitEnvironmentError
         PlanParseFailure{} -> exitCacheError
  HoogleOffline{}
    -> exitNetworkError
  HoogleNotFound{}
    -> exitNotFound
  HoogleRemoteError{}
    -> exitCacheError

-- | Envelope-level @actions@ map derived from the error constructor.
-- Most errors carry no command-specific recovery hints; the lookup
-- variants do, and the query / tier list / 'RemoteError' embedded in
-- the constructor are rendered here — never at the call site.
errorActions :: HyphaError -> Map Text Text
errorActions = \case
  HoogleOffline (HoogleQuery q) tiers -> Map.fromList
    [ ("retry_online",    "hypha lookup " <> q)
    , ("query",           q)
    , ("tiers_consulted", renderTierList tiers)
    ]
  HoogleNotFound (HoogleQuery q) tiers -> Map.fromList
    [ ("retry_with_prefix", "hypha lookup " <> q <> "*")
    , ("query",             q)
    , ("tiers_consulted",   renderTierList tiers)
    ]
  HoogleRemoteError (HoogleQuery q) tiers _remoteErr -> Map.fromList
    [ ("retry_offline",
        "hypha lookup " <> q <> " --offline")
    , ("raise_timeout",
        "HYPHA_HOOGLE_TIMEOUT=30 hypha lookup " <> q)
    , ("query",           q)
    , ("tiers_consulted", renderTierList tiers)
    ]
  _ -> Map.empty

-- | 'ExceptT'-friendly wrapper around 'discoverProjectRoot'.
discoverProjectRootE
  :: MonadIO m
  => Maybe FilePath -> ExceptT HyphaError m ProjectRoot
discoverProjectRootE mDir =
  ExceptT (liftIO (first DiscoveryFailure <$> discoverProjectRoot mDir))

-- | 'ExceptT'-friendly wrapper around 'loadBuildPlan'.  Threads the
-- 'ProjectRoot' into 'PlanFailure' so messages can name the directory.
loadBuildPlanE
  :: MonadIO m
  => ProjectRoot -> ExceptT HyphaError m BuildPlan
loadBuildPlanE root =
  ExceptT (liftIO (first (PlanFailure root) <$> loadBuildPlan root))
