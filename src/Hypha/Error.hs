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
    -- * Conversion
  , toOutcomeError
  , errorToOutcomeError
    -- * ExceptT helpers
  , discoverProjectRootE
  , loadBuildPlanE
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Bifunctor (first)
import Data.Text qualified as Text
import Data.Text (Text)

import Hypha.Exit
import Hypha.Output.Outcome (OutcomeError (..))
import Hypha.Project.Discovery (DiscoveryError (..), discoverProjectRoot)
import Hypha.Project.Plan (PlanError (..), loadBuildPlan)
import Hypha.Types.BuildPlan (BuildPlan, ProjectRoot (..))

-- | Umbrella error type produced by hypha.  Every fallible boundary of the
-- CLI funnels through this ADT.  Constructors embed precise sub-errors
-- (e.g. 'DiscoveryError', 'PlanError') so callers can pattern-match
-- without resorting to stringly-typed inspection.
--
-- Each constructor maps to exactly one 'ExitCode'; totality is checked by
-- the unit tests in @test/Unit/Errors.hs@.
data HyphaError
  = UserError         !Text             -- ^ bad CLI args, malformed path, conflicting flags
  | NotFound          !Text             -- ^ symbol/pkg absent from full fallback chain
  | NetworkError      !Text             -- ^ --offline with cache miss, 429, 503, ...
  | Corruption        !Text             -- ^ cache / parse / on-disk corruption
  | EnvError          !Text             -- ^ store unreachable, generic env failure
  | ToolMissing       !Text             -- ^ external binary (haddock/cabal/ghc) absent
  | DiscoveryFailure  !DiscoveryError   -- ^ project root discovery failed
  | PlanFailure       !ProjectRoot !PlanError -- ^ plan.json missing or unparseable
  deriving stock (Show, Eq)

errorCode :: HyphaError -> Text
errorCode = \case
  UserError{}        -> "USER_ERROR"
  NotFound{}         -> "NOT_FOUND"
  NetworkError{}     -> "NETWORK_ERROR"
  Corruption{}       -> "CORRUPTION"
  EnvError{}         -> "ENV_ERROR"
  ToolMissing{}      -> "TOOL_MISSING"
  DiscoveryFailure{} -> "ENV_ERROR"
  PlanFailure _ e -> case e of
    PlanNotFound{} -> "ENV_ERROR"
    PlanParseFailure{} -> "CORRUPTION"

errorMessage :: HyphaError -> Text
errorMessage = \case
  UserError msg
    -> msg
  NotFound msg
    -> msg
  NetworkError msg
    -> msg
  Corruption msg
    -> msg
  EnvError msg
    -> msg
  ToolMissing msg
    -> msg
  DiscoveryFailure (NoProjectFound location)
    -> "no cabal project found (searched up from " <> Text.pack location <> ")"
  PlanFailure (ProjectRoot r) e
    -> case e of
         PlanNotFound pth ->
              "plan.json missing under " <> Text.pack r
           <> " (cabal-plan: " <> Text.pack pth <> ")"
           <> "; run `cabal build --dry-run`"
         PlanParseFailure m -> "plan.json parse failure: " <> Text.pack m

errorExitCode :: HyphaError -> ExitCode
errorExitCode = \case
  UserError         _   -> exitUserError
  NotFound          _   -> exitNotFound
  NetworkError      _   -> exitNetworkError
  Corruption        _   -> exitCacheError
  EnvError          _   -> exitEnvironmentError
  ToolMissing       _   -> exitToolMissing
  DiscoveryFailure  _   -> exitEnvironmentError
  PlanFailure       _ e -> case e of
    PlanNotFound     _ -> exitEnvironmentError
    PlanParseFailure _ -> exitCacheError

-- | Convert a 'HyphaError' into the wire-format 'OutcomeError' triple.
-- Kept for legacy callers; new code should prefer 'errorToOutcomeError'.
toOutcomeError :: HyphaError -> (Text, Text, Int)
toOutcomeError e = (errorCode e, errorMessage e, unExitCode (errorExitCode e))

-- | Convert a 'HyphaError' into a structured 'OutcomeError'.
errorToOutcomeError :: HyphaError -> OutcomeError
errorToOutcomeError e = OutcomeError
  (errorCode e)
  (errorMessage e)
  (unExitCode (errorExitCode e))

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
