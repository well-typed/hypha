{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Cli.Run
  ( -- * Execution
    runCli
    -- * Helpers
  , withPlan
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (hFlush, stdout)
import qualified System.Exit as System
import qualified Data.Aeson as Aeson

import Hypha.Cli.Parser (GlobalFlags (..), Command (..))
import Hypha.Command.Package (runPackage)
import Hypha.Command.Search (runSearch)
import Hypha.Command.Versions (runVersions)
import Hypha.Error (HyphaError (..), errorExitCode, errorMessage, errorCode, ExitCode (..))
import Hypha.Logging (LogEvent (..), silentTracer, verboseTracer)
import Hypha.Output.Outcome (Outcome (..), OutcomeError (..), failureOutcome)
import Hypha.Output.Json (encodeEnvelope)
import Hypha.Types.BuildPlan (BuildPlan (..), emptyBuildPlan)
import Hypha.Types.PackageId (PackageName (..))

-- | Run the CLI with the given flags and command.
runCli :: GlobalFlags -> Command -> IO ()
runCli flags cmd = do
  let tracer = if gfVerbose flags then verboseTracer else silentTracer

  tracer (LogInfo "Starting hypha")

  -- For now, use emptyBuildPlan. In the future, this will load from project.
  let plan = emptyBuildPlan

  result <- case cmd of
    SearchCommand query extras -> do
      tracer (LogDebug $ "Search: " <> query)
      pure $ runSearch plan query extras
    PackageCommand pkgName -> do
      tracer (LogDebug $ "Package: " <> pkgName)
      pure $ runPackage plan pkgName
    VersionsCommand pkg -> do
      tracer (LogDebug $ "Versions: " <> pkg)
      pure $ runVersions plan (PackageName pkg)
    _ -> pure $ Left $ UserError "Command not yet implemented"

  case result of
    Right outcome -> do
      emitOutcome flags (commandName cmd) outcome
      System.exitSuccess
    Left err -> do
      let errObj = OutcomeError (errorCode err) (errorMessage err) (unExitCode (errorExitCode err))
          outcome = failureOutcome errObj :: Outcome Aeson.Value
      emitOutcome flags (commandName cmd) outcome
      System.exitWith (toSystemExitCode (errorExitCode err))

-- | Helper that runs an action with the build plan.
--   Abstracts plan-root discovery and error handling.
--
--   In the future, this will:
--   1. Discover project root
--   2. Load build plan from plan.json
--   3. Apply overrides
--   4. Pass the plan to the action
withPlan :: (BuildPlan -> Either HyphaError (Outcome Aeson.Value)) -> Either HyphaError (Outcome Aeson.Value)
withPlan action = action emptyBuildPlan

-- | Emit an outcome to stdout as JSON.
emitOutcome :: GlobalFlags -> Text -> Outcome Aeson.Value -> IO ()
emitOutcome _flags cmdName outcome = do
  let envelope = encodeEnvelope cmdName outcome
  LBS.hPut stdout (Aeson.encode envelope)
  hFlush stdout

-- | Convert our ExitCode to System.Exit.ExitCode.
toSystemExitCode :: ExitCode -> System.ExitCode
toSystemExitCode (ExitCode 0) = System.ExitSuccess
toSystemExitCode (ExitCode n) = System.ExitFailure n

-- | Get the command name for the envelope.
commandName :: Command -> Text
commandName (SearchCommand _ _)     = "search"
commandName (PackageCommand _)      = "package"
commandName (ModuleCommand _)       = "module"
commandName (SymbolCommand _)       = "symbol"
commandName (SourceCommand _)       = "source"
commandName (VersionsCommand _)     = "versions"
commandName (DepsCommand _ _ _)     = "deps"
commandName (WhatProvidesCommand _) = "whatprovides"
commandName DoctorCommand           = "doctor"
