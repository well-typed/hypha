{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Cli.Run
  ( -- * Execution
    runCli
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)

import Hypha.Cli.Parser (GlobalFlags (..), Command (..))
import Hypha.Command.Search (runSearch)
import Hypha.Error (HyphaError (..), errorToExitCode)
import Hypha.Exit (ExitCode (..), toSystemExitCode)
import Hypha.Logging (LogEvent (..), silentTracer, verboseTracer)
import Hypha.Output (OutcomeEnvelope, encodeEnvelope, errorEnvelope)
import Hypha.Types.BuildPlan (BuildPlan (..), emptyBuildPlan)
import qualified System.Exit as System

-- | Run the CLI with the given flags and command.
runCli :: GlobalFlags -> Command -> IO ()
runCli flags cmd = do
  let tracer = if gfVerbose flags then verboseTracer else silentTracer
      plan = emptyBuildPlan  -- TODO: Load from project

  tracer (LogInfo "Starting hypha")

  result <- case cmd of
    SearchCommand query extras -> do
      tracer (LogDebug $ "Search: " <> query)
      pure $ runSearch plan query extras
    _ -> pure $ Left $ CliError "Command not yet implemented"

  case result of
    Right envelope -> do
      emitEnvelope flags envelope
      System.exitSuccess
    Left err -> do
      let envelope = errorEnvelope (commandName cmd) [] err
      emitEnvelope flags envelope
      System.exitWith (toSystemExitCode (errorToExitCode err))

-- | Emit an envelope to stdout.
emitEnvelope :: GlobalFlags -> OutcomeEnvelope -> IO ()
emitEnvelope _flags envelope = do
  LBS.hPut stdout (encodeEnvelope envelope)
  hFlush stdout

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
