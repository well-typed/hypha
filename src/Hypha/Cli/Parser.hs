{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Hypha.Cli.Parser
  ( -- * Types (mostly re-exports)
    module Types
  , Command (..)
  , ClientCommand (..)
  , ServerCommand (..)
  , ClientCommandTag (..)
    -- * Parser
  , parseCli
  ) where

import Options.Applicative

import Hypha.Cli.Types as Types

-- | Parse the CLI arguments.
parseCli :: IO (GlobalFlags, Command)
parseCli = execParser opts
  where
    opts = info (cliParser <**> helper)
      ( fullDesc
      <> progDesc "Agent-first CLI for browsing Hackage and Hoogle"
      <> header "hypha — probe your Haskell build plan"
      )

cliParser :: Parser (GlobalFlags, Command)
cliParser = (,) <$> globalFlagsParser <*> commandParser

globalFlagsParser :: Parser GlobalFlags
globalFlagsParser = GlobalFlags
  <$> optional (strOption
        ( long "project-dir"
       <> metavar "DIR"
       <> help "Override project root"
        ))
  <*> many (strOption
        ( long "package-override"
       <> metavar "PKG=VER"
       <> help "Replace plan entry (repeatable)"
        ))
  <*> switch
        ( long "offline"
       <> help "No network, fail closed"
        )
  <*> switch
        ( long "human"
       <> help "Pretty ANSI text instead of JSON"
        )
  <*> switch
        ( long "pretty-json"
       <> help "Indent JSON output"
        )
  <*> switch
        ( long "full"
       <> help "Include all fields (default is compact)"
        )
  <*> optional (strOption
        ( long "select"
       <> metavar "FIELDS"
       <> help "Post-filter JSON output to listed fields"
        ))
  <*> switch
        ( long "quiet"
       <> short 'q'
       <> help "Suppress informational output"
        )
  <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Show debug output"
        )

commandParser :: Parser Command
commandParser = hsubparser
  ( command "lookup" (info lookupParser
        (progDesc "Tiered symbol resolution: cache → local Hoogle → remote\ne.g. hypha lookup lookup, hypha lookup 'a -> Maybe a'"))
 <> command "package" (info packageParser
        (progDesc "Package metadata (name, version, exposed modules)\ne.g. hypha package async"))
 <> command "module" (info moduleParser
        (progDesc "List module exports\ne.g. hypha module async/Control.Concurrent.Async"))
 <> command "symbol" (info symbolParser
        (progDesc "Symbol signature + haddock\ne.g. hypha symbol async/Control.Concurrent.Async/concurrently\nFormat: PKG/MOD/SYM"))
 <> command "source" (info sourceParser
        (progDesc "Source snippet for a symbol or module\ne.g. hypha source async/Control.Concurrent.Async/concurrently\nFormat: PKG/MOD[/SYM]"))
 <> command "versions" (info versionsParser
        (progDesc "Version history on Hackage\ne.g. hypha versions async"))
 <> command "deps" (info depsParser
        (progDesc "Dependencies of a package\ne.g. hypha deps async\nUse --reverse for reverse dependencies"))
 <> command "doctor" (info doctorParser
        (progDesc "Diagnose the environment"))
 <> command "server" (info serverParser
        (progDesc "Browse docs/source in the browser\ne.g. hypha server --port 4287"))
  )

lookupParser :: Parser Command
lookupParser = fmap ClientCommands $ LookupCommand
  <$> strArgument
        ( metavar "QUERY"
       <> help "Symbol name, qualified name, or type signature" )

packageParser :: Parser Command
packageParser = fmap ClientCommands $ PackageCommand
  <$> strArgument (metavar "PKG[@VER]" <> help "Package identifier")

moduleParser :: Parser Command
moduleParser = fmap ClientCommands $ ModuleCommand
  <$> strArgument (metavar "PKG/MOD" <> help "Module path (pkg/Module.Path)")

symbolParser :: Parser Command
symbolParser = fmap ClientCommands $ SymbolCommand
  <$> strArgument (metavar "PKG/MOD/SYM" <> help "Symbol path (pkg/Module/symbol)")

sourceParser :: Parser Command
sourceParser = fmap ClientCommands $ SourceCommand
  <$> strArgument (metavar "PKG/MOD[/SYM]" <> help "Source path")

versionsParser :: Parser Command
versionsParser = fmap ClientCommands $ VersionsCommand
  <$> strArgument (metavar "PKG" <> help "Package name")

depsParser :: Parser Command
depsParser = fmap ClientCommands $ DepsCommand
  <$> strArgument (metavar "PKG" <> help "Package name")
  <*> switch (long "reverse" <> help "Show reverse dependencies")
  <*> optional (option auto (long "depth" <> metavar "N" <> help "Limit dependency depth"))

doctorParser :: Parser Command
doctorParser = pure (ClientCommands DoctorCommand)

serverParser :: Parser Command
serverParser = fmap ServerCommands $ ServerCommand
  <$> option auto
        ( long "port"
       <> metavar "N"
       <> value 4287
       <> showDefault
       <> help "Port to bind to (default 4287)"
        )
  <*> optional (strOption
        ( long "bind"
       <> metavar "HOST:PORT"
       <> help "Explicit bind address (loopback only)"
        ))
  <*> switch
        ( long "prebuild"
       <> help "Pre-render Haddocks for every package in the build plan"
        )
  <*> option auto
        ( long "prebuild-jobs"
       <> metavar "N"
       <> value 4
       <> showDefault
       <> help "Maximum concurrent prebuild workers"
        )
