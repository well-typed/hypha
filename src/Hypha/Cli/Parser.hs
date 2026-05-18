{-# LANGUAGE DerivingStrategies #-}
module Hypha.Cli.Parser
  ( -- * Types
    GlobalFlags (..)
  , Command (..)
    -- * Parser
  , parseCli
  ) where

import Data.Text (Text)
import Options.Applicative

-- | Global flags that apply to all commands.
data GlobalFlags = GlobalFlags
  { gfProjectDir      :: !(Maybe FilePath)
    -- ^ Override project root.
  , gfPackageOverrides :: ![Text]
    -- ^ @PKG=VER@ overrides (repeatable).
  , gfGlobal            :: !Bool
    -- ^ Widen Hoogle to global stackage DB.
  , gfOffline           :: !Bool
    -- ^ No network, fail closed.
  , gfHuman             :: !Bool
    -- ^ Pretty ANSI text instead of JSON.
  , gfPrettyJson        :: !Bool
    -- ^ Indent JSON output.
  , gfFull              :: !Bool
    -- ^ Include all fields (default is compact).
  , gfSelect            :: !(Maybe Text)
    -- ^ Post-filter JSON output to listed fields.
  , gfQuiet             :: !Bool
    -- ^ Suppress informational output.
  , gfVerbose           :: !Bool
    -- ^ Show debug output.
  }
  deriving stock (Show, Eq)

-- | Subcommands.
data Command
  = SearchCommand !Text ![Text]
    -- ^ @search QUERY [+pkg ...]@
  | PackageCommand !Text
    -- ^ @package <pkg>[@ver]@
  | ModuleCommand !Text
    -- ^ @module <pkg>/<Mod>@
  | SymbolCommand !Text
    -- ^ @symbol <pkg>/<Mod>/<sym>@
  | SourceCommand !Text
    -- ^ @source <pkg>/<Mod>/<sym>@ or @<pkg>/<Mod>@
  | VersionsCommand !Text
    -- ^ @versions <pkg>@
  | DepsCommand !Text !Bool !(Maybe Int)
    -- ^ @deps <pkg> [--reverse] [--depth N]@
  | WhatProvidesCommand !Text
    -- ^ @whatprovides <symbol>@
  | DoctorCommand
    -- ^ @doctor@
  | ServerCommand !Int !(Maybe Text) !Bool !Int
    -- ^ @server [--port N] [--bind HOST:PORT] [--prebuild] [--prebuild-jobs N]@
  deriving stock (Show, Eq)

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
        ( long "global"
       <> help "Widen Hoogle to global stackage DB"
        )
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
  ( command "search" (info searchParser (progDesc "Hoogle search scoped to plan"))
 <> command "package" (info packageParser (progDesc "Package metadata"))
 <> command "module" (info moduleParser (progDesc "Module exports"))
 <> command "symbol" (info symbolParser (progDesc "Symbol details"))
 <> command "source" (info sourceParser (progDesc "Source code"))
 <> command "versions" (info versionsParser (progDesc "Version history"))
 <> command "deps" (info depsParser (progDesc "Dependencies"))
 <> command "whatprovides" (info whatProvidesParser (progDesc "Find symbol providers"))
 <> command "doctor" (info doctorParser (progDesc "Environment health check"))
 <> command "server" (info serverParser (progDesc "Local Haddock/source browser"))
  )

searchParser :: Parser Command
searchParser = SearchCommand
  <$> strArgument (metavar "QUERY" <> help "Search query")
  <*> many (strArgument (metavar "+PKG" <> help "Additional packages to search"))

packageParser :: Parser Command
packageParser = PackageCommand
  <$> strArgument (metavar "PKG[@VER]" <> help "Package identifier")

moduleParser :: Parser Command
moduleParser = ModuleCommand
  <$> strArgument (metavar "PKG/MOD" <> help "Module path (pkg/Module.Path)")

symbolParser :: Parser Command
symbolParser = SymbolCommand
  <$> strArgument (metavar "PKG/MOD/SYM" <> help "Symbol path (pkg/Module/symbol)")

sourceParser :: Parser Command
sourceParser = SourceCommand
  <$> strArgument (metavar "PKG/MOD[/SYM]" <> help "Source path")

versionsParser :: Parser Command
versionsParser = VersionsCommand
  <$> strArgument (metavar "PKG" <> help "Package name")

depsParser :: Parser Command
depsParser = DepsCommand
  <$> strArgument (metavar "PKG" <> help "Package name")
  <*> switch (long "reverse" <> help "Show reverse dependencies")
  <*> optional (option auto (long "depth" <> metavar "N" <> help "Limit dependency depth"))

whatProvidesParser :: Parser Command
whatProvidesParser = WhatProvidesCommand
  <$> strArgument (metavar "SYM" <> help "Symbol name")

doctorParser :: Parser Command
doctorParser = pure DoctorCommand

serverParser :: Parser Command
serverParser = ServerCommand
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
