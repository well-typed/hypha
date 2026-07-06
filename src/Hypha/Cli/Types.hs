{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Hypha.Cli.Types
  (
    HyphaOptions (..)
  , Command (..)
  , ClientCommand (..)
  , ServerCommand (..)
  , ClientCommandTag (..)
  , clientCommandTag
  , clientCommandName
  ) where

import Data.Text (Text)
import Data.Text qualified as T


-- | Options that apply to all commands.
data HyphaOptions = HyphaOptions
  { hoProjectDir      :: !(Maybe FilePath)
    -- ^ Override project root.
  , hoPackageOverrides :: ![Text]
    -- ^ @PKG=VER@ overrides (repeatable).
  , hoOffline           :: !Bool
    -- ^ No network, fail closed.  Honored by 'LookupCommand' (skips
    -- the remote Hoogle tier).
  , hoHuman             :: !Bool
    -- ^ Pretty ANSI text instead of JSON.
  , hoPrettyJson        :: !Bool
    -- ^ Indent JSON output.
  , hoFull              :: !Bool
    -- ^ Include all fields (default is compact).
  , hoSelect            :: !(Maybe Text)
    -- ^ Post-filter JSON output to listed fields.
  , hoQuiet             :: !Bool
    -- ^ Suppress informational output.
  , hoVerbose           :: !Bool
    -- ^ Show debug output.
  }
  deriving stock (Show, Eq)

data ClientCommandTag =
    LookupCmd
  | PackageCmd
  | ModuleCmd
  | SymbolCmd
  | SourceCmd
  | VersionsCmd
  | DepsCmd
  | DoctorCmd
  deriving stock (Show, Eq, Ord)

clientCommandName :: ClientCommandTag -> T.Text
clientCommandName = \case
  LookupCmd   -> "lookup"
  PackageCmd  -> "package"
  ModuleCmd   -> "module"
  SymbolCmd   -> "symbol"
  SourceCmd   -> "source"
  VersionsCmd -> "versions"
  DepsCmd     -> "deps"
  DoctorCmd   -> "doctor"

clientCommandTag :: ClientCommand -> ClientCommandTag
clientCommandTag = \case
  LookupCommand{}   -> LookupCmd
  PackageCommand{}  -> PackageCmd
  ModuleCommand{}   -> ModuleCmd
  SymbolCommand{}   -> SymbolCmd
  SourceCommand{}   -> SourceCmd
  VersionsCommand{} -> VersionsCmd
  DepsCommand{}     -> DepsCmd
  DoctorCommand{}   -> DoctorCmd

data Command
  = ServerCommands ServerCommand
  | ClientCommands ClientCommand
  deriving stock (Show, Eq, Ord)

data ServerCommand
  = ServerCommand !Int !(Maybe Text) !Bool !Int
  -- ^ @server [--port N] [--bind HOST:PORT] [--prebuild] [--prebuild-jobs N]@
  deriving stock (Show, Eq, Ord)

-- | Subcommands.
data ClientCommand
  = LookupCommand !Text
    -- ^ @lookup QUERY@ — tiered symbol resolution.
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
  | DoctorCommand
    -- ^ @doctor@
  deriving stock (Show, Eq, Ord)

