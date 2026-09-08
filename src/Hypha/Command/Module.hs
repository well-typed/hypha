{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Module
  ( runModule
  , runModuleFromDir
  , mkPid
  , compactKeys
  , fullKeys
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text (Text)
import Hypha.Encoding (readSourceFile)
import Hypha.BuildEnv.Type   (BuildEnv)
import Hypha.Cli.Types
import Hypha.Output.Outcome  (Outcome (..))
import Hypha.Source.Extensions (defaultLanguageSettings)
import Hypha.Source.Locate   (exportedNamesOf, findModuleFile, listExportedSymbols)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version)

compactKeys, fullKeys :: Set Text
compactKeys = Set.fromList ["package", "module", "exports"]
fullKeys    = compactKeys

-- | Build a 'PackageId' from a name and version produced by the build plan.
mkPid :: Text -> Version -> PackageId
mkPid name ver = PackageId (PackageName name) ver

-- | Run the @module@ command: list exported symbols for a given package module.
runModule :: BuildEnv IO -> PackageId -> Text -> IO (Outcome Value)
runModule env pid modPath = do
  exps <- listExportedSymbols env pid modPath
  pure (toOutcome pid modPath exps)

-- | Variant taking a pre-resolved source directory (resolver-driven).
runModuleFromDir :: FilePath -> PackageId -> Text -> IO (Outcome Value)
runModuleFromDir srcDir pid modPath = do
  mFile <- findModuleFile srcDir modPath
  exps <- case mFile of
    Nothing -> pure []
    Just f  -> exportedNamesOf defaultLanguageSettings f =<< readSourceFile f
  pure (toOutcome pid modPath exps)

toOutcome :: PackageId -> Text -> [Text] -> Outcome Value
toOutcome pid modPath exps =
  let pkg = unPackageName (pkgName pid)
  in Outcome
    (object
      [ "package" .= pkg
      , "module"  .= modPath
      , "exports" .= map (\nm -> object ["name" .= nm]) exps
      ])
    ModuleCmd
    False
    []
    (Map.fromList $
      [ ("module_index", "hypha module " <> pkg <> "/" <> modPath)
      , ("package_info", "hypha package " <> pkg)
      ]
      <> [ (nm, "hypha symbol " <> pkg <> "/" <> modPath <> "/" <> nm)
         | nm <- take 5 exps
         ])
