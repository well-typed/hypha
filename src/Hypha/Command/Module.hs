{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Module
  ( runModule
  , mkPid
  , compactKeys
  , fullKeys
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Hypha.BuildEnv.Type   (BuildEnv)
import Hypha.Output.Outcome  (Outcome (..), Related (..))
import Hypha.Source.Locate   (listExportedSymbols)
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
  let pkg = unPackageName (pkgName pid)
  pure $ OutcomeSuccess
    (object
      [ "package" .= pkg
      , "module"  .= modPath
      , "exports" .= map (\nm -> object ["name" .= nm]) exps
      ])
    False
    []
    (Map.fromList
      [ ("module_index", "hypha module " <> pkg <> "/" <> modPath)
      , ("package_info", "hypha package " <> pkg)
      ])
    [ Related nm ("hypha symbol " <> pkg <> "/" <> modPath <> "/" <> nm)
    | nm <- take 5 exps
    ]
