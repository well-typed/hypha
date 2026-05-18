{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Module
  ( runModule
  , compactKeys
  , fullKeys
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.BuildEnv.Type   (BuildEnv (..))
import Hypha.Output.Outcome  (Outcome (..), Related (..))
import Hypha.Source.Locate   (listExportedSymbols)
import Hypha.Types.PackageId (PackageId (..), PackageName (..))

compactKeys, fullKeys :: Set.Set Text
compactKeys = Set.fromList ["package", "module", "exports"]
fullKeys    = compactKeys

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
    False  -- not outside plan
    []     -- no overrides
    (Map.fromList
      [ ("module_index", "hypha module " <> pkg <> "/" <> modPath)
      , ("package_info", "hypha package " <> pkg)
      ])
    [ Related nm ("hypha symbol " <> pkg <> "/" <> modPath <> "/" <> nm)
    | nm <- take 5 exps
    ]
