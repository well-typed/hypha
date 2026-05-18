{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Versions
  ( -- * Execution
    runVersions
  ) where

import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)

import Hypha.Error (HyphaError (..))
import Hypha.Output.Outcome (Outcome, successOutcome)
import Hypha.Types.BuildPlan (BuildPlan (..), lookupPackage)
import Hypha.Types.PackageId (PackageName (..), Version (..))

-- | Execute the versions command.
--
--   Returns the pinned version from the build plan.
--   Available versions from Hackage are deferred to a later task.
runVersions :: BuildPlan -> PackageName -> Either HyphaError (Outcome Value)
runVersions plan pkgName =
  case lookupPackage pkgName plan of
    Nothing -> Left $ NotFound $ "Package '" <> unPackageName pkgName <> "' not in build plan"
    Just ver -> Right $ successOutcome (versionsResultToJSON pkgName ver)

-- | Convert the versions result to JSON.
versionsResultToJSON :: PackageName -> Version -> Value
versionsResultToJSON (PackageName name) (Version ver) = Aeson.object
  [ "package"          .= name
  , "pinned_version"   .= ver
  , "available_versions" .= ([] :: [Text])  -- TODO: fetch from Hackage in later task
  ]
