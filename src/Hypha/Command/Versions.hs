{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Versions
  ( -- * Field sets
    compactKeys
  , fullKeys
    -- * Execution
  , runVersions
  , runVersionsWithAvail
  ) where

import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Hypha.Error (HyphaError (..), NotFoundReason (..))
import Hypha.Output.Outcome (Outcome (..), Related (..))
import Hypha.Types.BuildPlan (BuildPlan, lookupPackage)
import Hypha.Types.PackageId (PackageName (..), Version (..))

compactKeys, fullKeys :: Set Text
compactKeys = Set.fromList ["package", "pinned_version", "available_versions"]
fullKeys    = compactKeys

-- | Execute the @versions@ command.
--
-- Returns the pinned version from the build plan.  The @available_versions@
-- field is intentionally empty until the Hackage API client is integrated
-- (issue 006 covers the client; wiring is a separate task post-alpha).
runVersions :: BuildPlan -> PackageName -> Either HyphaError (Outcome Value)
runVersions plan pkgName =
  case lookupPackage pkgName plan of
    Nothing -> Left (NotFound (NotFoundPackageInPlan pkgName))
    Just ver -> Right (mkSuccessOutcome pkgName ver)

mkSuccessOutcome :: PackageName -> Version -> Outcome Value
mkSuccessOutcome (PackageName name) (Version ver) =
  Outcome body False [] actions related
  where
    body = Aeson.object
      [ "package"            .= name
      , "pinned_version"     .= ver
      , "available_versions" .= ([] :: [Text])
      ]
    actions = Map.fromList
      [ ("package_info", "hypha package " <> name)
      , ("reverse_deps", "hypha deps " <> name <> " --reverse")
      ]
    related =
      [ Related "package" ("hypha package " <> name) ]

-- | Run the @versions@ command with available versions from Hackage.
runVersionsWithAvail :: BuildPlan -> PackageName -> [Version] -> Outcome Value
runVersionsWithAvail plan pkgName available =
  case lookupPackage pkgName plan of
    Nothing ->
      let name = unPackageName pkgName
          body = Aeson.object
            [ "package"            .= name
            , "pinned_version"     .= ("" :: Text)
            , "available_versions" .= map unVersion available
            ]
      in Outcome body True [] mempty []
    Just ver ->
      let (PackageName name) = pkgName
          (Version v) = ver
          body = Aeson.object
            [ "package"            .= name
            , "pinned_version"     .= v
            , "available_versions" .= map unVersion available
            ]
          actions = Map.fromList
            [ ("package_info", "hypha package " <> name)
            , ("reverse_deps", "hypha deps " <> name <> " --reverse")
            ]
          related =
            [ Related "package" ("hypha package " <> name) ]
      in Outcome body False [] actions related
