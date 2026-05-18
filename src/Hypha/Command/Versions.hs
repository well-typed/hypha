{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Versions
  ( -- * Field sets
    compactKeys
  , fullKeys
    -- * Execution
  , runVersions
  , runVersionsPure
  , runVersionsWithAvail
  ) where

import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Error (HyphaError (..))
import Hypha.Output.Outcome
  ( Outcome (..), Related (..), OutcomeError (..)
  , failureOutcome
  )
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
    Nothing -> Left $ NotFound
      ("Package '" <> unPackageName pkgName <> "' not in build plan")
    Just ver -> Right (mkSuccessOutcome pkgName ver)

-- | Total variant: produces a failure 'Outcome' on miss rather than an
-- 'Either'.  Used by the CLI dispatcher.
runVersionsPure :: BuildPlan -> PackageName -> Outcome Value
runVersionsPure plan pkgName =
  case runVersions plan pkgName of
    Right o  -> o
    Left err -> failureOutcome $ OutcomeError
      "NOT_FOUND"
      (case err of NotFound m -> m; _ -> Text.pack (show err))
      3

mkSuccessOutcome :: PackageName -> Version -> Outcome Value
mkSuccessOutcome (PackageName name) (Version ver) =
  OutcomeSuccess body False [] actions related
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
      in OutcomeSuccess body True [] mempty []
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
      in OutcomeSuccess body False [] actions related
