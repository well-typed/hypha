{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Package
  ( PackageResult (..)
    -- * Field sets
  , compactKeys
  , fullKeys
    -- * Execution
  , runPackage
  , mkSuccessOutcome
    -- * JSON helpers
  , packageOriginToJSON
  ) where

import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Error (HyphaError (..), NotFoundReason (..))
import Hypha.Output.Outcome (Outcome (..))
import Hypha.Types.BuildPlan
  ( BuildPlan, PackageOrigin (..), PlannedUnit (..), lookupUnit )
import Hypha.Types.PackageId
  ( PackageName (..), PackageId (..), PackageRef (..), Version (..)
  , parsePackageRef )

-- | Metadata for a single package, as returned by the @package@ command.
data PackageResult = PackageResult
  { prName           :: !Text
  , prVersion        :: !Text
  , prInPlan         :: !Bool
  , prIsLocal        :: !Bool
  , prDepsCount      :: !Int
  , prExposedModules :: ![Text]
    -- ^ Exposed modules parsed from the package's .cabal file, or empty if
    -- not yet resolved / source unavailable.
  , prOrigin         :: !PackageOrigin
    -- ^ Provenance of the package source.  Defaults to 'OriginHackage'
    -- for out-of-plan resolutions.
  }
  deriving stock (Show, Eq)

compactKeys, fullKeys :: Set Text
compactKeys = Set.fromList
  [ "name", "version", "in_plan", "is_local", "deps_count"
  , "exposed_modules", "origin" ]
fullKeys    = compactKeys

-- | Plan-only variant of the @package@ command, used by golden tests for the
-- in-plan path.  Production dispatch in "Hypha.Cli.Run" goes through the
-- 'Hypha.Package.Resolver.PackageResolver' which performs the full fallback
-- chain (plan → store → Hackage).
runPackage :: BuildPlan -> Text -> Either HyphaError (Outcome Value)
runPackage plan rawArg =
  let PackageRef pkg _ = parsePackageRef rawArg
      nameT            = unPackageName pkg
  in case lookupUnit pkg plan of
       Just pu -> Right (mkSuccessOutcome nameT
         (pkgVersion (puId pu)) (puIsLocal pu) (length (puDeps pu))
         (puOrigin pu) [])
       Nothing -> Left (NotFound (NotFoundPackageInPlan pkg))

-- | Build a success outcome from package metadata and a (possibly empty)
-- list of exposed modules.  When modules are provided, per-module related
-- actions are included so an agent can drill in immediately.
mkSuccessOutcome
  :: Text
  -> Version
  -> Bool
  -> Int
  -> PackageOrigin
  -> [Text]
  -> Outcome Value
mkSuccessOutcome rawName ver isLocal depsCount origin modules =
  let result = PackageResult
        { prName           = rawName
        , prVersion        = unVersion ver
        , prInPlan         = True
        , prIsLocal        = isLocal
        , prDepsCount      = depsCount
        , prExposedModules = modules
        , prOrigin         = origin
        }
      body = packageResultToJSON result
      -- Single hint channel.  "version_history" supersedes the former
      -- duplicate "versions" related entry; "module_index_hint" is the
      -- placeholder pointer to module-level lookups.
      actions = Map.fromList $
        [ ("version_history",    "hypha versions " <> rawName)
        , ("reverse_deps",       "hypha deps " <> rawName <> " --reverse")
        , ("module_index_hint",  "hypha module " <> rawName <> "/<Module>")
        ]
        <> [ (nm, "hypha module " <> rawName <> "/" <> nm) | nm <- modules ]
  in Outcome body False [] actions

packageResultToJSON :: PackageResult -> Value
packageResultToJSON r = Aeson.object
  [ "name"           .= prName r
  , "version"        .= prVersion r
  , "in_plan"        .= prInPlan r
  , "is_local"       .= prIsLocal r
  , "deps_count"     .= prDepsCount r
  , "exposed_modules" .= prExposedModules r
  , "origin"         .= packageOriginToJSON (prOrigin r)
  ]

-- | Tagged JSON for 'PackageOrigin'.  The @kind@ discriminator is
-- stable; additional metadata fields are present only when the variant
-- carries them.  Keep this in sync with any downstream JSON consumer
-- (agents, MCP).
packageOriginToJSON :: PackageOrigin -> Value
packageOriginToJSON = \case
  OriginHackage          -> Aeson.object [ "kind" .= ("hackage" :: Text) ]
  OriginDistribution     -> Aeson.object [ "kind" .= ("distribution" :: Text) ]
  OriginLocal p          -> Aeson.object
    [ "kind" .= ("local" :: Text), "path" .= Text.pack p ]
  OriginLocalTarball p   -> Aeson.object
    [ "kind" .= ("local-tarball" :: Text), "path" .= Text.pack p ]
  OriginRemoteTarball u  -> Aeson.object
    [ "kind" .= ("remote-tarball" :: Text), "url" .= u ]
  OriginSourceRepo url ref subdir -> Aeson.object
    [ "kind"   .= ("source-repository-package" :: Text)
    , "url"    .= url
    , "ref"    .= ref
    , "subdir" .= fmap Text.pack subdir
    ]
