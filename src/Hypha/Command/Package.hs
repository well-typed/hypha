{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Package
  ( PackageResult (..)
  , runPackage
  ) where

import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Error (HyphaError (..))
import Hypha.Output.Outcome (Outcome, successOutcome)
import Hypha.Types.BuildPlan (BuildPlan (..), lookupPackage)
import Hypha.Types.PackageId (PackageName (..), Version (..))

-- | Metadata for a single package, as returned by the @package@ command.
data PackageResult = PackageResult
  { prName       :: !Text
  , prVersion    :: !Text
  , prInPlan     :: !Bool
  , prIsLocal    :: !Bool
    -- ^ MVP placeholder; true only if the package is a local / inplace build.
  , prDepsCount  :: !Int
    -- ^ MVP placeholder; dependency counting deferred to Phase 10.
  }
  deriving stock (Show)

-- | Execute the @package@ command.
--
--   If the package is not present in the build plan we return a 'NotFound'
--   error (the caller should widen with @--any@ if desired).
runPackage :: BuildPlan -> Text -> Either HyphaError (Outcome Value)
runPackage plan rawName =
  let pkgName = PackageName rawName
      mVer    = lookupPackage pkgName plan
  in case mVer of
       Just ver ->
         let result = PackageResult
               { prName      = rawName
               , prVersion   = unVersion ver
               , prInPlan    = True
               , prIsLocal   = False
               , prDepsCount = 0
               }
         in Right $ successOutcome (packageResultToJSON result)
       Nothing ->
         Left $ NotFound
           ( Text.pack "package '" <> rawName <> Text.pack "' not in build plan (use --any to widen)" )

-- | Convert a 'PackageResult' to a JSON 'Value'.
packageResultToJSON :: PackageResult -> Value
packageResultToJSON r = Aeson.object
  [ "name"       .= prName r
  , "version"    .= prVersion r
  , "in_plan"    .= prInPlan r
  , "is_local"   .= prIsLocal r
  , "deps_count" .= prDepsCount r
  ]
