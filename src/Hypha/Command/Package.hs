{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Package
  ( PackageResult (..)
    -- * Field sets
  , compactKeys
  , fullKeys
    -- * Execution
  , runPackage
  , runPackagePure
  , mkSuccessOutcome
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
import Hypha.Types.BuildPlan (BuildPlan, PlannedUnit (..), lookupUnit)
import Hypha.Types.PackageId (PackageName (..), PackageId (..), Version (..))

-- | Metadata for a single package, as returned by the @package@ command.
data PackageResult = PackageResult
  { prName       :: !Text
  , prVersion    :: !Text
  , prInPlan     :: !Bool
  , prIsLocal    :: !Bool
  , prDepsCount  :: !Int
  }
  deriving stock (Show, Eq)

compactKeys, fullKeys :: Set Text
compactKeys = Set.fromList ["name", "version", "in_plan", "is_local", "deps_count"]
fullKeys    = compactKeys

-- | Plan-only variant of the @package@ command, used by golden tests for the
-- in-plan path.  Production dispatch in "Hypha.Cli.Run" goes through the
-- 'Hypha.Package.Resolver.PackageResolver' which performs the full fallback
-- chain (plan → store → Hackage).
runPackage :: BuildPlan -> Text -> Either HyphaError (Outcome Value)
runPackage plan rawArg =
  let (rawName, _mVerHint) = splitVersionHint rawArg
      pkgName              = PackageName rawName
  in case lookupUnit pkgName plan of
       Just pu -> Right (mkSuccessOutcome rawName (pkgVersion (puId pu)) (puIsLocal pu) (length (puDeps pu)))
       Nothing -> Left $ NotFound
         ("package '" <> rawName <> "' not in build plan")

-- | Total variant: never fails at the IO boundary, but encodes "not in plan"
-- as a structured 'OutcomeFailure' so the envelope shape stays consistent.
runPackagePure :: BuildPlan -> Text -> Outcome Value
runPackagePure plan rawArg =
  case runPackage plan rawArg of
    Right o  -> o
    Left err -> failureOutcome $ OutcomeError
      "NOT_FOUND"
      (case err of NotFound m -> m; _ -> Text.pack (show err))
      3

splitVersionHint :: Text -> (Text, Maybe Text)
splitVersionHint raw =
  case Text.splitOn "@" raw of
    [n]    -> (n, Nothing)
    [n, v] -> (n, Just v)
    (n:_)  -> (n, Nothing)
    []     -> ("", Nothing)

mkSuccessOutcome :: Text -> Version -> Bool -> Int -> Outcome Value
mkSuccessOutcome rawName ver isLocal depsCount =
  let result = PackageResult
        { prName      = rawName
        , prVersion   = unVersion ver
        , prInPlan    = True
        , prIsLocal   = isLocal
        , prDepsCount = depsCount
        }
      body = packageResultToJSON result
      actions = Map.fromList
        [ ("version_history", "hypha versions " <> rawName)
        , ("reverse_deps",    "hypha deps " <> rawName <> " --reverse")
        ]
  in OutcomeSuccess body False [] actions
       [ Related "versions" ("hypha versions " <> rawName)
       , Related "module_index_hint" ("hypha module " <> rawName <> "/<Module>")
       ]

packageResultToJSON :: PackageResult -> Value
packageResultToJSON r = Aeson.object
  [ "name"       .= prName r
  , "version"    .= prVersion r
  , "in_plan"    .= prInPlan r
  , "is_local"   .= prIsLocal r
  , "deps_count" .= prDepsCount r
  ]
