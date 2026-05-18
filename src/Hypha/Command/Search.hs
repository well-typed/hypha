{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Search
  ( -- * Types
    SearchResult (..)
  , SearchHit (..)
    -- * Execution
  , runSearch
  ) where

import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)

import Hypha.Types.BuildPlan (BuildPlan (..), PackageOverride (..))
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Error (HyphaError (..))
import Hypha.Output (OutcomeEnvelope, successEnvelope)

-- | Result of a search command.
data SearchResult = SearchResult
  { srQuery :: !Text
    -- ^ The original search query.
  , srHits  :: ![SearchHit]
    -- ^ The search results.
  }
  deriving stock (Show)

-- | A single search hit.
data SearchHit = SearchHit
  { shName      :: !Text
    -- ^ Symbol name.
  , shModule    :: !Text
    -- ^ Module path.
  , shPackage   :: !Text
    -- ^ Package name.
  , shVersion   :: !Text
    -- ^ Package version.
  , shSignature :: !(Maybe Text)
    -- ^ Type signature (if available).
  }
  deriving stock (Show)

-- | Execute the search command.
--
--   For now, this returns a stub result. Hoogle integration will be added later.
runSearch :: BuildPlan -> Text -> [Text] -> Either HyphaError OutcomeEnvelope
runSearch plan query _extraPkgs =
  let result = SearchResult
        { srQuery = query
        , srHits  = stubHits query
      }
  in Right $ successEnvelope "search" (map showOverride (bpOverrides plan)) (searchResultToJSON result)

-- | Generate stub search hits for demonstration.
stubHits :: Text -> [SearchHit]
stubHits _query =
  [ SearchHit
      { shName      = "map"
      , shModule    = "Data.Map.Strict"
      , shPackage   = "containers"
      , shVersion   = "0.6.7"
      , shSignature = Just "(a -> b) -> Map k a -> Map k b"
      }
  , SearchHit
      { shName      = "insert"
      , shModule    = "Data.Map.Strict"
      , shPackage   = "containers"
      , shVersion   = "0.6.7"
      , shSignature = Just "Ord k => k -> a -> Map k a -> Map k a"
      }
  ]

-- | Convert a search result to JSON.
searchResultToJSON :: SearchResult -> Value
searchResultToJSON (SearchResult query hits) = Aeson.object
  [ "query" .= query
  , "hits"  .= map hitToJSON hits
  ]

-- | Convert a search hit to JSON.
hitToJSON :: SearchHit -> Value
hitToJSON hit = Aeson.object $ concat
  [ [ "name"      .= shName hit
    , "module"    .= shModule hit
    , "package"   .= shPackage hit
    , "version"   .= shVersion hit
    ]
  , maybe [] (\s -> ["signature" .= s]) (shSignature hit)
  ]

-- Helper to show an override as text
showOverride :: PackageOverride -> Text
showOverride (PackageOverride (PackageName name) (Version ver)) = name <> "=" <> ver
