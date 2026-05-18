{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Search
  ( -- * Types
    SearchResult (..)
  , SearchHit (..)
    -- * Field sets
  , compactKeys
  , fullKeys
    -- * Execution
  , runSearch
  , runSearchWith
    -- * Internal (for testing)
  , hitToJSON
  , searchResultToJSON
  ) where

import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Error (HyphaError (..))
import Hypha.Hoogle.Type (Hoogle (..), HoogleHit (..), HoogleQuery (..))
import Hypha.Output.Outcome
  ( Outcome (..), OutcomeError (..), Related (..)
  , failureOutcome
  )
import Hypha.Types.BuildPlan (BuildPlan)

-- | Result of a search command.
data SearchResult = SearchResult
  { srQuery :: !Text
  , srHits  :: ![SearchHit]
  }
  deriving stock (Show, Eq)

-- | A single search hit.
data SearchHit = SearchHit
  { shName      :: !Text
  , shModule    :: !Text
  , shPackage   :: !Text
  , shVersion   :: !Text
  , shSignature :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

-- | Compact result keys: the JSON object emitted under @result@ when
-- @--full@ is not set.
compactKeys :: Set Text
compactKeys = Set.fromList ["query", "hits"]

-- | Full result keys.  Equal to compact for alpha; reserved for later
-- enrichment (e.g. @total@, @sources@).
fullKeys :: Set Text
fullKeys = compactKeys

-- | Pure variant used only when no Hoogle DB is available.  Returns a
-- failure outcome explaining that Hoogle is required.
--
-- Real call-sites should use 'runSearchWith' threaded with a 'Hoogle IO'.
runSearch :: BuildPlan -> Text -> [Text] -> Either HyphaError (Outcome Value)
runSearch _plan _query _extraPkgs =
  Right $ failureOutcome $ OutcomeError
    "USER_ERROR"
    "search requires a Hoogle DB; call runSearchWith via the CLI dispatcher"
    2

-- | Run a Hoogle search against the supplied 'Hoogle' record.
--
-- Builds a 'Related' list pointing at the first five hits so the agent can
-- recurse into them with @hypha symbol …@ — the "doorway" principle.
runSearchWith :: Monad m => Hoogle m -> Text -> [Text] -> m (Outcome Value)
runSearchWith hoogle q extras = do
  let queryText = Text.intercalate " " (q : map ("+" <>) extras)
  hits <- searchHoogle hoogle (HoogleQuery queryText)
  let shits = map fromHoogle hits
      body  = SearchResult { srQuery = queryText, srHits = shits }
      rel   =
        [ Related (shName h)
                  ("hypha symbol " <> shPackage h <> "/" <> shModule h <> "/" <> shName h)
        | h <- take 5 shits
        ]
  pure (OutcomeSuccess (searchResultToJSON body) False [] mempty rel)

fromHoogle :: HoogleHit -> SearchHit
fromHoogle h = SearchHit
  { shName      = hhName h
  , shModule    = hhModule h
  , shPackage   = hhPackage h
  , shVersion   = ""                       -- ^ filled in once cabal-plan lookup wired (post-MVP)
  , shSignature = if Text.null (hhSig h) then Nothing else Just (hhSig h)
  }

searchResultToJSON :: SearchResult -> Value
searchResultToJSON (SearchResult query hits) = Aeson.object
  [ "query" .= query
  , "hits"  .= map hitToJSON hits
  ]

hitToJSON :: SearchHit -> Value
hitToJSON hit = Aeson.object $
  [ "name"      .= shName hit
  , "module"    .= shModule hit
  , "package"   .= shPackage hit
  , "fetch"     .= ("hypha symbol "
                    <> shPackage hit <> "/" <> shModule hit <> "/" <> shName hit)
  ]
  ++ maybe [] (\s -> ["signature" .= s]) (shSignature hit)
  ++ (if Text.null (shVersion hit) then [] else ["version" .= shVersion hit])
