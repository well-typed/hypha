{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The live search box's query language.
--
-- A query is free text plus an optional @pkg:\<component\>@ token that
-- restricts the search to one component, e.g. @pkg:aeson decode@.  The
-- token may appear anywhere in the query, and names a component key
-- exactly (@pkg:hypha:exe:hypha-mcp@ works).  The same restriction can
-- also come from the scope toggle next to the search box; when both are
-- present the typed prefix wins, because it is the more deliberate of
-- the two.
module Hypha.Search.Query
  ( SearchQuery (..)
  , QueryError (..)
  , parseSearchQuery
  , scopeToken
  , scopeParam
  , resolveSearchQuery
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.ComponentName (ComponentKey (..))

-- | A parsed search query.
data SearchQuery = SearchQuery
  { sqScope :: !(Maybe ComponentKey)
      -- ^ The component to restrict the search to, if any.
  , sqText  :: !Text
      -- ^ The query with every @pkg:@ token removed.
  }
  deriving stock (Show, Eq)

-- | Why a query cannot be run.  Rendered as a row in the results
-- dropdown, so the user sees what is wrong instead of an empty list.
data QueryError
  = ScopeMissingName
      -- ^ A bare @pkg:@ with nothing after it.
  | ConflictingScopes !ComponentKey !ComponentKey
      -- ^ Two different @pkg:@ tokens in the same query.
  | UnknownScope !ComponentKey
      -- ^ The scope names no component of the build plan.
  deriving stock (Show, Eq)

scopePrefix :: Text
scopePrefix = "pkg:"

-- | Split the @pkg:@ tokens out of a raw query.
parseSearchQuery :: Text -> Either QueryError SearchQuery
parseSearchQuery raw = do
  scope <- case nubOrd (mapMaybe (Text.stripPrefix scopePrefix) ws) of
    []                    -> pure Nothing
    ss | any Text.null ss -> Left ScopeMissingName
    [s]                   -> pure (Just (ComponentKey s))
    (a : b : _)           -> Left (ConflictingScopes (ComponentKey a) (ComponentKey b))
  pure SearchQuery
    { sqScope = scope
    , sqText  = Text.unwords (filter (not . Text.isPrefixOf scopePrefix) ws)
    }
  where
    ws = Text.words raw

-- | The @pkg:@ token that scopes a query to a component.
scopeToken :: ComponentKey -> Text
scopeToken (ComponentKey k) = scopePrefix <> k

-- | Read the scope toggle's @pkg@ parameter.  The hidden input backing
-- the toggle is included in every request, so a switched-off toggle
-- arrives as an empty string rather than as an absent parameter.
scopeParam :: Maybe Text -> Maybe ComponentKey
scopeParam (Just t) | not (Text.null t) = Just (ComponentKey t)
scopeParam _                                 = Nothing

-- | Parse a query, fall back to the toggle's scope when the query names
-- none, and check the resulting scope against the plan's components.
resolveSearchQuery
  :: [ComponentKey]        -- ^ components of the build plan
  -> Maybe ComponentKey    -- ^ scope toggle, if switched on
  -> Text                  -- ^ raw query
  -> Either QueryError SearchQuery
resolveSearchQuery known toggle raw = do
  sq <- parseSearchQuery raw
  let scope = sqScope sq <|> toggle
  for_ scope $ \s -> unless (s `elem` known) (Left (UnknownScope s))
  pure sq { sqScope = scope }
