{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Source.Extract
  ( SymbolInfo (..)
  , extractSymbolInfo
  ) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.Doc (DocText (..))

-- | Information extracted from a source file for a single symbol.
data SymbolInfo = SymbolInfo
  { siSignature :: !(Maybe Text)
  , siHaddock   :: !(Maybe DocText)
  , siSigLine   :: !(Maybe Int)
    -- ^ Line of the bare @sym :: ...@ signature, when present.  This is
    -- the most faithful source anchor: it sits above any CPP @#ifdef@
    -- branches and never moves with platform-specific bodies.
  , siLine      :: !(Maybe Int)
    -- ^ First top-level definition line for the symbol after its
    -- signature.  Falls back to the signature line when the symbol has no
    -- visible definition (e.g. in a re-export module).
  }
  deriving stock (Show, Eq)

-- | Extract 'SymbolInfo' for a symbol from the source text of a module.
--
-- This is a naïve line-based parser sufficient for canonical Haskell
-- source layout.  It looks for:
--
-- 1. A type signature line @sym :: ...@
-- 2. Haddock comment lines (@-- |@ or @-- ^@) immediately preceding it
-- 3. The definition line (first line mentioning @sym@ after the signature)
--
-- Post-MVP this should be replaced by a @ghc-lib-parser@ based approach.
extractSymbolInfo :: Text -> Text -> SymbolInfo
extractSymbolInfo src sym =
  let ls       = numberedLines src
      mSigLine = findSignatureLine sym ls
  in SymbolInfo
       { siSignature = snd <$> mSigLine
       , siHaddock   = mSigLine >>= haddockForLine ls
       , siSigLine   = fst <$> mSigLine
       , siLine      = mSigLine >>= definitionAfter ls sym
       }

-- | Pair each line with its 1-based index.
numberedLines :: Text -> [(Int, Text)]
numberedLines = zip [1 :: Int ..] . Text.lines

-- | The first line that is a type signature for the given symbol.
findSignatureLine :: Text -> [(Int, Text)] -> Maybe (Int, Text)
findSignatureLine sym = listToMaybe . filter (isSignatureLine sym . snd)

-- | Find the definition line for a symbol that appears after the given
-- signature line index.
definitionAfter :: [(Int, Text)] -> Text -> (Int, Text) -> Maybe Int
definitionAfter ls sym (sigIdx, _) = listToMaybe
  [ j
  | (j, l) <- dropWhile (\(k, _) -> k <= sigIdx) ls
  , not (isCommentLine l)
  , isDefinitionLine sym l
  ]

-- | Extract the Haddock block immediately preceding a given line index.
haddockForLine :: [(Int, Text)] -> (Int, Text) -> Maybe DocText
haddockForLine ls (i, _) =
  let prior      = reverse (takeWhile (\(k, _) -> k < i) ls)
      block      = extractHaddockBlock prior
  in if null block
       then Nothing
       else Just (DocText (Text.unlines (reverse block)))

-- | Check whether a line is a type signature for the given symbol.
--
-- We require @sym@ to appear at the start of the line (after whitespace)
-- followed by " ::" (with possible spaces around the colon-colon).
isSignatureLine :: Text -> Text -> Bool
isSignatureLine sym l =
  let trimmed = Text.dropWhile (== ' ') l
  in  (sym <> " ::") `Text.isPrefixOf` trimmed
  ||  (sym <> "::")  `Text.isPrefixOf` trimmed

-- | Check whether a line is a definition of the given symbol.
--
-- A definition line starts with the symbol name (after whitespace) and is
-- not a comment or signature.
isDefinitionLine :: Text -> Text -> Bool
isDefinitionLine sym l =
  let trimmed = Text.dropWhile (== ' ') l
  in sym `Text.isPrefixOf` trimmed
     && not (" ::" `Text.isInfixOf` l)
     && not (isCommentLine l)

-- | Check whether a line is a comment (Haddock or plain).
-- We accept any line starting with @--@, not just @-- @, so that bare
-- Haddock separator lines (e.g. @  --@) are recognised.
isCommentLine :: Text -> Bool
isCommentLine l =
  let trimmed = Text.dropWhile (== ' ') l
  in "--" `Text.isPrefixOf` trimmed
     || "{-" `Text.isPrefixOf` trimmed
     || "-}" `Text.isPrefixOf` trimmed

-- | Extract a contiguous block of Haddock comment lines from a reversed list
-- of preceding lines (closest to the signature first).
--
-- We take the leading run of comment lines; if that run contains a Haddock
-- starter (@-- |@ or @-- ^@) we return the whole run, otherwise '[]'.
extractHaddockBlock :: [(Int, Text)] -> [Text]
extractHaddockBlock prior =
  let lines_     = map (Text.dropWhile (== ' ') . snd) prior
      commentRun = takeWhile isCommentLine lines_
      hasStarter = any isHaddockStarter commentRun
  in if hasStarter then commentRun else []

-- | A Haddock starter line begins with @-- |@ or @-- ^@.
isHaddockStarter :: Text -> Bool
isHaddockStarter t = "-- |" `Text.isPrefixOf` t || "-- ^" `Text.isPrefixOf` t
