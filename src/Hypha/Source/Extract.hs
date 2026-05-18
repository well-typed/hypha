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
  , siLine      :: !(Maybe Int)
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
  let ls       = zip [1 :: Int ..] (Text.lines src)
      -- Find the signature line: the first line where sym appears followed
      -- by " ::" (with possible whitespace).
      mSigLine = listToMaybe
        [ (i, l)
        | (i, l) <- ls
        , isSignatureLine sym l
        ]
      -- Find the definition line: first non-comment, non-signature line
      -- that starts with the symbol name after the signature.
      mDefLine = case mSigLine of
        Nothing    -> Nothing
        Just (i, _) -> listToMaybe
          [ j
          | (j, l) <- dropWhile (\(k, _) -> k <= i) ls
          , not (isCommentLine l)
          , isDefinitionLine sym l
          ]
      -- Extract Haddock from lines immediately preceding the signature.
      haddock  = case mSigLine of
        Nothing     -> Nothing
        Just (i, _) ->
          let prior = takeWhile (\(k, _) -> k < i) ls
              block = extractHaddockBlock (reverse prior)
          in if null block then Nothing
             else Just (DocText (Text.unlines (reverse block)))
  in SymbolInfo
       { siSignature = snd <$> mSigLine
       , siHaddock   = haddock
       , siLine      = mDefLine
       }

-- | Check whether a line is a type signature for the given symbol.
--
-- We require @sym@ to appear at the start of the line (after whitespace)
-- followed by " ::" (with possible spaces around the colon-colon).
isSignatureLine :: Text -> Text -> Bool
isSignatureLine sym l =
  let trimmed = Text.dropWhile (== ' ') l
      pat1    = sym <> " ::"
      pat2    = sym <> "::"
  in pat1 `Text.isPrefixOf` trimmed || pat2 `Text.isPrefixOf` trimmed

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
isCommentLine :: Text -> Bool
isCommentLine l =
  let trimmed = Text.dropWhile (== ' ') l
  in "-- " `Text.isPrefixOf` trimmed
     || "{-" `Text.isPrefixOf` trimmed
     || "-}" `Text.isPrefixOf` trimmed

-- | Extract a contiguous block of Haddock comment lines from a reversed list
-- of preceding lines.
--
-- Haddock lines start with @-- |@ or @-- ^@.  We also collect continuation
-- lines (plain @--@ comments) that follow a Haddock starter, stopping at the
-- first non-comment line or a blank line.
extractHaddockBlock :: [(Int, Text)] -> [Text]
extractHaddockBlock = go []
  where
    go _acc [] = []
    go acc ((_, l) : rest) =
      let trimmed = Text.dropWhile (== ' ') l
      in if isHaddockLine trimmed
           then go (trimmed : acc) rest
           else if isContinuationLine trimmed && not (null acc)
                  then go (trimmed : acc) rest
                  else acc

    isHaddockLine t =
      "-- |" `Text.isPrefixOf` t || "-- ^" `Text.isPrefixOf` t

    isContinuationLine t =
      "--" `Text.isPrefixOf` t
      && not ("-- |" `Text.isPrefixOf` t)
      && not ("-- ^" `Text.isPrefixOf` t)
      && not (Text.null (Text.drop 2 t))
