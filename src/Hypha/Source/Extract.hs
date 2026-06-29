{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Extract the signature, Haddock prelude, and source-line anchor
-- for a top-level symbol in a Haskell module.  The line-based parser
-- that used to live here has been replaced by "Hypha.Source.Parser"
-- (a @ghc-lib-parser@-backed scanner) for everything that involves
-- identifying the signature and definition lines.  The remaining
-- responsibility of this module — slicing the signature text and
-- the preceding Haddock comment block out of the original source —
-- is still line-based, because Haddock comments are not attached to
-- declarations in the parse tree we produce.
module Hypha.Source.Extract
  ( SymbolInfo (..)
  , extractSymbolInfo
  ) where

import Data.Text qualified as Text
import Data.Text (Text)
import Hypha.Source.Parser qualified as Parser
import Hypha.Types.Doc (DocText (..))

-- | Information extracted from a source file for a single symbol.
-- The shape predates the parser rewrite; downstream consumers
-- (notably "Hypha.Command.Symbol" and "Hypha.Command.Server") only
-- need 'siSignature', 'siHaddock', 'siSigLine', and 'siLine', so
-- those four fields are the contract.
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
extractSymbolInfo :: Text -> Text -> SymbolInfo
extractSymbolInfo src sym =
  let numbered = numberedLines src
      decls    = either (const []) id (Parser.parseDecls "<source>" src)
      mDecl    = Parser.findDecl sym decls
  in case mDecl of
       Nothing ->
         SymbolInfo Nothing Nothing Nothing Nothing
       Just d  ->
         let mSig = sigText numbered d
             hd   = haddockBefore numbered d
             defL = Parser.declDefLine d
         in SymbolInfo
              { siSignature = mSig
              , siHaddock   = hd
              , siSigLine   = Parser.declSigLine d
              , siLine      = case defL of
                                 Just _  -> defL
                                 Nothing -> Parser.declSigLine d
              }

-- Internals --------------------------------------------------------

-- | Pair each line with its 1-based index.
numberedLines :: Text -> [(Int, Text)]
numberedLines = zip [1 :: Int ..] . Text.lines

-- | Pull the signature text for a decl out of the line-numbered
-- source, joining continuation lines into a single whitespace-
-- collapsed string.
sigText :: [(Int, Text)] -> Parser.Decl -> Maybe Text
sigText ls d = do
  startLn <- Parser.declSigLine d
  let endLn = case Parser.declSigEndLine d of
                Just e  -> max startLn e
                Nothing -> startLn
      slice = [ t | (i, t) <- ls, i >= startLn, i <= endLn ]
  case slice of
    []    -> Nothing
    parts -> Just (Text.unwords (filter (not . Text.null) (map Text.strip parts)))

-- | Grab the contiguous Haddock comment block immediately preceding
-- the signature line, if any.  We accept the same comment shapes the
-- old hand-written parser did (@-- |@, @-- ^@, with @-- $@/@-- @
-- continuation lines).  Pure line-scanning is appropriate here:
-- Haddock comments are formally /not/ attached to the parsed AST we
-- produce (we disable Haddock mode in the parser so it stays cheap),
-- and the position information is what makes the scan precise: we
-- only look at the lines that touch the signature.
haddockBefore :: [(Int, Text)] -> Parser.Decl -> Maybe DocText
haddockBefore ls d = do
  startLn <- Parser.declSigLine d
  let prior   = reverse (takeWhile (\(k, _) -> k < startLn) ls)
      stripped = map (Text.stripStart . snd) prior
      block    = takeWhile isCommentLine stripped
      hasStart = any isHaddockStarter block
  if hasStart && not (null block)
    then Just (DocText (Text.unlines (reverse block)))
    else Nothing

isCommentLine :: Text -> Bool
isCommentLine t =
  "--" `Text.isPrefixOf` t
  || "{-" `Text.isPrefixOf` t
  || "-}" `Text.isPrefixOf` t

isHaddockStarter :: Text -> Bool
isHaddockStarter t = "-- |" `Text.isPrefixOf` t || "-- ^" `Text.isPrefixOf` t
