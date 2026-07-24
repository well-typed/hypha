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
    -- * Batch module extraction
  , ModuleDocInfo (..)
  , DocEntry (..)
  , extractModuleDoc
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
  , siKind      :: !(Maybe Parser.DeclKind)
    -- ^ Declaration kind when the parser could classify the symbol.
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
         SymbolInfo Nothing Nothing Nothing Nothing Nothing
       Just d  ->
         let mSig = sigText numbered d
             hd   = haddockBefore numbered d
             defL = Parser.declDefLine d
         in SymbolInfo
              { siSignature = mSig
              , siHaddock   = hd
              , siKind      = Just (Parser.declKind d)
              , siSigLine   = Parser.declSigLine d
              , siLine      = case defL of
                                 Just _  -> defL
                                 Nothing -> Parser.declSigLine d
              }

-- Batch module extraction -------------------------------------------

-- | Everything the module documentation view needs, extracted from a
-- single parse of the module source.
data ModuleDocInfo = ModuleDocInfo
  { mdiHeader  :: !(Maybe DocText)
    -- ^ The module-level @-- |@ comment block above the @module@
    -- keyword, when present.
  , mdiEntries :: ![DocEntry]
    -- ^ One entry per top-level declaration, in source order.
  }
  deriving stock (Show, Eq)

-- | A single top-level declaration, ready for rendering.
data DocEntry = DocEntry
  { deName      :: !Text
  , deKind      :: !Parser.DeclKind
  , deSignature :: !(Maybe Text)
    -- ^ The @name :: ...@ signature for values; for type\/class
    -- declarations without one, the raw source slice of the
    -- declaration body (clamped to 'declSliceLimit' lines).
  , deHaddock   :: !(Maybe DocText)
  , deSigLine   :: !(Maybe Int)
  , deDefLine   :: !(Maybe Int)
  }
  deriving stock (Show, Eq)

-- | Extract the whole module's documentation in one parse.  The
-- per-symbol path ('extractSymbolInfo') re-parses the module for every
-- query, which is fine for a single symbol card but quadratic when a
-- module page needs every export.
extractModuleDoc :: FilePath -> Text -> Either Parser.ParseError ModuleDocInfo
extractModuleDoc path src = do
  decls <- Parser.parseDecls path src
  let numbered = numberedLines src
  pure ModuleDocInfo
    { mdiHeader  = moduleHeaderBlock numbered
    , mdiEntries = map (entryFor numbered) decls
    }
  where
    entryFor ls d = DocEntry
      { deName      = Parser.declName d
      , deKind      = Parser.declKind d
      , deSignature = signatureFor ls d
      , deHaddock   = anchorLineOf d >>= haddockAbove ls
      , deSigLine   = Parser.declSigLine d
      , deDefLine   = Parser.declDefLine d
      }

    -- The line the decl's Haddock block sits above: the signature when
    -- there is one, else the first definition line.
    anchorLineOf d = case Parser.declSigLine d of
      Just n  -> Just n
      Nothing -> Parser.declDefLine d

    -- Values render their signature; type-ish decls without a @::@
    -- signature render the (clamped) source slice of their body so
    -- constructors, fields, and methods stay visible.  Function bodies
    -- are never sliced — they are implementation, not interface.
    signatureFor ls d = case sigText ls d of
      Just t  -> Just t
      Nothing
        | Parser.declKind d == Parser.DkFunction -> Nothing
        | otherwise -> do
            s <- Parser.declDefLine d
            e <- Parser.declDefEndLine d
            declSlice ls s e

-- | Maximum number of source lines a type\/class body slice may carry
-- before it is clamped with a trailing ellipsis.
declSliceLimit :: Int
declSliceLimit = 40

-- | Slice lines @[s .. e]@ out of the numbered source, clamped to
-- 'declSliceLimit' lines with a trailing @…@ marker when truncated.
declSlice :: [(Int, Text)] -> Int -> Int -> Maybe Text
declSlice ls s e =
  case [ t | (i, t) <- ls, i >= s, i <= e ] of
    []    -> Nothing
    slice ->
      let clamped = take declSliceLimit slice
          suffix  = [ "\x2026" | length slice > declSliceLimit ]
      in Just (Text.stripEnd (Text.unlines (clamped <> suffix)))

-- | The module-level Haddock header: the contiguous comment block that
-- ends directly above the @module@ keyword (allowing pragma and blank
-- lines in between), provided it contains a @-- |@ starter.  A comment
-- block separated from the header scan by a blank line boundary within
-- itself (e.g. a licence header higher up) is not collected.
moduleHeaderBlock :: [(Int, Text)] -> Maybe DocText
moduleHeaderBlock ls = do
  modLn <- lookupModuleLine
  collectHaddockBlock (linesAbove ls modLn)
  where
    lookupModuleLine =
      case [ i | (i, t) <- ls, isModuleLine (Text.stripStart t) ] of
        (i : _) -> Just i
        []      -> Nothing
    isModuleLine t = "module " `Text.isPrefixOf` t || t == "module"

-- | The source lines strictly above @anchor@, stripped of leading
-- whitespace and ordered nearest-to-the-anchor first — the shape
-- 'collectHaddockBlock' consumes.
linesAbove :: [(Int, Text)] -> Int -> [Text]
linesAbove ls anchor =
  map (Text.stripStart . snd) (reverse (takeWhile (\(k, _) -> k < anchor) ls))

-- | Collect a leading Haddock comment block from source lines ordered
-- nearest-to-the-anchor first (the reversed prefix above a declaration
-- signature or the @module@ keyword).
--
-- Blank and pragma lines between the block and its anchor are skipped
-- before collection.  This matches Haddock itself: a @-- |@ comment
-- attaches to the following declaration regardless of intervening
-- blank lines (whitespace is invisible to the parser), and pragmas
-- conventionally stack directly above a declaration.  A plain
-- @takeWhile isCommentLine@ would halt at the first blank line and so
-- be stricter than Haddock — silently dropping the documentation of
-- any symbol written doc-block / blank-line / signature, a common
-- idiom in @containers@, @text@, and friends.
--
-- Pragma lines are also excluded from the block itself: @{-# ... #-}@
-- satisfies 'isCommentLine' (it starts with @{-@) but is not prose.
-- The block is returned in source order, trailing whitespace trimmed,
-- only when it contains a @-- |@\/@-- ^@ starter.
collectHaddockBlock :: [Text] -> Maybe DocText
collectHaddockBlock stripped
  | not (null block) && any isHaddockStarter block =
      Just (DocText (Text.stripEnd (Text.unlines (reverse block))))
  | otherwise = Nothing
  where
    block = takeWhile (\t -> isCommentLine t && not (isPragmaLine t))
                      (dropWhile isSkippable stripped)
    isSkippable t = Text.null t || isPragmaLine t

isPragmaLine :: Text -> Bool
isPragmaLine = Text.isPrefixOf "{-#"

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
  haddockAbove ls startLn

-- | The Haddock comment block sitting above @startLn@, skipping any
-- blank\/pragma lines between the block and the declaration.
haddockAbove :: [(Int, Text)] -> Int -> Maybe DocText
haddockAbove ls startLn = collectHaddockBlock (linesAbove ls startLn)

isCommentLine :: Text -> Bool
isCommentLine t =
  "--" `Text.isPrefixOf` t
  || "{-" `Text.isPrefixOf` t
  || "-}" `Text.isPrefixOf` t

isHaddockStarter :: Text -> Bool
isHaddockStarter t = "-- |" `Text.isPrefixOf` t || "-- ^" `Text.isPrefixOf` t
