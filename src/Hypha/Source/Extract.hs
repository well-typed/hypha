{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Extract the signature, Haddock documentation, and source-line
-- anchor for a top-level symbol in a Haskell module.  Both the
-- signature /lines/ and the Haddock prose come from
-- "Hypha.Source.Parser" (a @ghc-lib-parser@-backed parse): the parser
-- attaches doc comments to their declarations exactly as Haddock does,
-- so no comment line scanning happens here anymore.  The only
-- line-based work left is slicing the signature /text/ (and, for
-- type\/class bodies, the declaration slice) out of the original source
-- by the line numbers the parser reports.
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
    -- ^ The declaration's Haddock prose as rendered by GHC (comment
    -- markers stripped, contiguous @-- |@ lines merged).
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
         let defL = Parser.declDefLine d
         in SymbolInfo
              { siSignature = sigText numbered d
              , siHaddock   = DocText <$> Parser.declDoc d
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
    -- ^ The module-level Haddock header (the @-- |@ block above the
    -- @module@ keyword), when present.
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
  (header, decls) <- Parser.parseModuleDoc path src
  let numbered = numberedLines src
  pure ModuleDocInfo
    { mdiHeader  = DocText <$> header
    , mdiEntries = map (entryFor numbered) decls
    }
  where
    entryFor ls d = DocEntry
      { deName      = Parser.declName d
      , deKind      = Parser.declKind d
      , deSignature = signatureFor ls d
      , deHaddock   = DocText <$> Parser.declDoc d
      , deSigLine   = Parser.declSigLine d
      , deDefLine   = Parser.declDefLine d
      }

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
