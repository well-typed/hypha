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
  , EntryOrigin (..)
  , extractModuleDoc
  , resolveModuleEntries
  ) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text (Text)

import Hypha.Search.Index (ModuleSource (..))
import Hypha.Search.Reexport qualified as Reexport
import Hypha.Source.Extensions qualified as Extensions
import Hypha.Source.Interface (ModuleInterface (..))
import Hypha.Source.Interface qualified as Interface
import Hypha.Source.Parser qualified as Parser
import Hypha.Types.Doc (DocText (..))
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

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
-- | Where a module-page entry came from.
--
-- A wrapper module's page is almost entirely re-exports, and saying which
-- module actually defines each entry is the difference between a useful
-- page and a list of names.
data EntryOrigin
  = EntryLocal
  | EntryReexport !ModulePath
  deriving stock (Show, Eq)

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
  , deOrigin    :: !EntryOrigin
    -- ^ Local declaration, or the module this entry is re-exported from.
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
    entryFor ls d = docEntryFrom ls d EntryLocal

-- | Build one entry from a declaration and its module's numbered lines.
--
-- Exported so the resolved module-page pass builds entries identically to
-- the local one: a re-exported entry must render the same way as if it had
-- been declared where it is shown.
docEntryFrom :: [(Int, Text)] -> Parser.Decl -> EntryOrigin -> DocEntry
docEntryFrom ls d origin = DocEntry
  { deName      = Parser.declName d
  , deKind      = Parser.declKind d
  , deSignature = signatureFor ls d
  , deHaddock   = DocText <$> Parser.declDoc d
  , deSigLine   = Parser.declSigLine d
  , deDefLine   = Parser.declDefLine d
  , deOrigin    = origin
  }
  where
    signatureFor lns decl = case sigText lns decl of
      Just t  -> Just t
      Nothing
        | Parser.declKind decl == Parser.DkFunction -> Nothing
        | otherwise -> do
            st <- Parser.declDefLine decl
            e  <- Parser.declDefEndLine decl
            declSlice lns st e

-- | Every entry a module's page should show, re-exports included.
--
-- 'extractModuleDoc' reports only locally declared declarations, so a pure
-- re-export module produced nothing: @Data.Map.Strict@ had an empty \"On
-- this page\" rail because it declares almost nothing.  Resolving its
-- exports to their definitions means the wrapper shows what Haddock shows,
-- each entry tagged with where the code actually lives.
resolveModuleEntries
  :: Extensions.LanguageSettings
  -> [ModuleSource]
  -> ModulePath
  -> Either Parser.ParseError ModuleDocInfo
resolveModuleEntries langs sources asking = do
  ifaces <- traverse parseOne sources
  let byName     = Map.fromList [ (miName i, i) | i <- ifaces ]
      linesOf    = Map.fromList
        [ (miName i, numberedLines (msContent ms))
        | (ms, i) <- zip sources ifaces
        ]
      resolution = Reexport.resolveComponent ifaces
  asked <- maybe (Left (missingModule asking)) Right (Map.lookup asking byName)
  pure ModuleDocInfo
    { mdiHeader  = DocText <$> miHeaderDoc asked
    , mdiEntries =
        [ docEntryFrom ls decl origin
        | name <- Reexport.expandedExportNames ifaces asking
        , Just res <- [Map.lookup (asking, name) resolution]
        , let defMod = Reexport.definitionModule asking (Reexport.resSite res)
        , Just defIface <- [Map.lookup defMod byName]
        , Just decl <- [Parser.findDecl (unSymbolName name) (miDecls defIface)]
        , let ls = Map.findWithDefault [] defMod linesOf
        , let origin = if defMod == asking then EntryLocal else EntryReexport defMod
        ]
    }
  where
    parseOne ms = Interface.parseInterface langs (msPath ms) (msContent ms)

-- | A URL can name a module the component does not have.  That is a
-- reportable absence, not a programmer error, so it travels as a
-- 'Parser.ParseError' rather than an exception.
missingModule :: ModulePath -> Parser.ParseError
missingModule m = Parser.ParseError
  { Parser.peMessage =
      "module " <> unModulePath m <> " is not part of this component"
  , Parser.peLine              = Nothing
  , Parser.peUnknownExtensions = []
  , Parser.peDiagnostics       = []
  }

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
