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
  , noSymbolInfo
  , extractSymbolInfo
  , symbolInfoFromDecl
  , numberedLines
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

import Hypha.Search.Index
  ( DefinitionRef (..), ImportedDefinitions (..), ModuleSource (..) )
import Hypha.Search.Reexport qualified as Reexport
import Hypha.Source.Extensions qualified as Extensions
import Hypha.Source.Interface (ModuleInterface (..))
import Hypha.Source.Interface qualified as Interface
import Hypha.Source.Parser (numberedLines)
import Hypha.Source.Parser qualified as Parser
import Hypha.Types.ComponentName (ComponentKey)
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
--
-- For callers that hold only bytes and a name.  A caller that already has
-- the declaration — because it resolved the symbol to it — should use
-- 'symbolInfoFromDecl' instead: this parses under
-- 'Extensions.defaultLanguageSettings', which is honest only outside a
-- component context.
extractSymbolInfo :: Text -> Text -> Either Parser.ParseError SymbolInfo
extractSymbolInfo src sym = do
  decls <- Parser.parseDecls "<source>" src
  pure $ case Parser.findDecl sym decls of
    Nothing -> noSymbolInfo
    Just d  -> symbolInfoFromDecl (numberedLines src) d

-- | Nothing known about the symbol: the module parsed and does not
-- declare it.  Distinct from a parse failure, which is a 'Left'.
noSymbolInfo :: SymbolInfo
noSymbolInfo = SymbolInfo Nothing Nothing Nothing Nothing Nothing

-- | 'SymbolInfo' for a declaration already in hand.
symbolInfoFromDecl :: [(Int, Text)] -> Parser.Decl -> SymbolInfo
symbolInfoFromDecl numbered d = SymbolInfo
  { siSignature = Parser.declSigTextIn numbered d
  , siHaddock   = DocText <$> Parser.declDoc d
  , siKind      = Just (Parser.declKind d)
  , siSigLine   = Parser.declSigLine d
  , siLine      = case Parser.declDefLine d of
      Just l  -> Just l
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
  , mdiSkipped :: ![(ModulePath, Parser.ParseError)]
    -- ^ Sibling modules of the component that would not parse.
    --
    -- Carried rather than thrown: the page asked about /one/ module, and
    -- one unparseable module elsewhere in the component must not take it
    -- down.  @base@ has a module that fails on indentation, which is
    -- enough to degrade every @base@ page to an export list when the whole
    -- component has to parse for any of it to render.  The caller reports
    -- these, so a thinner page still says why.
  }
  deriving stock (Show, Eq)

-- | Where a module-page entry came from.
--
-- A wrapper module's page is almost entirely re-exports, and saying which
-- module actually defines each entry is the difference between a useful
-- page and a list of names.
--
-- 'EntryReexport' carries a full 'DefinitionRef' rather than a
-- 'ModulePath' because a re-export can cross a package boundary: @base@'s
-- @Data.Traversable@ documents entries declared in @ghc-internal@.  One
-- constructor covers both cases — the renderer names the package only when
-- it differs from the page's — so a "re-export" of the very module being
-- rendered is not representable; that is 'EntryLocal'.
data EntryOrigin
  = EntryLocal
  | EntryReexport !DefinitionRef
  | EntryUnplaced !ModulePath
    -- ^ The module exports this name and we could not find out where it
    -- is defined — the index has no row for it and the component's own
    -- modules do not declare it.  The 'ModulePath' is the import we
    -- believed supplies it, which is a guess and must not be rendered as
    -- a definition site.
    --
    -- It had shared 'EntryReexport', so a page presented the guess as
    -- fact: @base@'s @Prelude@ told the reader that @Bool@, @True@,
    -- @Just@ and @map@ are all defined in @GHC.Internal.Control.Monad@,
    -- and linked there, while the index knew @Bool@ is @ghc-prim@'s.
  deriving stock (Show, Eq)

-- | A single top-level declaration, ready for rendering.

data DocEntry = DocEntry
  { deName      :: !Text
  , deKind      :: !(Maybe Parser.DeclKind)
    -- ^ 'Nothing' for an entry we could not place: there is no
    -- declaration to read a kind off.  It used to be filled in with
    -- 'Parser.DkFunction', which put @Bool@, @Maybe@ and @Functor@ under
    -- \"Values\" in the rail and gave them @#v:@ anchors that no @#t:@
    -- link from a prebuilt page could resolve.
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
    , mdiSkipped = []
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
  , deKind      = Just (Parser.declKind d)
  , deSignature = signatureFor ls d
  , deHaddock   = DocText <$> Parser.declDoc d
  , deSigLine   = Parser.declSigLine d
  , deDefLine   = Parser.declDefLine d
  , deOrigin    = origin
  }
  where
    signatureFor lns decl = case Parser.declSigTextIn lns decl of
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
-- this page\" rail because it declares almost nothing, and @base@'s
-- @Data.Traversable@ had one because it declares nothing at all.
--
-- @imported@ carries what the index resolved for names this component does
-- not declare, plus the sources of the modules it named.  An export the
-- index has no answer for falls back to resolving within the component, and
-- one we cannot describe at all is still listed with its origin and no
-- signature: a name without a signature is worth more to the reader than a
-- silently shorter page.
-- Lives in 'IO' for one reason: @cpphs@ signals an undefined build-time
-- macro by calling 'error' from pure code, so the only way to parse a
-- module without risking the whole request is
-- 'Interface.parseInterfaceIO'.  This used to call the pure sibling and
-- an unhandled @#error \"CURRENT_PACKAGE_KEY undefined\"@ answered the
-- page with a 500.
resolveModuleEntries
  :: Extensions.LanguageSettings
  -> ComponentKey                                  -- ^ the asking component
  -> [ModuleSource]
  -> ImportedDefinitions                           -- ^ what the index resolved
  -> ModulePath
  -> IO (Either Parser.ParseError ModuleDocInfo)
resolveModuleEntries langs compKey sources imported asking = do
  -- Per module, not all-or-nothing, for the component's own modules /and/
  -- for the ones borrowed from a dependency: 'traverse' over either made
  -- one unparseable sibling degrade every page to an export list.
  attempted      <- Interface.parseSources langs sources
  importedParsed <- mapM parseImported (Map.toList (idSources imported))
  let ok      = [ (ms, i) | (ms, Right i) <- attempted ]
      ifaces  = map snd ok
      skipped = [ (msDeclaredName ms, e) | (ms, Left e) <- attempted ]
                  ++ [ (msDeclaredName ms, e)
                     | (_, (_, ms, Left e)) <- importedParsed ]
      byName  = Map.fromList [ (miName i, i) | i <- ifaces ]
      linesOf = Map.fromList
        [ (miName i, numberedLines (msContent ms)) | (ms, i) <- ok ]
      -- Keyed on the module name the caller asked for, not on the parsed
      -- name: that is how the resolution refers to it.
      outsideOf = Map.fromList
        [ (m, (c, i, numberedLines (msContent ms)))
        | (m, (c, ms, Right i)) <- importedParsed
        ]
      resolution = Reexport.resolveComponent ifaces
  -- The module the page is about is the one failure that is fatal: with no
  -- parse of it there is no export list to walk.  Its own error beats the
  -- generic absence when we have one.
  pure $ do
    asked <- case Map.lookup asking byName of
      Just i  -> Right i
      Nothing -> Left (case lookup asking skipped of
        Just e  -> e
        Nothing -> missingModule asking)
    pure ModuleDocInfo
      { mdiHeader  = DocText <$> miHeaderDoc asked
      , mdiSkipped = skipped
      , mdiEntries =
          [ entry
          | name <- Reexport.expandedExportNames ifaces asking
          , Just res <- [Map.lookup (asking, name) resolution]
          , Just entry <-
              [entryFor byName linesOf outsideOf name (Reexport.resSite res)]
          ]
      }
  where
    parseImported (m, (c, ms)) = do
      (_, i) <- Interface.parseSource langs ms
      pure (m, (c, ms, i))

    -- The index's answer first, because it is the only transitively resolved
    -- one: @base@'s @Data.List@ reaches @GHC.Internal.Data.List@, which
    -- declares nothing and passes @mapAccumL@ along.  Reading the immediate
    -- import found no declaration and dropped the entry.
    entryFor byName linesOf outsideOf name site =
      case fromIndex outsideOf name of
        Just e  -> Just e
        Nothing -> case site of
          Reexport.DefinedOutside m
            | m /= asking -> case fromModule outsideOf name m of
                Just e  -> Just e
                -- Everything we can still say: the name, and where it came
                -- from.  Dropping it would leave no trace of a symbol the
                -- module genuinely exports.
                Nothing -> Just (placeholder name m)
          _ -> do
            let defMod = Reexport.definitionModule asking site
            defIface <- Map.lookup defMod byName
            decl     <- Parser.findDecl (unSymbolName name) (miDecls defIface)
            let ls     = Map.findWithDefault [] defMod linesOf
                origin = if defMod == asking
                           then EntryLocal
                           else EntryReexport (DefinitionRef compKey defMod)
            pure (docEntryFrom ls decl origin)

    -- An entry built from the definition site the index resolved.
    fromIndex outsideOf name = do
      def <- Map.lookup name (idSites imported)
      (_, i, ls) <- Map.lookup (drModule def) outsideOf
      decl <- Parser.findDecl (unSymbolName name) (miDecls i)
      pure (docEntryFrom ls decl (EntryReexport def))

    -- An entry built from a module we happen to have, when the index had no
    -- answer for this name.
    fromModule outsideOf name m = do
      (c, i, ls) <- Map.lookup m outsideOf
      decl <- Parser.findDecl (unSymbolName name) (miDecls i)
      pure (docEntryFrom ls decl (EntryReexport (DefinitionRef c m)))

    -- Everything we know when the defining source is out of reach: the
    -- name, and the import we believed supplies it -- carried as a guess,
    -- because that is what it is.
    placeholder name believed = DocEntry
      { deName      = unSymbolName name
      , deKind      = Nothing
      , deSignature = Nothing
      , deHaddock   = Nothing
      , deSigLine   = Nothing
      , deDefLine   = Nothing
      , deOrigin    = EntryUnplaced believed
      }

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

