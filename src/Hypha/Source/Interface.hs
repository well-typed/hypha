{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The parse-derived view of a single module: what it is called, what it
-- exports, what it imports, and what it declares.
--
-- Everything downstream reads this instead of re-deriving those facts by
-- other means.  Two of those other means were actively wrong:
--
-- * Module names were computed from file paths, so a stray @race.hs@
--   under a source dir became a module called @race@, and a component
--   whose @hs-source-dirs@ we failed to resolve contributed rows named
--   @compiler.GHC.Data.Word64Map.Internal@.  A module states its own
--   name; the path is evidence, not authority.
--
-- * Export lists came from a regex over the module header, which cannot
--   see the difference between @Map(..)@ and @Map@, and drops the
--   @module N@ re-export form entirely — the form @containers@' public
--   modules are largely built from.
module Hypha.Source.Interface
  ( ModuleInterface (..)
  , ExportItem (..)
  , ImportItem (..)
  , parseInterface
  , parseInterfaceIO
  , parseSource
  , parseSources
  , interfaceExportedNames
  , exportedNamesIO
  , declaredNames
  ) where

import Control.Exception (evaluate)
import Control.Exception.Safe (SomeException, displayException, try)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

import GHC.Hs
import GHC.Types.SrcLoc (unLoc)

import Hypha.Search.Index (ModuleSource (..))
import Hypha.Source.Extensions qualified as Extensions
import Hypha.Source.Parser (Decl)
import Hypha.Source.Parser qualified as Parser
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

-- | Everything one parse of a module tells us.
data ModuleInterface = ModuleInterface
  { miName      :: !ModulePath
  , miExports   :: !(Maybe [ExportItem])
    -- ^ 'Nothing' when the module has no explicit export list, which
    -- means \"everything declared here\" — a different fact from an empty
    -- list, so it gets a different representation.
  , miImports   :: ![ImportItem]
  , miDecls     :: ![Decl]
  , miHeaderDoc :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

-- | One entry of an export list.
data ExportItem
  = ExportSymbol !SymbolName ![SymbolName]
    -- ^ A name and its explicitly listed subordinates.  @Map(..)@ lists
    -- none of its own: the wildcard's contents are known at the
    -- definition site, not here.
  | ExportSymbolAll !SymbolName
    -- ^ The @T(..)@ form: the name and everything subordinate to it —
    -- a class's methods, a data type's constructors.  The subordinates
    -- are known at the definition site, so expansion happens against the
    -- component's declarations (see "Hypha.Search.Reexport"), not here.
  | ExportModule !ModulePath
    -- ^ The @module M@ re-export form.  Kept rather than flattened,
    -- because flattening it needs the target module's export list, which
    -- is "Hypha.Search.Reexport"'s job and not ours.
  deriving stock (Show, Eq)

-- | One import declaration, reduced to what re-export resolution needs.
data ImportItem = ImportItem
  { iiModule :: !ModulePath
  , iiNames  :: !(Maybe (Bool, [SymbolName]))
    -- ^ @Just (isHiding, names)@ for an explicit list; 'Nothing' for an
    -- unrestricted import.  The distinction decides whether the import
    -- can supply a given name at all.
  }
  deriving stock (Show, Eq)

-- | Parse a module and reduce it to its interface.
parseInterface
  :: Extensions.LanguageSettings
  -> FilePath
  -> Text
  -> Either Parser.ParseError ModuleInterface
parseInterface ls path src = do
  (hsMod, header, decls) <- Parser.parseModuleWith ls path src
  pure ModuleInterface
    { miName      = moduleNameOf hsMod
    , miExports   = exportsOf hsMod
    , miImports   = importsOf hsMod
    , miDecls     = decls
    , miHeaderDoc = header
    }

-- | 'parseInterface' with the impurity @cpphs@ smuggles in caught.
--
-- Some packages guard code with build-time-only CPP macros — @#error
-- \"CURRENT_PACKAGE_KEY undefined\"@ is the canonical one — that only a
-- real GHC invocation defines.  @cpphs@ reports that by calling 'error'
-- from pure code, so the failure escapes any @Either@: unhandled, one such
-- module aborted an entire index pass after five packages.
--
-- Forcing the parse inside 'try' turns it back into the typed failure the
-- rest of the pipeline already reports, so one module loses its rows and
-- every other module and package keeps its own.
--
-- The 'try' is @Control.Exception.Safe@'s, which rethrows asynchronous
-- exceptions.  This runs on a Warp handler thread, and the plain
-- 'Control.Exception.try' at @SomeException@ would swallow the timeout
-- manager's kill and report it as a parse failure — a request that should
-- have died would instead render a page missing every module it did not
-- get to.
parseInterfaceIO
  :: Extensions.LanguageSettings
  -> FilePath
  -> Text
  -> IO (Either Parser.ParseError ModuleInterface)
parseInterfaceIO ls path src = do
  outcome <- try (evaluate forced)
  pure $ case outcome of
    Right r                   -> r
    Left (e :: SomeException) -> Left Parser.ParseError
      { Parser.peMessage           = firstLine (Text.pack (displayException e))
      , Parser.peLine              = Nothing
      , Parser.peUnknownExtensions = []
      , Parser.peDiagnostics       = []
      }
  where
    -- Demanding the outer constructor is enough: cpphs runs ahead of the
    -- parser, so that is where it throws.
    forced = case parseInterface ls path src of
      Left e  -> Left e
      Right i -> Right i

    firstLine = Text.strip . Text.takeWhile (/= '\n')

-- | 'parseInterfaceIO' on a source we already hold, keeping the source
-- alongside its outcome so a caller can report a failure against the
-- module it belongs to.
--
-- One helper rather than one per caller: the indexer, the symbol card and
-- the module page each had their own copy of this three-line wrapper, and
-- the module page's copy was the pure 'parseInterface' -- which is how an
-- unpreprocessable module came to answer a page with a 500.
parseSource
  :: Extensions.LanguageSettings
  -> ModuleSource
  -> IO (ModuleSource, Either Parser.ParseError ModuleInterface)
parseSource ls ms = do
  r <- parseInterfaceIO ls (msPath ms) (msContent ms)
  pure (ms, r)

-- | 'parseSource' over a component's modules.  Per module, never
-- all-or-nothing: one unparseable sibling must not cost the others.
parseSources
  :: Extensions.LanguageSettings
  -> [ModuleSource]
  -> IO [(ModuleSource, Either Parser.ParseError ModuleInterface)]
parseSources ls = mapM (parseSource ls)

-- | A module with no @module … where@ header is an implicit @Main@ —
-- what GHC assumes, and what a bare script under a source dir is.
moduleNameOf :: HsModule GhcPs -> ModulePath
moduleNameOf m = case hsmodName m of
  Nothing -> ModulePath "Main"
  Just ln -> ModulePath (Text.pack (moduleNameString (unLoc ln)))

exportsOf :: HsModule GhcPs -> Maybe [ExportItem]
exportsOf m = fmap (mapMaybe (exportItem . unLoc) . unLoc) (hsmodExports m)

-- | Total over the constructor set on purpose.  A catch-all would turn a
-- future export form into silently missing rows, which is the class of
-- bug this module exists to remove — so when @ghc-lib-parser@ grows a
-- constructor, this stops compiling and someone decides what it means.
exportItem :: IE GhcPs -> Maybe ExportItem
exportItem = \case
  IEVar          _ n _      -> Just (ExportSymbol (wrapped n) [])
  IEThingAbs     _ n _      -> Just (ExportSymbol (wrapped n) [])
  IEThingAll     _ n _      -> Just (ExportSymbolAll (wrapped n))
  IEThingWith    _ n _ ss _ -> Just (ExportSymbol (wrapped n) (map wrapped ss))
  IEModuleContents _ lm     ->
    Just (ExportModule (ModulePath (Text.pack (moduleNameString (unLoc lm)))))
  -- Haddock section structure, not names.
  IEGroup{}    -> Nothing
  IEDoc{}      -> Nothing
  IEDocNamed{} -> Nothing

importsOf :: HsModule GhcPs -> [ImportItem]
importsOf m =
  [ ImportItem
      { iiModule = ModulePath (Text.pack (moduleNameString (unLoc (ideclName d))))
      , iiNames  = case ideclImportList d of
          Nothing               -> Nothing
          Just (interp, lNames) ->
            Just ( interp == EverythingBut
                 , mapMaybe (importedName . unLoc) (unLoc lNames)
                 )
      }
  | d <- map unLoc (hsmodImports m)
  ]

-- | An import list entry contributes the names it mentions, subordinates
-- included: @import M (Map(..), insertWith)@ can supply either.
importedName :: IE GhcPs -> Maybe SymbolName
importedName ie = case exportItem ie of
  Just (ExportSymbol n _)   -> Just n
  Just (ExportSymbolAll n)  -> Just n
  _                         -> Nothing

-- | Borrow "Hypha.Source.Parser"'s occurrence rendering so an export
-- list entry is spelled exactly as the declaration it refers to.
wrapped :: LIEWrappedName GhcPs -> SymbolName
wrapped = SymbolName . Parser.renderRdrName . ieWrappedName . unLoc

-- | The names a module exports.  A module without an explicit list
-- exports everything it declares.
interfaceExportedNames :: ModuleInterface -> [SymbolName]
interfaceExportedNames i = case miExports i of
  Nothing    -> declaredNames i
  Just items -> concatMap namesOf items
  where
    namesOf (ExportSymbol n subs) = n : subs
    namesOf (ExportSymbolAll n)   = [n]
    namesOf (ExportModule _)      = []

-- | The names a module declares itself, re-exports excluded.
declaredNames :: ModuleInterface -> [SymbolName]
declaredNames i =
  [ SymbolName n
  | d <- miDecls i
  , n <- Parser.declName d : Parser.declSiblings d
  ]

-- | The names a module's export list carries, read from its parse tree.
--
-- Its predecessor scraped the @module ... ( ... ) where@ header with a
-- hand-rolled scanner, which could not tell @Map(..)@ from @Map@ and
-- dropped the @module N@ re-export form entirely.  That scanner outlived
-- its replacement by two call sites, one of them the server's degraded
-- module page -- so the fallback view was populated by exactly the code
-- this module was written to remove.
exportedNamesIO
  :: Extensions.LanguageSettings
  -> FilePath
  -> Text
  -> IO (Either Parser.ParseError [SymbolName])
exportedNamesIO ls path src =
  fmap (fmap interfaceExportedNames) (parseInterfaceIO ls path src)
