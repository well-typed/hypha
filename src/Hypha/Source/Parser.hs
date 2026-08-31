{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
-- | Wrapper around @ghc-lib-parser@ that turns a Haskell source file
-- into a flat list of top-level 'Decl's keyed by symbol name.  Replaces
-- the hand-rolled line scanners in "Hypha.Source.Extract" and
-- "Hypha.Source.Locate" so multi-symbol signatures (@a, b :: T@),
-- operator declarations, GADT-style data declarations, and other
-- idioms beyond the @\<ident\> ::@ shape resolve correctly.
--
-- @ghc-lib-parser@ ships its own GHC sources, so the supported syntax
-- frontier is fixed by the @ghc-lib-parser-X.Y@ dependency, not by
-- the GHC that builds hypha — bumping GHC will not silently regress
-- what we can read.
module Hypha.Source.Parser
  ( Decl (..)
  , DeclKind (..)
  , renderDeclKind
  , ParseError (..)
  , parseErrorMessage
  , parseDecls
  , parseModuleDoc
  , parseModuleWith
  , findDecl
  , declSigText
  , declSigTextIn
  , declSigOrSliceIn
  , numberedLines
  , renderRdrName
  ) where

import Control.Exception (evaluate)
import Control.Exception.Safe (SomeException, displayException, try)
import Data.Foldable qualified as Foldable
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Text (Text)
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer qualified as SB
import GHC.Hs
import GHC.Parser.Lexer qualified as L
import GHC.Parser qualified as P
import GHC.Types.Name.Occurrence qualified as Occ
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Utils.Outputable (Outputable, SDoc, ppr, showSDocUnsafe)
import GHC.Data.Bag qualified as Bag
import GHC.Types.Error (errMsgSpan, getMessages)
import GHC.Types.SrcLoc

import Hypha.Source.Extensions qualified as Extensions
import Language.Preprocessor.Cpphs qualified as Cpphs
import System.IO.Unsafe (unsafePerformIO)

-- | A single top-level declaration extracted from a module.  When a
-- signature binds several names (@a, b :: T@) each name is its own
-- 'Decl' with the same 'declSiblings' / 'declSigLine', so a lookup
-- for either name returns the canonical record.
data Decl = Decl
  { declName       :: !Text
  , declSiblings   :: ![Text]
  , declKind       :: !DeclKind
  , declParent     :: !(Maybe Text)
    -- ^ The enclosing class or data type, for names that live inside a
    -- body (class methods, data constructors); 'Nothing' for a top-level
    -- declaration.  Lets a card say "method of @FromJSON@" and lets the
    -- @T(..)@ export form expand to its subordinates.
  , declSigLine    :: !(Maybe Int)
    -- ^ Start line of the @sym :: ...@ signature, 1-based.
  , declSigEndLine :: !(Maybe Int)
    -- ^ End line of the signature span (inclusive, 1-based).  Equal
    -- to 'declSigLine' for single-line signatures.  Lets callers
    -- re-slice the original source to recover the signature text
    -- with continuations intact.
  , declDefLine    :: !(Maybe Int)
  , declDefEndLine :: !(Maybe Int)
    -- ^ End line of the definition span (inclusive, 1-based).  For
    -- data\/class declarations this delimits the whole body so callers
    -- can slice the constructor\/method block out of the source.
  , declSignature  :: !(Maybe Text)
    -- ^ The declaration's signature, rendered from the parse tree rather
    -- than sliced out of the source.  A type has no comments in it, but
    -- a /span/ over the source does — GHC keeps per-argument Haddock as
    -- 'HsDocTy' nodes inside the type, and re-slicing the lines swept
    -- them up and collapsed the newlines, so @Text -- ^ Input text.@
    -- reached the user as though it were part of the type.  Rendered
    -- here, with those nodes removed, for the same reason 'declDoc'
    -- comes from the tree: the structured form is the one that is right.
    -- 'Nothing' for a declaration with no signature of its own.
  , declDoc        :: !(Maybe Text)
    -- ^ The Haddock documentation attached to this declaration, as
    -- rendered by GHC (comment markers already stripped, contiguous
    -- @-- |@ lines merged, non-doc comments dropped).  Populated from
    -- the parse tree's 'DocD' nodes, not by line scanning, so blank
    -- lines / stray comments / CPP between the doc and the declaration
    -- are handled exactly as Haddock handles them.
  }
  deriving stock (Show, Eq)

-- | What sort of top-level declaration a 'Decl' names.  Drives the
-- kind badges in the server UI and lets consumers separate types from
-- values without re-parsing the source.
data DeclKind
  = DkFunction
  | DkData
  | DkNewtype
  | DkClass
  | DkTypeSyn
  | DkTypeFamily
  | DkPatternSyn
  | DkForeign
  | DkClassMethod
    -- ^ A method of a class, anchored inside its body.
  | DkConstructor
    -- ^ A data constructor, anchored inside its type's body.
  | DkRecordField
    -- ^ A record field, anchored on its @field :: Type@ entry.  A field
    -- is a selector function in its own right — @getSum@, @appEndo@,
    -- @runReaderT@ — and is parented on the /type/ rather than on the
    -- constructor, so a field shared by several constructors is one
    -- declaration and @T(..)@ reaches it.
  deriving stock (Show, Eq)

-- | The wire spelling of a declaration kind.
--
-- Here rather than at each consumer, so the @symbol@ card and the server's
-- kind badges cannot drift apart.
renderDeclKind :: DeclKind -> Text
renderDeclKind = \case
  DkFunction     -> "function"
  DkData         -> "data"
  DkNewtype      -> "newtype"
  DkClass        -> "class"
  DkTypeSyn      -> "type"
  DkTypeFamily   -> "type-family"
  DkPatternSyn   -> "pattern"
  DkForeign      -> "foreign"
  DkClassMethod  -> "class-method"
  DkConstructor  -> "constructor"
  DkRecordField  -> "record-field"

-- | Carrier for any parser failure surfaced from @ghc-lib-parser@.
--
-- The message is GHC's own rendered diagnostic.  It used to be the
-- literal string @\"parse error\"@, which is what the server's module
-- page showed the user — a report that named neither the problem nor its
-- location.  'peUnknownExtensions' and 'peDiagnostics' carry the pragma
-- names GHC's flag table rejected and its complaints about the pragma
-- block: both are plausible causes of the failure, so neither is
-- dropped.
data ParseError = ParseError
  { peMessage           :: !Text
  , peLine              :: !(Maybe Int)
  , peUnknownExtensions :: ![Extensions.UnknownExtension]
  , peDiagnostics       :: ![Text]
  }
  deriving stock (Show, Eq)

-- | The rendered diagnostic.  Kept as a function so existing callers
-- that only want something printable need not know the record.
parseErrorMessage :: ParseError -> Text
parseErrorMessage = peMessage

-- | Parse @source@ as a Haskell module and return its top-level
-- declarations.  @path@ is used only as the source-span file name.
--
-- When the source carries CPP directives (@\{\-# LANGUAGE CPP #\-\}@,
-- @\#ifdef@, ...) @cpphs@ is run as a preprocessor first, and that step
-- does real IO: an @#include@ is resolved by reading the named file from
-- the including module's directory.  'unsafePerformIO' pins the result at
-- the boundary rather than pushing @IO@ through every caller, which is a
-- deliberate trade and not a claim that nothing happens.  What makes it
-- defensible is that 'parseModuleIO' catches the preprocessor's failures,
-- so this is total: it returns a 'ParseError', never a thrown 'ErrorCall'.
parseDecls :: FilePath -> Text -> Either ParseError [Decl]
parseDecls path source =
  unsafePerformIO (parseDeclsIO Extensions.defaultLanguageSettings path source)
{-# NOINLINE parseDecls #-}

-- | 'IO' variant of 'parseDecls' for callers that already live in
-- 'IO' and would prefer not to thread an 'unsafePerformIO' through
-- their stack.
parseDeclsIO
  :: Extensions.LanguageSettings -> FilePath -> Text -> IO (Either ParseError [Decl])
parseDeclsIO ls path source =
  fmap (fmap (\(_, _, ds) -> ds)) (parseModuleIO ls path source)

-- | Parse a module and return its Haddock header (the @-- |@ block
-- above the @module@ keyword, if any) alongside its top-level
-- declarations.  Both the header and each declaration's doc come from
-- the parse tree, so this is the single authoritative doc source — no
-- line scanning anywhere.
parseModuleDoc :: FilePath -> Text -> Either ParseError (Maybe Text, [Decl])
parseModuleDoc path source =
  fmap (\(_, hdr, ds) -> (hdr, ds))
       (parseModuleWith Extensions.defaultLanguageSettings path source)

-- | Parse a module and hand back the whole parse tree alongside the
-- header doc and declarations.  "Hypha.Source.Interface" needs the tree
-- itself (module name, export list, imports); everything else takes the
-- narrower views above.
parseModuleWith
  :: Extensions.LanguageSettings -> FilePath -> Text
  -> Either ParseError (HsModule GhcPs, Maybe Text, [Decl])
parseModuleWith ls path source = unsafePerformIO (parseModuleIO ls path source)
{-# NOINLINE parseModuleWith #-}

parseModuleIO
  :: Extensions.LanguageSettings -> FilePath -> Text
  -> IO (Either ParseError (HsModule GhcPs, Maybe Text, [Decl]))
parseModuleIO ls path source = do
  ePre <- preprocess
  case ePre of
    Left  e            -> pure (Left e)
    Right preprocessed -> parsePreprocessed preprocessed
  where
   -- cpphs reports @#error@ and an unparseable @#if@ by calling 'error'
   -- from pure code, so the failure escapes the 'Either' its type
   -- advertises.  Catching it here is what makes every entry point below
   -- total, including the 'unsafePerformIO' ones: without it, @hypha
   -- symbol@ on a module guarded by @#error "CURRENT_PACKAGE_KEY
   -- undefined"@ aborted with a raw 'ErrorCall' instead of a
   -- 'Hypha.Error.HyphaError'.
   --
   -- 'Control.Exception.Safe.try' rethrows asynchronous exceptions, so a
   -- timed-out server request still dies rather than being reported as a
   -- broken module.
   -- 'evaluate' is what makes the 'try' above actually catch: cpphs
   -- hands back a lazy 'String', so @Text.pack <$> runCpphs@ returns a
   -- thunk and the 'error' inside it fires wherever the text is first
   -- demanded — outside this handler.  Forcing the 'Text' here consumes
   -- the whole string, so the failure lands in the 'Either'.
   preprocess
     | not (needsCpp source) = pure (Right source)
     | otherwise = do
         out <- try (evaluate . Text.pack
                       =<< Cpphs.runCpphs (cpphsOpts (Extensions.lsCpp ls))
                             path (Text.unpack source))
         pure $ case out of
           Right t                   -> Right t
           Left (e :: SomeException) -> Left ParseError
             { peMessage = "the C preprocessor rejected this module: "
                             <> firstLine (Text.pack (displayException e))
             , peLine              = Nothing
             , peUnknownExtensions = []
             , peDiagnostics       = []
             }

   firstLine = Text.strip . Text.takeWhile (/= '\n')

   -- The module states its own requirements; read them rather than
   -- guessing at a whitelist (see "Hypha.Source.Extensions").  Pragmas
   -- are read from the *preprocessed* text so a pragma inside a live
   -- @#if@ branch counts.
   parsePreprocessed preprocessed = do
     scan <- Extensions.scanPragmas path preprocessed
     let (exts, unknown) =
           Extensions.resolveExtensions ls (Extensions.psExtensionNames scan)
         buf  = SB.stringToStringBuffer (Text.unpack preprocessed)
         loc  = mkRealSrcLoc (mkFastString path) 1 1
         st   = L.initParserState (Extensions.parserOptsFor exts) buf loc
     pure $ case L.unP P.parseModule st of
       L.POk _ (L _ hsMod) ->
         Right (hsMod, moduleHeaderDoc hsMod, declsFromModule hsMod)
       L.PFailed st' ->
         Left (parseFailure unknown (Extensions.psDiagnostics scan) st')

-- | Turn a failed parser state into our typed error, keeping GHC's own
-- diagnostic and the line it points at.
parseFailure
  :: [Extensions.UnknownExtension] -> [Text] -> L.PState -> ParseError
parseFailure unknown diags st =
  let msgs   = L.getPsErrorMessages st
      firstD = listToMaybe (Bag.bagToList (getMessages msgs))
  in ParseError
       { peMessage = case Extensions.renderDiagnostics msgs of
           (m : _) -> m
           []      -> "parse error"
       , peLine = do
           d <- firstD
           case errMsgSpan d of
             RealSrcSpan s _ -> Just (srcSpanStartLine s)
             _               -> Nothing
       , peUnknownExtensions = unknown
       , peDiagnostics       = diags
       }

-- | The module-level Haddock header, as rendered by GHC.
moduleHeaderDoc :: HsModule GhcPs -> Maybe Text
moduleHeaderDoc m = docTextOf <$> hsmodHaddockModHeader (hsmodExt m)

-- | Cheap pre-flight check: only invoke cpphs when the source
-- actually contains CPP directives.  Most Hackage modules don't, and
-- the preprocessor pass is non-trivial.
needsCpp :: Text -> Bool
needsCpp src =
     "CPP" `Text.isInfixOf` pragmaHead
  || any (`Text.isInfixOf` src) directives
  where
    -- @{-# LANGUAGE CPP #-}@ is one spelling; @{-# LANGUAGE
    -- ScopedTypeVariables, CPP #-}@ is another, and matching the literal
    -- @{-# LANGUAGE CPP@ missed it.  Pragmas precede the module header,
    -- so there is no need to scan a 5000-line file for one.
    pragmaHead = Text.take 4096 src

    -- @#ifdef@ and @#ifndef@ need no probes of their own: both start
    -- with @#if@.
    directives = ["\n#if", "\n#define", "\n#include"]

-- | cpphs configuration: behave like ghc -E, expand the conditional
-- branches reachable under no externally-supplied symbol table, and
-- keep blank lines in place so line numbers in the produced AST
-- still match the original source.
cpphsOpts :: Extensions.CppEnv -> Cpphs.CpphsOptions
cpphsOpts env = Cpphs.defaultCpphsOptions
  { -- cabal generates a @cabal_macros.h@ and passes it to every CPP
    -- invocation; hypha synthesises the same thing from the plan and
    -- passes it the same way.  Without it @__GLASGOW_HASKELL__@ and
    -- @MIN_VERSION_*@ are undefined, and an undefined macro is zero, so
    -- every version gate resolves to its oldest branch.
    Cpphs.preInclude = maybe [] pure (Extensions.cppPreInclude env)
    -- @#include@ of a package's own header fails without a search path,
    -- and a failed include takes the whole module out of the index.
  , Cpphs.includes   = Extensions.cppIncludeDirs env
  , Cpphs.boolopts = Cpphs.defaultBoolOptions
      { Cpphs.locations = True     -- emit @{-# LINE #-}@ pragmas so the
                                   -- parser's @usePosPrags@ option tracks
                                   -- original-source line numbers under
                                   -- @\#ifdef@-driven line drift
      , Cpphs.hashline  = False    -- pragma form, not @#line@
      , Cpphs.stripEol  = True
      , Cpphs.stripC89  = True
      , Cpphs.lang      = True     -- Haskell mode (vs C)
      , Cpphs.warnings  = False
      }
  }

-- | First decl whose name matches @q@.  Direct name matches win over
-- sibling matches: a query for @sourceListC@ in a module that declares
-- @sourceList, sourceListC :: T@ must return the decl whose
-- @declName == sourceListC@, not the @sourceList@ decl that lists
-- @sourceListC@ as a sibling.
findDecl :: Text -> [Decl] -> Maybe Decl
findDecl q ds =
  case listToMaybe (filter (\d -> declName d == q) ds) of
    Just d  -> Just d
    Nothing -> listToMaybe (filter (\d -> q `elem` declSiblings d) ds)

-- | Re-slice the original source text to recover the signature
-- string for a 'Decl' (with continuation lines joined into a single
-- whitespace-collapsed 'Text').  Returns 'Nothing' when the decl has
-- no signature.  The signature anchor is reliable across CPP because
-- the parser is configured with @usePosPrags = True@ and cpphs emits
-- @\{\-# LINE #\-\}@ pragmas: 'declSigLine' indexes into the /original/
-- source bytes you pass to this function, not into the post-cpphs
-- ones.
declSigText :: Text -> Decl -> Maybe Text
declSigText = declSigTextIn . numberedLines

-- | 'declSigText' over lines already numbered, for callers slicing many
-- declarations out of one module: numbering the source per declaration is
-- what made the batch path quadratic.
-- The rendered form wins when there is one: it is the declaration's type
-- as the parser understood it.  The slice remains for declarations that
-- have source worth showing and no type node to render — a data
-- constructor, and anything the renderer does not yet cover.
declSigTextIn :: [(Int, Text)] -> Decl -> Maybe Text
declSigTextIn ls d = case declSignature d of
  Just rendered -> Just rendered
  Nothing       -> sliceJoined ls (declSigLine d) (declSigEndLine d)

-- | 'declSigTextIn', falling back to the definition span for a data
-- constructor — the one kind that has real source worth showing and no
-- @::@ line of its own.  Without the fallback every constructor row in
-- the index carried an empty signature, which the search list rendered
-- as a blank column and @hypha lookup@ emitted as @sig: \"\"@.
declSigOrSliceIn :: [(Int, Text)] -> Decl -> Maybe Text
declSigOrSliceIn ls d = case declSigTextIn ls d of
  Just t  -> Just t
  Nothing
    | declKind d == DkConstructor ->
        sliceJoined ls (declDefLine d) (declDefEndLine d)
    | otherwise -> Nothing

-- | The numbered lines @[start .. end]@ joined into one, whitespace
-- collapsed.  @end@ defaults to @start@ and never precedes it.
sliceJoined :: [(Int, Text)] -> Maybe Int -> Maybe Int -> Maybe Text
sliceJoined ls mStart mEnd = do
  startLn <- mStart
  let endLn = case mEnd of
                Just e  -> max startLn e
                Nothing -> startLn
      slice = [ t | (i, t) <- ls, i >= startLn, i <= endLn ]
  case slice of
    []    -> Nothing
    parts -> Just (Text.unwords (filter (not . Text.null) (map Text.strip parts)))

-- | A module's lines, 1-based, the form every line-slicing helper takes.
numberedLines :: Text -> [(Int, Text)]
numberedLines = zip [1 :: Int ..] . Text.lines

-- Internals --------------------------------------------------------


declsFromModule :: HsModule GhcPs -> [Decl]
declsFromModule m =
  -- Each top-level node contributes at most a sig OR a binding, but a
  -- symbol typically has both.  Collapse them by name so 'findDecl'
  -- returns one record carrying both line numbers (sig + def) rather
  -- than whichever appeared first.  Order of first appearance is
  -- preserved.
  mergeByName (associateDocs (hsmodDecls m))

-- | Walk the top-level nodes in source order, stapling each
-- @DocCommentNext@ (@-- |@) block onto the declaration that follows it
-- and each @DocCommentPrev@ (@-- ^@) block onto the declaration that
-- precedes it.  GHC has already dropped non-doc comments and merged
-- each contiguous doc block into a single node, so association is a
-- plain left fold — blank lines, stray comments, and CPP @{-# LINE #-}@
-- pragmas between a doc and its declaration are invisible here just as
-- they are to Haddock itself.
associateDocs :: [LHsDecl GhcPs] -> [Decl]
associateDocs = go Nothing []
  where
    go _       acc []          = reverse acc
    go pending acc (ld : rest) = case unLoc ld of
      DocD _ (DocCommentNext d) -> go (pending `appendDoc` Just (docTextOf d)) acc rest
      DocD _ (DocCommentPrev d) -> go pending (attachPrev (docTextOf d) acc) rest
      -- Named chunks and section headers are not a declaration's doc.
      DocD _ _                  -> go pending acc rest
      -- Any real top-level node consumes the pending @-- |@ block: a
      -- doc binds to the declaration immediately following it, even one
      -- we don't emit (e.g. an instance), so the pending doc is cleared
      -- either way.
      _ -> let ds = nodeDecls pending ld
           in go Nothing (reverse ds ++ acc) rest

    -- A @-- ^@ block names the declaration it follows, which is the
    -- preceding /node/ — the type, class or function — not the last
    -- constructor or method that node's body happened to contribute.
    -- Body members are the ones carrying a parent, so skipping past them
    -- lands on the node itself: @data Colour = Red | Green@ followed by
    -- @-- ^ A colour.@ documents @Colour@, and used to document @Green@.
    -- (A body member's own @-- ^@ never reaches here: it lives inside the
    -- body and is associated by 'classMethods'.)
    attachPrev txt acc = case break isTopLevel acc of
      (_,    [])       -> acc
      (subs, d : rest) ->
        subs ++ (d { declDoc = declDoc d `appendDoc` Just txt } : rest)

    isTopLevel d = declParent d == Nothing

-- | The declarations a top-level node contributes, with the node's
-- pending @-- |@ doc applied.  A @-- |@ block before a type or class
-- names the type or class itself, not every declaration its body
-- contains: stapling it onto the methods and constructors too would hand
-- the type's prose to its members.  Everything else (a multi-name
-- signature, a function) belongs to one declaration, so its doc goes to
-- each sibling.
nodeDecls :: Maybe Text -> LHsDecl GhcPs -> [Decl]
nodeDecls pending ld = case unLoc ld of
  TyClD{} -> case declsFromTop ld of
    []         -> []
    (d : rest) -> d { declDoc = pending } : rest
  _ -> [ dcl { declDoc = pending } | dcl <- declsFromTop ld ]

-- | Combine two optional doc blocks, joining with a blank line so a
-- @-- |@ / @-- ^@ pair on the same binding reads as two paragraphs.
appendDoc :: Maybe Text -> Maybe Text -> Maybe Text
appendDoc Nothing    y          = y
appendDoc x          Nothing    = x
appendDoc (Just a)   (Just b)   = Just (a <> "\n\n" <> b)

-- | Render a located Haddock doc to plain text via GHC's own renderer.
docTextOf :: LHsDoc GhcPs -> Text
docTextOf = Text.pack . renderHsDocString . hsDocString . unLoc

mergeByName :: [Decl] -> [Decl]
mergeByName = go []
  where
    go acc []     = reverse acc
    go acc (d:ds) = case break (sameEntity d) acc of
      (_,    [])      -> go (d : acc) ds
      (pre, e : post) -> go (reverse pre ++ merge e d : post) ds

    -- Equal names are not enough: Haskell keeps type and value names in
    -- separate namespaces, so a constructor can share a name with an
    -- unrelated type in the same module —
    --
    -- >  type Object = Int
    -- >  data Value  = Object Object   -- aeson's own shape
    --
    -- and folding those two together loses one of them outright (the
    -- @Object@ constructor vanished into the synonym's decl, or the
    -- synonym inherited the constructor's kind, span and @v:@ anchor,
    -- depending on which came first).  Two decls are the same entity
    -- when their names agree /and/ their parents relate them: both
    -- top-level, or a member of the very type it is named after.
    sameEntity d e = declName e == declName d && relatedParents d e

    relatedParents d e = case (declParent d, declParent e) of
      (Nothing, Nothing) -> True
      -- @data Wrap = Wrap Int@: the constructor is the type's own, so the
      -- two are one declaration seen twice and merge into a single entry
      -- whose span covers the whole thing.
      (Just p,  Nothing) -> p == declName e
      (Nothing, Just q)  -> q == declName d
      -- A method's signature and its default body name the same class.
      (Just p,  Just q)  -> p == q

    -- Merging is by name, as before.  A data constructor sharing its
    -- type's name (@data Wrap = Wrap Int@) merges into the type's decl,
    -- which is what the page and the index should show for @Wrap@ — one
    -- entry whose span covers the whole declaration.  Constructors with
    -- their own names (@Red@, @Green@) are separate decls and never meet.
    merge a b = Decl
      { declName       = declName a
      , declSiblings   = declSiblings a `orEmpty` declSiblings b
        -- A bare 'DkFunction' is the least informative kind (every
        -- signature defaults to it), so any more specific kind from
        -- the other half of the merge wins.
      , declKind       = if declKind a == DkFunction then declKind b else declKind a
      , declParent     = parentOf (declParent a) (declParent b)
      , declSigLine    = declSigLine a    `orFirst` declSigLine b
      , declSigEndLine = declSigEndLine a `orFirst` declSigEndLine b
      , declDefLine    = declDefLine a    `orFirst` declDefLine b
      , declDefEndLine = declDefEndLine a `orFirst` declDefEndLine b
        -- The signature half of the merge is the one that has a type to
        -- render; the definition half never does.
      , declSignature  = declSignature a  `orFirst` declSignature b
      , declDoc        = declDoc a        `appendDoc` declDoc b
      }

    -- A merged type and its same-named constructor is the type: parent
    -- wins only when both halves carry one (a method's signature and its
    -- default body name the same class).
    parentOf (Just x) (Just _) = Just x
    parentOf _        _        = Nothing

    orFirst (Just x) _ = Just x
    orFirst Nothing  y = y

    orEmpty [] ys = ys
    orEmpty xs _  = xs

declsFromTop :: LHsDecl GhcPs -> [Decl]
declsFromTop ld = case unLoc ld of
  SigD _ (TypeSig _ lnames ty) ->
    let names    = map (rdrText . unLoc) lnames
        (mS, mE) = locLines ld
        rendered = renderSigWcType names ty
    in [ (sigDecl nm names mS mE DkFunction Nothing)
           { declSignature = Just rendered } | nm <- names ]
  SigD _ (PatSynSig _ lnames ty) ->
    let names    = map (rdrText . unLoc) lnames
        (mS, mE) = locLines ld
        rendered = renderSigType names ty
    in [ (sigDecl nm names mS mE DkPatternSyn Nothing)
           { declSignature = Just rendered } | nm <- names ]
  ValD _ (FunBind { fun_id = L _ rn }) ->
    let (mS, mE) = locLines ld
    in [ defDecl (rdrText rn) mS mE DkFunction Nothing ]
  ValD _ (PatSynBind _ (PSB { psb_id = L _ rn })) ->
    let (mS, mE) = locLines ld
    in [ defDecl (rdrText rn) mS mE DkPatternSyn Nothing ]
  TyClD _ tc ->
    let (mS, mE) = locLines ld
    in case tc of
         SynDecl { tcdLName = L _ rn } ->
           [ defDecl (rdrText rn) mS mE DkTypeSyn Nothing ]
         FamDecl { tcdFam = FamilyDecl { fdLName = L _ rn } } ->
           [ defDecl (rdrText rn) mS mE DkTypeFamily Nothing ]
         ClassDecl { tcdLName = L _ rn, tcdSigs = sigs, tcdMeths = meths
                   , tcdDocs = docs } ->
           let cls = rdrText rn
           in defDecl cls mS mE DkClass Nothing
                : classMethods cls sigs meths docs
         DataDecl { tcdLName = L _ rn, tcdDataDefn = defn } ->
           let k = case dd_cons defn of
                     NewTypeCon {} -> DkNewtype
                     _             -> DkData
           in defDecl (rdrText rn) mS mE k Nothing
                : constructorsOf (rdrText rn) defn
  ForD _ (ForeignImport { fd_name = L _ rn }) ->
    let (mS, mE) = locLines ld
    in [ defDecl (rdrText rn) mS mE DkForeign Nothing ]
  _ -> []

-- | A signature declaration: one 'Decl' per bound name, sharing the
-- signature's span and sibling list.
sigDecl :: Text -> [Text] -> Maybe Int -> Maybe Int -> DeclKind -> Maybe Text -> Decl
sigDecl nm names mS mE k parent = Decl
  { declName       = nm
  , declSiblings   = filter (/= nm) names
  , declKind       = k
  , declParent     = parent
  , declSigLine    = mS
  , declSigEndLine = mE
  , declDefLine    = Nothing
  , declDefEndLine = Nothing
  , declSignature  = Nothing
  , declDoc        = Nothing
  }

-- | @name[, name...] :: type@, rendered from the parse tree.
--
-- The name list is reproduced as written so a comma-grouped signature
-- still reads as one — @sourceList, sourceListC :: Monad m => [a] -> m ()@
-- is what the source says and what a reader expects to see.
renderSigWith :: (a -> SDoc) -> [Text] -> a -> Text
renderSigWith pp names ty =
  Text.intercalate ", " names <> " :: " <> renderPp' (pp ty)

-- | 'ppr' lays a long type out over several lines; a signature is one
-- line everywhere it is consumed (a YAML scalar, a table cell, a search
-- row), so the layout is flattened here rather than at each of them.
renderPp' :: SDoc -> Text
renderPp' = Text.unwords . Text.words . Text.pack . showSDocUnsafe

renderPp :: Outputable a => a -> Text
renderPp = renderPp' . ppr

-- | A whole-signature type with its per-argument docs removed.
renderSigWcType :: [Text] -> LHsSigWcType GhcPs -> Text
renderSigWcType names = renderSigWith ppr names . stripSigWcDocs

-- | 'renderSigWcType' for the bare 'LHsSigType' a class method carries.
renderSigType :: [Text] -> LHsSigType GhcPs -> Text
renderSigType names = renderSigWith ppr names . stripSigDocs

-- | A field's type, which is an 'LHsType' with no @forall@ wrapper.
renderFieldType :: [Text] -> LHsType GhcPs -> Text
renderFieldType names = renderSigWith ppr names . stripDocTy

-- | A data constructor rendered from the tree.
--
-- A constructor has no type of its own to render, so it used to slice its
-- source span — which is why @| BoxedRep Levity -- ^ boxed; represented
-- by a pointer@ reached the user as a signature, leading punctuation and
-- all.  Its Haddock is already carried on 'declDoc', so reproducing it
-- here was duplication as well as corruption.
renderConDecl :: ConDecl GhcPs -> Text
renderConDecl = renderPp . stripConDocs

-- | Every doc a constructor can carry: its own, its fields', and any
-- buried in its argument or result types.
stripConDocs :: ConDecl GhcPs -> ConDecl GhcPs
stripConDocs c = case c of
  ConDeclH98{}  -> c { con_doc    = Nothing
                     , con_args   = stripH98Details (con_args c)
                     }
  ConDeclGADT{} -> c { con_doc    = Nothing
                     , con_g_args = stripGadtDetails (con_g_args c)
                     , con_res_ty = stripDocTy (con_res_ty c)
                     }

stripH98Details :: HsConDeclH98Details GhcPs -> HsConDeclH98Details GhcPs
stripH98Details = \case
  PrefixCon tys args -> PrefixCon tys (map stripScaled args)
  InfixCon a b       -> InfixCon (stripScaled a) (stripScaled b)
  RecCon flds        -> RecCon (fmap (map stripConField) flds)

stripGadtDetails :: HsConDeclGADTDetails GhcPs -> HsConDeclGADTDetails GhcPs
stripGadtDetails = \case
  PrefixConGADT x args -> PrefixConGADT x (map stripScaled args)
  RecConGADT x flds    -> RecConGADT x (fmap (map stripConField) flds)

stripScaled :: HsScaled GhcPs (LHsType GhcPs) -> HsScaled GhcPs (LHsType GhcPs)
stripScaled (HsScaled m t) = HsScaled m (stripDocTy t)

stripConField :: LConDeclField GhcPs -> LConDeclField GhcPs
stripConField (L l f) =
  L l f { cd_fld_type = stripDocTy (cd_fld_type f), cd_fld_doc = Nothing }

stripSigWcDocs :: LHsSigWcType GhcPs -> LHsSigWcType GhcPs
stripSigWcDocs (HsWC x body) = HsWC x (stripSigDocs body)

stripSigDocs :: LHsSigType GhcPs -> LHsSigType GhcPs
stripSigDocs (L l (HsSig x bndrs body)) = L l (HsSig x bndrs (stripDocTy body))

-- | Remove every 'HsDocTy' node from a type.
--
-- A per-argument Haddock comment is not part of the type; GHC parks it in
-- the tree so Haddock can find it, and 'ppr' faithfully prints it back
-- out as @-- |@.  Removing the nodes is exact where a textual strip is
-- not: @(a --> b)@ is an operator, and everything after its @--@ is the
-- rest of the type rather than a comment.
--
-- The catch-all covers the leaf types, which cannot contain one.  A
-- composite form not listed here keeps its docs — the behaviour before
-- this function existed — rather than losing the type; a promoted tuple
-- is deliberately in that group, because its constructor's arity differs
-- across the @ghc-lib-parser@ pins the three supported compilers use and
-- a Haddock comment inside one is not worth a CPP branch.
stripDocTy :: LHsType GhcPs -> LHsType GhcPs
stripDocTy (L l ty) = case ty of
  HsDocTy _ inner _        -> stripDocTy inner
  HsForAllTy x tele body   -> L l (HsForAllTy x tele (stripDocTy body))
  HsQualTy x ctx body      -> L l (HsQualTy x (fmap (map stripDocTy) ctx)
                                             (stripDocTy body))
  HsFunTy x arr a b        -> L l (HsFunTy x arr (stripDocTy a) (stripDocTy b))
  HsParTy x a              -> L l (HsParTy x (stripDocTy a))
  HsAppTy x a b            -> L l (HsAppTy x (stripDocTy a) (stripDocTy b))
  HsAppKindTy x a k        -> L l (HsAppKindTy x (stripDocTy a) k)
  HsListTy x a             -> L l (HsListTy x (stripDocTy a))
  HsTupleTy x srt as       -> L l (HsTupleTy x srt (map stripDocTy as))
  HsSumTy x as             -> L l (HsSumTy x (map stripDocTy as))
  HsOpTy x p a op b        -> L l (HsOpTy x p (stripDocTy a) op (stripDocTy b))
  HsKindSig x a k          -> L l (HsKindSig x (stripDocTy a) (stripDocTy k))
  HsBangTy x b a           -> L l (HsBangTy x b (stripDocTy a))
  HsIParamTy x n a         -> L l (HsIParamTy x n (stripDocTy a))
  HsExplicitListTy x p as  -> L l (HsExplicitListTy x p (map stripDocTy as))
  _                        -> L l ty

-- | A definition declaration.
defDecl :: Text -> Maybe Int -> Maybe Int -> DeclKind -> Maybe Text -> Decl
defDecl nm mS mE k parent = Decl
  { declName       = nm
  , declSiblings   = []
  , declKind       = k
  , declParent     = parent
  , declSigLine    = Nothing
  , declSigEndLine = Nothing
  , declDefLine    = mS
  , declDefEndLine = mE
  , declSignature  = Nothing
  , declDoc        = Nothing
  }

-- | The class body's methods as declarations in their own right: every
-- @ClassOpSig@ (ordinary or generic @default@) and every default-method
-- binding, parented on the class so the @C(..)@ export form can expand
-- to them and a card can say "method of @C@".  Doc comments inside the
-- class body are stapled positionally, exactly as the top-level pass
-- staples the module's.
classMethods :: Text -> [LSig GhcPs] -> LHsBinds GhcPs -> [LDocDecl GhcPs] -> [Decl]
classMethods cls sigs meths docs = go Nothing [] (sortOn fst items)
  where
    items =
      [ (declStartLine d, BodyDecl d) | s <- sigs, d <- methodSigDecls cls s ]
        ++ [ (declStartLine d, BodyDecl d)
           | b <- Foldable.toList meths, Just d <- [methodBindDecl cls b] ]
        ++ [ (docLine ld, BodyNextDoc (docTextOf t))
           | ld <- docs, DocCommentNext t <- [unLoc ld] ]
        ++ [ (docLine ld, BodyPrevDoc (docTextOf t))
           | ld <- docs, DocCommentPrev t <- [unLoc ld] ]

    -- Sigs and binds anchor at their own span; docs sort by theirs.  A
    -- zero line is impossible for real source, so it only ties items the
    -- parser did not anchor — order among those is stable and irrelevant.
    declStartLine d = case declSigLine d of
      Just l  -> l
      Nothing -> case declDefLine d of
        Just l  -> l
        Nothing -> 0

    docLine ld = case locA (getLoc ld) of
      RealSrcSpan r _ -> srcSpanStartLine r
      _               -> 0

    go _       acc []          = reverse acc
    go pending acc ((_, BodyDecl d) : rest) =
      go Nothing (d { declDoc = pending } : acc) rest
    go pending acc ((_, BodyNextDoc t) : rest) =
      go (pending `appendDoc` Just t) acc rest
    go pending acc ((_, BodyPrevDoc t) : rest) =
      go pending (attachPrev t acc) rest

    attachPrev _   []       = []
    attachPrev txt (d : ds) = d { declDoc = declDoc d `appendDoc` Just txt } : ds

-- | A class body item reduced to what doc association needs.
data BodyItem
  = BodyDecl !Decl
  | BodyNextDoc !Text
  | BodyPrevDoc !Text

-- | A method's type signature, ordinary or generic @default@.
methodSigDecls :: Text -> LSig GhcPs -> [Decl]
methodSigDecls cls sigL = case unLoc sigL of
  ClassOpSig _ _ lnames ty ->
    let names    = map (rdrText . unLoc) lnames
        (mS, mE) = spanLines (locA (getLoc sigL))
        rendered = renderSigType names ty
    in [ (sigDecl nm names mS mE DkClassMethod (Just cls))
           { declSignature = Just rendered } | nm <- names ]
  _ -> []

-- | A default-method binding inside the class body.
methodBindDecl :: Text -> LHsBind GhcPs -> Maybe Decl
methodBindDecl cls b = case unLoc b of
  FunBind { fun_id = L _ rn } ->
    let (mS, mE) = spanLines (locA (getLoc b))
    in Just (defDecl (rdrText rn) mS mE DkClassMethod (Just cls))
  _ -> Nothing

-- | A data type's constructors and record fields, each a declaration
-- parented on the type.  Both carry whatever Haddock GHC attached to
-- them (@-- ^@ after a constructor, @-- ^@ after a field), which is the
-- only doc a constructor ever has.
constructorsOf :: Text -> HsDataDefn GhcPs -> [Decl]
constructorsOf tyName defn =
  [ d
  | lcon <- conDeclsOf defn
  , let con      = unLoc lcon
        (mS, mE) = spanLines (locA (getLoc lcon))
  , d <- [ (defDecl nm mS mE DkConstructor (Just tyName))
             { declSignature = Just (renderConDecl con)
             , declDoc       = fmap docTextOf (conDoc con)
             }
         | nm <- constructorNames con
         ]
           ++ fieldsOf tyName con
  ]

conDeclsOf :: HsDataDefn GhcPs -> [LConDecl GhcPs]
conDeclsOf defn = case dd_cons defn of
  NewTypeCon c     -> [c]
  DataTypeCons _ cs -> cs

constructorNames :: ConDecl GhcPs -> [Text]
constructorNames = \case
  ConDeclGADT { con_names = names } -> map (rdrText . unLoc) (NE.toList names)
  ConDeclH98  { con_name = L _ nm } -> [rdrText nm]

conDoc :: ConDecl GhcPs -> Maybe (LHsDoc GhcPs)
conDoc = \case
  ConDeclGADT { con_doc = d } -> d
  ConDeclH98  { con_doc = d } -> d

-- | A constructor's record fields as declarations of their own, anchored
-- on the @field :: Type@ entry so the field reads as the selector it is.
-- A field group binding several names (@x, y :: Int@) shares one span and
-- one sibling list, exactly as a multi-name top-level signature does.
fieldsOf :: Text -> ConDecl GhcPs -> [Decl]
fieldsOf tyName con =
  [ (sigDecl nm names mS mE DkRecordField (Just tyName))
      { declSignature = Just rendered, declDoc = fmap docTextOf mdoc }
  | lfld <- recordFieldsOf con
  , ConDeclField { cd_fld_names = lnames, cd_fld_type = fty
                , cd_fld_doc = mdoc } <- [unLoc lfld]
  , let (mS, mE) = spanLines (locA (getLoc lfld))
        names    = [ rdrText rn
                   | lname <- lnames
                   , FieldOcc { foLabel = L _ rn } <- [unLoc lname]
                   ]
        rendered = renderFieldType names fty
  , nm <- names
  ]

recordFieldsOf :: ConDecl GhcPs -> [LConDeclField GhcPs]
recordFieldsOf = \case
  ConDeclH98  { con_args   = RecCon flds }      -> unLoc flds
  ConDeclGADT { con_g_args = RecConGADT _ flds } -> unLoc flds
  _                                             -> []

locLines :: LHsDecl GhcPs -> (Maybe Int, Maybe Int)
locLines ld = spanLines (locA (getLoc ld))

-- | The start and end lines of a source span.
spanLines :: SrcSpan -> (Maybe Int, Maybe Int)
spanLines s = case s of
  RealSrcSpan r _ -> (Just (srcSpanStartLine r), Just (srcSpanEndLine r))
  _               -> (Nothing, Nothing)

rdrText :: RdrName -> Text
rdrText = Text.pack . Occ.occNameString . rdrNameOcc

-- | Public alias for 'rdrText'.  "Hypha.Source.Interface" renders export
-- and import list entries and must spell names exactly as the decls do,
-- so it borrows this rather than growing a second implementation.
renderRdrName :: RdrName -> Text
renderRdrName = rdrText
