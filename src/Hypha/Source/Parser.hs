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
  , ParseError (..)
  , parseErrorMessage
  , parseDecls
  , parseModuleDoc
  , parseModuleWith
  , findDecl
  , declSigText
  , declSigTextIn
  , numberedLines
  , renderRdrName
  ) where

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
  deriving stock (Show, Eq)

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
   preprocess
     | not (needsCpp source) = pure (Right source)
     | otherwise = do
         out <- try (Text.pack <$> Cpphs.runCpphs cpphsOpts path
                                     (Text.unpack source))
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
cpphsOpts :: Cpphs.CpphsOptions
cpphsOpts = Cpphs.defaultCpphsOptions
  { Cpphs.boolopts = Cpphs.defaultBoolOptions
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
declSigTextIn :: [(Int, Text)] -> Decl -> Maybe Text
declSigTextIn ls d = do
  startLn <- declSigLine d
  let endLn = case declSigEndLine d of
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

    attachPrev _   []       = []
    attachPrev txt (d : ds) = d { declDoc = declDoc d `appendDoc` Just txt } : ds

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
    go acc (d:ds) = case break ((== declName d) . declName) acc of
      (_,    [])      -> go (d : acc) ds
      (pre, e : post) -> go (reverse pre ++ merge e d : post) ds

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
  SigD _ (TypeSig _ lnames _ty) ->
    let names    = map (rdrText . unLoc) lnames
        (mS, mE) = locLines ld
    in [ sigDecl nm names mS mE DkFunction Nothing | nm <- names ]
  SigD _ (PatSynSig _ lnames _ty) ->
    let names    = map (rdrText . unLoc) lnames
        (mS, mE) = locLines ld
    in [ sigDecl nm names mS mE DkPatternSyn Nothing | nm <- names ]
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
  , declDoc        = Nothing
  }

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
  ClassOpSig _ _ lnames _ty ->
    let names    = map (rdrText . unLoc) lnames
        (mS, mE) = spanLines (locA (getLoc sigL))
    in [ sigDecl nm names mS mE DkClassMethod (Just cls) | nm <- names ]
  _ -> []

-- | A default-method binding inside the class body.
methodBindDecl :: Text -> LHsBind GhcPs -> Maybe Decl
methodBindDecl cls b = case unLoc b of
  FunBind { fun_id = L _ rn } ->
    let (mS, mE) = spanLines (locA (getLoc b))
    in Just (defDecl (rdrText rn) mS mE DkClassMethod (Just cls))
  _ -> Nothing

-- | A data type's constructors, each a declaration parented on the type.
constructorsOf :: Text -> HsDataDefn GhcPs -> [Decl]
constructorsOf tyName defn =
  [ defDecl nm mS mE DkConstructor (Just tyName)
  | con <- conDeclsOf defn
  , nm <- constructorNames (unLoc con)
  , let (mS, mE) = spanLines (locA (getLoc con))
  ]

conDeclsOf :: HsDataDefn GhcPs -> [LConDecl GhcPs]
conDeclsOf defn = case dd_cons defn of
  NewTypeCon c     -> [c]
  DataTypeCons _ cs -> cs

constructorNames :: ConDecl GhcPs -> [Text]
constructorNames = \case
  ConDeclGADT { con_names = names } -> map (rdrText . unLoc) (NE.toList names)
  ConDeclH98  { con_name = L _ nm } -> [rdrText nm]

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
