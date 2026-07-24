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
  , parseDecls
  , parseDeclsIO
  , parseModuleDoc
  , findDecl
  , declSigText
  ) where

import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Text (Text)
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer qualified as SB
import GHC.Hs
import GHC.LanguageExtensions qualified as LangExt
import GHC.Parser.Lexer qualified as L
import GHC.Parser qualified as P
import GHC.Types.Name.Occurrence qualified as Occ
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc
import GHC.Utils.Error (emptyDiagOpts)
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
  deriving stock (Show, Eq)

-- | Carrier for any parser failure surfaced from @ghc-lib-parser@.
newtype ParseError = ParseError { parseErrorMessage :: Text }
  deriving stock (Show, Eq)

-- | Parse @source@ as a Haskell module and return its top-level
-- declarations.  @path@ is used only as the source-span file name.
--
-- When the source carries CPP directives (@\{\-# LANGUAGE CPP #\-\}@,
-- @\#ifdef@, ...) @cpphs@ is run as a preprocessor first.  The whole
-- pipeline is pure: cpphs is normally @IO@ to resolve @#include@
-- directives, but we hold those off (@locations = False@,
-- @hashline   = False@) and feed it source bytes we already own, so
-- the @IO@ is artefactual.  We pin the purity at the boundary with
-- 'unsafePerformIO' rather than push @IO@ through every caller.
parseDecls :: FilePath -> Text -> Either ParseError [Decl]
parseDecls path source = unsafePerformIO (parseDeclsIO path source)
{-# NOINLINE parseDecls #-}

-- | 'IO' variant of 'parseDecls' for callers that already live in
-- 'IO' and would prefer not to thread an 'unsafePerformIO' through
-- their stack.
parseDeclsIO :: FilePath -> Text -> IO (Either ParseError [Decl])
parseDeclsIO path source = fmap (fmap snd) (parseModuleDocIO path source)

-- | Parse a module and return its Haddock header (the @-- |@ block
-- above the @module@ keyword, if any) alongside its top-level
-- declarations.  Both the header and each declaration's doc come from
-- the parse tree, so this is the single authoritative doc source — no
-- line scanning anywhere.
parseModuleDoc :: FilePath -> Text -> Either ParseError (Maybe Text, [Decl])
parseModuleDoc path source = unsafePerformIO (parseModuleDocIO path source)
{-# NOINLINE parseModuleDoc #-}

parseModuleDocIO :: FilePath -> Text -> IO (Either ParseError (Maybe Text, [Decl]))
parseModuleDocIO path source = do
  preprocessed <- if needsCpp source
    then Text.pack <$> Cpphs.runCpphs cpphsOpts path (Text.unpack source)
    else pure source
  let buf  = SB.stringToStringBuffer (Text.unpack preprocessed)
      loc  = mkRealSrcLoc (mkFastString path) 1 1
      opts = L.mkParserOpts
               enabledExtensions
               emptyDiagOpts
               []      -- supported langexts (only used for error messages)
               False   -- safeImports
               True    -- isHaddock — attach doc comments to the parse tree
               False   -- keep raw token stream
               True    -- honour @{-# LINE #-}@ pragmas
      st   = L.initParserState opts buf loc
  pure $ case L.unP P.parseModule st of
    L.POk _ (L _ hsMod) -> Right (moduleHeaderDoc hsMod, declsFromModule hsMod)
    L.PFailed _         -> Left (ParseError "parse error")

-- | The module-level Haddock header, as rendered by GHC.
moduleHeaderDoc :: HsModule GhcPs -> Maybe Text
moduleHeaderDoc m = docTextOf <$> hsmodHaddockModHeader (hsmodExt m)

-- | Cheap pre-flight check: only invoke cpphs when the source
-- actually contains CPP directives.  Most Hackage modules don't, and
-- the preprocessor pass is non-trivial.
needsCpp :: Text -> Bool
needsCpp src =
     "{-# LANGUAGE CPP" `Text.isInfixOf` src
  || "\n#if"    `Text.isInfixOf` src
  || "\n#ifdef" `Text.isInfixOf` src
  || "\n#ifndef" `Text.isInfixOf` src
  || "\n#define" `Text.isInfixOf` src
  || "\n#include" `Text.isInfixOf` src

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
declSigText source d = do
  startLn <- declSigLine d
  let endLn = case declSigEndLine d of
                Just e  -> max startLn e
                Nothing -> startLn
      ls    = zip [1 :: Int ..] (Text.lines source)
      slice = [ t | (i, t) <- ls, i >= startLn, i <= endLn ]
  case slice of
    []    -> Nothing
    parts -> Just (Text.unwords (filter (not . Text.null) (map Text.strip parts)))

-- Internals --------------------------------------------------------

-- | A generous bouquet of language extensions so we accept the long
-- tail of real-world Haskell without first parsing each file's
-- @LANGUAGE@ pragmas.  Most extensions only enable /semantics/ the
-- parser already accepts; the ones below are the ones with a real
-- /syntactic/ impact.
enabledExtensions :: EnumSet.EnumSet LangExt.Extension
enabledExtensions = EnumSet.fromList
  [ LangExt.BangPatterns
  , LangExt.DataKinds
  , LangExt.ExistentialQuantification
  , LangExt.FlexibleContexts
  , LangExt.FlexibleInstances
  , LangExt.GADTs
  , LangExt.KindSignatures
  , LangExt.LambdaCase
  , LangExt.MultiParamTypeClasses
  , LangExt.PatternSynonyms
  , LangExt.PolyKinds
  , LangExt.RankNTypes
  , LangExt.RecordWildCards
  , LangExt.ScopedTypeVariables
  , LangExt.StandaloneDeriving
  , LangExt.TupleSections
  , LangExt.TypeApplications
  , LangExt.TypeFamilies
  , LangExt.TypeOperators
  ]

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
      _ -> let ds = [ dcl { declDoc = pending } | dcl <- declsFromTop ld ]
           in go Nothing (reverse ds ++ acc) rest

    attachPrev _   []       = []
    attachPrev txt (d : ds) = d { declDoc = declDoc d `appendDoc` Just txt } : ds

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

    merge a b = Decl
      { declName       = declName a
      , declSiblings   = declSiblings a `orEmpty` declSiblings b
        -- A bare 'DkFunction' is the least informative kind (every
        -- signature defaults to it), so any more specific kind from
        -- the other half of the merge wins.
      , declKind       = if declKind a == DkFunction then declKind b else declKind a
      , declSigLine    = declSigLine a    `orFirst` declSigLine b
      , declSigEndLine = declSigEndLine a `orFirst` declSigEndLine b
      , declDefLine    = declDefLine a    `orFirst` declDefLine b
      , declDefEndLine = declDefEndLine a `orFirst` declDefEndLine b
      , declDoc        = declDoc a        `appendDoc` declDoc b
      }

    orFirst (Just x) _ = Just x
    orFirst Nothing  y = y

    orEmpty [] ys = ys
    orEmpty xs _  = xs

declsFromTop :: LHsDecl GhcPs -> [Decl]
declsFromTop ld = case unLoc ld of
  SigD _ (TypeSig _ lnames _ty) ->
    let names    = map (rdrText . unLoc) lnames
        (mS, mE) = locLines ld
    in [ sigDecl nm names mS mE DkFunction | nm <- names ]
  SigD _ (PatSynSig _ lnames _ty) ->
    let names    = map (rdrText . unLoc) lnames
        (mS, mE) = locLines ld
    in [ sigDecl nm names mS mE DkPatternSyn | nm <- names ]
  ValD _ (FunBind { fun_id = L _ rn }) ->
    let (mS, mE) = locLines ld
    in [ defDecl (rdrText rn) mS mE DkFunction ]
  ValD _ (PatSynBind _ (PSB { psb_id = L _ rn })) ->
    let (mS, mE) = locLines ld
    in [ defDecl (rdrText rn) mS mE DkPatternSyn ]
  TyClD _ tc ->
    let (mS, mE) = locLines ld
    in case tc of
         SynDecl { tcdLName = L _ rn } ->
           [ defDecl (rdrText rn) mS mE DkTypeSyn ]
         FamDecl { tcdFam = FamilyDecl { fdLName = L _ rn } } ->
           [ defDecl (rdrText rn) mS mE DkTypeFamily ]
         ClassDecl { tcdLName = L _ rn } ->
           [ defDecl (rdrText rn) mS mE DkClass ]
         DataDecl { tcdLName = L _ rn, tcdDataDefn = defn } ->
           let k = case dd_cons defn of
                     NewTypeCon {} -> DkNewtype
                     _             -> DkData
           in [ defDecl (rdrText rn) mS mE k ]
  ForD _ (ForeignImport { fd_name = L _ rn }) ->
    let (mS, mE) = locLines ld
    in [ defDecl (rdrText rn) mS mE DkForeign ]
  _ -> []
  where
    sigDecl nm names mS mE k = Decl
      { declName       = nm
      , declSiblings   = filter (/= nm) names
      , declKind       = k
      , declSigLine    = mS
      , declSigEndLine = mE
      , declDefLine    = Nothing
      , declDefEndLine = Nothing
      , declDoc        = Nothing
      }
    defDecl nm mS mE k = Decl
      { declName       = nm
      , declSiblings   = []
      , declKind       = k
      , declSigLine    = Nothing
      , declSigEndLine = Nothing
      , declDefLine    = mS
      , declDefEndLine = mE
      , declDoc        = Nothing
      }

locLines :: LHsDecl GhcPs -> (Maybe Int, Maybe Int)
locLines ld = case locA (getLoc ld) of
  RealSrcSpan s _ -> (Just (srcSpanStartLine s), Just (srcSpanEndLine s))
  _               -> (Nothing, Nothing)

rdrText :: RdrName -> Text
rdrText = Text.pack . Occ.occNameString . rdrNameOcc
