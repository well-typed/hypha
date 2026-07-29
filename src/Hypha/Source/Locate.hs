{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Source.Locate
  ( listExportedSymbols
  , locateSymbolDefinition
  , locateSymbolDefinitionInDir
  , locateDefinitionInComponent
  , LocatedDefinition (..)
  , Provenance (..)
  , findModuleFile
  , findModuleFileIn
  , SourceLocation (..)
    -- * Testing
  , parseExports
  , modulePathToFile
  , scanFile
  ) where

import Control.Monad (filterM)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type     (BuildEnv (..))
import Hypha.Search.Index
  ( DefinitionRef (..), ImportedDefinitions (..), ModuleSource (..) )
import Hypha.Search.Reexport   (DefinitionSite (..), Resolution (..))
import qualified Hypha.Search.Reexport as Reexport
import qualified Hypha.Source.Extensions as Extensions
import qualified Hypha.Source.Interface as Interface
import qualified Hypha.Source.Parser as Parser
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId   (PackageId (..))
import Hypha.Types.SymbolPath  (ModulePath (..), SymbolName (..))
import System.IO (hPutStrLn, stderr)

-- | Location of a symbol definition in a source file.
data SourceLocation = SourceLocation
  { slPath :: !FilePath
  , slLine :: !Int
  }
  deriving stock (Show, Eq)

-- | Inspect the package source directory and list candidate exported symbols
-- by scraping the @module ... ( ... ) where@ header naïvely.
--
-- A future improvement (post-MVP) is to use ghc-lib-parser for accurate parsing.
listExportedSymbols :: BuildEnv IO -> PackageId -> Text -> IO [Text]
listExportedSymbols env pid modPath = do
  mDir <- locatePackageSource env pid
  case mDir of
    Nothing -> pure []
    Just d  -> do
      let f = d </> modulePathToFile modPath
      ok <- doesFileExist f
      if not ok
        then pure []
        else parseExports <$> TIO.readFile f

-- | Convert a module path like @Control.Concurrent.Async@ to a file path
-- like @Control/Concurrent/Async.hs@.
modulePathToFile :: Text -> FilePath
modulePathToFile m = Text.unpack (Text.replace "." "/" m) <> ".hs"

-- | Locate the source file for a module under a package's source tree.
-- Tries the root first, then common @hs-source-dirs@ ("src", "library",
-- "lib", "Library"), then a bounded BFS for any prefix dir that contains
-- the module path.
findModuleFile :: FilePath -> Text -> IO (Maybe FilePath)
findModuleFile root modPath = do
  let relFile  = modulePathToFile modPath
      candidates =
        [ root </> relFile
        , root </> "src"     </> relFile
        , root </> "library" </> relFile
        , root </> "lib"     </> relFile
        , root </> "Library" </> relFile
        , root </> "source"  </> relFile
        , root </> "Source"  </> relFile
        ]
  m <- firstExisting candidates
  case m of
    Just p  -> pure (Just p)
    Nothing -> bfsFind root relFile 4

-- | Resolve a module path against an explicit list of source-root
-- candidates, in priority order.  The first existing file wins.
-- Unlike 'findModuleFile' this does /not/ walk heuristic subdirs — the
-- caller is expected to have already enumerated the component's
-- @hs-source-dirs@.
findModuleFileIn :: [FilePath] -> Text -> IO (Maybe FilePath)
findModuleFileIn roots modPath =
  let rel = modulePathToFile modPath
  in firstExisting [ r </> rel | r <- roots ]

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting []     = pure Nothing
firstExisting (p:ps) = do
  ok <- doesFileExist p
  if ok then pure (Just p) else firstExisting ps

-- | Bounded breadth-first search.  Visits @root@'s subdirectories up to the
-- given depth looking for any @<subdir>/<rel>@ file.  Skips hidden and
-- build-artifact directories.
bfsFind :: FilePath -> FilePath -> Int -> IO (Maybe FilePath)
bfsFind root rel depth
  | depth <= 0 = pure Nothing
  | otherwise = do
      entries <- listDirectory root
      let dirs = [ root </> e | e <- entries, not (isSkip e) ]
      subs <- filterM doesDirectoryExist dirs
      directHit <- firstExisting [ d </> rel | d <- subs ]
      case directHit of
        Just p  -> pure (Just p)
        Nothing -> tryDeeper subs
  where
    tryDeeper []     = pure Nothing
    tryDeeper (d:ds) = do
      m <- bfsFind d rel (depth - 1)
      case m of
        Just p  -> pure (Just p)
        Nothing -> tryDeeper ds

    isSkip name = case name of
      '.':_       -> True
      "dist"      -> True
      "dist-newstyle" -> True
      "build"     -> True
      "test"      -> True
      "tests"     -> True
      "bench"     -> True
      "benchmarks" -> True
      _           -> False

-- | Crude header parser.  Returns the comma-separated identifiers in the
-- module's explicit export list.  Designed to be conservative: when the
-- shape of the header is unfamiliar (no export list, missing @module@
-- keyword, exports written after @where@, etc.) it returns @[]@ rather
-- than guessing.
--
-- Specifically we require the @module@ keyword, an open paren that appears
-- before any @where@ token, and a matching close paren that appears before
-- the corresponding @where@.  Constructor lists @Foo(..)@, sub-exports
-- @Foo(Bar,Baz)@ and operators are tolerated but not deeply parsed; only the
-- leading identifier per entry is reported.
parseExports :: Text -> [Text]
parseExports src =
  case findModuleHeader src of
    Nothing  -> []
    Just hdr ->
      let (_, afterParen) = Text.breakOn "(" hdr
      in if Text.null afterParen
           then []
           else
             -- No cap: the list is naturally bounded by the header
             -- size, and truncating it silently drops real exports
             -- (Data.Map exports well over 100 names) — downstream
             -- consumers use this as an export *filter*, so a cap
             -- loses documentation, it doesn't just shorten a list.
             let inside    = stripBalanced (Text.drop 1 afterParen)
                 entries   = splitTopLevel inside
             in [ ident | e <- entries
                        , let ident = leading e
                        , not (Text.null ident)
                        ]
  where
    -- Find the slab from @module@ through the matching @where@.  Returns
    -- 'Nothing' when no module header is present.  Strips line comments
    -- inside the slab so '(' tokens inside @-- ...@ are ignored.
    findModuleHeader :: Text -> Maybe Text
    findModuleHeader t =
      let ls       = Text.lines t
          dropped  = map dropLineComment ls
          startIx  = findStart 0 dropped
      in case startIx of
           Nothing -> Nothing
           Just i  ->
             let rest    = drop i dropped
                 (h, _)  = breakIncludingWhere rest
             in Just (Text.unlines h)

    -- Index of the first line whose stripped prefix is @module @ (or @module\n@).
    findStart :: Int -> [Text] -> Maybe Int
    findStart _ []     = Nothing
    findStart i (l:ls)
      | startsModule l = Just i
      | otherwise      = findStart (i + 1) ls

    startsModule :: Text -> Bool
    startsModule l =
      let s = Text.dropWhile (`elem` (" \t" :: String)) l
      in "module " `Text.isPrefixOf` s || s == "module"

    -- Take lines up to and including the one containing "where" at top level.
    breakIncludingWhere :: [Text] -> ([Text], [Text])
    breakIncludingWhere = goB [] (0 :: Int)
      where
        goB acc _ []     = (reverse acc, [])
        goB acc d (l:ls) =
          let (d', sawWhere) = scanLine d l
          in if sawWhere
               then (reverse (l : acc), ls)
               else goB (l : acc) d' ls

        scanLine :: Int -> Text -> (Int, Bool)
        scanLine d0 l = goS d0 l False
          where
            goS d txt found = case Text.uncons txt of
              Nothing         -> (d, found)
              Just ('(', rest) -> goS (d + 1) rest found
              Just (')', rest) -> goS (max 0 (d - 1)) rest found
              Just _           ->
                if d == 0 && "where" `Text.isPrefixOf` txt
                  then (d, True)
                  else goS d (Text.drop 1 txt) found

    dropLineComment :: Text -> Text
    dropLineComment l = fst (Text.breakOn "--" l)

    -- Pull the matched, balanced contents of an open '(' (we've already
    -- dropped the '(' itself; return the bytes up to the matching ')').
    stripBalanced :: Text -> Text
    stripBalanced = goP 1 Text.empty
      where
        goP :: Int -> Text -> Text -> Text
        goP _ acc t | Text.null t = acc
        goP d acc t = case Text.uncons t of
          Nothing             -> acc
          Just ('(', rest)    -> goP (d + 1) (Text.snoc acc '(') rest
          Just (')', rest) | d <= 1 -> acc
                           | otherwise  -> goP (d - 1) (Text.snoc acc ')') rest
          Just (c, rest)      -> goP d (Text.snoc acc c) rest

    -- Split a comma-separated export list, respecting nested parens.
    splitTopLevel :: Text -> [Text]
    splitTopLevel = goT (0 :: Int) Text.empty
      where
        goT _ acc t | Text.null t = [acc]
        goT d acc t = case Text.uncons t of
          Nothing                       -> [acc]
          Just (',', rest) | d == 0     -> acc : goT 0 Text.empty rest
          Just ('(', rest)              -> goT (d + 1) (Text.snoc acc '(') rest
          Just (')', rest) | d > 0      -> goT (d - 1) (Text.snoc acc ')') rest
          Just (c,   rest)              -> goT d (Text.snoc acc c) rest

    leading :: Text -> Text
    leading e0 =
      let e   = trim e0
          e'  = stripPrefixWord "pattern" e
          e'' = stripPrefixWord "type" e'
      in Text.takeWhile (\c -> c /= '(' && c /= ',' && c /= ' ') e''

    stripPrefixWord :: Text -> Text -> Text
    stripPrefixWord w t = case Text.stripPrefix (w <> " ") t of
      Just rest -> rest
      Nothing   -> t

    trim :: Text -> Text
    trim = Text.dropWhile (`elem` (" \t\n\r" :: String))
         . Text.dropWhileEnd (`elem` (" \t\n\r" :: String))

-- | Find the line where a symbol is defined.
locateSymbolDefinition :: BuildEnv IO -> PackageId -> Text -> Text -> IO (Maybe SourceLocation)
locateSymbolDefinition env pid modPath sym = do
  mDir <- locatePackageSource env pid
  case mDir of
    Nothing -> pure Nothing
    Just d  -> locateSymbolDefinitionInDir d modPath sym

-- | Variant that takes a pre-resolved source directory (e.g. from
-- 'Hypha.Package.Resolver.resolveSrc').  Uses 'findModuleFile' so common
-- @hs-source-dirs@ layouts are covered.
locateSymbolDefinitionInDir :: FilePath -> Text -> Text -> IO (Maybe SourceLocation)
locateSymbolDefinitionInDir d modPath sym = do
  mFile <- findModuleFile d modPath
  case mFile of
    Nothing -> findInTree d modPath sym
    Just f  -> do
      scanned <- scanFileE sym f
      case scanned of
        Left e -> do
          -- Not \"not here\": we could not read the module at all.  Say so,
          -- then sweep, so a wrong answer is at least an explained one.
          hPutStrLn stderr $
            "hypha: " <> f <> " could not be parsed: "
              <> Text.unpack (Parser.parseErrorMessage e)
          findInTree d modPath sym
        Right (Just loc) -> pure (Just loc)
        Right Nothing    -> findInTree d modPath sym

-- | 'scanFileE' with the symbol name typed, for callers outside this
-- module.
scanFile
  :: SymbolName -> FilePath -> IO (Either Parser.ParseError (Maybe SourceLocation))
scanFile = scanFileE . unSymbolName

-- | Where a symbol is declared, and how confident we are about it.
data LocatedDefinition = LocatedDefinition
  { ldLocation   :: !SourceLocation
  , ldModule     :: !ModulePath
  , ldComponent  :: !ComponentKey
    -- ^ Which component the definition is in.  A re-export can cross a
    -- package boundary, and a card reporting only the module would send the
    -- reader to a module the page's package does not have.
  , ldProvenance :: !Provenance
  , ldDecl       :: !Parser.Decl
    -- ^ The declaration itself, so a caller building a symbol card reads
    -- the signature and Haddock off the parse that located it rather than
    -- reading and parsing the file a second time — which is what the card
    -- used to do, under the wrong language settings and discarding the
    -- parse error.
  , ldContent    :: !Text
    -- ^ The defining module's bytes, for slicing the declaration's text.
  }
  deriving stock (Show, Eq)

-- | The @GuessedBySweep@ arm exists so a fallback can never be mistaken
-- for a resolution.  Its predecessor returned the same 'SourceLocation'
-- either way, which is how a swept file came to be presented as fact.
data Provenance
  = Resolved !DefinitionSite
  | GuessedBySweep !Text          -- ^ why resolution was unavailable
  deriving stock (Show, Eq)

-- | Locate a symbol given every module of its component, plus the modules
-- of other components its exports resolve into.
--
-- Resolution, not sweeping: with the component in hand there is nothing to
-- guess.  A symbol the asking module re-exports is followed to its
-- definition — including into a dependency, when the caller supplied that
-- module's source — and a symbol whose definition we cannot reach is
-- reported absent rather than approximated by the first same-named binding
-- elsewhere in the package.
--
-- The component key is a parameter because the result has to name the
-- component the definition is in, and deriving that from a module path is
-- exactly the guess this module exists to remove.
locateDefinitionInComponent
  :: Extensions.LanguageSettings
  -> ComponentKey                                  -- ^ the asking component
  -> [ModuleSource]
  -> ImportedDefinitions                           -- ^ what the index resolved
  -> ModulePath
  -> SymbolName
  -> IO (Maybe LocatedDefinition)
locateDefinitionInComponent langs ownComponent sources imported asking sym =
  -- The index's answer first, because it is the only transitively resolved
  -- one.  Following the immediate import instead is a single hop, and
  -- @base:Data.List@ needs two: @GHC.Internal.Data.List@ declares nothing
  -- and passes @mapAccumL@ along from @GHC.Internal.Data.Traversable@, so
  -- scanning it reported the symbol absent.
  case resolvedSite of
    Just (def, ms) -> scanned (drComponent def) (drModule def)
                        (DefinedOutside (drModule def)) ms Nothing
    Nothing        -> byResolution
  where
    resolvedSite = do
      def <- Map.lookup sym (idSites imported)
      (_, ms) <- Map.lookup (drModule def) (idSources imported)
      pure (def, ms)

    byResolution = do
      parsed <- mapM parseOne sources
      mapM_ reportParseFailure [ (ms, e) | (ms, Left e) <- parsed ]
      let ifaces     = [ i | (_, Right i) <- parsed ]
          resolution = Reexport.resolveComponent ifaces
      case Map.lookup (asking, sym) resolution of
        Nothing -> do
          hPutStrLn stderr $
            "hypha: " <> Text.unpack (unModulePath asking) <> " does not export "
              <> Text.unpack (unSymbolName sym)
          pure Nothing
        Just res -> case resSite res of
          DefinedOutside m | m /= asking ->
            case Map.lookup m (idSources imported) of
              Nothing -> do
                hPutStrLn stderr $
                  "hypha: " <> Text.unpack (unModulePath asking) <> " re-exports "
                    <> Text.unpack (unSymbolName sym) <> " from "
                    <> Text.unpack (unModulePath m)
                    <> ", whose source was not supplied"
                pure Nothing
              Just (comp, ms) -> scanned comp m (resSite res) ms Nothing
          site -> do
            let target = Reexport.definitionModule asking site
            case [ (ms, i)
                 | (ms, Right i) <- parsed, Interface.miName i == target ] of
              []            -> pure Nothing
              ((ms, i) : _) -> scanned ownComponent target site ms (Just i)

    -- The parse we already have, or one made under this component's own
    -- language settings -- never 'scanFileE', which re-reads the file and
    -- parses it under the GHC2021 floor.  That discarded the cabal
    -- stanza's default-extensions at the last hop, so a component with
    -- @default-extensions: LambdaCase@ resolved the symbol and then failed
    -- to read the module it had resolved it to.
    scanned comp target site ms mIface = do
      r <- case mIface of
             Just i  -> pure (Right i)
             Nothing -> Interface.parseInterfaceIO langs (msPath ms) (msContent ms)
      case r of
        Left e -> do
          reportParseFailure (ms, e)
          pure Nothing
        Right iface -> pure $ do
          decl <- Parser.findDecl (unSymbolName sym) (Interface.miDecls iface)
          ln   <- declAnchor decl
          pure LocatedDefinition
            { ldLocation   = SourceLocation (msPath ms) ln
            , ldModule     = target
            , ldComponent  = comp
            , ldProvenance = Resolved site
            , ldDecl       = decl
            , ldContent    = msContent ms
            }

    -- Guarded: cpphs signals an undefined build-time macro by calling
    -- 'error' from pure code, and an unhandled one here would 500 the
    -- symbol card.
    parseOne ms = do
      r <- Interface.parseInterfaceIO langs (msPath ms) (msContent ms)
      pure (ms, r)

    reportParseFailure (ms, e) = hPutStrLn stderr $
      "hypha: " <> msPath ms <> " could not be parsed: "
        <> Text.unpack (Parser.parseErrorMessage e)

-- | Walk every @.hs@ file under @root@ (skipping build/test dirs) and
-- return the first hit whose top-level binding or type signature matches
-- @sym@.  Used as a fallback when the target module re-exports a symbol
-- defined in another module of the same package (e.g.
-- @Data.Map.Strict.lookup@ re-exported from @Data.Map.Internal@).
findInTree :: FilePath -> Text -> Text -> IO (Maybe SourceLocation)
findInTree root modPath sym = do
  hsFiles <- enumerateHs root 6
  go (rankBySharedSuffix modPath hsFiles)
  where
    go []     = pure Nothing
    go (f:fs) = do
      scanned <- scanFileE sym f
      case scanned of
        Right (Just loc) -> pure (Just loc)
        _                -> go fs

-- | Rank candidate files by how much of the module's path they share,
-- counted in /segments/ from the end.
--
-- @Data.Map.Internal@ shares three trailing segments with
-- @…\/src\/Data\/Map\/Internal.hs@ and one with @…\/src\/Data\/Set\/Internal.hs@.
-- The predecessor counted shared leading /characters/ between a dotted
-- module prefix and a slashed path, which stopped discriminating after the
-- first segment and let the sweep answer @Data.Map.Internal.balanceL@ with
-- @Data\/Set\/Internal.hs@.
rankBySharedSuffix :: Text -> [FilePath] -> [FilePath]
rankBySharedSuffix modPath = sortOn (negate . shared)
  where
    wanted = reverse (Text.splitOn "." modPath)

    shared fp =
      let segs = reverse (Text.splitOn "/" (Text.pack (dropDotHs fp)))
      in length (takeWhile id (zipWith (==) wanted segs))

    dropDotHs fp = case Text.stripSuffix ".hs" (Text.pack fp) of
      Just t  -> Text.unpack t
      Nothing -> fp

-- | Bounded recursive enumeration of every @.hs@ file under @root@.
-- Skips hidden directories and conventional non-library trees so the
-- @findInTree@ fallback stays cheap on real packages.
enumerateHs :: FilePath -> Int -> IO [FilePath]
enumerateHs _ depth | depth < 0 = pure []
enumerateHs dir depth = do
  entries <- listDirectory dir
  fmap concat . mapM (visit depth) $ map (dir </>) entries
  where
    visit d p = do
      isDir <- doesDirectoryExist p
      if isDir
        then if skipDir (fileName p)
               then pure []
               else enumerateHs p (d - 1)
        else if ".hs" `Text.isSuffixOf` Text.pack p
               then pure [p]
               else pure []

    fileName :: FilePath -> String
    fileName = reverse . takeWhile (/= '/') . reverse

    skipDir n = case n of
      '.':_           -> True
      "dist"          -> True
      "dist-newstyle" -> True
      "build"         -> True
      "test"          -> True
      "tests"         -> True
      "bench"         -> True
      "benchmarks"    -> True
      _               -> False

-- | Find the line in a file where @sym@ has a top-level signature or
-- definition, using "Hypha.Source.Parser" so multi-symbol signatures
-- (@a, b :: T@), operator declarations, and other shapes the previous
-- line-grep missed all resolve correctly.  Definition line wins over
-- signature line when both are present (matches the historical
-- semantics: callers prefer the binding body for source snippets).
-- | Parse-tree lookup of one symbol in one file.
--
-- The three outcomes are distinct on purpose.  @Left@ means the module
-- could not be read; @Right Nothing@ means it was read and does not
-- declare the symbol; @Right (Just loc)@ means it does.  Collapsing the
-- first two into @Nothing@ — which is what this function used to do —
-- turns \"unreadable module\" into \"look somewhere else\", and somewhere
-- else is the wrong answer: it is how @Data.Map.Internal.balanceL@ came to
-- report @Data\/Set\/Internal.hs@.
scanFileE :: Text -> FilePath -> IO (Either Parser.ParseError (Maybe SourceLocation))
scanFileE sym f = do
  src <- TIO.readFile f
  pure $ case Parser.parseDecls f src of
    Left e      -> Left e
    Right decls -> Right $ do
      d  <- Parser.findDecl sym decls
      ln <- declAnchor d
      Just (SourceLocation f ln)

-- | The line a declaration is anchored at: its body when it has one, its
-- signature otherwise.  Callers prefer the binding body for source
-- snippets, and a re-export module's entry has only a signature.
declAnchor :: Parser.Decl -> Maybe Int
declAnchor d = case Parser.declDefLine d of
  Just l  -> Just l
  Nothing -> Parser.declSigLine d
