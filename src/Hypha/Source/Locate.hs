{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Source.Locate
  ( listExportedSymbols
  , locateSymbolDefinition
  , locateSymbolDefinitionInDir
  , findModuleFile
  , findModuleFileIn
  , SourceLocation (..)
    -- * Testing
  , parseExports
  , modulePathToFile
  ) where

import Control.Monad (filterM)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type     (BuildEnv (..))
import qualified Hypha.Source.Parser as Parser
import Hypha.Types.PackageId   (PackageId (..))

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
             let inside    = stripBalanced (Text.drop 1 afterParen)
                 entries   = splitTopLevel inside
             in take 100 [ ident | e <- entries
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
      mLoc <- scanFile sym f
      case mLoc of
        Just loc -> pure (Just loc)
        Nothing  -> findInTree d modPath sym

-- | Walk every @.hs@ file under @root@ (skipping build/test dirs) and
-- return the first hit whose top-level binding or type signature matches
-- @sym@.  Used as a fallback when the target module re-exports a symbol
-- defined in another module of the same package (e.g.
-- @Data.Map.Strict.lookup@ re-exported from @Data.Map.Internal@).
findInTree :: FilePath -> Text -> Text -> IO (Maybe SourceLocation)
findInTree root modPath sym = do
  hsFiles <- enumerateHs root 6
  let prefix  = Text.unpack (Text.replace "." "/" (modulePrefix modPath))
      ranked  = sortByPrefix prefix hsFiles
  go ranked
  where
    go []     = pure Nothing
    go (f:fs) = do
      m <- scanFile sym f
      case m of
        Just loc -> pure (Just loc)
        Nothing  -> go fs

-- | Drop the last dotted segment of a module path so re-exports prefer
-- siblings before unrelated trees: e.g. @Data.Map.Strict@ → @Data.Map@,
-- which scores @Data/Map/Internal.hs@ above @Data/IntMap/Internal.hs@.
modulePrefix :: Text -> Text
modulePrefix m = case Text.breakOnEnd "." m of
  (p, _) | not (Text.null p) -> Text.dropEnd 1 p
  _                          -> m

-- | Sort file paths by how many leading characters they share with the
-- supplied prefix (descending).  Stable on ties.
sortByPrefix :: String -> [FilePath] -> [FilePath]
sortByPrefix prefix = map snd . sortBy (\(a,_) (b,_) -> compare b a) . map score
  where
    score fp = (matchLen prefix fp, fp)
    matchLen :: String -> FilePath -> Int
    matchLen p fp =
      let canonical = dropToPrefix p fp
      in commonLen p canonical
    -- Trim the path so it begins at the first occurrence of the prefix's
    -- leading char; otherwise leading "src/" wrecks the comparison.
    dropToPrefix :: String -> FilePath -> FilePath
    dropToPrefix []      fp = fp
    dropToPrefix (c : _) fp = dropWhile (/= c) fp
    commonLen :: String -> String -> Int
    commonLen []     _      = 0
    commonLen _      []     = 0
    commonLen (a:as) (b:bs)
      | a == b    = 1 + commonLen as bs
      | otherwise = 0

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = foldr insert []
  where
    insert x []     = [x]
    insert x (y:ys) = case cmp x y of
      GT -> y : insert x ys
      _  -> x : y : ys

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
scanFile :: Text -> FilePath -> IO (Maybe SourceLocation)
scanFile sym f = do
  src <- TIO.readFile f
  pure $ case Parser.parseDecls f src of
    Left _      -> Nothing
    Right decls -> do
      d <- Parser.findDecl sym decls
      ln <- case Parser.declDefLine d of
              Just l  -> Just l
              Nothing -> Parser.declSigLine d
      Just (SourceLocation f ln)
