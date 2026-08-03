{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Where a module's exports actually come from, according to the
-- compiler.
--
-- "Hypha.Search.Reexport" resolves re-exports from syntax, which is all a
-- parse tree offers: an export list says /which/ names a module exports
-- and never /whence/.  Only a renamer knows, and GHC's already ran — it
-- writes one fully-qualified origin per export into the @.hi@ file, and
-- @ghc --show-iface@ prints them:
--
-- > exports:
-- >   forkFinally
-- >   GHC.Internal.Conc.Bound.isCurrentThreadBound
--
-- That is the answer the syntactic pass cannot reach.  It is used as a
-- repair, not as the primary path: a @.hi@ exists only for a package that
-- has been built, it is tied to one exact GHC, and it carries no
-- signature, doc comment or source line — everything else still comes
-- from the source.
--
-- Rendering Haddock ourselves would answer the same question, and cost a
-- documentation build of every dependency to do it.  The interface files
-- are already on disk.
module Hypha.Source.Origins
  ( -- * What a compiled interface says
    ModuleOrigins (..)
  , parseShowIface
    -- * Asking for it
  , OriginOracle (..)
  , mkGhcOriginOracle
    -- * Errors
  , OriginError (..)
  , renderOriginError
    -- * Discovery
  , discoverPackageDbs
  ) where

import Control.Exception.Safe (IOException, try)
import Data.IORef qualified as IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory
  ( doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), (<.>))
import System.FilePath qualified as FP
import System.Process (readProcessWithExitCode)

import Hypha.Types.BuildPlan (CompilerId (..))
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), Version (..) )
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

-- | The defining module of each name a compiled module exports.
--
-- Keyed by the exported name because that is the question every caller
-- has: /this module exports @x@ — who declares it?/
newtype ModuleOrigins = ModuleOrigins
  { moOrigins :: Map SymbolName ModulePath }
  deriving stock (Show, Eq)

-- | Everything that can stop us answering, carried as itself so the
-- report can say which one happened and the caller can tell "not built"
-- apart from "wrong compiler".
data OriginError
  = OriginNoImportDirs !PackageId !Text
    -- ^ @ghc-pkg@ could not say where the package's interfaces live; the
    -- text is its own complaint.
  | OriginIfaceMissing !PackageId !ModulePath ![FilePath]
    -- ^ No @.hi@ at any of the paths we looked in.  Normal for a package
    -- that has not been built.
  | OriginToolFailed !Text !Int !Text
    -- ^ @(command, exit code, stderr)@.  A version-mismatched @.hi@ lands
    -- here, which is why the compiler's own words travel with it.
  | OriginNotAnInterface !Text
    -- ^ The dump carried no @interface \<module\>@ header, so whatever we
    -- read was not an interface at all.  Never degraded to "this module
    -- exports nothing": that would delete every export instead.
  | OriginCompilerMismatch !CompilerId !Text
    -- ^ The @ghc@ we found is not the one the plan was solved with, so
    -- its interface files would not be readable anyway.
  deriving stock (Show, Eq)

-- | Render an 'OriginError' for a stderr report.  The only place these
-- become 'Text'.
renderOriginError :: OriginError -> Text
renderOriginError = \case
  OriginNoImportDirs pid msg ->
    "no interface directory for " <> renderPkg pid <> ": " <> Text.strip msg
  OriginIfaceMissing pid m paths ->
    "no compiled interface for " <> unModulePath m <> " in " <> renderPkg pid
      <> " (looked in " <> Text.intercalate ", " (map Text.pack paths) <> ")"
  OriginToolFailed cmd code err ->
    cmd <> " exited " <> Text.pack (show code) <> ": " <> Text.strip err
  OriginNotAnInterface what ->
    "not an interface dump: " <> Text.strip (Text.take 200 what)
  OriginCompilerMismatch (CompilerId want) got ->
    "the ghc on PATH is " <> Text.strip got <> ", the plan wants " <> want

renderPkg :: PackageId -> Text
renderPkg pid = unPackageName (pkgName pid) <> "-" <> unVersion (pkgVersion pid)

-- | Ask the compiler where a module's exports come from.
--
-- A record of functions over @m@, like every other effect here, so the
-- indexer's repair pass can be driven by a pure stub in tests and by a
-- subprocess in production.
newtype OriginOracle m = OriginOracle
  { moduleOrigins :: PackageId -> ModulePath -> m (Either OriginError ModuleOrigins)
  }

-- Parsing ------------------------------------------------------------

-- | Read the @exports:@ section of a @ghc --show-iface@ dump.
--
-- GHC prints an export qualified by its defining module, and unqualified
-- when that module is the interface's own — so the header line is load
-- bearing, not decoration.
parseShowIface :: Text -> Either OriginError ModuleOrigins
parseShowIface dump = case interfaceModule (Text.lines dump) of
  Nothing -> Left (OriginNotAnInterface dump)
  Just self -> Right . ModuleOrigins . Map.fromList $
    [ (name, origin)
    | entry      <- exportEntries (Text.lines dump)
    , (name, mo) <- splitQualified entry
    , let origin = maybe self id mo
    ]

-- | The module an interface dump is for: @interface Control.Concurrent 9103@.
interfaceModule :: [Text] -> Maybe ModulePath
interfaceModule ls = listToMaybe
  [ ModulePath m
  | l <- ls
  , Just rest <- [Text.stripPrefix "interface " (Text.stripStart l)]
  , m : _ <- [Text.words rest]
  , looksLikeModule m
  ]

-- | Every name listed under @exports:@, class subordinates included.
--
-- The section ends at the next unindented @field:@ line — @direct module
-- dependencies:@ follows it and is full of module-shaped words, every one
-- of which would otherwise become an export that does not exist.
exportEntries :: [Text] -> [Text]
exportEntries ls =
  concatMap (Text.words . Text.map unbrace) (takeWhile indented body)
  where
    body   = drop 1 (dropWhile (/= "exports:") ls)
    -- The listing is indented; a new section is not.
    indented l = Text.null (Text.strip l) || Text.isPrefixOf " " l
    unbrace c = if c == '{' || c == '}' then ' ' else c

-- | Split @GHC.Internal.Conc.Bound.isCurrentThreadBound@ into its module
-- and its name, and report 'Nothing' for a bare name.
--
-- Not a split on the last dot: @GHC.Internal.Data.Bits..>>.@ is the
-- operator @.>>.@ from @GHC.Internal.Data.Bits@, and the last dot falls
-- inside the operator.  Not a split on the first non-module segment
-- either: in @GHC.Internal.Bits.Bits@ every segment is capitalised and
-- the last one is the class.  So: take capitalised segments while at
-- least one segment is left over, and the leftovers are the name.
splitQualified :: Text -> [(SymbolName, Maybe ModulePath)]
splitQualified entry
  | Text.null entry = []
  | otherwise = case go [] (Text.splitOn "." entry) of
      ([], name)   -> [(SymbolName name, Nothing)]
      (modSegs, name)
        | Text.null name -> []
        | otherwise ->
            [(SymbolName name, Just (ModulePath (Text.intercalate "." modSegs)))]
  where
    go acc = \case
      (s : rest@(_ : _)) | looksLikeModule s -> go (acc ++ [s]) rest
      rest                                   -> (acc, Text.intercalate "." rest)

-- | A capitalised, alphanumeric segment: what a module path is made of,
-- and what an operator never is.
--
-- The dot is a module character because this answers the question for a
-- whole path as well as for one segment — 'interfaceModule' asks about
-- @Control.Concurrent@, 'splitQualified' about @Control@.
looksLikeModule :: Text -> Bool
looksLikeModule s = case Text.uncons s of
  Just (c, _) -> c `elem` ['A' .. 'Z'] && Text.all isModuleChar s
  Nothing     -> False
  where
    isModuleChar c =
      c `elem` ['A' .. 'Z'] || c `elem` ['a' .. 'z']
        || c `elem` ['0' .. '9'] || c == '_' || c == '\'' || c == '.'

-- Asking GHC ---------------------------------------------------------

-- | An oracle backed by the @ghc@ and @ghc-pkg@ of the plan's compiler.
--
-- Refuses to answer at all when the @ghc@ we can reach is not the one the
-- plan was solved with: its @.hi@ format differs, so every answer would
-- be a tool failure reported once per module.  Import directories are
-- resolved once per package and remembered; only modules the caller
-- actually asks about cost a @--show-iface@.
mkGhcOriginOracle
  :: CompilerId
  -> [FilePath]          -- ^ extra package databases to stack on the global one
  -> IO (OriginOracle IO)
mkGhcOriginOracle cid dbs = do
  installed <- numericVersion
  case installed of
    Left err -> pure (OriginOracle (\_ _ -> pure (Left err)))
    Right v
      | v /= compilerVersion cid ->
          pure (OriginOracle (\_ _ -> pure (Left (OriginCompilerMismatch cid v))))
      | otherwise -> do
          dirsRef <- IORef.newIORef Map.empty
          pure (OriginOracle (originsVia dirsRef))
  where
    originsVia dirsRef pid m = do
      eDirs <- cachedImportDirs dirsRef pid
      case eDirs of
        Left err   -> pure (Left err)
        Right dirs -> do
          mHi <- firstExisting [ d </> modulePathFile m <.> "hi" | d <- dirs ]
          case mHi of
            Nothing ->
              pure (Left (OriginIfaceMissing pid m
                            [ d </> modulePathFile m <.> "hi" | d <- dirs ]))
            Just hi -> do
              out <- runTool "ghc" ["--show-iface", hi]
              pure (parseShowIface =<< out)

    cachedImportDirs dirsRef pid = do
      known <- IORef.readIORef dirsRef
      case Map.lookup pid known of
        Just cached -> pure cached
        Nothing     -> do
          fresh <- importDirs pid
          IORef.atomicModifyIORef' dirsRef (\m -> (Map.insert pid fresh m, ()))
          pure fresh

    importDirs pid = do
      out <- runTool "ghc-pkg"
               ( ["--global"] ++ map ("--package-db=" <>) dbs
                 ++ ["field", Text.unpack (renderPkg pid), "import-dirs"] )
      pure $ case out of
        Left (OriginToolFailed _ _ err) -> Left (OriginNoImportDirs pid err)
        Left err                        -> Left err
        Right text -> case mapMaybe (Text.stripPrefix "import-dirs:")
                                    (Text.lines text) of
          []   -> Left (OriginNoImportDirs pid text)
          -- Several when the store holds the same version under more than
          -- one hash; each is tried in turn, so no choice is made here.
          dirs -> Right (map (Text.unpack . Text.strip) dirs)

    compilerVersion (CompilerId c) = maybe c id (Text.stripPrefix "ghc-" c)

    numericVersion = fmap (fmap Text.strip) (runTool "ghc" ["--numeric-version"])

-- | Run a tool, keeping its own words on failure.
runTool :: FilePath -> [String] -> IO (Either OriginError Text)
runTool cmd args = do
  r <- try (readProcessWithExitCode cmd args "")
  pure $ case r of
    Left (e :: IOException) ->
      Left (OriginToolFailed (Text.pack cmd) (-1) (Text.pack (show e)))
    Right (ExitSuccess, out, _) -> Right (Text.pack out)
    Right (ExitFailure n, _, err) ->
      Left (OriginToolFailed (Text.pack cmd) n (Text.pack err))

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting []       = pure Nothing
firstExisting (p : ps) = do
  ok <- doesFileExist p
  if ok then pure (Just p) else firstExisting ps

-- | @Data.Map.Strict@ to @Data\/Map\/Strict@, the layout @.hi@ files use.
--
-- Through 'joinPath', which is total and knows the platform separator:
-- @foldr1 (\<\/\>)@ is neither, and rewriting @\'.\'@ to @\'\/\'@ by hand
-- is the shape of the Windows bug in issue #10.
modulePathFile :: ModulePath -> FilePath
modulePathFile =
  FP.joinPath . map Text.unpack . Text.splitOn "." . unModulePath

-- | The cabal store package databases for a compiler.
--
-- Boot packages live in the global database @ghc-pkg@ reads by default;
-- everything else lives in the store, one database per compiler.  Both
-- store layouts are checked — @~\/.cabal@ and the XDG one — because a
-- machine can have either, and @CABAL_DIR@ overrides both.
discoverPackageDbs :: CompilerId -> IO [FilePath]
discoverPackageDbs (CompilerId cid) = do
  mCabalDir <- lookupEnv "CABAL_DIR"
  home      <- getHomeDirectory
  let roots = maybe id (\d -> ((d </> "store") :)) mCabalDir
        [ home </> ".cabal" </> "store"
        , home </> ".local" </> "state" </> "cabal" </> "store"
        ]
  concat <$> mapM dbsUnder roots
  where
    -- The directory is the compiler id, sometimes with an ABI suffix.
    dbsUnder root = do
      ok <- doesDirectoryExist root
      if not ok
        then pure []
        else do
          entries <- either (const []) id
                       <$> try @IO @IOException (listDirectory root)
          existing [ root </> e </> "package.db"
                   | e <- entries
                   , cid `Text.isPrefixOf` Text.pack e ]

    existing = fmap concat . mapM (\p -> do
      ok <- doesDirectoryExist p
      pure [p | ok])
