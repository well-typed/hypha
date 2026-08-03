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
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import GHC.IO.Encoding.Failure (CodingFailureMode (RoundtripFailure))
import GHC.IO.Encoding.UTF8 (mkUTF8)
import System.Directory
  ( doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), (<.>))
import System.FilePath qualified as FP
import System.IO (Handle, hIsEOF, hSetEncoding)
import System.Process qualified as Process

import Hypha.Types.BuildPlan (CompilerId (..))
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), Version (..) )
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

-- | The defining modules of each name a compiled module exports.
--
-- Keyed by the exported name because that is the question every caller
-- has: /this module exports @x@ — who declares it?/
--
-- The value is a 'NonEmpty' because one module can export one name from
-- two places: ghc's own @GHC@ exports @XFixitySig@ from both
-- @Language.Haskell.Syntax.Binds@ and @…Extension@.  Keeping one would be
-- the same "first candidate wins" mistake the resolver was cured of — if
-- the kept origin has no indexed row and the dropped one does, the export
-- stays unresolved for no reason.
newtype ModuleOrigins = ModuleOrigins
  { moOrigins :: Map SymbolName (NonEmpty ModulePath) }
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
  | OriginNoExportSection !ModulePath
    -- ^ A header we understood, and no @exports:@ section after it.  The
    -- other door to the same lie, and distinct from a section that is
    -- present and empty — @GHC.Constants@ really has one of those.
  | OriginCompilerMissing !CompilerId ![Text]
    -- ^ No reachable @ghc@ matches the plan's compiler; the field lists
    -- what each candidate answered.  Its interface format would differ,
    -- so reading its @.hi@ files is not a degraded answer but no answer.
  | OriginStoreUnreadable !FilePath !Text
    -- ^ A package-database root we could not list.  Every package under
    -- it then reports as "not built", which is the wrong cause.
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
    "not an interface dump: " <> Text.strip what
  OriginNoExportSection m ->
    "the interface for " <> unModulePath m <> " has no exports section"
  OriginCompilerMissing (CompilerId want) tried ->
    "no ghc matching " <> want <> " is reachable (tried "
      <> Text.intercalate ", " tried <> ")"
  OriginStoreUnreadable root err ->
    "cannot list the package databases under " <> Text.pack root <> ": "
      <> Text.strip err

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
parseShowIface :: Text -> Either OriginError ModuleOrigins
parseShowIface = parseShowIfaceLines . Text.lines

-- | 'parseShowIface' over lines the caller already has.
--
-- GHC prints an export qualified by its defining module, and unqualified
-- when that module is the interface's own — so the header line is load
-- bearing, not decoration.
parseShowIfaceLines :: [Text] -> Either OriginError ModuleOrigins
parseShowIfaceLines ls = do
  self <- maybe (Left notAnInterface) Right (interfaceModule ls)
  body <- maybe (Left (OriginNoExportSection self)) Right (exportSection ls)
  -- 'flip' so the earlier origin stays first: the dump's order is GHC's
  -- own, and the caller probes these in order.  Deduplicated because one
  -- entry can name the same origin twice — @ModuleOrigins{ModuleOrigins
  -- moOrigins}@ is a type and a constructor of one name — and a repeated
  -- origin is one origin, not a second thing to try.
  pure . ModuleOrigins . Map.map NE.nub . Map.fromListWith (flip (<>)) $
    [ (name, fromMaybe self mo :| [])
    | entry      <- body
    , (name, mo) <- splitQualified entry
    ]
  where
    -- Truncated here rather than at the report: the constructor would
    -- otherwise retain the whole dump, and one of those is 12 MB.
    notAnInterface =
      OriginNotAnInterface (Text.take 200 (Text.unlines (take 3 ls)))

-- | The module an interface dump is for: @interface Control.Concurrent 9103@.
interfaceModule :: [Text] -> Maybe ModulePath
interfaceModule ls = listToMaybe
  [ ModulePath m
  | l <- ls
  , Just rest <- [Text.stripPrefix "interface " (Text.stripStart l)]
  , m : _ <- [Text.words rest]
  , looksLikeModule m
  ]

-- | Every name listed under @exports:@, class subordinates included, or
-- 'Nothing' when there is no such section.
--
-- The section ends at the next unindented @field:@ line — @direct module
-- dependencies:@ follows it and is full of module-shaped words, every one
-- of which would otherwise become an export that does not exist.
exportSection :: [Text] -> Maybe [Text]
exportSection ls = case break (== "exports:") ls of
  (_, [])        -> Nothing
  (_, _ : after) ->
    Just (concatMap (Text.words . Text.map unbrace) (takeWhile indented after))
  where
    unbrace c = if c == '{' || c == '}' then ' ' else c

-- | A line that continues a section rather than starting the next one.
-- The listing is indented; a new field is not.
indented :: Text -> Bool
indented l = Text.null (Text.strip l) || Text.isPrefixOf " " l

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
  -- A parent GHC marks with a trailing bar is not itself exported: it
  -- writes @IsString{fromString}@ when the class is exported and
  -- @IsString|{fromString}@ when only the method is, which is how
  -- @Data.ListLike@ re-exports @fromString@ alone.  Recording it would
  -- invent an export named @IsString|@ that no module has.
  --
  -- The bar is only a marker after a name that could not have ended in
  -- one: @(||)@ and @(<|)@ are real exports, and dropping every entry
  -- with a trailing bar deletes them.  A type operator parent is the one
  -- case this cannot tell apart, and GHC's own output cannot either.
  | Text.isSuffixOf "|" entry, looksLikeModule (Text.init entry) = []
  | otherwise = case go [] (Text.splitOn "." entry) of
      ([], name)   -> [(SymbolName name, Nothing)]
      (modSegs, name)
        | Text.null name -> []
        | otherwise ->
            [(SymbolName name, Just (ModulePath (Text.intercalate "." modSegs)))]
  where
    go acc = \case
      (s : rest@(_ : _)) | looksLikeModule s -> go (s : acc) rest
      rest -> (reverse acc, Text.intercalate "." rest)

-- | A capitalised, alphanumeric segment: what a module path is made of,
-- and what an operator never is.
--
-- The dot is a module character because this answers the question for a
-- whole path as well as for one segment — 'interfaceModule' asks about
-- @Control.Concurrent@, 'splitQualified' about @Control@.
looksLikeModule :: Text -> Bool
looksLikeModule s = case Text.uncons s of
  Just (c, _) -> isAsciiUpper c && Text.all isModuleChar s
  Nothing     -> False
  where
    isModuleChar c =
      isAsciiUpper c || isAsciiLower c || isDigit c
        || c == '_' || c == '\'' || c == '.'

-- Asking GHC ---------------------------------------------------------

-- | The @ghc@ and @ghc-pkg@ of one installation, named together so a
-- skewed pair cannot be assembled by accident.
data GhcToolchain = GhcToolchain
  { gtGhc    :: !FilePath
  , gtGhcPkg :: !FilePath
  }

-- | An oracle backed by the compiler the plan was solved with.
--
-- Returns the reason once rather than an oracle that refuses once per
-- module: a mismatch is a property of the machine, not of any module, and
-- @base@ alone would have printed it a hundred times.
--
-- Interface files are version-exact — @ghc-9.12.4 --show-iface@ on a
-- 9.10.3 @.hi@ exits 1 with @mismatched interface file versions@ — so the
-- version equality below is the right predicate, and the compiler is
-- /selected/ by it rather than merely checked against it.  Import
-- directories are resolved once per package and remembered; only modules
-- the caller actually asks about cost a @--show-iface@.
mkGhcOriginOracle
  :: CompilerId
  -> [FilePath]                  -- ^ package databases to stack on the global one
  -> (PackageId -> [FilePath])
     -- ^ interface directories the caller already knows, tried first.
     -- The project's own packages are not in any store database, and the
     -- plan knows where their build tree is.
  -> IO (Either OriginError (OriginOracle IO))
mkGhcOriginOracle cid dbs known = do
  found <- locateToolchain cid
  case found of
    Left err -> pure (Left err)
    Right tc -> do
      dirsRef <- IORef.newIORef Map.empty
      pure (Right (OriginOracle (originsVia tc dirsRef)))
  where
    originsVia tc dirsRef pid m = do
      eDirs <- cachedImportDirs tc dirsRef pid
      case eDirs of
        Left err   -> pure (Left err)
        Right dirs -> do
          let candidates = [ d </> modulePathFile m <.> "hi" | d <- dirs ]
          mHi <- firstExisting candidates
          case mHi of
            Nothing -> pure (Left (OriginIfaceMissing pid m candidates))
            Just hi -> fmap (>>= parseShowIfaceLines) (showIfaceHead (gtGhc tc) hi)

    cachedImportDirs tc dirsRef pid = do
      remembered <- IORef.readIORef dirsRef
      case Map.lookup pid remembered of
        Just cached -> pure cached
        Nothing     -> do
          fresh <- importDirs tc pid
          IORef.atomicModifyIORef' dirsRef (\m -> (Map.insert pid fresh m, ()))
          pure fresh

    -- What the caller knows beats what ghc-pkg knows, and skips a
    -- subprocess: a local package has no store entry to find.
    importDirs tc pid = case known pid of
      d : ds -> pure (Right (d : ds))
      []     -> askGhcPkg tc pid

    askGhcPkg tc pid = do
      out <- runTool (gtGhcPkg tc)
               ( ["--global"] ++ map ("--package-db=" <>) dbs
                 ++ ["field", Text.unpack (renderPkg pid), "import-dirs"] )
      pure $ case out of
        Left (OriginToolFailed _ _ err) -> Left (OriginNoImportDirs pid err)
        Left err                        -> Left err
        Right text -> case mapMaybe (Text.stripPrefix "import-dirs:")
                                    (Text.lines text) of
          []   -> Left (OriginNoImportDirs pid text)
          -- Several when the store holds one version under more than one
          -- hash; each is tried in turn, so no choice is made here.
          dirs -> Right (map (Text.unpack . Text.strip) dirs)

-- | The first reachable installation whose @ghc@ reports the plan's
-- version.
--
-- Versioned names first: ghcup, Debian and Nix all install @ghc-9.10.3@
-- beside the bare @ghc@ shim, so a project built with
-- @--project-file=cabal.ghc-9.12.4.project@ on a machine whose default is
-- 9.10.3 has its compiler right there on @$PATH@.  Asking for @ghc@ alone
-- would refuse the work with the right binary one name away.
locateToolchain :: CompilerId -> IO (Either OriginError GhcToolchain)
locateToolchain cid = go [] candidates
  where
    version = fromMaybe raw (Text.stripPrefix "ghc-" raw)
      where CompilerId raw = cid

    candidates =
      [ GhcToolchain ("ghc-" <> Text.unpack version)
                     ("ghc-pkg-" <> Text.unpack version)
      , GhcToolchain "ghc" "ghc-pkg"
      ]

    go tried [] = pure (Left (OriginCompilerMissing cid (reverse tried)))
    go tried (tc : rest) = do
      answer <- runTool (gtGhc tc) ["--numeric-version"]
      case answer of
        Right v | Text.strip v == version -> pure (Right tc)
        Right v  -> go (said tc (Text.strip v) : tried) rest
        Left err -> go (said tc (renderOriginError err) : tried) rest

    said tc what = Text.pack (gtGhc tc) <> " (" <> what <> ")"

-- | Run @ghc --show-iface@ and stop reading at the end of the exports
-- section.
--
-- Streamed rather than slurped because the rest of a dump is unfoldings
-- we throw away, and there is a lot of it: @GHC.Internal.ClosureTypes@
-- prints 12 MB, which as a 'String' from @readProcessWithExitCode@ is
-- hundreds of megabytes of transient cons cells inside a server that is
-- already holding an index.
showIfaceHead :: FilePath -> FilePath -> IO (Either OriginError [Text])
showIfaceHead ghc hi = handling $
  Process.withCreateProcess spec $ \_ mOut mErr ph ->
    case (mOut, mErr) of
      -- Unreachable while both pipes are requested above, and reported
      -- rather than asserted: a broken pipe is not a programmer error.
      (Nothing, _) -> pure (Left (noPipe "stdout"))
      (_, Nothing) -> pure (Left (noPipe "stderr"))
      (Just out, Just err) -> do
        utf8Lenient out
        utf8Lenient err
        (ls, closed) <- readHead out
        if closed
          then do
            -- We have what we came for; the child would otherwise spend
            -- seconds printing unfoldings into a pipe nobody reads.
            Process.terminateProcess ph
            _ <- Process.waitForProcess ph
            pure (Right ls)
          else do
            code <- Process.waitForProcess ph
            case code of
              ExitSuccess   -> pure (Right ls)
              ExitFailure n -> do
                errText <- TIO.hGetContents err
                pure (Left (OriginToolFailed (Text.pack ghc) n errText))
  where
    spec = (Process.proc ghc ["--show-iface", hi])
      { Process.std_out = Process.CreatePipe
      , Process.std_err = Process.CreatePipe
      }

    noPipe which = OriginToolFailed (Text.pack ghc) (-1) ("no " <> which <> " pipe")

    handling act = do
      r <- try act
      pure $ case r of
        Left (e :: IOException) ->
          Left (OriginToolFailed (Text.pack ghc) (-1) (Text.pack (show e)))
        Right ok -> ok

    -- A stray byte in a dump must not take down an index build.
    utf8Lenient h = hSetEncoding h (mkUTF8 RoundtripFailure)

-- | Lines up to and including the one that closes the exports section,
-- and whether we actually saw it close.
readHead :: Handle -> IO ([Text], Bool)
readHead h = go [] False
  where
    go acc inSection = do
      eof <- hIsEOF h
      if eof
        then pure (reverse acc, False)
        else do
          l <- TIO.hGetLine h
          if inSection && not (indented l)
            then pure (reverse (l : acc), True)
            else go (l : acc) (inSection || l == "exports:")

-- | Run a tool to completion, keeping its own words on failure.
runTool :: FilePath -> [String] -> IO (Either OriginError Text)
runTool cmd args = do
  r <- try (Process.readProcessWithExitCode cmd args "")
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

-- | The cabal store package databases for a compiler, and the roots we
-- could not read.
--
-- Boot packages live in the global database @ghc-pkg@ reads by default;
-- everything else lives in the store, one database per compiler.  Every
-- known store layout is searched — @$CABAL_DIR@, @~\/.cabal@ and the XDG
-- one — because a machine can have any of them, and a root we cannot list
-- travels back rather than silently contributing nothing: every package
-- under it would otherwise report as "never built", naming the wrong
-- cause.
discoverPackageDbs :: CompilerId -> IO ([FilePath], [OriginError])
discoverPackageDbs (CompilerId cid) = do
  mCabalDir <- lookupEnv "CABAL_DIR"
  home      <- getHomeDirectory
  let roots = maybe id (\d -> ((d </> "store") :)) mCabalDir
        [ home </> ".cabal" </> "store"
        , home </> ".local" </> "state" </> "cabal" </> "store"
        ]
  results <- mapM dbsUnder roots
  pure (concat [ ds | Right ds <- results ], [ e | Left e <- results ])
  where
    -- The directory is the compiler id, sometimes with an ABI suffix.
    dbsUnder root = do
      ok <- doesDirectoryExist root
      if not ok
        then pure (Right [])
        else do
          listed <- try @IO @IOException (listDirectory root)
          case listed of
            Left e        ->
              pure (Left (OriginStoreUnreadable root (Text.pack (show e))))
            Right entries -> Right <$> existing
              [ root </> e </> "package.db"
              | e <- entries
              , cid `Text.isPrefixOf` Text.pack e ]

    existing = fmap concat . mapM (\p -> do
      ok <- doesDirectoryExist p
      pure [p | ok])
