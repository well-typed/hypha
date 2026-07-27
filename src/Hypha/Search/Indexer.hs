{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Building the search index: walk a component's modules, extract rows,
-- persist them, publish them into the in-memory scorer.
--
-- Split from "Hypha.Search.Index" because the row types have to sit
-- /below/ the cache (the cache knows about rows; rows know nothing about
-- storage) while the builder sits /above/ it (it reads and writes through
-- the cache).  One module for both would be a dependency cycle.
--
-- Lifted out of "Hypha.Command.Server", which owned both the HTTP wiring
-- and the indexer at 843 lines.
module Hypha.Search.Indexer
  ( buildAndCacheIndex
  , hydrateFromCache
  , componentsForUnit
  , enumModulesIn
  , chooseSourceRoots
  , collectModuleRows
  , reexportRows
  , indexedRowOf
  , provisionalRow
  ) where

import Control.Exception (SomeException, evaluate, try)
import Data.IORef qualified as IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import System.Directory qualified as Dir
import System.FilePath qualified as FP
import System.IO (hPutStrLn, stderr)

import Hypha.Package.Resolver (PackageResolver (..))
import Hypha.Project.Components qualified as Comp
import Hypha.Search.Fuzzy qualified as Fuzzy
import Hypha.Search.Index
  (IndexRow (..), Visibility (Exposed))
import Hypha.Search.PackageCache (CacheOrigin (..))
import Hypha.Search.PackageCache qualified as Cache
import Hypha.Source.Locate qualified as Locate
import Hypha.Source.Parser qualified as Parser
import Hypha.Types.BuildPlan
import Hypha.Types.ComponentName (ComponentKey (..), componentKeyOf)
import Hypha.Types.PackageId
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))

-- | Module-name enumeration over an explicit list of source roots.
enumModulesIn :: [FilePath] -> IO [Text]
enumModulesIn roots = do
  paths <- concat <$> mapM
    (\r -> map (drop (length r + 1)) <$> findHs r 4)
    roots
  pure (map (Text.pack . hsToModule) paths)

-- | Enumerate every component of a unit (main + sublibs + exes) as
-- @(kind, sourceDirs)@ pairs.  Falls back to a single fallback entry
-- using the heuristic root walk when the unit has no parsed
-- components.
componentsForUnit
  :: BuildPlan -> PackageId -> FilePath
  -> IO [(Comp.ComponentKind, [FilePath])]
componentsForUnit plan pid d =
  case lookupUnit (pkgName pid) plan of
    Just pu | not (null (puLibComponents pu)) ->
      pure
        [ (Comp.ciKind c, Comp.ciHsSourceDirs c)
        | c <- puLibComponents pu
        ]
    _ -> do
      roots <- chooseSourceRoots d
      pure [(Comp.MainLib, roots)]

-- | Pull every cached component index into the in-memory ref.  A unit
-- counts as "fully hydrated" only when /every/ one of its components
-- has cached rows; otherwise it's reported as missing so the
-- background indexer rebuilds the whole set.
hydrateFromCache
  :: BuildPlan
  -> Cache.HyphaPackageCache
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IO [PackageId]
hydrateFromCache plan cache pids ref = go [] pids
  where
    go missing [] = pure (reverse missing)
    go missing (pid : rest) = do
      let pkgT  = unPackageName (pkgName pid)
          verT  = unVersion    (pkgVersion pid)
      kinds <- componentKinds plan pid
      case kinds of
        []  -> go (pid : missing) rest
        _   -> do
          let keys = [ unComponentKey (componentKeyOf (PackageName pkgT) k)
                     | k <- kinds ]
          hits <- mapM (\k -> Cache.haveCachedIndex cache k verT) keys
          if and hits
            then do
              mapM_ (loadKey verT) keys
              go missing rest
            else go (pid : missing) rest

    loadKey verT k = do
      rows <- Cache.readCachedIndex cache k verT
      let indexed = map indexedRowOf rows
      indexed `seq`
        IORef.atomicModifyIORef' ref (\old -> (indexed ++ old, ()))

    -- | Just the component kinds for a unit, mirroring the
    -- structure 'componentsForUnit' would emit.  We avoid needing a
    -- source dir here because hydrate works off the cache alone.
    componentKinds :: BuildPlan -> PackageId -> IO [Comp.ComponentKind]
    componentKinds p pid =
      case lookupUnit (pkgName pid) p of
        Just pu | not (null (puLibComponents pu)) ->
          pure [ Comp.ciKind c | c <- puLibComponents pu ]
        _ -> pure [Comp.MainLib]

-- | Walk the source trees of the given packages, extract their module
-- exports, persist the result to the cache, and prepend them to the
-- in-memory ref.  Packages whose source cannot be resolved are silently
-- skipped — the index is a best-effort fallback.
--
-- Per-module rows are built fully /outside/ the atomicModifyIORef'
-- critical section; prepending makes each insert O(|rows|) instead of
-- the O(|index|) behaviour of @old ++ rows@.
buildAndCacheIndex
  :: BuildPlan
  -> Cache.HyphaPackageCache
  -> PackageResolver IO
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IORef.IORef Int                    -- ^ packages-done counter
  -> IO ()
buildAndCacheIndex plan cache resolver pids ref doneRef =
  mapM_ indexUnit pids
  where
    -- Local + source-repository-package units land in the project DB;
    -- everything else (store packages) goes to the shared global DB.
    originFor :: PackageId -> CacheOrigin
    originFor pid = case lookupUnit (pkgName pid) plan of
      Just u | puIsLocal u -> OriginProject
      _                    -> OriginGlobal
    -- The done counter bumps once per /unit/, not per component, so
    -- the progress bar continues to read in package units.
    bump = IORef.atomicModifyIORef' doneRef (\n -> (n + 1, ()))

    indexUnit pid = do
      eDir <- resolveSrc resolver pid
      case eDir of
        Left _  -> bump
        Right d -> do
          comps <- componentsForUnit plan pid d
          mapM_ (indexComponent pid) comps
          bump

    indexComponent pid (kind, srcDirs) = do
      let pkgT    = unPackageName (pkgName    pid)
          verT    = unVersion    (pkgVersion pid)
          compKey = unComponentKey (componentKeyOf (PackageName pkgT) kind)
      mods   <- enumModulesIn srcDirs
      loaded <- catMaybes <$> mapM (loadModuleSrc srcDirs) mods
      localChunks <- mapM (\(m, f, s) -> collectModuleRows compKey m f s) loaded
      let flatLocal = concat localChunks
          -- Flagship rows: a re-exported symbol (e.g.
          -- @Data.Map.Strict.insertWith@, defined in
          -- @Data.Map.Strict.Internal@) is otherwise only searchable
          -- under its @.Internal@ definition site.  Surface it under the
          -- module that exposes it, the way Haddock lists it.
          reexport  = reexportRows compKey flatLocal
                        [ (m, Locate.parseExports s) | (m, _f, s) <- loaded ]
          flatRows  = map provisionalRow (flatLocal ++ reexport)
          indexed   = map indexedRowOf flatRows
      -- Persist before publishing into memory so a crash mid-stream
      -- never leaves the in-memory view ahead of the cache.
      Cache.writeCachedIndex cache (originFor pid) compKey verT flatRows
      indexed `seq`
        IORef.atomicModifyIORef' ref (\old -> (indexed ++ old, ()))

    -- | Resolve a module against an explicit list of source roots, in
    -- priority order, and read its source.  'Nothing' when no root
    -- contains the module file.
    loadModuleSrc srcDirs modPath = do
      mFile <- firstExistingModule srcDirs modPath
      case mFile of
        Nothing -> pure Nothing
        Just f  -> do
          src <- TIO.readFile f
          pure (Just (modPath, f, src))

    firstExistingModule [] _ = pure Nothing
    firstExistingModule (r:rs) modPath = do
      let candidate = r FP.</> Text.unpack (Text.replace "." "/" modPath) <> ".hs"
      ok <- Dir.doesFileExist candidate
      if ok then pure (Just candidate) else firstExistingModule rs modPath

-- | Adapt a legacy @(component, module, name, sig)@ tuple to an
-- 'IndexRow'.
--
-- Provisional on both new fields: this indexer cannot say where a
-- re-exported symbol is defined (that is exactly the defect
-- "Hypha.Search.Reexport" exists to fix) and does not yet read
-- @other-modules@.  Rows written here are generation-2 rows carrying
-- generation-1 knowledge, and the rewrite two commits from now replaces
-- this function along with the tuple pipeline feeding it.
provisionalRow :: (Text, Text, Text, Text) -> IndexRow
provisionalRow (comp, modPath, name, sig) = IndexRow
  { rowComponent  = ComponentKey comp
  , rowModule     = ModulePath modPath
  , rowName       = SymbolName name
  , rowSignature  = Signature sig
  , rowDefModule  = ModulePath modPath
  , rowVisibility = Exposed
  }

-- | An 'IndexRow' as the in-memory scorer wants it.
indexedRowOf :: IndexRow -> Fuzzy.IndexedRow
indexedRowOf r = Fuzzy.mkIndexedRow
  (unComponentKey (rowComponent r))
  (unModulePath   (rowModule r))
  (unSymbolName   (rowName r))
  (unSignature    (rowSignature r))

-- | Extract one cache row per top-level declaration from a single
-- module's source.  Signatures land in the @sig@ column courtesy of
-- "Hypha.Source.Parser", so a tier-1 lookup is self-sufficient and the
-- agent no longer needs a follow-up @hypha symbol@ just to learn the
-- type.
--
-- The export-list filter is best-effort: when the module has an
-- explicit @module M (a, b, ...) where@ header we restrict to those
-- names; otherwise (no header, or 'Locate.parseExports' could not read
-- one) we emit every top-level decl.  Over-inclusion is harmless for
-- the search index — internal names still resolve, and the agent sees
-- exactly the providers it would see today.
--
-- Some packages guard code with build-time-only CPP macros (e.g.
-- @#error "CURRENT_PACKAGE_KEY undefined"@, only ever defined by a
-- real GHC invocation) that "Hypha.Source.Parser" can never satisfy —
-- it has no compiler session to ask.  That is an inherent limit of
-- parsing without compiling, not something a smarter cpphs config can
-- fix.  So this forces the parse eagerly and catches any exception
-- (the CPP failure surfaces as a plain 'error' call deep inside
-- @cpphs@) at the single-module granularity: one unparseable module
-- loses its own rows, but 'buildAndCacheIndex' keeps indexing every
-- other module and package in the plan instead of aborting outright.
collectModuleRows :: Text -> Text -> FilePath -> Text -> IO [(Text, Text, Text, Text)]
collectModuleRows compKey modPath f src = do
  result <- try (evaluate rows)
  case result of
    Left (e :: SomeException) -> do
      hPutStrLn stderr $
        "warning: index build skipped module " <> Text.unpack modPath
          <> " (" <> Text.unpack compKey <> "): " <> Text.unpack (briefException e)
      pure []
    Right rs -> pure rs
  where
    decls    = either (const []) id (Parser.parseDecls f src)
    exps     = Set.fromList (Locate.parseExports src)
    keep nm  = Set.null exps || nm `Set.member` exps
    sigFor d = case Parser.declSigText src d of
                 Just t  -> t
                 Nothing -> Text.empty
    rows = [ (compKey, modPath, nm, sigFor d)
           | d <- decls
           , let nm = Parser.declName d
           , not (Text.null nm)
           , keep nm
           ]

-- | Flagship re-export rows for a component.
--
-- A module often re-exports symbols it does not itself declare — the
-- @containers@ public modules (@Data.Map.Strict@, ...) re-export nearly
-- everything from an @.Internal@ sibling.  'collectModuleRows' only
-- emits rows for locally-declared symbols, so those re-exports are only
-- searchable under the @.Internal@ definition site, and a search for
-- @insertWith@ lands the user on @Data.Map.Strict.Internal@ instead of
-- the module Haddock documents it under.
--
-- Given every local row already collected for the component and each
-- module's export list, this emits, per module, one row for each name
-- the module exports but does not declare, resolved to the signature
-- from wherever the component defines it.  Names the component never
-- declares (cross-package re-exports) are skipped — we have no
-- signature for them and the definition lives in another index entry.
reexportRows
  :: Text                             -- ^ component key
  -> [(Text, Text, Text, Text)]       -- ^ local rows: (compKey, module, name, sig)
  -> [(Text, [Text])]                 -- ^ (module, its export list) for every module
  -> [(Text, Text, Text, Text)]
reexportRows compKey local modExports =
  [ (compKey, modPath, nm, sig)
  | (modPath, exps) <- modExports
  , let localNames = Map.findWithDefault Set.empty modPath localByMod
  , nm  <- Set.toList (Set.fromList exps `Set.difference` localNames)
  , Just sig <- [Map.lookup nm defs]
  ]
  where
    -- Any component-local definition of a name gives us its signature;
    -- same-named re-exports (lazy vs strict @insertWith@) share it.
    defs       = Map.fromList [ (nm, sig) | (_, _, nm, sig) <- local ]
    localByMod = Map.fromListWith Set.union
                   [ (m, Set.singleton nm) | (_, m, nm, _) <- local ]

-- | Pick the source roots to scan for a package.  If any of the common
-- @hs-source-dirs@ subdirectories exist we walk those exclusively;
-- otherwise we fall back to the package root.  Walking both root /and/
-- the @src/@ subtree double-counts modules and produces duplicate
-- "src.Foo.Bar" / "Foo.Bar" rows in the search index.
chooseSourceRoots :: FilePath -> IO [FilePath]
chooseSourceRoots d = do
  let candidates = [ d FP.</> sub
                   | sub <- ["src", "library", "lib", "Library", "source", "Source"] ]
  existingSubs <- filterExisting candidates
  pure (if null existingSubs then [d] else existingSubs)

filterExisting :: [FilePath] -> IO [FilePath]
filterExisting [] = pure []
filterExisting (p : ps) = do
  ok <- Dir.doesDirectoryExist p
  rest <- filterExisting ps
  pure (if ok then p : rest else rest)

findHs :: FilePath -> Int -> IO [FilePath]
findHs _ depth | depth < 0 = pure []
findHs dir depth = do
  entries <- Dir.listDirectory dir
  let absEntries = map (dir FP.</>) entries
  concat <$> mapM (visit depth) absEntries
  where
    visit d p = do
      isDir <- Dir.doesDirectoryExist p
      if isDir
        then if skipDir (FP.takeFileName p)
               then pure []
               else findHs p (d - 1)
        else if ".hs" `Text.isSuffixOf` Text.pack p
               then pure [p]
               else pure []

    skipDir name = case name of
      '.':_ -> True
      "dist" -> True
      "dist-newstyle" -> True
      "test" -> True
      "tests" -> True
      "bench" -> True
      "benchmarks" -> True
      "Setup" -> True
      _ -> False

hsToModule :: FilePath -> String
hsToModule fp =
  let stripped = case Text.stripSuffix ".hs" (Text.pack fp) of
                   Just t  -> Text.unpack t
                   Nothing -> fp
      dotted   = map (\c -> if c == '/' then '.' else c) stripped
  in dotted

-- | First line of an exception's rendering.  A cpphs failure carries a
-- multi-line dump; one line is enough to identify which module lost its
-- rows and why.
briefException :: SomeException -> Text
briefException =
  Text.strip . Text.takeWhile (/= '\n') . Text.pack . show
