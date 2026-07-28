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
  ( -- * Building
    buildAndCacheIndex
  , hydrateFromCache
    -- * The pure core
  , ComponentIndex (..)
  , indexComponentPure
  , indexParsedComponent
    -- * Component discovery
  , componentsForUnit
  , componentModules
  , packageSources
  , loadModuleSources
  , enumModulesIn
  , chooseSourceRoots
  ) where

import Data.IORef qualified as IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
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
  (DefinitionRef (..), IndexRow (..), ModuleSource (..), Visibility (..))
import Hypha.Search.Reexport (DefinitionSite (..), Resolution (..))
import Hypha.Search.Reexport qualified as Reexport
import Hypha.Source.Extensions (LanguageSettings)
import Hypha.Source.Extensions qualified as Extensions
import Hypha.Source.Interface (ModuleInterface (..))
import Hypha.Source.Interface qualified as Interface
import Hypha.Search.PackageCache (CacheOrigin (..))
import Hypha.Search.PackageCache qualified as Cache
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
              mapM_ (loadKey pid verT) keys
              go missing rest
            else go (pid : missing) rest

    loadKey pid verT k = do
      rows <- Cache.readCachedIndex cache k verT
      let indexed = scorerRows (pkgName pid) (pkgVersion pid) rows
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
          compKey = componentKeyOf (PackageName pkgT) kind
          langs   = languageSettingsFor plan pid kind
      sources <- componentModules plan pid kind srcDirs
      parsed   <- mapM (parseGuarded langs) sources
      let ci       = indexParsedComponent compKey parsed
          flatRows = ciRows ci
          indexed  = scorerRows (pkgName pid) (pkgVersion pid) flatRows
      reportComponentIndex compKey ci
      -- Persist before publishing into memory so a crash mid-stream
      -- never leaves the in-memory view ahead of the cache.
      Cache.writeCachedIndex cache (originFor pid)
        (unComponentKey compKey) verT flatRows
      indexed `seq`
        IORef.atomicModifyIORef' ref (\old -> (indexed ++ old, ()))

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

-- The pure core ------------------------------------------------------

-- | What indexing one component produced, and what it could not.
data ComponentIndex = ComponentIndex
  { ciRows          :: ![IndexRow]
  , ciParseFailures :: ![(ModulePath, Parser.ParseError)]
  , ciNameMismatch  :: ![(ModulePath, ModulePath)]
    -- ^ @(name the stanza expected, name the source declares)@.  Real in
    -- the wild, and silently trusting either side produces rows nobody
    -- can reach.
  }
  deriving stock (Show, Eq)

-- | Build a component's rows.
--
-- Pure, because every judgement in here — what a module is called, which
-- module defines a symbol, which signature it carries — is a function of
-- the sources.  Mixing those judgements with file IO is what let the old
-- indexer paper over a parse failure: it logged the failure, then let the
-- re-export pass invent rows for the module anyway, resolving their
-- signatures through a name-keyed map.
indexComponentPure
  :: ComponentKey
  -> LanguageSettings
  -> [ModuleSource]
  -> ComponentIndex
indexComponentPure compKey langs sources = indexParsedComponent compKey
  [ (ms, Interface.parseInterface langs (msPath ms) (msContent ms))
  | ms <- sources
  ]

-- | The core, over parse results the caller obtained.
--
-- Parsing is the caller's job because it is where the exceptions are: see
-- 'Interface.parseInterfaceIO'.  Everything from here on is a function of
-- the sources.
indexParsedComponent
  :: ComponentKey
  -> [(ModuleSource, Either Parser.ParseError ModuleInterface)]
  -> ComponentIndex
indexParsedComponent compKey parsed = ComponentIndex
  { ciRows          = rows
  , ciParseFailures = failures
  , ciNameMismatch  = mismatches
  }
  where
    failures   = [ (msDeclaredName ms, e) | (ms, Left e)  <- parsed ]
    ok         = [ (ms, i)                | (ms, Right i) <- parsed ]
    mismatches =
      [ (msDeclaredName ms, miName i)
      | (ms, i) <- ok
      , msDeclaredName ms /= miName i
      ]

    ifaces = map snd ok

    -- Keyed on the name the source declares, which is the same key the
    -- resolution map uses.
    visibilityOf = Map.fromList [ (miName i, msVisibility ms) | (ms, i) <- ok ]
    ifaceOf      = Map.fromList [ (miName i, i)               | (_,  i) <- ok ]
    contentOf    = Map.fromList [ (miName i, msContent ms)    | (ms, i) <- ok ]

    rows =
      [ IndexRow
          { rowComponent  = compKey
          , rowModule     = presented
          , rowName       = name
          , rowSignature  = sig
          , rowDefinition = DefinitionRef compKey defMod
          , rowVisibility = Map.findWithDefault Internal presented visibilityOf
          }
      | ((presented, name), res) <- Map.toList (Reexport.resolveComponent ifaces)
      , not (isDefinedOutside (resSite res))
      , let defMod = Reexport.definitionModule presented (resSite res)
      , Just defIface <- [Map.lookup defMod ifaceOf]
        -- The signature is read from the module the resolver landed on.
        -- Looking it up in a component-wide name map is what published
        -- Data.IntMap.Lazy.insertWith with Data.Map's signature.
      , Just decl <- [Parser.findDecl (unSymbolName name) (miDecls defIface)]
      , let src = Map.findWithDefault "" defMod contentOf
      , let sig = Signature (maybe "" id (Parser.declSigText src decl))
      ]

-- | A symbol the component does not define gets no row: we have no
-- signature for it, and its definition belongs to another index entry.
isDefinedOutside :: DefinitionSite -> Bool
isDefinedOutside site = case site of
  DefinedOutside{} -> True
  DefinedHere      -> False
  DefinedIn{}      -> False

-- | Trace what a component's index pass could not do.  Never silent: a
-- module missing from the index is invisible to search, and the user has
-- no other way to find out.
reportComponentIndex :: ComponentKey -> ComponentIndex -> IO ()
reportComponentIndex compKey ci = do
  mapM_ reportFailure  (ciParseFailures ci)
  mapM_ reportMismatch (ciNameMismatch ci)
  where
    label = Text.unpack (unComponentKey compKey)

    reportFailure (m, e) = hPutStrLn stderr $
      "hypha index: " <> label <> " skipped module "
        <> Text.unpack (unModulePath m) <> ": "
        <> Text.unpack (Parser.parseErrorMessage e)

    reportMismatch (declared, actual) = hPutStrLn stderr $
      "hypha index: " <> label <> " expected module "
        <> Text.unpack (unModulePath declared) <> " but its source declares "
        <> Text.unpack (unModulePath actual) <> "; using the latter"

-- | The modules of one component, as the cabal stanza lists them.
--
-- Enumeration from @exposed-modules@ + @other-modules@ is what keeps a
-- stray script under a source dir from becoming a module: the filesystem
-- walk that used to do this turned @examples/race.hs@ into a module called
-- @race@.  The walk survives only for components whose cabal we could not
-- parse, and says so when it fires.
componentModules
  :: BuildPlan
  -> PackageId
  -> Comp.ComponentKind
  -> [FilePath]
  -> IO [ModuleSource]
componentModules plan pid kind srcDirs =
  case componentInfoFor plan pid kind of
    Just ci
      | not (null (Comp.ciExposedModules ci) && null (Comp.ciOtherModules ci)) ->
          load ([ (m, Exposed)    | m <- Comp.ciExposedModules ci ]
                  ++ [ (m, Internal) | m <- Comp.ciOtherModules ci ])
    _ -> do
      hPutStrLn stderr $
        "hypha index: " <> Text.unpack (unPackageName (pkgName pid))
          <> " has no cabal module list; falling back to a source-dir walk"
      walked <- enumModulesIn srcDirs
      load [ (m, Exposed) | m <- walked ]
  where
    load = loadModuleSources srcDirs

-- | Read each named module from the first source dir that has it.
--
-- Modules the stanza names but whose file we cannot find are dropped: a
-- @.hsc@ or @.chs@ source we do not preprocess is a real case, and it is
-- the module's rows we lose, not the component's.
loadModuleSources :: [FilePath] -> [(Text, Visibility)] -> IO [ModuleSource]
loadModuleSources srcDirs = fmap catMaybes . mapM loadOne
  where
    loadOne (modPath, vis) = do
      mFile <- firstExistingModule srcDirs modPath
      case mFile of
        Nothing -> pure Nothing
        Just f  -> do
          content <- TIO.readFile f
          pure (Just ModuleSource
            { msDeclaredName = ModulePath modPath
            , msPath         = f
            , msVisibility   = vis
            , msContent      = content
            })

    firstExistingModule [] _ = pure Nothing
    firstExistingModule (r : rs) modPath = do
      let candidate = r FP.</> Text.unpack (Text.replace "." "/" modPath) <> ".hs"
      ok <- Dir.doesFileExist candidate
      if ok then pure (Just candidate) else firstExistingModule rs modPath

-- | Every library component of a package, read straight from its cabal
-- file without a build plan.
--
-- The CLI's @hypha source@ has a package directory and no plan, but still
-- needs the component's module list to resolve a re-export: without it the
-- only option is the package-wide sweep, which is a guess.
packageSources :: FilePath -> IO [(Comp.ComponentInfo, [ModuleSource])]
packageSources pkgRoot = do
  mCabal <- Comp.findCabalFile pkgRoot
  case mCabal of
    Nothing    -> pure []
    Just cabal -> do
      comps <- Comp.parseLibComponents cabal pkgRoot
      mapM withSources comps
  where
    withSources ci = do
      srcs <- loadModuleSources (Comp.ciHsSourceDirs ci)
        ([ (m, Exposed)  | m <- Comp.ciExposedModules ci ]
           ++ [ (m, Internal) | m <- Comp.ciOtherModules ci ])
      pure (ci, srcs)

-- | The parsed cabal component matching a kind, when we have one.
componentInfoFor
  :: BuildPlan -> PackageId -> Comp.ComponentKind -> Maybe Comp.ComponentInfo
componentInfoFor plan pid kind = do
  pu <- lookupUnit (pkgName pid) plan
  case [ c | c <- puLibComponents pu, Comp.ciKind c == kind ] of
    (c : _) -> Just c
    []      -> Nothing

-- | The language settings a component fixes for its modules.  Without them
-- a module relying on a stanza-wide extension parses differently for us
-- than for the compiler.
languageSettingsFor
  :: BuildPlan -> PackageId -> Comp.ComponentKind -> LanguageSettings
languageSettingsFor plan pid kind =
  case componentInfoFor plan pid kind of
    Just ci -> Comp.ciLanguageSettings ci
    Nothing -> Extensions.defaultLanguageSettings

-- | Everything a package's rows contribute to the in-memory scorer: one
-- row per symbol, plus the package and module rows they imply.
--
-- Both the build path and the hydrate-from-cache path go through here, so
-- a warm cache and a fresh index cannot disagree about which entities are
-- searchable.
scorerRows :: PackageName -> Version -> [IndexRow] -> [Fuzzy.IndexedRow]
scorerRows pkg ver rows =
  Fuzzy.entityRows pkg ver rows ++ map Fuzzy.mkSymbolRow rows

-- | Parse one module, catching the exception cpphs raises for macros only
-- a real compiler defines.  One module loses its rows; the pass continues.
parseGuarded
  :: LanguageSettings
  -> ModuleSource
  -> IO (ModuleSource, Either Parser.ParseError ModuleInterface)
parseGuarded langs ms = do
  r <- Interface.parseInterfaceIO langs (msPath ms) (msContent ms)
  pure (ms, r)
