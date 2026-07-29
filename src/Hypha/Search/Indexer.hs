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
  , Hydrated (..)
  , hydrateFromCache
    -- * The pure core
  , ComponentIndex (..)
  , OutsideExport (..)
  , indexComponentPure
    -- * Component discovery
  , componentModules
  , languageSettingsFor
  , packageSources
  , enumModulesIn
  , chooseSourceRoots
  ) where

import Control.Exception qualified as Exception
import Control.Monad (foldM, void)
import Data.IORef qualified as IORef
import Data.Map.Strict qualified as Map
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
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
import Hypha.Search.Exports
  (Export (..), ExportChoice (..), ExportEnv, lookupExport)
import Hypha.Search.Exports qualified as Exports
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
componentsForUnit plan pid d = case componentsOf plan pid of
  Just cs -> pure [ (Comp.ciKind c, Comp.ciHsSourceDirs c) | c <- cs ]
  Nothing -> do
    roots <- chooseSourceRoots d
    pure [(Comp.MainLib, roots)]

-- | The components the plan records for a unit, or 'Nothing' when it
-- records none — every store dependency, whose @pkg-src@ we never parsed.
--
-- One derivation, because the build pass and the hydrate pass are two
-- halves of the same decision: they must agree on which component keys a
-- unit has, or hydration looks for keys the build never wrote.
componentsOf :: BuildPlan -> PackageId -> Maybe [Comp.ComponentInfo]
componentsOf plan pid = case lookupUnit (pkgName pid) plan of
  Just pu | not (null (puLibComponents pu)) -> Just (puLibComponents pu)
  _                                         -> Nothing

-- | The component kinds a unit has, mirroring 'componentsForUnit' without
-- needing a source directory: hydration works off the cache alone.
componentKindsOf :: BuildPlan -> PackageId -> NonEmpty Comp.ComponentKind
componentKindsOf plan pid = case componentsOf plan pid of
  Just (c : cs) -> Comp.ciKind c :| map Comp.ciKind cs
  _             -> Comp.MainLib :| []

-- | What hydration recovered: the exports of every component it loaded,
-- and the units it could not.
--
-- The environment is returned rather than rebuilt later because the
-- background pass needs it before it indexes anything: a warm cache
-- holding @ghc-internal@ is exactly how @base@ becomes resolvable in a run
-- that only rebuilds @base@.
data Hydrated = Hydrated
  { hyEnv     :: !ExportEnv
  , hyMissing :: ![PackageId]
  }

-- | Pull every cached component index into the in-memory ref.  A unit
-- counts as "fully hydrated" only when /every/ one of its components
-- has cached rows; otherwise it's reported as missing so the
-- background indexer rebuilds the whole set.
hydrateFromCache
  :: BuildPlan
  -> Cache.HyphaPackageCache
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IO Hydrated
hydrateFromCache plan cache pids ref = go Exports.emptyEnv [] pids
  where
    go env missing [] = pure Hydrated
      { hyEnv     = env
      , hyMissing = reverse missing
      }
    go env missing (pid : rest) = do
      let verT = unVersion (pkgVersion pid)
          keys = [ unComponentKey (componentKeyOf (pkgName pid) k)
                 | k <- NE.toList (componentKindsOf plan pid) ]
      hits <- mapM (\k -> Cache.haveCachedIndex cache k verT) keys
      if and hits
        then do
          env' <- foldM (loadKey verT) env keys
          -- Once per unit, not once per component key.
          publishRows ref [Fuzzy.mkPackageRow (pkgName pid) (pkgVersion pid)]
          go env' missing rest
        else go env (pid : missing) rest

    loadKey verT env k = do
      rows <- Cache.readCachedIndex cache k verT
      publishRows ref (componentScorerRows rows)
      pure (Exports.extendEnv rows env)


-- | Walk the source trees of the given packages, extract their module
-- exports, persist the result to the cache, and prepend them to the
-- in-memory ref.  Packages whose source cannot be resolved are skipped,
-- with a reason on stderr — the index is a best-effort fallback, not a
-- silent one.
--
-- Units are walked dependencies-first and each component's rows extend the
-- environment the next one resolves against.  That order is a correctness
-- requirement, not a performance one: @base@ has no signature for
-- @mapAccumL@ until @ghc-internal@ has been indexed.
--
-- Per-module rows are built fully /outside/ the atomicModifyIORef'
-- critical section; prepending makes each insert O(|rows|) instead of
-- the O(|index|) behaviour of @old ++ rows@.
buildAndCacheIndex
  :: BuildPlan
  -> Cache.HyphaPackageCache
  -> PackageResolver IO
  -> ExportEnv                          -- ^ what the warm cache already supplies
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IORef.IORef Int                    -- ^ packages-done counter
  -> IO ()
buildAndCacheIndex plan cache resolver env0 pids ref doneRef =
  void (foldM indexUnit env0 (topologicalOrder plan pids))
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

    indexUnit env pid = do
      eDir <- resolveSrc resolver pid
      case eDir of
        Left err -> do
          hPutStrLn stderr $
            "hypha index: no source for "
              <> Text.unpack (unPackageName (pkgName pid)) <> ": " <> show err
          bump
          pure env
        Right d -> do
          comps <- componentsForUnit plan pid d
          env'  <- foldM (indexComponent pid) env comps
          publishRows ref [Fuzzy.mkPackageRow (pkgName pid) (pkgVersion pid)]
          bump
          pure env'

    indexComponent pid env (kind, srcDirs) = do
      let pkgT    = unPackageName (pkgName    pid)
          verT    = unVersion    (pkgVersion pid)
          compKey = componentKeyOf (PackageName pkgT) kind
          langs   = languageSettingsFor plan (pkgName pid) kind
      sources <- componentModules plan pid kind srcDirs
      parsed   <- Interface.parseSources langs sources
      let ci       = indexParsedComponent compKey (dependencySet plan pid) env parsed
          flatRows = ciRows ci
      reportComponentIndex compKey ci
      -- Persist before publishing into memory so a crash mid-stream
      -- never leaves the in-memory view ahead of the cache.
      Cache.writeCachedIndex cache (originFor pid)
        (unComponentKey compKey) verT flatRows
      publishRows ref (componentScorerRows flatRows)
      pure (Exports.extendEnv flatRows env)

-- | The packages a unit may resolve a re-export through: its dependencies,
-- plus its own name.
--
-- Its own name is in there because a sub-library re-exporting from the
-- package's main library crosses a /component/ boundary without crossing a
-- package one, and 'lookupExport' filters on package names.
dependencySet :: BuildPlan -> PackageId -> Set PackageName
dependencySet plan pid =
  Set.insert (pkgName pid) $ case lookupUnit (pkgName pid) plan of
    Nothing -> Set.empty
    Just u  -> Set.fromList (map pkgName (puDeps u))

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

-- | An export the component does not itself declare.
--
-- Named rather than tupled because all three fields are module paths or
-- close to it, and a bare triple at a report site is unreadable.
data OutsideExport = OutsideExport
  { oeModule   :: !ModulePath   -- ^ the module that exports it
  , oeName     :: !SymbolName
  , oeExpected :: !ModulePath   -- ^ the import we believe supplies it
  }
  deriving stock (Show, Eq, Ord)

-- | What indexing one component produced, and what it could not.
data ComponentIndex = ComponentIndex
  { ciRows          :: ![IndexRow]
  , ciParseFailures :: ![(ModulePath, Parser.ParseError)]
  , ciNameMismatch  :: ![(ModulePath, ModulePath)]
    -- ^ @(name the stanza expected, name the source declares)@.  Real in
    -- the wild, and silently trusting either side produces rows nobody
    -- can reach.
  , ciUnresolved    :: ![OutsideExport]
    -- ^ Exports whose definition lives outside the component and which no
    -- indexed dependency could supply.  A symbol missing from the index is
    -- invisible, and the user has no other way to find out.
  , ciAmbiguous     :: ![(OutsideExport, ExportChoice)]
    -- ^ Exports more than one dependency could have supplied.  Resolved,
    -- deterministically, and worth saying so.
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
  -> Set PackageName          -- ^ the packages a re-export may resolve through
  -> ExportEnv                -- ^ what the components indexed so far export
  -> LanguageSettings
  -> [ModuleSource]
  -> ComponentIndex
indexComponentPure compKey deps env langs sources =
  indexParsedComponent compKey deps env
    [ (ms, Interface.parseInterface langs (msPath ms) (msContent ms))
    | ms <- sources
    ]

-- | The core, over parse results the caller obtained.
--
-- Parsing is the caller's job because it is where the exceptions are: see
-- 'Interface.parseInterfaceIO'.  Everything from here on is a function of
-- the sources and of what the dependencies exported.
indexParsedComponent
  :: ComponentKey
  -> Set PackageName
  -> ExportEnv
  -> [(ModuleSource, Either Parser.ParseError ModuleInterface)]
  -> ComponentIndex
indexParsedComponent compKey deps env parsed = ComponentIndex
  { ciRows          = localRows ++ outsideRows
  , ciParseFailures = failures
  , ciNameMismatch  = mismatches
  , ciUnresolved    = unresolved
  , ciAmbiguous     = ambiguous
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
    -- resolution map uses.  One map rather than three with identical key
    -- sets: the two 'findWithDefault' defaults the split version needed
    -- could never fire, so a reader had to reconstruct that argument to
    -- know the empty signature was not a sentinel.
    byModule :: Map.Map ModulePath (ModuleSource, ModuleInterface)
    byModule = Map.fromList [ (miName i, (ms, i)) | (ms, i) <- ok ]

    resolved = Map.toList (Reexport.resolveComponent ifaces)

    visibilityFor presented =
      maybe Internal (msVisibility . fst) (Map.lookup presented byModule)

    -- Exports this component declares, here or in a sibling module.
    localRows =
      [ IndexRow
          { rowComponent  = compKey
          , rowModule     = presented
          , rowName       = name
          , rowSignature  = sig
          , rowDefinition = DefinitionRef compKey defMod
          , rowVisibility = visibilityFor presented
          }
      | ((presented, name), res) <- resolved
      , defMod <- insideSite presented (resSite res)
      , Just (defSrc, defIface) <- [Map.lookup defMod byModule]
        -- The signature is read from the module the resolver landed on.
        -- Looking it up in a component-wide name map is what published
        -- Data.IntMap.Lazy.insertWith with Data.Map's signature.
      , Just decl <- [Parser.findDecl (unSymbolName name) (miDecls defIface)]
      , let sig = Signature
              (fromMaybe "" (Parser.declSigText (msContent defSrc) decl))
      ]

    -- Exports whose definition is in a dependency.  A site naming the
    -- asking module itself is "Hypha.Search.Reexport"'s "no import
    -- plausibly supplies this" fallback, not a claim about a dependency:
    -- looking it up would match any dependency exposing a module of the
    -- same name, so it goes straight to the unresolved report.
    (selfNamed, outside) =
      partition (\oe -> oeModule oe == oeExpected oe)
        [ OutsideExport presented name m
        | ((presented, name), DefinedOutside m) <- map (fmap resSite) resolved
        ]

    classified = [ (oe, lookupExport deps (oeExpected oe) (oeName oe) env)
                 | oe <- outside
                 ]

    outsideRows =
      [ IndexRow
          { rowComponent  = compKey
          , rowModule     = oeModule oe
          , rowName       = oeName oe
          , rowSignature  = exSignature (ecChosen ch)
          , rowDefinition = exDefinition (ecChosen ch)
          , rowVisibility = visibilityFor (oeModule oe)
          }
      | (oe, Just ch) <- classified
      ]

    unresolved = selfNamed ++ [ oe | (oe, Nothing) <- classified ]

    ambiguous =
      [ (oe, ch)
      | (oe, Just ch) <- classified
      , not (null (ecRejected ch))
      ]

-- | The definition module when it is inside this component, and nothing
-- when it is not.  A list rather than a 'Maybe' so it drops straight into
-- the row comprehension.
insideSite :: ModulePath -> DefinitionSite -> [ModulePath]
insideSite asking site = case site of
  DefinedHere      -> [asking]
  DefinedIn m      -> [m]
  DefinedOutside _ -> []

-- | Trace what a component's index pass could not do.  Never silent: a
-- module or symbol missing from the index is invisible to search, and the
-- user has no other way to find out.
reportComponentIndex :: ComponentKey -> ComponentIndex -> IO ()
reportComponentIndex compKey ci = do
  mapM_ reportFailure    (ciParseFailures ci)
  mapM_ reportMismatch   (ciNameMismatch ci)
  mapM_ reportUnresolved (ciUnresolved ci)
  mapM_ reportAmbiguous  (ciAmbiguous ci)
  where
    label = Text.unpack (unComponentKey compKey)

    reportUnresolved oe = hPutStrLn stderr $
      "hypha index: " <> label <> " could not resolve "
        <> Text.unpack (unModulePath (oeModule oe)) <> "."
        <> Text.unpack (unSymbolName (oeName oe))
        <> " through " <> Text.unpack (unModulePath (oeExpected oe))
        <> "; no indexed dependency exports it"

    reportAmbiguous (oe, ch) = hPutStrLn stderr $
      "hypha index: " <> label <> " resolved "
        <> Text.unpack (unModulePath (oeModule oe)) <> "."
        <> Text.unpack (unSymbolName (oeName oe)) <> " to "
        <> renderRef (exDefinition (ecChosen ch)) <> ", rejecting "
        <> unwords (map renderRef (ecRejected ch))

    renderRef r =
      Text.unpack (unComponentKey (drComponent r)) <> ":"
        <> Text.unpack (unModulePath (drModule r))

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
  case componentInfoFor plan (pkgName pid) kind of
    Just ci
      | not (null (stanzaModules ci)) -> load (stanzaModules ci)
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

-- | The modules a cabal stanza names, with the visibility it gives them.
stanzaModules :: Comp.ComponentInfo -> [(Text, Visibility)]
stanzaModules ci =
  [ (m, Exposed)  | m <- Comp.ciExposedModules ci ]
    ++ [ (m, Internal) | m <- Comp.ciOtherModules ci ]

-- | Every library component of a package, read straight from its cabal
-- file without a build plan.
--
-- The CLI's @hypha source@ has a package directory and no plan, but still
-- needs the component's module list to resolve a re-export: without it the
-- only option is the package-wide sweep, which is a guess.
--
-- An empty result is always announced: 'Comp.parseLibComponents' returns
-- @[]@ both for a cabal file we cannot read and for one that names no
-- library, and a caller told only "no components" would silently fall
-- back to the sweep it was written to avoid.
packageSources :: FilePath -> IO [(Comp.ComponentInfo, [ModuleSource])]
packageSources pkgRoot = do
  mCabal <- Comp.findCabalFile pkgRoot
  case mCabal of
    Nothing    -> do
      report "has no cabal file"
      pure []
    Just cabal -> do
      comps <- Comp.parseLibComponents cabal pkgRoot
      if null comps
        then do
          report ("cabal file " <> cabal <> " named no library component")
          pure []
        else mapM withSources comps
  where
    report why = hPutStrLn stderr $
      "hypha: " <> pkgRoot <> " " <> why
        <> "; a symbol can only be located by sweeping the package"

    withSources ci = do
      srcs <- loadModuleSources (Comp.ciHsSourceDirs ci) (stanzaModules ci)
      pure (ci, srcs)

-- | The parsed cabal component matching a kind, when we have one.
--
-- Keyed on the package /name/: 'lookupUnit' is, and a caller holding only
-- a component key (the server, from a URL) has no version to offer.
componentInfoFor
  :: BuildPlan -> PackageName -> Comp.ComponentKind -> Maybe Comp.ComponentInfo
componentInfoFor plan pkg kind = do
  pu <- lookupUnit pkg plan
  case [ c | c <- puLibComponents pu, Comp.ciKind c == kind ] of
    (c : _) -> Just c
    []      -> Nothing

-- | The language settings a component fixes for its modules.  Without them
-- a module relying on a stanza-wide extension parses differently for us
-- than for the compiler.
--
-- The server calls this too, on the way to a module page: two derivations
-- would mean the page could parse a module differently from the way the
-- index did, which is the divergence this layer exists to remove.
languageSettingsFor
  :: BuildPlan -> PackageName -> Comp.ComponentKind -> LanguageSettings
languageSettingsFor plan pkg kind =
  case componentInfoFor plan pkg kind of
    Just ci -> Comp.ciLanguageSettings ci
    Nothing -> Extensions.defaultLanguageSettings

-- | Everything one /component's/ rows contribute to the in-memory scorer:
-- one row per symbol, plus the module rows they imply.
--
-- Both the build path and the hydrate-from-cache path go through here, so
-- a warm cache and a fresh index cannot disagree about which entities are
-- searchable.  The package row is published once per unit by the caller,
-- since a package is not a per-component fact.
componentScorerRows :: [IndexRow] -> [Fuzzy.IndexedRow]
componentScorerRows rows =
  Fuzzy.moduleRows rows ++ map Fuzzy.mkSymbolRow rows

-- | Publish rows into the scorer's shared view.
--
-- The spine is forced before the critical section, not after: the rows are
-- built per module and 'seq' on the list only forced its first cell, so the
-- lowercasing every 'Fuzzy.indexedRow' does was still a thunk the /search/
-- thread paid for on the first query.
publishRows :: IORef.IORef [Fuzzy.IndexedRow] -> [Fuzzy.IndexedRow] -> IO ()
publishRows ref rows = do
  n <- Exception.evaluate (length rows)
  n `seq` IORef.atomicModifyIORef' ref (\old -> (rows ++ old, ()))

