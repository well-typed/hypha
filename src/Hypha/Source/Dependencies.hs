{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The modules of a package's dependency closure, indexed on demand.
--
-- @hypha source base\/Data.List\/sortOn@ has to leave @base@ to answer:
-- @Data.List@ re-exports @sortOn@ from @GHC.Internal.Data.List@, which
-- re-exports it from @GHC.Internal.Data.OldList@, and both hops cross into
-- @ghc-internal@.  Since GHC 9.10 made @base@ a facade over
-- @ghc-internal@, that is not an edge case but the shape of every @base@
-- symbol anyone asks about.
--
-- @hypha server@ already crosses those boundaries, because its index has
-- transitively resolved definition sites and it loads exactly the modules
-- those sites name.  The CLI has no index; what it has is the build plan,
-- which names every dependency, and 'Hypha.Package.Resolver', which can
-- say where any of their sources already are.  This module is the bridge:
-- an 'OutsideReach' backed by the plan instead of by an index.
--
-- Two costs are deliberately not paid here.
--
-- A dependency is indexed only once a candidate names a module it might
-- have, and only once — answering @base@'s facades costs @ghc-internal@
-- and nothing else on a warm walk.
--
-- And the walk never fetches /speculatively/.  Probing "does any of these
-- two hundred units happen to declare @GHC.Internal.Data.OldList@?" is a
-- guess by nature, so it runs on 'resolveSrcLocal': what is already
-- unpacked answers, and what is not becomes a 'GapNoLocalSource' the
-- failure report can name.  Calling 'resolveSrc' there instead falls
-- through to a tarball extraction and then to Hackage over HTTP, so a
-- single missed module name walked the whole closure and downloaded it —
-- @hypha source base\/Prelude\/lines@ answered correctly and printed
-- @Hackage HTTP 404 for \'rts\'@ on the way.
--
-- Probing for free is not the same as never fetching, though, and
-- conflating the two is what made this walk answer only on a machine that
-- happened to have the right dependency unpacked already.  Nothing on this
-- path puts a dependency's source on disk: the cabal store keeps no
-- sources (no store entry has a @src@ directory) and @cabal build@ unpacks
-- nothing a query can read, so on a cold machine every cross-package chain
-- died at the first hop and @base\/Data.List\/sortOn@ — the whole point of
-- the exercise — reported @NOT_FOUND@.  So when the unpacked dependencies
-- are exhausted, 'Hypha.Source.Origins.ModuleOwnerOracle' says which unit
-- exposes the module (from the package databases, no source needed) and
-- that /one/ unit is fetched.  Speculation stays free; the commitment
-- pays, once, for the package that actually owns the answer.
module Hypha.Source.Dependencies
  ( dependencyReach
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import System.IO (hPutStrLn, stderr)

import Hypha.Error (errorMessage)
import Hypha.Package.Resolver (PackageResolver (..))
import Hypha.Project.Components qualified as Comp
import Hypha.Search.Index (ModuleSource (..), Visibility (..))
import Hypha.Search.Indexer qualified as Indexer
import Hypha.Source.Extensions (LanguageSettings, defaultLanguageSettings)
import Hypha.Source.Locate (findModuleFileIn)
import Hypha.Source.Origins (ModuleOwnerOracle (..), OriginError)
import Hypha.Source.Reach
  ( OutsideModule (..), OutsideReach (..), ReachGap (..) )
import Hypha.Types.BuildPlan (BuildPlan, PlannedUnit (..), lookupUnit)
import Hypha.Types.ComponentName (ComponentKey, componentKeyOf)
import Hypha.Types.PackageId (PackageId (..), PackageName, renderPackageId)
import Hypha.Types.SymbolPath (ModulePath (..))

-- | Where one module of one dependency lives, and how to read it.
--
-- Paths rather than contents: a dependency contributes a few hundred
-- entries and a search reads two of them, so holding every module's bytes
-- to answer one question would be the wrong trade.
data OutsideEntry = OutsideEntry
  { oeComponent  :: !ComponentKey
  , oeFile       :: !FilePath
  , oeVisibility :: !Visibility
  , oeLanguage   :: !LanguageSettings
  }

-- | What the walk has learned so far, and what it has left to try.
data Closure = Closure
  { clPending :: ![PackageId]
    -- ^ Dependencies not yet indexed, nearest first.
  , clModules :: !(Map ModulePath OutsideEntry)
    -- ^ Where each module of the dependencies already indexed lives.
  , clGaps    :: ![ReachGap]
    -- ^ What the walk could not read, newest first.
  , clFetched :: !(Set PackageName)
    -- ^ Units whose source the owner path has already tried to fetch.
    --
    -- One attempt each, and the record is kept even when it failed: the
    -- retry would fail the same way, and without it a module two chains
    -- both miss on would fetch twice.
  , clOracle  :: !(Maybe (Either OriginError (ModuleOwnerOracle IO)))
    -- ^ The owner oracle, once something has needed it.
    --
    -- 'Nothing' until then, because building it runs @ghc --numeric-version@
    -- to select the plan's compiler: a walk that answers from unpacked
    -- dependencies must not pay for a subprocess, and a machine with no
    -- matching compiler must not be told about it on a query that
    -- succeeded.
  }

-- | An 'OutsideReach' over the dependency closure of one package.
--
-- 'orSites' is empty: a definition site is a transitively resolved fact
-- and only the index has those.  Everything here is reached by following
-- imports, which is what "Hypha.Source.Locate" does with the modules this
-- hands it.
--
-- The owner oracle arrives as an action rather than a value so that
-- nothing is spent on it unless a lookup actually misses: it selects the
-- plan's compiler by running it.  Injectable for the same reason the
-- search bounds are — the fetch-the-owner path is untestable while the
-- only way to reach it is a machine-wide @ghc-pkg@.
dependencyReach
  :: BuildPlan
  -> PackageResolver IO
  -> IO (Either OriginError (ModuleOwnerOracle IO))
     -- ^ how to ask which unit owns a module, run at most once
  -> PackageId          -- ^ the package being asked about
  -> IO (OutsideReach IO)
dependencyReach plan resolver mkOracle pid = do
  let (deps, missing) = dependencyClosure plan pid
  ref <- newIORef Closure
    { clPending = deps
    , clModules = Map.empty
      -- The plan not knowing the package we were asked about is not the
      -- same as it having no dependencies, and the difference is the
      -- whole reach: every lookup below will miss, and without this the
      -- user would be told nothing was reachable and never why.
    , clGaps    = map GapUnitNotInPlan missing
    , clFetched = Set.empty
    , clOracle  = Nothing
    }
  pure OutsideReach
    { orSites  = Map.empty
    , orModule = lookupModule plan resolver mkOracle ref
    , orGaps   = reverse . clGaps <$> readIORef ref
    }

-- | Answer for one module, indexing dependencies until one has it.
--
-- Dependencies are tried nearest-first and each is indexed at most once,
-- so a chain that stays inside the first dependency — which is what a
-- facade package's chain does — never touches the rest of the closure.
--
-- Once every dependency that is already unpacked has been asked and none
-- has the module, ownership decides: @ghc-pkg@ names the one unit that
-- exposes it and that unit alone is fetched.  Without this step the walk
-- can only ever answer on a machine where somebody has already unpacked
-- the right dependency — on a cold one, @base\/Data.List\/sortOn@ reports
-- @NOT_FOUND@ for a chain that is perfectly followable.  With it, the
-- speculation stays free and only the commitment pays.
lookupModule
  :: BuildPlan
  -> PackageResolver IO
  -> IO (Either OriginError (ModuleOwnerOracle IO))
  -> IORef Closure
  -> ModulePath
  -> IO (Maybe OutsideModule)
lookupModule plan resolver mkOracle ref m = go
  where
    go = do
      cl <- readIORef ref
      case Map.lookup m (clModules cl) of
        Just entry -> Just <$> readEntry entry
        Nothing    -> case clPending cl of
          (dep : _)  -> do
            (found, gaps) <- indexDependency resolver dep
            -- Atomic because 'indexDependency' is slow and the value it
            -- is folded into was read before it ran: a plain write-back
            -- would drop whatever a concurrent lookup had learned in the
            -- meantime.  'OutsideReach' says nothing about being
            -- single-threaded, and the server consumes the same type.
            atomicModifyIORef' ref (\c -> (record dep found gaps c, ()))
            go
          []         -> fromOwner

    -- The unpacked dependencies are exhausted.  Ask who owns the module,
    -- fetch that one unit, and answer from it.
    --
    -- The version comes from the plan, not from @ghc-pkg@: the databases
    -- can hold several versions of a package and only the plan says which
    -- one this project builds against.  A unit the plan does not name is
    -- not a unit we may answer from, so it is reported rather than used.
    fromOwner = do
      eOracle <- oracle
      case eOracle of
        Left err -> Nothing <$ addGaps [GapOwnerUnaskable err]
        Right o  -> moduleOwners o m >>= \case
          Left err   -> Nothing <$ addGaps [GapOwnerUnaskable err]
          Right pids -> case mapMaybe planned pids of
            []          -> pure Nothing
            (owner : _) -> fetchOwner owner

    -- Only the plan's own version of a unit the databases named, so
    -- ownership decides /which/ package and the plan decides which
    -- version.
    planned pid = puId <$> lookupUnit (pkgName pid) plan

    fetchOwner owner = do
      alreadyTried <- Set.member (pkgName owner) . clFetched <$> readIORef ref
      if alreadyTried then pure Nothing else do
        atomicModifyIORef' ref
          (\c -> (c { clFetched = Set.insert (pkgName owner) (clFetched c) }, ()))
        resolveSrc resolver owner >>= \case
          -- The resolver's own error is the useful half and it cannot be
          -- carried in a 'ReachGap' without 'Hypha.Source.Reach' depending
          -- on 'Hypha.Error', which depends on it.  So it is reported here
          -- and the gap records that the owner was known — the failure is
          -- never merely dropped.
          Left err -> do
            hPutStrLn stderr $
              "hypha: " <> Text.unpack (renderPackageId owner)
                <> " owns " <> Text.unpack (unModulePath m)
                <> " and could not be fetched: "
                <> Text.unpack (errorMessage err)
            Nothing <$ addGaps [GapOwnerUnfetchable owner m]
          Right dir -> do
            (found, gaps) <- modulesUnder owner dir
            atomicModifyIORef' ref (\c -> (record owner found gaps c, ()))
            go

    -- Built at most once, and only on the path that needs it.
    oracle = do
      cl <- readIORef ref
      case clOracle cl of
        Just built -> pure built
        Nothing    -> do
          built <- mkOracle
          atomicModifyIORef' ref (\c -> (c { clOracle = Just built }, ()))
          pure built

    addGaps gaps =
      atomicModifyIORef' ref (\c -> (c { clGaps = reverse gaps <> clGaps c }, ()))

    -- Left-biased on modules: the nearer dependency keeps a module name
    -- both of them have, which is the same tie-break the compiler's own
    -- package ordering makes.  Dropping @dep@ from the pending list by
    -- identity rather than by position, because the list may have moved
    -- on under a concurrent lookup.
    record dep found gaps c = c
      { clPending = filter (/= dep) (clPending c)
      , clModules = clModules c <> found
      , clGaps    = reverse gaps <> clGaps c
      }

    readEntry entry = do
      content <- TIO.readFile (oeFile entry)
      pure OutsideModule
        { omComponent = oeComponent entry
        , omLanguage  = oeLanguage entry
        , omSource    = ModuleSource
            { msDeclaredName = m
            , msPath         = oeFile entry
            , msVisibility   = oeVisibility entry
            , msContent      = content
            }
        }

-- | Index one dependency: where its library modules are, which component
-- owns each, and the language settings that component's stanza sets.
--
-- Read from the cabal file, not guessed from the directory tree.  The
-- predecessor walked @src\/@ (and five other conventional names) four
-- levels deep, which loses two whole classes of module: anything deeper
-- than four directories — @GHC\/Internal\/Control\/Monad\/ST\/Lazy.hs@ is
-- five, along with thirteen of @ghc-internal@'s modules — and every
-- module of a dependency whose @hs-source-dirs@ is none of those six
-- names, which includes most local packages in a multi-package repo.
-- Neither loss was distinguishable from "the symbol is not there".
--
-- The stanza also settles three things the walk could only guess: which
-- component owns a module, whether it is exposed, and which extensions it
-- is parsed under.
indexDependency
  :: PackageResolver IO
  -> PackageId
  -> IO (Map ModulePath OutsideEntry, [ReachGap])
indexDependency resolver dep = resolveSrcLocal resolver dep >>= \case
  Nothing  -> pure (Map.empty, [GapNoLocalSource dep])
  Just dir -> modulesUnder dep dir

-- | The library modules of one unpacked package, however its source got
-- there.
--
-- Split from 'indexDependency' because the owner path in 'lookupModule'
-- has the directory already — it fetched it — and reading the stanzas is
-- the same work either way.
modulesUnder
  :: PackageId
  -> FilePath
  -> IO (Map ModulePath OutsideEntry, [ReachGap])
modulesUnder dep dir = Comp.findCabalFile dir >>= \case
    Nothing    -> heuristic
    Just cabal -> do
      comps <- Comp.parseLibComponents cabal dir
      if null comps then heuristic else do
        entries <- mapM fromComponent comps
        pure (Map.unions entries, [])
  where
    -- Only when there is no library stanza to read.  Announced, because
    -- the walk is the guess this function exists to replace: a module it
    -- misses looks exactly like a module that is not there.
    heuristic = do
      roots <- Indexer.chooseSourceRoots dir
      found <- Indexer.enumModuleFilesIn roots
      pure
        ( Map.fromList
            [ (m, OutsideEntry (componentKeyOf (pkgName dep) Comp.MainLib)
                    file Exposed defaultLanguageSettings)
            | (m, file) <- found
            ]
        , [GapNoModuleList dep dir]
        )

    fromComponent ci = do
      let key = componentKeyOf (pkgName dep) (Comp.ciKind ci)
      located <- mapM (locate ci key) (Indexer.stanzaModules ci)
      pure (Map.fromList (catMaybes located))

    -- A stanza can name a module whose file is absent (a generated
    -- module, a CPP-selected platform variant).  That is not reportable:
    -- nothing asked for it, and the chain only ever looks up names it
    -- read out of an import list.
    locate ci key (name, vis) = do
      mFile <- findModuleFileIn (Comp.ciHsSourceDirs ci) name
      pure $ do
        file <- mFile
        pure
          ( ModulePath name
          , OutsideEntry
              { oeComponent  = key
              , oeFile       = file
              , oeVisibility = vis
              , oeLanguage   = Comp.ciLanguageSettings ci
              }
          )

-- | Every package the given one can reach through the plan, nearest
-- first, plus the packages the plan had no unit for.
--
-- Transitive, because one hop is not the shape of the problem: a facade
-- can re-export from a package that re-exports from a third.  Breadth-first
-- so the ordering matches the order a reader would guess, and bounded by
-- the visited set rather than by a depth limit — the plan is a DAG of a few
-- hundred units at most.
--
-- Keyed on the package name, which is what 'lookupUnit' is keyed on: a
-- plan holds one version of each package by construction.
dependencyClosure :: BuildPlan -> PackageId -> ([PackageId], [PackageName])
dependencyClosure plan pid =
  (go (Set.singleton (pkgName pid)) (depsOf (pkgName pid)), missingRoot)
  where
    go _ [] = []
    go seen (d : ds)
      | pkgName d `Set.member` seen = go seen ds
      | otherwise =
          d : go (Set.insert (pkgName d) seen) (ds <> depsOf (pkgName d))

    depsOf n = maybe [] puDeps (lookupUnit n plan)

    -- The unit for the package we were asked about is the one absence
    -- that matters: without it there is no dependency list at all.  A
    -- transitive unit the plan does not name simply contributes nothing,
    -- and the plan naming every unit is the invariant, so this stays a
    -- one-element check rather than a walk-wide accumulation.
    missingRoot = [ pkgName pid | Nothing <- [lookupUnit (pkgName pid) plan] ]
