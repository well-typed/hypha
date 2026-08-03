{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The modules of a package's dependency closure, unpacked on demand.
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
-- which names every dependency, and 'Hypha.Package.Resolver' , which can
-- put any of their sources on disk.  This module is the bridge: an
-- 'OutsideReach' backed by the plan instead of by an index.
--
-- Demand-driven, one dependency at a time.  Unpacking the whole closure up
-- front would make every @hypha source@ on a package with forty
-- dependencies pay for forty tarball extractions to answer a question that
-- usually needs none of them; answering @base@'s facades costs
-- @ghc-internal@ and nothing else.
module Hypha.Source.Dependencies
  ( dependencyReach
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import System.IO (hPutStrLn, stderr)

import Hypha.Error (errorMessage)
import Hypha.Package.Resolver (PackageResolver (..))
import Hypha.Project.Components qualified as Comp
import Hypha.Search.Index
  (ModuleSource (..), OutsideReach (..), Visibility (..))
import Hypha.Search.Indexer qualified as Indexer
import Hypha.Types.BuildPlan (BuildPlan, PlannedUnit (..), lookupUnit)
import Hypha.Types.ComponentName (ComponentKey, componentKeyOf)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..))

-- | What the walk has learned so far, and what it has left to try.
data Closure = Closure
  { clPending :: ![PackageId]
    -- ^ Dependencies not yet unpacked, nearest first.
  , clModules :: !(Map ModulePath (ComponentKey, FilePath))
    -- ^ Where each module of the dependencies already unpacked lives.
  }

-- | An 'OutsideReach' over the dependency closure of one package.
--
-- 'orSites' is empty: a definition site is a transitively resolved fact
-- and only the index has those.  Everything here is reached by following
-- imports, which is what "Hypha.Source.Locate" does with the modules this
-- hands it.
dependencyReach
  :: BuildPlan
  -> PackageResolver IO
  -> PackageId          -- ^ the package being asked about
  -> IO (OutsideReach IO)
dependencyReach plan resolver pid = do
  ref <- newIORef (Closure (dependencyClosure plan pid) Map.empty)
  pure OutsideReach
    { orSites  = Map.empty
    , orModule = lookupModule resolver ref
    }

-- | Answer for one module, unpacking dependencies until one has it.
--
-- Dependencies are tried nearest-first and each is unpacked at most once,
-- so a chain that stays inside the first dependency — which is what a
-- facade package's chain does — never touches the rest of the closure.
lookupModule
  :: PackageResolver IO
  -> IORef Closure
  -> ModulePath
  -> IO (Maybe (ComponentKey, ModuleSource))
lookupModule resolver ref m = go
  where
    go = do
      cl <- readIORef ref
      case Map.lookup m (clModules cl) of
        Just (comp, file) -> Just . (,) comp <$> readSource file
        Nothing           -> case clPending cl of
          []          -> pure Nothing
          (dep : ps)  -> do
            found <- indexDependency resolver dep
            writeIORef ref cl
              { clPending = ps
                -- Left-biased: the nearer dependency keeps a module name
                -- both of them have, which is the same tie-break the
                -- compiler's own package ordering makes.
              , clModules = clModules cl <> found
              }
            go

    readSource file = do
      content <- TIO.readFile file
      pure ModuleSource
        { msDeclaredName = m
        , msPath         = file
        -- The walk sees files, not a cabal stanza, so it cannot tell an
        -- exposed module from an other-module.  Nothing downstream of a
        -- located definition reads this, and claiming 'Internal' for a
        -- module a facade openly re-exports would be the worse guess.
        , msVisibility   = Exposed
        , msContent      = content
        }

-- | Put one dependency's sources on disk and record where its modules are.
--
-- A dependency we cannot resolve is not fatal — the chain may not need it
-- — but it is never silent: a search that came up short because a tarball
-- would not extract must not look like one that came up short because the
-- symbol is not there.
indexDependency
  :: PackageResolver IO
  -> PackageId
  -> IO (Map ModulePath (ComponentKey, FilePath))
indexDependency resolver dep = resolveSrc resolver dep >>= \case
  Left err -> do
    hPutStrLn stderr $
      "hypha: cannot read the source of " <> Text.unpack (renderPkg dep)
        <> ", so a re-export into it cannot be followed: "
        <> Text.unpack (errorMessage err)
    pure Map.empty
  Right dir -> do
    roots <- Indexer.chooseSourceRoots dir
    found <- Indexer.enumModuleFilesIn roots
    pure $ Map.fromList
      [ (ModulePath name, (compKey, file)) | (name, file) <- found ]
  where
    compKey = componentKeyOf (pkgName dep) Comp.MainLib

-- | Every package the given one can reach through the plan, nearest first.
--
-- Transitive, because one hop is not the shape of the problem: a facade
-- can re-export from a package that re-exports from a third.  Breadth-first
-- so the ordering matches the order a reader would guess, and bounded by
-- the visited set rather than by a depth limit — the plan is a DAG of a few
-- hundred units at most.
dependencyClosure :: BuildPlan -> PackageId -> [PackageId]
dependencyClosure plan pid = go (Set.singleton (pkgName pid)) (depsOf pid)
  where
    go _ [] = []
    go seen (d : ds)
      | pkgName d `Set.member` seen = go seen ds
      | otherwise = d : go (Set.insert (pkgName d) seen) (ds ++ depsOf d)

    depsOf p = maybe [] puDeps (lookupUnit (pkgName p) plan)

renderPkg :: PackageId -> Text.Text
renderPkg pid = unPackageName (pkgName pid) <> "-" <> unVersion (pkgVersion pid)
