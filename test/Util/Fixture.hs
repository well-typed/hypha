{-# LANGUAGE OverloadedStrings #-}
-- | Loader for the @test/fixtures/reexport@ component, shared by the
-- index, locate and module-doc suites so they cannot drift apart on what
-- the fixture contains.
--
-- The module list and the visibilities come from the fixture's own cabal
-- file, through the same 'Indexer.packageSources' the CLI uses: a
-- hardcoded copy here would have been a second declaration of the same
-- facts, free to disagree with the file it mirrors.  It also means the
-- cabal-reading path is exercised end to end rather than only in
-- isolation.
module Util.Fixture
  ( fixtureSources
  , depSources
  , sourcesFor
    -- * The fixture packages, as a plan and a resolver see them
  , reexportId
  , reexportDepId
  , reexportDir
  , reexportDepDir
  , asyncId
  , asyncDir
  , fixturePlan
  , fixtureResolver
  , resolverFor
  , planBackedReach
  , planlessReach
    -- * Who owns a module, without a compiler
  , ownerOracleOver
  , noOwnerOracle
  ) where

import           Data.Map.Strict qualified as Map
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           System.FilePath ((</>))

import Hypha.Error (HyphaError (..), NotFoundReason (..))
import Hypha.Package.Resolver (PackageResolver (..), ResolvedPackage (..))
import Hypha.Search.Index (ModuleSource (..), Visibility (..))
import Hypha.Search.Indexer (packageSources)
import Hypha.Source.Dependencies (dependencyReach)
import Hypha.Source.Origins (ModuleOwnerOracle (..), OriginError)
import Hypha.Source.Reach (OutsideReach)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan )
import Hypha.Types.PackageId
  (PackageId (..), PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..))

-- | Load explicitly named sources, for the fixtures a cabal file cannot
-- describe: a module whose file name disagrees with its header, or one
-- that only some suites want in the component.
sourcesFor :: [(FilePath, Text, Visibility)] -> IO [ModuleSource]
sourcesFor = mapM $ \(fp, declared, vis) -> do
  content <- TIO.readFile fp
  pure ModuleSource
    { msDeclaredName = ModulePath declared
    , msPath         = fp
    , msVisibility   = vis
    , msContent      = content
    }

-- | The fixture component, with the visibility its cabal file declares.
--
-- @Fixture.Internal@ is deliberately exposed: that is the @containers@
-- situation, where an @.Internal@ module is public and must still lose to
-- the wrapper that documents it.
fixtureSources :: IO [ModuleSource]
fixtureSources = componentSources "test/fixtures/reexport"

-- | The @reexport-dep@ component: the package @Fixture.Imported@
-- re-exports from.  Kept separate from 'fixtureSources' because the whole
-- point of the fixture is that the two are different components.
depSources :: IO [ModuleSource]
depSources = componentSources "test/fixtures/reexport-dep"

componentSources :: FilePath -> IO [ModuleSource]
componentSources root = concatMap snd <$> packageSources root

-- The fixture packages, named once.  These were duplicated across the
-- golden and the unit suite, which is two declarations of one fact and
-- exactly the drift the module header warns about.

reexportId, reexportDepId, asyncId :: PackageId
reexportId    = PackageId (PackageName "reexport") (Version "0.1.0")
reexportDepId = PackageId (PackageName "reexport-dep") (Version "0.1.0")
asyncId       = PackageId (PackageName "async") (Version "2.2.5")

reexportDir, reexportDepDir, asyncDir :: FilePath
reexportDir    = "test" </> "fixtures" </> "reexport"
reexportDepDir = "test" </> "fixtures" </> "reexport-dep"
asyncDir       = "test" </> "fixtures" </> "fake-cabal-store" </> "ghc-9.6.7"
                   </> "async-2.2.5-abc123456789" </> "share" </> "async"

-- | A plan in which @reexport@ depends on @reexport-dep@.  That edge is
-- the whole input to 'dependencyReach': it is what tells the CLI which
-- packages a re-export is allowed to leave into.
fixturePlan :: BuildPlan
fixturePlan = emptyBuildPlan
  { bpUnits = Map.fromList
      [ (PackageName "reexport", unit reexportId reexportDir [reexportDepId])
      , (PackageName "reexport-dep", unit reexportDepId reexportDepDir [])
      ]
  }
  where
    unit pid dir deps = PlannedUnit
      { puId            = pid
      , puDeps          = deps
      , puIsLocal       = True
      , puOrigin        = OriginLocal dir
      , puSrcDir        = Just dir
      , puDistDir       = Nothing
      , puLibComponents = []
      }

-- | Answers for the two fixture packages and nothing else, so a lookup for
-- any other package fails the way the real resolver fails rather than
-- quietly handing back a directory.
fixtureResolver :: PackageResolver IO
fixtureResolver = resolverFor
  [ (reexportId, reexportDir)
  , (reexportDepId, reexportDepDir)
  ]

-- | A resolver over an explicit table of packages.
--
-- 'resolveSrcLocal' answers the same table as 'resolveSrc': every fixture
-- package is already on disk, which is the case the local probe exists
-- for.  Nothing here ever reaches the network, which is the property the
-- dependency walk depends on.
resolverFor :: [(PackageId, FilePath)] -> PackageResolver IO
resolverFor table = PackageResolver
  { resolvePkg = \name -> pure $ case dirOf name of
      Just (pid, dir) -> Right ResolvedPackage
        { rpPkgId         = pid
        , rpIsOutsidePlan = False
        , rpIsLocal       = True
        , rpDepsCount     = 0
        , rpOrigin        = OriginLocal dir
        }
      Nothing -> Left (NotFound (NotFoundPackageInPlan name))
  , resolveSrc = \pid -> pure $ case dirOf (pkgName pid) of
      Just (_, dir) -> Right dir
      Nothing       -> Left (NotFound (NotFoundPackageInPlan (pkgName pid)))
  , resolveSrcLocal = \pid -> pure (snd <$> dirOf (pkgName pid))
  , fetchVrs   = \_ -> pure (Right [])
  }
  where
    dirOf name =
      case [ (pid, dir) | (pid, dir) <- table, pkgName pid == name ] of
        (hit : _) -> Just hit
        []        -> Nothing

-- | An owner oracle over an explicit module table, standing in for
-- @ghc-pkg@.
--
-- The production oracle selects the plan's compiler by running it and then
-- shells out per module, so a suite that used it would be testing the
-- machine.  What the walk needs from it is one fact per module, and this
-- supplies exactly that.
ownerOracleOver
  :: [(ModulePath, PackageId)]
  -> IO (Either OriginError (ModuleOwnerOracle IO))
ownerOracleOver table = pure $ Right $ ModuleOwnerOracle $ \m ->
  pure (Right [ pid | (owned, pid) <- table, owned == m ])

-- | An oracle that knows nothing, which is what every test about the
-- unpacked-dependency walk wants: it must reach its answers without
-- ownership having to rescue it.
noOwnerOracle :: IO (Either OriginError (ModuleOwnerOracle IO))
noOwnerOracle = ownerOracleOver []

-- | The real producer over the fixture plan — the same call the CLI makes.
planBackedReach :: IO (OutsideReach IO)
planBackedReach =
  dependencyReach fixturePlan fixtureResolver noOwnerOracle reexportId

-- | The same producer with no plan, which is what the CLI builds outside a
-- project.
--
-- 'Hypha.Source.Reach.noOutsideReach' would be a second, weaker stand-in
-- for this: the two coincide today, and a test pinned to the stand-in would
-- keep passing if the production path stopped agreeing with it.
planlessReach :: IO (OutsideReach IO)
planlessReach =
  dependencyReach emptyBuildPlan fixtureResolver noOwnerOracle reexportId
