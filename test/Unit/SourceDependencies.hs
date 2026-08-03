{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The /producer/ of the CLI's 'OutsideReach', on its own.
--
-- "Unit.SourceLocate" proves the locator follows a chain given the right
-- modules; this proves the build plan is what hands them over.  Both would
-- still pass if 'dependencyReach' returned 'noOutsideReach' — that gap is
-- exactly how issue #20 survived a green suite, with @hypha server@
-- resolving @base@'s facades and the CLI reporting them unfollowable.
module Unit.SourceDependencies (tests) where

import qualified Data.Text as Text
import           System.FilePath ((</>))

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Error (HyphaError (..), NotFoundReason (..))
import Hypha.Package.Resolver (PackageResolver (..), ResolvedPackage (..))
import Hypha.Search.Index (ModuleSource (..), OutsideReach (..))
import Hypha.Source.Dependencies (dependencyReach)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan )
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..))
import qualified Data.Map.Strict as Map

reexportId, reexportDepId :: PackageId
reexportId    = PackageId (PackageName "reexport") (Version "0.1.0")
reexportDepId = PackageId (PackageName "reexport-dep") (Version "0.1.0")

reexportDir, reexportDepDir :: FilePath
reexportDir    = "test" </> "fixtures" </> "reexport"
reexportDepDir = "test" </> "fixtures" </> "reexport-dep"

-- | @reexport@ depends on @reexport-dep@ and nothing else does.
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

-- | Answers only for the packages listed, so an unexpected lookup fails
-- the way the real resolver fails rather than quietly handing back a
-- directory.
resolverFor :: [(PackageName, FilePath)] -> PackageResolver IO
resolverFor known = PackageResolver
  { resolvePkg = \name -> pure $ case lookup name known of
      Just dir -> Right ResolvedPackage
        { rpPkgId         = PackageId name (Version "0.1.0")
        , rpIsOutsidePlan = False
        , rpIsLocal       = True
        , rpDepsCount     = 0
        , rpOrigin        = OriginLocal dir
        }
      Nothing -> Left (NotFound (NotFoundPackageInPlan name))
  , resolveSrc = \pid -> pure $ case lookup (pkgName pid) known of
      Just dir -> Right dir
      Nothing  -> Left (NotFound (NotFoundPackageInPlan (pkgName pid)))
  , fetchVrs   = \_ -> pure (Right [])
  }

fixtureResolver :: PackageResolver IO
fixtureResolver = resolverFor
  [ (PackageName "reexport", reexportDir)
  , (PackageName "reexport-dep", reexportDepDir)
  ]

tests :: TestTree
tests = testGroup "Unit.SourceDependencies"
  [ testCase "a dependency's module is reached through the plan" $ do
      reach <- dependencyReach fixturePlan fixtureResolver reexportId
      orModule reach (ModulePath "Dep.Internal") >>= \case
        Nothing -> fail "the plan named reexport-dep but its module was not reached"
        Just (comp, ms) -> do
          comp @?= ComponentKey "reexport-dep"
          msDeclaredName ms @?= ModulePath "Dep.Internal"
          assertBool "the file really was read, not stubbed"
            ("depThing ::" `Text.isInfixOf` msContent ms)

  , testCase "the intermediate facade is reached too, not just the declaration" $ do
      -- Both hops of the chain have to be readable, or the descent stops
      -- at the facade with nothing to ask about the next hop.
      reach <- dependencyReach fixturePlan fixtureResolver reexportId
      orModule reach (ModulePath "Dep.Facade") >>= \case
        Nothing        -> fail "Dep.Facade was not reached"
        Just (_, ms)   -> msDeclaredName ms @?= ModulePath "Dep.Facade"

  , testCase "a module no dependency has is a miss, not a wrong answer" $ do
      reach <- dependencyReach fixturePlan fixtureResolver reexportId
      found <- orModule reach (ModulePath "Dep.NoSuchModule")
      fmap fst found @?= Nothing

  , testCase "the asking package's own modules are not offered as outside" $ do
      -- The reach is the closure /around/ the package, and the locator
      -- already holds the package's own sources.  Serving them here would
      -- attribute them to whichever dependency the walk happened to reach
      -- first.
      reach <- dependencyReach fixturePlan fixtureResolver reexportId
      found <- orModule reach (ModulePath "Fixture.Internal")
      fmap fst found @?= Nothing

  , testCase "with no plan nothing is reachable" $ do
      -- The plan-less CLI path: no dependency graph, so no candidate can
      -- be followed and 'locateDefinitionInComponent' reports the
      -- re-export instead of guessing at it.
      reach <- dependencyReach emptyBuildPlan fixtureResolver reexportId
      found <- orModule reach (ModulePath "Dep.Internal")
      fmap fst found @?= Nothing

  , testCase "a dependency whose source will not resolve is skipped, not fatal" $ do
      -- Reported on stderr by 'dependencyReach'; what matters here is that
      -- the lookup returns rather than throwing, so one unreachable
      -- dependency cannot take the command down with it.
      reach <- dependencyReach fixturePlan (resolverFor []) reexportId
      found <- orModule reach (ModulePath "Dep.Internal")
      fmap fst found @?= Nothing
  ]
