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

import           Data.IORef (newIORef, modifyIORef', readIORef)
import qualified Data.List as List
import qualified Data.Text as Text

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Package.Resolver (PackageResolver (..))
import Hypha.Source.Dependencies (dependencyReach)
import Hypha.Source.Extensions (LanguageSettings (..))
import Hypha.Source.Reach
  ( OutsideModule (..), OutsideReach (..), ReachGap (..) )
import Hypha.Search.Index (ModuleSource (..), Visibility (..))
import Hypha.Types.BuildPlan (emptyBuildPlan)
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..), pkgName)
import Hypha.Types.SymbolPath (ModulePath (..))
import Util.Fixture
  (fixturePlan, fixtureResolver, reexportDepId, reexportId, resolverFor)

tests :: TestTree
tests = testGroup "Unit.SourceDependencies"
  [ testCase "a dependency's module is reached through the plan" $ do
      reach <- dependencyReach fixturePlan fixtureResolver reexportId
      orModule reach (ModulePath "Dep.Internal") >>= \case
        Nothing -> fail "the plan named reexport-dep but its module was not reached"
        Just om -> do
          omComponent om @?= ComponentKey "reexport-dep"
          -- The path and the bytes, not the name we looked it up by: the
          -- declared name is the lookup key by construction, so asserting
          -- it cannot fail and says nothing about which file was opened.
          assertBool "the path is inside the dependency"
            ("reexport-dep" `List.isInfixOf` msPath (omSource om))
          assertBool "the file really was read, not stubbed"
            ("depThing ::" `Text.isInfixOf` msContent (omSource om))

  , testCase "the intermediate facade is reached too, not just the declaration" $ do
      -- Both hops of the chain have to be readable, or the descent stops
      -- at the facade with nothing to ask about the next hop.
      reach <- dependencyReach fixturePlan fixtureResolver reexportId
      orModule reach (ModulePath "Dep.Facade") >>= \case
        Nothing -> fail "Dep.Facade was not reached"
        Just om -> assertBool "the facade's own file"
          ("Dep/Facade.hs" `List.isSuffixOf` msPath (omSource om))

  , testCase "a module deeper than a directory walk would reach is found" $ do
      -- Critical from review: the predecessor enumerated a dependency by
      -- walking src/ four levels deep, which silently lost fourteen of
      -- ghc-internal's modules -- GHC/Internal/Control/Monad/ST/Lazy.hs is
      -- five deep -- and every module of a dependency whose hs-source-dirs
      -- is not one of six guessed names.  The cabal stanza has no depth.
      reach <- dependencyReach fixturePlan fixtureResolver reexportId
      orModule reach (ModulePath "Dep.Deep.Down.Below.Buried") >>= \case
        Nothing -> fail "a module five directories deep was not reached"
        Just om -> assertBool "the buried file"
          ("Buried.hs" `List.isSuffixOf` msPath (omSource om))

  , testCase "a module's visibility and extensions come from its own stanza" $ do
      -- Not guessed.  The walk claimed Exposed for everything and left the
      -- language settings to whoever asked, which reintroduced across a
      -- package boundary the very bug the locator documents fixing one hop
      -- earlier: a dependency with stanza-wide extensions and no per-module
      -- pragma failed to parse.
      reach <- dependencyReach fixturePlan fixtureResolver reexportId
      orModule reach (ModulePath "Dep.Hidden") >>= \case
        Nothing -> fail "Dep.Hidden was not reached"
        Just om -> do
          msVisibility (omSource om) @?= Internal
          assertBool "the dependency's own default-extensions"
            (not (null (lsDefaultOn (omLanguage om))))

  , testCase "a module no dependency has is a miss, not a wrong answer" $ do
      reach <- dependencyReach fixturePlan fixtureResolver reexportId
      found <- orModule reach (ModulePath "Dep.NoSuchModule")
      fmap omComponent found @?= Nothing

  , testCase "the asking package's own modules are not offered as outside" $ do
      -- The reach is the closure /around/ the package, and the locator
      -- already holds the package's own sources.  Serving them here would
      -- attribute them to whichever dependency the walk happened to reach
      -- first.
      reach <- dependencyReach fixturePlan fixtureResolver reexportId
      found <- orModule reach (ModulePath "Fixture.Internal")
      fmap omComponent found @?= Nothing

  , testCase "with no plan nothing is reachable, and the plan is why" $ do
      -- The plan-less CLI path: no dependency graph, so no candidate can
      -- be followed.  The gap is the whole point -- without it the caller
      -- reports "nothing was reachable" and never why, which is
      -- indistinguishable from the symbol not existing.
      reach <- dependencyReach emptyBuildPlan fixtureResolver reexportId
      found <- orModule reach (ModulePath "Dep.Internal")
      fmap omComponent found @?= Nothing
      gaps <- orGaps reach
      gaps @?= [GapUnitNotInPlan (PackageName "reexport")]

  , testCase "a dependency with no local source is skipped, and reported" $ do
      -- Skipped rather than fatal, so one unreachable dependency cannot
      -- take the command down.  Reported rather than swallowed, because a
      -- search that came up short for want of a tarball must not read as
      -- one that came up short because the symbol is absent.  Asserting the
      -- gap is what keeps the report from being deleted unnoticed.
      reach <- dependencyReach fixturePlan (resolverFor []) reexportId
      found <- orModule reach (ModulePath "Dep.Internal")
      fmap omComponent found @?= Nothing
      gaps <- orGaps reach
      gaps @?= [GapNoLocalSource reexportDepId]

  , testCase "the walk never asks for a source it would have to fetch" $ do
      -- Critical from review: this used to call resolveSrc, which falls
      -- through to a tarball extraction and then to Hackage, so one missed
      -- module name walked the whole closure over the network -- 273 units
      -- on this repo's own plan -- and printed a 404 for a package the
      -- query never needed beside an answer that was already correct.
      asked <- newIORef []
      let base      = fixtureResolver
          recording = base
            { resolveSrc = \p -> do
                modifyIORef' asked (pkgName p :)
                resolveSrc base p
            }
      reach <- dependencyReach fixturePlan recording reexportId
      _     <- orModule reach (ModulePath "Dep.NoSuchModule")
      fetched <- readIORef asked
      fetched @?= []
  ]
