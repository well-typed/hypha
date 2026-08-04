{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for locating a definition given a component's modules.
--
-- Resolution, not sweeping: a symbol the asking module re-exports is
-- followed to its declaration, including across a package boundary, and a
-- symbol whose defining source is out of reach is reported absent rather
-- than approximated by the first same-named binding elsewhere.
module Unit.SourceLocate (tests) where

import           Data.List (isInfixOf)
import qualified Data.Map.Strict as Map

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified GHC.LanguageExtensions as LangExt

import qualified Hypha.Source.Locate as Locate
import           Hypha.Search.Index
                   ( DefinitionRef (..), ImportedDefinitions (..)
                   , ModuleSource (..), Visibility (..) )
import           Hypha.Source.Reach
                   ( OutsideReach, SymbolSearchFailure (..), noOutsideReach
                   , reachFrom )
import           Hypha.Source.Extensions
                   (LanguageSettings (..), defaultLanguageSettings)
import           Hypha.Types.ComponentName (ComponentKey (..))
import           Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))
import           Util.Fixture (depSources, fixtureSources, sourcesFor)

-- | What the server hands a browsing pass: the definition site the index
-- resolved for each name, plus the dependency modules those sites name.
depDefinitions :: [ModuleSource] -> ImportedDefinitions
depDefinitions dep = ImportedDefinitions
  { idSites = Map.fromList
      [ ( SymbolName "depThing"
        , DefinitionRef (ComponentKey "reexport-dep") (ModulePath "Dep.Internal")
        ) ]
  , idSources = Map.fromList
      [ (msDeclaredName d, (ComponentKey "reexport-dep", d)) | d <- dep ]
  }

-- | The dependency's sources with no resolved definition site, which is
-- what the index leaves behind for a name it could not place — and what
-- the CLI has always, since it has no index at all.  Forces the locator
-- down its own resolution path instead of the index's answer.
depSourcesOnly :: [ModuleSource] -> ImportedDefinitions
depSourcesOnly dep = (depDefinitions dep) { idSites = Map.empty }

-- The fixture components carry no stanza-wide extensions, so one setting
-- for every module is the truth here rather than a simplification.
depReach, depReachSourcesOnly :: [ModuleSource] -> OutsideReach IO
depReach            = reachFrom (const defaultLanguageSettings) . depDefinitions
depReachSourcesOnly = reachFrom (const defaultLanguageSettings) . depSourcesOnly

tests :: TestTree
tests = testGroup "Unit.SourceLocate"
  [ testCase "an intra-component re-export resolves to its definition" $ do
      srcs <- fixtureSources
      mLd  <- Locate.locateDefinitionInComponent defaultLanguageSettings
                (ComponentKey "reexport") srcs noOutsideReach
                (ModulePath "Fixture.Wrapper") (SymbolName "insertBag")
      case mLd of
        Right ld -> do
          Locate.ldModule ld    @?= ModulePath "Fixture.Internal"
          Locate.ldComponent ld @?= ComponentKey "reexport"
        Left err -> failWith err "expected to locate insertBag in Fixture.Internal"

  , testCase "a symbol defined in a dependency is located in that package" $ do
      srcs <- fixtureSources
      dep  <- depSources
      mLd <- Locate.locateDefinitionInComponent defaultLanguageSettings
               (ComponentKey "reexport") srcs (depReach dep)
               (ModulePath "Fixture.Imported") (SymbolName "depThing")
      case mLd of
        Right ld -> do
          Locate.ldModule ld    @?= ModulePath "Dep.Internal"
          Locate.ldComponent ld @?= ComponentKey "reexport-dep"
          assertBool "points into the dependency's tree"
            ("reexport-dep" `isInfixOf` Locate.slPath (Locate.ldLocation ld))
        Left err -> failWith err "expected to locate depThing in reexport-dep"

  , testCase "a two-hop re-export resolves through the dependency's own facade" $ do
      -- Fixture.TwoHop -> Dep.Facade -> Dep.Internal, the shape that made
      -- /pkg/base/Data.List/mapAccumL say "symbol not found": the immediate
      -- import only passes the symbol along, so scanning it finds nothing.
      -- The definition site is supplied directly, as the index resolved it.
      srcs <- fixtureSources
      dep  <- depSources
      mLd <- Locate.locateDefinitionInComponent defaultLanguageSettings
               (ComponentKey "reexport") srcs (depReach dep)
               (ModulePath "Fixture.TwoHop") (SymbolName "depThing")
      case mLd of
        Right ld -> do
          Locate.ldModule ld    @?= ModulePath "Dep.Internal"
          Locate.ldComponent ld @?= ComponentKey "reexport-dep"
        Left err -> failWith err "expected to locate depThing through the two-hop chain"

  , testCase "a two-hop re-export resolves with no index answer to lean on" $ do
      -- The same chain, minus the definition site: the CLI's position,
      -- which has a build plan and no index.  Fixture.TwoHop imports only
      -- Dep.Facade, so the ranked candidates stop one module short of the
      -- declaration and the second hop has to come from asking Dep.Facade
      -- which of /its/ imports could supply the name.  This is issue #20:
      -- `hypha source base/Data.List/sortOn` gave up here.
      srcs <- fixtureSources
      dep  <- depSources
      mLd <- Locate.locateDefinitionInComponent defaultLanguageSettings
               (ComponentKey "reexport") srcs (depReachSourcesOnly dep)
               (ModulePath "Fixture.TwoHop") (SymbolName "depThing")
      case mLd of
        Right ld -> do
          Locate.ldModule ld    @?= ModulePath "Dep.Internal"
          Locate.ldComponent ld @?= ComponentKey "reexport-dep"
          assertBool "points into the dependency's tree"
            ("reexport-dep" `isInfixOf` Locate.slPath (Locate.ldLocation ld))
        Left err -> failWith err "expected the descent to reach Dep.Internal"

  , testCase "resolution tries every candidate, not the first one supplied" $ do
      -- Fixture.ViaFacade imports Dep.Facade and Dep.Internal; both sources
      -- are supplied and only the second declares depThing.  With no index
      -- answer to short-circuit on, the locator has to walk the ranked
      -- candidates rather than commit to the first it can open.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/ViaFacade.hs"
          , "Fixture.ViaFacade", Exposed) ]
      dep  <- depSources
      mLd <- Locate.locateDefinitionInComponent defaultLanguageSettings
               (ComponentKey "reexport") srcs (depReachSourcesOnly dep)
               (ModulePath "Fixture.ViaFacade") (SymbolName "depThing")
      case mLd of
        Right ld -> do
          Locate.ldModule ld    @?= ModulePath "Dep.Internal"
          Locate.ldComponent ld @?= ComponentKey "reexport-dep"
        Left err -> failWith err "expected to locate depThing past Dep.Facade"

  , testCase "a symbol whose dependency source is absent is not guessed at" $ do
      -- And it says so as \"the chain ran out\", naming the candidate it
      -- could not open.  Reporting this as \"no such symbol\" is what let a
      -- missing dependency read as a missing definition.
      srcs <- fixtureSources
      mLd  <- Locate.locateDefinitionInComponent defaultLanguageSettings
                (ComponentKey "reexport") srcs noOutsideReach
                (ModulePath "Fixture.Imported") (SymbolName "depThing")
      mLd @?= Left (SearchNoSupplier [ModulePath "Dep.Internal"])

  , testCase "the located definition is read under the component's own extensions" $ do
      -- Fixture.Unboxed needs MagicHash and declares no pragma, so only the
      -- cabal stanza's default-extensions make it readable.  Resolution used
      -- these settings and the final step re-read the file under the GHC2021
      -- floor, so the card answered "not found" for a definition it had just
      -- located.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Unboxed.hs", "Fixture.Unboxed", Exposed) ]
      let magicHash = defaultLanguageSettings
            { lsDefaultOn = [LangExt.MagicHash, LangExt.UnboxedTuples] }
      mLd <- Locate.locateDefinitionInComponent magicHash
               (ComponentKey "reexport") srcs noOutsideReach
               (ModulePath "Fixture.Unboxed") (SymbolName "unboxedAdd")
      case mLd of
        Right ld -> Locate.ldModule ld @?= ModulePath "Fixture.Unboxed"
        Left err -> failWith err "expected to locate unboxedAdd under the stanza's extensions"

  , testCase "without those extensions the module is unreadable, not empty" $ do
      -- The other half of the pair: absent MagicHash the module does not
      -- parse, and \"could not read it\" must not present as \"no such
      -- symbol\".  It now says which of the two it is.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Unboxed.hs", "Fixture.Unboxed", Exposed) ]
      mLd <- Locate.locateDefinitionInComponent defaultLanguageSettings
               (ComponentKey "reexport") srcs noOutsideReach
               (ModulePath "Fixture.Unboxed") (SymbolName "unboxedAdd")
      mLd @?= Left (SearchModuleUnparsed (ModulePath "Fixture.Unboxed"))

    -- The bounds, which had no coverage at all: the whole point of typing
    -- them is that a search which stopped early does not read as one that
    -- finished, and that distinction is only worth anything if it holds.

  , testCase "a chain longer than the hop limit reports the limit, not absence" $ do
      srcs <- fixtureSources
      dep  <- depSources
      mLd  <- Locate.locateDefinitionInComponentWith
                (Locate.defaultSearchBounds { Locate.sbHopLimit = 1 })
                defaultLanguageSettings
                (ComponentKey "reexport") srcs (depReachSourcesOnly dep)
                (ModulePath "Fixture.TwoHop") (SymbolName "depThing")
      -- Fixture.TwoHop -> Dep.Facade -> Dep.Internal needs two hops; with
      -- one it stops on the frontier Dep.Facade handed it.
      mLd @?= Left (SearchHopLimit 1 [ModulePath "Dep.Internal"])

  , testCase "a spent parse budget reports the budget, not absence" $ do
      srcs <- fixtureSources
      dep  <- depSources
      mLd  <- Locate.locateDefinitionInComponentWith
                (Locate.defaultSearchBounds { Locate.sbParseBudget = 1 })
                defaultLanguageSettings
                (ComponentKey "reexport") srcs (depReachSourcesOnly dep)
                (ModulePath "Fixture.TwoHop") (SymbolName "depThing")
      -- One parse reaches Dep.Facade and no further.
      mLd @?= Left (SearchParseBudget 1)

  , testCase "the same chain answers when the bounds allow it" $ do
      -- The control for the two above: without it they would pass just as
      -- well if the fixture had no reachable definition at all.
      srcs <- fixtureSources
      dep  <- depSources
      mLd  <- Locate.locateDefinitionInComponentWith
                (Locate.SearchBounds { Locate.sbHopLimit = 2
                                     , Locate.sbParseBudget = 2 })
                defaultLanguageSettings
                (ComponentKey "reexport") srcs (depReachSourcesOnly dep)
                (ModulePath "Fixture.TwoHop") (SymbolName "depThing")
      case mLd of
        Right ld -> Locate.ldModule ld @?= ModulePath "Dep.Internal"
        Left err -> failWith err "expected two hops and two parses to suffice"

  , testCase "a private homonym reached through an open import does not win" $ do
      -- The wide-ring shape issue #20 asked for a fixture of:
      -- Dep.WideFacade declares nothing and offers six candidates, five of
      -- them open imports.  One of those, Dep.Homonym, declares a private
      -- depThing it does not export -- which is the realistic hazard for a
      -- short name in GHC.Internal.*.  Reading "declares it" as "defines
      -- it" answers with the homonym's file.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/ViaWide.hs"
          , "Fixture.ViaWide", Exposed) ]
      dep  <- depSources
      mLd <- Locate.locateDefinitionInComponent defaultLanguageSettings
               (ComponentKey "reexport") srcs (depReachSourcesOnly dep)
               (ModulePath "Fixture.ViaWide") (SymbolName "depThing")
      case mLd of
        Right ld -> do
          Locate.ldModule ld @?= ModulePath "Dep.Internal"
          assertBool "not the private homonym"
            (not ("Homonym" `isInfixOf` Locate.slPath (Locate.ldLocation ld)))
        Left err -> failWith err "expected the exporting module to win"
  ]

-- | Fail with the reason the search gave, so a broken expectation says
-- which arm it landed in rather than only that it did not succeed.
failWith :: SymbolSearchFailure -> String -> IO a
failWith err what = fail (what <> "; got: " <> show err)
