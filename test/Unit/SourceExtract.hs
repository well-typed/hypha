{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for the batch module-documentation extraction in
-- "Hypha.Source.Extract" — one parse per module, header prose plus a
-- 'DocEntry' per top-level declaration, in source order.
module Unit.SourceExtract (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Data.List (sort)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import           Hypha.Search.Index (DefinitionRef (..))
import qualified Hypha.Source.Extract as Extract
import           Hypha.Source.Extract
                   ( DocEntry (..), EntryOrigin (..), ModuleDocInfo (..)
                   , resolveModuleEntries )
import           Hypha.Source.Extensions (defaultLanguageSettings)
import qualified Hypha.Source.Parser  as Parser
import           Hypha.Source.Parser  (parseErrorMessage)
import           Hypha.Types.ComponentName (ComponentKey (..))
import           Hypha.Types.Doc      (DocText (..))
import           Hypha.Types.SymbolPath (ModulePath (..))
import           Util.Fixture (depSources, fixtureSources)

tests :: TestTree
tests = testGroup "Unit.SourceExtract"
  [ testCase "extractModuleDoc returns header and entries in source order" $ do
      let src = Text.unlines
            [ "-- | Fixture module header."
            , "--"
            , "-- Second paragraph."
            , "{-# LANGUAGE BangPatterns #-}"
            , "module Fixture (Colour (..), run) where"
            , ""
            , "-- | A colour."
            , "data Colour = Red | Green"
            , ""
            , "-- | Run it."
            , "run :: Int -> Int"
            , "run x = x"
            ]
      case Extract.extractModuleDoc "Fixture.hs" src of
        Left e  -> assertFailure (show e)
        Right d -> do
          fmap unDocText (mdiHeader d)
            @?= Just " Fixture module header.\n\n Second paragraph."
          map deName (mdiEntries d) @?= ["Colour", "run"]
          map deKind (mdiEntries d) @?= [Parser.DkData, Parser.DkFunction]
          case mdiEntries d of
            [colour, run] -> do
              deSignature colour @?= Just "data Colour = Red | Green"
              deSignature run    @?= Just "run :: Int -> Int"
              fmap unDocText (deHaddock colour) @?= Just " A colour."
            es -> assertFailure ("expected two entries, got " <> show (length es))

  , testCase "module without header prose yields Nothing" $ do
      let src = Text.unlines
            [ "module Bare where"
            , ""
            , "x :: Int"
            , "x = 1"
            ]
      case Extract.extractModuleDoc "Bare.hs" src of
        Left e  -> assertFailure (show e)
        Right d -> mdiHeader d @?= Nothing

  , testCase "pragmas stacked above the header never leak into the prose" $ do
      let src = Text.unlines
            [ "{-# LANGUAGE DerivingStrategies #-}"
            , "{-# LANGUAGE OverloadedStrings  #-}"
            , "-- | Real header prose."
            , "module Pragmatic where"
            , "x = 1"
            ]
      case Extract.extractModuleDoc "Pragmatic.hs" src of
        Left e  -> assertFailure (show e)
        Right d -> fmap unDocText (mdiHeader d) @?= Just " Real header prose."

  , testCase "license comment separated by a blank line is not a header" $ do
      let src = Text.unlines
            [ "-- Copyright (c) nobody"
            , ""
            , "module Bare where"
            , "x = 1"
            ]
      case Extract.extractModuleDoc "Bare.hs" src of
        Left e  -> assertFailure (show e)
        Right d -> mdiHeader d @?= Nothing

  , testCase "long data declarations are clamped to 40 lines" $ do
      let cons  = [ "  | C" <> Text.pack (show i) | i <- [1 :: Int .. 60] ]
          src   = Text.unlines $
            [ "module Big where"
            , "data Big = C0"
            ] <> cons
      case Extract.extractModuleDoc "Big.hs" src of
        Left e  -> assertFailure (show e)
        Right d -> do
          sigT <- maybe (assertFailure "no entry") pure
                    (deSignature =<< safeHead (mdiEntries d))
          length (Text.lines sigT) @?= 41   -- 40 source lines + ellipsis
          assertBool "ends with ellipsis"
            ("\x2026" `Text.isSuffixOf` sigT)

  , testCase "doc block separated from its declaration by a blank line still attaches" $ do
      -- The containers idiom: a @-- |@ block, a blank line, then the
      -- signature.  GHC attaches the comment across the blank line and
      -- renders it marker-free, so extraction reports the prose (not a
      -- blank box).
      let src = Text.unlines
            [ "module M where"
            , ""
            , "-- | Insert with a function."
            , "--"
            , "-- > insertWith (++) 5 \"x\" m"
            , ""
            , "insertWith :: Int -> Int"
            , "insertWith = id"
            ]
      case Extract.extractModuleDoc "M.hs" src of
        Left e  -> assertFailure (show e)
        Right d -> case mdiEntries d of
          (e : _) -> fmap unDocText (deHaddock e)
                       @?= Just " Insert with a function.\n\n > insertWith (++) 5 \"x\" m"
          []      -> assertFailure "expected an entry"

  , testCase "extractSymbolInfo recovers a doc block sitting above a blank line" $ do
      let src = Text.unlines
            [ "module M where"
            , ""
            , "-- | Insert with a function."
            , ""
            , "insertWith :: Int -> Int"
            , "insertWith = id"
            ]
          info = Extract.extractSymbolInfo src "insertWith"
      fmap unDocText (Extract.siHaddock info)
        @?= Just " Insert with a function."

  , testCase "a plain comment between the doc block and the signature is ignored" $ do
      -- The @insertWithKey@ idiom: the real @-- |@ block, a blank line,
      -- then a non-doc @-- ...@ implementation note directly above the
      -- signature.  GHC drops the non-doc comment and keeps the real
      -- doc; the old line scanner grabbed the note and lost the doc.
      let src = Text.unlines
            [ "module M where"
            , ""
            , "-- | The real doc."
            , ""
            , "-- See Note: some implementation detail"
            , "insertWithKey :: Int -> Int"
            , "insertWithKey = id"
            ]
          info = Extract.extractSymbolInfo src "insertWithKey"
      fmap unDocText (Extract.siHaddock info) @?= Just " The real doc."

  , testCase "function bodies are never sliced into the signature slot" $ do
      let src = Text.unlines
            [ "module M where"
            , "noSig x ="
            , "  x + 1"
            ]
      case Extract.extractModuleDoc "M.hs" src of
        Left e  -> assertFailure (show e)
        Right d -> (deSignature =<< safeHead (mdiEntries d)) @?= Nothing

  , testCase "a wrapper module's entries include its re-exports" $ do
      -- Data.Map.Strict's page was empty because entries came only from
      -- local declarations, and that module declares almost nothing.
      srcs <- fixtureSources
      case resolveModuleEntries defaultLanguageSettings (ComponentKey "reexport") srcs
             Map.empty (ModulePath "Fixture.Wrapper") of
        Left e     -> assertFailure (show e)
        Right info -> do
          sort (map deName (mdiEntries info))
            @?= ["Bag", "insertBag", "otherOnly", "sizeBag"]
          [ deOrigin e | e <- mdiEntries info, deName e == "insertBag" ]
            @?= [EntryReexport (DefinitionRef (ComponentKey "reexport")
                                              (ModulePath "Fixture.Internal"))]
          [ deOrigin e | e <- mdiEntries info, deName e == "otherOnly" ]
            @?= [EntryReexport (DefinitionRef (ComponentKey "reexport")
                                              (ModulePath "Fixture.Other"))]

  , testCase "a re-exported entry carries the definition's haddock" $ do
      srcs <- fixtureSources
      case resolveModuleEntries defaultLanguageSettings (ComponentKey "reexport") srcs
             Map.empty (ModulePath "Fixture.Wrapper") of
        Left e     -> assertFailure (show e)
        Right info ->
          assertBool "insertBag has documentation"
            (or [ deHaddock e /= Nothing
                | e <- mdiEntries info, deName e == "insertBag" ])

  , testCase "a definition module's entries are all local" $ do
      srcs <- fixtureSources
      case resolveModuleEntries defaultLanguageSettings (ComponentKey "reexport") srcs
             Map.empty (ModulePath "Fixture.Internal") of
        Left e     -> assertFailure (show e)
        Right info ->
          assertBool "all local"
            (all ((== EntryLocal) . deOrigin) (mdiEntries info))

  , testCase "a module the component does not have is a reportable absence" $ do
      srcs <- fixtureSources
      case resolveModuleEntries defaultLanguageSettings (ComponentKey "reexport") srcs
             Map.empty (ModulePath "Fixture.Nope") of
        Right _ -> assertFailure "expected an error for an unknown module"
        Left e  -> assertBool "names the module"
          ("Fixture.Nope" `Text.isInfixOf` parseErrorMessage e)

  , testCase "a facade page shows its dependency's entry, with haddock" $ do
      srcs <- fixtureSources
      dep  <- depSources
      let imported = Map.fromList
            [ (ModulePath "Dep.Internal", (ComponentKey "reexport-dep", d))
            | d <- dep
            ]
      case resolveModuleEntries defaultLanguageSettings (ComponentKey "reexport")
             srcs imported (ModulePath "Fixture.Imported") of
        Left e     -> assertFailure (show e)
        Right info -> do
          [ deOrigin e | e <- mdiEntries info, deName e == "depThing" ]
            @?= [ EntryReexport (DefinitionRef (ComponentKey "reexport-dep")
                                               (ModulePath "Dep.Internal")) ]
          [ deSignature e | e <- mdiEntries info, deName e == "depThing" ]
            @?= [Just "depThing :: Int -> Int"]
          assertBool "the dependency's haddock is carried over"
            (or [ deHaddock e /= Nothing
                | e <- mdiEntries info, deName e == "depThing" ])

  , testCase "an entry whose owner is unknown is listed, not dropped" $ do
      -- No imported sources at all: the page must still name depThing.
      -- Omitting it leaves the reader with no way to know it exists.
      srcs <- fixtureSources
      case resolveModuleEntries defaultLanguageSettings (ComponentKey "reexport")
             srcs Map.empty (ModulePath "Fixture.Imported") of
        Left e     -> assertFailure (show e)
        Right info -> do
          map deName (mdiEntries info) @?= ["depThing"]
          map deSignature (mdiEntries info) @?= [Nothing]
  ]
  where
    safeHead []      = Nothing
    safeHead (x : _) = Just x
