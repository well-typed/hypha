{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for 'Hypha.Source.Interface': the single parse-derived view
-- of a module that the indexer, the module page and the symbol card all
-- read.
--
-- It replaces two habits that cost us correct rows.  Module names came
-- from file paths, so a stray @race.hs@ became a module called @race@ and
-- an unresolved source dir prefixed every name with @compiler.@.  Export
-- lists came from a regex over the module header, which got @Type(..)@
-- bundles and @module N@ re-export forms wrong.
module Unit.SourceInterface (tests) where

import qualified Data.Text.IO as TIO

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Source.Extensions (defaultLanguageSettings)
import Hypha.Source.Interface
  ( ExportItem (..), ImportItem (..), ModuleInterface (..)
  , declaredNames, interfaceExportedNames, parseInterface )
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

load :: FilePath -> IO ModuleInterface
load fp = do
  src <- TIO.readFile fp
  case parseInterface defaultLanguageSettings fp src of
    Left e  -> fail ("parse failed for " <> fp <> ": " <> show e)
    Right i -> pure i

tests :: TestTree
tests = testGroup "Unit.SourceInterface"
  [ testCase "module name comes from the parse tree, not the path" $ do
      i <- load "test/fixtures/reexport/src/Fixture/Renamed.hs"
      miName i @?= ModulePath "Fixture.Declared"

  , testCase "explicit export list is recorded with subordinates" $ do
      i <- load "test/fixtures/reexport/src/Fixture/Internal.hs"
      let names = map unSymbolName (interfaceExportedNames i)
      assertBool "Bag exported"          ("Bag" `elem` names)
      assertBool "insertBag exported"    ("insertBag" `elem` names)
      assertBool "internalOnly exported" ("internalOnly" `elem` names)

  , testCase "module re-export form is preserved, not flattened away" $ do
      i <- load "test/fixtures/reexport/src/Fixture/Wrapper.hs"
      assertBool "module Fixture.Other present"
        (ExportModule (ModulePath "Fixture.Other") `elem` maybe [] id (miExports i))

  , testCase "imports carry their explicit name lists" $ do
      i <- load "test/fixtures/reexport/src/Fixture/Wrapper.hs"
      let fromInternal =
            [ ii | ii <- miImports i, iiModule ii == ModulePath "Fixture.Internal" ]
      case fromInternal of
        [ii] -> case iiNames ii of
          Just (hiding, ns) -> do
            hiding @?= False
            assertBool "insertBag listed" (SymbolName "insertBag" `elem` ns)
            assertBool "Bag listed"       (SymbolName "Bag" `elem` ns)
          Nothing -> fail "expected an explicit import list"
        _ -> fail "expected exactly one import of Fixture.Internal"

  , testCase "an unrestricted import is Nothing, not an empty list" $ do
      -- The distinction decides whether the import can supply a name:
      -- an empty list supplies nothing, no list supplies everything.
      i <- load "test/fixtures/reexport/src/Fixture/Wrapper.hs"
      let fromOther =
            [ iiNames ii
            | ii <- miImports i, iiModule ii == ModulePath "Fixture.Other" ]
      fromOther @?= [Nothing]

  , testCase "no export list means Nothing, and decls are still listed" $ do
      i <- load "test/fixtures/reexport/src/script.hs"
      miExports i @?= Nothing
      map unSymbolName (declaredNames i) @?= ["main"]

  , testCase "a module with no header is the implicit Main" $ do
      i <- load "test/fixtures/reexport/src/script.hs"
      miName i @?= ModulePath "Main"

  , testCase "declared names exclude re-exports" $ do
      i <- load "test/fixtures/reexport/src/Fixture/Wrapper.hs"
      declaredNames i @?= []

  , testCase "the module header doc is carried" $ do
      i <- load "test/fixtures/reexport/src/Fixture/Other.hs"
      assertBool "header present" (miHeaderDoc i /= Nothing)
  ]
