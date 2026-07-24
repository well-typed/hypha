{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for the batch module-documentation extraction in
-- "Hypha.Source.Extract" — one parse per module, header prose plus a
-- 'DocEntry' per top-level declaration, in source order.
module Unit.SourceExtract (tests) where

import qualified Data.Text as Text

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import qualified Hypha.Source.Extract as Extract
import           Hypha.Source.Extract (DocEntry (..), ModuleDocInfo (..))
import qualified Hypha.Source.Parser  as Parser
import           Hypha.Types.Doc      (DocText (..))

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
  ]
  where
    safeHead []      = Nothing
    safeHead (x : _) = Just x
