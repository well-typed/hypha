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
            @?= Just "-- | Fixture module header.\n--\n-- Second paragraph."
          map deName (mdiEntries d) @?= ["Colour", "run"]
          map deKind (mdiEntries d) @?= [Parser.DkData, Parser.DkFunction]
          case mdiEntries d of
            [colour, run] -> do
              deSignature colour @?= Just "data Colour = Red | Green"
              deSignature run    @?= Just "run :: Int -> Int"
              fmap unDocText (deHaddock colour) @?= Just "-- | A colour."
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
