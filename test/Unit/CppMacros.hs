{-# LANGUAGE OverloadedStrings #-}
-- | The synthesised @cabal_macros.h@.
--
-- These are the definitions cabal generates for a build and hypha has to
-- reproduce from the plan, because without them every conditional is
-- evaluated with an empty macro environment and an undefined macro is
-- zero — so a module gated on @__GLASGOW_HASKELL__ >= 710@ is indexed
-- from its pre-7.10 branch.
module Unit.CppMacros (tests) where

import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import System.IO.Temp (withSystemTempDirectory)

import Hypha.Source.CppMacros
  ( CppEnv (..), ghcVersionMacro, materialiseMacroHeader, noCppEnv
  , renderMacroHeader )
import Hypha.Source.Extensions (LanguageSettings (..), defaultLanguageSettings)
import Hypha.Source.Parser (Decl (..), parseModuleWith)
import Hypha.Types.PackageId (PackageName (..), Version (..))

tests :: TestTree
tests = testGroup "Unit.CppMacros"
  [ testGroup "__GLASGOW_HASKELL__"
      -- GHC's own encoding is major * 100 + minor, so 9.10.3 is 910 and
      -- not 9103 or 910.3.  Getting this wrong flips every version gate
      -- in the ecosystem at once, silently.
      [ testCase "is major * 100 + minor" $ do
          ghcVersionMacro "ghc-9.10.3" @?= Just (910, 3)
          ghcVersionMacro "ghc-9.6.7"  @?= Just (906, 7)
          ghcVersionMacro "ghc-9.12.4" @?= Just (912, 4)

      , testCase "a two-component version has patch level 0" $
          ghcVersionMacro "ghc-9.10" @?= Just (910, 0)

      , testCase "an unrecognisable compiler id yields nothing" $ do
          -- Better no definition than a wrong one: an undefined macro
          -- makes gates fall to their else branch, a wrong one makes
          -- them fall to a confidently incorrect branch.
          ghcVersionMacro "unknown"    @?= Nothing
          ghcVersionMacro "ghc-"       @?= Nothing
          ghcVersionMacro ""           @?= Nothing
      ]

  , testGroup "MIN_VERSION_<pkg>"
      [ testCase "compares the way cabal's own macro does" $ do
          let hdr = renderMacroHeader "ghc-9.10.3"
                      [(PackageName "base", Version "4.20.2.0")]
          assertBool "defines the macro"
            ("#define MIN_VERSION_base(major1,major2,minor)" `Text.isInfixOf` hdr)
          assertBool "defines the version string"
            ("#define VERSION_base \"4.20.2.0\"" `Text.isInfixOf` hdr)
          assertBool "carries the three components"
            (   "(major1) <  4"                  `Text.isInfixOf` hdr
             && "(major2) <  20"                 `Text.isInfixOf` hdr
             && "(minor) <= 2"                   `Text.isInfixOf` hdr)

      , testCase "a dash in the package name becomes an underscore" $ do
          -- @base-compat@ is @MIN_VERSION_base_compat@; a macro name is
          -- a C identifier and cannot carry a dash.
          let hdr = renderMacroHeader "ghc-9.10.3"
                      [(PackageName "base-compat", Version "0.15.0")]
          assertBool "underscored"
            ("MIN_VERSION_base_compat(" `Text.isInfixOf` hdr)
          assertBool "no dash survives in a macro name"
            (not ("MIN_VERSION_base-compat" `Text.isInfixOf` hdr))

      , testCase "a short version is padded, not rejected" $ do
          -- @containers-0.7@ has two components; the macro takes three.
          let hdr = renderMacroHeader "ghc-9.10.3"
                      [(PackageName "containers", Version "0.7")]
          assertBool "defines the macro"
            ("MIN_VERSION_containers(" `Text.isInfixOf` hdr)
          assertBool "minor defaults to 0"
            ("(minor) <= 0" `Text.isInfixOf` hdr)
      ]

  , testCase "the compiler macro is emitted" $ do
      let hdr = renderMacroHeader "ghc-9.10.3" []
      assertBool "__GLASGOW_HASKELL__"
        ("#define __GLASGOW_HASKELL__ 910" `Text.isInfixOf` hdr)
      assertBool "patch level"
        ("#define __GLASGOW_HASKELL_PATCHLEVEL1__ 3" `Text.isInfixOf` hdr)

  , testCase "a gated module is parsed from the branch the plan implies" $
      -- The end of the wire, and the whole point: without the header the
      -- gates below are evaluated against an empty macro environment, an
      -- undefined macro is 0, and both resolve to their else branch.
      withSystemTempDirectory "hypha-cpp" $ \tmp -> do
        let header = renderMacroHeader "ghc-9.10.3"
                       [(PackageName "base", Version "4.20.2.0")]
        path <- materialiseMacroHeader tmp header
        let ls = defaultLanguageSettings
                   { lsCpp = noCppEnv { cppPreInclude = Just path } }
            src = Text.unlines
              [ "{-# LANGUAGE CPP #-}"
              , "module M where"
              , "#if __GLASGOW_HASKELL__ >= 710"
              , "modern :: Int -> Int"
              , "modern = id"
              , "#else"
              , "ancient :: Int -> Int"
              , "ancient = id"
              , "#endif"
              , "#if MIN_VERSION_base(4,18,0)"
              , "newBase :: Int"
              , "newBase = 1"
              , "#else"
              , "oldBase :: Int"
              , "oldBase = 0"
              , "#endif"
              ]
        case parseModuleWith ls "M.hs" src of
          Left e -> fail ("unexpected parse failure: " <> show e)
          Right (_, _, ds) -> do
            let names = map declName ds
            assertBool ("expected the modern branch, got " <> show names)
              ("modern" `elem` names && "newBase" `elem` names)
            assertBool ("the dead branch must not be indexed, got " <> show names)
              ("ancient" `notElem` names && "oldBase" `notElem` names)

  , testCase "without the header the oldest branch wins" $ do
      -- Pinned so the failure this fixes cannot come back unnoticed:
      -- this is exactly what every module got before.
      let src = Text.unlines
            [ "{-# LANGUAGE CPP #-}"
            , "module M where"
            , "#if __GLASGOW_HASKELL__ >= 710"
            , "modern :: Int -> Int"
            , "modern = id"
            , "#else"
            , "ancient :: Int -> Int"
            , "ancient = id"
            , "#endif"
            ]
      case parseModuleWith defaultLanguageSettings "M.hs" src of
        Left e -> fail ("unexpected parse failure: " <> show e)
        Right (_, _, ds) ->
          map declName ds @?= ["ancient"]

  , testCase "an unknown compiler leaves the macro undefined" $ do
      let hdr = renderMacroHeader "wat" [(PackageName "base", Version "4.20.2.0")]
      assertBool "no compiler macro"
        (not ("__GLASGOW_HASKELL__" `Text.isInfixOf` hdr))
      assertBool "package macros still emitted"
        ("MIN_VERSION_base(" `Text.isInfixOf` hdr)
  ]
