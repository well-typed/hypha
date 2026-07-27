{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for 'Hypha.Source.Extensions'.  The regression that
-- motivated the module — @Data.Map.Internal@'s @type role@ annotation
-- failing to parse under a hand-written extension whitelist — is the
-- first case in the suite.
module Unit.SourceExtensions (tests) where

import qualified Data.Text as Text

import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.LanguageExtensions as LangExt

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Source.Extensions
  ( LanguageSettings (..), PragmaScan (..), UnknownExtension (..)
  , defaultLanguageSettings, extensionFromFlagName
  , resolveExtensions, scanPragmas )

tests :: TestTree
tests = testGroup "Unit.SourceExtensions"
  [ testCase "module pragmas enable RoleAnnotations and MagicHash" $ do
      let src = Text.unlines
            [ "{-# LANGUAGE CPP #-}"
            , "{-# LANGUAGE RoleAnnotations #-}"
            , "{-# LANGUAGE MagicHash #-}"
            , "module M where"
            ]
      scan <- scanPragmas "M.hs" src
      let (exts, unknown) =
            resolveExtensions defaultLanguageSettings (psExtensionNames scan)
      unknown @?= []
      psDiagnostics scan @?= []
      assertBool "RoleAnnotations on" (EnumSet.member LangExt.RoleAnnotations exts)
      assertBool "MagicHash on"      (EnumSet.member LangExt.MagicHash exts)

  , testCase "GHC2021 floor is present without any pragma" $ do
      let (exts, _) = resolveExtensions defaultLanguageSettings []
      assertBool "ScopedTypeVariables in GHC2021"
        (EnumSet.member LangExt.ScopedTypeVariables exts)
      assertBool "MagicHash NOT in the floor"
        (not (EnumSet.member LangExt.MagicHash exts))

  , testCase "No- prefix turns an extension off, last pragma wins" $ do
      let (exts, _) = resolveExtensions defaultLanguageSettings
                        ["ScopedTypeVariables", "NoScopedTypeVariables"]
      assertBool "turned off" (not (EnumSet.member LangExt.ScopedTypeVariables exts))

  , testCase "flag aliases resolve through GHC's own table" $ do
      -- Two names, one extension, in both directions: Rank2Types is an
      -- alias of RankNTypes, and RecordPuns is an alias of
      -- NamedFieldPuns.  A table derived from 'show' over the Extension
      -- constructors would resolve neither alias, which is why we read
      -- xFlags — the same table GHC's own flag parser uses.
      extensionFromFlagName "Rank2Types" @?= Right [(LangExt.RankNTypes, True)]
      extensionFromFlagName "RecordPuns" @?= Right [(LangExt.NamedFieldPuns, True)]

  , testCase "language selectors expand to their extension set" $
      case extensionFromFlagName "Haskell2010" of
        Left e   -> fail ("Haskell2010 rejected: " <> show (unUnknownExtension e))
        Right xs -> assertBool "non-empty" (not (null xs))

  , testCase "unknown extension is reported, not dropped" $ do
      let (_, unknown) = resolveExtensions defaultLanguageSettings ["NoSuchExtension"]
      map unUnknownExtension unknown @?= ["NoSuchExtension"]

  , testCase "cabal default-extensions apply below module pragmas" $ do
      let ls = defaultLanguageSettings { lsDefaultOn = [LangExt.MagicHash] }
          (exts, _) = resolveExtensions ls ["NoMagicHash"]
      assertBool "module pragma wins over cabal default"
        (not (EnumSet.member LangExt.MagicHash exts))

  , testCase "OPTIONS_GHC -X pragmas are picked up too" $ do
      let src = Text.unlines
            [ "{-# OPTIONS_GHC -XRoleAnnotations #-}"
            , "module M where"
            ]
      scan <- scanPragmas "M.hs" src
      assertBool "RoleAnnotations seen"
        ("RoleAnnotations" `elem` psExtensionNames scan)

  , testCase "an unreadable LANGUAGE pragma is a diagnostic, not a crash" $ do
      -- getOptions throws on a name GHC does not know.  A reader must
      -- survive that: report it and carry on.
      scan <- scanPragmas "M.hs" (Text.unlines
        [ "{-# LANGUAGE NoSuchExtensionAtAll #-}"
        , "module M where"
        ])
      assertBool "diagnostic recorded" (not (null (psDiagnostics scan)))
  ]
