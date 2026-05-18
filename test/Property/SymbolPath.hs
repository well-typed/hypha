{-# LANGUAGE OverloadedStrings #-}
module Property.SymbolPath (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range
import Test.Falsify.Property (gen, assert)
import qualified Test.Falsify.Predicate as P
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.SymbolPath
  ( SymbolPath (..), ModulePath (..), SymbolName (..), ParseError (..)
  , parseSymbolPath, renderSymbolPath )

genIdentText :: Gen.Gen Text
genIdentText = do
  c  <- Gen.elem $ pure 'a' <> pure 'b' <> pure 'F'
  cs <- Gen.list (Range.between (0, 8))
                 (Gen.elem (pure 'a' <> pure 'B' <> pure '2' <> pure '_'))
  pure (Text.pack (c : cs))

genModulePath :: Gen.Gen Text
genModulePath = do
  segs <- Gen.list (Range.between (1, 4)) $ do
    c  <- Gen.elem (pure 'A' <> pure 'B' <> pure 'C')
    cs <- Gen.list (Range.between (0, 5)) (Gen.elem (pure 'a' <> pure '2'))
    pure (Text.pack (c : cs))
  pure (Text.intercalate "." segs)

genSymbolPath :: Gen.Gen SymbolPath
genSymbolPath = do
  pkgT <- genIdentText
  hasVer <- Gen.bool False
  mVer <- if hasVer then Just <$> genIdentText else pure Nothing
  hasMod <- Gen.bool False
  mMod <- if hasMod then Just <$> genModulePath else pure Nothing
  hasSym <- Gen.bool False
  mSym <- if (hasMod && hasSym) then Just <$> genIdentText else pure Nothing
  case parseSymbolPath (assemble pkgT mVer mMod mSym) of
    Right sp -> pure sp
    Left  e  -> error ("genSymbolPath produced unparseable input: " <> show e)

assemble :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text
assemble pkg mv mm ms =
       pkg
    <> maybe "" ("@" <>) mv
    <> maybe "" ("/" <>) mm
    <> maybe "" ("/" <>) ms

tests :: TestTree
tests = testGroup "SymbolPath"
  [ testProperty "parse . render = Right" $ do
      sp <- gen genSymbolPath
      assert $ P.eq P..$ ("expected", Right sp) P..$ ("got", parseSymbolPath (renderSymbolPath sp))
  , testProperty "specific: pkg only" $ do
      assert $ P.eq P..$ ("expected", Right (SymbolPath (PackageName "async") Nothing Nothing Nothing))
                   P..$ ("got", parseSymbolPath "async")
  , testProperty "specific: pkg + ver + module + symbol" $ do
      assert $ P.eq P..$ ("expected", Right (SymbolPath
                            (PackageName "async")
                            (Just (Version "2.2.5"))
                            (Just (ModulePath "Control.Concurrent.Async"))
                            (Just (SymbolName "concurrently"))))
                   P..$ ("got", parseSymbolPath "async@2.2.5/Control.Concurrent.Async/concurrently")
  , testProperty "rejects: symbol without module" $ do
      assert $ P.eq P..$ ("expected", Left SymbolWithoutModule)
                   P..$ ("got", parseSymbolPath "async//concurrently")
  ]
