{-# LANGUAGE OverloadedStrings #-}
module Property.HaddockRewrite (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range
import Test.Falsify.Property (gen, assert)
import qualified Test.Falsify.Predicate as P
import qualified Data.Text as Text
import Data.Text (Text)

import Hypha.Server.Haddock.Rewrite
  ( EmbedContext (..), rewriteEmbeddedDocHtml, rewriteHaddockHtml )

genHtml :: Gen.Gen Text
genHtml = do
  segs <- Gen.list (Range.between (1, 4)) $ Gen.elem
            (   pure "<a href=\"../async-2.2.5/index.html\">x</a>"
             <> pure "<a href=\"#anchor\">y</a>"
             <> pure "<p>hello</p>"
             <> pure "<a href=\"http://example.com\">z</a>")
  pure (Text.concat segs)

tests :: TestTree
tests = testGroup "Haddock.Rewrite"
  [ testProperty "rewrite is idempotent" $ do
      h <- gen genHtml
      let once  = rewriteHaddockHtml h
          twice = rewriteHaddockHtml once
      assert $ P.eq P..$ ("once", once) P..$ ("twice", twice)

  , testProperty "rewrite preserves non-pkg hrefs" $ do
      let h = "<a href=\"#frag\">x</a><a href=\"http://x\">y</a>" :: Text
          r = rewriteHaddockHtml h
      assert $ P.eq P..$ ("expected", h) P..$ ("got", r)

  , testProperty "embedded rewrite is idempotent" $ do
      h <- gen genEmbeddedHtml
      let once  = rewriteEmbeddedDocHtml ctx h
          twice = rewriteEmbeddedDocHtml ctx once
      assert $ P.eq P..$ ("once", once) P..$ ("twice", twice)

  , testProperty "embedded rewrite: cross-package link goes to /haddock/" $
      expectEmbedded
        "<a href=\"../base-4.19.0.0/Data-Int.html#t:Int\">Int</a>"
        "<a href=\"/haddock/base-4.19.0.0/Data-Int.html#t:Int\">Int</a>"

  , testProperty "embedded rewrite: sibling module goes to /pkg/ and keeps the fragment" $
      expectEmbedded
        "<a href=\"Data-Map-Strict.html#v:lookup\">lookup</a>"
        "<a href=\"/pkg/containers/Data.Map.Strict#v:lookup\">lookup</a>"

  , testProperty "embedded rewrite: src pages go to the raw haddock route" $
      expectEmbedded
        "<a href=\"src/Data.Map.html#lookup\">Source</a>"
        "<a href=\"/haddock/containers-0.7/src/Data.Map.html#lookup\">Source</a>"

  , testProperty "embedded rewrite: package index pages go to the raw haddock route" $
      expectEmbedded
        "<a href=\"doc-index.html\">Index</a>"
        "<a href=\"/haddock/containers-0.7/doc-index.html\">Index</a>"

  , testProperty "embedded rewrite: img src is rewritten too" $
      expectEmbedded
        "<img src=\"src/diagram.html\">"
        "<img src=\"/haddock/containers-0.7/src/diagram.html\">"

  , testProperty "embedded rewrite leaves anchors, absolute, and scheme URLs alone" $ do
      let h = Text.concat
            [ "<a href=\"#g:1\">top</a>"
            , "<a href=\"/pkg/containers\">abs</a>"
            , "<a href=\"https://haskell.org\">web</a>"
            , "<a href=\"mailto:x@y.z\">mail</a>"
            ] :: Text
      assert $ P.eq
        P..$ ("expected", h)
        P..$ ("got", rewriteEmbeddedDocHtml ctx h)
  ]
  where
    ctx = EmbedContext { ecComponent = "containers", ecPkgVer = "containers-0.7" }

    expectEmbedded h expected =
      assert $ P.eq
        P..$ ("expected", expected :: Text)
        P..$ ("got", rewriteEmbeddedDocHtml ctx h)

genEmbeddedHtml :: Gen.Gen Text
genEmbeddedHtml = do
  segs <- Gen.list (Range.between (1, 5)) $ Gen.elem
            (   pure "<a href=\"../async-2.2.5/index.html\">x</a>"
             <> pure "<a href=\"Data-Map-Strict.html#v:lookup\">l</a>"
             <> pure "<a href=\"src/Data.Map.html\">s</a>"
             <> pure "<a href=\"doc-index.html\">i</a>"
             <> pure "<a href=\"#anchor\">y</a>"
             <> pure "<img src=\"src/pic.html\">"
             <> pure "<p>hello</p>"
             <> pure "<a href=\"http://example.com\">z</a>")
  pure (Text.concat segs)
