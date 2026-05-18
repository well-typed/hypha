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

import Hypha.Server.Haddock.Rewrite (rewriteHaddockHtml)

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
  ]
