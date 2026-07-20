{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for "Hypha.Server.Haddock.Extract" — slicing the
-- embeddable regions out of a prebuilt Haddock module page.
module Unit.HaddockExtract (tests) where

import qualified Data.Text    as Text
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Hypha.Server.Haddock.Extract
  ( PrebuiltParts (..), extractModuleDocHtml )

fixturePath :: FilePath
fixturePath = "test" </> "fixtures" </> "haddock" </> "module-fixture.html"

tests :: TestTree
tests = testGroup "Unit.HaddockExtract"
  [ testCase "extracts interface, description, and contents regions" $ do
      html <- TIO.readFile fixturePath
      case extractModuleDocHtml html of
        Nothing -> assertFailure "expected PrebuiltParts, got Nothing"
        Just pp -> do
          assertBool "interface has the value anchor"
            ("id=\"v:frob\"" `Text.isInfixOf` ppInterface pp)
          assertBool "interface keeps nested constructor tables"
            ("MkGadget" `Text.isInfixOf` ppInterface pp)
          assertBool "interface stops at its own closing div"
            (not ("Produced by Haddock" `Text.isInfixOf` ppInterface pp))
          desc <- maybe (assertFailure "no description") pure (ppDescription pp)
          assertBool "description prose present"
            ("A fixture module for extraction tests." `Text.isInfixOf` desc)
          toc <- maybe (assertFailure "no contents") pure (ppContents pp)
          assertBool "contents links to groups" ("#g:1" `Text.isInfixOf` toc)

  , testCase "page without #interface is rejected" $
      extractModuleDocHtml "<html><body><p>no interface here</p></body></html>"
        @?= Nothing
  ]
