{-# LANGUAGE OverloadedStrings #-}
module Golden.Server (tests) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import Lucid (renderText, p_, code_, toHtml)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Hypha.Server.Ui.Layout (shellPage)
import Hypha.Types.BuildPlan (PackageOrigin (..))

tests :: TestTree
tests = testGroup "Golden.Server"
  [ goldenVsString
      "home page renders deterministically"
      goldenFile
      renderHome
  ]
  where
    goldenFile = "test" </> "Golden" </> "golden" </> "server-home.html"

renderHome :: IO LBS.ByteString
renderHome =
  let page = shellPage "fixture-project" []
               [("async", OriginHackage), ("containers", OriginHackage)] body
      body = p_ $ do
        toHtml ("Welcome to hypha. Press " :: Text.Text)
        code_ "s"
        toHtml (" to search." :: Text.Text)
      txt  = LText.toStrict (renderText page)
  in pure (LBS.fromStrict (Text.encodeUtf8 txt))
