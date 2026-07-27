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

import Hypha.Server.ModuleDoc
import Hypha.Server.Ui.Layout (shellPage)
import Hypha.Server.Ui.ModuleDoc (modulePage)
import Hypha.Source.Extract (DocEntry (..), EntryOrigin (..), ModuleDocInfo (..))
import Hypha.Source.Parser (DeclKind (..))
import Hypha.Types.BuildPlan (PackageOrigin (..))
import Hypha.Types.Doc (DocText (..))

tests :: TestTree
tests = testGroup "Golden.Server"
  [ goldenVsString
      "home page renders deterministically"
      (goldenFile "server-home.html")
      renderHome
  , goldenVsString
      "module page renders source-extracted docs"
      (goldenFile "server-module-source.html")
      (renderPage (modulePage "fixture-pkg" "Data.Fixture" sourceView))
  , goldenVsString
      "module page embeds prebuilt haddock"
      (goldenFile "server-module-prebuilt.html")
      (renderPage (modulePage "fixture-pkg" "Data.Fixture" prebuiltView))
  , goldenVsString
      "module page degrades to exports with a visible reason"
      (goldenFile "server-module-exports.html")
      (renderPage (modulePage "fixture-pkg" "Data.Fixture"
        (ViewExportsOnly ["frob", "Gadget"]
           "module source could not be parsed: parse error")))
  ]
  where
    goldenFile name = "test" </> "Golden" </> "golden" </> name

    sourceView = ViewFromSource SourceDoc
      { sdInfo = ModuleDocInfo
          { mdiHeader  = Just (DocText "Fixture module header prose.")
          , mdiEntries =
              [ DocEntry
                  { deName      = "Gadget"
                  , deKind      = DkData
                  , deSignature = Just "data Gadget = MkGadget !Int"
                  , deHaddock   = Just (DocText "A gadget.")
                  , deSigLine   = Nothing
                  , deDefLine   = Just 12
                  , deOrigin    = EntryLocal
                  }
              , DocEntry
                  { deName      = "frob"
                  , deKind      = DkFunction
                  , deSignature = Just "frob :: Gadget -> Int"
                  , deHaddock   = Just (DocText "Frobnicate the gadget.")
                  , deSigLine   = Just 17
                  , deDefLine   = Just 18
                  , deOrigin    = EntryLocal
                  }
              ]
          }
      , sdRawHaddock = Just "fixture-pkg-0.1.0.0"
      }

    prebuiltView = ViewPrebuilt PrebuiltDoc
      { pdPkgVer      = "fixture-pkg-0.1.0.0"
      , pdDescription = Just "<div class=\"doc\"><p>Prebuilt description.</p></div>"
      , pdInterface   =
          "<div class=\"top\"><p class=\"src\"><a id=\"v:frob\" class=\"def\">frob</a>\
          \ :: Int -&gt; Int</p><div class=\"doc\"><p>Frobnicate.</p></div></div>"
      , pdContents    = Just "<ul><li><a href=\"#g:1\">Basics</a></li></ul>"
      }

    renderPage page =
      pure (LBS.fromStrict (Text.encodeUtf8 (LText.toStrict (renderText page))))

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
