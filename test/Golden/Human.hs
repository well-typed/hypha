{-# LANGUAGE OverloadedStrings #-}
module Golden.Human (tests) where

import qualified Data.Text as Text
import Prettyprinter (layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Terminal (renderStrict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8

import Hypha.Output.Human (renderSymbolCard)

tests :: TestTree
tests = testGroup "Golden.Human"
  [ goldenVsString
      "human-symbol-async-concurrently produces expected ANSI"
      goldenFile
      renderTestCard
  ]
  where
    goldenFile = "test" </> "Golden" </> "golden" </> "human-symbol-async-concurrently.ansi"

renderTestCard :: IO LBS.ByteString
renderTestCard = do
  let doc = renderSymbolCard
              "concurrently"                    -- name
              "function"                        -- kind
              "concurrently :: IO a -> IO b -> IO (a, b)"  -- signature
              "Run two IO actions concurrently." -- haddock
              "src/Control/Concurrent/Async.hs" -- source path
              42                                -- source line
      rendered = renderStrict (layoutPretty defaultLayoutOptions doc)
  pure (LBS.fromStrict (BS8.pack (Text.unpack rendered)))
