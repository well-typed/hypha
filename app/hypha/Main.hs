module Main (main) where

import Control.Exception.Safe

import Hypha.Cli.Parser (parseCli)
import Hypha.Cli.Run (runCli, topLevelHandler)

-- | The main hypha CLI entrypoint.
main :: IO ()
main = handleAny topLevelHandler $ do
  (opts, cmd) <- parseCli
  runCli opts cmd
