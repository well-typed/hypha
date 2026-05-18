module Main (main) where

import Hypha.Cli.Parser (parseCli)
import Hypha.Cli.Run (runCli)

main :: IO ()
main = do
  (flags, cmd) <- parseCli
  runCli flags cmd
