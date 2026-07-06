module Main (main) where

import Hypha.Cli.Parser (parseCli)
import Hypha.Cli.Run (runClientMain, runServerMain)
import Hypha.Cli.Types (Command (..))

-- | The main hypha CLI entrypoint.
main :: IO ()
main = do
  (opts, cmd) <- parseCli
  case cmd of
    ClientCommands ccmd -> runClientMain opts ccmd
    ServerCommands scmd -> runServerMain opts scmd
