module Main (main) where

import Hypha.Cli.Parser (parseCli)
import Hypha.Cli.Run (runClientMain, runServerMain)
import Hypha.Cli.Types (Command (..))
import Hypha.Encoding (setUtf8Encoding)

-- | The main hypha CLI entrypoint.
main :: IO ()
main = do
  -- Before 'parseCli': @--help@ prints an em-dash, which is fatal on a
  -- handle left with the locale's ASCII encoding (issue #9).
  setUtf8Encoding
  (opts, cmd) <- parseCli
  case cmd of
    ClientCommands ccmd -> runClientMain opts ccmd
    ServerCommands scmd -> runServerMain opts scmd
