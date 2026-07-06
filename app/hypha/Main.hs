module Main (main) where

import Control.Exception.Safe

import Hypha.Cli.Parser (parseCli)
import Hypha.Cli.Run (runCli, topLevelHandler, processError, processOutcome)
import Hypha.Cli.Types (commandTag)
import Hypha.Types

-- | The main hypha CLI entrypoint.  The command tag attached to an
-- error envelope is a pure function of the parsed command, so it is
-- paired with the error here — at the rendering boundary — rather than
-- carried through the 'Hypha' monad.
main :: IO ()
main = handleAny topLevelHandler $ do
  (opts, cmd) <- parseCli
  result <- runHypha opts $ runCli cmd
  case result of
    Right outcome -> processOutcome opts outcome
    Left err      -> processError opts (commandTag cmd) err