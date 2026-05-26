module Main (main) where

import Control.Exception.Safe

import Hypha.Cli.Parser (parseCli)
import Hypha.Cli.Run (runCli, topLevelHandler)

-- | The single, top-level @catchAny@ for the @hypha@ binary.  Library
-- code (see "Hypha.Hoogle.Remote", "Hypha.Hackage.*") only catches the
-- specific exception families it knows how to handle structurally
-- ('HttpException', the targeted 'IOException' patterns).  Everything
-- else — programmer errors, async cancellations, exotic IO failures —
-- propagates here, where 'topLevelHandler' routes genuine crashes to
-- the @INTERNAL_ERROR@ envelope and lets the normal 'ExitCode' control
-- signal through to the runtime.
main :: IO ()
main = handleAny topLevelHandler $ do
  (flags, cmd) <- parseCli
  runCli flags cmd
