module Main (main) where

import Control.Exception.Safe (catchAny)

import Hypha.Cli.Parser (parseCli)
import Hypha.Cli.Run (reportInternalError, runCli)

-- | The single, top-level @catchAny@ for the @hypha@ binary.  Library
-- code (see "Hypha.Hoogle.Remote", "Hypha.Hackage.*") only catches the
-- specific exception families it knows how to handle structurally
-- ('HttpException', the targeted 'IOException' patterns).  Everything
-- else — programmer errors, async cancellations, exotic IO failures —
-- propagates here and is reported as a single, well-formed
-- @INTERNAL_ERROR@ envelope by 'reportInternalError'.
main :: IO ()
main = (do
  (flags, cmd) <- parseCli
  runCli flags cmd
  ) `catchAny` reportInternalError
