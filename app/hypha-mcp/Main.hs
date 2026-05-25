module Main (main) where

import Control.Exception.Safe (catchAny)

import Hypha.Cli.Run (reportInternalError)
import Hypha.Mcp.Server (runMcpStdio)

-- | Top-level @catchAny@ for the @hypha-mcp@ binary.  Shares the same
-- last-resort handler as the @hypha@ CLI so an unhandled exception
-- crash terminates with a structured @INTERNAL_ERROR@ envelope and
-- exit code 9.  Specific exception families ('HttpException', ENOENT
-- on tool binaries) are still caught inside the library at their
-- proper layer.
main :: IO ()
main = runMcpStdio `catchAny` reportInternalError
