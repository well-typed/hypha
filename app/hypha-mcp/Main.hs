module Main (main) where

import Control.Exception.Safe (catchAny)

import Hypha.Cli.Run (reportInternalError)
import Hypha.Encoding (setUtf8Encoding)
import Hypha.Mcp.Server (runMcpStdio)

-- | Top-level @catchAny@ for the @hypha-mcp@ binary.  Shares the same
-- last-resort handler as the @hypha@ CLI so an unhandled exception
-- crash terminates with a structured @INTERNAL_ERROR@ envelope and
-- exit code 9.  Specific exception families ('HttpException', ENOENT
-- on tool binaries) are still caught inside the library at their
-- proper layer.
main :: IO ()
main = start `catchAny` reportInternalError
  where
    -- 'setUtf8Encoding' comes before the banner, which carries an
    -- em-dash (issue #9).  The JSON-RPC traffic itself is written as
    -- raw bytes and so bypasses the handle's encoding, but stderr and
    -- any handle we open later do not.
    start = setUtf8Encoding >> runMcpStdio
