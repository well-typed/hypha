-- | End-to-end coverage for issue #9: both binaries must survive a
-- non-UTF-8 locale.
--
-- GHC derives every 'Handle'\'s encoding from the locale, so under
-- @C@/@POSIX@ — the default in bare containers, where @LANG@ is unset
-- — writing any non-ASCII character is a hard 'IOError':
--
-- > hypha: <stdout>: commitBuffer: invalid argument (cannot encode character '\8212')
--
-- Both entry points print non-ASCII unconditionally (the @hypha —
-- probe your Haskell build plan@ header, the @hypha-mcp v0.0.0 —@
-- banner), so the crash needed no unusual input: @hypha --help@ was
-- enough.  These tests spawn the real binaries with every locale
-- variable forced to @C@ and pin that the output still arrives, as
-- UTF-8, with the process exiting cleanly.
--
-- @build-tool-depends@ (see @hypha.cabal@) puts the freshly-built
-- binaries on @$PATH@ so these cannot pick up an older system install.
module Golden.Encoding (tests) where

import           Data.List          (isInfixOf, isPrefixOf)
import           System.Environment (getEnvironment)
import           System.Exit        (ExitCode (..))
import           System.Process     (CreateProcess (..), proc,
                                     readCreateProcessWithExitCode)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests = testGroup "Golden.Encoding"
  [ testCase "hypha --help under LC_ALL=C" $ do
      (ec, out, err) <- runInCLocale "hypha" ["--help"]
      assertNoEncodingCrash err
      assertEqual "exit code" ExitSuccess ec
      assertBool
        ("--help must keep its em-dash as UTF-8; got:\n" <> out)
        ("hypha \8212 probe" `isInfixOf` out)

  , testCase "hypha-mcp banner under LC_ALL=C" $ do
      -- Empty stdin: the server prints its banner, sees EOF, exits.
      (ec, _out, err) <- runInCLocale "hypha-mcp" []
      assertNoEncodingCrash err
      assertEqual "exit code" ExitSuccess ec
      assertBool
        ("banner must keep its em-dash as UTF-8; got:\n" <> err)
        ("MCP stdio server" `isInfixOf` err && "\8212" `isInfixOf` err)

  , testCase "non-ASCII argv under LC_ALL=C" $ do
      -- The other half of the locale problem, and the quieter one:
      -- 'getArgs' decodes with the /filesystem/ encoding, whose C-locale
      -- default escapes every byte >= 0x80 into lone surrogates.
      -- 'Data.Text.pack' then flattens those to U+FFFD, so hypha would
      -- act on a mangled argument and report success rather than fail.
      -- An unknown command is enough to get the argument echoed back,
      -- and needs no build plan in the working directory.
      (_ec, out, err) <- runInCLocale "hypha" ["caf\233"]
      assertNoEncodingCrash err
      assertBool
        ("argv must survive verbatim, not as U+FFFD; got:\n" <> out <> err)
        ("caf\233" `isInfixOf` (out <> err))
  ]

-- | Run a binary with every locale category forced to @C@, inheriting
-- the rest of the environment (notably @$PATH@, which cabal points at
-- the freshly-built executables).
runInCLocale :: FilePath -> [String] -> IO (ExitCode, String, String)
runInCLocale exe args = do
  env0 <- getEnvironment
  let env1 = [ kv | kv@(k, _) <- env0, not (isLocaleVar k) ]
             <> [("LC_ALL", "C"), ("LANG", "C")]
  readCreateProcessWithExitCode (proc exe args) { env = Just env1 } ""
  where
    isLocaleVar k = k == "LANG" || k == "LANGUAGE" || "LC_" `isPrefixOf` k

-- | The failure mode from issue #9, quoted from the report so the
-- assertion message stays greppable.
assertNoEncodingCrash :: String -> IO ()
assertNoEncodingCrash err =
  assertBool
    ("locale-derived encoding crash (issue #9); stderr was:\n" <> err)
    (not (any (`isInfixOf` err) ["commitBuffer", "cannot encode character"]))
