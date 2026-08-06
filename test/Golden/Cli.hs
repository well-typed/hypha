{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | End-to-end coverage that spawns the real @hypha@ binary and pins
-- the shape of what users actually see.
--
-- These tests exist because two regressions had to be debugged
-- twice: a stale second envelope and an @INTERNAL_ERROR: ExitSuccess@
-- stderr line printed on every successful command.  Both were
-- consequences of @System.exitWith@ raising an 'ExitCode' exception
-- that the top-level @handleAny@ misclassified as a crash.  The
-- in-process unit tests pin the handler logic; these tests pin the
-- observable end-to-end behaviour from the user's perspective:
--
--   * A successful command emits /exactly one/ JSON object on stdout.
--   * Stderr stays empty on success.
--   * The process exits with status 0.
--   * No @INTERNAL_ERROR@ envelope appears anywhere.
--
-- @build-tool-depends: hypha:hypha@ (see @hypha.cabal@) puts the
-- freshly-built binary on @$PATH@ so these tests cannot accidentally
-- pick up an older system install.
module Golden.Cli (tests) where

import           Control.Exception          (IOException, try)
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.KeyMap          as KeyMap
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           System.Directory           (copyFile, createDirectoryIfMissing)
import           System.Exit                (ExitCode (..))
import           System.FilePath            ((</>))
import           System.IO.Temp             (withSystemTempDirectory)
import           System.Process             (readProcessWithExitCode)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests = testGroup "Golden.Cli"
  [ testCase "hypha --help: clean stdout/stderr, exit 0, no INTERNAL_ERROR" $
      assertCleanInvocation ["--help"]

  , testCase "hypha --version: clean stdout/stderr, exit 0, no INTERNAL_ERROR" $
      -- --version is provided by optparse-applicative when configured;
      -- if hypha doesn't expose it the binary should still terminate
      -- cleanly with a usage envelope rather than the spurious crash
      -- envelope this suite was created to prevent.  We assert only
      -- the regression invariant, not the exact text.
      assertNoInternalErrorPollution ["--version"]

    -- Issue #20, through the wiring rather than around it.
    --
    -- Every in-process test of this behaviour hands the locator a reach
    -- built in the test.  The bug was that the CLI built it from nothing:
    -- `runSourceArm` passed `noImportedDefinitions`, so `hypha source
    -- base/Data.List/sortOn` reported NOT_FOUND while the resolution
    -- machinery was working perfectly.  Reverting one token in
    -- `Hypha.Cli.Run` reproduces it with the rest of the suite green, so
    -- this spawns the binary and follows the chain for real.
  , testCase "hypha source follows a cross-package re-export end to end" $ do
      (ec, out, err) <- readProcessWithExitCode "hypha"
        [ "--json", "--project-dir", facadeProject
        , "source", "reexport/Fixture.TwoHop/depThing" ] ""
      assertEqual ("exit code (stderr: " <> err <> ")") ExitSuccess ec
      -- The path is the assertion: it has to name the file in the
      -- dependency that declares the symbol, not the facade the question
      -- was asked through.
      assertBool
        ("expected a path inside reexport-dep; got:\n" <> out <> err)
        ("reexport-dep" `isInfixOfStr` out)
      assertBool
        ("expected the declaring module in defined_in; got:\n" <> out)
        ("Dep.Internal" `isInfixOfStr` out)

  , testCase "hypha symbol fills the card from the definition site" $ do
      -- The other half of the issue's title.  Before the reach reached
      -- this command, the same argument produced a card with no signature
      -- and no source while `hypha source` landed on the declaration.
      (ec, out, err) <- readProcessWithExitCode "hypha"
        [ "--json", "--project-dir", facadeProject
        , "symbol", "reexport/Fixture.TwoHop/depThing" ] ""
      assertEqual ("exit code (stderr: " <> err <> ")") ExitSuccess ec
      assertBool
        ("expected a signature on the card; got:\n" <> out)
        ("signature" `isInfixOfStr` out)
      assertBool
        ("expected the declaring module in defined_in; got:\n" <> out)
        ("Dep.Internal" `isInfixOfStr` out)

  , testCase "hypha source says how the search ended, not just that it did" $
      withNodepProjectForThisMachine $ \nodepProject -> do
        -- The honest-failure arm, through the binary.  Same package, in a
        -- plan that records no dependency edge, so the chain genuinely
        -- cannot be followed.  What the envelope must not do is report that
        -- as a bare "not found": the search stopped for a reason, and the
        -- reason is what tells an agent whether to retry, widen, or believe
        -- it.  The structured fields are the fix for that; the message alone
        -- was only ever a sentence on stderr.
        (ec, out, _err) <- readProcessWithExitCode "hypha"
          [ "--json", "--project-dir", nodepProject
          , "source", "reexport/Fixture.TwoHop/depThing" ] ""
        assertBool "a failing exit code" (ec /= ExitSuccess)
        assertBool
          ("expected the candidates it tried; got:\n" <> out)
          ("candidates_considered" `isInfixOfStr` out)
        -- The two halves of the verdict, together: @exhausted@ is only the
        -- honest word for a frontier that drained with nothing left unread,
        -- so a gap list beside it would be the contradiction the third
        -- verdict exists to prevent.  Asserting the pair is what keeps the
        -- rewritten plan honest as well -- if the owner oracle were still
        -- unreachable here, this would say so rather than quietly measure a
        -- different scenario.
        assertBool
          ("expected the search outcome; got:\n" <> out)
          ("exhausted" `isInfixOfStr` out)
        assertBool
          ("nothing may have been left unread on an exhausted search; got:\n" <> out)
          (not ("unreadable_dependencies" `isInfixOfStr` out))
        assertBool
          ("expected the facade's own import to be named; got:\n" <> out)
          ("Dep.Facade" `isInfixOfStr` out)

    -- Critical from review, measured against a real plan: routing `symbol`
    -- through the shared locator turned "no such module" into a confident
    -- wrong answer.  `hypha symbol containers/Data.Map.Strct/insertWith`
    -- (one letter missing) exited 0 with the *IntMap* signature and
    -- haddock, under `module: Data.Map.Strct` -- the misspelling echoed
    -- back as if it were real.  The package-wide scan ranks by shared path
    -- suffix, so it always finds something.
  , testCase "a module the package does not have is refused, not scanned for" $ do
      (ec, out, _err) <- readProcessWithExitCode "hypha"
        [ "--json", "--project-dir", facadeProject
        , "symbol", "reexport/Fixture.TwoHopp/depThing" ] ""
      assertBool ("a failing exit code; got:\n" <> out) (ec /= ExitSuccess)
      assertBool
        ("expected the module_absent verdict; got:\n" <> out)
        ("module_absent" `isInfixOfStr` out)
      -- The wrong answer this replaces was a *filled-in card*, so the
      -- absence of one is the assertion that matters.
      assertBool
        ("no signature may be offered for a module that is not there; got:\n" <> out)
        (not ("signature" `isInfixOfStr` out))

  , testCase "hypha source refuses the same absent module" $ do
      -- Both commands share the locator, so both had the defect.
      (ec, out, _err) <- readProcessWithExitCode "hypha"
        [ "--json", "--project-dir", facadeProject
        , "source", "reexport/Fixture.TwoHopp/depThing" ] ""
      assertBool ("a failing exit code; got:\n" <> out) (ec /= ExitSuccess)
      assertBool
        ("expected the module_absent verdict; got:\n" <> out)
        ("module_absent" `isInfixOfStr` out)
  ]
  where
    facadeProject = "test/fixtures/facade-project"

-- | The same package in a plan with the dependency edge removed, so the
-- re-export is unfollowable for a reason the plan explains.
nodepProjectFixture :: FilePath
nodepProjectFixture = "test/fixtures/facade-project-nodep"

-- | Where a cabal project keeps its plan, relative to the project root.
planPath :: FilePath
planPath = "dist-newstyle" </> "cache" </> "plan.json"

-- | The no-dependency-edge project, rewritten to name a compiler this
-- machine can actually reach.
--
-- The committed fixture says @ghc-9.6.7@, and that turned the test above
-- into a question about the machine rather than about hypha.  With no
-- dependency edge the walk has nothing to index, so it goes straight to the
-- owner oracle -- and building that oracle selects the /plan's/ compiler by
-- running it.  Where no reachable @ghc@ reports 9.6.7 the oracle cannot be
-- built at all, the reach records a @GapOwnerUnaskable@, and @search@ is
-- correctly @blocked@ rather than @exhausted@.  So the test passed under the
-- 9.6.7 CI image and failed under 9.10.3 and 9.12.4 for no reason other than
-- which compilers those images have on @$PATH@.
--
-- Naming the compiler that is on @$PATH@ puts the scenario back the way the
-- test describes it: the package databases are reachable, they answer that
-- nothing exposes the module, and the frontier drains with nothing left
-- unread -- which is what makes @exhausted@ the honest verdict.  Only the
-- @compiler-id@ is touched; the fixture stays the source of truth for
-- everything that makes it a plan with no dependency edge, including the
-- @pkg-src@ path, which cabal-plan reads relative to the working directory
-- and not to the project root.
withNodepProjectForThisMachine :: (FilePath -> IO a) -> IO a
withNodepProjectForThisMachine k = do
  compiler <- reachableCompilerId
  plan     <- fixturePlanObject
  withSystemTempDirectory "hypha-nodep-project" $ \root -> do
    copyFile (nodepProjectFixture </> "cabal.project") (root </> "cabal.project")
    createDirectoryIfMissing True (root </> "dist-newstyle" </> "cache")
    LBS8.writeFile (root </> planPath) $ Aeson.encode $ Aeson.Object $
      KeyMap.insert "compiler-id" (Aeson.String compiler) plan
    k root

-- | The fixture's plan as an object, so one field can be replaced without
-- restating the rest of it here.
--
-- This decodes a committed fixture, not anything hypha produced: the file is
-- input to the program under test, and the test is the one place that has to
-- vary a value in it.
fixturePlanObject :: IO Aeson.Object
fixturePlanObject = do
  decoded <- Aeson.eitherDecodeFileStrict' (nodepProjectFixture </> planPath)
  case decoded of
    Left err               -> assertFailure
      ("the fixture plan is unreadable, so the test would measure nothing: "
        <> err)
    Right (Aeson.Object o) -> pure o
    Right _                -> assertFailure
      "the fixture plan is not a JSON object"

-- | The @ghc@ on @$PATH@, as the @compiler-id@ a plan would record.
--
-- A failure here is the test's own precondition failing, so it is reported
-- as one: the suite cannot ask what a reachable compiler answers on a
-- machine that has none, and silently asserting the other verdict instead
-- would be measuring a different thing under the same name.
reachableCompilerId :: IO Text
reachableCompilerId = do
  run <- try @IOException (readProcessWithExitCode "ghc" ["--numeric-version"] "")
  case run of
    Left err -> assertFailure
      ("this test needs a ghc on $PATH to name in the plan: " <> show err)
    Right (ExitSuccess, out, _err) ->
      pure ("ghc-" <> Text.strip (Text.pack out))
    Right (ec, _out, err) -> assertFailure
      ("ghc --numeric-version failed (" <> show ec <> "): " <> err)

-- | Strict invariant: stdout is exactly one well-formed JSON object,
-- stderr is empty, exit code is 'ExitSuccess', and at no point does
-- the @INTERNAL_ERROR@ marker leak into either stream.
assertCleanInvocation :: [String] -> IO ()
assertCleanInvocation args = do
  (ec, out, err) <- readProcessWithExitCode "hypha" args ""
  assertEqual "stderr must be empty on success" "" err
  assertEqual "exit code" ExitSuccess ec
  assertBool "no INTERNAL_ERROR marker on stdout" $
    not ("INTERNAL_ERROR" `isInfixOfStr` out)
  assertSingleJsonObjectOrPlainText out

-- | Weaker invariant used for invocations whose stdout we don't pin
-- exactly: only assert that the @INTERNAL_ERROR@ regression marker is
-- absent from both streams and that there is no second envelope
-- appended to stdout.
assertNoInternalErrorPollution :: [String] -> IO ()
assertNoInternalErrorPollution args = do
  (_ec, out, err) <- readProcessWithExitCode "hypha" args ""
  assertBool "no INTERNAL_ERROR marker on stderr" $
    not ("INTERNAL_ERROR" `isInfixOfStr` err)
  assertBool "no INTERNAL_ERROR marker on stdout" $
    not ("INTERNAL_ERROR" `isInfixOfStr` out)

-- | Either stdout decodes as exactly one JSON value with no trailing
-- bytes, or it's plain text / YAML (e.g. @--help@) with no embedded
-- envelope.  The point is that we never emit two responses for one
-- invocation.
assertSingleJsonObjectOrPlainText :: String -> IO ()
assertSingleJsonObjectOrPlainText raw =
  case Aeson.decode @Aeson.Value (LBS8.pack raw) of
    Just _ ->
      -- It parsed as one JSON value covering all the bytes; good.
      pure ()
    Nothing ->
      -- Not pure JSON.  Make sure no envelope is /embedded/ in plain
      -- text — that is the exact shape of the regression (help text
      -- followed by a stray envelope).  We check for the error code
      -- marker that every internal-error envelope carries.
      assertBool
        ("plain-text output must not contain an embedded envelope; got:\n"
          <> raw)
        (not ("INTERNAL_ERROR" `isInfixOfStr` raw))

-- | @isInfixOf@ for strings.  We avoid pulling in @Data.List@'s
-- polymorphic version under another name to keep the import list flat.
isInfixOfStr :: String -> String -> Bool
isInfixOfStr needle hay = needle `isInfixOf'` hay
  where
    isInfixOf' n h
      | length n > length h = False
      | n == take (length n) h = True
      | otherwise = case h of
          []     -> False
          (_:hs) -> isInfixOf' n hs

