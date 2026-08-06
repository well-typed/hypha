{-# LANGUAGE OverloadedStrings #-}
module Golden.Source (tests) where

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Hypha.BuildEnv.Mock (MockBuildEnv (..), emptyMock, mkMockBuildEnv)
import Hypha.Command.Source (runSource)
import Hypha.Command.Source qualified as Source
import Hypha.Output.Json
  ( EnvelopeOpts (..), encodeEnvelopeValue, encodeErrorEnvelope
  , encodeOutcomeBytes )
import Hypha.Output.Outcome (Outcome)
import Hypha.Source.Reach (OutsideReach, noOutsideReach)
import Util.Fixture
  ( asyncDir, asyncId, planBackedReach, planlessReach, reexportDir
  , reexportId )

tests :: TestTree
tests = testGroup "Golden.Source"
  [ goldenVsString
      "source-async-concurrently produces expected JSON"
      (golden "source-async-concurrently.compact.json")
      runSourceCommand

    -- Issue #20: the facade shape.  @Fixture.TwoHop@ re-exports
    -- @depThing@ from @Dep.Facade@, which re-exports it from
    -- @Dep.Internal@, and both hops cross into another package — the shape
    -- @base@ has had for every symbol since GHC 9.10 turned it into a
    -- facade over @ghc-internal@.  The @path@ in the golden is the point:
    -- it names the file that declares the symbol, over in the dependency,
    -- not the facade the question was asked through.
  , goldenVsString
      "source follows a re-export across a package boundary"
      (golden "source-facade-two-hop.compact.json")
      (runFacadeSource =<< planBackedReach)

    -- The other half of the same behaviour.  With no plan there is no
    -- dependency graph to follow, so the same query has to come back
    -- empty-handed rather than answer with a same-named binding from
    -- somewhere else in the package -- and it has to say which of the two
    -- it is, which is what the pinned error carries.
  , goldenVsString
      "source outside a plan reports the re-export it cannot follow"
      (golden "source-facade-no-plan.compact.json")
      (runFacadeSource =<< planlessReach)
  ]
  where
    golden name = "test" </> "Golden" </> "golden" </> name

runSourceCommand :: IO LBS.ByteString
runSourceCommand = do
  -- Create a mock build env with async source
  let mock = emptyMock
        { mockPackages = Map.fromList
            [ (asyncId, (Just asyncDir, Nothing))
            ]
        }
      env = mkMockBuildEnv mock
      modPath = "Control.Concurrent.Async" :: Text
      sym = Nothing :: Maybe Text  -- No symbol, just module header

  result <- runSource env noOutsideReach asyncId modPath sym
  case result of
    Left err -> do
      putStrLn ("Source command failed: " ++ show err)
      error "Source command failed unexpectedly"
    Right outcome -> pure (encodeSuccess outcome)

runFacadeSource :: OutsideReach IO -> IO LBS.ByteString
runFacadeSource reach = do
  let env = mkMockBuildEnv emptyMock
        { mockPackages =
            Map.fromList [ (reexportId, (Just reexportDir, Nothing)) ]
        }
  result <- runSource env reach reexportId "Fixture.TwoHop" (Just "depThing")
  pure $ case result of
    Right outcome -> encodeSuccess outcome
    -- The plan-less arm is /meant/ to fail, and the shape of that failure
    -- is what is under test: pinning it here is what stops it degrading
    -- into a guess later.
    Left err      -> encodeValue (encodeErrorEnvelope err)

encodeSuccess :: Outcome Value -> LBS.ByteString
encodeSuccess = encodeOutcomeBytes envelopeOpts compactKeys fullKeys

encodeValue :: Value -> LBS.ByteString
encodeValue = encodeEnvelopeValue envelopeOpts

envelopeOpts :: EnvelopeOpts
envelopeOpts = EnvelopeOpts
  { eoFull       = False
  , eoSelect     = []
  , eoPrettyJson = False
  }

-- The command's own field sets, not a copy of them: a copy would decide
-- what the golden shows independently of what the CLI shows, so a field
-- added to one and not the other would go unnoticed.
compactKeys, fullKeys :: Set Text
compactKeys = Source.compactKeys
fullKeys    = Source.fullKeys
