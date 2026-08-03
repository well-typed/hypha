{-# LANGUAGE OverloadedStrings #-}
module Golden.Source (tests) where

import qualified Data.ByteString.Lazy as LBS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Hypha.BuildEnv.Mock (MockBuildEnv (..), emptyMock, mkMockBuildEnv)
import Hypha.Command.Source (runSource)
import Hypha.Error (HyphaError (..), NotFoundReason (..))
import Hypha.Output.Json
  ( EnvelopeOpts (..), encodeEnvelopeValue, encodeErrorEnvelope
  , encodeOutcomeBytes )
import Hypha.Output.Outcome (Outcome)
import Hypha.Package.Resolver (PackageResolver (..), ResolvedPackage (..))
import Hypha.Search.Index (OutsideReach, noOutsideReach)
import Hypha.Source.Dependencies (dependencyReach)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Data.Aeson (Value)
import qualified Data.Map.Strict as Map

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
    -- somewhere else in the package.
  , goldenVsString
      "source outside a plan reports the re-export it cannot follow"
      (golden "source-facade-no-plan.compact.json")
      (runFacadeSource noOutsideReach)
  ]
  where
    golden name = "test" </> "Golden" </> "golden" </> name

runSourceCommand :: IO LBS.ByteString
runSourceCommand = do
  -- Create a mock build env with async source
  let asyncSrcDir = "test" </> "fixtures" </> "fake-cabal-store" </> "ghc-9.6.7"
                    </> "async-2.2.5-abc123456789" </> "share" </> "async"
      asyncId = PackageId (PackageName "async") (Version "2.2.5")
      mock = emptyMock
        { mockPackages = Map.fromList
            [ (asyncId, (Just asyncSrcDir, Nothing))
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

-- The two fixture packages, as the plan and the resolver see them.

reexportId, reexportDepId :: PackageId
reexportId    = PackageId (PackageName "reexport") (Version "0.1.0")
reexportDepId = PackageId (PackageName "reexport-dep") (Version "0.1.0")

reexportDir, reexportDepDir :: FilePath
reexportDir    = "test" </> "fixtures" </> "reexport"
reexportDepDir = "test" </> "fixtures" </> "reexport-dep"

-- | A plan in which @reexport@ depends on @reexport-dep@.  That edge is
-- the whole input to 'dependencyReach': it is what tells the CLI which
-- packages a re-export is allowed to leave into.
fixturePlan :: BuildPlan
fixturePlan = emptyBuildPlan
  { bpUnits = Map.fromList
      [ (PackageName "reexport", unit reexportId reexportDir [reexportDepId])
      , (PackageName "reexport-dep", unit reexportDepId reexportDepDir [])
      ]
  }
  where
    unit pid dir deps = PlannedUnit
      { puId            = pid
      , puDeps          = deps
      , puIsLocal       = True
      , puOrigin        = OriginLocal dir
      , puSrcDir        = Just dir
      , puDistDir       = Nothing
      , puLibComponents = []
      }

-- | Answers for the two fixture packages and nothing else, so a lookup for
-- any other package fails the way the real resolver fails rather than
-- quietly handing back a directory.
fixtureResolver :: PackageResolver IO
fixtureResolver = PackageResolver
  { resolvePkg = \name -> pure $ case dirOf name of
      Just (pid, dir) -> Right ResolvedPackage
        { rpPkgId         = pid
        , rpIsOutsidePlan = False
        , rpIsLocal       = True
        , rpDepsCount     = 0
        , rpOrigin        = OriginLocal dir
        }
      Nothing -> Left (NotFound (NotFoundPackageInPlan name))
  , resolveSrc = \pid -> pure $ case dirOf (pkgName pid) of
      Just (_, dir) -> Right dir
      Nothing       -> Left (NotFound (NotFoundPackageInPlan (pkgName pid)))
  , fetchVrs   = \_ -> pure (Right [])
  }
  where
    dirOf name
      | name == pkgName reexportId    = Just (reexportId, reexportDir)
      | name == pkgName reexportDepId = Just (reexportDepId, reexportDepDir)
      | otherwise                     = Nothing

-- | The real producer over the fixture plan — the same call the CLI makes.
planBackedReach :: IO (OutsideReach IO)
planBackedReach = dependencyReach fixturePlan fixtureResolver reexportId

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

compactKeys, fullKeys :: Set Text
compactKeys = Set.fromList ["package", "module", "symbol", "path", "line", "snippet"]
fullKeys = Set.fromList ["package", "version", "module", "symbol", "path", "line", "snippet"]
