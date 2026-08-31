module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Golden.Cli
import qualified Golden.Encoding
import qualified Golden.Human
import qualified Golden.Lookup
import qualified Golden.Package
import qualified Golden.Server
import qualified Golden.Source
import qualified Golden.Symbol
import qualified Golden.Versions
import qualified Property.BuildPlanOrder
import qualified Property.ComponentName
import qualified Property.LookupCascade
import qualified Property.LookupOutcomeShape
import qualified Property.HackageCache
import qualified Property.HaddockRewrite
import qualified Property.OutputJson
import qualified Property.SearchRanking
import qualified Property.SymbolPath
import qualified Unit.BuildEnv
import qualified Unit.BuildEnvCompose
import qualified Unit.BuildPlanOrder
import qualified Unit.CliParser
import qualified Unit.Components
import qualified Unit.CppMacros
import qualified Unit.Deps
import qualified Unit.Doctor
import qualified Unit.EmbeddedAssets
import qualified Unit.Hackage
import qualified Unit.Mcp
import qualified Unit.PackageCache
import qualified Unit.PackageCacheFingerprint
import qualified Unit.HoogleLocalGen
import qualified Unit.GhcIncludes
import qualified Unit.HoogleRemote
import qualified Unit.PackageCacheLookup
import qualified Unit.Module
import qualified Unit.Project
import qualified Unit.Server
import qualified Unit.ServerSlots
import qualified Unit.Haddock
import qualified Unit.HaddockExtract
import qualified Unit.InternalError
import qualified Unit.ImportedSources
import qualified Unit.LookupPrepLocal
import qualified Unit.RepoCache
import qualified Unit.Route
import qualified Unit.SourceExtensions
import qualified Unit.SearchCollapse
import qualified Unit.SearchExports
import qualified Unit.SearchIndexBuild
import qualified Unit.SearchIndexCache
import qualified Unit.SearchReexport
import qualified Unit.SourceInterface
import qualified Unit.SourceExtract
import qualified Unit.SourceDependencies
import qualified Unit.SourceLocate
import qualified Unit.SourceOrigins
import qualified Unit.SourceParser

import Hypha.Encoding (setUtf8Encoding)

main :: IO ()
main = do
  -- Same pin as the binaries: the pipes we read child output from
  -- inherit the locale encoding otherwise, which would make these
  -- tests fail for the very reason they exist (issue #9).
  setUtf8Encoding
  defaultMain allTests

allTests :: TestTree
allTests = testGroup "hypha"
  [ Golden.Cli.tests
  , Golden.Encoding.tests
  , Golden.Human.tests
  , Golden.Lookup.tests
  , Golden.Package.tests
  , Golden.Server.tests
  , Golden.Source.tests
  , Golden.Symbol.tests
  , Golden.Versions.tests
  , Property.BuildPlanOrder.tests
  , Property.ComponentName.tests
  , Property.LookupCascade.tests
  , Property.LookupOutcomeShape.tests
  , Property.HaddockRewrite.tests
  , Property.SearchRanking.tests
  , Property.SymbolPath.tests
  , Property.HackageCache.tests
  , Property.OutputJson.tests
  , Unit.BuildEnv.tests
  , Unit.BuildEnvCompose.tests
  , Unit.BuildPlanOrder.tests
  , Unit.CliParser.tests
  , Unit.Components.tests
  , Unit.CppMacros.tests
  , Unit.Deps.tests
  , Unit.Doctor.tests
  , Unit.EmbeddedAssets.tests
  , Unit.Project.tests
  , Unit.Hackage.tests
  , Unit.Mcp.tests
  , Unit.PackageCache.tests
  , Unit.PackageCacheFingerprint.tests
  , Unit.HoogleLocalGen.tests
  , Unit.GhcIncludes.tests
  , Unit.HoogleRemote.tests
  , Unit.PackageCacheLookup.tests
  , Unit.Module.tests
  , Unit.Server.tests
  , Unit.ServerSlots.tests
  , Unit.Haddock.tests
  , Unit.HaddockExtract.tests
  , Unit.InternalError.tests
  , Unit.ImportedSources.tests
  , Unit.LookupPrepLocal.tests
  , Unit.RepoCache.tests
  , Unit.Route.tests
  , Unit.SourceExtensions.tests
  , Unit.SearchCollapse.tests
  , Unit.SearchExports.tests
  , Unit.SearchIndexBuild.tests
  , Unit.SearchIndexCache.tests
  , Unit.SearchReexport.tests
  , Unit.SourceInterface.tests
  , Unit.SourceExtract.tests
  , Unit.SourceDependencies.tests
  , Unit.SourceLocate.tests
  , Unit.SourceOrigins.tests
  , Unit.SourceParser.tests
  ]
