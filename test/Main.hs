module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Golden.Cli
import qualified Golden.Human
import qualified Golden.Lookup
import qualified Golden.Package
import qualified Golden.Server
import qualified Golden.Source
import qualified Golden.Symbol
import qualified Golden.Versions
import qualified Property.ComponentName
import qualified Property.LookupCascade
import qualified Property.LookupOutcomeShape
import qualified Property.HackageCache
import qualified Property.HaddockRewrite
import qualified Property.OutputJson
import qualified Property.SymbolPath
import qualified Unit.BuildEnv
import qualified Unit.BuildEnvCompose
import qualified Unit.Components
import qualified Unit.Deps
import qualified Unit.Doctor
import qualified Unit.EmbeddedAssets
import qualified Unit.Hackage
import qualified Unit.Mcp
import qualified Unit.PackageCache
import qualified Unit.PackageCacheFingerprint
import qualified Unit.HoogleLocalGen
import qualified Unit.HoogleRemote
import qualified Unit.PackageCacheLookup
import qualified Unit.Module
import qualified Unit.Project
import qualified Unit.Server
import qualified Unit.ServerSlots
import qualified Unit.Haddock
import qualified Unit.HaddockExtract
import qualified Unit.InternalError
import qualified Unit.LookupPrepLocal
import qualified Unit.RepoCache
import qualified Unit.SourceExtract
import qualified Unit.SourceParser

main :: IO ()
main = defaultMain (testGroup "hypha"
  [ Golden.Cli.tests
  , Golden.Human.tests
  , Golden.Lookup.tests
  , Golden.Package.tests
  , Golden.Server.tests
  , Golden.Source.tests
  , Golden.Symbol.tests
  , Golden.Versions.tests
  , Property.ComponentName.tests
  , Property.LookupCascade.tests
  , Property.LookupOutcomeShape.tests
  , Property.HaddockRewrite.tests
  , Property.SymbolPath.tests
  , Property.HackageCache.tests
  , Property.OutputJson.tests
  , Unit.BuildEnv.tests
  , Unit.BuildEnvCompose.tests
  , Unit.Components.tests
  , Unit.Deps.tests
  , Unit.Doctor.tests
  , Unit.EmbeddedAssets.tests
  , Unit.Project.tests
  , Unit.Hackage.tests
  , Unit.Mcp.tests
  , Unit.PackageCache.tests
  , Unit.PackageCacheFingerprint.tests
  , Unit.HoogleLocalGen.tests
  , Unit.HoogleRemote.tests
  , Unit.PackageCacheLookup.tests
  , Unit.Module.tests
  , Unit.Server.tests
  , Unit.ServerSlots.tests
  , Unit.Haddock.tests
  , Unit.HaddockExtract.tests
  , Unit.InternalError.tests
  , Unit.LookupPrepLocal.tests
  , Unit.RepoCache.tests
  , Unit.SourceExtract.tests
  , Unit.SourceParser.tests
  ])
