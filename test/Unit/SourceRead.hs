{-# LANGUAGE OverloadedStrings #-}
-- | Reading a module off disk must not depend on the file being UTF-8,
-- and one unreadable package must not take the rest of the index with it.
--
-- Both cases come from the same field report: @c2hs-0.28.8@ ships a
-- Latin-1 source, the read threw, and every package after it in the
-- build order was never indexed while the server reported itself ready.
module Unit.SourceRead (tests) where

import           Control.Exception (throwIO)
import qualified Data.ByteString as BS
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Encoding (readSourceFile)
import Hypha.Package.Resolver (PackageResolver (..))
import Hypha.Search.Exports (emptyEnv)
import Hypha.Search.Fuzzy (Entity (..), IndexedRow (..))
import Hypha.Search.Index (ModuleSource (..))
import Hypha.Search.Indexer (buildAndCacheIndex, packageSources)
import Hypha.Search.PackageCache (openPackageCacheAt)
import Hypha.Project.BuildContext (hostBuildContext)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan
  , unpinnedUnitIdFor )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..))
import Util.Fixture (reexportDepDir, reexportDepId, resolverFor)

tests :: TestTree
tests = testGroup "Unit.SourceRead"
  [ testLatin1SourceReads
  , testLatin1PackageLoads
  , testOneBrokenUnitDoesNotAbortTheIndex
  ]

-- | @Lexers.hs@ from c2hs, in miniature: a comment with an @ä@ encoded as
-- the single byte 0xE4.
latin1Module :: BS.ByteString
latin1Module = BS.concat
  [ "module Lexers where\n-- Autor: J\xe4ger\nlexers :: Int\nlexers = 1\n" ]

testLatin1SourceReads :: TestTree
testLatin1SourceReads =
  testCase "readSourceFile decodes a Latin-1 file instead of throwing" $
    withSystemTempDirectory "hypha-latin1" $ \dir -> do
      let f = dir </> "Lexers.hs"
      BS.writeFile f latin1Module
      txt <- readSourceFile f
      -- The undecodable byte becomes U+FFFD and everything around it
      -- survives, which is how GHC reads the same file.
      assertBool "replacement character present" (Text.any (== '\xFFFD') txt)
      assertBool "declaration survives" ("lexers :: Int" `Text.isInfixOf` txt)

testLatin1PackageLoads :: TestTree
testLatin1PackageLoads =
  testCase "packageSources loads a package with a Latin-1 module" $
    withSystemTempDirectory "hypha-latin1-pkg" $ \dir -> do
      writeFile (dir </> "lex.cabal") $ unlines
        [ "cabal-version: 3.0", "name: lex", "version: 0.1", "build-type: Simple"
        , "", "library", "  exposed-modules: Lexers", "  build-depends: base"
        , "  default-language: Haskell2010" ]
      BS.writeFile (dir </> "Lexers.hs") latin1Module
      srcs <- packageSources dir hostBuildContext
      map msDeclaredName (concatMap snd srcs) @?= [ModulePath "Lexers"]

-- | A resolver that blows up on one package and answers the fixture for
-- the other.  An exception, not a 'Left': the 'Left' path is already
-- reported per unit; this is the path nothing reported before.
testOneBrokenUnitDoesNotAbortTheIndex :: TestTree
testOneBrokenUnitDoesNotAbortTheIndex =
  testCase "buildAndCacheIndex skips a unit that throws and indexes the rest" $
    withSystemTempDirectory "hypha-isolate" $ \dir -> do
      createDirectoryIfMissing True dir
      cache   <- openPackageCacheAt (dir </> "g.db") Nothing
      rowsRef <- IORef.newIORef []
      doneRef <- IORef.newIORef (0 :: Int)
      let broken   = PackageId (PackageName "broken") (Version "1.0")
          good     = resolverFor [(reexportDepId, reexportDepDir)]
          resolver = good
            { resolveSrc = \pid ->
                if pid == broken
                  then throwIO (userError "disk on fire")
                  else resolveSrc good pid }
          plan = emptyBuildPlan
            { bpUnits = Map.fromList
                [ (PackageName "broken", unit broken)
                , (pkgName reexportDepId, unit reexportDepId) ] }
          unit pid = PlannedUnit
            { puId = pid, puUnitId = unpinnedUnitIdFor pid, puDeps = []
            , puIsLocal = False, puOrigin = OriginHackage, puSrcDir = Nothing
            , puDistDir = Nothing, puLibComponents = [] }
      buildAndCacheIndex plan cache resolver Nothing emptyEnv
        [broken, reexportDepId] rowsRef doneRef
      rows <- IORef.readIORef rowsRef
      done <- IORef.readIORef doneRef
      done @?= 2
      assertBool "the healthy package was indexed"
        (any ((== EntityPackage (pkgName reexportDepId) (pkgVersion reexportDepId)) . irEntity) rows)
