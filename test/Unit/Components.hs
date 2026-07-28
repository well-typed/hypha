{-# LANGUAGE OverloadedStrings #-}
module Unit.Components (tests) where

import Data.Foldable (for_)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified GHC.Driver.Session as GHCLang
import qualified GHC.LanguageExtensions as LangExt

import Hypha.Project.Components
  ( ComponentInfo (..), ComponentKind (..), parseLibComponents )
import Hypha.Source.Extensions (LanguageSettings (..), defaultLanguageSettings)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan
  , moduleOwner )
import Hypha.Types.ComponentName
  ( ComponentKey (..), componentKeyOf, parseComponentKey )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..))

tests :: TestTree
tests = testGroup "Unit.Components"
  [ testCase "parses main lib + two sublibs + two exes from fixture" $ do
      let root  = "test" </> "fixtures" </> "cabal"
          cabal = root </> "hypha.cabal"
      comps <- parseLibComponents cabal root
      let summary =
            sort [ ( renderKind (ciKind c)
                   , sort (ciHsSourceDirs c)
                   )
                 | c <- comps
                 ]
      summary @?=
        [ ( "exe:hypha-cli",     [root </> "app"] )
        , ( "exe:wrap",         [root </> "app/wrap"] )
        , ( "lib",              [root </> "src"] )
        , ( "sublib:bench",     [root </> "bench-src"] )
        , ( "sublib:internal",  [root </> "internal-src"] )
        ]
  , testCase "missing cabal file returns []" $ do
      res <- parseLibComponents "/does/not/exist.cabal" "/does/not"
      res @?= []

  , testCase "cabal other-modules, default-extensions and language are read" $ do
      let root = "test" </> "fixtures" </> "cabal"
      comps <- parseLibComponents (root </> "extensions.cabal") "/pkg"
      case comps of
        [c] -> do
          ciExposedModules    c @?= ["Fixture.Wrapper"]
          ciOtherModules      c @?= ["Fixture.Internal"]
          ciUnknownExtensions c @?= []
          let ls = ciLanguageSettings c
          lsLanguage   ls @?= Just GHCLang.GHC2021
          lsDefaultOn  ls @?= [LangExt.MagicHash]
          lsDefaultOff ls @?= [LangExt.ImplicitPrelude]
        _ -> fail ("expected exactly one component, got " <> show (length comps))

  , testCase "component keys round-trip through their rendering" $
      for_ [ (PackageName "containers", MainLib)
           , (PackageName "hypha",      SubLib "hypha-internal")
           , (PackageName "hypha",      Exe "hypha-mcp")
           ] $ \(pkg, kind) -> do
        let key = componentKeyOf pkg kind
        parseComponentKey (unComponentKey key) @?= Just (unPackageName pkg, kind)

  , testCase "a key with too many colons is rejected, not mis-parsed" $
      -- The encoding is @pkg@ / @pkg:sub@ / @pkg:exe:name@, so a further
      -- colon would make it ambiguous.  Cabal forbids one in a component
      -- name; assert we do not silently accept it either.
      parseComponentKey "hypha:a:b:c" @?= Nothing

  , testCase "a dependency's exposed module resolves to its component" $
      moduleOwner ownerPlan
        (PackageId (PackageName "facade") (Version "1.0"))
        (ModulePath "Dep.Internal")
        @?= Just (PackageId (PackageName "dep") (Version "2.0"), MainLib)

  , testCase "a module no dependency exposes has no owner" $
      moduleOwner ownerPlan
        (PackageId (PackageName "facade") (Version "1.0"))
        (ModulePath "Nowhere.At.All")
        @?= Nothing

  , testCase "a non-dependency exposing the module is not its owner" $
      -- stranger exposes Dep.Internal too, but lonely does not depend on
      -- it.  Only the asking unit's dependency list says which one it meant.
      moduleOwner ownerPlan
        (PackageId (PackageName "lonely") (Version "1.0"))
        (ModulePath "Dep.Internal")
        @?= Nothing
  ]
  where
    renderKind :: ComponentKind -> String
    renderKind MainLib     = "lib"
    renderKind (SubLib s)  = "sublib:" <> Text.unpack s
    renderKind (Exe    s)  = "exe:"    <> Text.unpack s

-- | facade depends on dep, which exposes Dep.Internal.  lonely depends on
-- nothing.  stranger exposes Dep.Internal but is nobody's dependency.
ownerPlan :: BuildPlan
ownerPlan = emptyBuildPlan
  { bpUnits = Map.fromList
      [ (PackageName "facade",   planUnit "facade"   "1.0" ["dep"] [])
      , (PackageName "dep",      planUnit "dep"      "2.0" []      ["Dep.Internal"])
      , (PackageName "lonely",   planUnit "lonely"   "1.0" []      [])
      , (PackageName "stranger", planUnit "stranger" "1.0" []      ["Dep.Internal"])
      ]
  }
  where
    planUnit n v deps exposed = PlannedUnit
      { puId            = PackageId (PackageName n) (Version v)
      , puDeps          = [ PackageId (PackageName d) (Version "2.0") | d <- deps ]
      , puIsLocal       = False
      , puOrigin        = OriginDistribution
      , puSrcDir        = Nothing
      , puDistDir       = Nothing
      , puLibComponents = case exposed of
          [] -> []
          _  -> [ ComponentInfo
                    { ciKind              = MainLib
                    , ciHsSourceDirs      = ["src"]
                    , ciExposedModules    = exposed
                    , ciOtherModules      = []
                    , ciLanguageSettings  = defaultLanguageSettings
                    , ciUnknownExtensions = []
                    } ]
      }
