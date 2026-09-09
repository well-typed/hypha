{-# LANGUAGE OverloadedStrings #-}
-- | Which modules a component has, resolved from its cabal stanza rather
-- than from a walk of its @hs-source-dirs@.
--
-- The fixture is a package whose two executables share one source
-- directory with a script cabal never builds — the shape that made
-- @hypha server@ offer a module link for an unbuilt neighbour and answer
-- it with "module … is not part of this component".
module Unit.ComponentModules (tests) where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import System.FilePath (takeFileName, (</>))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Project.BuildContext (BuildContext (..), hostBuildContext)
import Hypha.Project.Components
  ( ComponentInfo (..), ComponentKind (..), parseLibComponents )
import Hypha.Search.Index (ModuleSource (..))
import Hypha.Search.Indexer
  ( componentModuleNames, componentModules, languageSettingsFor )
import Hypha.Source.Interface (ModuleInterface (..), parseSources)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan
  , unpinnedUnitIdFor )
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), Version (..) )
import Hypha.Types.SymbolPath (ModulePath (..))

import Distribution.System (Arch (X86_64), OS (Linux), Platform (..))

-- | The fixture package root, and the platform its stanzas resolve for —
-- pinned so a conditional never reads differently per developer.
root :: FilePath
root = "test" </> "fixtures" </> "cabal"

linux :: BuildContext
linux = hostBuildContext { bcPlatform = Platform X86_64 Linux }

scriptsPkg :: PackageId
scriptsPkg = PackageId (PackageName "scripts") (Version "0.1.0")

-- | A one-unit plan whose components are the fixture's: the indexer and
-- the server both read a component's module list out of exactly this.
fixturePlan :: IO (BuildPlan, [ComponentInfo])
fixturePlan = do
  comps <- parseLibComponents (root </> "scripts.cabal") root linux
  let unit = PlannedUnit
        { puId            = scriptsPkg
        , puUnitId        = unpinnedUnitIdFor scriptsPkg
        , puDeps          = []
        , puIsLocal       = True
        , puOrigin        = OriginLocal root
        , puSrcDir        = Just root
        , puDistDir       = Nothing
        , puLibComponents = comps
        }
  pure ( emptyBuildPlan
           { bpUnits        = Map.fromList [(pkgName scriptsPkg, unit)]
           , bpBuildContext = linux
           }
       , comps
       )

-- | The source dirs the fixture's cabal gives one component.
dirsFor :: [ComponentInfo] -> ComponentKind -> [FilePath]
dirsFor comps kind =
  concat [ ciHsSourceDirs c | c <- comps, ciKind c == kind ]

sourcesFor :: ComponentKind -> IO [ModuleSource]
sourcesFor kind = do
  (plan, comps) <- fixturePlan
  componentModules plan scriptsPkg kind (dirsFor comps kind)

namesFor :: ComponentKind -> IO [ModulePath]
namesFor kind = do
  (plan, comps) <- fixturePlan
  componentModuleNames plan scriptsPkg kind (dirsFor comps kind)

tests :: TestTree
tests = testGroup "Unit.ComponentModules"
  [ testCase "an executable's main-is is one of its modules" $ do
      -- alpha-tool has other-modules, so its stanza list was non-empty and
      -- therefore trusted -- with the main module missing from it.  Every
      -- page and every symbol card for that module answered "not part of
      -- this component".
      srcs <- sourcesFor (Exe "alpha-tool")
      sort (map (unModulePath . msDeclaredName) srcs)
        @?= ["Main", "Shared.Helper"]
      sort (map msPath srcs)
        @?= [ root </> "scripts" </> "Shared" </> "Helper.hs"
            , root </> "scripts" </> "alpha-tool.hs"
            ]

  , testCase "an executable that is only a main-is has exactly that module" $ do
      -- beta-tool's stanza list was empty, which sent componentModules to
      -- its filesystem walk -- and the walk swept up every unbuilt script
      -- sharing the directory.
      srcs <- sourcesFor (Exe "beta-tool")
      map (unModulePath . msDeclaredName) srcs @?= ["Main"]
      map msPath srcs @?= [root </> "scripts" </> "beta-tool.hs"]

  , testCase "a script no stanza names is not a module of any component" $ do
      -- gamma-stray.hs sits beside both executables' main-is files and
      -- cabal builds it for neither.
      alpha <- sourcesFor (Exe "alpha-tool")
      beta  <- sourcesFor (Exe "beta-tool")
      lib   <- sourcesFor MainLib
      let names = map (takeFileName . msPath) (alpha <> beta <> lib)
      filter ("gamma-stray.hs" ==) names @?= []

  , testCase "a library still resolves its exposed modules" $ do
      srcs <- sourcesFor MainLib
      map (unModulePath . msDeclaredName) srcs @?= ["Scripts.Lib"]

  , testCase "the listed modules are the ones a page can resolve" $ do
      -- The package page used to enumerate a component by walking its
      -- source dirs while the module page resolved it from the stanza, so
      -- it offered links no module page could answer.  The two lists have
      -- to agree, and to agree on the /parsed/ name: a file with no
      -- `module ... where` header is an implicit Main, and a path-derived
      -- name for it matches nothing.
      (plan, comps) <- fixturePlan
      let check kind = do
            listed <- namesFor kind
            srcs   <- sourcesFor kind
            let langs = languageSettingsFor plan (pkgName scriptsPkg) kind
            parsed <- parseSources langs srcs
            sort listed @?= sort [ miName i | (_, Right i) <- parsed ]
      mapM_ check [Exe "alpha-tool", Exe "beta-tool", MainLib]
      -- The plan really does describe all three, so a typo in a kind
      -- cannot make the check above vacuous.
      length comps @?= 3
  ]
