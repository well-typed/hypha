{-# LANGUAGE OverloadedStrings #-}
module Unit.Components (tests) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)
import qualified Data.Text as Text
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified GHC.Driver.Session as GHCLang
import qualified GHC.LanguageExtensions as LangExt

import Distribution.Compiler (CompilerFlavor (GHC))
import Distribution.System
  ( Arch (X86_64, JavaScript), OS (Linux, Windows), Platform (..) )
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Flag (mkFlagName)
import Distribution.Types.VersionRange (anyVersion)

import Hypha.Project.BuildContext (BuildContext (..), hostBuildContext)
import Hypha.Project.Components
  ( ComponentInfo (..), ComponentKind (..), evalCondition, parseLibComponents )
import Hypha.Source.Extensions (LanguageSettings (..))

-- | A context that pins the platform, so a fixture's @os()@ and
-- @arch()@ branches resolve the same way on every developer's machine.
onPlatform :: Arch -> OS -> BuildContext
onPlatform arch os = hostBuildContext { bcPlatform = Platform arch os }

linux :: BuildContext
linux = onPlatform X86_64 Linux

tests :: TestTree
tests = testGroup "Unit.Components"
  [ testCase "parses main lib + two sublibs + two exes from fixture" $ do
      let root  = "test" </> "fixtures" </> "cabal"
          cabal = root </> "hypha.cabal"
      comps <- parseLibComponents cabal root linux
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
      res <- parseLibComponents "/does/not/exist.cabal" "/does/not" linux
      res @?= []

  , testCase "cabal other-modules, default-extensions and language are read" $ do
      let root = "test" </> "fixtures" </> "cabal"
      comps <- parseLibComponents (root </> "extensions.cabal") "/pkg" linux
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

  , testCase "conditional stanzas contribute the branches this platform builds" $ do
      -- base declares GHC.Event only in the else-branch of
      -- `if os(windows)`, so reading the unconditional node alone meant
      -- the indexer -- which trusts a non-empty stanza list -- never
      -- looked for it.  The branch that matches the platform is taken,
      -- and the one that does not is left out: a Windows-only module is
      -- on disk in the sdist and cannot preprocess off Windows, so
      -- unioning both reported parse failures for modules this platform
      -- never builds.
      let root = "test" </> "fixtures" </> "cabal"
      comps <- parseLibComponents (root </> "conditional.cabal") "/pkg" linux
      case comps of
        [c] -> do
          sort (ciExposedModules c) @?= ["Fixture.Always", "Fixture.Posix"]
          sort (ciOtherModules c)
            @?= ["Fixture.AlwaysHidden", "Fixture.Hidden.Posix"]
          -- `impl(ghc >= 9.10)` is not a platform fact, so both of its
          -- branches still contribute -- hence src-new and MagicHash.
          sort (ciHsSourceDirs c) @?= ["/pkg/src", "/pkg/src-new"]
          lsDefaultOn (ciLanguageSettings c) @?= [LangExt.MagicHash]
        _ -> fail ("expected exactly one component, got " <> show (length comps))

  , testCase "the same fixture on Windows takes the other branch" $ do
      -- The mirror image, so the filter is pinned as platform-driven
      -- rather than as "drop anything named Windows".
      let root = "test" </> "fixtures" </> "cabal"
      comps <- parseLibComponents (root </> "conditional.cabal") "/pkg"
                 (onPlatform X86_64 Windows)
      case comps of
        [c] -> do
          sort (ciExposedModules c) @?= ["Fixture.Always", "Fixture.Windows"]
          sort (ciOtherModules c)
            @?= ["Fixture.AlwaysHidden", "Fixture.Hidden.Windows"]
        _ -> fail ("expected exactly one component, got " <> show (length comps))

  , testCase "an elif arm is reached when the arm before it does not match" $ do
      -- os(windows) / elif arch(javascript) / else: the middle arm is
      -- only reachable if the false-branch of the first is descended.
      let root = "test" </> "fixtures" </> "cabal"
      comps <- parseLibComponents (root </> "conditional.cabal") "/pkg"
                 (onPlatform JavaScript Linux)
      case comps of
        [c] -> sort (ciOtherModules c)
                 @?= ["Fixture.AlwaysHidden", "Fixture.Hidden.Javascript"]
        _ -> fail ("expected exactly one component, got " <> show (length comps))

  , testCase "a decidable half settles a condition its other half cannot" $ do
      -- Kleene, not strict: `os(windows) && flag(x)` is decided off
      -- Windows even though the flag assignment is unknown, and
      -- `os(linux) || flag(x)` is decided on Linux.  Without this, one
      -- unknown flag beside an os() test would pull a Windows-only
      -- module back into the list.
      let onLinux = evalCondition (Platform X86_64 Linux)
          flagX   = Var (PackageFlag (mkFlagName "x"))
      onLinux (CAnd (Var (OS Windows)) flagX) @?= Just False
      onLinux (COr  (Var (OS Linux))   flagX) @?= Just True
      onLinux (CNot (Var (OS Windows)))       @?= Just True
      onLinux (Var (Arch X86_64))             @?= Just True
      -- Undecidable stays undecidable, which is what unions the branches.
      onLinux flagX                           @?= Nothing
      onLinux (Var (Impl GHC anyVersion))     @?= Nothing
      onLinux (CAnd (Var (OS Linux)) flagX)   @?= Nothing

  , testCase "a module named in two branches is listed once" $ do
      -- An undecidable condition still unions, and the union can name
      -- the same module twice; two rows for one file would be read and
      -- indexed twice.
      let root = "test" </> "fixtures" </> "cabal"
      comps <- parseLibComponents (root </> "conditional.cabal") "/pkg" linux
      case comps of
        [c] -> do
          let mods = ciExposedModules c ++ ciOtherModules c
          length mods @?= length (nubOrd mods)
        _ -> fail "expected exactly one component"

  ]
  where
    renderKind :: ComponentKind -> String
    renderKind MainLib     = "lib"
    renderKind (SubLib s)  = "sublib:" <> Text.unpack s
    renderKind (Exe    s)  = "exe:"    <> Text.unpack s
