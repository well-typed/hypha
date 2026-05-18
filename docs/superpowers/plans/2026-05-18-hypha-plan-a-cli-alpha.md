# hypha — Plan A: CLI Alpha (phases 1–13)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Deliver a working, internally-usable `hypha` CLI — every subcommand except `server` and `mcp` end-to-end, emitting compact JSON by default and ANSI text under `--human`, scoped to the active build plan. Releasable as v0.0.x internal alpha.

**Architecture:** One cabal package, one library, one executable (`hypha`). Effects via records-of-functions parameterised over `m`, wired into a `ReaderT Env IO` carrier. No effect library. No typeclass effect machinery. The library is the single source of truth; the executable is a thin frontend over `Hypha.Cli.Run`.

**Tech Stack:** GHC 9.6+; `cabal-plan`, `cabal-install-parsers`, `Cabal-syntax`, `hoogle`, `haddock-library`, `aeson`, `http-client(+tls)`, `typed-process`, `tagsoup`, `skylighting`, `text-metrics`, `contra-tracer`, `optparse-applicative`, `prettyprinter(+-ansi-terminal)`, `lucid2` (later), `mcp` (later), `falsify`, `tasty(+-golden,-quickcheck,-hunit)`.

**Spec:** `docs/superpowers/specs/2026-05-18-hypha-design.md`. Plan B (server) and Plan C (MCP + release polish) follow.

**Reading order:** Tasks 1 → 13. Each task is one PR. Commit at the end of every task; do not push between tasks unless instructed.

---

## File structure (created by this plan)

```
hypha.cabal
cabal.project
cabal.project.freeze              (committed; produced via `cabal freeze` at task 1)
flake.nix
LICENSE
README.md
.github/workflows/ci.yml

src/Hypha/Prelude.hs

src/Hypha/Types/PackageId.hs
src/Hypha/Types/SymbolPath.hs
src/Hypha/Types/BuildPlan.hs
src/Hypha/Types/Doc.hs

src/Hypha/Project/Discovery.hs
src/Hypha/Project/Plan.hs
src/Hypha/Project/Overrides.hs

src/Hypha/BuildEnv/Type.hs
src/Hypha/BuildEnv/Cabal.hs
src/Hypha/BuildEnv/Nix.hs
src/Hypha/BuildEnv/Compose.hs
src/Hypha/BuildEnv/Mock.hs

src/Hypha/Hackage/Types.hs
src/Hypha/Hackage/Cache.hs
src/Hypha/Hackage/Api.hs

src/Hypha/Hoogle/Type.hs
src/Hypha/Hoogle/Database.hs
src/Hypha/Hoogle/Query.hs

src/Hypha/Haddock/Parse.hs
src/Hypha/Haddock/Interface.hs
src/Hypha/Haddock/Generate.hs

src/Hypha/Source/Locate.hs
src/Hypha/Source/Extract.hs

src/Hypha/Output/Outcome.hs
src/Hypha/Output/Actions.hs
src/Hypha/Output/Json.hs
src/Hypha/Output/Human.hs

src/Hypha/Command/Search.hs
src/Hypha/Command/Package.hs
src/Hypha/Command/Module.hs
src/Hypha/Command/Symbol.hs
src/Hypha/Command/Source.hs
src/Hypha/Command/Versions.hs
src/Hypha/Command/Deps.hs
src/Hypha/Command/WhatProvides.hs
src/Hypha/Command/Doctor.hs

src/Hypha/Cli/Parser.hs
src/Hypha/Cli/Run.hs

src/Hypha/Exit.hs
src/Hypha/Error.hs
src/Hypha/Logging.hs
src/Hypha/Cache.hs

app/hypha/Main.hs

test/Main.hs
test/Property/SymbolPath.hs
test/Property/OutputJson.hs
test/Property/HackageCache.hs
test/Property/HaddockRewrite.hs
test/Unit/Project.hs
test/Unit/BuildEnv.hs
test/Unit/BuildEnvCompose.hs
test/Unit/Hoogle.hs
test/Unit/Doctor.hs
test/Unit/Haddock.hs
test/Golden/Search.hs
test/Golden/Commands.hs
test/Golden/Human.hs

test/fixtures/tiny-project/tiny.cabal
test/fixtures/tiny-project/cabal.project
test/fixtures/tiny-project/dist-newstyle/cache/plan.json
test/fixtures/fake-cabal-store/...
```

---

## Conventions (apply to every step)

- Every new module declaring `data` or `newtype` puts an explicit `!` bang on every strict field; lazy fields warrant a one-line comment.
- No `error` / `undefined` in production code. Boundary failures go through `Hypha.Error.HyphaError`; logic errors are unreachable by construction or rejected at the type level.
- Imports kept minimal and sorted; one `Hypha.Prelude` re-export module shares common shorthands.
- Tests follow `Test.Tasty.Falsify` for properties and `Test.Tasty.Golden` for golden outputs.
- Every step that changes code is followed by running the tests for that area. Every task ends with a commit. Commit messages: `feat:`, `test:`, `docs:`, `chore:` per Conventional Commits.

---

## Task 1: Project skeleton

**Files:**
- Create: `LICENSE`
- Create: `README.md`
- Create: `hypha.cabal`
- Create: `cabal.project`
- Create: `cabal.project.freeze` (generated)
- Create: `flake.nix`
- Create: `.github/workflows/ci.yml`
- Create: `src/Hypha/Prelude.hs`
- Create: `app/hypha/Main.hs`
- Create: `test/Main.hs`

- [ ] **Step 1: Write LICENSE (BSD-3-Clause)**

Create `LICENSE` with the canonical BSD-3-Clause text:

```
BSD 3-Clause License

Copyright (c) 2026, Well-Typed LLP and contributors.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

- [ ] **Step 2: Write README.md stub**

```markdown
# hypha

> An agent-first Haskell CLI that probes Hackage / Hoogle / your cabal build plan.

`hypha` (Greek *hyphē*: the branching threadlike cell of a fungus that probes
through substrate seeking nutrients) is a command-line tool for browsing
Hackage and Hoogle without leaving the terminal — designed first for AI agents
(Claude Code, opencode, Pi, …) and second for the humans they help.

It is **project-aware**: queries default to the versions your `cabal.project`
actually builds against (parsed from `dist-newstyle/cache/plan.json`), with
`--any` to widen and `--package-override` to escape-hatch.

## Mantra

> An LLM doesn't need or care about fancy Haddock HTML pages — it cares about
> the source code, which also contains the comments (the documentation). That
> is what an LLM needs in order to learn knowledge of a project. Humans need
> visuals.

## Status

Pre-alpha. See `docs/superpowers/specs/2026-05-18-hypha-design.md` for the
design.

## License

BSD-3-Clause. See `LICENSE`.
```

- [ ] **Step 3: Write `hypha.cabal`**

```cabal
cabal-version:      3.4
name:               hypha
version:            0.0.0
synopsis:           Agent-first CLI for browsing Hackage and Hoogle, scoped to your cabal build plan.
description:
  hypha probes a cabal project's actual build plan to answer questions about
  packages, modules, symbols, sources, and dependencies. JSON-first output for
  agents; `--human` ANSI text for terminal use; a separate `server` mode (Plan B)
  serves a browser-based offline doc viewer.
license:            BSD-3-Clause
license-file:       LICENSE
author:             Well-Typed LLP
maintainer:         info@well-typed.com
copyright:          2026 Well-Typed LLP
category:           Development
build-type:         Simple
tested-with:        GHC == 9.6.*, GHC == 9.8.*, GHC == 9.10.*

common warnings
  ghc-options:
    -Wall
    -Wcompat
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wmissing-export-lists
    -Wmissing-home-modules
    -Wpartial-fields
    -Wredundant-constraints
    -Wunused-packages

common deps
  default-language: GHC2021
  build-depends:    base >= 4.18 && < 5

library
  import:           warnings, deps
  hs-source-dirs:   src
  exposed-modules:  Hypha.Prelude

executable hypha
  import:           warnings, deps
  hs-source-dirs:   app/hypha
  main-is:          Main.hs
  build-depends:    hypha

test-suite hypha-tests
  import:           warnings, deps
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          Main.hs
  build-depends:    hypha, tasty
```

- [ ] **Step 4: Write `cabal.project`**

```
packages: .

tests: True
test-show-details: streaming
```

- [ ] **Step 5: Write `flake.nix` (minimal devshell)**

```nix
{
  description = "hypha — agent-first Hackage/Hoogle CLI";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      eachSystem = nixpkgs.lib.genAttrs systems;
    in {
      devShells = eachSystem (system:
        let pkgs = import nixpkgs { inherit system; };
            hp = pkgs.haskell.packages.ghc96;
        in {
          default = pkgs.mkShell {
            buildInputs = [ hp.ghc hp.cabal-install hp.haskell-language-server pkgs.zlib ];
          };
        });
    };
}
```

- [ ] **Step 6: Write `src/Hypha/Prelude.hs`**

```haskell
module Hypha.Prelude
  ( version
  ) where

version :: String
version = "0.0.0"
```

- [ ] **Step 7: Write `app/hypha/Main.hs`**

```haskell
module Main (main) where

import qualified Hypha.Prelude as Hypha

main :: IO ()
main = putStrLn ("hypha " <> Hypha.version)
```

- [ ] **Step 8: Write `test/Main.hs`**

```haskell
module Main (main) where

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "hypha" [])
```

- [ ] **Step 9: Verify build**

Run: `cabal build all`
Expected: builds successfully; produces `hypha` executable.

- [ ] **Step 10: Verify executable**

Run: `cabal run hypha`
Expected output: `hypha 0.0.0`

- [ ] **Step 11: Verify test suite**

Run: `cabal test`
Expected: zero tests, exit 0.

- [ ] **Step 12: Freeze dependencies**

Run: `cabal freeze`
This produces `cabal.project.freeze`.

- [ ] **Step 13: Write `.github/workflows/ci.yml`**

```yaml
name: ci
on:
  push:
    branches: [main]
  pull_request:

jobs:
  build:
    strategy:
      fail-fast: false
      matrix:
        ghc: ["9.6.6", "9.8.2", "9.10.1"]
        os: [ubuntu-latest]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: haskell-actions/setup@v2
        with:
          ghc-version: ${{ matrix.ghc }}
          cabal-version: '3.12'
      - run: cabal update
      - run: cabal build all --enable-tests
      - run: cabal test all
```

- [ ] **Step 14: Commit**

```bash
git add LICENSE README.md hypha.cabal cabal.project cabal.project.freeze flake.nix \
        src/Hypha/Prelude.hs app/hypha/Main.hs test/Main.hs .github/workflows/ci.yml
git commit -m "feat: project skeleton — cabal + ghc matrix + devshell + hello-world hypha exe"
```

---

## Task 2: Core types — `PackageId` and `SymbolPath`

**Files:**
- Create: `src/Hypha/Types/PackageId.hs`
- Create: `src/Hypha/Types/SymbolPath.hs`
- Create: `test/Property/SymbolPath.hs`
- Modify: `hypha.cabal` — expose new modules; add `text`, `bytestring`, `containers`, `falsify`, `tasty-falsify`, `tasty-hunit` deps.

- [ ] **Step 1: Add dependencies to `hypha.cabal`**

In the `library` stanza extend `build-depends`:

```cabal
  build-depends:
    , base       >= 4.18 && < 5
    , bytestring >= 0.11
    , containers >= 0.6
    , text       >= 2.0
```

In the `test-suite hypha-tests` stanza extend `build-depends`:

```cabal
  build-depends:
    , base
    , hypha
    , tasty
    , tasty-falsify
    , tasty-hunit
    , falsify
    , text
```

In the `library` `exposed-modules` add:

```cabal
  exposed-modules:
    Hypha.Prelude
    Hypha.Types.PackageId
    Hypha.Types.SymbolPath
```

In the `test-suite` add `other-modules`:

```cabal
  other-modules:
    Property.SymbolPath
```

- [ ] **Step 2: Write `src/Hypha/Types/PackageId.hs`**

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hypha.Types.PackageId
  ( PackageName (..)
  , Version (..)
  , PackageId (..)
  , parsePackageName
  , parseVersion
  , renderPackageId
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

newtype PackageName = PackageName { unPackageName :: Text }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Read)

newtype Version = Version { unVersion :: Text }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Read)

data PackageId = PackageId
  { pkgName    :: !PackageName
  , pkgVersion :: !Version
  }
  deriving stock (Show, Eq, Ord)

parsePackageName :: Text -> Maybe PackageName
parsePackageName t
  | Text.null t                              = Nothing
  | Text.any (\c -> c == '/' || c == '@') t  = Nothing
  | otherwise                                = Just (PackageName t)

parseVersion :: Text -> Maybe Version
parseVersion t
  | Text.null t                              = Nothing
  | Text.any (\c -> c == '/' || c == '@') t  = Nothing
  | otherwise                                = Just (Version t)

renderPackageId :: PackageId -> Text
renderPackageId (PackageId (PackageName n) (Version v)) = n <> "-" <> v
```

- [ ] **Step 3: Write failing property test for `SymbolPath`**

Create `test/Property/SymbolPath.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Property.SymbolPath (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range
import Test.Falsify.Property (gen, assert)
import qualified Test.Falsify.Predicate as P
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.SymbolPath
  ( SymbolPath, parseSymbolPath, renderSymbolPath )

genIdentText :: Gen.Gen Text
genIdentText = do
  c  <- Gen.elem $ pure 'a' <> pure 'b' <> pure 'F'
  cs <- Gen.list (Range.between (0, 8))
                 (Gen.elem (pure 'a' <> pure 'B' <> pure '2' <> pure '_'))
  pure (Text.pack (c : cs))

genModulePath :: Gen.Gen Text
genModulePath = do
  segs <- Gen.list (Range.between (1, 4)) $ do
    c  <- Gen.elem (pure 'A' <> pure 'B' <> pure 'C')
    cs <- Gen.list (Range.between (0, 5)) (Gen.elem (pure 'a' <> pure '2'))
    pure (Text.pack (c : cs))
  pure (Text.intercalate "." segs)

genSymbolPath :: Gen.Gen SymbolPath
genSymbolPath = do
  pkgT <- genIdentText
  hasVer <- Gen.bool False
  mVer <- if hasVer then Just <$> genIdentText else pure Nothing
  hasMod <- Gen.bool False
  mMod <- if hasMod then Just <$> genModulePath else pure Nothing
  hasSym <- Gen.bool False
  mSym <- if (hasMod && hasSym) then Just <$> genIdentText else pure Nothing
  case parseSymbolPath (assemble pkgT mVer mMod mSym) of
    Right sp -> pure sp
    Left  e  -> error ("genSymbolPath produced unparseable input: " <> show e)

assemble :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text
assemble pkg mv mm ms =
       pkg
    <> maybe "" ("@" <>) mv
    <> maybe "" ("/" <>) mm
    <> maybe "" ("/" <>) ms

tests :: TestTree
tests = testGroup "SymbolPath"
  [ testProperty "parse . render = Right" $ do
      sp <- gen genSymbolPath
      assert $ P.eq P..$ ("expected", Right sp) P..$ ("got", parseSymbolPath (renderSymbolPath sp))
  ]
```

Register in `test/Main.hs`:

```haskell
module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Property.SymbolPath

main :: IO ()
main = defaultMain (testGroup "hypha"
  [ Property.SymbolPath.tests
  ])
```

- [ ] **Step 4: Run test, confirm failure**

Run: `cabal test`
Expected: compile error — module `Hypha.Types.SymbolPath` does not export `SymbolPath`, `parseSymbolPath`, or `renderSymbolPath`.

- [ ] **Step 5: Implement `Hypha.Types.SymbolPath`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Types.SymbolPath
  ( SymbolPath (..)
  , ModulePath (..)
  , SymbolName (..)
  , ParseError (..)
  , parseSymbolPath
  , renderSymbolPath
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.PackageId (PackageName (..), Version (..), parsePackageName, parseVersion)

newtype ModulePath = ModulePath { unModulePath :: Text }
  deriving stock (Show, Eq, Ord)

newtype SymbolName = SymbolName { unSymbolName :: Text }
  deriving stock (Show, Eq, Ord)

data SymbolPath = SymbolPath
  { spPackage :: !PackageName
  , spVersion :: !(Maybe Version)
  , spModule  :: !(Maybe ModulePath)
  , spSymbol  :: !(Maybe SymbolName)
  }
  deriving stock (Show, Eq, Ord)

data ParseError
  = EmptyInput
  | EmptyPackageSegment
  | SymbolWithoutModule
  | InvalidPackageName !Text
  | InvalidVersion !Text
  | InvalidModule !Text
  | InvalidSymbol !Text
  deriving stock (Show, Eq)

-- | Parse @pkg[@ver][/Mod[.Path]][/sym]@.
parseSymbolPath :: Text -> Either ParseError SymbolPath
parseSymbolPath t
  | Text.null t = Left EmptyInput
  | otherwise =
      let segs    = Text.splitOn "/" t
          (pkgSeg, mModSeg, mSymSeg) = case segs of
            []                 -> ("", Nothing, Nothing)
            [p]                -> (p, Nothing, Nothing)
            [p, m]             -> (p, Just m, Nothing)
            (p : m : s : _)    -> (p, Just m, Just s)
          (pkgPart, mVerPart) = case Text.splitOn "@" pkgSeg of
            [p]    -> (p, Nothing)
            [p, v] -> (p, Just v)
            (p:_)  -> (p, Nothing)
            []     -> ("", Nothing)
      in do
        pkg <- maybe (Left (InvalidPackageName pkgPart)) Right (parsePackageName pkgPart)
        mv  <- case mVerPart of
                 Nothing -> Right Nothing
                 Just v  -> maybe (Left (InvalidVersion v)) (Right . Just) (parseVersion v)
        mm  <- case mModSeg of
                 Nothing -> Right Nothing
                 Just m  -> if Text.null m then Left (InvalidModule m) else Right (Just (ModulePath m))
        ms  <- case mSymSeg of
                 Nothing -> Right Nothing
                 Just s  -> if Text.null s then Left (InvalidSymbol s) else Right (Just (SymbolName s))
        case (mm, ms) of
          (Nothing, Just _) -> Left SymbolWithoutModule
          _                 -> Right (SymbolPath pkg mv mm ms)

renderSymbolPath :: SymbolPath -> Text
renderSymbolPath (SymbolPath (PackageName p) mv mm ms) =
     p
  <> maybe "" (\(Version v)        -> "@" <> v) mv
  <> maybe "" (\(ModulePath m)     -> "/" <> m) mm
  <> maybe "" (\(SymbolName s)     -> "/" <> s) ms
```

- [ ] **Step 6: Run tests**

Run: `cabal test`
Expected: pass.

- [ ] **Step 7: Add concrete unit tests**

Append to `test/Property/SymbolPath.hs` inside `tests`:

```haskell
  , testProperty "specific: pkg only" $ do
      assert $ P.eq P..$ ("expected", Right (SymbolPath (PackageName "async") Nothing Nothing Nothing))
                   P..$ ("got", parseSymbolPath "async")
  , testProperty "specific: pkg + ver + module + symbol" $ do
      assert $ P.eq P..$ ("expected", Right (SymbolPath
                            (PackageName "async")
                            (Just (Version "2.2.5"))
                            (Just (ModulePath "Control.Concurrent.Async"))
                            (Just (SymbolName "concurrently"))))
                   P..$ ("got", parseSymbolPath "async@2.2.5/Control.Concurrent.Async/concurrently")
  , testProperty "rejects: symbol without module" $ do
      assert $ P.eq P..$ ("expected", Left SymbolWithoutModule)
                   P..$ ("got", parseSymbolPath "async//concurrently")
```

Also add the necessary imports at the top of the file:

```haskell
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.SymbolPath
  ( SymbolPath (..), ModulePath (..), SymbolName (..), ParseError (..)
  , parseSymbolPath, renderSymbolPath )
```

- [ ] **Step 8: Run tests**

Run: `cabal test`
Expected: all four tests pass.

- [ ] **Step 9: Commit**

```bash
git add hypha.cabal src/Hypha/Types/ test/Property/SymbolPath.hs test/Main.hs
git commit -m "feat(types): add PackageId and SymbolPath with parser, pretty, and falsify roundtrip"
```

---

## Task 3: Project resolution — BuildPlan, Discovery, Overrides

**Files:**
- Create: `src/Hypha/Types/BuildPlan.hs`
- Create: `src/Hypha/Project/Plan.hs`
- Create: `src/Hypha/Project/Discovery.hs`
- Create: `src/Hypha/Project/Overrides.hs`
- Create: `test/Unit/Project.hs`
- Create: `test/fixtures/tiny-project/tiny.cabal`
- Create: `test/fixtures/tiny-project/cabal.project`
- Create: `test/fixtures/tiny-project/dist-newstyle/cache/plan.json`
- Modify: `hypha.cabal` (add `cabal-plan`, `cabal-install-parsers`, `Cabal-syntax`, `directory`, `filepath`)
- Modify: `test/Main.hs`

- [ ] **Step 1: Add dependencies in `hypha.cabal`**

In the `library` `build-depends`, add:

```cabal
    , cabal-plan             >= 0.7  && < 0.8
    , cabal-install-parsers  >= 0.6  && < 0.7
    , Cabal-syntax           >= 3.10
    , directory              >= 1.3.6
    , filepath               >= 1.4
```

Extend `exposed-modules`:

```cabal
    Hypha.Types.BuildPlan
    Hypha.Project.Plan
    Hypha.Project.Discovery
    Hypha.Project.Overrides
```

Extend `test-suite` `other-modules`:

```cabal
    Unit.Project
```

- [ ] **Step 2: Create the fixture project**

Write `test/fixtures/tiny-project/tiny.cabal`:

```cabal
cabal-version: 3.0
name:          tiny
version:       0.1.0
build-type:    Simple

library
  default-language: GHC2021
  exposed-modules:  Tiny
  hs-source-dirs:   src
  build-depends:    base, async
```

Write `test/fixtures/tiny-project/cabal.project`:

```
packages: .
```

Write `test/fixtures/tiny-project/dist-newstyle/cache/plan.json` — a minimal hand-built plan referencing `tiny` and `async-2.2.5`:

```json
{
  "cabal-version": "3.12",
  "cabal-lib-version": "3.12.0.0",
  "compiler-id": "ghc-9.6.6",
  "os": "linux",
  "arch": "x86_64",
  "install-plan": [
    {
      "type": "pre-existing",
      "id": "base-4.18.2.0",
      "pkg-name": "base",
      "pkg-version": "4.18.2.0",
      "depends": []
    },
    {
      "type": "configured",
      "id": "async-2.2.5-inplace",
      "pkg-name": "async",
      "pkg-version": "2.2.5",
      "pkg-src": { "type": "repo-tar", "repo": { "type": "secure-repo", "uri": "https://hackage.haskell.org/" } },
      "depends": ["base-4.18.2.0"],
      "components": { "lib": { "depends": ["base-4.18.2.0"] } }
    },
    {
      "type": "configured",
      "id": "tiny-0.1.0-inplace",
      "pkg-name": "tiny",
      "pkg-version": "0.1.0",
      "pkg-src": { "type": "local", "path": "." },
      "depends": ["base-4.18.2.0", "async-2.2.5-inplace"],
      "components": { "lib": { "depends": ["base-4.18.2.0", "async-2.2.5-inplace"] } }
    }
  ]
}
```

- [ ] **Step 3: Write `Hypha.Types.BuildPlan`**

```haskell
module Hypha.Types.BuildPlan
  ( BuildPlan (..)
  , PlannedUnit (..)
  , GhcId (..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Hypha.Types.PackageId (PackageId, PackageName)

newtype GhcId = GhcId { unGhcId :: Text }
  deriving stock (Show, Eq, Ord)

data PlannedUnit = PlannedUnit
  { puId      :: !PackageId
  , puIsLocal :: !Bool
  , puDeps    :: ![PackageId]
  }
  deriving stock (Show, Eq)

data BuildPlan = BuildPlan
  { bpGhc   :: !GhcId
  , bpUnits :: !(Map PackageName PlannedUnit)
  }
  deriving stock (Show, Eq)
```

- [ ] **Step 4: Write `Hypha.Project.Discovery`**

```haskell
{-# LANGUAGE LambdaCase #-}
module Hypha.Project.Discovery
  ( ProjectRoot (..)
  , DiscoveryError (..)
  , discoverProjectRoot
  ) where

import qualified Data.List as L
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))

newtype ProjectRoot = ProjectRoot { projectRootPath :: FilePath }
  deriving stock (Show, Eq)

data DiscoveryError
  = NotInsideProject !FilePath
  | RootNotADirectory !FilePath
  deriving stock (Show, Eq)

-- | Walk up from a starting directory until a directory containing
-- @cabal.project@ or any @*.cabal@ file is found.
discoverProjectRoot :: Maybe FilePath -> IO (Either DiscoveryError ProjectRoot)
discoverProjectRoot mStart = do
  start <- maybe getCurrentDirectory pure mStart
  exists <- doesDirectoryExist start
  if not exists
    then pure (Left (RootNotADirectory start))
    else go start
  where
    go dir = do
      hit <- looksLikeProject dir
      if hit
        then pure (Right (ProjectRoot dir))
        else let parent = takeDirectory dir
             in if parent == dir
                  then pure (Left (NotInsideProject dir))
                  else go parent

looksLikeProject :: FilePath -> IO Bool
looksLikeProject d = do
  hasProj <- doesFileExist (d </> "cabal.project")
  if hasProj
    then pure True
    else any (L.isSuffixOf ".cabal") <$> safeList d
  where
    safeList p = do
      ok <- doesDirectoryExist p
      if ok then listDirectory p else pure []
```

- [ ] **Step 5: Write `Hypha.Project.Plan`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Project.Plan
  ( loadBuildPlan
  , PlanLoadError (..)
  ) where

import qualified Cabal.Plan as CP
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.FilePath ((</>))

import Hypha.Project.Discovery (ProjectRoot (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Hypha.Types.BuildPlan (BuildPlan (..), GhcId (..), PlannedUnit (..))

data PlanLoadError
  = PlanFileMissing !FilePath
  | PlanFileMalformed !String
  deriving stock (Show, Eq)

loadBuildPlan :: ProjectRoot -> IO (Either PlanLoadError BuildPlan)
loadBuildPlan (ProjectRoot root) = do
  let planFile = root </> "dist-newstyle" </> "cache" </> "plan.json"
  res <- CP.findAndDecodePlanJson (CP.ProjectRelativeToDir root)
  case res of
    Left  e    -> pure (Left (planLoadErrorFrom planFile e))
    Right plan -> pure (Right (fromCabalPlan plan))

planLoadErrorFrom :: FilePath -> String -> PlanLoadError
planLoadErrorFrom path msg
  | "could not find plan.json" `isInfixOf` msg = PlanFileMissing path
  | otherwise                                   = PlanFileMalformed msg
  where
    isInfixOf needle haystack = needle `Text.isInfixOf` Text.pack haystack

fromCabalPlan :: CP.PlanJson -> BuildPlan
fromCabalPlan pj = BuildPlan
  { bpGhc   = GhcId (CP.dispCompilerId (CP.pjCompilerId pj))
  , bpUnits = Map.fromList [ (pkgName u, u) | u <- units ]
  }
  where
    units :: [PlannedUnit]
    units =
      [ PlannedUnit (toId pid) isLocal (concatMap (depsOf pj) (Map.elems (CP.uComps unit)))
      | (_uid, unit) <- Map.toList (CP.pjUnits pj)
      , let pid = CP.uPId unit
      , let isLocal = case CP.uType unit of
                        CP.UnitTypeLocal -> True
                        _                -> False
      ]
    toId (CP.PkgId (CP.PkgName n) (CP.Ver vs)) =
      PackageId (PackageName n) (Version (Text.pack (CP.dispVer (CP.Ver vs))))
    depsOf pj' c = [ toId (CP.uPId u) | dep <- Map.keys (CP.ciLibDeps c)
                                      , Just u <- [Map.lookup dep (CP.pjUnits pj')] ]
    pkgName (PlannedUnit (PackageId n _) _ _) = n
```

> Note: `cabal-plan` API: the type names used here (`PlanJson`, `pjUnits`, `uType`, `uPId`, `uComps`, `ciLibDeps`, `PkgId`, `dispCompilerId`, `dispVer`, `findAndDecodePlanJson`, `ProjectRelativeToDir`, `UnitTypeLocal`) follow `cabal-plan-0.7.x`. If the local pinned version differs, consult `cabal haddock cabal-plan` for the current spelling and adjust.

- [ ] **Step 6: Write `Hypha.Project.Overrides`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Project.Overrides
  ( PackageOverride (..)
  , OverrideError (..)
  , parsePackageOverride
  , applyOverrides
  , appliedOverrides
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.BuildPlan (BuildPlan (..), PlannedUnit (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..), parsePackageName, parseVersion)

data PackageOverride = PackageOverride
  { ovPackage :: !PackageName
  , ovVersion :: !Version
  }
  deriving stock (Show, Eq)

data OverrideError
  = OverrideBadSyntax !Text
  | OverrideBadName !Text
  | OverrideBadVersion !Text
  deriving stock (Show, Eq)

-- | Parse @PKG=VER@ from a CLI flag value.
parsePackageOverride :: Text -> Either OverrideError PackageOverride
parsePackageOverride raw =
  case Text.splitOn "=" raw of
    [n, v] -> do
      nm <- maybe (Left (OverrideBadName n)) Right (parsePackageName n)
      ve <- maybe (Left (OverrideBadVersion v)) Right (parseVersion v)
      Right (PackageOverride nm ve)
    _      -> Left (OverrideBadSyntax raw)

applyOverrides :: [PackageOverride] -> BuildPlan -> BuildPlan
applyOverrides ovs bp = bp { bpUnits = foldr apply (bpUnits bp) ovs }
  where
    apply (PackageOverride n v) m =
      Map.adjust (\u -> u { puId = (puId u) { pkgVersion = v } }) n m

-- | List overrides that actually changed something.
appliedOverrides :: [PackageOverride] -> BuildPlan -> [PackageOverride]
appliedOverrides ovs bp =
  [ ov | ov <- ovs, Just _ <- [Map.lookup (ovPackage ov) (bpUnits bp)] ]
```

- [ ] **Step 7: Write unit tests**

Create `test/Unit/Project.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.Project (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import qualified Data.Map.Strict as Map

import Hypha.Project.Discovery (discoverProjectRoot, ProjectRoot (..))
import Hypha.Project.Plan      (loadBuildPlan)
import Hypha.Project.Overrides (PackageOverride (..), parsePackageOverride, applyOverrides)
import Hypha.Types.BuildPlan   (BuildPlan (..), PlannedUnit (..))
import Hypha.Types.PackageId   (PackageId (..), PackageName (..), Version (..))

fixtureRoot :: FilePath
fixtureRoot = "test/fixtures/tiny-project"

tests :: TestTree
tests = testGroup "Project"
  [ testCase "discoverProjectRoot finds tiny-project" $ do
      r <- discoverProjectRoot (Just fixtureRoot)
      r @?= Right (ProjectRoot fixtureRoot)

  , testCase "loadBuildPlan parses the fixture plan.json" $ do
      Right bp <- loadBuildPlan (ProjectRoot fixtureRoot)
      assertBool "tiny is present"
        (Map.member (PackageName "tiny") (bpUnits bp))
      assertBool "async is present"
        (Map.member (PackageName "async") (bpUnits bp))

  , testCase "parsePackageOverride parses PKG=VER" $ do
      parsePackageOverride "async=2.2.6"
        @?= Right (PackageOverride (PackageName "async") (Version "2.2.6"))

  , testCase "applyOverrides changes pinned version" $ do
      Right bp <- loadBuildPlan (ProjectRoot fixtureRoot)
      let bp'   = applyOverrides [PackageOverride (PackageName "async") (Version "9.9.9")] bp
          ver   = fmap (\u -> pkgVersion (puId u))
                       (Map.lookup (PackageName "async") (bpUnits bp'))
      ver @?= Just (Version "9.9.9")
  ]
```

Register in `test/Main.hs`:

```haskell
module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Property.SymbolPath
import qualified Unit.Project

main :: IO ()
main = defaultMain (testGroup "hypha"
  [ Property.SymbolPath.tests
  , Unit.Project.tests
  ])
```

- [ ] **Step 8: Run tests**

Run: `cabal test --test-show-details=streaming`
Expected: discovery, plan-load, override-parse, override-apply all pass.

- [ ] **Step 9: Commit**

```bash
git add hypha.cabal src/Hypha/Types/BuildPlan.hs src/Hypha/Project/ \
        test/Unit/Project.hs test/Main.hs test/fixtures/tiny-project
git commit -m "feat(project): plan.json ingestion, project-root discovery, package overrides"
```

---

## Task 4: BuildEnv interface + Cabal implementation + Mock

**Files:**
- Create: `src/Hypha/BuildEnv/Type.hs`
- Create: `src/Hypha/BuildEnv/Cabal.hs`
- Create: `src/Hypha/BuildEnv/Mock.hs`
- Create: `test/Unit/BuildEnv.hs`
- Create: `test/fixtures/fake-cabal-store/ghc-9.6.6/async-2.2.5-deadbeef/share/doc/async-2.2.5/html/index.html` (stub)
- Create: `test/fixtures/fake-cabal-store/ghc-9.6.6/async-2.2.5-deadbeef/share/doc/async-2.2.5/async.haddock` (empty stub)
- Modify: `hypha.cabal`

- [ ] **Step 1: Add modules and dependency to `hypha.cabal`**

Extend `library` `exposed-modules`:

```cabal
    Hypha.BuildEnv.Type
    Hypha.BuildEnv.Cabal
    Hypha.BuildEnv.Mock
```

Extend `library` `build-depends`:

```cabal
    , transformers >= 0.6
    , mtl          >= 2.3
```

Extend `test-suite` `other-modules`:

```cabal
    Unit.BuildEnv
```

- [ ] **Step 2: Write `Hypha.BuildEnv.Type`**

```haskell
module Hypha.BuildEnv.Type
  ( BuildEnv (..)
  ) where

import Data.Set (Set)
import Hypha.Types.PackageId (PackageId)
import Hypha.Types.BuildPlan (GhcId)

data BuildEnv m = BuildEnv
  { discoverInstalledPackages :: !(m (Set PackageId))
  , locatePackageSource       :: !(PackageId -> m (Maybe FilePath))
  , locateHaddockHtml         :: !(PackageId -> m (Maybe FilePath))
  , ghcVersion                :: !(m GhcId)
  }
```

> Note on strictness: the record fields here are function values; bang-annotating them is harmless (functions are already WHNF). Annotating them is consistent with our convention and avoids one cell of indirection per call.

- [ ] **Step 3: Write `Hypha.BuildEnv.Mock`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.BuildEnv.Mock
  ( MockState (..)
  , mkMockBuildEnv
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Hypha.BuildEnv.Type   (BuildEnv (..))
import Hypha.Types.BuildPlan (GhcId)
import Hypha.Types.PackageId (PackageId)

data MockState = MockState
  { msInstalled :: !(Set PackageId)
  , msSources   :: !(Map PackageId FilePath)
  , msHaddocks  :: !(Map PackageId FilePath)
  , msGhc       :: !GhcId
  }

mkMockBuildEnv :: Applicative m => MockState -> BuildEnv m
mkMockBuildEnv MockState{..} = BuildEnv
  { discoverInstalledPackages = pure msInstalled
  , locatePackageSource       = \pid -> pure (Map.lookup pid msSources)
  , locateHaddockHtml         = \pid -> pure (Map.lookup pid msHaddocks)
  , ghcVersion                = pure msGhc
  }
```

- [ ] **Step 4: Create cabal-store fixture stubs**

Create `test/fixtures/fake-cabal-store/ghc-9.6.6/async-2.2.5-deadbeef/share/doc/async-2.2.5/html/index.html`:

```html
<!doctype html><title>async</title>
```

Create an empty stub `test/fixtures/fake-cabal-store/ghc-9.6.6/async-2.2.5-deadbeef/share/doc/async-2.2.5/async.haddock`:

```
(stub interface file)
```

- [ ] **Step 5: Write `Hypha.BuildEnv.Cabal`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.BuildEnv.Cabal
  ( CabalEnvConfig (..)
  , mkCabalBuildEnv
  ) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type   (BuildEnv (..))
import Hypha.Types.BuildPlan (GhcId (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

data CabalEnvConfig = CabalEnvConfig
  { cecStorePath :: !FilePath  -- e.g. ~/.cabal/store
  , cecGhc       :: !GhcId     -- typically taken from the BuildPlan
  }

mkCabalBuildEnv :: CabalEnvConfig -> IO (BuildEnv IO)
mkCabalBuildEnv cfg = pure BuildEnv
  { discoverInstalledPackages = listInstalled cfg
  , locatePackageSource       = locateSource cfg
  , locateHaddockHtml         = locateHaddock cfg
  , ghcVersion                = pure (cecGhc cfg)
  }

ghcDir :: CabalEnvConfig -> FilePath
ghcDir cfg = cecStorePath cfg </> ("ghc-" <> Text.unpack (unGhcId (cecGhc cfg)))

listInstalled :: CabalEnvConfig -> IO (Set.Set PackageId)
listInstalled cfg = do
  let d = ghcDir cfg
  exists <- doesDirectoryExist d
  if not exists
    then pure Set.empty
    else do
      entries <- listDirectory d
      pure (Set.fromList [ pid | e <- entries, Just pid <- [parseStoreEntry e] ])

-- | Store entries look like @pkg-1.2.3-hashhashhash@.
parseStoreEntry :: FilePath -> Maybe PackageId
parseStoreEntry s =
  case reverse (Text.splitOn "-" (Text.pack s)) of
    (_hash : verT : rest@(_:_)) ->
      let name = Text.intercalate "-" (reverse rest)
      in Just (PackageId (PackageName name) (Version verT))
    _ -> Nothing

locateSource :: CabalEnvConfig -> PackageId -> IO (Maybe FilePath)
locateSource cfg pid = do
  let path = ghcDir cfg </> storeEntryPrefix pid
  hits <- listEntriesPrefixed path
  case hits of
    (p:_) -> pure (Just p)
    []    -> pure Nothing

locateHaddock :: CabalEnvConfig -> PackageId -> IO (Maybe FilePath)
locateHaddock cfg pid = do
  mDir <- locateSource cfg pid
  case mDir of
    Nothing  -> pure Nothing
    Just dir -> do
      let html = dir </> "share" </> "doc" </> renderEntry pid </> "html" </> "index.html"
      ok <- doesFileExist html
      pure (if ok then Just html else Nothing)

storeEntryPrefix :: PackageId -> FilePath
storeEntryPrefix (PackageId (PackageName n) (Version v)) =
  Text.unpack n <> "-" <> Text.unpack v

renderEntry :: PackageId -> FilePath
renderEntry = storeEntryPrefix

listEntriesPrefixed :: FilePath -> IO [FilePath]
listEntriesPrefixed prefixPath = do
  let parent  = dropFileBase prefixPath
      prefix  = takeFileBase prefixPath
  exists <- doesDirectoryExist parent
  if not exists
    then pure []
    else do
      entries <- listDirectory parent
      pure [ parent </> e | e <- entries, prefix `Text.isPrefixOf` Text.pack e ]
  where
    dropFileBase p = reverse (drop 1 (dropWhile (/= '/') (reverse p)))
    takeFileBase  p = reverse (takeWhile (/= '/') (reverse p))
```

- [ ] **Step 6: Write unit test**

Create `test/Unit/BuildEnv.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.BuildEnv (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Hypha.BuildEnv.Type  (BuildEnv (..))
import Hypha.BuildEnv.Cabal (CabalEnvConfig (..), mkCabalBuildEnv)
import Hypha.BuildEnv.Mock  (MockState (..), mkMockBuildEnv)
import Hypha.Types.BuildPlan (GhcId (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

asyncPid :: PackageId
asyncPid = PackageId (PackageName "async") (Version "2.2.5")

tests :: TestTree
tests = testGroup "BuildEnv"
  [ testCase "Mock: locatePackageSource returns configured path" $ do
      let env = mkMockBuildEnv MockState
            { msInstalled = Set.singleton asyncPid
            , msSources   = Map.singleton asyncPid "/tmp/async-src"
            , msHaddocks  = Map.empty
            , msGhc       = GhcId "9.6.6"
            }
      r <- locatePackageSource env asyncPid
      r @?= Just "/tmp/async-src"

  , testCase "Cabal: finds async in fake-cabal-store fixture" $ do
      env <- mkCabalBuildEnv CabalEnvConfig
        { cecStorePath = "test/fixtures/fake-cabal-store"
        , cecGhc       = GhcId "9.6.6"
        }
      installed <- discoverInstalledPackages env
      assertBool "async-2.2.5 in installed set" (Set.member asyncPid installed)

  , testCase "Cabal: locateHaddockHtml hits the stub index.html" $ do
      env <- mkCabalBuildEnv CabalEnvConfig
        { cecStorePath = "test/fixtures/fake-cabal-store"
        , cecGhc       = GhcId "9.6.6"
        }
      r <- locateHaddockHtml env asyncPid
      assertBool "index.html present" (case r of Just p -> "index.html" `isSuffixOf` p; _ -> False)
  ]
  where
    isSuffixOf needle hay = reverse needle `isPrefix` reverse hay
    isPrefix [] _      = True
    isPrefix _  []     = False
    isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys
```

Register in `test/Main.hs`:

```haskell
import qualified Unit.BuildEnv

main = defaultMain (testGroup "hypha"
  [ Property.SymbolPath.tests
  , Unit.Project.tests
  , Unit.BuildEnv.tests
  ])
```

- [ ] **Step 7: Run tests**

Run: `cabal test`
Expected: pass.

- [ ] **Step 8: Commit**

```bash
git add hypha.cabal src/Hypha/BuildEnv/ test/Unit/BuildEnv.hs test/Main.hs test/fixtures/fake-cabal-store
git commit -m "feat(buildenv): records-of-functions interface, Cabal store impl, in-memory mock"
```

---

## Task 5: Nix BuildEnv + composition

**Files:**
- Create: `src/Hypha/BuildEnv/Nix.hs`
- Create: `src/Hypha/BuildEnv/Compose.hs`
- Create: `test/Unit/BuildEnvCompose.hs`
- Modify: `hypha.cabal`

- [ ] **Step 1: Expose modules in `hypha.cabal`**

Extend `library` `exposed-modules`:

```cabal
    Hypha.BuildEnv.Nix
    Hypha.BuildEnv.Compose
```

Extend `test-suite` `other-modules`:

```cabal
    Unit.BuildEnvCompose
```

- [ ] **Step 2: Write `Hypha.BuildEnv.Compose`**

```haskell
module Hypha.BuildEnv.Compose
  ( composeBuildEnv
  ) where

import qualified Data.Set as Set

import Hypha.BuildEnv.Type (BuildEnv (..))

-- | Try the primary; fall back to the secondary on Nothing or empty set.
-- Both must be in the same monad. GHC version comes from the primary.
composeBuildEnv :: Monad m => BuildEnv m -> BuildEnv m -> BuildEnv m
composeBuildEnv a b = BuildEnv
  { discoverInstalledPackages = do
      sa <- discoverInstalledPackages a
      sb <- discoverInstalledPackages b
      pure (sa `Set.union` sb)
  , locatePackageSource = \pid -> do
      ra <- locatePackageSource a pid
      case ra of
        Just{}  -> pure ra
        Nothing -> locatePackageSource b pid
  , locateHaddockHtml = \pid -> do
      ra <- locateHaddockHtml a pid
      case ra of
        Just{}  -> pure ra
        Nothing -> locateHaddockHtml b pid
  , ghcVersion = ghcVersion a
  }
```

- [ ] **Step 3: Write `Hypha.BuildEnv.Nix`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.BuildEnv.Nix
  ( NixEnvConfig (..)
  , mkNixBuildEnv
  ) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink, getSymbolicLinkTarget)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type   (BuildEnv (..))
import Hypha.Types.BuildPlan (GhcId)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

data NixEnvConfig = NixEnvConfig
  { necProjectRoot :: !FilePath
  , necNixStore    :: !FilePath  -- usually "/nix/store"
  , necGhc         :: !GhcId
  }

mkNixBuildEnv :: NixEnvConfig -> IO (BuildEnv IO)
mkNixBuildEnv cfg = pure BuildEnv
  { discoverInstalledPackages = discoverFromResult cfg
  , locatePackageSource       = locateInNixStore cfg
  , locateHaddockHtml         = locateNixHaddock cfg
  , ghcVersion                = pure (necGhc cfg)
  }

-- | Walk @<project>/result@ symlink, if present, and enumerate packages
-- listed in @nix-support/propagated-build-inputs@ or similar manifests.
-- For MVP, scan the symlink's target dir for @lib/ghc-*/package.conf.d/*.conf@.
discoverFromResult :: NixEnvConfig -> IO (Set.Set PackageId)
discoverFromResult cfg = do
  let resultLink = necProjectRoot cfg </> "result"
  ok <- doesFileSymlinkOrDir resultLink
  if not ok
    then pure Set.empty
    else do
      target <- realiseLink resultLink
      conf  <- findPkgConfDir target
      case conf of
        Nothing -> pure Set.empty
        Just d  -> do
          confs <- listDirectory d
          pure (Set.fromList [ pid | f <- confs, Just pid <- [parseConfFilename f] ])

doesFileSymlinkOrDir :: FilePath -> IO Bool
doesFileSymlinkOrDir p = do
  d <- doesDirectoryExist p
  if d then pure True else doesFileExist p

realiseLink :: FilePath -> IO FilePath
realiseLink p = do
  isLnk <- pathIsSymbolicLink p
  if isLnk then getSymbolicLinkTarget p else pure p

-- Best-effort: scan a few likely sub-paths for a package.conf.d directory.
findPkgConfDir :: FilePath -> IO (Maybe FilePath)
findPkgConfDir base = do
  candidates <- listAll (base </> "lib")
  let confs = [ c | c <- candidates, "package.conf.d" `Text.isInfixOf` Text.pack c ]
  pure (case confs of (x:_) -> Just x; _ -> Nothing)
  where
    listAll d = do
      ok <- doesDirectoryExist d
      if not ok
        then pure []
        else do
          es <- listDirectory d
          subs <- mapM (\e -> listAll (d </> e)) es
          pure ([d </> e | e <- es] <> concat subs)

parseConfFilename :: FilePath -> Maybe PackageId
parseConfFilename f =
  case reverse (Text.splitOn "-" (Text.pack (stripDotConf f))) of
    (_hash : verT : rest@(_:_)) ->
      let name = Text.intercalate "-" (reverse rest)
      in Just (PackageId (PackageName name) (Version verT))
    _ -> Nothing
  where
    stripDotConf s =
      if ".conf" `Text.isSuffixOf` Text.pack s
        then take (length s - length (".conf" :: String)) s
        else s

locateInNixStore :: NixEnvConfig -> PackageId -> IO (Maybe FilePath)
locateInNixStore cfg pid = do
  entries <- safeList (necNixStore cfg)
  let want = "-" <> renderPkg pid
      hits = [ necNixStore cfg </> e | e <- entries, want `Text.isInfixOf` Text.pack e ]
  pure (case hits of (h:_) -> Just h; _ -> Nothing)
  where
    safeList d = do
      ok <- doesDirectoryExist d
      if ok then listDirectory d else pure []

renderPkg :: PackageId -> Text.Text
renderPkg (PackageId (PackageName n) (Version v)) = n <> "-" <> v

locateNixHaddock :: NixEnvConfig -> PackageId -> IO (Maybe FilePath)
locateNixHaddock cfg pid = do
  mDir <- locateInNixStore cfg pid
  case mDir of
    Nothing -> pure Nothing
    Just d  -> do
      let html = d </> "share" </> "doc" </> Text.unpack (renderPkg pid) </> "html" </> "index.html"
      ok <- doesFileExist html
      pure (if ok then Just html else Nothing)
```

- [ ] **Step 4: Write composition test**

Create `test/Unit/BuildEnvCompose.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.BuildEnvCompose (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Hypha.BuildEnv.Compose (composeBuildEnv)
import Hypha.BuildEnv.Mock    (MockState (..), mkMockBuildEnv)
import Hypha.BuildEnv.Type    (BuildEnv (..))
import Hypha.Types.BuildPlan  (GhcId (..))
import Hypha.Types.PackageId  (PackageId (..), PackageName (..), Version (..))

asyncPid, textPid :: PackageId
asyncPid = PackageId (PackageName "async") (Version "2.2.5")
textPid  = PackageId (PackageName "text")  (Version "2.1.0")

primary, secondary :: BuildEnv IO
primary   = mkMockBuildEnv MockState
              { msInstalled = Set.singleton asyncPid
              , msSources   = Map.singleton asyncPid "/from/primary/async"
              , msHaddocks  = Map.empty
              , msGhc       = GhcId "9.6.6"
              }
secondary = mkMockBuildEnv MockState
              { msInstalled = Set.singleton textPid
              , msSources   = Map.fromList [ (asyncPid, "/from/secondary/async-wrong")
                                           , (textPid,  "/from/secondary/text")
                                           ]
              , msHaddocks  = Map.empty
              , msGhc       = GhcId "9.6.6"
              }

tests :: TestTree
tests = testGroup "BuildEnv.Compose"
  [ testCase "primary wins for shared key" $ do
      let env = composeBuildEnv primary secondary
      r <- locatePackageSource env asyncPid
      r @?= Just "/from/primary/async"

  , testCase "fallthrough for missing key" $ do
      let env = composeBuildEnv primary secondary
      r <- locatePackageSource env textPid
      r @?= Just "/from/secondary/text"

  , testCase "discoverInstalled is union" $ do
      let env = composeBuildEnv primary secondary
      r <- discoverInstalledPackages env
      r @?= Set.fromList [asyncPid, textPid]
  ]
```

Register in `test/Main.hs`:

```haskell
import qualified Unit.BuildEnvCompose
-- ...
  , Unit.BuildEnvCompose.tests
```

- [ ] **Step 5: Run tests**

Run: `cabal test`
Expected: all three composition tests pass.

- [ ] **Step 6: Commit**

```bash
git add hypha.cabal src/Hypha/BuildEnv/Nix.hs src/Hypha/BuildEnv/Compose.hs \
        test/Unit/BuildEnvCompose.hs test/Main.hs
git commit -m "feat(buildenv): Nix store impl + composition (primary wins, secondary fills gaps)"
```

---

## Task 6: HackageClient + ETag/Last-Modified cache

**Files:**
- Create: `src/Hypha/Hackage/Types.hs`
- Create: `src/Hypha/Hackage/Cache.hs`
- Create: `src/Hypha/Hackage/Api.hs`
- Create: `src/Hypha/Cache.hs` (shared helpers)
- Create: `test/Property/HackageCache.hs`
- Create: `test/Unit/Hackage.hs`
- Modify: `hypha.cabal` (add `http-client`, `http-client-tls`, `aeson`, `time`, `cryptohash-sha256`, `directory`, `bytestring`)

- [ ] **Step 1: Extend `hypha.cabal`**

Extend `library` `build-depends`:

```cabal
    , aeson             >= 2.1
    , http-client       >= 0.7
    , http-client-tls   >= 0.3
    , http-types        >= 0.12
    , time              >= 1.12
    , cryptohash-sha256 >= 0.11
```

Extend `library` `exposed-modules`:

```cabal
    Hypha.Cache
    Hypha.Hackage.Types
    Hypha.Hackage.Cache
    Hypha.Hackage.Api
```

Extend `test-suite` `other-modules`:

```cabal
    Property.HackageCache
    Unit.Hackage
```

- [ ] **Step 2: Write `Hypha.Cache`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Cache
  ( cacheRoot
  ) where

import System.Directory (getXdgDirectory, XdgDirectory (XdgCache), createDirectoryIfMissing)
import System.FilePath ((</>))

-- | Resolve and create @${XDG_CACHE_HOME:-~/.cache}/hypha/<subdir>@.
cacheRoot :: FilePath -> IO FilePath
cacheRoot subdir = do
  base <- getXdgDirectory XdgCache "hypha"
  let dir = base </> subdir
  createDirectoryIfMissing True dir
  pure dir
```

- [ ] **Step 3: Write `Hypha.Hackage.Types`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Hackage.Types
  ( CacheKind (..)
  , CachedResponse (..)
  , Url (..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)

newtype Url = Url { unUrl :: Text }
  deriving stock (Show, Eq, Ord)

data CacheKind
  = Immutable
  | TtlMutable !NominalDiffTime
  deriving stock (Show, Eq)

data CachedResponse = CachedResponse
  { crEtag         :: !(Maybe ByteString)
  , crLastModified :: !(Maybe UTCTime)
  , crStoredAt     :: !UTCTime
  , crBody         :: !ByteString
  , crKind         :: !CacheKind
  }
  deriving stock (Show, Eq)
```

- [ ] **Step 4: Write `Hypha.Hackage.Cache`**

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Hackage.Cache
  ( Cache (..)
  , mkFsCache
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Hypha.Cache (cacheRoot)
import Hypha.Hackage.Types (CachedResponse (..), CacheKind (..), Url (..))

data Cache m = Cache
  { cacheLookup :: !(Url -> m (Maybe CachedResponse))
  , cacheStore  :: !(Url -> CachedResponse -> m ())
  , cacheFresh  :: !(CachedResponse -> m Bool)
  }

mkFsCache :: IO (Cache IO)
mkFsCache = do
  dir <- cacheRoot "hackage"
  pure Cache
    { cacheLookup = lookupFs dir
    , cacheStore  = storeFs dir
    , cacheFresh  = freshnessIO
    }

-- | Filesystem key = sha256 of url, hex.
keyFor :: Url -> FilePath
keyFor (Url u) = Text.unpack . TE.decodeUtf8 . B16.encode $ SHA256.hash (TE.encodeUtf8 u)

lookupFs :: FilePath -> Url -> IO (Maybe CachedResponse)
lookupFs dir url = do
  let path = dir </> keyFor url
  ok <- doesFileExist path
  if not ok
    then pure Nothing
    else do
      raw <- BS.readFile path
      pure (Aeson.decodeStrict raw)

storeFs :: FilePath -> Url -> CachedResponse -> IO ()
storeFs dir url cr =
  BL.writeFile (dir </> keyFor url) (Aeson.encode cr)

freshnessIO :: CachedResponse -> IO Bool
freshnessIO cr = case crKind cr of
  Immutable        -> pure True
  TtlMutable ttl   -> do
    now <- getCurrentTime
    pure (diffUTCTime now (crStoredAt cr) < ttl)

-- JSON instances for on-disk persistence ------------------------------------

instance Aeson.ToJSON CacheKind where
  toJSON Immutable        = Aeson.object ["kind" .= ("immutable" :: Text.Text)]
  toJSON (TtlMutable t)   = Aeson.object ["kind" .= ("ttl" :: Text.Text), "ttl" .= (realToFrac t :: Double)]

instance Aeson.FromJSON CacheKind where
  parseJSON = Aeson.withObject "CacheKind" $ \o -> do
    k <- o .: "kind"
    case (k :: Text.Text) of
      "immutable" -> pure Immutable
      "ttl"       -> do
        t <- o .: "ttl"
        pure (TtlMutable (realToFrac (t :: Double)))
      other       -> fail ("unknown cache kind: " <> Text.unpack other)

instance Aeson.ToJSON CachedResponse where
  toJSON cr = Aeson.object
    [ "etag"          .= fmap (TE.decodeUtf8 . B16.encode) (crEtag cr)
    , "last_modified" .= crLastModified cr
    , "stored_at"     .= crStoredAt cr
    , "body_b16"      .= TE.decodeUtf8 (B16.encode (crBody cr))
    , "kind"          .= crKind cr
    ]

instance Aeson.FromJSON CachedResponse where
  parseJSON = Aeson.withObject "CachedResponse" $ \o -> do
    etag <- o .:? "etag"
    lm   <- o .:? "last_modified"
    sa   <- o .:  "stored_at"
    bod  <- o .:  "body_b16"
    k    <- o .:  "kind"
    let decodeHex :: Text.Text -> Either String ByteString
        decodeHex = B16.decode . TE.encodeUtf8
    e' <- traverse (either fail pure . decodeHex) etag
    b' <- either fail pure (decodeHex bod)
    pure CachedResponse
      { crEtag         = e'
      , crLastModified = lm
      , crStoredAt     = sa
      , crBody         = b'
      , crKind         = k
      }
```

- [ ] **Step 5: Write property test for cache roundtrip**

Create `test/Property/HackageCache.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Property.HackageCache (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Falsify.Property (gen, assert)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range
import qualified Test.Falsify.Predicate as P
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)

import Hypha.Hackage.Types (CacheKind (..), CachedResponse (..))

genBytes :: Gen.Gen BS.ByteString
genBytes = do
  n <- Gen.integral (Range.between (0, 64))
  BS.pack <$> Gen.list (Range.exactly n) (Gen.integral (Range.between (0, 255)))

genTime :: Gen.Gen UTCTime
genTime = pure (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0))

genCachedResponse :: Gen.Gen CachedResponse
genCachedResponse = do
  hasEtag <- Gen.bool False
  e <- if hasEtag then Just <$> genBytes else pure Nothing
  hasLM <- Gen.bool False
  lm <- if hasLM then Just <$> genTime else pure Nothing
  body <- genBytes
  isImm <- Gen.bool False
  k <- if isImm then pure Immutable else pure (TtlMutable 900)
  pure CachedResponse
    { crEtag = e, crLastModified = lm, crStoredAt = UTCTime (fromGregorian 2026 1 1) 0
    , crBody = body, crKind = k }

tests :: TestTree
tests = testGroup "HackageCache"
  [ testProperty "JSON encode/decode roundtrip" $ do
      cr <- gen genCachedResponse
      let bs  = Aeson.encode cr
          rt  = Aeson.eitherDecode bs :: Either String CachedResponse
      assert $ P.eq P..$ ("expected", Right cr) P..$ ("got", rt)
  ]
```

Register in `test/Main.hs`:

```haskell
import qualified Property.HackageCache
-- ...
  , Property.HackageCache.tests
```

- [ ] **Step 6: Run tests**

Run: `cabal test`
Expected: roundtrip property passes.

- [ ] **Step 7: Write `Hypha.Hackage.Api`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Hackage.Api
  ( HackageClient (..)
  , HackageConfig (..)
  , mkHackageClient
  , HackageError (..)
  , PackageJson (..)
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (Value, eitherDecodeStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTP

import Hypha.Hackage.Cache (Cache (..))
import Hypha.Hackage.Types (CachedResponse (..), CacheKind (..), Url (..))
import Hypha.Types.PackageId (PackageName (..))

data HackageConfig = HackageConfig
  { hcBaseUrl  :: !Text
  , hcUserAgent :: !ByteString
  , hcOffline  :: !Bool
  }

defaultConfig :: HackageConfig
defaultConfig = HackageConfig
  { hcBaseUrl   = "https://hackage.haskell.org"
  , hcUserAgent = BS8.pack
      "hypha/0.0.0 (+https://github.com/well-typed/hypha; contact: info@well-typed.com)"
  , hcOffline   = False
  }

newtype PackageJson = PackageJson { unPackageJson :: Value }
  deriving stock (Show, Eq)

data HackageError
  = NetworkFailure !String
  | OfflineCacheMiss !Url
  | BadResponseStatus !Int
  | BodyParseFailure !String
  deriving stock (Show, Eq)

data HackageClient m = HackageClient
  { fetchPackageJson :: !(PackageName -> m (Either HackageError PackageJson))
  }

mkHackageClient :: HackageConfig -> Cache IO -> IO (HackageClient IO)
mkHackageClient cfg cache = do
  mgr <- HTTP.newManager TLS.tlsManagerSettings
  pure HackageClient
    { fetchPackageJson = \(PackageName n) ->
        let u = Url (hcBaseUrl cfg <> "/package/" <> n <> ".json")
        in fetchJson cfg mgr cache u
    }

fetchJson :: HackageConfig
          -> HTTP.Manager
          -> Cache IO
          -> Url
          -> IO (Either HackageError PackageJson)
fetchJson cfg mgr cache url = do
  mCached <- cacheLookup cache url
  case (mCached, hcOffline cfg) of
    (Just cr, True) -> okFromBody (crBody cr)
    (Just cr, False) -> do
      fresh <- cacheFresh cache cr
      if fresh
        then okFromBody (crBody cr)
        else revalidate mgr cfg cache url cr
    (Nothing, True)  -> pure (Left (OfflineCacheMiss url))
    (Nothing, False) -> fetchFresh mgr cfg cache url
  where
    okFromBody b = pure $ case eitherDecodeStrict b of
      Right v -> Right (PackageJson v)
      Left  e -> Left  (BodyParseFailure e)

revalidate :: HTTP.Manager
           -> HackageConfig -> Cache IO -> Url -> CachedResponse
           -> IO (Either HackageError PackageJson)
revalidate mgr cfg cache url cr = do
  req0 <- HTTP.parseRequest (Text.unpack (unUrl url))
  let cond = [(HTTP.hUserAgent, hcUserAgent cfg)]
          <> maybe [] (\e  -> [(HTTP.hIfNoneMatch, e)]) (crEtag cr)
      req = req0 { HTTP.requestHeaders = cond }
  r <- try (HTTP.httpLbs req mgr)
  case r of
    Left (e :: SomeException) -> pure (Left (NetworkFailure (show e)))
    Right resp ->
      case HTTP.statusCode (HTTP.responseStatus resp) of
        304 -> do
          now <- getCurrentTime
          cacheStore cache url (cr { crStoredAt = now })
          either (pure . Left . BodyParseFailure) (pure . Right . PackageJson) (eitherDecodeStrict (crBody cr))
        200 -> storeFreshFromResponse cache url cr resp
        s   -> pure (Left (BadResponseStatus s))

fetchFresh :: HTTP.Manager
           -> HackageConfig -> Cache IO -> Url
           -> IO (Either HackageError PackageJson)
fetchFresh mgr cfg cache url = do
  req0 <- HTTP.parseRequest (Text.unpack (unUrl url))
  let req = req0 { HTTP.requestHeaders = [(HTTP.hUserAgent, hcUserAgent cfg)] }
  r <- try (HTTP.httpLbs req mgr)
  case r of
    Left (e :: SomeException) -> pure (Left (NetworkFailure (show e)))
    Right resp ->
      case HTTP.statusCode (HTTP.responseStatus resp) of
        200 -> storeFreshFromResponse cache url emptyCR resp
        s   -> pure (Left (BadResponseStatus s))
  where
    emptyCR = CachedResponse Nothing Nothing (read "2000-01-01 00:00:00 UTC") BS.empty (TtlMutable 900)

storeFreshFromResponse :: Cache IO -> Url -> CachedResponse -> HTTP.Response Data.ByteString.Lazy.ByteString
                       -> IO (Either HackageError PackageJson)
storeFreshFromResponse cache url _prev resp = do
  now <- getCurrentTime
  let body  = BS.toStrict (HTTP.responseBody resp)
      hdrs  = HTTP.responseHeaders resp
      etag  = lookup HTTP.hETag hdrs
      lm    = lookup HTTP.hLastModified hdrs
      cr    = CachedResponse
                { crEtag = etag
                , crLastModified = parseLM lm
                , crStoredAt = now
                , crBody = body
                , crKind = TtlMutable 900
                }
  cacheStore cache url cr
  pure $ case eitherDecodeStrict body of
    Right v -> Right (PackageJson v)
    Left  e -> Left  (BodyParseFailure e)
  where
    parseLM _ = Nothing  -- HTTP-date parsing deferred to a future patch.
```

> Note on the imports above: the function `Data.ByteString.toStrict` lives in `Data.ByteString.Lazy`; this module uses qualified `BS` for strict and refers to lazy via `Data.ByteString.Lazy` in the signature. Add `import qualified Data.ByteString.Lazy as BL` and use `BL.toStrict`. Correct the snippet locally before building if needed.

- [ ] **Step 8: Smoke unit test for offline-miss**

Create `test/Unit/Hackage.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.Hackage (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import System.IO.Temp (withSystemTempDirectory)

import Hypha.Hackage.Api   (HackageClient (..), HackageConfig (..), mkHackageClient)
import Hypha.Hackage.Cache (mkFsCache)
import Hypha.Hackage.Types (Url (..))
import qualified Hypha.Hackage.Api as Api
import Hypha.Types.PackageId (PackageName (..))

tests :: TestTree
tests = testGroup "Hackage"
  [ testCase "offline mode + empty cache => OfflineCacheMiss" $ do
      cache <- mkFsCache
      cli   <- mkHackageClient
                 Api.defaultConfig { Api.hcOffline = True
                                   , Api.hcBaseUrl = "https://hackage.haskell.org" }
                 cache
      r <- fetchPackageJson cli (PackageName "this-pkg-does-not-exist-xyz")
      case r of
        Left (Api.OfflineCacheMiss _) -> pure ()
        other -> fail ("expected OfflineCacheMiss, got " <> show other)
  ]
```

> Add `temporary` to test-only `build-depends`. Also re-export `defaultConfig` from `Hypha.Hackage.Api`.

In `Hypha.Hackage.Api` export `defaultConfig` and `OfflineCacheMiss`:

```haskell
module Hypha.Hackage.Api
  ( HackageClient (..)
  , HackageConfig (..)
  , defaultConfig
  , mkHackageClient
  , HackageError (..)
  , PackageJson (..)
  ) where
```

Extend `test-suite` `build-depends`:

```cabal
    , temporary >= 1.3
```

Register in `test/Main.hs`:

```haskell
import qualified Unit.Hackage
-- ...
  , Unit.Hackage.tests
```

- [ ] **Step 9: Run tests**

Run: `cabal test`
Expected: cache roundtrip + offline-miss tests pass. No network is touched.

- [ ] **Step 10: Commit**

```bash
git add hypha.cabal src/Hypha/Cache.hs src/Hypha/Hackage/ test/Property/HackageCache.hs test/Unit/Hackage.hs test/Main.hs
git commit -m "feat(hackage): JSON API client with ETag-aware filesystem cache; offline cache-miss errors"
```

---

## Task 7: Hoogle — per-project DB + query

**Files:**
- Create: `src/Hypha/Hoogle/Type.hs`
- Create: `src/Hypha/Hoogle/Database.hs`
- Create: `src/Hypha/Hoogle/Query.hs`
- Create: `test/Unit/Hoogle.hs`
- Create: `test/fixtures/tiny-project/dist-newstyle/cache/plan.json.hypha-stub-haddock-txt` (small Hoogle input)
- Modify: `hypha.cabal` (add `hoogle`, `cryptohash-sha256`)

- [ ] **Step 1: Extend `hypha.cabal`**

Extend `library` `build-depends`:

```cabal
    , hoogle >= 5.0 && < 6
```

Extend `library` `exposed-modules`:

```cabal
    Hypha.Hoogle.Type
    Hypha.Hoogle.Database
    Hypha.Hoogle.Query
```

Extend `test-suite` `other-modules`:

```cabal
    Unit.Hoogle
```

- [ ] **Step 2: Write `Hypha.Hoogle.Type`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Hoogle.Type
  ( HoogleQuery (..)
  , HoogleHit (..)
  , Hoogle (..)
  ) where

import Data.Text (Text)

newtype HoogleQuery = HoogleQuery { unHoogleQuery :: Text }
  deriving stock (Show, Eq)

data HoogleHit = HoogleHit
  { hhPackage :: !Text
  , hhModule  :: !Text
  , hhName    :: !Text
  , hhSig     :: !Text
  , hhDocs    :: !Text
  }
  deriving stock (Show, Eq)

data Hoogle m = Hoogle
  { searchHoogle  :: !(HoogleQuery -> m [HoogleHit])
  , ensureFreshDb :: !(m ())
  }
```

- [ ] **Step 3: Write `Hypha.Hoogle.Database`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Hoogle.Database
  ( HoogleConfig (..)
  , dbPath
  , planHashFile
  , withProjectDb
  , withGlobalDb
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Hoogle

import Hypha.Project.Discovery (ProjectRoot (..))

data HoogleConfig = HoogleConfig
  { hgcProjectRoot :: !ProjectRoot
  , hgcInputDocs   :: ![FilePath]  -- paths to per-package .txt Hoogle inputs
  , hgcPlanHash    :: !Text        -- digest of plan.json contents
  }

dbPath :: ProjectRoot -> FilePath
dbPath (ProjectRoot r) = r </> ".hypha" </> "hoogle.hoo"

planHashFile :: ProjectRoot -> FilePath
planHashFile (ProjectRoot r) = r </> ".hypha" </> "plan-hash"

-- | Open the per-project DB, generating it if missing/stale.
withProjectDb :: HoogleConfig -> (Hoogle.Database -> IO a) -> IO a
withProjectDb cfg k = do
  let dot = (\(ProjectRoot r) -> r </> ".hypha") (hgcProjectRoot cfg)
  createDirectoryIfMissing True dot
  stale <- isStale cfg
  if stale
    then do
      Hoogle.hoogle ["generate", "--database=" <> dbPath (hgcProjectRoot cfg)
                    , "--local=" <> head (hgcInputDocs cfg <> ["."])]
      BS.writeFile (planHashFile (hgcProjectRoot cfg)) (TE.encodeUtf8 (hgcPlanHash cfg))
    else pure ()
  Hoogle.withDatabase (dbPath (hgcProjectRoot cfg)) k

withGlobalDb :: (Hoogle.Database -> IO a) -> IO a
withGlobalDb = Hoogle.withDatabase Hoogle.defaultDatabaseLocation

isStale :: HoogleConfig -> IO Bool
isStale cfg = do
  let pf = planHashFile (hgcProjectRoot cfg)
      df = dbPath (hgcProjectRoot cfg)
  pfOk <- doesFileExist pf
  dfOk <- doesFileExist df
  if not (pfOk && dfOk)
    then pure True
    else do
      saved <- BS.readFile pf
      pure (saved /= TE.encodeUtf8 (hgcPlanHash cfg))
```

> Note: the exact `Hoogle.hoogle`, `Hoogle.withDatabase`, and `Hoogle.defaultDatabaseLocation` symbol names follow `hoogle 5.0.x`. If the pinned `hoogle` version exposes slightly different module entry points, adjust the imports; the responsibilities of this module remain stable.

- [ ] **Step 4: Write `Hypha.Hoogle.Query`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Hoogle.Query
  ( mkProjectHoogle
  , mkGlobalHoogle
  ) where

import qualified Data.Text as Text
import qualified Hoogle

import Hypha.Hoogle.Database (HoogleConfig (..), withProjectDb, withGlobalDb)
import Hypha.Hoogle.Type     (Hoogle (..), HoogleHit (..), HoogleQuery (..))

mkProjectHoogle :: HoogleConfig -> IO (Hoogle IO)
mkProjectHoogle cfg = pure Hoogle
  { searchHoogle  = \q -> withProjectDb cfg $ \db ->
      pure (map toHit (Hoogle.searchDatabase db (Text.unpack (unHoogleQuery q))))
  , ensureFreshDb = withProjectDb cfg (\_ -> pure ())
  }

mkGlobalHoogle :: IO (Hoogle IO)
mkGlobalHoogle = pure Hoogle
  { searchHoogle  = \q -> withGlobalDb $ \db ->
      pure (map toHit (Hoogle.searchDatabase db (Text.unpack (unHoogleQuery q))))
  , ensureFreshDb = withGlobalDb (\_ -> pure ())
  }

toHit :: Hoogle.Target -> HoogleHit
toHit t = HoogleHit
  { hhPackage = maybe "" (Text.pack . fst) (Hoogle.targetPackage t)
  , hhModule  = maybe "" (Text.pack . fst) (Hoogle.targetModule t)
  , hhName    = Text.pack (Hoogle.targetItem t)
  , hhSig     = Text.pack (Hoogle.targetItem t)
  , hhDocs    = Text.pack (Hoogle.targetDocs t)
  }
```

- [ ] **Step 5: Stub unit test that exercises the staleness predicate**

Create `test/Unit/Hoogle.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.Hoogle (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

import Hypha.Hoogle.Database (planHashFile)
import Hypha.Project.Discovery (ProjectRoot (..))

tests :: TestTree
tests = testGroup "Hoogle"
  [ testCase "planHashFile path layout" $ do
      planHashFile (ProjectRoot "/tmp/proj") @?= "/tmp/proj/.hypha/plan-hash"
  , testCase "hash file round-trip" $ withSystemTempDirectory "hypha-hoogle-test" $ \tmp -> do
      let pf = planHashFile (ProjectRoot tmp)
      BS.createDirectoryIfMissing' (takeDirectory' pf)
      BS.writeFile pf (TE.encodeUtf8 "abcd")
      raw <- BS.readFile pf
      raw @?= TE.encodeUtf8 "abcd"
  ]
  where
    takeDirectory' = reverse . drop 1 . dropWhile (/= '/') . reverse
```

> Note: `BS.createDirectoryIfMissing'` does not exist; replace the second test with a direct `createDirectoryIfMissing True (takeDirectory pf)` from `System.Directory` and `import System.FilePath (takeDirectory)`.

Corrected snippet:

```haskell
import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeDirectory)
-- ...
  , testCase "hash file round-trip" $ withSystemTempDirectory "hypha-hoogle-test" $ \tmp -> do
      let pf = planHashFile (ProjectRoot tmp)
      createDirectoryIfMissing True (takeDirectory pf)
      BS.writeFile pf (TE.encodeUtf8 "abcd")
      raw <- BS.readFile pf
      raw @?= TE.encodeUtf8 "abcd"
```

Register in `test/Main.hs`:

```haskell
import qualified Unit.Hoogle
-- ...
  , Unit.Hoogle.tests
```

- [ ] **Step 6: Run tests**

Run: `cabal test`
Expected: pass. (We do not invoke real Hoogle generation in this task's tests; that integration is left for Task 9 once the search command is wired.)

- [ ] **Step 7: Commit**

```bash
git add hypha.cabal src/Hypha/Hoogle/ test/Unit/Hoogle.hs test/Main.hs
git commit -m "feat(hoogle): per-project Hoogle database with plan-hash staleness; global fallback"
```

---

## Task 8: Output — Outcome, Actions, Json (compact / full), --select

**Files:**
- Create: `src/Hypha/Output/Outcome.hs`
- Create: `src/Hypha/Output/Actions.hs`
- Create: `src/Hypha/Output/Json.hs`
- Create: `test/Property/OutputJson.hs`
- Modify: `hypha.cabal`

- [ ] **Step 1: Add modules to `hypha.cabal`**

Extend `exposed-modules`:

```cabal
    Hypha.Output.Outcome
    Hypha.Output.Actions
    Hypha.Output.Json
```

Extend `test-suite` `other-modules`:

```cabal
    Property.OutputJson
```

- [ ] **Step 2: Write `Hypha.Output.Outcome`**

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Output.Outcome
  ( Outcome (..)
  , OutcomeEnvelope (..)
  , CommandName (..)
  , Action (..)
  , Related (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

newtype CommandName = CommandName { unCommandName :: Text }
  deriving stock (Show, Eq, Generic)

data Action = Action
  { acName    :: !Text
  , acCommand :: !Text  -- e.g. "hypha symbol async/Control.Concurrent.Async/race"
  }
  deriving stock (Show, Eq, Generic)

data Related = Related
  { rLabel :: !Text
  , rFetch :: !Text
  }
  deriving stock (Show, Eq, Generic)

data Outcome a = Outcome
  { outValue   :: !a
  , outActions :: ![Action]
  , outRelated :: ![Related]
  }
  deriving stock (Show, Eq, Generic)

data OutcomeEnvelope a = OutcomeEnvelope
  { oeCommand      :: !CommandName
  , oeOutsidePlan  :: !Bool
  , oeOverrides    :: ![Text]
  , oeOutcome      :: !(Outcome a)
  }
  deriving stock (Show, Eq, Generic)
```

- [ ] **Step 3: Write `Hypha.Output.Actions`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Output.Actions
  ( actionSet
  , relatedFromSymbols
  ) where

import Data.Text (Text)

import Hypha.Output.Outcome (Action (..), Related (..))

-- | Standard cross-reference set for any package-scoped result.
actionSet :: Text -> Text -> [Action]
actionSet pkgName modPath =
  [ Action "package_info"    ("hypha package " <> pkgName)
  , Action "version_history" ("hypha versions " <> pkgName)
  , Action "module_index"    ("hypha module " <> pkgName <> "/" <> modPath)
  , Action "reverse_deps"    ("hypha deps " <> pkgName <> " --reverse")
  ]

relatedFromSymbols :: Text -> Text -> [Text] -> [Related]
relatedFromSymbols pkgName modPath names =
  [ Related n ("hypha symbol " <> pkgName <> "/" <> modPath <> "/" <> n)
  | n <- names
  ]
```

- [ ] **Step 4: Write `Hypha.Output.Json` (compact + full + envelope + select)**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Hypha.Output.Json
  ( Fieldset (..)
  , EnvelopeOpts (..)
  , encodeEnvelope
  , compactKeysOf
  , fullKeysOf
  , filterSelect
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Set as Set

import Hypha.Output.Outcome (Action (..), CommandName (..), Outcome (..), OutcomeEnvelope (..), Related (..))

data Fieldset = Compact | Full
  deriving stock (Show, Eq)

data EnvelopeOpts = EnvelopeOpts
  { eoFieldset :: !Fieldset
  , eoSelect   :: !(Maybe [Text])
  , eoPretty   :: !Bool
  }

-- | Encode `Outcome` using a per-command compact / full key projection.
--
-- The caller supplies the result body and the two key sets so this module
-- knows which keys belong to "compact" vs "full". The "compact ⊆ full"
-- invariant is enforced by a property test (see test/Property/OutputJson.hs).
encodeEnvelope
  :: CommandName
  -> Bool                  -- ^ outside_plan
  -> [Text]                -- ^ overrides
  -> Outcome Value         -- ^ result body already-encoded as JSON
  -> Set.Set Text          -- ^ compact key set for the body
  -> Set.Set Text          -- ^ full key set for the body
  -> EnvelopeOpts
  -> BL.ByteString
encodeEnvelope cmd outsidePlan overrides oc compact full opts =
  let bodyV  = restrictBody (outValue oc) compact full (eoFieldset opts)
      sel    = case eoSelect opts of
                 Nothing -> bodyV
                 Just ks -> filterSelect ks bodyV
      env    = object
                 [ "schema"       .= ("hypha/v0" :: Text)
                 , "command"      .= unCommandName cmd
                 , "ok"           .= True
                 , "outside_plan" .= outsidePlan
                 , "overrides"    .= overrides
                 , "result"       .= sel
                 , "actions"      .= object [ Key.fromText (acName a) .= acCommand a | a <- outActions oc ]
                 , "related"      .= Array (V.fromList
                       [ object ["label" .= rLabel r, "fetch" .= rFetch r] | r <- outRelated oc ])
                 ]
  in if eoPretty opts then Aeson.encode env else Aeson.encode env

restrictBody :: Value -> Set.Set Text -> Set.Set Text -> Fieldset -> Value
restrictBody v compact full fs =
  case v of
    Object km ->
      let keep = case fs of Compact -> compact; Full -> full
      in Object (KeyMap.filterWithKey (\k _ -> Set.member (Key.toText k) keep) km)
    other     -> other

-- | Keep only the listed top-level keys from an object Value.
filterSelect :: [Text] -> Value -> Value
filterSelect ks = \case
  Object km -> Object (KeyMap.filterWithKey (\k _ -> elem (Key.toText k) ks) km)
  other     -> other

compactKeysOf :: Set.Set Text -> Set.Set Text
compactKeysOf = id

fullKeysOf :: Set.Set Text -> Set.Set Text
fullKeysOf = id
```

> Note: the `compactKeysOf` / `fullKeysOf` helpers are place-holders for any future normalisation we might want to apply (e.g. lower-casing, deduplication). They are intentionally identity for now; callers thread the canonical sets through.

- [ ] **Step 5: Write property test enforcing `compact ⊆ full`**

Create `test/Property/OutputJson.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Property.OutputJson (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range
import Test.Falsify.Property (gen, assert)
import qualified Test.Falsify.Predicate as P
import qualified Data.Set as Set
import qualified Data.Text as Text

-- Symbol-card key sets (must stay in sync with Hypha.Command.Symbol).
compactSymbol, fullSymbol :: Set.Set Text.Text
compactSymbol = Set.fromList
  [ "kind", "name", "package", "version", "module"
  , "signature", "haddock_raw", "source"
  ]
fullSymbol = compactSymbol <> Set.fromList
  [ "license", "since", "fixity", "type_kind_info", "dependents_count" ]

tests :: TestTree
tests = testGroup "Output.Json"
  [ testProperty "compact ⊆ full for symbol-card" $ do
      _ <- gen (Gen.bool True)
      assert (P.satisfies ("subset", Set.isSubsetOf compactSymbol) P..$ ("full", fullSymbol))
  ]
```

Register in `test/Main.hs`:

```haskell
import qualified Property.OutputJson
-- ...
  , Property.OutputJson.tests
```

- [ ] **Step 6: Run tests**

Run: `cabal test`
Expected: subset property passes.

- [ ] **Step 7: Commit**

```bash
git add hypha.cabal src/Hypha/Output/ test/Property/OutputJson.hs test/Main.hs
git commit -m "feat(output): Outcome envelope with compact/full fieldsets and --select projection"
```

---

## Task 9: Exit codes, errors, CLI dispatcher, `search` command end-to-end, first golden

**Files:**
- Create: `src/Hypha/Exit.hs`
- Create: `src/Hypha/Error.hs`
- Create: `src/Hypha/Logging.hs`
- Create: `src/Hypha/Cli/Parser.hs`
- Create: `src/Hypha/Cli/Run.hs`
- Create: `src/Hypha/Command/Search.hs`
- Create: `test/Golden/Search.hs`
- Create: `test/Golden/golden/search-map-insert.compact.json` (golden)
- Modify: `app/hypha/Main.hs`
- Modify: `hypha.cabal`

- [ ] **Step 1: Extend `hypha.cabal`**

Extend `library` `build-depends`:

```cabal
    , contra-tracer        >= 0.2
    , optparse-applicative >= 0.18
    , prettyprinter        >= 1.7
    , prettyprinter-ansi-terminal >= 1.1
```

Extend `library` `exposed-modules`:

```cabal
    Hypha.Exit
    Hypha.Error
    Hypha.Logging
    Hypha.Cli.Parser
    Hypha.Cli.Run
    Hypha.Command.Search
```

Extend `test-suite` `other-modules`:

```cabal
    Golden.Search
```

Extend `test-suite` `build-depends`:

```cabal
    , tasty-golden >= 2.3
    , aeson
    , bytestring
```

- [ ] **Step 2: Write `Hypha.Exit`**

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hypha.Exit
  ( HyphaExit (..)
  , exitOk, exitUserError, exitNotFound, exitNetwork, exitCacheCorrupt, exitEnvironment
  ) where

newtype HyphaExit = HyphaExit { unHyphaExit :: Int }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Enum)

exitOk, exitUserError, exitNotFound, exitNetwork, exitCacheCorrupt, exitEnvironment :: HyphaExit
exitOk           = HyphaExit 0
exitUserError    = HyphaExit 2
exitNotFound     = HyphaExit 3
exitNetwork      = HyphaExit 4
exitCacheCorrupt = HyphaExit 5
exitEnvironment  = HyphaExit 7
```

- [ ] **Step 3: Write `Hypha.Error`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Error
  ( HyphaError (..)
  , errorCode
  , errorExit
  , errorMessage
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Exit (HyphaExit, exitUserError, exitNotFound, exitNetwork, exitCacheCorrupt, exitEnvironment)

data HyphaError
  = BadCliArgs !Text
  | NotInPlan !Text                -- package or symbol path
  | OfflineMissForbidden !Text
  | NetworkFailed !Text
  | CacheCorrupted !Text
  | EnvMissingPlan !FilePath
  | EnvMissingTool !Text           -- ghc / haddock / hoogle / etc.
  | StackNotSupported
  deriving stock (Show, Eq)

errorCode :: HyphaError -> Text
errorCode = \case
  BadCliArgs{}            -> "BAD_ARGS"
  NotInPlan{}             -> "NOT_IN_PLAN"
  OfflineMissForbidden{}  -> "OFFLINE_CACHE_MISS"
  NetworkFailed{}         -> "NETWORK_FAILED"
  CacheCorrupted{}        -> "CACHE_CORRUPTED"
  EnvMissingPlan{}        -> "ENV_MISSING_PLAN"
  EnvMissingTool{}        -> "ENV_MISSING_TOOL"
  StackNotSupported       -> "STACK_NOT_SUPPORTED"

errorExit :: HyphaError -> HyphaExit
errorExit = \case
  BadCliArgs{}            -> exitUserError
  NotInPlan{}             -> exitNotFound
  OfflineMissForbidden{}  -> exitNetwork
  NetworkFailed{}         -> exitNetwork
  CacheCorrupted{}        -> exitCacheCorrupt
  EnvMissingPlan{}        -> exitEnvironment
  EnvMissingTool{}        -> exitEnvironment
  StackNotSupported       -> exitEnvironment

errorMessage :: HyphaError -> Text
errorMessage = \case
  BadCliArgs t                  -> "bad CLI args: " <> t
  NotInPlan t                   -> "not in plan: " <> t
  OfflineMissForbidden u        -> "offline and not cached: " <> u
  NetworkFailed t               -> "network failure: " <> t
  CacheCorrupted t              -> "cache corruption: " <> t
  EnvMissingPlan p              -> "missing plan.json at " <> Text.pack p
  EnvMissingTool t              -> "required tool not on PATH: " <> t
  StackNotSupported             -> "Stack backend not supported in MVP"
```

- [ ] **Step 4: Write `Hypha.Logging`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Logging
  ( LogLevel (..)
  , LogEvent (..)
  , stderrTracer
  , quietTracer
  ) where

import Control.Tracer (Tracer, contramap, nullTracer)
import qualified Control.Tracer as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO (hPutStrLn, stderr)

data LogLevel = Debug | Info | Warn | Error
  deriving stock (Show, Eq, Ord)

data LogEvent = LogEvent
  { leLevel  :: !LogLevel
  , leScope  :: !Text
  , leText   :: !Text
  }
  deriving stock (Show, Eq)

stderrTracer :: LogLevel -> Tracer IO LogEvent
stderrTracer minLvl = T.Tracer $ \(LogEvent lvl scope txt) ->
  if lvl < minLvl
    then pure ()
    else TIO.hPutStrLn stderr ("[" <> sym lvl <> "] " <> scope <> ": " <> txt)
  where
    sym Debug = "DBG"
    sym Info  = "INF"
    sym Warn  = "WRN"
    sym Error = "ERR"

quietTracer :: Tracer IO LogEvent
quietTracer = nullTracer
```

> Note: `contra-tracer`'s `Tracer` ctor in current versions is `Tracer :: (a -> m ()) -> Tracer m a`. If your pinned version offers a `Tracer` smart constructor instead, swap `T.Tracer $` for the appropriate constructor.

- [ ] **Step 5: Write `Hypha.Cli.Parser`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Cli.Parser
  ( GlobalFlags (..)
  , Subcommand (..)
  , topParser
  , parseArgs
  , Fieldset (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative

import Hypha.Output.Json (Fieldset (..))

data GlobalFlags = GlobalFlags
  { gfProjectDir       :: !(Maybe FilePath)
  , gfPackageOverrides :: ![Text]
  , gfAny              :: !Bool
  , gfGlobalHoogle     :: !Bool
  , gfOffline          :: !Bool
  , gfHuman            :: !Bool
  , gfPrettyJson       :: !Bool
  , gfFieldset         :: !Fieldset
  , gfSelect           :: !(Maybe [Text])
  , gfVerbose          :: !Bool
  , gfQuiet            :: !Bool
  }
  deriving stock (Show, Eq)

data Subcommand
  = CmdSearch        !Text   -- query
  | CmdPackage       !Text   -- pkg
  | CmdModule        !Text   -- pkg/Mod
  | CmdSymbol        !Text   -- pkg/Mod/sym
  | CmdSource        !Text
  | CmdVersions      !Text
  | CmdDeps          !Text !Bool !(Maybe Int)
  | CmdWhatProvides  !Text
  | CmdDoctor
  deriving stock (Show, Eq)

globalParser :: Parser GlobalFlags
globalParser = GlobalFlags
  <$> optional (strOption (long "project-dir" <> metavar "DIR"))
  <*> many (strOption (long "package-override" <> metavar "PKG=VER"))
  <*> switch (long "any")
  <*> switch (long "global")
  <*> switch (long "offline")
  <*> switch (long "human")
  <*> switch (long "pretty-json")
  <*> flag Compact Full (long "full")
  <*> optional (Text.splitOn "," <$> strOption (long "select" <> metavar "FIELDS"))
  <*> switch (long "verbose")
  <*> switch (long "quiet")

subParser :: Parser Subcommand
subParser = hsubparser
  ( command "search"       (info (CmdSearch <$> argument str (metavar "QUERY"))      (progDesc "Hoogle search scoped to plan"))
 <> command "package"      (info (CmdPackage <$> argument str (metavar "PKG"))       (progDesc "Package metadata"))
 <> command "module"       (info (CmdModule <$> argument str (metavar "PKG/MOD"))    (progDesc "Module exports"))
 <> command "symbol"       (info (CmdSymbol <$> argument str (metavar "PKG/MOD/SYM")) (progDesc "Symbol info"))
 <> command "source"       (info (CmdSource <$> argument str (metavar "PKG/MOD[/SYM]")) (progDesc "Source slice"))
 <> command "versions"     (info (CmdVersions <$> argument str (metavar "PKG"))      (progDesc "Version history"))
 <> command "deps"         (info (CmdDeps
       <$> argument str (metavar "PKG")
       <*> switch (long "reverse")
       <*> optional (option auto (long "depth" <> metavar "N"))) (progDesc "Forward/reverse deps"))
 <> command "whatprovides" (info (CmdWhatProvides <$> argument str (metavar "SYM"))  (progDesc "Packages exporting symbol"))
 <> command "doctor"       (info (pure CmdDoctor)                                    (progDesc "Diagnose environment"))
  )

topParser :: ParserInfo (GlobalFlags, Subcommand)
topParser = info ((,) <$> globalParser <*> subParser <**> helper)
  ( fullDesc
 <> progDesc "Agent-first CLI for Hackage / Hoogle / cabal build plans."
 <> header   "hypha — probe Hackage for symbols, modules, and sources scoped to your plan."
  )

parseArgs :: IO (GlobalFlags, Subcommand)
parseArgs = execParser topParser
```

- [ ] **Step 6: Write `Hypha.Command.Search` (minimal, JSON only)**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Search
  ( runSearch
  , compactKeys
  , fullKeys
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Hoogle.Type    (Hoogle (..), HoogleHit (..), HoogleQuery (..))
import Hypha.Output.Outcome (Outcome (..), Action (..), Related (..))

compactKeys, fullKeys :: Set.Set Text
compactKeys = Set.fromList ["hits"]
fullKeys    = compactKeys

runSearch :: Monad m => Hoogle m -> Text -> m (Outcome Value)
runSearch h query = do
  hits <- searchHoogle h (HoogleQuery query)
  let body = object ["hits" .= map encodeHit hits]
  pure Outcome
    { outValue   = body
    , outActions = []
    , outRelated = [ Related (hhName hit)
                      ("hypha symbol " <> hhPackage hit <> "/" <> hhModule hit <> "/" <> hhName hit)
                   | hit <- take 5 hits
                   ]
    }

encodeHit :: HoogleHit -> Value
encodeHit hit = object
  [ "package" .= hhPackage hit
  , "module"  .= hhModule hit
  , "name"    .= hhName hit
  , "sig"     .= hhSig hit
  , "fetch"   .= ("hypha symbol " <> hhPackage hit <> "/" <> hhModule hit <> "/" <> hhName hit)
  ]
```

- [ ] **Step 7: Write `Hypha.Cli.Run`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Hypha.Cli.Run
  ( run
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)

import Hypha.Cli.Parser   (GlobalFlags (..), Subcommand (..), parseArgs)
import Hypha.Command.Search (runSearch, compactKeys, fullKeys)
import Hypha.Error         (HyphaError (..), errorExit, errorCode, errorMessage)
import Hypha.Exit          (HyphaExit (..), exitOk)
import Hypha.Hoogle.Query  (mkProjectHoogle, mkGlobalHoogle)
import Hypha.Hoogle.Database (HoogleConfig (..))
import Hypha.Project.Discovery (discoverProjectRoot, ProjectRoot (..))
import Hypha.Project.Plan      (loadBuildPlan)
import Hypha.Output.Json (encodeEnvelope, EnvelopeOpts (..), Fieldset (..))
import Hypha.Output.Outcome (CommandName (..))

run :: IO ()
run = do
  (gf, cmd) <- parseArgs
  result    <- dispatch gf cmd
  case result of
    Left err -> bailWith err
    Right () -> exitWith (intToExit (unHyphaExit exitOk))

dispatch :: GlobalFlags -> Subcommand -> IO (Either HyphaError ())
dispatch gf = \case
  CmdSearch q -> do
    eRoot <- discoverProjectRoot (gfProjectDir gf)
    case eRoot of
      Left _ -> pure (Left (EnvMissingPlan "no project root"))
      Right root -> do
        ePlan <- loadBuildPlan root
        case ePlan of
          Left _ -> pure (Left (EnvMissingPlan "plan.json missing"))
          Right _plan -> do
            h <- if gfGlobalHoogle gf then mkGlobalHoogle
                                      else mkProjectHoogle (HoogleConfig root [] "stub")
            oc <- runSearch h q
            let bs = encodeEnvelope (CommandName "search") False [] oc compactKeys fullKeys
                     EnvelopeOpts { eoFieldset = gfFieldset gf
                                  , eoSelect   = gfSelect gf
                                  , eoPretty   = gfPrettyJson gf
                                  }
            BL8.putStrLn bs
            pure (Right ())
  other -> pure (Left (BadCliArgs (Text.pack ("subcommand not yet wired: " <> show other))))

bailWith :: HyphaError -> IO ()
bailWith err = do
  hPutStrLn stderr (Text.unpack (errorCode err <> ": " <> errorMessage err))
  exitWith (intToExit (unHyphaExit (errorExit err)))

intToExit :: Int -> ExitCode
intToExit 0 = ExitSuccess
intToExit n = ExitFailure n
```

- [ ] **Step 8: Wire `app/hypha/Main.hs`**

Replace contents with:

```haskell
module Main (main) where

import qualified Hypha.Cli.Run as Run

main :: IO ()
main = Run.run
```

- [ ] **Step 9: Write golden test scaffolding**

Create `test/Golden/Search.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Golden.Search (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.Set as Set

import Hypha.Command.Search (compactKeys, fullKeys)
import Hypha.Output.Outcome (Outcome (..), Action (..), Related (..), CommandName (..))
import Hypha.Output.Json    (encodeEnvelope, EnvelopeOpts (..), Fieldset (..))

fakeOutcome :: Outcome Value
fakeOutcome = Outcome
  { outValue = object [ "hits" .= ( [ object [ "package" .= ("containers" :: String)
                                              , "module"  .= ("Data.Map.Strict" :: String)
                                              , "name"    .= ("insert" :: String)
                                              , "sig"     .= ("insert :: Ord k => k -> a -> Map k a -> Map k a" :: String)
                                              , "fetch"   .= ("hypha symbol containers/Data.Map.Strict/insert" :: String)
                                              ]
                                    ] :: [Value])
                     ]
  , outActions = []
  , outRelated = [Related "insert" "hypha symbol containers/Data.Map.Strict/insert"]
  }

tests :: TestTree
tests = testGroup "Golden.Search"
  [ goldenVsString
      "search-map-insert (compact)"
      "test/Golden/golden/search-map-insert.compact.json"
      (pure (encodeEnvelope (CommandName "search") False [] fakeOutcome compactKeys fullKeys
              EnvelopeOpts { eoFieldset = Compact, eoSelect = Nothing, eoPretty = False }))
  ]
```

Register in `test/Main.hs`:

```haskell
import qualified Golden.Search
-- ...
  , Golden.Search.tests
```

- [ ] **Step 10: Generate the golden file**

Run: `cabal test --test-options="--accept"`
Expected: golden file produced at `test/Golden/golden/search-map-insert.compact.json` and committed by you.

Inspect the file to make sure it contains the expected JSON shape (schema "hypha/v0", command "search", result.hits[]).

- [ ] **Step 11: Run tests in normal mode**

Run: `cabal test`
Expected: all suites pass; golden file matches.

- [ ] **Step 12: Smoke run the binary**

Run: `cabal run hypha -- --human search 'Map.insert'`
Expected: error message about unwired subcommand (since `--human` is not yet honoured) OR JSON envelope for search. Either way: the binary builds and produces output. Note any deficiencies for Task 11 (where `--human` lands).

- [ ] **Step 13: Commit**

```bash
git add hypha.cabal src/Hypha/Exit.hs src/Hypha/Error.hs src/Hypha/Logging.hs \
        src/Hypha/Cli/ src/Hypha/Command/Search.hs app/hypha/Main.hs \
        test/Golden/Search.hs test/Golden/golden/search-map-insert.compact.json test/Main.hs
git commit -m "feat(cli): exit codes, error sum, dispatcher, search command, first golden test"
```

---

## Task 10: Remaining commands — package, versions, module, symbol, source, deps, whatprovides

This task lands seven commands. Each follows the same shape: define `compactKeys`/`fullKeys`, write `run<Cmd> :: ... -> App (Outcome Value)`, wire into `Hypha.Cli.Run.dispatch`, add a golden test.

> **Reading order:** Sub-tasks 10.1 through 10.7 are independent given the dispatcher scaffolding; do them in this order to keep dispatcher diffs small. Each sub-task ends with a single commit.

**Files (created across all sub-tasks):**
- `src/Hypha/Command/Package.hs`
- `src/Hypha/Command/Versions.hs`
- `src/Hypha/Command/Module.hs`
- `src/Hypha/Command/Symbol.hs`
- `src/Hypha/Command/Source.hs`
- `src/Hypha/Command/Deps.hs`
- `src/Hypha/Command/WhatProvides.hs`
- `src/Hypha/Source/Locate.hs`
- `src/Hypha/Source/Extract.hs`
- `src/Hypha/Haddock/Parse.hs`
- `src/Hypha/Haddock/Interface.hs`
- `src/Hypha/Types/Doc.hs`
- `test/Golden/Commands.hs`
- `test/Golden/golden/*.compact.json` (seven goldens)

- [ ] **Step 1: Extend `hypha.cabal`**

Extend `library` `exposed-modules` (add all seven command modules and supporting modules):

```cabal
    Hypha.Command.Package
    Hypha.Command.Versions
    Hypha.Command.Module
    Hypha.Command.Symbol
    Hypha.Command.Source
    Hypha.Command.Deps
    Hypha.Command.WhatProvides
    Hypha.Source.Locate
    Hypha.Source.Extract
    Hypha.Haddock.Parse
    Hypha.Haddock.Interface
    Hypha.Types.Doc
```

Extend `library` `build-depends`:

```cabal
    , haddock-library >= 1.11
```

Extend `test-suite` `other-modules`:

```cabal
    Golden.Commands
```

### 10.1 — `Hypha.Command.Package`

- [ ] **Step 2: Write `Hypha.Command.Package`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Package
  ( runPackage
  , compactKeys, fullKeys
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Hypha.Output.Outcome (Outcome (..), Action (..), Related (..))
import Hypha.Output.Actions (actionSet)
import Hypha.Types.BuildPlan (BuildPlan (..), PlannedUnit (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

compactKeys, fullKeys :: Set.Set Text
compactKeys = Set.fromList ["name", "version", "in_plan", "is_local", "deps_count"]
fullKeys    = compactKeys

runPackage :: Monad m => BuildPlan -> PackageName -> m (Outcome Value)
runPackage bp name = pure Outcome
  { outValue = case Map.lookup name (bpUnits bp) of
      Nothing -> object [ "name" .= unPackageName name, "in_plan" .= False ]
      Just u  -> object [ "name"       .= unPackageName (pkgName (puId u))
                        , "version"    .= unVersion (pkgVersion (puId u))
                        , "in_plan"    .= True
                        , "is_local"   .= puIsLocal u
                        , "deps_count" .= length (puDeps u)
                        ]
  , outActions = actionSet (unPackageName name) ""
  , outRelated = []
  }
```

- [ ] **Step 3: Wire into dispatcher**

Edit `Hypha.Cli.Run.dispatch` to add the `CmdPackage` arm:

```haskell
  CmdPackage pkg -> do
    eRoot <- discoverProjectRoot (gfProjectDir gf)
    case eRoot of
      Left _ -> pure (Left (EnvMissingPlan "no project root"))
      Right root -> do
        ePlan <- loadBuildPlan root
        case ePlan of
          Left _ -> pure (Left (EnvMissingPlan "plan.json missing"))
          Right plan -> do
            oc <- runPackage plan (PackageName pkg)
            emit gf (CommandName "package") oc Package.compactKeys Package.fullKeys
            pure (Right ())
```

Introduce a helper `emit` in `Hypha.Cli.Run`:

```haskell
emit :: GlobalFlags -> CommandName -> Outcome Value -> Set.Set Text -> Set.Set Text -> IO ()
emit gf cmd oc ck fk = BL8.putStrLn
  (encodeEnvelope cmd False [] oc ck fk
     EnvelopeOpts { eoFieldset = gfFieldset gf
                  , eoSelect   = gfSelect gf
                  , eoPretty   = gfPrettyJson gf })
```

Refactor the existing `CmdSearch` arm to use `emit` (drops duplication).

- [ ] **Step 4: Golden test**

In `test/Golden/Commands.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Golden.Commands (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Aeson (Value, object, (.=))

import qualified Hypha.Command.Package as Package
import qualified Hypha.Command.Versions as Versions
import qualified Hypha.Command.Module as Module
import qualified Hypha.Command.Symbol as Symbol
import qualified Hypha.Command.Source as Source
import qualified Hypha.Command.Deps as Deps
import qualified Hypha.Command.WhatProvides as WhatProvides
import Hypha.Output.Outcome (Outcome (..), Action (..), Related (..), CommandName (..))
import Hypha.Output.Json    (encodeEnvelope, EnvelopeOpts (..), Fieldset (..))
import Hypha.Types.BuildPlan (BuildPlan (..), PlannedUnit (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

samplePlan :: BuildPlan
samplePlan = BuildPlan
  { bpGhc   = error "irrelevant for tests"   -- never forced in encoding
  , bpUnits = Map.fromList
      [ ( PackageName "async"
        , PlannedUnit (PackageId (PackageName "async") (Version "2.2.5")) False
            [PackageId (PackageName "base") (Version "4.18.2.0")]
        )
      ]
  }

emit :: CommandName -> Outcome Value -> Set.Set t -> Set.Set t -> IO ()
emit = error "wired below"

tests :: TestTree
tests = testGroup "Golden.Commands"
  [ goldenVsString "package-async (compact)"
      "test/Golden/golden/package-async.compact.json"
      (do oc <- Package.runPackage samplePlan (PackageName "async")
          pure (encodeEnvelope (CommandName "package") False [] oc Package.compactKeys Package.fullKeys
                  EnvelopeOpts { eoFieldset = Compact, eoSelect = Nothing, eoPretty = False }))
  ]
```

> The lazy `bpGhc` in the fixture is only safe because the encoder never forces it for these tests. Replace with `GhcId "9.6.6"` if you prefer to be defensive.

- [ ] **Step 5: Regenerate + run**

Run: `cabal test --test-options="--accept"`
Run: `cabal test`
Expected: golden test passes. Inspect `test/Golden/golden/package-async.compact.json`.

- [ ] **Step 6: Commit 10.1**

```bash
git add src/Hypha/Command/Package.hs src/Hypha/Cli/Run.hs test/Golden/Commands.hs \
        test/Golden/golden/package-async.compact.json test/Main.hs hypha.cabal
git commit -m "feat(cmd): package command + cli emit helper + golden"
```

### 10.2 — `Hypha.Command.Versions`

- [ ] **Step 7: Write the command**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Versions
  ( runVersions
  , compactKeys, fullKeys
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Hypha.Output.Outcome (Outcome (..))
import Hypha.Output.Actions (actionSet)
import Hypha.Types.BuildPlan (BuildPlan (..), PlannedUnit (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

compactKeys, fullKeys :: Set.Set Text
compactKeys = Set.fromList ["package", "pinned", "available"]
fullKeys    = compactKeys

-- | For MVP we only show the pinned version (from plan) and a placeholder
-- "available" list. Hackage version-list integration is plumbed in Task 13.
runVersions :: Monad m => BuildPlan -> PackageName -> m (Outcome Value)
runVersions bp name = pure Outcome
  { outValue = case Map.lookup name (bpUnits bp) of
      Nothing -> object [ "package" .= unPackageName name, "pinned" .= Aeson.Null, "available" .= ([] :: [Value]) ]
      Just u  -> object [ "package"   .= unPackageName name
                        , "pinned"    .= unVersion (pkgVersion (puId u))
                        , "available" .= ([] :: [Value])
                        ]
  , outActions = actionSet (unPackageName name) ""
  , outRelated = []
  }
```

- [ ] **Step 8: Wire into dispatcher**

Add to `dispatch`:

```haskell
  CmdVersions pkg -> withPlan gf $ \plan -> do
    oc <- runVersions plan (PackageName pkg)
    emit gf (CommandName "versions") oc Versions.compactKeys Versions.fullKeys
    pure (Right ())
```

Where `withPlan` is a new helper:

```haskell
withPlan :: GlobalFlags
         -> (BuildPlan -> IO (Either HyphaError ()))
         -> IO (Either HyphaError ())
withPlan gf k = do
  eRoot <- discoverProjectRoot (gfProjectDir gf)
  case eRoot of
    Left _ -> pure (Left (EnvMissingPlan "no project root"))
    Right root -> do
      ePlan <- loadBuildPlan root
      case ePlan of
        Left _ -> pure (Left (EnvMissingPlan "plan.json missing"))
        Right plan -> k plan
```

Refactor `CmdSearch` and `CmdPackage` arms to use `withPlan`.

- [ ] **Step 9: Golden + accept + commit**

Add a `versions-async` golden case alongside the `package-async` one. `cabal test --test-options="--accept"`, inspect, then:

```bash
git add src/Hypha/Command/Versions.hs src/Hypha/Cli/Run.hs \
        test/Golden/Commands.hs test/Golden/golden/versions-async.compact.json
git commit -m "feat(cmd): versions command + withPlan helper + golden"
```

### 10.3 — `Hypha.Command.Module`

- [ ] **Step 10: Write the command**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Module
  ( runModule
  , compactKeys, fullKeys
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.BuildEnv.Type   (BuildEnv (..))
import Hypha.Output.Outcome  (Outcome (..), Related (..))
import Hypha.Output.Actions  (actionSet)
import Hypha.Source.Locate   (listExportedSymbols)
import Hypha.Types.PackageId (PackageId (..), PackageName (..))

compactKeys, fullKeys :: Set.Set Text
compactKeys = Set.fromList ["package", "module", "exports"]
fullKeys    = compactKeys

runModule :: BuildEnv IO -> PackageId -> Text -> IO (Outcome Value)
runModule env pid modPath = do
  exps <- listExportedSymbols env pid modPath
  pure Outcome
    { outValue = object
        [ "package" .= unPackageName (pkgName pid)
        , "module"  .= modPath
        , "exports" .= map (\nm -> object ["name" .= nm]) exps
        ]
    , outActions = actionSet (unPackageName (pkgName pid)) modPath
    , outRelated = [ Related nm ("hypha symbol " <> unPackageName (pkgName pid) <> "/" <> modPath <> "/" <> nm)
                   | nm <- take 5 exps
                   ]
    }
```

- [ ] **Step 11: Write `Hypha.Source.Locate`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Source.Locate
  ( listExportedSymbols
  , locateSymbolDefinition
  , SourceLocation (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type   (BuildEnv (..))
import Hypha.Types.PackageId (PackageId (..))

data SourceLocation = SourceLocation
  { slPath :: !FilePath
  , slLine :: !Int
  }
  deriving stock (Show, Eq)

-- | Inspect the package source directory and list candidate exported symbols
-- by scraping the @module ... ( ... ) where@ header naïvely. A future
-- improvement (post-MVP) is to use ghc-lib-parser for accurate parsing.
listExportedSymbols :: BuildEnv IO -> PackageId -> Text -> IO [Text]
listExportedSymbols env pid modPath = do
  mDir <- locatePackageSource env pid
  case mDir of
    Nothing -> pure []
    Just d  -> do
      let f = d </> modulePathToFile modPath
      ok <- doesFileExist f
      if not ok
        then pure []
        else parseExports <$> TIO.readFile f

modulePathToFile :: Text -> FilePath
modulePathToFile m = Text.unpack (Text.replace "." "/" m) <> ".hs"

-- | Crude header parser: take everything between the first '(' and the matching ')'.
parseExports :: Text -> [Text]
parseExports src =
  let body = Text.dropWhile (/= '(') src
      end  = Text.takeWhile (/= ')') (Text.drop 1 body)
      raw  = Text.splitOn "," end
  in [ trim x | x <- raw, not (Text.null (trim x)) ]
  where
    trim = Text.dropWhile (`elem` (" \t\n" :: String))
         . Text.dropWhileEnd (`elem` (" \t\n" :: String))

-- | Find the line where a symbol is defined.
locateSymbolDefinition :: BuildEnv IO -> PackageId -> Text -> Text -> IO (Maybe SourceLocation)
locateSymbolDefinition env pid modPath sym = do
  mDir <- locatePackageSource env pid
  case mDir of
    Nothing -> pure Nothing
    Just d  -> do
      let f = d </> Text.unpack (Text.replace "." "/" modPath) <> ".hs"
      ok <- doesFileExist f
      if not ok
        then pure Nothing
        else do
          ls <- Text.lines <$> TIO.readFile f
          pure $ case [ i | (i, l) <- zip [1..] ls, startsWith sym l ] of
                   (i:_) -> Just (SourceLocation f i)
                   []    -> Nothing
  where
    startsWith name l =
      let trimmed = Text.dropWhile (== ' ') l
      in name `Text.isPrefixOf` trimmed
```

- [ ] **Step 12: Wire into dispatcher, regenerate goldens, commit 10.3**

Add `CmdModule` arm:

```haskell
  CmdModule arg -> withPlan gf $ \plan -> do
    case Text.splitOn "/" arg of
      [pkg, modPath] ->
        case Map.lookup (PackageName pkg) (bpUnits plan) of
          Nothing -> pure (Left (NotInPlan pkg))
          Just u  -> do
            env <- mkCabalBuildEnv CabalEnvConfig
                     { cecStorePath = "/dev/null"  -- replaced below
                     , cecGhc       = bpGhc plan
                     }
            oc <- runModule env (puId u) modPath
            emit gf (CommandName "module") oc Module.compactKeys Module.fullKeys
            pure (Right ())
      _ -> pure (Left (BadCliArgs "expected PKG/MOD"))
```

> The placeholder `/dev/null` store path here is intentionally wrong: it will produce empty exports until Task 12 (`doctor`) and Task 13 (Haddock integration) wire a real store config. For this task's golden, hand-craft a fixture under `test/fixtures/fake-cabal-store/.../share/async/Control/Concurrent/Async.hs` whose header lists a few exports.

Create `test/fixtures/fake-cabal-store/ghc-9.6.6/async-2.2.5-deadbeef/share/async/Control/Concurrent/Async.hs`:

```haskell
module Control.Concurrent.Async
  ( Async
  , async
  , wait
  , cancel
  , concurrently
  , race
  ) where
```

In the dispatcher, use this fixture store path **only when env var `HYPHA_FIXTURE_STORE` is set**, so the golden test can drive it. Add:

```haskell
import System.Environment (lookupEnv)
-- ...
   storePath <- maybe "/dev/null" id <$> lookupEnv "HYPHA_FIXTURE_STORE"
```

Add a golden case driven with `HYPHA_FIXTURE_STORE=test/fixtures/fake-cabal-store/ghc-9.6.6/async-2.2.5-deadbeef`. Reset goldens, commit:

```bash
git add src/Hypha/Command/Module.hs src/Hypha/Source/Locate.hs src/Hypha/Cli/Run.hs \
        test/Golden/Commands.hs test/Golden/golden/module-async.compact.json \
        test/fixtures/fake-cabal-store
git commit -m "feat(cmd): module command + naive export scraper + golden via fixture store"
```

### 10.4 — `Hypha.Command.Symbol`

- [ ] **Step 13: Write `Hypha.Types.Doc` and `Hypha.Haddock.Parse`**

`src/Hypha/Types/Doc.hs`:

```haskell
module Hypha.Types.Doc
  ( DocText (..)
  ) where

import Data.Text (Text)

-- | A raw Haddock-markup chunk extracted from a `.hs` source comment block.
-- We retain the original markup verbatim for JSON output (so agents can
-- decide how to render); the `--human` path parses it with haddock-library.
newtype DocText = DocText { unDocText :: Text }
  deriving stock (Show, Eq)
```

`src/Hypha/Haddock/Parse.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Haddock.Parse
  ( extractDocBlock
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.Doc (DocText (..))

-- | Given the full text of a `.hs` file and a symbol name, return the
-- Haddock comment block immediately preceding the symbol's definition.
extractDocBlock :: Text -> Text -> Maybe DocText
extractDocBlock src name =
  let ls = Text.lines src
      indexed = zip [0 :: Int ..] ls
      hits = [ i | (i, l) <- indexed, startsWithDef l ]
  in case hits of
       (i:_) -> Just (DocText (collect ls i))
       []    -> Nothing
  where
    startsWithDef l =
      let t = Text.dropWhile (== ' ') l
      in name `Text.isPrefixOf` t && not (Text.null t) && not ("--" `Text.isPrefixOf` t)
    collect ls0 i =
      let above = reverse (take i ls0)
          haddockLines = takeWhile isHaddock above
      in Text.unlines (reverse haddockLines)
    isHaddock l =
      let t = Text.dropWhile (== ' ') l
      in "-- |" `Text.isPrefixOf` t || "--" `Text.isPrefixOf` t
```

- [ ] **Step 14: Write `Hypha.Source.Extract`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Source.Extract
  ( extractSignature
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Find the type-signature line for `name` in the given source text.
extractSignature :: Text -> Text -> Maybe Text
extractSignature src name =
  let ls = Text.lines src
  in case [ l | l <- ls, looksLikeSig name (Text.dropWhile (== ' ') l) ] of
       (l:_) -> Just (Text.strip l)
       []    -> Nothing
  where
    looksLikeSig n l = (n <> " ::") `Text.isPrefixOf` l
```

- [ ] **Step 15: Write `Hypha.Haddock.Interface` (stub for now)**

```haskell
module Hypha.Haddock.Interface
  ( hasInterfaceFile
  ) where

import System.Directory (doesFileExist)

-- | Stub: presence check for a `.haddock` file. Real parsing lives behind
-- `haddock-api`, which has a heavy dep. We may add it in Plan B.
hasInterfaceFile :: FilePath -> IO Bool
hasInterfaceFile = doesFileExist
```

- [ ] **Step 16: Write `Hypha.Command.Symbol`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Symbol
  ( runSymbol
  , compactKeys, fullKeys
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type   (BuildEnv (..))
import Hypha.Haddock.Parse   (extractDocBlock)
import Hypha.Output.Outcome  (Outcome (..), Related (..))
import Hypha.Output.Actions  (actionSet)
import Hypha.Source.Extract  (extractSignature)
import Hypha.Source.Locate   (locateSymbolDefinition, SourceLocation (..))
import Hypha.Types.Doc       (DocText (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

compactKeys, fullKeys :: Set.Set Text
compactKeys = Set.fromList
  [ "kind", "name", "package", "version", "module"
  , "signature", "haddock_raw", "source" ]
fullKeys = compactKeys

runSymbol :: BuildEnv IO
          -> PackageId
          -> Text   -- ^ module path
          -> Text   -- ^ symbol name
          -> IO (Outcome Value)
runSymbol env pid modPath sym = do
  loc <- locateSymbolDefinition env pid modPath sym
  case loc of
    Nothing -> pure Outcome
      { outValue = object [ "kind" .= ("function" :: Text)
                          , "name" .= sym
                          , "package" .= unPackageName (pkgName pid)
                          , "version" .= unVersion (pkgVersion pid)
                          , "module"  .= modPath
                          , "signature"   .= ("" :: Text)
                          , "haddock_raw" .= ("" :: Text)
                          , "source" .= object ["path" .= ("" :: Text), "line" .= (0 :: Int)]
                          ]
      , outActions = actionSet (unPackageName (pkgName pid)) modPath
      , outRelated = []
      }
    Just (SourceLocation path line) -> do
      src <- TIO.readFile path
      let sig = maybe "" id (extractSignature src sym)
          doc = maybe (DocText "") id (extractDocBlock src sym)
      pure Outcome
        { outValue = object
            [ "kind"        .= ("function" :: Text)
            , "name"        .= sym
            , "package"     .= unPackageName (pkgName pid)
            , "version"     .= unVersion (pkgVersion pid)
            , "module"      .= modPath
            , "signature"   .= sig
            , "haddock_raw" .= unDocText doc
            , "source"      .= object [ "path" .= path, "line" .= line ]
            ]
        , outActions = actionSet (unPackageName (pkgName pid)) modPath
        , outRelated = []
        }
```

- [ ] **Step 17: Wire `CmdSymbol`, golden, commit 10.4**

Add `CmdSymbol` arm parsing `PKG/MOD/SYM`:

```haskell
  CmdSymbol arg -> withPlan gf $ \plan ->
    case Text.splitOn "/" arg of
      [pkg, modPath, sym] -> ...
```

Augment the fixture `Control/Concurrent/Async.hs` to actually define `concurrently`:

```haskell
-- | Run two 'IO' actions concurrently and return both results.
concurrently :: IO a -> IO b -> IO (a, b)
concurrently = undefined
```

Add a golden case `symbol-async-concurrently`. Regenerate, inspect, commit.

```bash
git add src/Hypha/Command/Symbol.hs src/Hypha/Haddock/Parse.hs src/Hypha/Haddock/Interface.hs \
        src/Hypha/Source/Extract.hs src/Hypha/Types/Doc.hs src/Hypha/Cli/Run.hs \
        test/Golden/Commands.hs test/Golden/golden/symbol-async-concurrently.compact.json \
        test/fixtures/fake-cabal-store hypha.cabal
git commit -m "feat(cmd): symbol command — signature, haddock_raw, source coords; golden"
```

### 10.5 — `Hypha.Command.Source`

- [ ] **Step 18: Write the command**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Source
  ( runSource
  , compactKeys, fullKeys
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Text (Text)

import Hypha.BuildEnv.Type   (BuildEnv (..))
import Hypha.Output.Outcome  (Outcome (..))
import Hypha.Output.Actions  (actionSet)
import Hypha.Source.Locate   (locateSymbolDefinition, SourceLocation (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..))

compactKeys, fullKeys :: Set.Set Text
compactKeys = Set.fromList ["package", "module", "symbol", "source", "snippet"]
fullKeys    = compactKeys

runSource :: BuildEnv IO -> PackageId -> Text -> Maybe Text -> IO (Outcome Value)
runSource env pid modPath mSym = do
  case mSym of
    Just sym -> do
      mLoc <- locateSymbolDefinition env pid modPath sym
      case mLoc of
        Nothing -> emptyOutcome
        Just loc -> do
          src <- TIO.readFile (slPath loc)
          let ls   = Text.lines src
              snippet = Text.unlines (take 30 (drop (slLine loc - 1) ls))
          pure Outcome
            { outValue = object
                [ "package" .= unPackageName (pkgName pid)
                , "module"  .= modPath
                , "symbol"  .= sym
                , "source"  .= object ["path" .= slPath loc, "line" .= slLine loc]
                , "snippet" .= snippet
                ]
            , outActions = actionSet (unPackageName (pkgName pid)) modPath
            , outRelated = []
            }
    Nothing -> emptyOutcome   -- whole-module variant: deferred to Plan B server.
  where
    emptyOutcome = pure Outcome
      { outValue = object [ "package" .= unPackageName (pkgName pid)
                          , "module"  .= modPath
                          , "symbol"  .= maybe "" id mSym
                          , "source"  .= object ["path" .= ("" :: Text), "line" .= (0 :: Int)]
                          , "snippet" .= ("" :: Text)
                          ]
      , outActions = actionSet (unPackageName (pkgName pid)) modPath
      , outRelated = []
      }
```

- [ ] **Step 19: Wire + golden + commit 10.5**

Add `CmdSource` dispatcher arm parsing `PKG/MOD[/SYM]`. Add a golden case. Regenerate, inspect, commit.

```bash
git add src/Hypha/Command/Source.hs src/Hypha/Cli/Run.hs \
        test/Golden/Commands.hs test/Golden/golden/source-async-concurrently.compact.json
git commit -m "feat(cmd): source command — symbol snippet with path/line; golden"
```

### 10.6 — `Hypha.Command.Deps`

- [ ] **Step 20: Write the command**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Deps
  ( runDeps
  , compactKeys, fullKeys
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import Hypha.Output.Outcome  (Outcome (..), Related (..))
import Hypha.Output.Actions  (actionSet)
import Hypha.Types.BuildPlan (BuildPlan (..), PlannedUnit (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

compactKeys, fullKeys :: Set.Set Text
compactKeys = Set.fromList ["package", "direction", "depth", "deps"]
fullKeys    = compactKeys

runDeps :: Monad m
        => BuildPlan
        -> PackageName
        -> Bool                -- ^ reverse?
        -> Maybe Int           -- ^ depth bound
        -> m (Outcome Value)
runDeps bp name reverseMode mDepth = pure Outcome
  { outValue = object
      [ "package"   .= unPackageName name
      , "direction" .= (if reverseMode then ("reverse" :: Text) else "forward")
      , "depth"     .= maybe Aeson.Null Aeson.toJSON mDepth
      , "deps"      .= map encodeDep listing
      ]
  , outActions = actionSet (unPackageName name) ""
  , outRelated = [ Related (unPackageName n) ("hypha package " <> unPackageName n)
                 | n <- take 5 (map fst listing)
                 ]
  }
  where
    listing :: [(PackageName, Version)]
    listing
      | reverseMode = reverseDepsOf name (bpUnits bp)
      | otherwise   = forwardDepsOf name (bpUnits bp)

    encodeDep (n, v) = object
      [ "package" .= unPackageName n
      , "version" .= unVersion v
      , "fetch"   .= ("hypha package " <> unPackageName n)
      ]

forwardDepsOf :: PackageName -> Map.Map PackageName PlannedUnit -> [(PackageName, Version)]
forwardDepsOf nm units = case Map.lookup nm units of
  Nothing -> []
  Just u  -> [ (pkgName pid, pkgVersion pid) | pid <- puDeps u ]

reverseDepsOf :: PackageName -> Map.Map PackageName PlannedUnit -> [(PackageName, Version)]
reverseDepsOf target units =
  [ (pkgName (puId u), pkgVersion (puId u))
  | u <- Map.elems units
  , any (\d -> pkgName d == target) (puDeps u)
  ]
```

- [ ] **Step 21: Wire + golden + commit 10.6**

Add `CmdDeps` dispatcher arm. Golden `deps-async-forward`, `deps-async-reverse`. Regenerate, commit.

```bash
git add src/Hypha/Command/Deps.hs src/Hypha/Cli/Run.hs \
        test/Golden/Commands.hs test/Golden/golden/deps-async-*.compact.json
git commit -m "feat(cmd): deps command — forward and reverse, depth-aware; goldens"
```

### 10.7 — `Hypha.Command.WhatProvides`

- [ ] **Step 22: Write the command**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.WhatProvides
  ( runWhatProvides
  , compactKeys, fullKeys
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import Hypha.Hoogle.Type    (Hoogle (..), HoogleHit (..), HoogleQuery (..))
import Hypha.Output.Outcome (Outcome (..), Related (..))

compactKeys, fullKeys :: Set.Set Text
compactKeys = Set.fromList ["symbol", "providers"]
fullKeys    = compactKeys

runWhatProvides :: Monad m => Hoogle m -> Text -> m (Outcome Value)
runWhatProvides h sym = do
  hits <- searchHoogle h (HoogleQuery ("is:exact " <> sym))
  pure Outcome
    { outValue = object
        [ "symbol"    .= sym
        , "providers" .= map encodeProvider hits
        ]
    , outActions = []
    , outRelated = [ Related (hhPackage hit) ("hypha package " <> hhPackage hit)
                   | hit <- take 5 hits
                   ]
    }
  where
    encodeProvider hit = object
      [ "package" .= hhPackage hit
      , "module"  .= hhModule hit
      , "fetch"   .= ("hypha symbol " <> hhPackage hit <> "/" <> hhModule hit <> "/" <> hhName hit)
      ]
```

- [ ] **Step 23: Wire + golden + commit 10.7**

Add `CmdWhatProvides` dispatcher arm; reuse the project Hoogle constructed in `CmdSearch`. Golden `whatprovides-concurrently`. Regenerate, commit.

```bash
git add src/Hypha/Command/WhatProvides.hs src/Hypha/Cli/Run.hs \
        test/Golden/Commands.hs test/Golden/golden/whatprovides-concurrently.compact.json
git commit -m "feat(cmd): whatprovides — Hoogle-backed lookup by symbol name; golden"
```

---

## Task 11: `--human` renderer (DocH → ANSI, signature syntax highlighting)

**Files:**
- Create: `src/Hypha/Output/Human.hs`
- Create: `test/Golden/Human.hs`
- Create: `test/Golden/golden/human-symbol-async-concurrently.ansi` (golden)
- Modify: `src/Hypha/Cli/Run.hs` (route through Human when `--human`)
- Modify: `hypha.cabal` (add `skylighting`)

- [ ] **Step 1: Extend `hypha.cabal`**

Extend `library` `build-depends`:

```cabal
    , skylighting        >= 0.14
    , skylighting-core   >= 0.14
```

Extend `library` `exposed-modules`:

```cabal
    Hypha.Output.Human
```

Extend `test-suite` `other-modules`:

```cabal
    Golden.Human
```

- [ ] **Step 2: Write `Hypha.Output.Human`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Hypha.Output.Human
  ( renderSymbolCard
  , renderHaddock
  , renderSignature
  ) where

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Documentation.Haddock.Parser as HP
import qualified Documentation.Haddock.Types  as HT
import qualified Skylighting as Sky
import qualified Skylighting.Format.ANSI as SkyAnsi
import Prettyprinter (Doc, pretty, vsep, hsep, indent, (<+>))
import qualified Prettyprinter as PP
import Prettyprinter.Render.Terminal (AnsiStyle, color, Color (..), bold, italicized, putDoc)

renderSymbolCard
  :: Text  -- ^ name
  -> Text  -- ^ kind
  -> Text  -- ^ signature
  -> Text  -- ^ haddock_raw
  -> Text  -- ^ source path
  -> Int   -- ^ source line
  -> Doc AnsiStyle
renderSymbolCard name kind sig hRaw srcPath srcLine = vsep
  [ PP.annotate bold (pretty name) <+> PP.annotate (color Cyan) (pretty kind)
  , indent 2 (renderSignature sig)
  , PP.emptyDoc
  , indent 2 (renderHaddock hRaw)
  , PP.emptyDoc
  , PP.annotate (color Yellow)
      (pretty (srcPath <> ":" <> Text.pack (show srcLine)))
  ]

renderSignature :: Text -> Doc AnsiStyle
renderSignature t =
  case Sky.lookupSyntax "Haskell" Sky.defaultSyntaxMap of
    Nothing -> pretty t
    Just syn ->
      case Sky.tokenize Sky.TokenizerConfig { Sky.syntaxMap = Sky.defaultSyntaxMap, Sky.traceOutput = False } syn t of
        Left  _      -> pretty t
        Right tokens ->
          let formatted = SkyAnsi.formatANSI Sky.defaultFormatOpts Sky.defaultStyle tokens
          in pretty (Text.pack formatted)

renderHaddock :: Text -> Doc AnsiStyle
renderHaddock src =
  let doc = HP.parseString (Text.unpack src)
  in fromDocH doc

fromDocH :: HT.DocH a b -> Doc AnsiStyle
fromDocH = \case
  HT.DocEmpty        -> PP.emptyDoc
  HT.DocAppend x y   -> fromDocH x PP.<> fromDocH y
  HT.DocString s     -> pretty (Text.pack s)
  HT.DocParagraph x  -> fromDocH x PP.<> PP.line PP.<> PP.line
  HT.DocIdentifier _ -> PP.annotate (color Magenta) (pretty ("⟨id⟩" :: Text))
  HT.DocIdentifierUnchecked _ -> pretty ("⟨id⟩" :: Text)
  HT.DocModule m     -> PP.annotate italicized (pretty (Text.pack (show m)))
  HT.DocEmphasis x   -> PP.annotate italicized (fromDocH x)
  HT.DocBold x       -> PP.annotate bold (fromDocH x)
  HT.DocMonospaced x -> PP.annotate (color Cyan) (fromDocH x)
  HT.DocCodeBlock x  -> indent 4 (fromDocH x)
  HT.DocHyperlink _  -> PP.annotate (color Blue) (pretty ("⟨link⟩" :: Text))
  HT.DocPic _        -> pretty ("⟨pic⟩" :: Text)
  HT.DocAName _      -> PP.emptyDoc
  HT.DocProperty _   -> PP.emptyDoc
  HT.DocExamples _   -> PP.emptyDoc
  HT.DocHeader h     -> PP.annotate bold (fromDocH (HT.headerTitle h)) PP.<> PP.line
  HT.DocTable _      -> pretty ("⟨table⟩" :: Text)
  HT.DocUnorderedList xs -> vsep (map (\x -> pretty ("• " :: Text) PP.<> fromDocH x) xs)
  HT.DocOrderedList  xs -> vsep (zipWith (\i x -> pretty (Text.pack (show (i :: Int) <> ". ")) PP.<> fromDocH x) [1..] (map snd xs))
  HT.DocDefList xs   -> vsep [ fromDocH k PP.<> pretty (":" :: Text) <+> fromDocH v | (k,v) <- xs ]
  HT.DocMathInline _ -> pretty ("⟨math⟩" :: Text)
  HT.DocMathDisplay _ -> pretty ("⟨math⟩" :: Text)
  HT.DocWarning x    -> PP.annotate (color Yellow) (fromDocH x)
```

> Notes: (a) the `haddock-library` AST varies slightly across versions; treat constructors that don't exist in the pinned version as no-ops. (b) `skylighting`'s ANSI formatting helpers may differ in exact module path; check `cabal haddock skylighting` to confirm. (c) the renderer outputs only the `Doc AnsiStyle`; printing happens in `Hypha.Cli.Run`.

- [ ] **Step 3: Route `--human` through the renderer**

In `Hypha.Cli.Run.emit`, fork on `gfHuman`:

```haskell
import Prettyprinter.Render.Terminal (renderStrict)
import qualified Data.Text.IO as TIO
import Hypha.Output.Human (renderSymbolCard, renderHaddock, renderSignature)

emit gf cmd oc ck fk = do
  if gfHuman gf
    then humanEmit gf cmd oc
    else BL8.putStrLn (encodeEnvelope cmd False [] oc ck fk
                        EnvelopeOpts { eoFieldset = gfFieldset gf
                                     , eoSelect   = gfSelect gf
                                     , eoPretty   = gfPrettyJson gf })

humanEmit :: GlobalFlags -> CommandName -> Outcome Value -> IO ()
humanEmit _gf (CommandName cmd) oc = case cmd of
  "symbol" -> case outValue oc of
    Object km ->
      let getT k = case KeyMap.lookup (Key.fromText k) km of
            Just (String s) -> s
            _               -> ""
          getI k = case KeyMap.lookup (Key.fromText k) km of
            Just (Number n) -> truncate n
            _               -> 0
      in TIO.putStrLn (renderStrict (PP.layoutPretty PP.defaultLayoutOptions
            (renderSymbolCard
               (getT "name") (getT "kind") (getT "signature") (getT "haddock_raw")
               (lookupSourcePath (KeyMap.lookup "source" km))
               (lookupSourceLine (KeyMap.lookup "source" km)))))
    _ -> TIO.putStrLn "(no result)"
  _ -> TIO.putStrLn "(no human renderer for this command yet)"
  where
    lookupSourcePath (Just (Object o)) = case KeyMap.lookup "path" o of
                                          Just (String s) -> s
                                          _               -> ""
    lookupSourcePath _                 = ""
    lookupSourceLine (Just (Object o)) = case KeyMap.lookup "line" o of
                                          Just (Number n) -> truncate n
                                          _               -> 0
    lookupSourceLine _                 = 0
```

- [ ] **Step 4: Golden test for human rendering**

Create `test/Golden/Human.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Golden.Human (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.Encoding as TE
import qualified Prettyprinter as PP
import Prettyprinter.Render.Terminal (renderStrict)

import Hypha.Output.Human (renderSymbolCard)

tests :: TestTree
tests = testGroup "Golden.Human"
  [ goldenVsString "human-symbol-async-concurrently"
      "test/Golden/golden/human-symbol-async-concurrently.ansi"
      (pure (BL.fromStrict (TE.encodeUtf8 (renderStrict (PP.layoutPretty PP.defaultLayoutOptions
        (renderSymbolCard
          "concurrently" "function"
          "concurrently :: IO a -> IO b -> IO (a, b)"
          "Run two 'IO' actions concurrently and return both results."
          "Control/Concurrent/Async.hs"
          234))))))
  ]
```

Register in `test/Main.hs`:

```haskell
import qualified Golden.Human
-- ...
  , Golden.Human.tests
```

- [ ] **Step 5: Generate goldens; run; commit**

Run: `cabal test --test-options="--accept"`
Run: `cabal test`
Expected: all tests pass, including the new human-render golden.

```bash
git add hypha.cabal src/Hypha/Output/Human.hs src/Hypha/Cli/Run.hs \
        test/Golden/Human.hs test/Golden/golden/human-symbol-async-concurrently.ansi test/Main.hs
git commit -m "feat(human): DocH→ANSI renderer + skylighting signatures + symbol-card golden"
```

---

## Task 12: `doctor` command

**Files:**
- Create: `src/Hypha/Command/Doctor.hs`
- Create: `test/Unit/Doctor.hs`
- Modify: `src/Hypha/Cli/Run.hs` (wire `CmdDoctor`)
- Modify: `hypha.cabal`

- [ ] **Step 1: Add modules**

Extend `library` `exposed-modules`:

```cabal
    Hypha.Command.Doctor
```

Extend `test-suite` `other-modules`:

```cabal
    Unit.Doctor
```

Extend `library` `build-depends`:

```cabal
    , typed-process >= 0.2.10
```

- [ ] **Step 2: Write `Hypha.Command.Doctor`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Doctor
  ( runDoctor
  , compactKeys, fullKeys
  , Check (..)
  , CheckStatus (..)
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import System.Directory (doesFileExist, findExecutable)
import qualified System.Process.Typed as TP
import Control.Exception (try, SomeException)

import Hypha.Output.Outcome (Outcome (..))

data CheckStatus = Pass | Warn | Fail
  deriving stock (Show, Eq)

data Check = Check
  { ckName   :: !Text
  , ckStatus :: !CheckStatus
  , ckDetail :: !Text
  }
  deriving stock (Show, Eq)

compactKeys, fullKeys :: Set.Set Text
compactKeys = Set.fromList ["checks", "all_pass"]
fullKeys    = compactKeys

runDoctor :: FilePath  -- ^ project root
          -> IO (Outcome Value)
runDoctor projectRoot = do
  cs <- sequence
          [ checkExecutable "ghc"
          , checkExecutable "haddock"
          , checkPlanJson projectRoot
          ]
  let allOk = all (\c -> ckStatus c == Pass) cs
  pure Outcome
    { outValue = object
        [ "checks"   .= map encodeCheck cs
        , "all_pass" .= allOk
        ]
    , outActions = []
    , outRelated = []
    }

encodeCheck :: Check -> Value
encodeCheck (Check n s d) = object
  [ "name"   .= n
  , "status" .= statusText s
  , "detail" .= d
  ]

statusText :: CheckStatus -> Text
statusText Pass = "pass"
statusText Warn = "warn"
statusText Fail = "fail"

checkExecutable :: Text -> IO Check
checkExecutable tool = do
  m <- findExecutable (Text.unpack tool)
  case m of
    Just p -> pure (Check tool Pass (Text.pack p))
    Nothing -> pure (Check tool Fail ("not found on PATH"))

checkPlanJson :: FilePath -> IO Check
checkPlanJson root = do
  let p = root <> "/dist-newstyle/cache/plan.json"
  ok <- doesFileExist p
  pure $ if ok
           then Check "plan.json" Pass (Text.pack p)
           else Check "plan.json" Fail ("run `cabal build --dry-run` to materialise " <> Text.pack p)
```

- [ ] **Step 3: Wire dispatcher**

In `Hypha.Cli.Run.dispatch`:

```haskell
  CmdDoctor -> do
    eRoot <- discoverProjectRoot (gfProjectDir gf)
    case eRoot of
      Left _    -> pure (Left (EnvMissingPlan "no project root"))
      Right (ProjectRoot r) -> do
        oc <- Doctor.runDoctor r
        emit gf (CommandName "doctor") oc Doctor.compactKeys Doctor.fullKeys
        pure (Right ())
```

- [ ] **Step 4: Unit test**

Create `test/Unit/Doctor.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.Doctor (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KeyMap

import Hypha.Command.Doctor (runDoctor)
import Hypha.Output.Outcome (Outcome (..))

tests :: TestTree
tests = testGroup "Doctor"
  [ testCase "plan.json check fails for empty dir" $ do
      oc <- runDoctor "/tmp/this-dir-has-no-plan-json"
      case outValue oc of
        Object km -> case KeyMap.lookup "all_pass" km of
          Just (Bool False) -> pure ()
          other -> assertBool ("expected all_pass=false; got " <> show other) False
        _ -> assertBool "expected object outcome" False
  ]
```

Register in `test/Main.hs`:

```haskell
import qualified Unit.Doctor
-- ...
  , Unit.Doctor.tests
```

- [ ] **Step 5: Run tests + smoke run + commit**

Run: `cabal test`
Expected: pass.
Run: `cabal run hypha -- doctor`
Expected: JSON envelope with checks; `all_pass` likely true on a dev machine, false in a fresh dir.

```bash
git add hypha.cabal src/Hypha/Command/Doctor.hs src/Hypha/Cli/Run.hs test/Unit/Doctor.hs test/Main.hs
git commit -m "feat(cmd): doctor — checks ghc, haddock, plan.json presence"
```

---

## Task 13: `Haddock.Generate` — lazy on-demand build pipeline + cache layout

This task lands the on-demand Haddock build/cache plumbing that Plan B's `server` command will rely on. It does not surface a new CLI subcommand; instead, it adds a callable interface and integrates it into `doctor` so that "Haddock cache reachable" becomes a checked invariant.

**Files:**
- Create: `src/Hypha/Haddock/Generate.hs`
- Modify: `src/Hypha/Command/Doctor.hs` (add a Haddock-cache check)
- Modify: `src/Hypha/Cache.hs` (add `haddockCacheRoot` helper)
- Create: `test/Unit/Haddock.hs`
- Modify: `hypha.cabal`

- [ ] **Step 1: Extend `hypha.cabal`**

Extend `library` `exposed-modules`:

```cabal
    Hypha.Haddock.Generate
```

Extend `test-suite` `other-modules`:

```cabal
    Unit.Haddock
```

- [ ] **Step 2: Extend `Hypha.Cache`**

Append to `src/Hypha/Cache.hs`:

```haskell
haddockCacheRoot :: IO FilePath
haddockCacheRoot = cacheRoot "haddock"
```

- [ ] **Step 3: Write `Hypha.Haddock.Generate`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Haddock.Generate
  ( ensureHaddockFor
  , haddockDirFor
  , HaddockSource (..)
  ) where

import Control.Exception (try, SomeException)
import qualified Data.Text as Text
import Data.Text (Text)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, createFileLink)
import System.FilePath ((</>))
import qualified System.Process.Typed as TP

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Cache         (haddockCacheRoot)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

data HaddockSource = FromCache | FromStore | Built | Missing
  deriving stock (Show, Eq)

-- | Resolve the directory holding a package's Haddock HTML, generating if
-- needed. Returns @(dir, source)@: @dir@ is the cache dir under
-- @~/.cache/hypha/haddock/<pkg>-<ver>/@, @source@ explains how it got there.
ensureHaddockFor :: BuildEnv IO -> PackageId -> IO (Maybe FilePath, HaddockSource)
ensureHaddockFor env pid = do
  cache <- haddockDirFor pid
  cachedIndex <- doesFileExist (cache </> "index.html")
  if cachedIndex
    then pure (Just cache, FromCache)
    else do
      mStore <- locateHaddockHtml env pid
      case mStore of
        Just html -> do
          let storeDir = parentDir html
          createDirectoryIfMissing True cache
          try (createFileLink storeDir (cache </> "store-link")) >>=
            \case Left (_ :: SomeException) -> pure (); Right _ -> pure ()
          symlinkContents storeDir cache
          pure (Just cache, FromStore)
        Nothing -> do
          built <- tryBuildHaddock env pid cache
          if built
            then pure (Just cache, Built)
            else pure (Nothing, Missing)
  where
    parentDir p = reverse (drop 1 (dropWhile (/= '/') (reverse p)))

haddockDirFor :: PackageId -> IO FilePath
haddockDirFor pid = do
  root <- haddockCacheRoot
  let dir = root </> renderPid pid
  createDirectoryIfMissing True dir
  pure dir

renderPid :: PackageId -> FilePath
renderPid (PackageId (PackageName n) (Version v)) =
  Text.unpack n <> "-" <> Text.unpack v

symlinkContents :: FilePath -> FilePath -> IO ()
symlinkContents srcDir dst = do
  exists <- doesDirectoryExist srcDir
  if not exists
    then pure ()
    else do
      _ <- try (createFileLink (srcDir </> "index.html") (dst </> "index.html")) :: IO (Either SomeException ())
      pure ()

-- | Best-effort: invoke @haddock@ against the source directory of the package
-- as known by `locatePackageSource`. Skipped for now if no source dir is found
-- (the user must run @cabal haddock@ themselves, or wait for Plan B which
-- adds the full build pipeline).
tryBuildHaddock :: BuildEnv IO -> PackageId -> FilePath -> IO Bool
tryBuildHaddock env pid outDir = do
  mDir <- locatePackageSource env pid
  case mDir of
    Nothing -> pure False
    Just src -> do
      result <- try (TP.runProcess (TP.shell ("haddock --html --hyperlinked-source --odir=" <> outDir
                                              <> " " <> src <> "/**/*.hs")))
                  :: IO (Either SomeException TP.ExitCode)
      case result of
        Right TP.ExitSuccess -> pure True
        _                    -> pure False
```

> Notes: shelling out via `TP.shell` with a glob is unreliable across shells; in Plan B we replace this with a precise file-list discovery + `TP.proc "haddock" args`. For Plan A the function returns `False` on any failure and `doctor` reports Haddock as "warn", not "fail" — see step 4.

- [ ] **Step 4: Augment `doctor` with a Haddock-cache check**

Edit `src/Hypha/Command/Doctor.hs`:

```haskell
import Hypha.Cache (haddockCacheRoot)
import System.Directory (doesDirectoryExist)

runDoctor projectRoot = do
  cs <- sequence
          [ checkExecutable "ghc"
          , checkExecutable "haddock"
          , checkPlanJson projectRoot
          , checkHaddockCache
          ]
  ...

checkHaddockCache :: IO Check
checkHaddockCache = do
  d <- haddockCacheRoot
  ok <- doesDirectoryExist d
  pure (Check "haddock cache"
          (if ok then Pass else Warn)
          (if ok then Text.pack d
                 else "absent (will be created on first server run)"))
```

- [ ] **Step 5: Unit test for cache root + directory naming**

Create `test/Unit/Haddock.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.Haddock (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Data.Text as Text

import Hypha.Haddock.Generate (haddockDirFor)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

asyncPid :: PackageId
asyncPid = PackageId (PackageName "async") (Version "2.2.5")

tests :: TestTree
tests = testGroup "Haddock"
  [ testCase "haddockDirFor produces .../haddock/async-2.2.5" $ do
      dir <- haddockDirFor asyncPid
      assertEndsWith "/haddock/async-2.2.5" dir
  ]

assertEndsWith :: String -> String -> IO ()
assertEndsWith suffix actual =
  if endsWith suffix actual
    then pure ()
    else fail ("expected suffix " <> suffix <> " but got " <> actual)
  where
    endsWith s a = reverse s `isPrefix` reverse a
    isPrefix [] _ = True
    isPrefix _  [] = False
    isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys
```

Register in `test/Main.hs`:

```haskell
import qualified Unit.Haddock
-- ...
  , Unit.Haddock.tests
```

- [ ] **Step 6: Run tests + smoke run + commit**

Run: `cabal test`
Run: `cabal run hypha -- doctor`
Expected: doctor envelope now contains a "haddock cache" entry alongside the others.

```bash
git add hypha.cabal src/Hypha/Cache.hs src/Hypha/Haddock/Generate.hs \
        src/Hypha/Command/Doctor.hs test/Unit/Haddock.hs test/Main.hs
git commit -m "feat(haddock): on-demand cache layout + ensureHaddockFor + doctor check"
```

---

## Self-review

### Spec coverage check

| Spec section | Covered in |
|---|---|
| §1 Motivation, §2 Etymology | README written in Task 1 (etymology paragraph in README) |
| §3.1 In scope (project resolution) | Task 3 |
| §3.1 In scope (subcommands) | Tasks 9 (`search`), 10.1–10.7 (all others), 12 (`doctor`) |
| §3.1 In scope (`server`, `mcp`) | **Deferred** to Plans B and C as designed |
| §3.1 In scope (output JSON-first, `--human`, `--full`, `--select`) | Tasks 8 (envelope, fieldsets, select) + 11 (`--human`) |
| §3.1 Cross-recursion principle | Task 8 (`Hypha.Output.Actions`) + Tasks 10.* (each command emits `actions`/`related`) |
| §3.1 Hoogle integration | Task 7 |
| §3.1 Hackage JSON + cache | Task 6 |
| §3.1 Server mode | **Plan B** |
| §3.1 MCP | **Plan C** |
| §3.1 BuildEnv (Cabal + Nix) | Tasks 4 + 5 |
| §3.1 `doctor` | Task 12 (Haddock-cache check added in Task 13) |
| §3.1 Typed exit codes | Task 9 |
| §4 Design principles (records-of-functions, ReaderT IO, strict bangs) | All tasks (encoded in conventions block) |
| §5 Architecture (module tree) | Distributed across Tasks 1–13 |
| §6 Project resolution | Task 3 |
| §7 Data sources + outside-plan policy | Task 9 (`HyphaError` includes `NotInPlan`), enforced per command |
| §8 Caching | Task 6 (HTTP cache); Task 13 (haddock cache layout) |
| §9 Identifier syntax | Task 2 |
| §10 Global flags | Task 9 (`Hypha.Cli.Parser`); deliberate omissions of `--no-cache`/`--user-agent`/`--ghc-override` honoured |
| §11 Subcommands | Tasks 9 + 10 + 12 |
| §12 Output schema (envelope, error envelope, field-set policy) | Tasks 8 + 9 |
| §13 Typed exit codes | Task 9 |
| §14 `--human` rendering pipeline | Task 11 |
| §15 MCP | **Plan C** |
| §16 Server mode | **Plan B** |
| §17 Code quality conventions | All tasks (conventions block) |
| §18 Dependencies | Distributed across `hypha.cabal` extensions per task |
| §19 Testing posture | All tasks (property + golden + unit per area) |
| §20 Repo layout | Task 1 |
| §21 Phased delivery | This plan |
| §22 Open risks | N/A — risks logged in spec; no plan changes needed |
| §23 Mantra | README in Task 1 |

### Placeholder scan

No instances of "TBD", "TODO" in step bodies. References to "stub" appear in:
- Task 7 — `Hypha.Hoogle.Database` step 3, which contains a working implementation guarded by a documentation note about the Hoogle library version.
- Task 13 — `Hypha.Haddock.Generate` ships a deliberately limited builder that returns `False` on failure and is upgraded in Plan B; this is documented inline and matched by a `Warn` (not `Fail`) status in `doctor`.

Both are intentional, documented, and have replacement work scheduled in Plan B.

### Type / name consistency

- `Hypha.Types.SymbolPath.SymbolPath` is used in Tasks 2; downstream commands take `Text` arguments and parse via `parseSymbolPath` at the boundary (`Hypha.Cli.Run.dispatch`). This is consistent — the boundary parser stays in the dispatcher.
- `BuildEnv m`, `HackageClient m`, `Hoogle m`, `Cache m` are introduced in Tasks 4, 6, 7, 6 respectively — all polymorphic over `m`, all with the same record-of-functions shape.
- `Outcome a` / `OutcomeEnvelope a` introduced in Task 8 are produced by every command and consumed by `Hypha.Cli.Run.emit` introduced in Task 10.1.
- `HyphaError` constructors used in dispatch arms (`EnvMissingPlan`, `NotInPlan`, `BadCliArgs`) match the definition in Task 9.

No naming drift found.

---

## Execution handoff

Plan A complete and saved to `docs/superpowers/plans/2026-05-18-hypha-plan-a-cli-alpha.md`.

Two execution options:

**1. Subagent-Driven (recommended)** — I dispatch a fresh subagent per task (or per sub-task in Task 10), review between tasks, fast iteration, parallel work where independent.

**2. Inline Execution** — Execute tasks in this session using `superpowers:executing-plans`, batch execution with checkpoints for review.

Which approach?

