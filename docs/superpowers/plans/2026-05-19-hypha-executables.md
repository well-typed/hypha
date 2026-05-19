# Hypha Executables Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Index every cabal `executable NAME` stanza of every package in the build plan as a first-class browsable component in `hypha server` — own sidebar entry (`pkg:exe:name`), own URL, own search-index rows — without breaking the existing sub-library plumbing.

**Architecture:** Extend `ComponentInfo` with a kind tag (`MainLib | SubLib | Exe`). Reuse the existing component-aware indexer, cache, sidebar renderer, and handler dispatch — only the key formatter, the cabal-parse step, and the sidebar tag styling change. The composite cache key gains a third form: `pkg:exe:name` (distinct from `pkg:sublib`).

**Tech Stack:** Haskell (GHC 9.6+), `Cabal-syntax 3.10+` (already on the build via the sublib work), `lucid2`, `servant`, `sqlite-simple`, `tasty`/`tasty-hunit`/`falsify`.

---

## File Structure

**Modified files:**
- `src/Hypha/Project/Components.hs` — replace `ciSublib :: Maybe Text` with `ciKind :: ComponentKind`; rename `parseLibComponents` → `parseComponents` and walk `condExecutables`.
- `src/Hypha/Types/ComponentName.hs` — replace `cnSublib :: Maybe Text` with `cnKind :: ComponentKind`; teach `parseComponentName` / `renderComponentName` about the `pkg:exe:name` form.
- `src/Hypha/Command/Server.hs` — adapt `componentKey`, `componentNames`, `componentsForUnit`, `hydrateFromCache`, and `resolveComponentDirs` to the kind tag.
- `src/Hypha/Server/Ui/Tree.hs` — render an `:exe:name` suffix in a new `.exe-tag` span.
- `ui/css/components/tree.css` — add `.exe-tag` rule alongside `.sublib-tag`.
- `test/Property/ComponentName.hs` — extend round-trip property + unit cases for the exe form.
- `test/Unit/Components.hs` — extend fixture assertions.
- `test/fixtures/cabal/nike.cabal` — extend with two `executable` stanzas.
- `test/Golden/golden/server-home.html` — regenerate if the sidebar fixture changes.

**No new files.**

---

### Task 1: Pivot `ComponentInfo` from `Maybe Text` to `ComponentKind`

**Files:**
- Modify: `src/Hypha/Project/Components.hs`
- Modify: `src/Hypha/Command/Server.hs` (call sites)

This task only renames the field; we still emit `MainLib` / `SubLib`
and skip executables.  Splitting the rename from the new behaviour
keeps the diff readable and lets tests pass at every step.

- [ ] **Step 1: Update `ComponentInfo` to carry a kind sum**

In `src/Hypha/Project/Components.hs`, replace the `ComponentInfo`
data type plus the helper `toComponent`:

```haskell
-- | The kind of library or executable component we discovered in a
-- cabal file.  'MainLib' represents the unnamed @library@ stanza;
-- 'SubLib' is a named @library NAME@ stanza; 'Exe' is an
-- @executable NAME@ stanza.
data ComponentKind
  = MainLib
  | SubLib !Text
  | Exe    !Text
  deriving stock (Show, Eq, Ord)

-- | One library or executable component of a package.
data ComponentInfo = ComponentInfo
  { ciKind         :: !ComponentKind
  , ciHsSourceDirs :: ![FilePath]
    -- ^ Absolute paths.  Falls back to the package root when the
    -- stanza omits @hs-source-dirs@ (cabal default).
  }
  deriving stock (Show, Eq)
```

Replace the body of `parseLibComponents`'s helper:

```haskell
    toComponent kind bi =
      let raw  = map UP.getSymbolicPath (PD.hsSourceDirs bi)
          dirs = if null raw
                   then [pkgRoot]
                   else map (pkgRoot </>) raw
      in ComponentInfo kind dirs
```

And update its callers in the same function:

```haskell
        Just gpd ->
          let mainComp =
                [ toComponent MainLib
                    (PD.libBuildInfo (PD.condTreeData ct))
                | ct <- maybe [] (:[]) (PD.condLibrary gpd)
                ]
              subComps =
                [ toComponent (SubLib (Text.pack (UC.unUnqualComponentName n)))
                    (PD.libBuildInfo (PD.condTreeData ct))
                | (n, ct) <- PD.condSubLibraries gpd
                ]
          in pure (mainComp ++ subComps)
```

Update the module export list to expose `ComponentKind (..)`:

```haskell
module Hypha.Project.Components
  ( ComponentInfo (..)
  , ComponentKind (..)
  , parseLibComponents
  , findCabalFile
  ) where
```

- [ ] **Step 2: Update call sites in `Hypha.Command.Server`**

Three references to `Comp.ciSublib` exist; switch each to the matching
shape via `Comp.ciKind`.  In `componentsForUnit`:

```haskell
componentsForUnit
  :: BuildPlan -> PackageId -> FilePath
  -> IO [(Comp.ComponentKind, [FilePath])]
componentsForUnit plan pid d =
  case lookupUnit (pkgName pid) plan of
    Just pu | not (null (puLibComponents pu)) ->
      pure
        [ (Comp.ciKind c, Comp.ciHsSourceDirs c)
        | c <- puLibComponents pu
        ]
    _ -> do
      roots <- chooseSourceRoots d
      pure [(Comp.MainLib, roots)]
```

In `componentKey`, replace the `Maybe Text` formatter with one that
takes a `ComponentKind`:

```haskell
-- | Compute the cache key for one library or executable component.
--   * 'MainLib' → bare package name.
--   * 'SubLib s' → @pkg:s@.
--   * 'Exe s'    → @pkg:exe:s@.
componentKey :: Text -> Comp.ComponentKind -> Text
componentKey pkgT Comp.MainLib     = pkgT
componentKey pkgT (Comp.SubLib s)  = pkgT <> ":" <> s
componentKey pkgT (Comp.Exe    s)  = pkgT <> ":exe:" <> s
```

In `componentNames`:

```haskell
componentNames :: BuildPlan -> PackageId -> [Text]
componentNames plan pid =
  let pkgT = unPackageName (pkgName pid)
  in case lookupUnit (pkgName pid) plan of
       Just pu | not (null (puLibComponents pu)) ->
         [ componentKey pkgT (Comp.ciKind c) | c <- puLibComponents pu ]
       _ -> [pkgT]
```

In `hydrateFromCache.componentSublibs` (rename it to
`componentKinds` while you're there):

```haskell
    componentKinds :: BuildPlan -> PackageId -> IO [Comp.ComponentKind]
    componentKinds p pid =
      case lookupUnit (pkgName pid) p of
        Just pu | not (null (puLibComponents pu)) ->
          pure [ Comp.ciKind c | c <- puLibComponents pu ]
        _ -> pure [Comp.MainLib]
```

And in the body:

```haskell
      kinds <- componentKinds plan pid
      case kinds of
        []  -> go (pid : missing) rest
        _   -> do
          let keys = [ componentKey pkgT k | k <- kinds ]
          ...
```

`resolveComponentDirs` (still operating on the old `Maybe Text` shape
in the previous file): update the matching predicate to compare
`Comp.ciKind c == cnKind cn` once Task 2 lands.  For this task, leave
it temporarily comparing on a derived helper:

```haskell
componentKindOf :: Comp.ComponentInfo -> Comp.ComponentKind
componentKindOf = Comp.ciKind
```

and update the predicate in `resolveComponentDirs` to use it (the
sublib variant only — Task 2 widens the surface):

```haskell
          let mDirs = case lookupUnit (cnPackage cn) plan of
                Just pu | not (null (puLibComponents pu)) ->
                  case [ Comp.ciHsSourceDirs c
                       | c <- puLibComponents pu
                       , componentKindOf c == cnKindOfCN cn ] of
                    (xs : _) -> Just xs
                    []       -> Nothing
                _ -> Nothing
```

Define `cnKindOfCN` as a temporary shim that maps the old
`ComponentName { cnSublib }` shape onto `ComponentKind`:

```haskell
cnKindOfCN :: ComponentName -> Comp.ComponentKind
cnKindOfCN cn = case cnSublib cn of
  Nothing -> Comp.MainLib
  Just s  -> Comp.SubLib s
```

Task 2 collapses this shim once `ComponentName` itself learns about
`ComponentKind`.

- [ ] **Step 3: Build to confirm everything compiles**

```bash
~/.ghcup/bin/cabal build all
```

Expected: PASS.

- [ ] **Step 4: Run the test suite**

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: all 99 tests pass — behaviour unchanged.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Project/Components.hs src/Hypha/Command/Server.hs
git commit -m "refactor(project): ComponentInfo carries ComponentKind, not Maybe Text"
```

---

### Task 2: Teach `ComponentName` about `ComponentKind`

**Files:**
- Modify: `src/Hypha/Types/ComponentName.hs`
- Modify: `test/Property/ComponentName.hs`
- Modify: `src/Hypha/Command/Server.hs` (drop the `cnKindOfCN` shim)

- [ ] **Step 1: Write the failing extension to the property test**

In `test/Property/ComponentName.hs`, replace the body with:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Property.ComponentName (tests) where

import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import Test.Falsify.Property (gen, assert)

import Hypha.Project.Components (ComponentKind (..))
import Hypha.Types.ComponentName
  ( ComponentName (..), parseComponentName, renderComponentName )
import Hypha.Types.PackageId (PackageName (..))

tests :: TestTree
tests = testGroup "Property.ComponentName"
  [ testCase "parses simple package" $
      parseComponentName "nike"
        @?= ComponentName (PackageName "nike") MainLib
  , testCase "parses pkg:sublib" $
      parseComponentName "nike:lib-breakdown"
        @?= ComponentName (PackageName "nike") (SubLib "lib-breakdown")
  , testCase "parses pkg:exe:name" $
      parseComponentName "nike:exe:nike-cli"
        @?= ComponentName (PackageName "nike") (Exe "nike-cli")
  , testCase "renders main lib" $
      renderComponentName (ComponentName (PackageName "nike") MainLib)
        @?= "nike"
  , testCase "renders sublib" $
      renderComponentName
        (ComponentName (PackageName "nike") (SubLib "lib-breakdown"))
        @?= "nike:lib-breakdown"
  , testCase "renders exe" $
      renderComponentName
        (ComponentName (PackageName "nike") (Exe "nike-cli"))
        @?= "nike:exe:nike-cli"
  , testCase "empty sublib suffix collapses to MainLib" $
      parseComponentName "nike:"
        @?= ComponentName (PackageName "nike") MainLib
  , testCase "empty exe suffix collapses to MainLib" $
      parseComponentName "nike:exe:"
        @?= ComponentName (PackageName "nike") MainLib
  , testCase "exe disambiguation: pkg:foo is sublib, pkg:exe:foo is exe" $ do
      let a = parseComponentName "pkg:foo"
          b = parseComponentName "pkg:exe:foo"
      renderComponentName a @?= "pkg:foo"
      renderComponentName b @?= "pkg:exe:foo"
  , testProperty "render . parse . render = render (all three kinds)" $ do
      pkg  <- gen (Gen.elem (pure "nike" <> pure "containers" <> pure "happy"))
      kind <- gen (Gen.elem
                     (   pure MainLib
                      <> pure (SubLib "lib-foo")
                      <> pure (Exe "foo")))
      let cn   = ComponentName (PackageName (Text.pack pkg)) kind
          got  = renderComponentName
                   (parseComponentName (renderComponentName cn))
          want = renderComponentName cn
      assert $ P.eq P..$ ("want", want) P..$ ("got", got)
  ]
```

- [ ] **Step 2: Run the test suite to see the new cases fail**

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -8
```

Expected: FAIL — `ComponentName` constructor expects `Maybe Text`,
not `ComponentKind`.

- [ ] **Step 3: Rewrite `Hypha.Types.ComponentName`**

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | A cabal-style component reference: a package name with an
-- optional sub-library or executable qualifier.
--
-- Encoded form:
--
-- * @nike@                 — main library.
-- * @nike:lib-breakdown@   — sub-library @lib-breakdown@.
-- * @nike:exe:nike-cli@    — executable @nike-cli@.
--
-- The composite form is what cabal-install uses on the command line,
-- and what we put in the @pkg@ column of the SQLite search cache so
-- sublibs and executables don't need a schema migration.
module Hypha.Types.ComponentName
  ( ComponentName (..)
  , parseComponentName
  , renderComponentName
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Project.Components (ComponentKind (..))
import Hypha.Types.PackageId    (PackageName (..))

-- | Reference to a single library or executable component.
data ComponentName = ComponentName
  { cnPackage :: !PackageName
  , cnKind    :: !ComponentKind
  }
  deriving stock (Show, Eq, Ord)

-- | Split a textual reference on @:@.  The grammar is:
--
-- * @pkg@               → 'MainLib'
-- * @pkg:exe:name@      → 'Exe name'
-- * @pkg:name@          → 'SubLib name'
--
-- Empty suffixes (@pkg:@, @pkg:exe:@) collapse to 'MainLib' so they
-- round-trip cleanly with the bare @pkg@ form.
parseComponentName :: Text -> ComponentName
parseComponentName raw =
  case Text.breakOn ":" raw of
    (pkg, rest)
      | Text.null rest -> ComponentName (PackageName pkg) MainLib
      | otherwise      ->
          let afterColon = Text.drop 1 rest
          in case Text.stripPrefix "exe:" afterColon of
               Just exeName
                 | Text.null exeName -> ComponentName (PackageName pkg) MainLib
                 | otherwise         -> ComponentName (PackageName pkg) (Exe exeName)
               Nothing
                 | Text.null afterColon -> ComponentName (PackageName pkg) MainLib
                 | otherwise            -> ComponentName (PackageName pkg) (SubLib afterColon)

-- | Inverse of 'parseComponentName'.
renderComponentName :: ComponentName -> Text
renderComponentName (ComponentName (PackageName p) MainLib)    = p
renderComponentName (ComponentName (PackageName p) (SubLib s)) = p <> ":" <> s
renderComponentName (ComponentName (PackageName p) (Exe    s)) = p <> ":exe:" <> s
```

- [ ] **Step 4: Drop the temporary shim in `Hypha.Command.Server`**

Delete `cnKindOfCN` and `componentKindOf` from
`Hypha.Command.Server.hs`.  Update `resolveComponentDirs` to compare
on `cnKind` directly:

```haskell
          let mDirs = case lookupUnit (cnPackage cn) plan of
                Just pu | not (null (puLibComponents pu)) ->
                  case [ Comp.ciHsSourceDirs c
                       | c <- puLibComponents pu
                       , Comp.ciKind c == cnKind cn ] of
                    (xs : _) -> Just xs
                    []       -> Nothing
                _ -> Nothing
          case mDirs of
            Just dirs -> pure (Just (d, dirs))
            Nothing | cnKind cn == Comp.MainLib -> do
              roots <- chooseSourceRoots d
              pure (Just (d, roots))
            Nothing -> pure Nothing
```

- [ ] **Step 5: Build + test**

```bash
~/.ghcup/bin/cabal build all
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: all tests pass — the new property cases are green and the
existing 99 still pass (sublibs untouched).

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Types/ComponentName.hs test/Property/ComponentName.hs \
        src/Hypha/Command/Server.hs
git commit -m "feat(types): ComponentName carries ComponentKind (incl. exe:)"
```

---

### Task 3: Walk `condExecutables` in the cabal parser

**Files:**
- Modify: `src/Hypha/Project/Components.hs`
- Modify: `test/fixtures/cabal/nike.cabal`
- Modify: `test/Unit/Components.hs`

- [ ] **Step 1: Extend the fixture cabal with two executables**

Edit `test/fixtures/cabal/nike.cabal` and append:

```cabal

executable nike-cli
  default-language: Haskell2010
  hs-source-dirs:   app
  main-is:          Main.hs
  build-depends:    base

executable wrap
  default-language: Haskell2010
  hs-source-dirs:   app/wrap
  main-is:          Main.hs
  build-depends:    base
```

- [ ] **Step 2: Extend the failing unit test**

Replace the assertions in `test/Unit/Components.hs` to expect five
components:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.Components (tests) where

import Data.List (sort)
import qualified Data.Text as Text
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Project.Components
  ( ComponentInfo (..), ComponentKind (..), parseLibComponents )

tests :: TestTree
tests = testGroup "Unit.Components"
  [ testCase "parses main lib + two sublibs + two exes from fixture" $ do
      let root  = "test" </> "fixtures" </> "cabal"
          cabal = root </> "nike.cabal"
      comps <- parseLibComponents cabal root
      let summary =
            sort [ ( renderKind (ciKind c)
                   , sort (ciHsSourceDirs c)
                   )
                 | c <- comps
                 ]
      summary @?=
        [ ( "exe:nike-cli", [root </> "app"] )
        , ( "exe:wrap",     [root </> "app/wrap"] )
        , ( "lib",          [root </> "src"] )
        , ( "sublib:bench",    [root </> "bench-src"] )
        , ( "sublib:internal", [root </> "internal-src"] )
        ]
  , testCase "missing cabal file returns []" $ do
      res <- parseLibComponents "/does/not/exist.cabal" "/does/not"
      res @?= []
  ]
  where
    renderKind :: ComponentKind -> String
    renderKind MainLib     = "lib"
    renderKind (SubLib s)  = "sublib:" <> Text.unpack s
    renderKind (Exe    s)  = "exe:"    <> Text.unpack s
```

- [ ] **Step 3: Run, confirm the new case fails**

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -15
```

Expected: FAIL — `parseLibComponents` only emits three entries.

- [ ] **Step 4: Walk executables in `parseLibComponents`**

In `src/Hypha/Project/Components.hs`, extend the parser body:

```haskell
        Just gpd ->
          let mainComp =
                [ toComponent MainLib
                    (PD.libBuildInfo (PD.condTreeData ct))
                | ct <- maybe [] (:[]) (PD.condLibrary gpd)
                ]
              subComps =
                [ toComponent (SubLib (Text.pack (UC.unUnqualComponentName n)))
                    (PD.libBuildInfo (PD.condTreeData ct))
                | (n, ct) <- PD.condSubLibraries gpd
                ]
              exeComps =
                [ toComponent (Exe (Text.pack (UC.unUnqualComponentName n)))
                    (PD.buildInfo (PD.condTreeData ct))
                | (n, ct) <- PD.condExecutables gpd
                ]
          in pure (mainComp ++ subComps ++ exeComps)
```

Note: executables use the record selector `buildInfo` (from
`Distribution.Types.Executable`), not `libBuildInfo`.

- [ ] **Step 5: Run tests; expect green**

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: all 100+ tests pass.

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Project/Components.hs \
        test/Unit/Components.hs test/fixtures/cabal/nike.cabal
git commit -m "feat(project): parseComponents also walks condExecutables"
```

---

### Task 4: Sidebar renders an `.exe-tag` span

**Files:**
- Modify: `src/Hypha/Server/Ui/Tree.hs`
- Modify: `ui/css/components/tree.css`

- [ ] **Step 1: Extend the sidebar renderer**

Replace the body of `Hypha.Server.Ui.Tree`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Tree
  ( packageTree
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

-- | Sidebar package list linking to each component's overview page.
-- Entries are composite names of the form @pkg@, @pkg:sublib@, or
-- @pkg:exe:name@; the trailing tag is rendered in a muted span and
-- the URL has its @:@ percent-encoded.
packageTree :: [Text] -> Html ()
packageTree = ul_ [class_ "tree"] . mconcat . map renderEntry
  where
    renderEntry :: Text -> Html ()
    renderEntry compName =
      let (pkgPart, tail_) = case Text.breakOn ":" compName of
            (a, b) | Text.null b -> (a, Nothing)
                   | otherwise   -> (a, Just (Text.drop 1 b))
          (kindCls, suffix, hrefSuffix) = case tail_ of
            Nothing  -> ("", Nothing, "")
            Just t   -> case Text.stripPrefix "exe:" t of
              Just e  -> ("exe-tag",    Just (":exe:" <> e), "%3Aexe%3A" <> e)
              Nothing -> ("sublib-tag", Just (":"     <> t), "%3A"       <> t)
          hrefText = pkgPart <> hrefSuffix
      in li_ $ a_ [href_ ("/pkg/" <> hrefText)] $ do
           toHtml pkgPart
           case suffix of
             Nothing -> pure ()
             Just s  -> span_ [class_ kindCls] (toHtml s)
```

- [ ] **Step 2: Add the `.exe-tag` rule to the stylesheet**

Append to `ui/css/components/tree.css`:

```css

/* Executable tag rendered after the parent package name in the
 * sidebar (e.g. ":exe:nike-cli").  Reuses the muted treatment of
 * .sublib-tag but borrows the secondary accent so executables stay
 * visually distinct from sub-libraries at a glance. */
.tree li a .exe-tag {
  color: var(--accent-2);
  font-size: 0.85em;
  margin-left: 0.1em;
}
```

- [ ] **Step 3: Regenerate the golden home-page fixture**

```bash
~/.ghcup/bin/cabal test all --test-options="--accept" 2>&1 | tail -5
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: tests pass (with golden updated or unchanged depending on
whether the fixture project has any exes).

- [ ] **Step 4: Commit**

```bash
git add src/Hypha/Server/Ui/Tree.hs ui/css/components/tree.css \
        test/Golden/golden/server-home.html 2>/dev/null || true
git commit -m "feat(server): sidebar renders :exe:name tag for executables"
```

---

### Task 5: Live smoke + final test sweep

**Files:** none (verification only).

- [ ] **Step 1: Clear the search cache so we re-index from scratch**

```bash
rm -f ~/.cache/hypha/hypha.db*
```

- [ ] **Step 2: Boot the server, wait for indexing**

```bash
~/.ghcup/bin/cabal run hypha -- server --bind 127.0.0.1:4315 >/tmp/exes.log 2>&1 &
SVR=$!
sleep 30
```

- [ ] **Step 3: Inspect the sidebar for `:exe:` entries**

```bash
curl -s http://127.0.0.1:4315/ | grep -oE '/pkg/[A-Za-z0-9_-]+%3Aexe%3A[A-Za-z0-9_-]+' | sort -u | head -10
```

Expected: at least the local plan's executables (e.g. `happy:exe:happy`,
`alex:exe:alex`, `hsc2hs:exe:hsc2hs`).  Empty output is acceptable
only if the local plan ships zero executables — verify with
`grep '^executable' *.cabal`.

- [ ] **Step 4: Inspect one executable's overview page**

```bash
curl -s 'http://127.0.0.1:4315/pkg/happy%3Aexe%3Ahappy' \
  | grep -oE '<h1>[^<]+</h1>|class="module-list"' | head -5
```

Expected: `<h1>happy:exe:happy</h1>` plus a `module-list` class.

- [ ] **Step 5: Tear down the server**

```bash
kill $SVR
wait 2>/dev/null
```

- [ ] **Step 6: Run the full suite one final time**

```bash
~/.ghcup/bin/cabal test all 2>&1 | tail -5
```

Expected: all tests pass.

- [ ] **Step 7: Final commit (only if any deterministic golden
  changes still need committing)**

```bash
git status --short
# If the only modified file is test/Golden/golden/server-home.html,
# commit it; otherwise skip this step.
git add test/Golden/golden/server-home.html 2>/dev/null || true
git commit -m "test(golden): refresh server-home for exe sidebar entries" \
  --allow-empty
```

---

## Self-Review

**Spec coverage:**

- **Component discovery gains kind tag** → Tasks 1 + 3.
- **Composite name grammar (`pkg:exe:name`)** → Task 2.
- **Disambiguation safeguard** → Task 2 (test case
  `exe disambiguation: pkg:foo is sublib, pkg:exe:foo is exe`).
- **Indexer + cache + handlers** → Task 1 (all key formatting goes
  through the new `ComponentKind`-aware `componentKey`).
- **Sidebar UI with `.exe-tag` styling** → Task 4.
- **Property + unit tests** → Tasks 2 + 3.
- **Manual smoke** → Task 5.

**Placeholder scan:** no "TBD"/"TODO"; every step has the code or
command it needs.

**Type consistency:** `ComponentKind`, `ComponentInfo`, `ciKind`,
`ciHsSourceDirs`, `cnPackage`, `cnKind`, `componentKey`,
`componentNames`, `componentsForUnit`, `componentKinds`,
`resolveComponentDirs` appear identically across every task.  The
`cnKindOfCN`/`componentKindOf` shim introduced in Task 1 is explicitly
deleted in Task 2 Step 4.

**Risk note:** because Task 1 is a pure rename, executable rows do
not start populating the cache until Task 3 lands.  Tests stay green
the whole time because no test asserts the presence of exe rows
until Task 3 introduces the fixture extension.
