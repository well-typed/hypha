# Cross-Package Re-Exports Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `hypha server` show a symbol under the package that publishes it — `base:Data.Traversable.mapAccumL`, not only `ghc-internal:GHC.Internal.Data.Traversable.mapAccumL` — in search and in per-package browsing.

**Architecture:** Re-export resolution currently stops at the component boundary and the indexer discards every export whose definition lives outside it. Resolution now crosses one hop into the owning dependency: units are indexed dependency-first, threading an `ExportEnv` built from the rows already produced, and a definition site is identified by `(component, module)` rather than by module alone. Module pages and symbol cards take the same hop by loading the owning dependency's sources on demand.

**Tech Stack:** Haskell, `cabal`, `ghc-lib-parser`, `sqlite-simple`, `tasty` (HUnit / Golden / Falsify). No new package dependencies.

**Spec:** `docs/superpowers/specs/2026-07-28-cross-package-reexports-design.md`
**Issue:** <https://gitlab.well-typed.com/well-typed/hypha/-/issues/11>

## Global Constraints

Every task's requirements implicitly include this section.

- **Repo root is the working directory.** Do not use `.worktrees/agent-N/`; this branch (`adinapoli/more-server-improvements`) is being worked directly.
- **The toolchain is hidden by the sandbox.** `ghc` and `cabal` live in `~/.ghcup/bin`. Any build or test command needs `dangerouslyDisableSandbox: true` and `PATH=$HOME/.ghcup/bin:$PATH`.
- **Build command:** `PATH=$HOME/.ghcup/bin:$PATH cabal build all`
- **Test command:** `PATH=$HOME/.ghcup/bin:$PATH cabal test all`
- **Single-suite run:** `PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "<pattern>"'`
- **Git identity is not configured in this environment.** Every commit must pass it inline:
  ```
  git -c user.name="Alfredo Di Napoli" -c user.email="alfredo@well-typed.com" commit -m "..."
  ```
- **Commit message trailer** on every commit:
  ```
  Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
  ```
- **Every new test module must be added to `other-modules:` in the `hypha-tests` stanza of `hypha.cabal`** and imported plus listed in `test/Main.hs`. A test module that is not wired in compiles nowhere and runs never.
- **Strict bangs on every field** of every `data`/`newtype` introduced here. A lazy field needs a one-line comment saying why.
- **No `error`, `undefined`, or partial record.** Boundary failures go through `Hypha.Error.HyphaError` or a typed result.
- **Never swallow an error branch.** `Left _ -> pure fallback` and `_err` are banned. Every dropped export, ambiguous choice, and unresolvable owner is traced to stderr.
- **Do not stringify a domain type at a call site.** `ComponentKey`, `ModulePath`, `SymbolName`, `Signature`, `DefinitionRef` travel as themselves; `Text` conversion happens in the rendering layer.
- **Conventional Commits:** `feat:`, `fix:`, `test:`, `docs:`, `chore:`.
- **Imports minimal and sorted**, matching the surrounding module's style.

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `src/Hypha/Search/Index.hs` | Row types, free of storage and of the builder | Add `DefinitionRef`; `rowDefModule` → `rowDefinition`; `currentIndexFormat` 2 → 3 |
| `src/Hypha/Search/Exports.hs` | **New.** What already-indexed components export, and choosing among candidates | Create |
| `src/Hypha/Search/Cache.hs` | SQLite row storage | `def_pkg` column, migration, decode |
| `src/Hypha/Search/Indexer.hs` | Building rows, component discovery, the index pass | Consume the env; report outside exports; dependency-first pass |
| `src/Hypha/Search/Collapse.hs` | Ranking and collapsing presentations | Group across components; fix `definitionHref` |
| `src/Hypha/Search/Reexport.hs` | Per-component resolution | Add `outsideModulesFor` |
| `src/Hypha/Types/BuildPlan.hs` | Plan queries | Add `topologicalOrder` |
| `src/Hypha/Project/Components.hs` | Cabal component parsing | Add `moduleOwner` |
| `src/Hypha/Source/Extract.hs` | Module-page entries from parse trees (pure) | `EntryOrigin` carries a `DefinitionRef`; accept imported sources |
| `src/Hypha/Source/Locate.hs` | Definition location (IO over given sources) | Accept imported sources |
| `src/Hypha/Command/Server.hs` | HTTP wiring; the only place that does the IO hop | Thread env; load imported sources for pages and cards |
| `src/Hypha/Server/Ui/Search.hs` | Search result rendering | Show the definition's component |
| `src/Hypha/Server/Ui/ModuleDoc.hs` | Module page rendering | Link a cross-package origin to its own package |
| `test/fixtures/reexport-dep/` | **New.** A second fixture package, re-exported by the first | Create |
| `test/Util/Row.hs` | Row constructors for tests | Add `rowFrom` |
| `test/Util/Fixture.hs` | Fixture loaders | Add `depSources` |
| `test/Unit/SearchExports.hs` | **New.** `ExportEnv` coverage | Create |
| `test/Unit/BuildPlanOrder.hs` | **New.** `topologicalOrder` coverage | Create |
| `test/Property/BuildPlanOrder.hs` | **New.** `topologicalOrder` permutation property | Create |

---

### Task 1: `DefinitionRef` — a definition site names its component

A definition site identified by module alone is not an identity: two packages can expose the same module name. `Collapse.definitionHref` already reads the consequence wrongly, building the definition link from the *presentation's* component and the *definition's* module. This task is a type change plus that fix; no cross-package rows exist yet, so behaviour is otherwise unchanged.

**Files:**
- Modify: `src/Hypha/Search/Index.hs:63-88`
- Modify: `src/Hypha/Search/Cache.hs:70-80`, `:214-237`, `:262-283`
- Modify: `src/Hypha/Search/Indexer.hs:305-324`
- Modify: `src/Hypha/Search/Collapse.hs:43-55`, `:102-131`, `:141-147`
- Modify: `src/Hypha/Server/Ui/Search.hs:108-116`
- Modify: `test/Util/Row.hs`
- Modify: `test/Unit/SearchCollapse.hs`, `test/Unit/SearchIndexCache.hs`, `test/Unit/SearchIndexBuild.hs:60-70`, `test/Unit/Server.hs:90-110`

**Interfaces:**
- Produces:
  ```haskell
  -- Hypha.Search.Index
  data DefinitionRef = DefinitionRef
    { drComponent :: !ComponentKey
    , drModule    :: !ModulePath
    }
    deriving stock (Show, Eq, Ord)

  data IndexRow = IndexRow
    { rowComponent  :: !ComponentKey
    , rowModule     :: !ModulePath
    , rowName       :: !SymbolName
    , rowSignature  :: !Signature
    , rowDefinition :: !DefinitionRef
    , rowVisibility :: !Visibility
    }

  currentIndexFormat :: Int   -- now 3

  -- Hypha.Search.Collapse
  srDefinition :: SymbolResult -> DefinitionRef

  -- test/Util/Row.hs
  rowFrom :: Text -> Text -> Text -> Text -> DefinitionRef -> Visibility -> IndexRow
  ```

- [ ] **Step 1: Write the failing test**

In `test/Unit/SearchCollapse.hs`, add to the imports:

```haskell
import Hypha.Search.Index (DefinitionRef (..), IndexRow, Visibility (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Search.Collapse
  ( SearchResult (..), SymbolResult (..), collapseRows, definitionHref, resultHref )
import Util.Row (rowFrom, rowIn)
```

and add this test case to the `testGroup` list:

```haskell
  , testCase "the definition link names the defining component, not the presenting one" $
      -- base presents mapAccumL; ghc-internal defines it.  Building the
      -- link from the presentation's component would point at a module
      -- base does not have.
      case collapse [ rowFrom "base" "Data.Traversable" "mapAccumL" "sig"
                        (DefinitionRef (ComponentKey "ghc-internal")
                                       (ModulePath "GHC.Internal.Data.Traversable"))
                        Exposed
                    ] of
        [ResultSymbol s] ->
          definitionHref s
            @?= "/pkg/ghc-internal/GHC.Internal.Data.Traversable/mapAccumL"
        _ -> fail "expected one result"
```

Also update the existing `mapRow`-based assertion at line 36 from `srDefModule s @?= ModulePath "Data.Map.Strict.Internal"` to:

```haskell
          drModule (srDefinition s) @?= ModulePath "Data.Map.Strict.Internal"
```

- [ ] **Step 2: Run the test to verify it fails**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "SearchCollapse"'
```

Expected: compile failure — `Data constructor not in scope: DefinitionRef`, `Variable not in scope: rowFrom`.

- [ ] **Step 3: Add `DefinitionRef` to `Hypha.Search.Index`**

Export it (`DefinitionRef (..)`) and replace the `rowDefModule` field. Replace the existing `IndexRow` haddock and definition (lines 63-79) with:

```haskell
-- | One search-index entry.
--
-- 'rowDefinition' is where the symbol is actually declared: the row's own
-- component and module for a local declaration, another module of the
-- component for an intra-package re-export, and another /component/ for a
-- cross-package one.  Carrying the component is what lets search collapse
-- @base:Data.Traversable.mapAccumL@ into the same result as
-- @ghc-internal:GHC.Internal.Data.Traversable.mapAccumL@ without also
-- merging two unrelated packages that happen to expose a module of the
-- same name.
--
-- A module alone was never an identity, which is why this is a pair.
data DefinitionRef = DefinitionRef
  { drComponent :: !ComponentKey
  , drModule    :: !ModulePath
  }
  deriving stock (Show, Eq, Ord)

data IndexRow = IndexRow
  { rowComponent  :: !ComponentKey
  , rowModule     :: !ModulePath
  , rowName       :: !SymbolName
  , rowSignature  :: !Signature
  , rowDefinition :: !DefinitionRef
  , rowVisibility :: !Visibility
  }
  deriving stock (Show, Eq, Ord)
```

Bump the format and extend its comment:

```haskell
-- | Bumped whenever a row's meaning changes.
--
-- Generation 1 rows are not migrated but discarded: their module names
-- may have come from file paths and their signatures may have been
-- resolved by symbol name, and neither defect is detectable per row.
-- Generation 2 rows go the same way for the same reason: a stored
-- @def_mod@ cannot be attributed to a component after the fact.  The only
-- honest options are to re-index or to lie.
currentIndexFormat :: Int
currentIndexFormat = 3
```

- [ ] **Step 4: Store the definition component**

In `src/Hypha/Search/Cache.hs`, after the two existing `migrateAddColumn` calls for `pkg_index` (line 78), add:

```haskell
  migrateAddColumn conn "pkg_index" "def_pkg"    "TEXT NOT NULL DEFAULT ''"
```

Add the column to the `CREATE TABLE` list (after `def_mod`, line 121):

```haskell
    \  , def_pkg TEXT NOT NULL \
```

Extend `rowColumns`:

```haskell
rowColumns :: Text
rowColumns = "pkg, mod, name, sig, def_mod, def_pkg, visibility"
```

Replace `fromStored` with the seven-column version. An empty `def_pkg` can only reach us from a writer that is not this build, since `ensureIndexFormat` clears older generations on open — so it is an anomaly to report, defaulted to the row's own component:

```haskell
fromStored :: (Text, Text, Text, Text, Text, Text, Text) -> (IndexRow, Maybe Text)
fromStored (pkg, modPath, name, sig, defMod, defPkg, vis) =
  ( IndexRow
      { rowComponent  = ComponentKey pkg
      , rowModule     = ModulePath modPath
      , rowName       = SymbolName name
      , rowSignature  = Signature sig
      , rowDefinition = DefinitionRef (ComponentKey definingPkg) (ModulePath defMod)
      , rowVisibility = maybe Internal id (visibilityFromText vis)
      }
  , case (visibilityFromText vis, Text.null defPkg) of
      (Just _,  False) -> Nothing
      (Nothing, _)     -> Just
        (pkg <> "/" <> modPath <> ": unrecognised visibility " <> Text.pack (show vis))
      (Just _,  True)  -> Just
        (pkg <> "/" <> modPath <> ": no definition component; assuming " <> pkg)
  )
  where
    definingPkg = if Text.null defPkg then pkg else defPkg
```

Extend the insert (line 262-283) — the tuple gains a field and the SQL a column:

```haskell
      let expanded =
            [ ( unComponentKey (rowComponent r)
              , ver
              , unModulePath (rowModule r)
              , unSymbolName (rowName r)
              , unSignature (rowSignature r)
              , unModulePath (drModule (rowDefinition r))
              , unComponentKey (drComponent (rowDefinition r))
              , visibilityToText (rowVisibility r)
              )
            | r <- rows
            ]
      executeMany (icConn c)
        "INSERT INTO pkg_index \
        \  (pkg, version, mod, name, sig, def_mod, def_pkg, visibility) \
        \VALUES (?,?,?,?,?,?,?,?)"
        expanded
```

Update the `Hypha.Search.Index` import at line 42 to bring in `DefinitionRef (..)`.

- [ ] **Step 5: Update the indexer's row construction**

In `src/Hypha/Search/Indexer.hs`, replace line 311 (`rowDefModule = defMod`) with:

```haskell
          , rowDefinition = DefinitionRef compKey defMod
```

Every row this function builds is still same-component, so the key is `compKey`. Add `DefinitionRef (..)` to the `Hypha.Search.Index` import list at line 45-46.

- [ ] **Step 6: Update collapse and the search UI**

In `src/Hypha/Search/Collapse.hs`, rename the field and re-key:

```haskell
data SymbolResult = SymbolResult
  { srComponent  :: !ComponentKey
  , srModule     :: !ModulePath
    -- ^ The presentation the user lands on: the most public module that
    -- exposes this definition.
  , srName       :: !SymbolName
  , srSignature  :: !Signature
  , srDefinition :: !DefinitionRef
  , srAlternates :: !Int
    -- ^ How many other presentations were folded in.  Rendered as a small
    -- affordance linking the definition site, so nothing is hidden.
  }
  deriving stock (Show, Eq)
```

```haskell
symbolKey :: IndexRow -> (ComponentKey, DefinitionRef, SymbolName)
symbolKey r = (rowComponent r, rowDefinition r, rowName r)
```

```haskell
  , srDefinition = rowDefinition r
```

```haskell
-- | Where the @+N@ affordance points: the definition site, so the escape
-- hatch out of a collapsed group is one click.
--
-- The component comes from the definition, not from the presentation: a
-- re-export can cross a package boundary, and @\/pkg\/base\/GHC.Internal…@
-- is a module @base@ does not have.
definitionHref :: SymbolResult -> Text
definitionHref s =
  "/pkg/" <> unComponentKey (drComponent (srDefinition s))
    <> "/" <> unModulePath (drModule (srDefinition s))
    <> "/" <> unSymbolName (srName s)
```

Import `DefinitionRef (..)` from `Hypha.Search.Index`.

In `src/Hypha/Server/Ui/Search.hs:114`, replace `unModulePath (srDefModule s)` with `unModulePath (drModule (srDefinition s))` and add `DefinitionRef (..)` to its `Hypha.Search.Index` import (adding the import if the module does not already have one).

- [ ] **Step 7: Update the test helpers and the remaining call sites**

`test/Util/Row.hs` — keep `rowIn`'s arity and same-component meaning, add the cross-component constructor:

```haskell
module Util.Row
  ( row
  , rowIn
  , rowFrom
  ) where

import Hypha.Search.Index (DefinitionRef (..), IndexRow (..), Visibility (..))

-- | A locally-declared, exposed row.
row :: Text -> Text -> Text -> Text -> IndexRow
row comp modPath name sig = rowIn comp modPath name sig modPath Exposed

-- | A row defined in another module of the /same/ component.
rowIn :: Text -> Text -> Text -> Text -> Text -> Visibility -> IndexRow
rowIn comp modPath name sig defMod =
  rowFrom comp modPath name sig
    (DefinitionRef (ComponentKey comp) (ModulePath defMod))

-- | A row whose definition may live in another component.
rowFrom :: Text -> Text -> Text -> Text -> DefinitionRef -> Visibility -> IndexRow
rowFrom comp modPath name sig def vis = IndexRow
  { rowComponent  = ComponentKey comp
  , rowModule     = ModulePath modPath
  , rowName       = SymbolName name
  , rowSignature  = Signature sig
  , rowDefinition = def
  , rowVisibility = vis
  }
```

Then fix the four remaining references the compiler will point at:
- `test/Unit/SearchIndexCache.hs:40` → `map (drModule . rowDefinition) rows @?= [ModulePath "Data.Map.Strict.Internal"]`
- `test/Unit/SearchIndexBuild.hs:65` → `[ drModule (rowDefinition r) | ... ]`
- `test/Unit/Server.hs:96,104` → `srDefinition = DefinitionRef (ComponentKey "<same component as srComponent>") (ModulePath "…")`

- [ ] **Step 8: Run the full suite**

```
PATH=$HOME/.ghcup/bin:$PATH cabal build all && PATH=$HOME/.ghcup/bin:$PATH cabal test all
```

Expected: PASS, including the new `definitionHref` case. No golden file should change — no rendered output depends on the field name.

- [ ] **Step 9: Commit**

```bash
git add src/Hypha/Search/Index.hs src/Hypha/Search/Cache.hs \
        src/Hypha/Search/Indexer.hs src/Hypha/Search/Collapse.hs \
        src/Hypha/Server/Ui/Search.hs test/Util/Row.hs \
        test/Unit/SearchCollapse.hs test/Unit/SearchIndexCache.hs \
        test/Unit/SearchIndexBuild.hs test/Unit/Server.hs
git -c user.name="Alfredo Di Napoli" -c user.email="alfredo@well-typed.com" \
  commit -m "refactor(search): identify a definition site by component and module

A ModulePath alone is not an identity -- two packages can expose the same
module name -- and definitionHref already read the consequence wrongly,
building the link from the presenting component and the defining module.
IndexRow.rowDefModule becomes rowDefinition :: DefinitionRef, the cache
gains a def_pkg column, and the index format goes to 3.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: `topologicalOrder` — index dependencies first

`ghc-internal`'s rows must exist before `base`'s are built. This is the ordering primitive; nothing consumes it yet.

**Files:**
- Modify: `src/Hypha/Types/BuildPlan.hs:14-22` (exports), append the function
- Create: `test/Unit/BuildPlanOrder.hs`
- Create: `test/Property/BuildPlanOrder.hs`
- Modify: `hypha.cabal` (`other-modules:` of `hypha-tests`)
- Modify: `test/Main.hs`

**Interfaces:**
- Produces:
  ```haskell
  -- Hypha.Types.BuildPlan
  topologicalOrder :: BuildPlan -> [PackageId] -> [PackageId]
  ```

- [ ] **Step 1: Write the failing unit test**

Create `test/Unit/BuildPlanOrder.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for the order the indexer walks units in.
--
-- A component's cross-package re-exports resolve against the rows its
-- dependencies already produced, so "dependencies first" is a correctness
-- requirement, not a performance one.
module Unit.BuildPlanOrder (tests) where

import qualified Data.Map.Strict as Map

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan
  , topologicalOrder )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

pid :: Text -> PackageId
pid n = PackageId (PackageName n) (Version "1.0")

unit :: Text -> [Text] -> (PackageName, PlannedUnit)
unit n deps =
  ( PackageName n
  , PlannedUnit
      { puId            = pid n
      , puDeps          = map pid deps
      , puIsLocal       = False
      , puOrigin        = OriginDistribution
      , puSrcDir        = Nothing
      , puDistDir       = Nothing
      , puLibComponents = []
      }
  )

-- | base -> ghc-internal -> ghc-prim, the shape this work exists for.
plan :: BuildPlan
plan = emptyBuildPlan
  { bpUnits = Map.fromList
      [ unit "base"         ["ghc-internal", "ghc-prim"]
      , unit "ghc-internal" ["ghc-prim"]
      , unit "ghc-prim"     []
      ]
  }

names :: [PackageId] -> [Text]
names = map (unPackageName . pkgName)

tests :: TestTree
tests = testGroup "Unit.BuildPlanOrder"
  [ testCase "dependencies come before dependents" $
      names (topologicalOrder plan [pid "base", pid "ghc-internal", pid "ghc-prim"])
        @?= ["ghc-prim", "ghc-internal", "base"]

  , testCase "the input order does not decide the result" $
      names (topologicalOrder plan [pid "ghc-prim", pid "base", pid "ghc-internal"])
        @?= ["ghc-prim", "ghc-internal", "base"]

  , testCase "a dependency absent from the input does not constrain the order" $
      -- ghc-internal is already cached, so it is not in the list.  base
      -- must still come out, and ghc-prim before it.
      names (topologicalOrder plan [pid "base", pid "ghc-prim"])
        @?= ["ghc-prim", "base"]

  , testCase "a unit the plan does not know is kept" $
      assertBool "unknown unit present"
        (pid "mystery" `elem` topologicalOrder plan [pid "base", pid "mystery"])

  , testCase "a cycle yields every unit exactly once" $ do
      let cyclic = emptyBuildPlan
            { bpUnits = Map.fromList [ unit "a" ["b"], unit "b" ["a"] ] }
          out = topologicalOrder cyclic [pid "a", pid "b"]
      length out @?= 2
      assertBool "a present" (pid "a" `elem` out)
      assertBool "b present" (pid "b" `elem` out)
  ]
```

Add `import Data.Text (Text)` to that module's imports.

- [ ] **Step 2: Wire the module in and run to verify it fails**

Add `Unit.BuildPlanOrder` to `other-modules:` in the `hypha-tests` stanza of `hypha.cabal`. In `test/Main.hs` add `import qualified Unit.BuildPlanOrder` beside the other `Unit.` imports, and `Unit.BuildPlanOrder.tests` to the test list.

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "BuildPlanOrder"'
```

Expected: compile failure — `Variable not in scope: topologicalOrder`.

- [ ] **Step 3: Implement `topologicalOrder`**

Add `topologicalOrder` to the `-- * Queries` export block of `src/Hypha/Types/BuildPlan.hs`, add `import Data.List (foldl')` and `import Data.Set qualified as Set` (matching the module's existing `qualified` style — it uses `import qualified Data.Map.Strict as Map`, so write `import qualified Data.Set as Set`), and append:

```haskell
-- | The given units, dependencies before dependents.
--
-- Only edges /within/ the input matter: a dependency that is already
-- cached is not in the list and has no order to constrain.  Each distinct
-- unit is emitted exactly once, and nothing is ever dropped — a unit the
-- plan does not know has no edges and a unit inside a cycle is emitted
-- when its own traversal returns.  That makes a cycle produce an
-- arbitrary but total order rather than a hang, which matters because
-- a plan is only a DAG by construction, not by type.
--
-- The indexer needs this because a component's cross-package re-exports
-- resolve against the rows its dependencies already produced.
topologicalOrder :: BuildPlan -> [PackageId] -> [PackageId]
topologicalOrder bp pids = reverse (snd (foldl' visit (Set.empty, []) pids))
  where
    -- Restricting to the input is what makes the "already cached" case
    -- work: an edge to a unit we are not indexing is not an edge.
    wanted = Set.fromList pids

    visit (seen, acc) pid
      | pid `Set.member` seen = (seen, acc)
      | otherwise =
          -- Marked before recursing, so a back edge terminates.
          let (seen', acc') = foldl' visit (Set.insert pid seen, acc) (depsOf pid)
          in (seen', pid : acc')

    depsOf pid = case Map.lookup (pkgName pid) (bpUnits bp) of
      Nothing -> []
      Just u  -> [ d | d <- puDeps u, d `Set.member` wanted ]
```

- [ ] **Step 4: Run the unit test to verify it passes**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "BuildPlanOrder"'
```

Expected: PASS, 5 cases.

- [ ] **Step 5: Add the permutation property**

Create `test/Property/BuildPlanOrder.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | The ordering must never lose or invent a unit.  A silently dropped
-- unit is a package that simply never gets indexed, with nothing in the
-- output to say so.
module Property.BuildPlanOrder (tests) where

import           Data.Containers.ListUtils (nubOrd)
import           Data.List (sort)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import           Test.Falsify.Property (gen, assert)
import qualified Test.Falsify.Range as Range

import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan
  , topologicalOrder )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

pid :: Text -> PackageId
pid n = PackageId (PackageName n) (Version "1.0")

-- | Names drawn from a small alphabet so dependency edges actually hit.
genName :: Gen.Gen Text
genName = do
  i <- Gen.inRange (Range.between (0, 9 :: Int))
  pure (Text.pack ("p" <> show i))

genUnit :: Gen.Gen (PackageName, PlannedUnit)
genUnit = do
  n    <- genName
  deps <- Gen.list (Range.between (0, 4)) genName
  pure
    ( PackageName n
    , PlannedUnit
        { puId            = pid n
        , puDeps          = map pid deps
        , puIsLocal       = False
        , puOrigin        = OriginDistribution
        , puSrcDir        = Nothing
        , puDistDir       = Nothing
        , puLibComponents = []
        }
    )

tests :: TestTree
tests = testGroup "Property.BuildPlanOrder"
  [ testProperty "the order is a permutation of the distinct input" $ do
      units <- gen (Gen.list (Range.between (0, 10)) genUnit)
      inputs <- gen (Gen.list (Range.between (0, 10)) genName)
      let bp  = emptyBuildPlan { bpUnits = Map.fromList units }
          ins = map pid inputs
      assert $ P.eq
        .$ ("ordered", sort (topologicalOrder bp ins))
        .$ ("input",   sort (nubOrd ins))
  ]
```

If the `Test.Falsify` import shape here does not match this repo's, copy the import block and assertion style verbatim from `test/Property/SearchRanking.hs` and adapt — that file is the reference for how falsify is used here.

- [ ] **Step 6: Wire it in and run**

Add `Property.BuildPlanOrder` to `other-modules:` and to `test/Main.hs`.

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "BuildPlanOrder"'
```

Expected: PASS, 5 unit cases plus 1 property.

- [ ] **Step 7: Commit**

```bash
git add src/Hypha/Types/BuildPlan.hs test/Unit/BuildPlanOrder.hs \
        test/Property/BuildPlanOrder.hs test/Main.hs hypha.cabal
git -c user.name="Alfredo Di Napoli" -c user.email="alfredo@well-typed.com" \
  commit -m "feat(plan): order units dependencies-first

A component's cross-package re-exports resolve against the rows its
dependencies already produced, so the index pass has to walk the plan in
dependency order.  Cycle-safe and loses nothing: a unit the plan does not
know, or one inside a cycle, still comes out exactly once.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: `Hypha.Search.Exports` — what already-indexed components export

The lookup table a façade's exports resolve against. Built from `IndexRow`s rather than from parse trees, which buys transitivity: a dependency's rows already carry their own resolved `DefinitionRef`, so a chain through two packages lands on the true definition without any extra search.

**Files:**
- Create: `src/Hypha/Search/Exports.hs`
- Modify: `hypha.cabal` (`exposed-modules:` of the library, after `Hypha.Search.Collapse`)
- Create: `test/Unit/SearchExports.hs`
- Modify: `hypha.cabal` (`other-modules:` of `hypha-tests`), `test/Main.hs`

**Interfaces:**
- Consumes: `DefinitionRef (..)`, `IndexRow (..)` from Task 1.
- Produces:
  ```haskell
  -- Hypha.Search.Exports
  data Export = Export
    { exDefinition :: !DefinitionRef
    , exSignature  :: !Signature
    }

  data ExportChoice = ExportChoice
    { ecChosen   :: !Export
    , ecRejected :: ![DefinitionRef]
    }

  data ExportEnv                                    -- abstract
  emptyEnv     :: ExportEnv
  extendEnv    :: [IndexRow] -> ExportEnv -> ExportEnv
  envFromRows  :: [IndexRow] -> ExportEnv
  lookupExport :: Set PackageName -> ModulePath -> SymbolName -> ExportEnv
               -> Maybe ExportChoice
  ```

- [ ] **Step 1: Write the failing test**

Create `test/Unit/SearchExports.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for the cross-package export environment.
--
-- The dependency filter is the load-bearing part: two unrelated packages
-- can expose a module of the same name, and without the filter a facade in
-- one would resolve against the other.
module Unit.SearchExports (tests) where

import qualified Data.Set as Set

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Exports
  ( Export (..), ExportChoice (..), envFromRows, lookupExport )
import Hypha.Search.Index (DefinitionRef (..), IndexRow, Visibility (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))
import Util.Row (row, rowFrom)

-- | What ghc-internal contributes: mapAccumL declared where it is shown.
ghcInternalRows :: [IndexRow]
ghcInternalRows =
  [ row "ghc-internal" "GHC.Internal.Data.Traversable" "mapAccumL" "sigL" ]

deps :: Set.Set PackageName
deps = Set.fromList [PackageName "ghc-internal", PackageName "ghc-prim"]

tests :: TestTree
tests = testGroup "Unit.SearchExports"
  [ testCase "a dependency's export is found with its signature and definition" $
      case lookupExport deps (ModulePath "GHC.Internal.Data.Traversable")
             (SymbolName "mapAccumL") (envFromRows ghcInternalRows) of
        Just ch -> do
          exSignature (ecChosen ch) @?= Signature "sigL"
          exDefinition (ecChosen ch)
            @?= DefinitionRef (ComponentKey "ghc-internal")
                              (ModulePath "GHC.Internal.Data.Traversable")
          ecRejected ch @?= []
        Nothing -> fail "expected a hit"

  , testCase "a component that is not a dependency is invisible" $
      lookupExport (Set.fromList [PackageName "ghc-prim"])
        (ModulePath "GHC.Internal.Data.Traversable") (SymbolName "mapAccumL")
        (envFromRows ghcInternalRows)
        @?= Nothing

  , testCase "an export already re-exported by the dependency keeps its true definition" $
      -- ghc-internal's GHC.Internal.Data.List presents mapAccumL but
      -- GHC.Internal.Data.Traversable defines it.  A facade resolving
      -- through the List module must land on Traversable, not on List.
      case lookupExport deps (ModulePath "GHC.Internal.Data.List")
             (SymbolName "mapAccumL")
             (envFromRows
                [ rowFrom "ghc-internal" "GHC.Internal.Data.List" "mapAccumL" "sigL"
                    (DefinitionRef (ComponentKey "ghc-internal")
                                   (ModulePath "GHC.Internal.Data.Traversable"))
                    Exposed
                ]) of
        Just ch ->
          drModule (exDefinition (ecChosen ch))
            @?= ModulePath "GHC.Internal.Data.Traversable"
        Nothing -> fail "expected a hit"

  , testCase "two dependencies exposing the same module report the rejected one" $ do
      let env = envFromRows
            ( row "alpha" "Shared.Mod" "thing" "sigA"
            : row "beta"  "Shared.Mod" "thing" "sigB"
            : [] )
          both = Set.fromList [PackageName "alpha", PackageName "beta"]
      case lookupExport both (ModulePath "Shared.Mod") (SymbolName "thing") env of
        Just ch -> do
          -- Lexicographic on the component, so the winner does not depend
          -- on the order rows arrived in.
          drComponent (exDefinition (ecChosen ch)) @?= ComponentKey "alpha"
          ecRejected ch
            @?= [DefinitionRef (ComponentKey "beta") (ModulePath "Shared.Mod")]
        Nothing -> fail "expected a hit"

  , testCase "a name no dependency exports is a miss" $
      lookupExport deps (ModulePath "GHC.Internal.Data.Traversable")
        (SymbolName "notThere") (envFromRows ghcInternalRows)
        @?= Nothing
  ]
```

- [ ] **Step 2: Wire the test module in and run to verify it fails**

Add `Unit.SearchExports` to `other-modules:` and to `test/Main.hs`.

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "SearchExports"'
```

Expected: compile failure — `Could not find module 'Hypha.Search.Exports'`.

- [ ] **Step 3: Create the module**

Create `src/Hypha/Search/Exports.hs`:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | What the components indexed so far export, so a component's
-- cross-package re-exports have something to resolve against.
--
-- "Hypha.Search.Reexport" answers /within/ a component: a name no module
-- of the component declares resolves to 'DefinedOutside', naming the
-- import believed to supply it.  Until this module existed the indexer
-- discarded those, which is why @base@ — since GHC 9.10 almost entirely a
-- facade over @ghc-internal@ — contributed 308 rows where @ghc-internal@
-- contributed 1240, and why searching for @mapAccumL@ never found it in
-- @base@.
--
-- Built from 'IndexRow's rather than from parse trees, which buys
-- transitivity for nothing: a dependency's rows already carry their own
-- resolved 'DefinitionRef', so a chain through two packages lands on the
-- real definition without this module searching for it.
module Hypha.Search.Exports
  ( Export (..)
  , ExportChoice (..)
  , ExportEnv
  , emptyEnv
  , envFromRows
  , extendEnv
  , lookupExport
  ) where

import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import Hypha.Search.Index (DefinitionRef (..), IndexRow (..))
import Hypha.Types.ComponentName
  ( ComponentKey (..), ComponentName (..), parseComponentName )
import Hypha.Types.PackageId (PackageName)
import Hypha.Types.SymbolPath (ModulePath, Signature, SymbolName)

-- | One component's answer for a @(module, name)@ pair.
data Export = Export
  { exDefinition :: !DefinitionRef
  , exSignature  :: !Signature
  }
  deriving stock (Show, Eq)

-- | The chosen answer, plus the candidates it beat.
--
-- A choice among several is not a failure — the caller gets a usable
-- 'Export' either way — but it is worth reporting, so the rejected
-- candidates travel rather than being discarded at the point of choice.
data ExportChoice = ExportChoice
  { ecChosen   :: !Export
  , ecRejected :: ![DefinitionRef]   -- ^ empty when the choice was forced
  }
  deriving stock (Show, Eq)

-- | Every @(module, name)@ pair the indexed components present.
--
-- The value is a 'NonEmpty' because two components can expose a module of
-- the same name.  Collapsing that to one at insertion time would silently
-- pick a winner for a question only the /asking/ component can answer,
-- which is what 'lookupExport' is for.
newtype ExportEnv = ExportEnv (Map (ModulePath, SymbolName) (NonEmpty Export))
  deriving stock (Show, Eq)

emptyEnv :: ExportEnv
emptyEnv = ExportEnv Map.empty

envFromRows :: [IndexRow] -> ExportEnv
envFromRows rows = extendEnv rows emptyEnv

-- | Add one component's rows.  Called once per component per pass, so the
-- 'NonEmpty' lists grow with the number of components exposing a module
-- name, not with the number of times the pass runs.
extendEnv :: [IndexRow] -> ExportEnv -> ExportEnv
extendEnv rows (ExportEnv env) = ExportEnv (Map.unionWith (<>) added env)
  where
    added = Map.fromListWith (<>)
      [ ((rowModule r, rowName r), exportOf r :| []) | r <- rows ]

    exportOf r = Export
      { exDefinition = rowDefinition r
      , exSignature  = rowSignature r
      }

-- | The export a component should resolve @(module, name)@ to, given the
-- packages it depends on.
--
-- The dependency filter is what keeps two unrelated packages exposing a
-- module of the same name from resolving against each other.  A package
-- may also re-export from its own sub-libraries, so the caller is
-- expected to include the asking unit's own package name in the set.
--
-- Ties break lexicographically on the component, so the winner never
-- depends on the order components were indexed in.
lookupExport
  :: Set PackageName
  -> ModulePath
  -> SymbolName
  -> ExportEnv
  -> Maybe ExportChoice
lookupExport deps m n (ExportEnv env) = do
  candidates <- Map.lookup (m, n) env
  case sortOn (unComponentKey . drComponent . exDefinition)
         (NE.filter fromDep candidates) of
    []       -> Nothing
    (e : es) -> Just ExportChoice
      { ecChosen   = e
      , ecRejected = map exDefinition es
      }
  where
    fromDep e = packageOf (drComponent (exDefinition e)) `Set.member` deps
    packageOf = cnPackage . parseComponentName . unComponentKey
```

Add `Hypha.Search.Exports` to the library's `exposed-modules:` in `hypha.cabal`, after `Hypha.Search.Collapse`.

- [ ] **Step 4: Run the test to verify it passes**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "SearchExports"'
```

Expected: PASS, 5 cases.

- [ ] **Step 5: Commit**

```bash
git add src/Hypha/Search/Exports.hs test/Unit/SearchExports.hs \
        test/Main.hs hypha.cabal
git -c user.name="Alfredo Di Napoli" -c user.email="alfredo@well-typed.com" \
  commit -m "feat(search): add the cross-package export environment

What the components indexed so far export, keyed on (module, name), so a
facade component's DefinedOutside exports have something to resolve
against.  Candidates are filtered to the asking unit's dependencies, and
the rejected ones travel with the choice instead of being dropped at the
point of decision.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: the indexer writes rows for cross-package re-exports

The core fix. `indexParsedComponent` stops discarding `DefinedOutside` exports and resolves them through the env, reporting the ones it cannot.

**Files:**
- Create: `test/fixtures/reexport-dep/reexport-dep.cabal`
- Create: `test/fixtures/reexport-dep/src/Dep/Internal.hs`
- Create: `test/fixtures/reexport/src/Fixture/Imported.hs`
- Modify: `test/fixtures/reexport/reexport.cabal`
- Modify: `test/Util/Fixture.hs`
- Modify: `src/Hypha/Search/Indexer.hs:243-352`
- Modify: `test/Unit/SearchIndexBuild.hs`

**Interfaces:**
- Consumes: `ExportEnv`, `ExportChoice (..)`, `Export (..)`, `lookupExport`, `envFromRows` (Task 3); `DefinitionRef (..)` (Task 1).
- Produces:
  ```haskell
  -- Hypha.Search.Indexer
  data OutsideExport = OutsideExport
    { oeModule   :: !ModulePath
    , oeName     :: !SymbolName
    , oeExpected :: !ModulePath
    }

  data ComponentIndex = ComponentIndex
    { ciRows          :: ![IndexRow]
    , ciParseFailures :: ![(ModulePath, Parser.ParseError)]
    , ciNameMismatch  :: ![(ModulePath, ModulePath)]
    , ciUnresolved    :: ![OutsideExport]
    , ciAmbiguous     :: ![(OutsideExport, ExportChoice)]
    }

  indexComponentPure
    :: ComponentKey -> Set PackageName -> ExportEnv
    -> LanguageSettings -> [ModuleSource] -> ComponentIndex

  indexParsedComponent
    :: ComponentKey -> Set PackageName -> ExportEnv
    -> [(ModuleSource, Either Parser.ParseError ModuleInterface)]
    -> ComponentIndex
  ```

- [ ] **Step 1: Create the dependency fixture**

`test/fixtures/reexport-dep/reexport-dep.cabal`:

```
cabal-version: 2.4
name:          reexport-dep
version:       0.1.0
synopsis:      Dependency fixture: the package a reexport facade re-exports from

library
  hs-source-dirs:     src
  exposed-modules:    Dep.Internal
  default-language:   GHC2021
  build-depends:      base
```

`test/fixtures/reexport-dep/src/Dep/Internal.hs`:

```haskell
-- | Definition site in another package.  Mirrors
-- @GHC.Internal.Data.Traversable@, which is where @base@'s
-- @Data.Traversable@ exports actually live.
module Dep.Internal
  ( depThing
  , depUnused
  ) where

-- | Re-exported by @Fixture.Imported@ in the neighbouring fixture package.
depThing :: Int -> Int
depThing n = n + 1

-- | Exported here and by nobody else, so a cross-package pass cannot
-- claim it just because it is in scope.
depUnused :: Bool
depUnused = True
```

`test/fixtures/reexport/src/Fixture/Imported.hs` — the façade, shaped exactly like `base`'s `Data.Traversable`: an explicit export list, one open import, no declarations of its own:

```haskell
-- | A pure facade over another package.  The @base@ shape since GHC 9.10:
-- an explicit export list, one open import, and nothing declared here.
module Fixture.Imported
  ( depThing
  ) where

import Dep.Internal
```

Add `Fixture.Imported` to the `exposed-modules:` of `test/fixtures/reexport/reexport.cabal`.

- [ ] **Step 2: Add the fixture loader**

In `test/Util/Fixture.hs`, export `depSources` and add:

```haskell
-- | The @reexport-dep@ component: the package @Fixture.Imported@
-- re-exports from.  Kept separate from 'fixtureSources' because the point
-- of the fixture is that the two are different components.
depSources :: IO [ModuleSource]
depSources = sourcesFor
  [ ("test/fixtures/reexport-dep/src/Dep/Internal.hs", "Dep.Internal", Exposed) ]
```

Add `("test/fixtures/reexport/src/Fixture/Imported.hs", "Fixture.Imported", Exposed)` to the `fixtureSources` list.

- [ ] **Step 3: Write the failing test**

In `test/Unit/SearchIndexBuild.hs`, update the imports:

```haskell
import qualified Data.Set as Set

import Hypha.Search.Exports (envFromRows, emptyEnv)
import Hypha.Search.Index (DefinitionRef (..), IndexRow (..), Visibility (..))
import Hypha.Search.Indexer
  ( ComponentIndex (..), OutsideExport (..), indexComponentPure )
import Hypha.Types.PackageId (PackageName (..))
import Util.Fixture (depSources, fixtureSources, sourcesFor)
```

Every existing call to `indexComponentPure` in this file gains the two new arguments. The existing `fixture` helper becomes:

```haskell
-- | The fixture component with its dependency already indexed, which is
-- the state the real pass reaches by walking units dependency-first.
fixture :: IO ComponentIndex
fixture = do
  srcs <- fixtureSources
  dep  <- depIndex
  pure (indexComponentPure (ComponentKey "reexport") reexportDeps
          (envFromRows (ciRows dep)) defaultLanguageSettings srcs)

-- | The dependency component, indexed on its own with nothing before it.
depIndex :: IO ComponentIndex
depIndex = do
  srcs <- depSources
  pure (indexComponentPure (ComponentKey "reexport-dep")
          (Set.singleton (PackageName "reexport-dep")) emptyEnv
          defaultLanguageSettings srcs)

reexportDeps :: Set.Set PackageName
reexportDeps = Set.fromList [PackageName "reexport", PackageName "reexport-dep"]
```

The existing single-module call at line 49-51 becomes:

```haskell
      let ci = indexComponentPure (ComponentKey "reexport") reexportDeps emptyEnv
                 defaultLanguageSettings srcs
```

Add these cases to the `testGroup`:

```haskell
  , testCase "a symbol re-exported from another package gets a row" $ do
      ci <- fixture
      case rowsFor ci "depThing" of
        [r] -> do
          rowModule r     @?= ModulePath "Fixture.Imported"
          rowDefinition r @?= DefinitionRef (ComponentKey "reexport-dep")
                                            (ModulePath "Dep.Internal")
          -- The signature comes from the dependency's parse, not from a
          -- name-keyed guess inside this component.
          rowSignature r  @?= Signature "depThing :: Int -> Int"
        other -> fail ("expected one depThing row, got " <> show (length other))

  , testCase "a dependency symbol nobody re-exports gets no row here" $ do
      ci <- fixture
      rowsFor ci "depUnused" @?= []

  , testCase "an unresolvable cross-package export is reported, not dropped" $ do
      -- The same facade with an empty environment: the dependency has not
      -- been indexed, so there is no signature to give.  Silently
      -- producing nothing is what hid base for a whole release.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Imported.hs", "Fixture.Imported", Exposed) ]
      let ci = indexComponentPure (ComponentKey "reexport") reexportDeps emptyEnv
                 defaultLanguageSettings srcs
      ciRows ci @?= []
      ciUnresolved ci
        @?= [ OutsideExport
                { oeModule   = ModulePath "Fixture.Imported"
                , oeName     = SymbolName "depThing"
                , oeExpected = ModulePath "Dep.Internal"
                } ]

  , testCase "intra-package re-export rows are unchanged" $ do
      ci <- fixture
      let mods = sort (map (unModulePath . rowModule) (rowsFor ci "insertBag"))
      mods @?= [ "Fixture.Facade", "Fixture.Internal", "Fixture.Strict"
               , "Fixture.StrictInternal", "Fixture.Wrapper" ]
```

- [ ] **Step 4: Run to verify it fails**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "SearchIndexBuild"'
```

Expected: compile failure — `indexComponentPure` applied to too many arguments, `OutsideExport` not in scope.

- [ ] **Step 5: Extend `ComponentIndex`**

In `src/Hypha/Search/Indexer.hs`, replace the `ComponentIndex` block (lines 245-254) with:

```haskell
-- | An export the component does not define.
--
-- Named rather than tupled because all three fields are module paths or
-- close to it, and a bare triple at a report site is unreadable.
data OutsideExport = OutsideExport
  { oeModule   :: !ModulePath   -- ^ the module that exports it
  , oeName     :: !SymbolName
  , oeExpected :: !ModulePath   -- ^ the import we believe supplies it
  }
  deriving stock (Show, Eq, Ord)

-- | What indexing one component produced, and what it could not.
data ComponentIndex = ComponentIndex
  { ciRows          :: ![IndexRow]
  , ciParseFailures :: ![(ModulePath, Parser.ParseError)]
  , ciNameMismatch  :: ![(ModulePath, ModulePath)]
    -- ^ @(name the stanza expected, name the source declares)@.  Real in
    -- the wild, and silently trusting either side produces rows nobody
    -- can reach.
  , ciUnresolved    :: ![OutsideExport]
    -- ^ Exports whose definition lives outside the component and which no
    -- dependency's rows could supply.  A symbol missing from the index is
    -- invisible, and the user has no other way to find out.
  , ciAmbiguous     :: ![(OutsideExport, ExportChoice)]
    -- ^ Exports more than one dependency could have supplied.  Resolved,
    -- deterministically, and worth saying so.
  }
  deriving stock (Show, Eq)
```

Add to the export list: `OutsideExport (..)`, and to the imports:

```haskell
import Data.Set (Set)
import qualified Data.Set as Set

import Hypha.Search.Exports
  ( Export (..), ExportChoice (..), ExportEnv, lookupExport )
import qualified Hypha.Search.Exports as Exports
import Hypha.Search.Index
  (DefinitionRef (..), IndexRow (..), ModuleSource (..), Visibility (..))
import Hypha.Types.PackageId
```

- [ ] **Step 6: Resolve outside exports through the env**

Replace `indexComponentPure` and `indexParsedComponent` (lines 264-331), and delete `isDefinedOutside`:

```haskell
indexComponentPure
  :: ComponentKey
  -> Set PackageName          -- ^ the unit's dependencies, plus its own name
  -> ExportEnv                -- ^ what the components indexed so far export
  -> LanguageSettings
  -> [ModuleSource]
  -> ComponentIndex
indexComponentPure compKey deps env langs sources =
  indexParsedComponent compKey deps env
    [ (ms, Interface.parseInterface langs (msPath ms) (msContent ms))
    | ms <- sources
    ]

-- | The core, over parse results the caller obtained.
--
-- Parsing is the caller's job because it is where the exceptions are: see
-- 'Interface.parseInterfaceIO'.  Everything from here on is a function of
-- the sources and of what the dependencies exported.
indexParsedComponent
  :: ComponentKey
  -> Set PackageName
  -> ExportEnv
  -> [(ModuleSource, Either Parser.ParseError ModuleInterface)]
  -> ComponentIndex
indexParsedComponent compKey deps env parsed = ComponentIndex
  { ciRows          = localRows ++ outsideRows
  , ciParseFailures = failures
  , ciNameMismatch  = mismatches
  , ciUnresolved    = unresolved
  , ciAmbiguous     = ambiguous
  }
  where
    failures   = [ (msDeclaredName ms, e) | (ms, Left e)  <- parsed ]
    ok         = [ (ms, i)                | (ms, Right i) <- parsed ]
    mismatches =
      [ (msDeclaredName ms, miName i)
      | (ms, i) <- ok
      , msDeclaredName ms /= miName i
      ]

    ifaces = map snd ok

    -- Keyed on the name the source declares, which is the same key the
    -- resolution map uses.
    visibilityOf = Map.fromList [ (miName i, msVisibility ms) | (ms, i) <- ok ]
    ifaceOf      = Map.fromList [ (miName i, i)               | (_,  i) <- ok ]
    contentOf    = Map.fromList [ (miName i, msContent ms)    | (ms, i) <- ok ]

    resolved = Map.toList (Reexport.resolveComponent ifaces)

    visibilityFor presented = Map.findWithDefault Internal presented visibilityOf

    -- Exports this component declares, here or in a sibling module.
    localRows =
      [ IndexRow
          { rowComponent  = compKey
          , rowModule     = presented
          , rowName       = name
          , rowSignature  = sig
          , rowDefinition = DefinitionRef compKey defMod
          , rowVisibility = visibilityFor presented
          }
      | ((presented, name), res) <- resolved
      , defMod <- insideSite presented (resSite res)
      , Just defIface <- [Map.lookup defMod ifaceOf]
        -- The signature is read from the module the resolver landed on.
        -- Looking it up in a component-wide name map is what published
        -- Data.IntMap.Lazy.insertWith with Data.Map's signature.
      , Just decl <- [Parser.findDecl (unSymbolName name) (miDecls defIface)]
      , let src = Map.findWithDefault "" defMod contentOf
      , let sig = Signature (maybe "" id (Parser.declSigText src decl))
      ]

    -- Exports whose definition is in a dependency.  A site that names the
    -- asking module itself is 'Reexport's "no import plausibly supplies
    -- this" fallback, not a claim about a dependency: looking that up
    -- would match any dependency exposing a module of the same name, so it
    -- goes straight to the unresolved report.
    outside =
      [ OutsideExport presented name m
      | ((presented, name), DefinedOutside m) <- map (fmap resSite) resolved
      , m /= presented
      ]

    selfNamed =
      [ OutsideExport presented name m
      | ((presented, name), DefinedOutside m) <- map (fmap resSite) resolved
      , m == presented
      ]

    classified = [ (oe, lookupExport deps (oeExpected oe) (oeName oe) env)
                 | oe <- outside
                 ]

    outsideRows =
      [ IndexRow
          { rowComponent  = compKey
          , rowModule     = oeModule oe
          , rowName       = oeName oe
          , rowSignature  = exSignature (ecChosen ch)
          , rowDefinition = exDefinition (ecChosen ch)
          , rowVisibility = visibilityFor (oeModule oe)
          }
      | (oe, Just ch) <- classified
      ]

    unresolved = selfNamed ++ [ oe | (oe, Nothing) <- classified ]

    ambiguous =
      [ (oe, ch)
      | (oe, Just ch) <- classified
      , not (null (ecRejected ch))
      ]

-- | The definition module when it is inside this component; empty when it
-- is not.  A list rather than a 'Maybe' so it drops straight into the row
-- comprehension.
insideSite :: ModulePath -> DefinitionSite -> [ModulePath]
insideSite asking = \case
  DefinedHere      -> [asking]
  DefinedIn m      -> [m]
  DefinedOutside _ -> []
```

Add `import Data.Functor ((<&>))` only if needed; `fmap` over a tuple's second component is `Prelude`'s `Functor ((,) a)` instance and needs no import.

- [ ] **Step 7: Report the two new categories**

Replace `reportComponentIndex` (lines 337-352) with:

```haskell
-- | Trace what a component's index pass could not do.  Never silent: a
-- module or symbol missing from the index is invisible to search, and the
-- user has no other way to find out.
reportComponentIndex :: ComponentKey -> ComponentIndex -> IO ()
reportComponentIndex compKey ci = do
  mapM_ reportFailure    (ciParseFailures ci)
  mapM_ reportMismatch   (ciNameMismatch ci)
  mapM_ reportUnresolved (ciUnresolved ci)
  mapM_ reportAmbiguous  (ciAmbiguous ci)
  where
    label = Text.unpack (unComponentKey compKey)

    reportFailure (m, e) = hPutStrLn stderr $
      "hypha index: " <> label <> " skipped module "
        <> Text.unpack (unModulePath m) <> ": "
        <> Text.unpack (Parser.parseErrorMessage e)

    reportMismatch (declared, actual) = hPutStrLn stderr $
      "hypha index: " <> label <> " expected module "
        <> Text.unpack (unModulePath declared) <> " but its source declares "
        <> Text.unpack (unModulePath actual) <> "; using the latter"

    reportUnresolved oe = hPutStrLn stderr $
      "hypha index: " <> label <> " could not resolve "
        <> Text.unpack (unModulePath (oeModule oe)) <> "."
        <> Text.unpack (unSymbolName (oeName oe))
        <> " through " <> Text.unpack (unModulePath (oeExpected oe))
        <> "; no indexed dependency exports it"

    reportAmbiguous (oe, ch) = hPutStrLn stderr $
      "hypha index: " <> label <> " resolved "
        <> Text.unpack (unModulePath (oeModule oe)) <> "."
        <> Text.unpack (unSymbolName (oeName oe)) <> " to "
        <> renderRef (exDefinition (ecChosen ch)) <> ", rejecting "
        <> unwords (map renderRef (ecRejected ch))

    renderRef r =
      Text.unpack (unComponentKey (drComponent r)) <> ":"
        <> Text.unpack (unModulePath (drModule r))
```

- [ ] **Step 8: Keep the existing call site compiling**

`indexComponent` inside `buildAndCacheIndex` (line 177) calls `indexParsedComponent compKey parsed`. Task 5 threads the real env; for now pass the unit's dependency set and an empty env so the build stays green:

```haskell
      let ci       = indexParsedComponent compKey (dependencySet plan pid)
                       Exports.emptyEnv parsed
```

and add the helper, which Task 5 also uses:

```haskell
-- | The packages a unit may resolve a re-export through: its
-- dependencies, plus its own name, because a sub-library re-exporting
-- from the package's main library crosses a component boundary without
-- crossing a package one.
dependencySet :: BuildPlan -> PackageId -> Set PackageName
dependencySet plan pid = Set.insert (pkgName pid) $ case lookupUnit (pkgName pid) plan of
  Nothing -> Set.empty
  Just u  -> Set.fromList (map pkgName (puDeps u))
```

Export `dependencySet` from the module's `-- * Component discovery` block.

- [ ] **Step 9: Run the tests to verify they pass**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "SearchIndexBuild"'
```

Expected: PASS, including the four new cases.

Then the whole suite:

```
PATH=$HOME/.ghcup/bin:$PATH cabal test all
```

`Unit.SourceExtract` may now see `Fixture.Imported` in `fixtureSources`. If a count-based assertion there breaks, update the expected list to include it — do not remove the module from the fixture.

- [ ] **Step 10: Commit**

```bash
git add test/fixtures/reexport-dep test/fixtures/reexport \
        test/Util/Fixture.hs src/Hypha/Search/Indexer.hs \
        test/Unit/SearchIndexBuild.hs
git -c user.name="Alfredo Di Napoli" -c user.email="alfredo@well-typed.com" \
  commit -m "fix(search): index exports whose definition is in a dependency

The indexer discarded every export resolving to DefinedOutside, which for
a facade package means almost all of them: base contributed 308 rows
against ghc-internal's 1240, and no query could reach
base:Data.Traversable.mapAccumL.  Those exports now resolve through the
dependencies' rows, and the ones that cannot are reported rather than
dropped.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: the index pass walks dependencies first and threads the env

The pure core can now resolve across packages; this gives it something to resolve against.

**Files:**
- Modify: `src/Hypha/Search/Indexer.hs:87-186`
- Modify: `src/Hypha/Command/Server.hs:201-207`
- Modify: `test/Unit/SearchIndexCache.hs`

**Interfaces:**
- Consumes: `topologicalOrder` (Task 2); `ExportEnv`, `extendEnv`, `lookupExport` (Task 3); `dependencySet` (Task 4).
- Produces:
  ```haskell
  -- Hypha.Search.Indexer
  data Hydrated = Hydrated
    { hyEnv     :: !ExportEnv
    , hyMissing :: ![PackageId]
    }

  hydrateFromCache
    :: BuildPlan -> Cache.HyphaPackageCache -> [PackageId]
    -> IORef.IORef [Fuzzy.IndexedRow] -> IO Hydrated

  buildAndCacheIndex
    :: BuildPlan -> Cache.HyphaPackageCache -> PackageResolver IO
    -> ExportEnv -> [PackageId]
    -> IORef.IORef [Fuzzy.IndexedRow] -> IORef.IORef Int -> IO ()
  ```

- [ ] **Step 1: Write the failing test**

In `test/Unit/SearchIndexCache.hs`, add imports:

```haskell
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Hypha.Search.Exports (Export (..), ExportChoice (..), lookupExport)
import Hypha.Search.Indexer (Hydrated (..), hydrateFromCache)
import Hypha.Search.PackageCache
  ( CacheOrigin (..), openPackageCacheAt, writeCachedIndex )
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan )
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Hypha.Types.SymbolPath (Signature (..), SymbolName (..))
```

and this case:

```haskell
  , testCase "hydration hands back an environment the next unit can resolve against" $
      withSystemTempDirectory "hypha-hyd" $ \dir -> do
        c <- openPackageCacheAt (dir </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "ghc-internal" "9.1003.0"
          [ rowIn "ghc-internal" "GHC.Internal.Data.Traversable" "mapAccumL"
              "mapAccumL :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)"
              "GHC.Internal.Data.Traversable" Exposed
          ]
        let pid  = PackageId (PackageName "ghc-internal") (Version "9.1003.0")
            plan = emptyBuildPlan
              { bpUnits = Map.singleton (PackageName "ghc-internal") PlannedUnit
                  { puId            = pid
                  , puDeps          = []
                  , puIsLocal       = False
                  , puOrigin        = OriginDistribution
                  , puSrcDir        = Nothing
                  , puDistDir       = Nothing
                  , puLibComponents = []
                  }
              }
        ref <- IORef.newIORef []
        hyd <- hydrateFromCache plan c [pid] ref
        hyMissing hyd @?= []
        case lookupExport (Set.singleton (PackageName "ghc-internal"))
               (ModulePath "GHC.Internal.Data.Traversable")
               (SymbolName "mapAccumL") (hyEnv hyd) of
          Just ch -> exSignature (ecChosen ch)
            @?= Signature
                  "mapAccumL :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)"
          Nothing -> fail "cached rows did not reach the environment"
```

- [ ] **Step 2: Run to verify it fails**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "SearchIndexCache"'
```

Expected: compile failure — `Data constructor not in scope: Hydrated`; `hydrateFromCache` applied to the wrong result type.

- [ ] **Step 3: Return an environment from hydration**

In `src/Hypha/Search/Indexer.hs`, add `Hydrated (..)` to the exports and replace `hydrateFromCache` (lines 87-130) with:

```haskell
-- | What hydration recovered: the exports of every component it loaded,
-- and the units it could not.
--
-- The environment is returned rather than rebuilt later because the
-- background pass needs it before it indexes anything: a warm cache
-- holding @ghc-internal@ is exactly how @base@ becomes resolvable in a
-- run that only rebuilds @base@.
data Hydrated = Hydrated
  { hyEnv     :: !ExportEnv
  , hyMissing :: ![PackageId]
  }

-- | Pull every cached component index into the in-memory ref.  A unit
-- counts as "fully hydrated" only when /every/ one of its components
-- has cached rows; otherwise it's reported as missing so the
-- background indexer rebuilds the whole set.
hydrateFromCache
  :: BuildPlan
  -> Cache.HyphaPackageCache
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IO Hydrated
hydrateFromCache plan cache pids ref = go Exports.emptyEnv [] pids
  where
    go env missing [] = pure Hydrated
      { hyEnv     = env
      , hyMissing = reverse missing
      }
    go env missing (pid : rest) = do
      let pkgT  = unPackageName (pkgName pid)
          verT  = unVersion    (pkgVersion pid)
      kinds <- componentKinds plan pid
      case kinds of
        []  -> go env (pid : missing) rest
        _   -> do
          let keys = [ unComponentKey (componentKeyOf (PackageName pkgT) k)
                     | k <- kinds ]
          hits <- mapM (\k -> Cache.haveCachedIndex cache k verT) keys
          if and hits
            then do
              env' <- foldM (loadKey pid verT) env keys
              go env' missing rest
            else go env (pid : missing) rest

    loadKey pid verT env k = do
      rows <- Cache.readCachedIndex cache k verT
      let indexed = scorerRows (pkgName pid) (pkgVersion pid) rows
      indexed `seq`
        IORef.atomicModifyIORef' ref (\old -> (indexed ++ old, ()))
      pure (Exports.extendEnv rows env)

    -- | Just the component kinds for a unit, mirroring the
    -- structure 'componentsForUnit' would emit.  We avoid needing a
    -- source dir here because hydrate works off the cache alone.
    componentKinds :: BuildPlan -> PackageId -> IO [Comp.ComponentKind]
    componentKinds p pid =
      case lookupUnit (pkgName pid) p of
        Just pu | not (null (puLibComponents pu)) ->
          pure [ Comp.ciKind c | c <- puLibComponents pu ]
        _ -> pure [Comp.MainLib]
```

Add `import Control.Monad (foldM)`.

- [ ] **Step 4: Thread the environment through the build pass**

Replace `buildAndCacheIndex` (lines 140-186) with:

```haskell
-- | Walk the source trees of the given packages, extract their module
-- exports, persist the result to the cache, and prepend them to the
-- in-memory ref.  Packages whose source cannot be resolved are skipped,
-- with a reason on stderr — the index is a best-effort fallback, not a
-- silent one.
--
-- Units are walked dependencies-first and each component's rows extend
-- the environment the next one resolves against.  That order is a
-- correctness requirement: @base@ has no signature for @mapAccumL@ until
-- @ghc-internal@ has been indexed.
--
-- Per-module rows are built fully /outside/ the atomicModifyIORef'
-- critical section; prepending makes each insert O(|rows|) instead of
-- the O(|index|) behaviour of @old ++ rows@.
buildAndCacheIndex
  :: BuildPlan
  -> Cache.HyphaPackageCache
  -> PackageResolver IO
  -> ExportEnv                          -- ^ what the warm cache already supplies
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IORef.IORef Int                    -- ^ packages-done counter
  -> IO ()
buildAndCacheIndex plan cache resolver env0 pids ref doneRef =
  void (foldM indexUnit env0 (topologicalOrder plan pids))
  where
    -- Local + source-repository-package units land in the project DB;
    -- everything else (store packages) goes to the shared global DB.
    originFor :: PackageId -> CacheOrigin
    originFor pid = case lookupUnit (pkgName pid) plan of
      Just u | puIsLocal u -> OriginProject
      _                    -> OriginGlobal
    -- The done counter bumps once per /unit/, not per component, so
    -- the progress bar continues to read in package units.
    bump = IORef.atomicModifyIORef' doneRef (\n -> (n + 1, ()))

    indexUnit env pid = do
      eDir <- resolveSrc resolver pid
      case eDir of
        Left err -> do
          hPutStrLn stderr $
            "hypha index: no source for "
              <> Text.unpack (unPackageName (pkgName pid)) <> ": " <> show err
          bump
          pure env
        Right d -> do
          comps <- componentsForUnit plan pid d
          env'  <- foldM (indexComponent pid) env comps
          bump
          pure env'

    indexComponent pid env (kind, srcDirs) = do
      let pkgT    = unPackageName (pkgName    pid)
          verT    = unVersion    (pkgVersion pid)
          compKey = componentKeyOf (PackageName pkgT) kind
          langs   = languageSettingsFor plan pid kind
      sources <- componentModules plan pid kind srcDirs
      parsed  <- mapM (parseGuarded langs) sources
      let ci       = indexParsedComponent compKey (dependencySet plan pid) env parsed
          flatRows = ciRows ci
          indexed  = scorerRows (pkgName pid) (pkgVersion pid) flatRows
      reportComponentIndex compKey ci
      -- Persist before publishing into memory so a crash mid-stream
      -- never leaves the in-memory view ahead of the cache.
      Cache.writeCachedIndex cache (originFor pid)
        (unComponentKey compKey) verT flatRows
      indexed `seq`
        IORef.atomicModifyIORef' ref (\old -> (indexed ++ old, ()))
      pure (Exports.extendEnv flatRows env)
```

Add `import Control.Monad (foldM, void)` and `topologicalOrder` to the `Hypha.Types.BuildPlan` import (that module is imported unqualified at line 56, so the name is already in scope once exported — no import edit needed if the import has no explicit list; check and add if it does).

Note the `Left err` arm: the old code was `Left _ -> bump`, a silently skipped package. Reporting it is required by the global constraints.

- [ ] **Step 5: Update the server wiring**

In `src/Hypha/Command/Server.hs`, replace lines 201-207:

```haskell
  hyd <- Indexer.hydrateFromCache plan cache pids indexRef
  let missing = Indexer.hyMissing hyd
  IORef.writeIORef totalRef (length missing)
  case missing of
    [] -> IORef.writeIORef readyRef True
    _  -> do
      _ <- forkIO $ do
        r <- try (Indexer.buildAndCacheIndex plan cache resolver
                    (Indexer.hyEnv hyd) missing indexRef doneRef)
        case r :: Either SomeException () of
          Left e  -> hPutStrLn stderr ("hypha index build failed: " <> show e)
          Right _ -> pure ()
        IORef.writeIORef readyRef True
      pure ()
```

- [ ] **Step 6: Run the tests**

```
PATH=$HOME/.ghcup/bin:$PATH cabal build all && PATH=$HOME/.ghcup/bin:$PATH cabal test all
```

Expected: PASS, including the new hydration case.

- [ ] **Step 7: Commit**

```bash
git add src/Hypha/Search/Indexer.hs src/Hypha/Command/Server.hs \
        test/Unit/SearchIndexCache.hs
git -c user.name="Alfredo Di Napoli" -c user.email="alfredo@well-typed.com" \
  commit -m "feat(search): walk units dependencies-first, threading the export env

Hydration now hands back what the warm cache exports, and the background
pass folds over topologicalOrder extending that environment per component.
A run that rebuilds only base can still resolve it, because ghc-internal's
cached rows are already in the environment.  A package whose source cannot
be resolved is reported instead of silently skipped.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

### Task 6: collapse folds a definition's presentations across packages

Rows now exist in both packages. Without this, `mapAccumL` returns two results; with it, one — presented by `base`, with the `ghc-internal` definition one click away.

**Files:**
- Modify: `src/Hypha/Search/Collapse.hs:62-131`
- Modify: `test/Unit/SearchCollapse.hs`

**Interfaces:**
- Consumes: `DefinitionRef (..)`, `srDefinition` (Task 1).
- Produces: no signature change; `symbolKey`'s type narrows to `(DefinitionRef, SymbolName)`.

- [ ] **Step 1: Write the failing test**

Add to `test/Unit/SearchCollapse.hs`:

```haskell
  , testCase "base's presentation wins over ghc-internal's definition" $ do
      -- The issue in one case: mapAccumL is declared in ghc-internal and
      -- published by base.  One result, presented by base.
      let ghcInternal = ModulePath "GHC.Internal.Data.Traversable"
          def = DefinitionRef (ComponentKey "ghc-internal") ghcInternal
      case collapse
             [ rowFrom "ghc-internal" "GHC.Internal.Data.Traversable"
                 "mapAccumL" "sig" def Exposed
             , rowFrom "ghc-internal" "GHC.Internal.Data.List"
                 "mapAccumL" "sig" def Exposed
             , rowFrom "base" "Data.Traversable" "mapAccumL" "sig" def Exposed
             ] of
        [ResultSymbol s] -> do
          srComponent s  @?= ComponentKey "base"
          srModule s     @?= ModulePath "Data.Traversable"
          srDefinition s @?= def
          srAlternates s @?= 2
          resultHref s   @?= "/pkg/base/Data.Traversable/mapAccumL"
        other -> fail ("expected one collapsed result, got " <> show (length other))

  , testCase "two packages that merely share a module name stay two results" $ do
      -- Same module name, same symbol, different definitions.  Collapsing
      -- these would claim one package's code is the other's.
      let results = collapse
            [ rowFrom "alpha" "Shared.Mod" "thing" "sig"
                (DefinitionRef (ComponentKey "alpha") (ModulePath "Shared.Mod")) Exposed
            , rowFrom "beta"  "Shared.Mod" "thing" "sig"
                (DefinitionRef (ComponentKey "beta")  (ModulePath "Shared.Mod")) Exposed
            ]
      length results @?= 2

  , testCase "the component breaks a tie between identical presentations" $ do
      -- Two packages presenting one definition under the same module name.
      -- Which wins does not matter; that it is the same one every run does.
      let def = DefinitionRef (ComponentKey "core") (ModulePath "Core.Internal")
          rows =
            [ rowFrom "zeta"  "Facade" "thing" "sig" def Exposed
            , rowFrom "alpha" "Facade" "thing" "sig" def Exposed
            ]
      case (collapse rows, collapse (reverse rows)) of
        ([ResultSymbol a], [ResultSymbol b]) -> do
          srComponent a @?= ComponentKey "alpha"
          srComponent b @?= ComponentKey "alpha"
        _ -> fail "expected one collapsed result from each order"
```

The existing "strict and lazy stay two results" case must keep passing: both are `containers` rows with different `drModule`s, so their keys still differ.

- [ ] **Step 2: Run to verify it fails**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "SearchCollapse"'
```

Expected: FAIL — `expected one collapsed result, got 2` (base and ghc-internal are still keyed apart by component).

- [ ] **Step 3: Re-key the group and extend the rank**

In `src/Hypha/Search/Collapse.hs`, update the `collapseRows` haddock and `symbolKey`:

```haskell
-- | Fold every presentation of one definition into a single result.
--
-- The group key is @(definition, name)@ — not the name, and not the name
-- plus signature.  @Data.Map.Strict.insertWith@ and
-- @Data.Map.Lazy.insertWith@ have the same name /and/ the same signature
-- and are different functions; they differ only in where they are defined,
-- which is why that is the key.
--
-- The presenting component is deliberately /not/ part of the key.
-- @base:Data.Traversable.mapAccumL@ and
-- @ghc-internal:GHC.Internal.Data.Traversable.mapAccumL@ are one
-- function published under two surfaces, and the author meant it to be
-- consumed from @base@.  'DefinitionRef' carries its own component, so
-- two packages that merely share a module name still key apart.
--
-- Input order (already ranked) is preserved: a group appears where its
-- first member appeared.
```

```haskell
symbolKey :: IndexRow -> (DefinitionRef, SymbolName)
symbolKey r = (rowDefinition r, rowName r)
```

Extend `presentationRank` with the component tiebreak:

```haskell
-- | Ordered: exposed before internal, then a path with no @Internal@
-- segment, then fewer segments, then lexicographic on the module, then on
-- the component.  Total and deterministic, so the winner does not depend
-- on the order SQLite happened to return rows in — and now that a group
-- can span components, the module alone is no longer a total order.
--
-- This ladder is what picks @base:Data.Traversable@ (2 segments, no
-- @Internal@) over @ghc-internal:GHC.Internal.Data.Traversable@ (4
-- segments, one @Internal@).  It ranks by the shape of the surface, not by
-- any notion of which package the project "meant" to depend on; the @+N@
-- affordance is the escape hatch when the shape misleads.
presentationRank :: IndexRow -> (Int, Int, Int, Text, Text)
presentationRank row =
  ( case rowVisibility row of Exposed -> 0; Internal -> 1
  , if "Internal" `elem` segments then 1 else 0
  , length segments
  , unModulePath (rowModule row)
  , unComponentKey (rowComponent row)
  )
  where
    segments = Text.splitOn "." (unModulePath (rowModule row))
```

- [ ] **Step 4: Run the tests to verify they pass**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "SearchCollapse"'
```

Expected: PASS, all cases including the three new ones.

- [ ] **Step 5: Run the whole suite and check the golden files**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test all
```

Expected: PASS. `Golden.Human` and `Golden.Server` render fixed row sets; if a golden diff appears, read it before accepting — a *fewer results* diff is this change working, a changed signature or module is a bug.

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Search/Collapse.hs test/Unit/SearchCollapse.hs
git -c user.name="Alfredo Di Napoli" -c user.email="alfredo@well-typed.com" \
  commit -m "feat(search): collapse a definition's presentations across packages

mapAccumL is declared in ghc-internal and published by base; they are one
function under two surfaces.  The group key drops the presenting component
-- DefinitionRef carries its own, so packages that merely share a module
name still key apart -- and presentationRank gains a component tiebreak now
that a group can span components.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

### Task 7: module pages render entries defined in a dependency

`/pkg/base/Data.Traversable` currently renders an empty page. The pure resolution says which dependency modules it needs; the server loads them; the pure pass builds their entries exactly as it builds local ones.

**Files:**
- Modify: `src/Hypha/Search/Reexport.hs:20-28` (exports), append `outsideModulesFor`
- Modify: `src/Hypha/Project/Components.hs` (exports), append `moduleOwner`
- Modify: `src/Hypha/Source/Extract.hs:95-115`, `:159-194`
- Modify: `src/Hypha/Server/Ui/ModuleDoc.hs:122-141`
- Modify: `src/Hypha/Command/Server.hs:367-401`, append `importedSourcesFor`
- Modify: `test/Unit/SourceExtract.hs`
- Modify: `test/Golden/Server.hs:50-70`

**Interfaces:**
- Consumes: `DefinitionRef (..)` (Task 1); `dependencySet` (Task 4).
- Produces:
  ```haskell
  -- Hypha.Search.Reexport
  outsideModulesFor :: [ModuleInterface] -> ModulePath -> [ModulePath]

  -- Hypha.Project.Components
  moduleOwner :: BuildPlan -> PackageId -> ModulePath -> Maybe (PackageId, ComponentKind)

  -- Hypha.Source.Extract
  data EntryOrigin = EntryLocal | EntryReexport !DefinitionRef

  resolveModuleEntries
    :: Extensions.LanguageSettings
    -> ComponentKey
    -> [ModuleSource]                      -- ^ the asking component
    -> Map ModulePath (ComponentKey, ModuleSource)   -- ^ imported modules
    -> ModulePath
    -> Either Parser.ParseError ModuleDocInfo

  -- Hypha.Command.Server
  importedSourcesFor
    :: BuildPlan -> PackageResolver IO -> Text -> [ModuleSource] -> ModulePath
    -> IO (Map ModulePath (ComponentKey, ModuleSource))
  ```

Note: `moduleOwner` goes in `Hypha.Project.Components` because that module already owns `ComponentInfo` and `ciExposedModules`; it cannot go in `BuildPlan`, which imports `Components`.

- [ ] **Step 1: Write the failing test for `outsideModulesFor`**

In `test/Unit/SearchReexport.hs`, add:

```haskell
  , testCase "a facade's outside modules are the imports its exports need" $ do
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Imported.hs", "Fixture.Imported", Exposed) ]
      ifaces <- traverse parseFixture srcs
      Reexport.outsideModulesFor ifaces (ModulePath "Fixture.Imported")
        @?= [ModulePath "Dep.Internal"]

  , testCase "a module that defines what it exports needs nothing outside" $ do
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Internal.hs", "Fixture.Internal", Exposed) ]
      ifaces <- traverse parseFixture srcs
      Reexport.outsideModulesFor ifaces (ModulePath "Fixture.Internal") @?= []
```

with a helper matching how the file already parses fixtures (copy the existing parse helper in that module; if it has none, add):

```haskell
parseFixture :: ModuleSource -> IO ModuleInterface
parseFixture ms =
  case Interface.parseInterface defaultLanguageSettings (msPath ms) (msContent ms) of
    Right i -> pure i
    Left e  -> fail (Text.unpack (Parser.parseErrorMessage e))
```

- [ ] **Step 2: Run to verify it fails**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "SearchReexport"'
```

Expected: compile failure — `outsideModulesFor` not in scope.

- [ ] **Step 3: Implement `outsideModulesFor`**

Add to the exports of `src/Hypha/Search/Reexport.hs` and append:

```haskell
-- | The modules outside the component that the asking module's exports
-- resolve into.
--
-- A module page needs the /sources/ of these, not just their names: the
-- entries it renders carry haddock and line numbers that only the
-- defining module has.  Returning the list separately is what lets the
-- server load exactly those and keeps this module free of IO.
--
-- Deduplicated and sorted, so the caller's read set does not depend on
-- export-list order.
outsideModulesFor :: [ModuleInterface] -> ModulePath -> [ModulePath]
outsideModulesFor ifaces asking =
  Set.toList (Set.fromList
    [ m
    | n <- expandedExportNames ifaces asking
    , Just res <- [Map.lookup (asking, n) resolution]
    , DefinedOutside m <- [resSite res]
    , m /= asking
    ])
  where
    resolution = resolveComponent ifaces
```

- [ ] **Step 4: Run to verify it passes, then commit the primitive**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "SearchReexport"'
```

Expected: PASS.

```bash
git add src/Hypha/Search/Reexport.hs test/Unit/SearchReexport.hs
git -c user.name="Alfredo Di Napoli" -c user.email="alfredo@well-typed.com" \
  commit -m "feat(search): report which outside modules a facade's exports need

A module page has to read the defining module's source to render haddock
and line numbers.  Naming those modules without doing the IO is what keeps
Reexport pure.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 5: Write the failing test for `moduleOwner`**

In `test/Unit/Components.hs`, add:

```haskell
  , testCase "a dependency's exposed module resolves to its component" $
      Comp.moduleOwner ownerPlan
        (PackageId (PackageName "facade") (Version "1.0"))
        (ModulePath "Dep.Internal")
        @?= Just (PackageId (PackageName "dep") (Version "2.0"), Comp.MainLib)

  , testCase "a module no dependency exposes has no owner" $
      Comp.moduleOwner ownerPlan
        (PackageId (PackageName "facade") (Version "1.0"))
        (ModulePath "Nowhere.At.All")
        @?= Nothing

  , testCase "a non-dependency exposing the module is not its owner" $
      -- stranger exposes Dep.Internal too but facade does not depend on it.
      Comp.moduleOwner ownerPlan
        (PackageId (PackageName "lonely") (Version "1.0"))
        (ModulePath "Dep.Internal")
        @?= Nothing
```

with a plan fixture in that module (the file already imports what it needs for `ComponentInfo`; add `BuildPlan`/`PlannedUnit` imports as the compiler asks):

```haskell
-- | facade depends on dep, which exposes Dep.Internal.  lonely depends on
-- nothing.  stranger exposes Dep.Internal but is nobody's dependency.
ownerPlan :: BuildPlan
ownerPlan = emptyBuildPlan
  { bpUnits = Map.fromList
      [ (PackageName "facade", planUnit "facade" "1.0" ["dep"] [])
      , (PackageName "dep",    planUnit "dep"    "2.0" []      ["Dep.Internal"])
      , (PackageName "lonely", planUnit "lonely" "1.0" []      [])
      , (PackageName "stranger", planUnit "stranger" "1.0" []  ["Dep.Internal"])
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
          _  -> [ Comp.ComponentInfo
                    { Comp.ciKind             = Comp.MainLib
                    , Comp.ciHsSourceDirs     = ["src"]
                    , Comp.ciExposedModules   = exposed
                    , Comp.ciOtherModules     = []
                    , Comp.ciLanguageSettings = defaultLanguageSettings
                    , Comp.ciUnknownExtensions = []
                    } ]
      }
```

Check `ComponentInfo`'s full field list in `src/Hypha/Project/Components.hs:48-66` and fill every field — a partial record is banned.

- [ ] **Step 6: Run to verify it fails**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "Components"'
```

Expected: compile failure — `moduleOwner` not in scope.

- [ ] **Step 7: Implement `moduleOwner`**

It goes in `src/Hypha/Types/BuildPlan.hs`, beside `forwardDepsOf` — *not* in `Hypha.Project.Components`, which cannot see `BuildPlan` because `BuildPlan` imports it. The test from Step 5 therefore imports it from `Hypha.Types.BuildPlan`; adjust that import if you wrote it against `Comp.`.

Append:

```haskell
-- | Which of a unit's dependencies exposes a module, and under which
-- component.
--
-- Answered from the plan alone: no source is read and no index is
-- consulted, so a module page can find the owner of a cross-package
-- re-export before deciding what to load.
--
-- Scoped to the asking unit's dependencies, plus the unit itself so a
-- sub-library re-exporting from the main library resolves.  Two unrelated
-- packages can expose a module of the same name, and only the asking
-- unit's dependency list says which one it meant.  Ties among dependencies
-- break lexicographically so the answer does not depend on plan order.
moduleOwner
  :: BuildPlan
  -> PackageId
  -> ModulePath
  -> Maybe (PackageId, ComponentKind)
moduleOwner bp asking m =
  case sortOn (unPackageName . pkgName . fst) candidates of
    (c : _) -> Just c
    []      -> Nothing
  where
    scope = pkgName asking : case Map.lookup (pkgName asking) (bpUnits bp) of
      Nothing -> []
      Just u  -> map pkgName (puDeps u)

    candidates =
      [ (puId u, ciKind c)
      | n <- scope
      , Just u <- [Map.lookup n (bpUnits bp)]
      , c <- puLibComponents u
      , unModulePath m `elem` ciExposedModules c
      ]
```

Add `moduleOwner` to the `-- * Queries` export block, `import Data.List (sortOn)`, and bring `ciKind`, `ciExposedModules` and `ComponentKind` into the existing `Hypha.Project.Components` import.

- [ ] **Step 8: Run to verify it passes**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "Components"'
```

Expected: PASS, 3 new cases.

- [ ] **Step 9: Write the failing test for cross-package module entries**

In `test/Unit/SourceExtract.hs`, add:

```haskell
  , testCase "a facade page shows its dependency's entry, with haddock" $ do
      srcs <- fixtureSources
      dep  <- depSources
      let imported = Map.fromList
            [ (ModulePath "Dep.Internal", (ComponentKey "reexport-dep", d))
            | d <- dep
            ]
      case Extract.resolveModuleEntries defaultLanguageSettings
             (ComponentKey "reexport") srcs imported (ModulePath "Fixture.Imported") of
        Left e -> fail (Text.unpack (Parser.parseErrorMessage e))
        Right info -> do
          [ deOrigin e | e <- mdiEntries info, deName e == "depThing" ]
            @?= [ EntryReexport (DefinitionRef (ComponentKey "reexport-dep")
                                               (ModulePath "Dep.Internal")) ]
          [ deSignature e | e <- mdiEntries info, deName e == "depThing" ]
            @?= [Just "depThing :: Int -> Int"]
          assertBool "the dependency's haddock is carried over"
            (not (null [ () | e <- mdiEntries info
                            , deName e == "depThing"
                            , Just _ <- [deHaddock e] ]))

  , testCase "an entry whose owner is unknown is listed, not dropped" $ do
      -- No imported sources at all: the page must still name depThing.
      -- Omitting it would leave the reader with no way to know it exists.
      srcs <- fixtureSources
      case Extract.resolveModuleEntries defaultLanguageSettings
             (ComponentKey "reexport") srcs Map.empty
             (ModulePath "Fixture.Imported") of
        Left e -> fail (Text.unpack (Parser.parseErrorMessage e))
        Right info -> do
          map deName (mdiEntries info) @?= ["depThing"]
          [ deSignature e | e <- mdiEntries info ] @?= [Nothing]
```

Existing assertions at lines 177-180 change from `EntryReexport (ModulePath "Fixture.Internal")` to `EntryReexport (DefinitionRef (ComponentKey "reexport") (ModulePath "Fixture.Internal"))`, and likewise for `Fixture.Other`.

- [ ] **Step 10: Run to verify it fails**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "SourceExtract"'
```

Expected: compile failure — `resolveModuleEntries` applied to too many arguments.

- [ ] **Step 11: Change `EntryOrigin` and `resolveModuleEntries`**

In `src/Hypha/Source/Extract.hs`, replace the `EntryOrigin` definition (lines 99-101):

```haskell
-- | Where an entry's declaration lives.
--
-- 'EntryReexport' carries a full 'DefinitionRef' rather than a
-- 'ModulePath' because a re-export can cross a package boundary:
-- @base@'s @Data.Traversable@ documents entries declared in
-- @ghc-internal@.  One constructor covers both cases — the renderer shows
-- the package only when it differs from the page's — so a "re-export" of
-- the very module being rendered is not representable; that is
-- 'EntryLocal'.
data EntryOrigin
  = EntryLocal
  | EntryReexport !DefinitionRef
  deriving stock (Show, Eq)
```

Replace `resolveModuleEntries` (lines 166-194):

```haskell
-- | Every entry a module's page should show, re-exports included.
--
-- 'extractModuleDoc' reports only locally declared declarations, so a pure
-- re-export module produced nothing: @Data.Map.Strict@ had an empty \"On
-- this page\" rail because it declares almost nothing, and @base@'s
-- @Data.Traversable@ had one because it declares nothing at all.
--
-- @imported@ supplies the modules of /other/ components that this
-- module's exports resolve into, keyed by module name; the caller obtains
-- them from 'Reexport.outsideModulesFor' plus the plan.  An export whose
-- owner is missing from that map is still listed, with its origin and no
-- signature: a name we cannot describe is worth more to the reader than a
-- silently shorter page.
resolveModuleEntries
  :: Extensions.LanguageSettings
  -> ComponentKey
  -> [ModuleSource]
  -> Map ModulePath (ComponentKey, ModuleSource)
  -> ModulePath
  -> Either Parser.ParseError ModuleDocInfo
resolveModuleEntries langs compKey sources imported asking = do
  ifaces <- traverse parseOne sources
  importedIfaces <- traverse parseImported (Map.toList imported)
  let byName  = Map.fromList [ (miName i, i) | i <- ifaces ]
      linesOf = Map.fromList
        [ (miName i, numberedLines (msContent ms))
        | (ms, i) <- zip sources ifaces
        ]
      -- Keyed on the module name the map used, not on the parsed name:
      -- the caller asked for this module by name and that is how the
      -- resolution refers to it.
      outsideOf = Map.fromList
        [ (m, (c, i, numberedLines (msContent ms)))
        | (m, (c, ms, i)) <- importedIfaces
        ]
      resolution = Reexport.resolveComponent ifaces
  asked <- maybe (Left (missingModule asking)) Right (Map.lookup asking byName)
  pure ModuleDocInfo
    { mdiHeader  = DocText <$> miHeaderDoc asked
    , mdiEntries =
        [ entry
        | name <- Reexport.expandedExportNames ifaces asking
        , Just res <- [Map.lookup (asking, name) resolution]
        , Just entry <- [entryFor byName linesOf outsideOf asking name (Reexport.resSite res)]
        ]
    }
  where
    parseOne ms = Interface.parseInterface langs (msPath ms) (msContent ms)

    parseImported (m, (c, ms)) = do
      i <- Interface.parseInterface langs (msPath ms) (msContent ms)
      pure (m, (c, ms, i))

    -- An entry from inside the component, from a named dependency module,
    -- or a name-only placeholder when the dependency's source is absent.
    entryFor byName linesOf outsideOf asked name site = case site of
      Reexport.DefinedOutside m
        | m /= asked -> case Map.lookup m outsideOf of
            Just (c, i, ls) -> do
              decl <- Parser.findDecl (unSymbolName name) (miDecls i)
              pure (docEntryFrom ls decl (EntryReexport (DefinitionRef c m)))
            Nothing -> Just (placeholder name (DefinitionRef compKey m))
      _ -> do
        let defMod = Reexport.definitionModule asked site
        defIface <- Map.lookup defMod byName
        decl     <- Parser.findDecl (unSymbolName name) (miDecls defIface)
        let ls     = Map.findWithDefault [] defMod linesOf
            origin = if defMod == asked
                       then EntryLocal
                       else EntryReexport (DefinitionRef compKey defMod)
        pure (docEntryFrom ls decl origin)

    -- Everything we know when the defining source is out of reach: the
    -- name and where it came from.
    placeholder name def = DocEntry
      { deName      = unSymbolName name
      , deKind      = Parser.DkFunction
      , deSignature = Nothing
      , deHaddock   = Nothing
      , deSigLine   = Nothing
      , deDefLine   = Nothing
      , deOrigin    = EntryReexport def
      }
```

Add `DefinitionRef (..)` and `ComponentKey (..)` to the imports, plus `Data.Map.Strict (Map)`. Confirm `Parser.DkFunction`'s constructor name against `src/Hypha/Source/Parser.hs` and use the actual "unknown kind" constructor if one exists.

The `DefinedOutside` arm with `m == asked` falls through to the general arm, where `definitionModule` returns `asked`, `byName` finds it, and `findDecl` fails — so the export is omitted. That matches the indexer's treatment of the same case.

- [ ] **Step 12: Render a cross-package origin correctly**

In `src/Hypha/Server/Ui/ModuleDoc.hs`, the `reexportNote` and `srcModule` helpers currently build hrefs with the page's `pkgT`. Replace lines 127-141:

```haskell
    reexportNote = case deOrigin e of
      EntryLocal          -> mempty
      EntryReexport def   ->
        a_ [ class_ "decl-origin"
           , href_ ("/pkg/" <> unComponentKey (drComponent def)
                      <> "/" <> unModulePath (drModule def)
                      <> "/" <> deName e)
           , title_ (if unComponentKey (drComponent def) == pkgT
                       then "Defined in another module of this package"
                       else "Defined in another package")
           ]
           (toHtml ("from " <> originLabel def))

    -- The package is named only when it differs, so an intra-package
    -- re-export reads exactly as it did before.
    originLabel def
      | unComponentKey (drComponent def) == pkgT = unModulePath (drModule def)
      | otherwise = unComponentKey (drComponent def) <> ":" <> unModulePath (drModule def)

    -- The source link follows the definition, because that is where the
    -- lines this entry reports actually are — including into another
    -- package.
    (srcComponent, srcModule) = case deOrigin e of
      EntryLocal        -> (pkgT, modT)
      EntryReexport def -> ( unComponentKey (drComponent def)
                           , unModulePath (drModule def) )
```

and update `srcLink` to use `srcComponent` in place of `pkgT`:

```haskell
           , href_ ("/source/" <> srcComponent <> "/" <> srcModule
                     <> "?line=" <> tshow n <> "#L" <> tshow n)
```

Import `DefinitionRef (..)` from `Hypha.Search.Index` and `ComponentKey (..)` from `Hypha.Types.ComponentName`.

- [ ] **Step 13: Load the imported sources in the server**

In `src/Hypha/Command/Server.hs`, add:

```haskell
-- | The modules of /other/ components that a module's exports resolve
-- into, keyed by module name.
--
-- Two hops, both cheap: 'Reexport.outsideModulesFor' says which module
-- names the page needs, 'moduleOwner' says which dependency exposes each
-- one — from the plan alone — and only then is anything read.  One extra
-- parse per page view, against one per indexed package if the index pass
-- did this instead.
importedSourcesFor
  :: BuildPlan
  -> PackageResolver IO
  -> Text                    -- ^ component name from the URL
  -> [ModuleSource]          -- ^ the asking component's modules
  -> ModulePath
  -> IO (Map ModulePath (ComponentKey, Index.ModuleSource))
importedSourcesFor plan resolver pkgT sources asking = do
  parsed <- mapM parseOne sources
  let ifaces  = [ i | Right i <- parsed ]
      wanted  = Reexport.outsideModulesFor ifaces asking
      cn      = parseComponentName pkgT
  ePid <- resolvePkg resolver (cnPackage cn)
  case ePid of
    Left err -> do
      hPutStrLn stderr $
        "hypha server: cannot resolve " <> Text.unpack pkgT
          <> " to find its dependencies: " <> show err
      pure Map.empty
    Right rp -> Map.fromList . catMaybes <$> mapM (loadOwner (rpPkgId rp)) wanted
  where
    langs = componentLanguageSettings plan pkgT

    parseOne ms = Interface.parseInterfaceIO langs (msPath ms) (msContent ms)

    loadOwner pid m = case moduleOwner plan pid m of
      Nothing -> do
        hPutStrLn stderr $
          "hypha server: no dependency of "
            <> Text.unpack (unPackageName (pkgName pid)) <> " exposes "
            <> Text.unpack (unModulePath m) <> "; its entries will have no signature"
        pure Nothing
      Just (ownerPid, kind) -> do
        let ownerKey = componentKeyOf (pkgName ownerPid) kind
        eDir <- resolveSrc resolver ownerPid
        case eDir of
          Left err -> do
            hPutStrLn stderr $
              "hypha server: no source for " <> Text.unpack (unComponentKey ownerKey)
                <> ": " <> show err
            pure Nothing
          Right d -> do
            comps <- Indexer.componentsForUnit plan ownerPid d
            let dirs = concat [ ds | (k, ds) <- comps, k == kind ]
            srcs <- Indexer.loadModuleSources dirs [(unModulePath m, Index.Exposed)]
            case srcs of
              (ms : _) -> pure (Just (m, (ownerKey, ms)))
              []       -> do
                hPutStrLn stderr $
                  "hypha server: " <> Text.unpack (unComponentKey ownerKey)
                    <> " has no source file for " <> Text.unpack (unModulePath m)
                pure Nothing
```

Then update `sourceView` (line 375-377):

```haskell
        sources <- lift (componentSourcesFor plan resolver pkgT dirs)
        imported <- lift (importedSourcesFor plan resolver pkgT sources (ModulePath modT))
        let langs   = componentLanguageSettings plan pkgT
            compKey = componentKeyOf (cnPackage cn) (cnKind cn)
        case Extract.resolveModuleEntries langs compKey sources imported
               (ModulePath modT) of
```

Add the imports the compiler asks for: `Data.Maybe (catMaybes)`, `Hypha.Types.BuildPlan (moduleOwner)`, `Hypha.Search.Reexport qualified as Reexport`, `Hypha.Source.Interface qualified as Interface`, `Hypha.Types.ComponentName (componentKeyOf)`.

- [ ] **Step 14: Update the golden fixtures**

`test/Golden/Server.hs:56,65` construct `DocEntry`s with `deOrigin = EntryLocal`, which still typechecks. Run the goldens and inspect any diff:

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "Golden"'
```

Expected: PASS with no diff — these fixtures are all `EntryLocal`, and the rendering of a local entry is unchanged.

- [ ] **Step 15: Run the whole suite**

```
PATH=$HOME/.ghcup/bin:$PATH cabal build all && PATH=$HOME/.ghcup/bin:$PATH cabal test all
```

Expected: PASS.

- [ ] **Step 16: Commit**

```bash
git add src/Hypha/Types/BuildPlan.hs src/Hypha/Source/Extract.hs \
        src/Hypha/Server/Ui/ModuleDoc.hs src/Hypha/Command/Server.hs \
        test/Unit/Components.hs test/Unit/SourceExtract.hs
git -c user.name="Alfredo Di Napoli" -c user.email="alfredo@well-typed.com" \
  commit -m "feat(server): render module entries defined in a dependency

/pkg/base/Data.Traversable was an empty page: resolveModuleEntries looked
the definition module up among the component's own modules and dropped
every miss.  The pure pass now takes the imported modules the resolution
names, obtained from the plan by moduleOwner, and builds their entries
identically to local ones.  EntryOrigin carries a DefinitionRef so the
origin link and the source link point at the owning package.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

### Task 8: symbol cards find the definition in the owning package

`/pkg/base/Data.Traversable/mapAccumL` has no source snippet: `locateDefinitionInComponent` returns `Nothing` for a `DefinedOutside` site. It now looks in the imported sources the server supplies.

**Files:**
- Modify: `src/Hypha/Source/Locate.hs:315-360`
- Modify: `src/Hypha/Command/Server.hs:243-246`
- Modify: `test/Unit/Module.hs` or `test/Unit/SourceExtract.hs` (wherever `locateDefinitionInComponent` is covered — check with `grep -rn locateDefinitionInComponent test/`)

**Interfaces:**
- Consumes: `importedSourcesFor` (Task 7); `DefinitionRef (..)` (Task 1).
- Produces:
  ```haskell
  -- Hypha.Source.Locate
  locateDefinitionInComponent
    :: Extensions.LanguageSettings
    -> [ModuleSource]
    -> Map ModulePath (ComponentKey, ModuleSource)   -- ^ imported modules
    -> ModulePath
    -> SymbolName
    -> IO (Maybe LocatedDefinition)

  data LocatedDefinition = LocatedDefinition
    { ldLocation   :: !SourceLocation
    , ldModule     :: !ModulePath
    , ldComponent  :: !ComponentKey     -- ^ new
    , ldProvenance :: !Provenance
    }
  ```

- [ ] **Step 1: Write the failing test**

Find the existing coverage first:

```
grep -rn "locateDefinitionInComponent" test/
```

Add to whichever module has it (create `test/Unit/SourceLocate.hs`, wired into `hypha.cabal` and `test/Main.hs`, if there is none):

```haskell
  , testCase "a symbol defined in a dependency is located in that package" $ do
      srcs <- fixtureSources
      dep  <- depSources
      let imported = Map.fromList
            [ (ModulePath "Dep.Internal", (ComponentKey "reexport-dep", d))
            | d <- dep
            ]
      mLd <- Locate.locateDefinitionInComponent defaultLanguageSettings srcs
               imported (ModulePath "Fixture.Imported") (SymbolName "depThing")
      case mLd of
        Just ld -> do
          Locate.ldModule ld    @?= ModulePath "Dep.Internal"
          Locate.ldComponent ld @?= ComponentKey "reexport-dep"
          assertBool "points into the dependency's tree"
            ("reexport-dep" `isInfixOf` Locate.slPath (Locate.ldLocation ld))
        Nothing -> fail "expected to locate depThing in reexport-dep"

  , testCase "a symbol whose dependency source is absent is not guessed at" $ do
      srcs <- fixtureSources
      mLd <- Locate.locateDefinitionInComponent defaultLanguageSettings srcs
               Map.empty (ModulePath "Fixture.Imported") (SymbolName "depThing")
      mLd @?= Nothing
```

- [ ] **Step 2: Run to verify it fails**

```
PATH=$HOME/.ghcup/bin:$PATH cabal test hypha-tests --test-options='-p "Locate"'
```

Expected: compile failure — too many arguments; `ldComponent` not in scope.

- [ ] **Step 3: Take the imported sources**

In `src/Hypha/Source/Locate.hs`, add `ldComponent` to `LocatedDefinition`:

```haskell
data LocatedDefinition = LocatedDefinition
  { ldLocation   :: !SourceLocation
  , ldModule     :: !ModulePath
  , ldComponent  :: !ComponentKey
    -- ^ Which component the definition is in.  A re-export can cross a
    -- package boundary, and a card that reported only the module would
    -- send the reader to a module the page's package does not have.
  , ldProvenance :: !Provenance
  }
  deriving stock (Show, Eq)
```

and replace `locateDefinitionInComponent`. Note the new `ComponentKey`
parameter: the function has to report which component the definition is in,
and deriving that from a module path is exactly the mistake this whole
change removes, so it is passed in.

```haskell
locateDefinitionInComponent
  :: Extensions.LanguageSettings
  -> ComponentKey                                   -- ^ the asking component
  -> [ModuleSource]
  -> Map ModulePath (ComponentKey, ModuleSource)    -- ^ imported modules
  -> ModulePath
  -> SymbolName
  -> IO (Maybe LocatedDefinition)
locateDefinitionInComponent langs ownComponent sources imported asking sym = do
  parsed <- mapM parseOne sources
  mapM_ reportParseFailure [ (ms, e) | (ms, Left e) <- parsed ]
  let ifaces     = [ i | (_, Right i) <- parsed ]
      resolution = Reexport.resolveComponent ifaces
  case Map.lookup (asking, sym) resolution of
    Nothing -> do
      hPutStrLn stderr $
        "hypha: " <> Text.unpack (unModulePath asking) <> " does not export "
          <> Text.unpack (unSymbolName sym)
      pure Nothing
    Just res -> case resSite res of
      DefinedOutside m | m /= asking -> case Map.lookup m imported of
        Nothing -> do
          hPutStrLn stderr $
            "hypha: " <> Text.unpack (unModulePath asking) <> " re-exports "
              <> Text.unpack (unSymbolName sym) <> " from "
              <> Text.unpack (unModulePath m)
              <> ", whose source was not supplied"
          pure Nothing
        Just (comp, ms) -> scanned comp m (resSite res) ms
      site -> do
        let target = Reexport.definitionModule asking site
        case [ ms | (ms, Right i) <- parsed, Interface.miName i == target ] of
          []       -> pure Nothing
          (ms : _) -> scanned ownComponent target site ms
  where
    parseOne ms = do
      r <- Interface.parseInterfaceIO langs (msPath ms) (msContent ms)
      pure (ms, r)

    reportParseFailure (ms, e) = hPutStrLn stderr $
      "hypha: " <> msPath ms <> " could not be parsed: "
        <> Text.unpack (Parser.parseErrorMessage e)

    scanned comp target site ms = do
      r <- scanFileE (unSymbolName sym) (msPath ms)
      case r of
        Left e -> do
          reportParseFailure (ms, e)
          pure Nothing
        Right Nothing    -> pure Nothing
        Right (Just loc) -> pure (Just LocatedDefinition
          { ldLocation   = loc
          , ldModule     = target
          , ldComponent  = comp
          , ldProvenance = Resolved site
          })
```

Update the test from Step 1 to pass `(ComponentKey "reexport")` as the second argument. Import `ComponentKey (..)` and `Data.Map.Strict (Map)`.

- [ ] **Step 4: Update the server's symbol-card wiring**

In `src/Hypha/Command/Server.hs`, replace lines 243-246:

```haskell
            sources  <- componentSourcesFor plan resolver pkgT dirs
            imported <- importedSourcesFor plan resolver pkgT sources (ModulePath modT)
            let langs   = componentLanguageSettings plan pkgT
                cn      = parseComponentName pkgT
                compKey = componentKeyOf (cnPackage cn) (cnKind cn)
            mLd <- Locate.locateDefinitionInComponent langs compKey sources
                     imported (ModulePath modT) (SymbolName symT)
```

`SymbolCardData.scdModule` currently carries only the module. Extend it so the card can say which package the definition is in — check `src/Hypha/Server/App.hs` (or wherever `SymbolCardData` is defined; `grep -rn "data SymbolCardData" src/`) and add:

```haskell
  , scdComponent :: !Text   -- ^ the component the definition is in
```

set from `unComponentKey (Locate.ldComponent ld)`, and render it in the card's provenance line beside `scdModule` when it differs from the page's component. Follow the surrounding rendering style in `src/Hypha/Server/Ui/Doc.hs`.

- [ ] **Step 5: Run the tests**

```
PATH=$HOME/.ghcup/bin:$PATH cabal build all && PATH=$HOME/.ghcup/bin:$PATH cabal test all
```

Expected: PASS. A `Golden.Symbol` diff is expected here — the card gained a component field. Inspect the diff, confirm it only adds the package where a cross-package definition exists, and accept it.

- [ ] **Step 6: Commit**

```bash
git add src/Hypha/Source/Locate.hs src/Hypha/Command/Server.hs \
        src/Hypha/Server/App.hs src/Hypha/Server/Ui/Doc.hs test/ hypha.cabal
git -c user.name="Alfredo Di Napoli" -c user.email="alfredo@well-typed.com" \
  commit -m "feat(server): locate a re-exported definition in the owning package

locateDefinitionInComponent returned Nothing for any DefinedOutside site,
so /pkg/base/Data.Traversable/mapAccumL had no source snippet.  It now
takes the imported modules the server resolved from the plan, and reports
which component the definition is in rather than leaving the reader to
assume it is this one.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

### Task 9: verify against the real index

Two bugs in the previous round of this work — an exponential resolver and `cpphs` raising `error` from pure code — passed 273 unit tests and were only exposed by a real pass. This task is not optional and its findings are not "nice to have".

**Files:**
- Modify: `docs/superpowers/plans/2026-07-28-cross-package-reexports.md` (a Notes section recording the measurement and any deviation)
- Possibly modify: whatever the pass exposes

- [ ] **Step 1: Discard the old index and re-index**

The format bump clears generation-2 rows on open, so simply starting the server re-indexes. Run it in the background and capture stderr — the reports added in Tasks 4 and 5 are the measurement:

```
PATH=$HOME/.ghcup/bin:$PATH cabal run hypha -- server --port 8099 > /tmp/hypha-index.log 2>&1
```

Let it finish (the previous full pass was ~90s for 5 packages and several minutes for 282). Watch the log for progress.

- [ ] **Step 2: Measure `base`**

```
sqlite3 ~/.cache/hypha/hypha.db \
  "select pkg, count(*) from pkg_index where pkg in ('base','ghc-internal') group by pkg;"
sqlite3 ~/.cache/hypha/hypha.db \
  "select mod, def_pkg, def_mod from pkg_index where name='mapAccumL' order by pkg, mod;"
```

Expected: `base` in the low thousands, up from 308. A row for `base` / `Data.Traversable` / `mapAccumL` with `def_pkg = ghc-internal` and `def_mod = GHC.Internal.Data.Traversable`.

If `base` is still ~308, the cross-package rows are not being written: check whether `topologicalOrder` actually put `ghc-internal` first (add a temporary `log` if needed) and whether `dependencySet` includes it.

- [ ] **Step 3: Run the index audit**

```
bash scripts/index-audit.sh
```

Expected: zero rows with a lowercase module segment, zero name mismatches, and no regression in the parse-failure count against the 159 recorded in the previous round's notes.

If the script has no check for the new column, add one: every row's `def_pkg` must be non-empty, and every `def_pkg` must be a package that has rows of its own.

- [ ] **Step 4: Check the report volume**

```
grep -c "could not resolve" /tmp/hypha-index.log
grep "could not resolve" /tmp/hypha-index.log | head -40
grep -c "rejecting" /tmp/hypha-index.log
```

The unresolved set should be bounded and explainable — mostly exports of modules that failed to parse (the 159 known CPP casualties) and re-exports from packages outside the plan. Thousands of lines, or a pattern that is neither of those, is a bug in the resolution and must be diagnosed before this task closes.

- [ ] **Step 5: Check the pages by hand**

The sandbox blocks localhost `curl` in this environment, so ask the human to check three URLs on the running server:

- `/search?q=mapAccumL` — `base : Data.Traversable` first, with a `+N` affordance whose link is `/pkg/ghc-internal/GHC.Internal.Data.Traversable/mapAccumL`.
- `/pkg/base/Data.Traversable` — entries listed with haddock, each tagged `from ghc-internal:GHC.Internal.Data.Traversable`.
- `/pkg/base/Data.Traversable/mapAccumL` — a source snippet, from `ghc-internal`'s tree.

- [ ] **Step 6: Build against all three GHCs**

```
PATH=$HOME/.ghcup/bin:$PATH cabal build all --project-file=cabal.ghc-9.6.7.project
PATH=$HOME/.ghcup/bin:$PATH cabal build all --project-file=cabal.ghc-9.12.4.project
```

Expected: both succeed. GHC 9.6's `base` is *not* a façade over `ghc-internal`, so its `base` row count will not jump — that is correct, not a regression.

- [ ] **Step 7: Record the measurement**

Append a `## Notes` section to this plan file with: the before/after row counts for `base` and `ghc-internal`, the total row and package counts, the unresolved-export count with its top categories, and every deviation from this plan with the reason. The previous round's plan (`docs/superpowers/plans/2026-07-27-hypha-index-correctness.md`) is the format to follow.

- [ ] **Step 8: Commit**

```bash
git add docs/superpowers/plans/2026-07-28-cross-package-reexports.md scripts/index-audit.sh
git -c user.name="Alfredo Di Napoli" -c user.email="alfredo@well-typed.com" \
  commit -m "docs: record the cross-package re-export measurement

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

## Self-Review

**Spec coverage.**

| Spec section | Task |
|---|---|
| `Hypha.Search.Exports` / `ExportEnv` | 3 |
| `BuildPlan.topologicalOrder` | 2 |
| `DefinitionRef`, `IndexRow` change | 1 |
| `EntryOrigin` carries a `DefinitionRef` | 7 |
| Cache `def_pkg`, format 3 | 1 |
| Index pass: `Hydrated`, dep-ordered fold | 5 |
| Indexer resolves outside exports | 4 |
| Collapse across packages, `presentationRank`, `definitionHref` | 6 (with `definitionHref` in 1) |
| Browsing: `outsideModulesFor`, `moduleOwner`, `resolveModuleEntries` | 7 |
| Browsing: symbol card | 8 |
| Error handling: env miss, ambiguity | 4 |
| Error handling: unknown owner on a page | 7 (placeholder entry) |
| Error handling: no build plan | 7 (`importedSourcesFor` returns an empty map with a reason; `resolveModuleEntries` then emits placeholders) |
| Fixture, unit, property, golden tests | 2, 3, 4, 6, 7, 8 |
| Real re-index + audit, cross-GHC | 9 |

**Deviations from the spec, and why.**

- `moduleOwner` lives in `Hypha.Types.BuildPlan`, not `Hypha.Project.Components`. The spec put it in `Components`, but `BuildPlan` imports `Components`, so that would be a module cycle. Task 7 Step 7 says so explicitly.
- `lookupExport` returns `Maybe ExportChoice`, not `Maybe (Export, Ambiguity)`. `Reexport.Ambiguity`'s payload is a `NonEmpty ModulePath`, and here the rejected candidates differ by *component* — a module alone cannot name them. The spec has been updated to match.
- `locateDefinitionInComponent` gains a `ComponentKey` parameter beyond what the spec described. It has to report which component the definition is in, and deriving that from a module path is the precise mistake this change exists to remove.

**Open risk, flagged rather than designed around.** `presentationRank` picks the winner from the shape of the module path. A façade package with shorter module names than the package it wraps would out-rank it. Task 6's test pins the `base`/`ghc-internal` case and the determinism, not the general question, and the `+N` affordance is the escape hatch. The alternative — ranking by whether the project depends on the package directly — was considered during brainstorming and rejected because it makes search results depend on the current project's plan.
