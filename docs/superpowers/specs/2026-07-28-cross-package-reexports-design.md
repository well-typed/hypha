# Hypha Server — Cross-Package Re-Exports

- **Date:** 2026-07-28
- **Status:** Draft (design)
- **Author:** Alfredo Di Napoli (with Claude Code)
- **Branch:** `adinapoli/more-server-improvements`
- **Issue:** [#11](https://gitlab.well-typed.com/well-typed/hypha/-/issues/11)
  — "`hypha server` gets confused about the internal exports when searching"

## Problem

Searching for `mapAccumL` finds it in `ghc-internal` and not in `base`.
That is technically where the declaration lives and it is not what the
library author published: `mapAccumL` is meant to be consumed from
`base`, re-exported by `Data.Traversable`. Hypha should mirror the
package surface as authors intended, in global search and in per-package
browsing alike.

### Root cause

Re-export resolution stops at the component boundary.

`Hypha.Search.Reexport.resolveComponent` resolves each
`(module, exported name)` pair against the modules of *one* component.
A name no module of that component declares resolves to
`DefinedOutside m`, naming the import believed to supply it. The
indexer then discards those pairs outright:

```haskell
-- src/Hypha/Search/Indexer.hs:315
, not (isDefinedOutside (resSite res))
```

with the justification that "we have no signature for it, and its
definition belongs to another index entry". True for the component in
isolation. Its consequence at the package level is that a package which
is *entirely* a re-export façade contributes almost nothing.

Since GHC 9.10 that describes `base`. `base-4.20.2.0/src/Data/Traversable.hs`
is an explicit export list followed by one line:

```haskell
module Data.Traversable (Traversable(..), for, forM, mapAccumL, …) where

import GHC.Internal.Data.Traversable
```

Measured against the live index (`~/.cache/hypha/hypha.db`,
generation 2, 282 packages):

```text
base          |  308 rows          ghc-internal | 1240 rows
```

`base`'s 308 rows come from the handful of modules that still declare
something (`Data.List.NonEmpty`, `Data.Functor.Classes`,
`Data.Bifoldable`, …). `Data.Traversable`, `Data.List`, `Data.Maybe`,
`Control.Monad` and `Prelude` contribute **zero**.

### The same hole, three symptoms

1. **Search** — `Indexer.indexParsedComponent` writes no row, so no
   query can reach `base:Data.Traversable.mapAccumL`.

2. **Module page** — `Extract.resolveModuleEntries` resolves, then does
   `Map.lookup defMod byName` against the component's own modules. For a
   cross-package re-export that lookup misses and the entry is dropped,
   so `/pkg/base/Data.Traversable` renders an empty page. Intra-package
   wrappers such as `Data.Map.Strict` *do* get full entries, which makes
   the façade pages look arbitrarily broken.

3. **Symbol card** — `Source.Locate` cannot locate the definition
   either, so `/pkg/base/Data.Traversable/mapAccumL` has no source
   snippet.

### A latent fourth defect

`IndexRow.rowDefModule` identifies a definition site by module alone.
Two packages can expose the same module name, so a module is not an
identity. `Collapse.definitionHref` already reads the consequence
wrongly: it builds the definition link from the *presentation's*
component and the *definition's* module. With cross-package rows in the
index that link would point at a module the package does not have.

## Approach

Make resolution cross exactly **one hop** into the owning dependency,
with the build plan's dependency graph as the authority on who owns a
module. Nothing in the design requires reading a dependency's sources
during an index pass, and nothing requires a resolution pass over the
whole plan at once.

Two new pieces, both small. No new package dependencies.

### `Hypha.Search.Exports` — what already-indexed components export

```haskell
newtype ExportEnv = ExportEnv (Map (ModulePath, SymbolName) (NonEmpty Export))

data Export = Export
  { exDefinition :: !DefinitionRef
  , exSignature  :: !Signature
  }

emptyEnv     :: ExportEnv
extendEnv    :: [IndexRow] -> ExportEnv -> ExportEnv
lookupExport :: Set PackageName    -- ^ the asking unit's dependencies
             -> ModulePath -> SymbolName -> ExportEnv
             -> Maybe (Export, Ambiguity)
```

`Nothing` means no dependency of this unit exports that pair. A `Just`
always carries a chosen `Export`, and the `Ambiguity` says whether the
choice was forced or made among rejected candidates — the same shape
`Reexport.Resolution` already uses, so ambiguity is reportable rather
than a failure.

Built from `IndexRow`s rather than from parse trees, which buys
transitivity for free: a dependency's rows already carry their own
resolved `DefinitionRef`, so resolving `base` through the env lands on
the true definition even when `ghc-internal` itself re-exported it.

`lookupExport` filters candidates to the asking unit's dependencies
before choosing, so two unrelated packages exposing a module of the same
name cannot contaminate each other. A key with several surviving
candidates is genuine ambiguity: report it and take the lexicographic
winner, reusing the `Ambiguity` vocabulary `Reexport` already has.

### `BuildPlan.topologicalOrder` — dependencies first

```haskell
-- | The given units, dependencies before dependents.  Units the plan
-- does not know keep their relative order at the end.
topologicalOrder :: BuildPlan -> [PackageId] -> [PackageId]
```

`ghc-internal` is indexed before `base`, always. A plan is a DAG; should
a cycle ever appear, the units still in the cycle are emitted in input
order rather than dropped.

## Type changes

`IndexRow.rowDefModule :: ModulePath` becomes:

```haskell
data DefinitionRef = DefinitionRef
  { drComponent :: !ComponentKey
  , drModule    :: !ModulePath
  }

data IndexRow = IndexRow
  { rowComponent  :: !ComponentKey
  , rowModule     :: !ModulePath
  , rowName       :: !SymbolName
  , rowSignature  :: !Signature
  , rowDefinition :: !DefinitionRef
  , rowVisibility :: !Visibility
  }
```

A definition site was never identified by a module alone; carrying the
component is what makes the collapse key and the definition link sound.

`Extract.EntryOrigin` gets the same treatment:

```haskell
data EntryOrigin = EntryLocal | EntryReexport !DefinitionRef
```

One constructor covers same-package and cross-package re-exports. The
renderer shows the package only when it differs from the page's
component, so `EntryReexport` cannot represent a "re-export" of the very
module being rendered — that state is `EntryLocal`.

### Cache

- `pkg_index` gains `def_pkg TEXT NOT NULL DEFAULT ''` through the
  existing `migrateAddColumn`.
- `currentIndexFormat` 2 → 3.

Generation-2 rows are discarded rather than migrated, for the reason
already recorded for generation 1: a stored `def_mod` cannot be
attributed to a component after the fact, so the only honest options are
to re-index or to lie. Re-indexing every package is acceptable — hypha
is pre-alpha and the pass is measured in minutes.

## Data flow

### Index pass (search)

1. `hydrateFromCache` already reads every cached row. It now returns
   what it loaded as an `ExportEnv` beside the missing units:

   ```haskell
   data Hydrated = Hydrated
     { hyEnv     :: !ExportEnv
     , hyMissing :: ![PackageId]
     }
   ```

2. `buildAndCacheIndex` folds over `topologicalOrder`-sorted units,
   threading the env explicitly (`foldM`, no `IORef` — the ordering is
   the point and an `IORef` would hide it).

3. Each component resolves exactly as today. Every export that resolves
   to `DefinedOutside m` is then looked up as `(m, name)` in the env,
   filtered to the unit's `puDeps`. A hit produces a complete row: the
   dependency's signature, the dependency's `DefinitionRef`, and the
   asking module's own visibility.

4. The finished component's rows extend the env for later units.

The pure core stays pure: `indexParsedComponent` takes the env and the
dependency set as arguments.

### Collapse (search results)

Group key becomes `(rowDefinition, rowName)`. The component drops out of
the key because `DefinitionRef` already carries it —
`base:Data.Traversable.mapAccumL` and
`ghc-internal:GHC.Internal.Data.Traversable.mapAccumL` share a
definition and fold into one result.

`presentationRank` is unchanged in spirit and picks `base`:

| criterion | `base:Data.Traversable` | `ghc-internal:GHC.Internal.Data.Traversable` |
|---|---|---|
| exposed before internal | exposed | exposed |
| no `Internal` segment | 0 | 1 |
| fewer segments | 2 | 4 |

It gains `unComponentKey` as a final tiebreak so the order stays total
now that one group can span components.

`definitionHref` is corrected to build the link from
`drComponent (srDefinition s)`, not from the presentation's component.

The `+N` affordance keeps its existing meaning: nothing is hidden, the
definition site is one click away.

### Browsing (module page, symbol card)

Pure → IO → pure, so `Extract` and `Locate` stay free of IO:

1. **Pure.** Resolve the component and collect the outside modules the
   asked-for module's exports actually need:

   ```haskell
   -- Hypha.Search.Reexport
   outsideModulesFor :: [ModuleInterface] -> ModulePath -> [ModulePath]
   ```

2. **IO** (server layer). For each such module, find its owner from the
   plan alone — no source reads:

   ```haskell
   moduleOwner :: BuildPlan -> PackageId -> ModulePath
              -> Maybe (PackageId, Comp.ComponentKind)
   ```

   walking `puDeps` → each dependency's `puLibComponents` →
   `ciExposedModules`. Then load and read just those modules.

3. **Pure.** `resolveModuleEntries` takes the extra sources keyed by
   module and builds their entries identically to local ones — real
   haddock, real signature — tagged `EntryReexport` with the owning
   component.

One extra parse per page view, not per index pass. `Source.Locate` takes
the same extra sources so the symbol card finds the snippet in the
owning package's tree.

## Error handling

Every path that currently drops an export without a word gains a report.
A symbol missing from the index or from a page is invisible, and the user
has no other way to find out.

- **Env miss** — the dependency is unindexed, or its module failed to
  parse. No row, and the export is recorded in
  `ComponentIndex.ciUnresolvedExports` and traced to stderr alongside the
  existing parse-failure and name-mismatch reports:

  ```haskell
  data UnresolvedExport = UnresolvedExport
    { ueModule   :: !ModulePath   -- ^ the module that exports it
    , ueName     :: !SymbolName
    , ueExpected :: !ModulePath   -- ^ the import we believed supplies it
    }
  ```
- **Ambiguous env hit** after dependency filtering — deterministic
  lexicographic winner, reported through `Ambiguity`.
- **`moduleOwner` miss on a page** — the entry is still listed, with its
  origin and without a signature, plus a page-level note. Listing a name
  we cannot describe beats omitting it.
- **No build plan** (`hypha module` / `hypha source` on a bare package
  directory) — there is no dependency graph, so cross-package entries
  report that the definition lives in another package instead of
  degrading silently.

## Testing

- **Fixture.** A second package under `test/fixtures/reexport-dep/`
  whose module a `test/fixtures/reexport/` façade re-exports — the
  `base`/`ghc-internal` shape in miniature.
- **Unit.**
  - `lookupExport` honours the dependency filter and reports ambiguity.
  - `indexParsedComponent` with a seeded env emits the façade row
    carrying the dependency's signature and `DefinitionRef`, and records
    an unresolved export when the env has no answer.
  - `collapseRows` folds rows from two components sharing a
    `DefinitionRef` into one result; the `base`-shaped presentation wins;
    `definitionHref` points at the dependency's component.
  - `topologicalOrder` puts dependencies first and keeps plan-unknown
    units in input order.
  - `resolveModuleEntries` renders a cross-package entry with haddock
    and signature, and a signature-less entry when the owner is unknown.
- **Property (falsify).** `topologicalOrder` is a permutation of its
  input.
- **Golden.** Search result and module page for a cross-package
  re-export.
- **Real re-index plus `scripts/index-audit.sh`.** Not optional. The
  previous round of this work had 273 green tests and two bugs that only
  a real pass exposed: an exponential resolver, and `cpphs` raising
  `error` from pure code. Success criteria:
  - `base` rows rise from 308 to the low thousands.
  - Searching `mapAccumL` shows `base:Data.Traversable` first, with the
    `ghc-internal` definition behind the `+N` affordance.
  - `/pkg/base/Data.Traversable` lists its entries with haddock.
  - Unresolved-export reports are a bounded, explainable set rather than
    thousands of lines.
  - Zero name mismatches; no regression in the parse-failure count.
- **Cross-GHC.** `cabal build all` against 9.6.7, 9.10.3 and 9.12.4, as
  the previous round did.

## Out of scope

- The `MIN_VERSION_*` / `CALLCONV` CPP synthesis still open from the
  previous round (159 modules skipped). Unrelated to this issue.
- `doctor`'s index-health surface, also still open.
- Re-exports through more than one package hop are covered *only*
  because a dependency's rows already carry their resolved
  `DefinitionRef`. No explicit multi-hop search is implemented, and none
  is needed for the façade shape this issue is about.
