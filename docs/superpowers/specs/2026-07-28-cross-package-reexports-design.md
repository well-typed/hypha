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
identity. The definition link already reads the consequence wrongly: it
is built from the *presentation's* component and the *definition's*
module. With cross-package rows in the
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
data ExportChoice = ExportChoice
  { ecChosen   :: !Export
  , ecRejected :: ![DefinitionRef]   -- ^ empty when the choice was forced
  }

lookupExport :: Set PackageName    -- ^ the asking unit's dependencies
             -> ModulePath -> SymbolName -> ExportEnv -> Maybe ExportChoice
```

`Nothing` means no dependency of this unit exports that pair. A `Just`
always carries a chosen `Export`, with `ecRejected` saying whether the
choice was forced or made among candidates — the same
resolved-but-reportable shape `Reexport.Resolution` uses. It does not
reuse `Reexport.Ambiguity`, whose payload is a `NonEmpty ModulePath`:
here the rejected candidates differ by *component*, so a module alone
could not name them.

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

A folded-in presentation is linked from its own component, and the
defining module from `drComponent (srDefinition s)` — never from the
presentation the reader landed on, since a re-export crosses package
boundaries and `/pkg/base/GHC.Internal…` is a module `base` does not
have.  As shipped this is one function, `presentationHref`, applied to
the definition seen as a presentation.

The `+N` affordance keeps its existing meaning: nothing is hidden, the
definition site is one click away.  The disclosure lists one row per
folded-in presentation and tags the defining one rather than repeating
it — the defining module is usually a presentation as well, so a
separate "defines it" row named it twice and opened `N+1` rows.

### Browsing (module page, symbol card)

Browsing asks the index, because the index holds the only *transitively*
resolved answer.  This section originally proposed deriving the owner
from the plan — `Reexport.outsideModulesFor` plus a
`BuildPlan.moduleOwner` walking `puDeps` → `puLibComponents` →
`ciExposedModules`.  Both were written and then deleted: `puLibComponents`
is filled from a unit's unpacked `.cabal`, which only local units have, so
**every dependency's component list is empty** and `moduleOwner` could
never resolve one.  Measured on the real plan: `hypha` has 81 exposed
modules, `base` / `ghc-internal` / `containers` have `[]`.

What ships instead:

1. **IO** (server layer).  `Cache.lookupRowsInModule` returns the rows
   the index already holds for the asked-for module, each carrying a
   resolved `DefinitionRef`.  For every definition in another component,
   resolve that component's source directory and load just the module
   named.  The result is an `ImportedDefinitions` — the sites, plus the
   sources that answer them.

2. **IO.** `resolveModuleEntries` and `locateDefinitionInComponent` take
   it, parse each module once under the component's own language
   settings, and build entries identically to local ones — real haddock,
   real signature — tagged `EntryReexport` with the defining component.

Both parse in `IO` rather than purely, because `cpphs` reports an
undefined build-time macro by calling `error` from pure code: unguarded,
one such module answers the page with a 500.  One parse per module per
page view, and the definition the symbol card renders comes from that
parse rather than from a second read of the same file.

An index miss is not an absence: the index may still be building, and
class methods have no rows at all (see the notes below), so both callers
fall back to resolving within the component.

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
- **No definition site for an entry** — the index has no row for it and
  the component cannot resolve it either.  The entry is still listed,
  with its origin and without a signature.  Listing a name we cannot
  describe beats omitting it.
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
    the definition link points at the dependency's component.
  - `topologicalOrder` puts dependencies first and keeps plan-unknown
    units in input order.
  - `resolveModuleEntries` renders a cross-package entry with haddock
    and signature, and a signature-less entry when the owner is unknown.
- **Property (falsify).** `topologicalOrder` is a permutation of its
  input.
- **Golden.** Search result and module page for a cross-package
  re-export.
- **Real re-index, driving the real handlers.** Not optional. The
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

---

## Implementation notes

Filled in from the execution log after the branch landed; the per-task
plan the log came from is not kept.

### Measurement, 2026-07-28

Full re-index of this project's plan on GHC 9.10.3, row format 2 → 3.

| | before | after |
|---|---|---|
| total rows | 61 531 | **63 146** |
| packages | 282 | **283** |
| `base` | 308 | **1450** |
| `ghc-internal` | 1240 | 1245 |
| cross-package rows (`def_pkg <> pkg`) | 0 (unrepresentable) | **1615**, across 33 packages |

Top cross-package consumers: `base` 1142, `serialise` 76, `time-compat`
74, `base-compat` 68, `os-string` 68, `conduit-extra` 35,
`ansi-terminal` 31, `filepath` 26, `tls` 24.  `Data.Traversable`,
`Control.Monad`, `Data.Foldable`, `Data.List`, `Data.Maybe` and
`Prelude` had contributed **zero** rows before this work.

Every one of the ~7200 unresolved-export reports traces back to a module
that does not parse without CPP preprocessing: the module contributes no
rows, and every module re-exporting from it loses exactly what it
re-exported.  That is the tail the 07-27 spec's §7 addresses, and it is
still open.

### The process failure worth recording

The first two attempts at the browsing fix shipped green, on tests that
injected `ImportedDefinitions` directly and therefore never exercised
`moduleOwner` — the one link that was broken.  **Pure-function tests
cannot verify a path whose IO they replace.** Any future change to a
browsing path should be verified by driving `buildServerConfig` and
calling `scSymbolLookup` / `scModuleDoc`, and should leave behind a test
that exercises the producer of the definition-site map, not only its
consumers.

### Found while verifying, NOT fixed: class methods and constructors are never indexed

Pre-existing and unrelated to this work, but larger than the defect this
design set out to fix, so it should not stay unrecorded.

| symbol | rows in the whole index |
|---|---|
| `traverse` | 4 (none in `base` or `ghc-internal`) |
| `fmap`, `Just`, `mempty`, `liftA2` | **0** |

`GHC.Internal.Data.Traversable` yields a row for `Traversable` — the
class — and none for `traverse` or `sequenceA`.  `Parser.findDecl`
matches top-level declarations, and a class's methods and a data type's
constructors are not top-level declarations, so the indexer has no
declaration to read a signature from and writes no row.  The export side
already knows about them: `Traversable(..)` is recorded with its
subordinates.  Fixing it means teaching `Hypha.Source.Parser` to emit
class methods and constructors as declarations of their own, with the
signatures GHC already attaches.

### Deviations from the design

- `lookupExport` returns `Maybe ExportChoice`, not
  `Maybe (Export, Ambiguity)`: here the rejected candidates differ by
  *component*, and a module path alone cannot name them.
- `locateDefinitionInComponent` takes a `ComponentKey` beyond what this
  design described.  It has to report which component the definition is
  in, and deriving that from a module path is the precise mistake the
  change exists to remove.
- `Hypha.Command.Source` was an unplanned second caller.  It gets a
  `ComponentKey` built from the cabal file name and an empty
  imported-module map — a bare package directory has no plan, so no
  dependency graph — and reports the cross-package case rather than
  leaving it silently unresolved.
- `foldl'` is imported qualified from `Data.Foldable`: base 4.20 added it
  to the `Prelude`, so an unqualified import is redundant on 9.10 and
  required on 9.6, and `-Werror` rejects either choice.

### Open risk, flagged rather than designed around

`presentationRank` picks the winner from the shape of the module path, so
a façade package with shorter module names than the package it wraps
would out-rank it.  The tests pin the `base`/`ghc-internal` case and the
determinism, not the general question; the `+N` disclosure is the escape
hatch.  Ranking by whether the project depends on a package directly was
considered and rejected, because it makes search results depend on the
asking project's plan.
