# Hypha Server — Index, Parse and Search Correctness

- **Date:** 2026-07-27
- **Status:** Draft (design)
- **Author:** Alfredo Di Napoli (with Claude Code)
- **Branch:** `adinapoli/more-server-improvements`

## Problem

Nine defects, reported against the running server or found while
verifying those reports, share three root causes: the parser guesses at
language extensions instead of reading them, the indexer derives module
identity from file paths instead of from the module, and re-exports are
resolved by symbol *name* instead of by definition *site*. Everything
below follows from those three.

### Reported

1. **No Hackage link for boot libraries.** `/pkg/containers` shows no
   "↗ Hackage" affordance, `/pkg/ListLike` does.
   `Ui/Tree.hs:119` ends `hackageLink _ _ _ = mempty`, and `containers`
   is `OriginDistribution`, not `OriginHackage`. Every distribution
   package *is* published on Hackage, so the link is correct there.

2. **Package-name matches do not rank first.** Searching `containers`
   should offer the package itself as the first result. The index holds
   only symbol rows (`pkg_index(pkg, version, mod, name, sig)`), so the
   query scores 150 via a package-field infix hit and competes with
   thousands of symbol rows that score up to 1000.

3. **Lowercase / path-shaped module names in results.** Confirmed live
   in the cache, not a stale binary:

   ```text
   async:exe:concasync|2.2.6|concasync
   cryptohash-sha256|0.11.102.1|src-bench.bench-sha256
   ghc-lib-parser|9.10.3.20250912|compiler.GHC.Data.Word64Map.Internal
   ```

   `enumModulesIn` walks the source dirs and dots the *relative file
   path*; `examples/concasync.hs` becomes the module `concasync`, and a
   component whose `hs-source-dirs` we failed to resolve yields
   `compiler.GHC.…`. 329 of 58 223 cached rows carry a module name that
   is entirely lowercase.

4. **`.Internal` modules surface instead of their flagship wrapper.**
   Commit `ac41fc4` added flagship rows, so `containers|Data.Map.Strict|
   insertWith` now exists — but the `.Internal` row still exists beside
   it with no ordering preference, so which one the user sees is
   arbitrary.

5. **Wrapper modules have an empty "On this page" rail.**
   `/pkg/containers/Data.Map.Strict` lists no entries because
   `Extract.extractModuleDoc` reports only *locally declared*
   declarations and that module declares almost nothing;
   `mdiEntries = []` makes `tocRail` render nothing.

6. **`/pkg/containers/Data.Map.Internal` fails with "module source
   could not be parsed: parse error".** `Source/Parser.hs:214` carries a
   hand-written `enabledExtensions` whitelist and ignores the module's
   own `{-# LANGUAGE #-}` pragmas. `Data/Map/Internal.hs:476` is
   `type role Map nominal representational`, which needs
   `RoleAnnotations` — absent from the list, as is the `MagicHash` that
   module also declares.

### Found while verifying

7. **The module qualifier is ignored when locating a definition.**

   ```console
   $ hypha source 'containers/Data.Map.Internal/balanceL'
     path: …/containers-0.7/src/Data/Set/Internal.hs
   $ hypha source 'containers/Data.Map.Internal/insertWith'
     path: …/containers-0.7/src/Data/Map/Strict/Internal.hs
   ```

   `scanFile` (`Locate.hs:373`) turns the item-6 parse failure into
   `Left _ -> Nothing`, indistinguishable from "the symbol is not in
   this module", so `locateSymbolDefinitionInDir` falls through to
   `findInTree`, which sweeps the whole package. Its sibling preference
   (`sortByPrefix`) compares a *dotted* module prefix against *slashed*
   file paths, so it stops discriminating after the first path segment.

8. **Snippet text and line number can come from different files.**
   For `Data.Map.Internal/balanceL` the reported line 1746 is
   `balanceL x l r = case r of` in `Data/Set/Internal.hs` (the swept
   file), while the rendered snippet is line 1746 of
   `Data/Map/Internal.hs` — a Haddock comment. `SourceLocation` already
   carries `slPath`; the snippet path is resolved a second time
   independently, so the two can disagree.

9. **Stale rows are never invalidated.** Every bug above has been
   writing rows into `~/.cache/hypha/hypha.db` for weeks, and nothing
   in the cache identifies which code version produced them.

### Consequential damage

Item 6 also poisons the index. When a module fails to parse, the
indexer logs a warning and keeps going, so the module contributes no
local rows — but the re-export pass then invents rows for it, resolving
signatures through a **name-keyed** map (`Command/Server.hs:719`).
Result, live in the cache today:

```text
containers|0.7|Data.IntMap.Lazy|insertWith|
  insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
```

`Data.IntMap.Lazy.insertWith` has been given `Data.Map`'s signature.
Any design that resolves re-exports by name alone has this bug latent
in it.

## Goals

- A module's identity, exports, imports and declarations come from its
  parse tree; the file path is evidence, not authority.
- The parser's language settings come from the module's pragmas and its
  component's cabal stanza, never from a curated list.
- Re-exports resolve to a **definition site** (module *and* name), so
  signatures are read rather than guessed and "same symbol" is a fact,
  not a heuristic.
- Search offers packages, modules and symbols as distinct entities and
  ranks the most public presentation of a symbol first.
- Every failure that degrades output is typed and surfaced.

## Non-Goals

- Popularity or reverse-dependency ranking. Ordering *within* a kind
  stays lexical/structural for now; a revdeps score is a separate
  project.
- Generating Haddock where none exists.
- Changing the MCP tool surface or the CLI's YAML shape (fields may be
  *added*; nothing renamed or removed).
- Full CPP fidelity. We have no compiler session, so macros that only a
  real GHC invocation defines stay out of reach (§1.4 narrows the gap
  where cabal already knows the answer).

## Architecture

Four new modules, each with one job, replacing logic currently spread
across `Command/Server.hs` (817 lines) and `Source/{Parser,Locate}.hs`:

| Module | Responsibility |
|---|---|
| `Hypha.Source.Extensions` | Resolve `LanguageSettings` + pragmas → `EnumSet Extension`. |
| `Hypha.Source.Interface` | One parse → `ModuleInterface` (name, exports, imports, decls, header doc). |
| `Hypha.Search.Reexport` | Pure: `[ModuleInterface]` → definition site per exported name. |
| `Hypha.Search.Index` | Row types, build, hydrate, cache I/O. Moved out of `Command/Server.hs`. |

Data flow, once per component:

```text
cabal stanza ──► LanguageSettings ──┐
                                    ├─► Source.Interface.parseInterface
module source ──► pragmas ──────────┘            │
                                                 ▼
                                        [ModuleInterface]
                                                 │
                            Search.Reexport.resolveComponent
                                                 │
                                                 ▼
                            Map (ModulePath, SymbolName) DefinitionSite
                                                 │
                    ┌────────────────────────────┼──────────────────────┐
                    ▼                            ▼                      ▼
             Search.Index rows            module page entries    symbol card / locate
             (+ SQLite, + memory)         ("On this page")       (def site + snippet)
```

`ModulePath` and `SymbolName` already exist in `Hypha.Types.SymbolPath`
and are adopted throughout the index and search layers, which currently
pass bare `Text`. Four newtypes join them, each replacing a `Text` that
is "really" something else at a boundary we are already touching:

| Type | Was | Home |
|---|---|---|
| `Signature` | `Text` (`sig` column, `srSignature`) | `Hypha.Types.SymbolPath` |
| `ComponentKey` | `Text` from `componentKey` | `Hypha.Types.ComponentName` |
| `SrcLine` | `Int` line numbers | `Hypha.Source.Interface` |
| `UnknownExtension` | dropped on the floor | `Hypha.Source.Extensions` |

## §1 Parse layer — language settings, not a whitelist

### 1.1 Extension resolution

`Source/Parser.hs`'s `enabledExtensions :: EnumSet Extension` constant
is deleted. Extensions are resolved per module, in the order GHC itself
uses:

```haskell
-- | Language settings a component's cabal stanza fixes for every
-- module in it.
data LanguageSettings = LanguageSettings
  { lsLanguage         :: !(Maybe Language)   -- ^ @default-language@
  , lsDefaultOn        :: ![Extension]        -- ^ @default-extensions@
  , lsDefaultOff       :: ![Extension]        -- ^ its @No…@ entries
  }

resolveExtensions
  :: LanguageSettings
  -> [PragmaExtension]        -- ^ from the module's own pragmas
  -> (EnumSet Extension, [UnknownExtension])
```

1. Base: `languageExtensions (Just GHC2021)`, unioned with
   `languageExtensions lsLanguage` when the stanza names one. GHC2021 is
   a *floor*, not a substitute: we are a reader, and a permissive floor
   can only widen the syntax we accept. Extensions that change parses
   rather than widen them (`TemplateHaskell`, `UnboxedTuples`,
   `Arrows`, `TransformListComp`, `LinearTypes`, `OverloadedRecordDot`,
   `MagicHash`) are *not* in that floor, so they still require an
   explicit pragma — exactly the compiler's behaviour.
2. Apply `lsDefaultOn` / `lsDefaultOff`.
3. Apply the module's pragmas last, in source order, so a later `No…`
   wins.

### 1.2 Pragma extraction

`GHC.Parser.Header.getOptions` (shipped by `ghc-lib-parser`, verified
present in 9.10.3.20250912 and in range for our `>= 9.10 && < 9.13`
bound) lexes `{-# LANGUAGE #-}` and `{-# OPTIONS_GHC -X… #-}` into
`[Located String]`. `-X<name>` is mapped to `Extension` through
`GHC.Driver.Session.xFlags` — the same table GHC's flag parser uses, so
aliases (`Rank2Types`, `NamedFieldPuns` → `RecordPuns`,
`GeneralisedNewtypeDeriving`) come for free and we hand-maintain
nothing. `-XHaskell2010` / `-XGHC2021` / `-XGHC2024` route through
`languageExtensions`.

A name the table does not know becomes an `UnknownExtension` in the
result — surfaced (stderr trace on the index path, view reason on the
page path), never dropped. Extraction needs a lexer, and the lexer
needs options: we bootstrap with the GHC2021 floor to read pragmas,
then re-initialise with the resolved set. GHC bootstraps the same way.

### 1.3 Typed parse errors

```haskell
data ParseError = ParseError
  { peMessage  :: !Text        -- ^ GHC's rendered diagnostic
  , peLocation :: !(Maybe SrcLine)
  , peUnknownExtensions :: ![UnknownExtension]
  }
```

`L.PFailed` currently collapses to the literal string `"parse error"`,
which is what the user reads on the module page. GHC's `PsMessages` are
available from the failed parser state; render them and keep the line.
`ViewExportsOnly`'s reason then says *what* failed and *where*.

### 1.4 CPP: measure first, then decide

`needsCpp`/`cpphs` stay as they are for the core work. Two facts make
the current setup lossy: `#include "containers.h"` cannot resolve
because includes are held off to keep the pipeline pure, and
`MIN_VERSION_*` macros are undefined because cabal, not us, generates
them.

Both are fixable *in principle* — the component's `include-dirs` are in
the cabal stanza, and every `MIN_VERSION_x(a,b,c)` is derivable from
the build plan's own dependency versions — and doing so would remove
the `unsafePerformIO` that only exists to hide cpphs's `IO`. But we do
not know yet whether any module still fails after §1.1–§1.3.

So §1 ships with instrumentation: the indexer counts modules that fail
to parse, per package, and `hypha doctor` reports the total. Baseline
before, measurement after. §7 is designed below and implemented only if
that number is non-zero.

## §2 Locate layer — no silent sweeps, no mismatched pairs

### 2.1 Distinguish failure from absence

```haskell
scanFile :: SymbolName -> FilePath -> IO (Either ParseError (Maybe SourceLocation))
```

`Left` means "this module could not be parsed"; `Right Nothing` means
"parsed fine, symbol not declared here". Only the second justifies
looking elsewhere. The first is reported.

### 2.2 Resolve re-exports, do not sweep

When the requested module parses and does not declare the symbol,
`Search.Reexport` (§3.2) answers *which module defines it*, from
imports and export lists. `findInTree`'s package-wide sweep survives
only for packages whose cabal we could not parse, and when it fires the
returned location records that it was a guess, so the symbol card can
say so instead of presenting a swept file as fact:

```haskell
data LocatedDefinition = LocatedDefinition
  { ldLocation   :: !SourceLocation
  , ldProvenance :: !Provenance
  }

data Provenance
  = Resolved !DefinitionSite
  | GuessedBySweep !Text        -- ^ why resolution was unavailable
```

`sortByPrefix`'s dotted-vs-slashed comparison is deleted along with the
hand-rolled `sortBy`; sibling preference is computed on `ModulePath`
segments (`Data.Map.Internal` vs `Data.Set.Internal` share one segment,
not four characters).

### 2.3 One location, one file

`Command/Source` and the server's symbol card take the snippet from
`slPath` of the `SourceLocation` they were handed. The second,
independent file resolution is removed, so a line number can no longer
be paired with a different file's text — the mismatch becomes
unrepresentable rather than merely fixed.

## §3 Index layer — module identity and definition sites

### 3.1 Module enumeration and naming

Enumeration comes from the cabal stanza: `exposed-modules` plus
`other-modules` (`ComponentInfo` gains `ciOtherModules`,
`ciDefaultExtensions`, `ciLanguage`). Stray scripts under a source dir
are no longer mistaken for modules, and `hs-source-dirs` we cannot
resolve no longer bleed path segments into names.

The name we *store* is the parse tree's `hsmodName`. When it disagrees
with the path we enumerated, the parse tree wins and the disagreement
is warned about — that combination (`src/Foo.hs` declaring
`module Bar`) is real in the wild and silently trusting either side
produces unreachable rows.

Path walking survives as the fallback for packages with no parsable
cabal, and rows produced that way are marked, so we can see how often
it happens.

### 3.2 Definition sites

```haskell
-- Hypha.Search.Reexport (pure)
data DefinitionSite
  = DefinedHere
  | DefinedIn      !ModulePath   -- ^ same component, resolved
  | DefinedOutside !ModulePath   -- ^ cross-package: the module we import from
  deriving stock (Eq, Show)

-- | Why a definition site was chosen, when more than one could have
-- been. Kept so the choice is testable and reportable.
data Ambiguity
  = Unambiguous
  | ResolvedAmongst !(NonEmpty ModulePath)  -- ^ rejected candidates

resolveComponent
  :: [ModuleInterface]
  -> Map (ModulePath, SymbolName) (DefinitionSite, Ambiguity)
```

Algorithm, per exported name of each module:

1. Declared locally → `DefinedHere`.
2. Otherwise, consider imports that could supply the name (explicitly
   listed, or unrestricted). For each import target inside the
   component, recurse into *its* exports. Memoised DFS with a visited
   set, so an import cycle terminates instead of diverging.
3. No candidate inside the component → `DefinedOutside`, naming the
   import we believe supplies it. These rows carry no signature and are
   not indexed as definitions, but the module page can still list the
   name (§5).
4. More than one candidate → pick by longest shared `ModulePath`
   segment prefix, then lexicographically, and record `Ambiguity` so
   the choice is testable and reportable rather than an accident of
   list order.

Export items of the form `module Data.Map.Internal` are expanded to
that module's exports; `Type(..)` bundles contribute their
subordinates.

This is what fixes items 3–5 at once and retires the name-keyed map
that gave `Data.IntMap.Lazy.insertWith` a `Map` signature: a signature
is read from the module the algorithm resolved to, or the row is not
written.

### 3.3 Row type and storage

```haskell
data IndexRow = IndexRow
  { rowComponent  :: !ComponentKey
  , rowModule     :: !ModulePath
  , rowName       :: !SymbolName
  , rowSignature  :: !Signature
  , rowDefModule  :: !ModulePath    -- ^ == rowModule when declared here
  , rowVisibility :: !Visibility
  }

data Visibility = Exposed | Internal   -- ^ cabal exposed- vs other-modules
```

`pkg_index` gains `def_mod TEXT NOT NULL` and `visibility TEXT NOT
NULL`. `readIndex`/`writeIndex` swap their four-tuples for `IndexRow`.

### 3.4 Cache invalidation

A `kv` entry `index_format` records the row-format generation. On
`openIndexCache`: ensure schema, read the key, and when it is absent or
older than the current generation, delete every row from `pkg_index`
and `pkg_index_meta`, then write the key. One wipe, one re-index, no
`rm` in the release notes. `migrateAddColumn` stays for the schema step
so opening an old DB never throws before the wipe runs.

## §4 Search layer — entities, ranking, collapse

### 4.1 Three kinds of result

```haskell
data ResultKind
  = KindPackage
  | KindModule
  | KindSymbol
```

`IndexedRow` gains the kind and keeps its precomputed lowercase fields.
Package and module rows are *synthesised* from the rows already in
hand, on both the build path and the hydrate-from-cache path, so no new
storage and no divergence between the two.

Scoring adds a kind-aware term above the existing field scores: an
exact package-name hit outranks an exact module hit, which outranks any
symbol hit. `containers` lands `/pkg/containers` first;
`Data.Map.Strict` lands the module page.

### 4.2 Collapse by definition

A package result has no signature and a module result has no definition
site, so `SearchResult` is a sum rather than a record with fields that
are meaningless for two of its three shapes:

```haskell
data SearchResult
  = ResultPackage !PackageName !Version
  | ResultModule  !ComponentKey !ModulePath !Visibility
  | ResultSymbol  !SymbolResult

data SymbolResult = SymbolResult
  { srComponent  :: !ComponentKey
  , srModule     :: !ModulePath      -- ^ the presentation module
  , srName       :: !SymbolName
  , srSignature  :: !Signature
  , srDefModule  :: !ModulePath
  , srAlternates :: !Int             -- ^ collapsed siblings
  }

resultHref :: SearchResult -> Text
```

`ResultKind` (§4.1) stays as the *scoring* discriminant on
`IndexedRow`, where the flat, precomputed representation is what keeps
per-keystroke scoring allocation-free; `SearchResult` is the rendering
type, built once per response.

Symbol hits group by `(component, defModule, name)`. The winner is the
most public presentation: `Exposed` before `Internal`, then a path with
no `Internal` segment, then fewer segments, then lexicographic. The
group's size becomes `srAlternates`, rendered as a small `+2` affordance
that links the definition site, so nothing is hidden — item 4's
requested behaviour with an escape hatch.

Grouping on the *definition* is what makes this safe.
`Data.Map.Strict.insertWith` and `Data.Map.Lazy.insertWith` have
identical signatures and different definition modules
(`Data.Map.Strict.Internal` and `Data.Map.Internal`), so they stay two
results; `Data.Map.Strict.Internal.insertWith` folds into its wrapper.
Grouping by name, or by name and signature, would have merged the first
pair — which is why neither is used.

`resultsFragment`'s `(Text, Text, Text, Text)` tuple is replaced by
`SearchResult`, and the href is built by `resultHref`, not by string
concatenation at the call site.

## §5 Module page — re-exported entries

`DocEntry` gains provenance:

```haskell
data EntryOrigin
  = EntryLocal
  | EntryReexport !ModulePath
```

A wrapper module's page is assembled from its resolved exports (§3.2):
names, signatures and definition modules come from the index when the
component is already indexed — reusing work instead of re-deriving it —
and Haddock text is lifted by parsing only the distinct definition
modules the page actually needs. When the component has no index yet,
resolution runs demand-driven from the requested module's imports, with
a bounded re-export chain depth; hitting the bound is reported in the
view, not silently truncated.

`tocRail` then fills for `Data.Map.Strict`, and each re-exported entry
says "re-exported from `Data.Map.Strict.Internal`" with a source link
to the definition, which is where the code genuinely lives.

`ViewExportsOnly` remains the last resort, now carrying §1.3's typed
reason.

## §6 Hackage link

```haskell
hackageLink :: PackageName -> Version -> PackageOrigin -> Html ()
```

`OriginHackage` and `OriginDistribution` both render a versioned link
(`/package/containers-0.7`); local, source-repo and tarball origins
still render nothing, because for those the version genuinely does not
identify a Hackage listing. A boot library from an unreleased GHC can
404 — rare, and honest, which beats linking a version we are not
building against. The `Text`/`Text` parameters become
`PackageName`/`Version` on the way past.

## §7 CPP fidelity (conditional on §1.4's measurement)

Designed now, implemented only if modules still fail to parse:

```haskell
data CppEnv = CppEnv
  { ceIncludeDirs :: ![FilePath]        -- ^ component @include-dirs@
  , ceDefines     :: ![(Text, Text)]    -- ^ synthesised MIN_VERSION_*
  }
```

`#include` resolves against the component's own include dirs, so
`containers.h` is found. `MIN_VERSION_pkg(a,b,c)` macros are generated
from the build plan's resolved dependency versions — the same
information cabal uses to generate them. cpphs then runs in honest
`IO`, and `Source/Parser.hs`'s `unsafePerformIO` (whose only
justification is hiding that `IO`) goes away.

## Testing

**Fixtures.** A new `test/fixtures/reexport/` package: an `Internal`
module using a role annotation, `MagicHash` and `#include`; a wrapper
re-exporting it; a `Strict` variant so the strict/lazy pair pins the
collapse rule; a sibling with a same-named symbol so the ambiguity rule
is exercised; a stray `script.hs` under the source dir; and a module
whose `hsmodName` disagrees with its path. Real containers source stays
out of the test tree — the fixture asserts the same shapes and does not
depend on the store.

**Unit (HUnit).**
- `resolveExtensions`: `No…` override, alias mapping, unknown name
  reported, cabal `default-extensions` honoured, `GHC2021` floor.
- `ParseError` carries GHC's message and a line for a deliberately
  broken module.
- `resolveComponent`: local, one-hop, chained, `module M` re-export
  form, cross-package `DefinedOutside`, cycle terminates, ambiguity
  recorded and resolved deterministically.
- `scanFile`: `Left` on unparsable, `Right Nothing` on absent.
- Collapse ordering and `srAlternates` counting.
- `hackageLink` for all six origins.

**Golden.** Index rows for the fixture; the wrapper module page;
`/search` results for `reexport` (package first), `Fixture.Wrapper`
(module first) and the collapsed symbol.

**Property (falsify).**
- Collapse preserves the set of `(component, defModule, name)` keys —
  it never merges two definitions and never drops a name.
- For every package in a generated plan, querying its exact name puts
  its package row first.
- Every stored module name parses as a dotted Haskell module name
  (upper-case initial segment per component).

**Regression on the real cache.** After the wipe and re-index of this
project's own plan: zero rows whose module name is entirely lowercase,
zero rows whose module name contains a source-dir segment, and
`Data.IntMap.Lazy.insertWith`'s signature mentions `IntMap`.

## Risks

- **`ghc-lib-parser` API drift.** `getOptions` and `xFlags` are stable
  across 9.10 and 9.12, the only versions our bound admits, and CI
  builds both project files. If a signature does differ, the shim is
  confined to `Source.Extensions`.
- **One-time re-index cost.** The wipe forces a full rebuild on next
  server start. The progress bar already covers this path; the indexer
  runs in the background and search reports "building".
- **Demand-driven module loading on large packages.** Bounded chain
  depth plus index reuse keeps the module page off the pathological
  path; the bound is reported when hit.
- **`Command/Server.hs` surgery.** Moving indexing to
  `Search.Index` touches a 817-line module that also owns the server
  wiring. Mechanical extraction first, behaviour changes after, so the
  diff stays reviewable.
