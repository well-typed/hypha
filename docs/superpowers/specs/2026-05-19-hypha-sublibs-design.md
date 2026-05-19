# Hypha: Private (sub-)libraries support — design

**Date:** 2026-05-19
**Status:** Draft, pending implementation

## Problem

`hypha server` browses the project's build plan and indexes every unit's
exported symbols.  Local packages with private/internal libraries
(cabal `library <name>` stanzas) are partially handled:

- `plan.json` lists the sub-component, so its dependencies feed the
  graph correctly.
- The indexer walks the parent package's source tree with a heuristic
  root list (`src/`, `library/`, `lib/`, …) and a bounded directory
  scan, so any sublib whose `hs-source-dirs` happens to fall under
  those roots is partially picked up.
- Anything else — e.g. `library internal hs-source-dirs: internal-src`
  — is silently invisible to the search index, the sidebar, and the
  module/symbol pages.

Concretely: on the `nike` package (which has private libraries), every
sublib symbol is unreachable from `hypha server`.

## Goal

Treat every library component (main + sublib) of every package in the
plan as a first-class browsable unit.  Sublibs appear as separate
sidebar entries (cabal-style `pkg:sublib`), get their own URL routes,
have their own cached index rows, and are searchable just like
top-level packages.

Scope: local packages **and** dependencies.  Sublib parsing happens
once per `(pkg, version)`; the SQLite cache persists the result, so
the cost is paid only on first encounter.

## Non-goals

- Hierarchical sidebar UI (parent collapses into children).  Flat list
  of `pkg:sublib` entries is enough for now; can be layered on later.
- Schema migration of the existing cache.  We reuse the existing
  `pkg`-keyed rows by storing the composite `nike:lib-foo` in the
  `pkg` column.
- Indexing executables/tests/benchmarks.  Libraries only.

## Approach

Three concrete layers change.

### 1. Component discovery (new module `Hypha.Project.Components`)

```haskell
data ComponentInfo = ComponentInfo
  { ciSublib       :: !(Maybe Text)   -- Nothing = main library
  , ciHsSourceDirs :: ![FilePath]     -- absolute paths
  }

parseLibComponents
  :: FilePath  -- ^ cabal file path
  -> FilePath  -- ^ package root (for resolving relative source dirs)
  -> IO [ComponentInfo]
```

Implementation:

- `Distribution.PackageDescription.Parsec.parseGenericPackageDescription`
  (from `Cabal-syntax`, new direct dep; already transitively present
  via `cabal-install`).
- Flatten `condLibrary` (main) + `condSubLibraries`.  Take the
  unconditional baseline of the cond-tree: this is enough for source
  layout in practice.
- Per stanza, read `libBuildInfo . hsSourceDirs`; resolve each entry
  against the package root.  Empty list (cabal default) → `[pkgRoot]`.
- Returns `[]` on parse failure or missing cabal file.  Callers fall
  back to the existing heuristic root walk so packages with broken
  cabal files keep behaving as today.

### 2. Build-plan extension

`Hypha.Types.BuildPlan.PlannedUnit` gains:

```haskell
, puLibComponents :: ![ComponentInfo]
```

Populated by `Hypha.Project.Plan.toPlannedUnit`:

- Local packages (`puIsLocal = True`): glob `<puSrcDir>/*.cabal`,
  take the first match (cabal projects forbid more than one).
- Dependencies: glob `<sourceCacheDir>/<pkg>-<ver>/*.cabal`, same
  rule.

A package-name-matched filename is not required — cabal allows the
file to be named anything ending in `.cabal`.

A new `Hypha.Project.Components.componentsFor :: PlannedUnit -> IO [ComponentInfo]`
encapsulates the lookup + parse with the fallback to `[]`.

### 3. Composite name plumbing

New small module `Hypha.Types.ComponentName`:

```haskell
data ComponentName = ComponentName
  { cnPackage :: !PackageName
  , cnSublib  :: !(Maybe Text)
  }

parseComponentName  :: Text -> ComponentName    -- splits on first ':'
renderComponentName :: ComponentName -> Text    -- rejoins with ':'
```

The Server layer threads `ComponentName` (or its rendered form) through
five places:

- `App.scPackages :: [Text]` — list of rendered names for the sidebar.
- `App.scModuleExports :: Text -> Text -> IO [Text]` — `pkg` arg is now
  composite.
- `App.scSymbolLookup`, `App.scSourceText`, `App.scPackageInfo` — same.
- `Package.Resolver` gains `resolveComponent :: ComponentName -> IO
  (Either ResolveError ResolvedComponent)` returning `(parent
  ResolvedPackage, [FilePath] sourceDirs)`.
- `Command.Server` indexer iterates `(PlannedUnit, ComponentInfo)`
  pairs instead of `PlannedUnit`; the cache row key uses the rendered
  composite name in the existing `pkg` column.

URLs carry `:` percent-encoded: `nike%3Alib-breakdown`.  Servant
`Capture "pkg" String` decodes automatically; one new helper
`urlPkg :: ComponentName -> Text` emits the encoded form in `href`
values.  Display text stays the human-readable `nike:lib-breakdown`.

## Data flow

1. **Startup**.  `loadBuildPlan` builds the unit map; for each unit
   `componentsFor` is called and the result stored.  Parse failures
   are logged once + ignored.

2. **Indexer**.  `buildAndCacheIndex` iterates units; for each unit
   emits one job per `ComponentInfo`.  Job key =
   `(PackageId, Maybe Sublib)`.  Cache `haveIndex` / `readIndex` /
   `writeIndex` continue to use a single `pkg` column with the
   composite name stored verbatim.  Hydrate path mirrors this.  The
   done-counter bumps once per **unit** (not per component) so the
   progress bar stays calibrated to package count.

3. **Search**.  Index rows already carry the composite name.  The
   fuzzy scorer is unchanged — the qualifier haystack becomes
   `nike:lib-breakdown.Foo.Bar.baz`, which still matches sensibly.
   Results render to `/pkg/nike%3Alib-breakdown/...`.

4. **Symbol / source / module pages**.  `pkg` capture decoded by
   Servant → handed to the resolver.  `parseComponentName` peels off
   the optional sublib suffix.  Source dir candidates come from the
   matching `ComponentInfo.ciHsSourceDirs`.

## Error handling

- Missing or unparseable `.cabal` → `puLibComponents = []`; indexer
  falls back to the existing heuristic root walk (current behaviour
  preserved).
- URL refers to an unknown sublib → `resolveComponent` returns
  `Left ComponentNotFound`; pages render the existing
  "Package … not found" warning.
- Empty sublib suffix (`pkg:`) treated as `pkg`.
- Sublib name containing `:` is impossible in cabal grammar; we don't
  guard against it.

## Cache impact

No schema migration.  Existing `pkg_index_meta` + `pkg_index` rows
gain entries keyed on composite names.  A user upgrading from the
prior hypha can simply blow away the cache, or let it warm up the
sublib rows on first run while existing main-library rows are reused.

## Performance

- One extra `.cabal` parse per package on startup — amortised by
  hot-disk read.  243-package build plan: ~1–2s once at boot, before
  any HTTP traffic.
- Indexer fanout grows with sublib count, which is small in practice
  (single-digits per affected package).
- Cache rows roughly stable: sublibs add their exports but no main-lib
  rows are duplicated (per-component source roots no longer overlap).

## UI

Sidebar lists each component as its own item.  Sublibs render with the
prefix `:sublib-name` after the parent (e.g. `nike:lib-breakdown`).
The package overview page (`/pkg/<name>`) shows the version + the list
of modules belonging to that component only.

No new CSS strictly required; existing `.module-list` styling
suffices.  A tiny tweak adds a muted `<span class="sublib-tag">`
treatment for the `:sublib` suffix to set sublib entries apart
visually.

## Testing

- **Unit / property — `Hypha.Types.ComponentName`**: QuickCheck
  round-trip `parseComponentName . renderComponentName == id`; check
  that names without `:` round-trip with `cnSublib = Nothing`.
- **Unit — `Hypha.Project.Components`**: golden fixture cabal with one
  main library (`hs-source-dirs: src`) plus two sublibs
  (`library internal hs-source-dirs: internal-src`,
  `library bench hs-source-dirs: bench-src`).  Assert
  `parseLibComponents` returns three components with the expected
  names + absolute paths.
- **Unit — `Hypha.Project.Plan`**: extend the test fixture to assert
  `puLibComponents` is populated for the local fixture package.
- **Cache round-trip**: write `("nike:lib-breakdown", "0.1", rows)` to
  `Cache.writeIndex`, read it back via `Cache.readIndex`, assert
  equality.
- **Golden — `Golden.Server`**: extend the home-page fixture so the
  sidebar tree includes one sublib entry; verify HTML diff is exactly
  the new entry + its `href` is percent-encoded.
- **Manual smoke**: run `hypha server` against `nike`; confirm sidebar
  lists `nike` + every `nike:<sublib>`; click `nike:lib-foo`; module
  list shows sublib modules; pick a symbol; symbol card renders.

## Out of scope (future)

- Hierarchical / collapsible sidebar grouping per parent package.
- Indexing test-suite / benchmark / executable components.
- Resolving cross-component re-exports inside a multi-lib package.
- Custom build-tool sublib variants (`build-tool-depends`).
