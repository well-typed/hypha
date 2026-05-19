# Hypha: Executable component support — design

**Date:** 2026-05-19
**Status:** Draft, pending implementation
**Builds on:** `2026-05-19-hypha-sublibs-design.md` (sub-library indexing)

## Problem

`hypha server` browses the library components of every package in the
plan — main library + every `library NAME` sub-library — but ignores
executable stanzas entirely.  That means useful targets like `happy`,
`alex`, `hsc2hs`, or the project-local CLI executables are invisible
to the doc browser, even though their `Main.hs` and `other-modules`
sit right next to the library sources.

## Goal

Treat every `executable NAME` cabal stanza as a first-class browsable
unit.  Each appears as its own sidebar entry, has its own URL route,
and its own search-index rows — alongside the main and sub-library
entries we already support.

Scope: same as sublibs — local packages **and** dependencies.  The
indexing cost is paid once per `(pkg, version)` and persists in the
existing SQLite cache.

## Non-goals

- Test suites and benchmarks.  The mental model is "things a user
  might want to read source for"; tests/benches are noisier and rarely
  what someone is looking up.  Add later if there's demand.
- Indexing the `build-tool-depends` graph.  Out of scope.
- Schema migration of the existing cache.  The composite cache key
  scheme expands; no column changes.

## Approach

Build directly on the sub-library plumbing.  The three layers from
the sublib spec — discovery, build-plan model, composite name — each
gain a small extension instead of being rewritten.

### 1. Component discovery — gain a kind tag

`Hypha.Project.Components` currently returns
`ComponentInfo { ciSublib :: Maybe Text, ciHsSourceDirs :: [FilePath] }`.
We replace `ciSublib` with a sum that names the kind explicitly:

```haskell
data ComponentKind
  = MainLib
  | SubLib !Text
  | Exe    !Text
  deriving stock (Show, Eq, Ord)

data ComponentInfo = ComponentInfo
  { ciKind         :: !ComponentKind
  , ciHsSourceDirs :: ![FilePath]
  }
```

`parseLibComponents` is renamed `parseComponents`.  It already walks
`condLibrary` (`MainLib`) and `condSubLibraries` (`SubLib`); it gains
a third branch over `condExecutables`, taking the executable's
`buildInfo . hsSourceDirs` exactly like the library case.

Executables carry an additional `modulePath :: FilePath` (the
`Main.hs`).  We do **not** treat it specially: the existing
`enumModulesIn` walks every `.hs` file under the source roots, and
`parseExports` reads `module Main (main) where` headers correctly,
yielding the expected `main` symbol.

### 2. Composite name grammar

Sub-libraries today use the bare `pkg:foo` form, matching cabal's
shorthand for an unambiguous sub-library reference.  Executables get
the explicit form `pkg:exe:foo`, matching cabal-install's component
disambiguator (`cabal run pkg:exe:foo`).  Plain `pkg` keeps meaning
the main library.

`Hypha.Types.ComponentName` becomes:

```haskell
data ComponentName = ComponentName
  { cnPackage :: !PackageName
  , cnKind    :: !ComponentKind
  }
```

`parseComponentName` recognises the three forms:

- `"nike"`              → `ComponentName "nike" MainLib`
- `"nike:lib-foo"`      → `ComponentName "nike" (SubLib "lib-foo")`
- `"nike:exe:nike-cli"` → `ComponentName "nike" (Exe "nike-cli")`

`renderComponentName` is the inverse.  Edge cases: empty kind tail
(`"pkg:"`, `"pkg:exe:"`) collapses to `MainLib`.

### 3. Disambiguation safeguard

A package may legally have a sublib named `lib-foo` **and** an
executable also named `lib-foo`.  Without the explicit `exe:` infix
both would collide on the cache key `pkg:lib-foo`.  The `exe:` infix
prevents the collision: `pkg:lib-foo` (sublib) and `pkg:exe:lib-foo`
(exe) are distinct rows.

### 4. Indexer + cache + handlers

These already iterate `[(PlannedUnit, ComponentInfo)]`.  Switching
from `ciSublib` to `ciKind` is a mechanical change at the call sites:

- `componentKey` produces:
  - `MainLib`  → `pkgT`
  - `SubLib s` → `pkgT <> ":" <> s`
  - `Exe s`    → `pkgT <> ":exe:" <> s`
- `componentNames`, `componentsForUnit`, `hydrateFromCache`,
  `buildAndCacheIndex` all keep their existing shapes; only the key
  formatter changes.
- `resolveComponentDirs` already finds the matching `ComponentInfo`
  by predicate on `ciSublib`.  The predicate becomes
  `ciKind c == cnKind cn`.

### 5. UI

Sidebar already renders the suffix after the parent name in a muted
`.sublib-tag` span.  Add a second tag style for executables:

- `.sublib-tag` keeps its current treatment (`:lib-foo`).
- `.exe-tag` renders `:exe:foo` in the same muted style with a
  slightly different accent colour (e.g. `--accent-2`) so the
  executable nature is recognisable at a glance.

`Hypha.Server.Ui.Tree`'s `renderEntry` parses three forms: bare,
`:sublib`, `:exe:name`.

URLs use the same percent-encoded form for `:` as today
(`%3Aexe%3A`); Servant decodes once into the raw composite name and
the handler parses it.

## Data flow

Identical to the sublib pipeline; the only material difference is
that the indexer now also walks executable source dirs and writes
extra rows under `pkg:exe:name` cache keys.  Done-counter semantics
unchanged: one bump per unit, not per component.

## Error handling

- `.cabal` parse failure → empty component list → heuristic
  source-root fallback (current behaviour, unchanged).
- URL with unknown component → `Nothing` from `resolveComponentDirs`
  → existing "not found" branch.
- Empty kind tail (`pkg:`, `pkg:exe:`) collapses to `MainLib` —
  consistent with the sublib spec.

## Cache impact

- No schema change.  The `pkg_index` and `pkg_index_meta` tables keep
  composite names in the existing `pkg` column.
- Existing caches survive: sublib + main-library rows are reused
  as-is; executable rows are added on first encounter of the new
  binary.
- A user who wants a clean re-index can delete
  `~/.cache/hypha/hypha.db`.

## Performance

- Cabal parse cost is unchanged — we already parse every `.cabal`
  on startup.
- Indexer fanout grows by however many executables a package
  defines.  Typical: 0–3 per package.  Total cache size grows
  proportionally; query cost stays linear in row count.
- Hot-path scoring unchanged.

## UI examples

```
Sidebar:
  alex
  alex:exe:alex                 (exe-tag style)
  happy
  happy-lib
  happy-lib:backend-glr         (sublib-tag style)
  happy-lib:backend-lalr        (sublib-tag style)
  happy-lib:frontend            (sublib-tag style)
  happy-lib:grammar             (sublib-tag style)
  happy-lib:tabular             (sublib-tag style)
  happy:exe:happy               (exe-tag style)
  hsc2hs
  hsc2hs:exe:hsc2hs             (exe-tag style)
  nike
  nike:exe:nike-cli             (exe-tag style)
  ...
```

URL examples:

- `/pkg/happy%3Aexe%3Ahappy`            — exe overview
- `/pkg/happy%3Aexe%3Ahappy/Main`       — exe module
- `/pkg/happy%3Aexe%3Ahappy/Main/main`  — exe symbol

## Testing

- **Property — `Hypha.Types.ComponentName`**: extend the existing
  property to include the `Exe` case.  Round-trip
  `parseComponentName . renderComponentName` for all three kinds.
- **Unit — `Hypha.Project.Components`**: extend the fixture cabal
  with an `executable nike-cli hs-source-dirs: app` stanza and a
  second `executable wrap hs-source-dirs: app/wrap` stanza.  Assert
  `parseComponents` returns five components (1 main + 2 sublibs + 2
  exes) with the expected source dirs.
- **Cache round-trip**: write rows under `"nike:exe:nike-cli"` /
  `"0.1"` and read them back; assert equality.
- **Golden — `Golden.Server`**: extend the home-page fixture to
  include an exe entry in the sidebar; verify the `href` is
  `%3Aexe%3A`-encoded and the rendered text shows `:exe:` in an
  `<span class="exe-tag">`.
- **Disambiguation**: unit test asserting `parseComponentName` and
  `componentKey` produce distinct keys for a fictional package with
  both a sublib `foo` and an exe `foo`.
- **Manual smoke**: `hypha server` against the local project;
  confirm `happy:exe:happy`, `hsc2hs:exe:hsc2hs`, `alex:exe:alex`
  sidebar entries appear; click one; module list shows `Main` plus
  the exe's `other-modules`.

## Out of scope (future)

- Test-suite (`condTestSuites`) and benchmark (`condBenchmarks`)
  components.  Same plumbing would extend cleanly when justified.
- CLI / MCP support for the composite identifier syntax — still
  shared with the sublib backlog.
- Cross-component link resolution in source view.
