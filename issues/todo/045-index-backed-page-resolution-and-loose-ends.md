# Reuse the index for same-component pages, and close the loose ends

**Status:** todo
**Type:** refactor
**Found by:** code review of `adinapoli/more-server-improvements`

A collection of findings from the same review, each small on its own.

## 1. Every symbol card re-parses the whole component

`scSymbolLookup` and `scModuleDoc` call `componentSourcesFor` (reads every
module of the component) and then parse all of them, per request, with no
memoisation — ~250 `ghc-lib-parser` parses to render one `base` card.

The index short-circuit only fires for *cross*-component definitions,
because `importedSourcesFor` filters to those
(`drComponent … /= pkgT`), so the common intra-component case always takes
the slow path. The 07-28 design said the page should reuse the index
"instead of re-deriving it"; only the cross-package half does.

**Fix:** consult the index for same-component definitions too (rows already
carry `rowDefinition`), and/or try the asked-for module first, fanning out
only when it does not declare the name. Consider caching parsed interfaces
per `(component, version)` alongside the index.

`hypha source` inherits the same cost: it loads and parses an entire
package to locate one symbol.

## 2. Search cost is linear in the matching row set per keystroke

`Collapse.collapseRows` builds `Map.fromListWith (<>)` over every ranked
row before producing the first group, so the caller's `take 50` cannot
exploit `sortOn`'s laziness. At 63 146 rows a single-letter query maps most
of the index on every keystroke. Inherent to computing `srAlternates`
correctly, so this is a measurement to keep an eye on rather than an
obvious defect — recorded so it is a known quantity.

## 3. `Reexport` rebuilds a map per interface

`expandedExportNames ifaces` is not shared across the comprehension in
`resolveComponent`, so `byName` is rebuilt once per interface — |modules|
constructions of |modules| entries. `candidates` also re-walks
`miImports`/`miExports` per name per round. Thread the map; make
`expandedExportNamesIn :: Map ModulePath ModuleInterface -> …` the
primitive.

## 4. An intra-package chain that ends outside the package loses its row

`viable`'s `resolvedInside` accepts only `DefinedHere` / `DefinedIn`, and
`resolveComponent` reaches its fixpoint before any cross-package
information exists. So for `A → B → (dependency)` inside one component:
`B` is not viable for `A`, `A` falls to `outsideFor` and names sibling
`B`, and `lookupExport` misses because `B` belongs to the component being
indexed. `A` gets no row and is reported unresolved naming a sibling.

Rare in the current corpus (all 7217 unresolved reports trace to
unparseable modules), but pin it with a fixture either way, and fix by
passing the `ExportEnv` into `resolveComponent` or by running the outside
resolution as a second fixpoint round.

## 5. Fields that are computed and never read

- `Reexport.resAmbiguity` — its haddock says the rejected candidates are
  "kept so the choice is testable and *reportable*"; nothing reports them,
  so the recorded "ambiguous resolutions: 0" only ever counted the
  cross-package kind (`ciAmbiguous`). Either add
  `ciResolvedAmongst` and a fifth `reportComponentIndex` clause, or delete
  `Ambiguity` and return a bare `DefinitionSite`.
- `ParseError.peUnknownExtensions` — unreachable now that the supported-name
  list and the resolver read the same tables. `peDiagnostics` is populated
  and never read; fold it into what `parseErrorMessage` renders so a page
  can say "the pragma block did not read" as the likely cause.
- `Locate.Provenance.GuessedBySweep` — never constructed. `Ui.Doc` renders
  a "best guess" warning nobody can trigger, and `Unit/Server.hs` builds
  the value by hand. Either wire the cabal-less sweep through
  `LocatedDefinition` so the affordance can fire, or delete the arm (and
  with it, the one-constructor `Provenance`).

## 6. `Cache.fromStored` reports one anomaly of two

A row with both an unrecognised visibility *and* an empty `def_pkg`
reports only the first; the missing component is silently defaulted to the
row's own package. Return `[Text]` and concatenate.

## 7. Windows path assumptions

`Locate.rankBySharedSuffix` splits on a literal `'/'` and strips `".hs"` by
hand; `Indexer.hsToModule` replaces `/` with `.`. `System.FilePath` is
already imported — use `splitDirectories` / `takeBaseName`. Issue #10 was
a POSIX-path assumption that broke every Windows build.

## 8. `hypha doctor` should own the whole-index invariant

The deleted `scripts/index-audit.sh` checked five row shapes, four of which
are now unit-tested. The fifth — "rows attributed to a component that has
no rows of its own" — is a cross-row invariant no unit test can make. It
is the `IndexHealth` surface the 07-27 design specified and that was never
implemented; put it there rather than in a shell script that shells out to
`sqlite3`.
