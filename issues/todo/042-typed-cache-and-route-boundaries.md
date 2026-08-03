# Type the cache keys and the URL builders

**Status:** todo
**Type:** refactor
**Found by:** code review of `adinapoli/more-server-improvements`

The index rows became domain types on that branch; their keys and their
URLs did not. Three related seams, each one where a `Text` stands in for a
type we already have.

## 1. `writeIndex` can wipe one component and insert another

`Hypha.Search.Cache.writeIndex` takes the component as a `Text` parameter
*and* reads it from `rowComponent` of every row:

```haskell
writeIndex c pkg ver rows = do
  executeNamed ... "DELETE FROM pkg_index WHERE pkg = :p AND version = :v" ...
  let expanded = [ (unComponentKey (rowComponent r), ver, ...) | r <- rows ]
```

The DELETE keys off the parameter, the INSERT off the rows. They agree only
because every caller passes the key it built the rows with. Nothing
enforces it, and a mismatched pair silently deletes one component's rows
and inserts another's inside one transaction.

**Fix:** `writeIndex :: IndexCache -> Version -> [IndexRow] -> IO ()`,
deriving the component from the rows (grouping if mixed input must be
accepted), so the two cannot disagree.

## 2. The whole cache API is `Text` outside, typed inside

`writeIndex` / `readIndex` / `haveIndex` / `lookupRowsByName` /
`lookupRowsInModule` / `readFingerprint` / `writeFingerprint` all take
`Text -> Text` and return `[IndexRow]`, so every call site does
`unComponentKey` / `unVersion` — the call-site stringification `CLAUDE.md`
names explicitly. Take `ComponentKey` and `Version` and unwrap once,
inside the module that owns the SQL.

## 3. One route builder, and escape what goes into it

Five hand-rolled `/pkg/…` and `/source/…` builders (`Search.Collapse`
×2, `Ui.ModuleDoc` ×3, `Ui.Doc`, `Ui.Tree`) and two spellings of the
`component:module` label (`Ui.Doc.definedIn`, `Ui.ModuleDoc.originLabel`).
The bug this branch fixed — a link pointing at a module the package does
not have — *is* a route-construction bug, so leaving five places to build
routes invites a sixth to drift.

Worse, only `Ui.Tree` percent-encodes: names come from `occNameString`, so
an operator arrives bare and `(/)` yields
`/pkg/base/GHC.Real//` — an extra path segment — while `(%)` yields an
invalid escape.

**Fix:** one module (`Hypha.Types.Route`, below both the search and the UI
layers) exposing `pkgPath` / `modPath` / `symPath` / `sourcePath` over
`ComponentKey` / `ModulePath` / `SymbolName`, plus `qualifiedLabel`, with
percent-encoding applied there. Add an operator-name case to the tests.

**Done.** `Hypha.Types.Route` exists and owns every `/pkg/…` and
`/source/…` path; `hrefFrom` percent-encodes each segment through
`encodePathSegments`. `Unit.Route` covers `#`, `/`, `?` and a `/` inside a
segment. What remains of this section is the label duplication:
`Ui.Doc.definedIn` and `Ui.ModuleDoc.originLabel` still spell
`component:module` separately, and `Collapse.presentationLabel` is a
third.

## 4. While in the area: `SymbolCardData`'s three `Text` fields

`scdModule`, `scdComponent` and `scdRequested` are `Text`, unwrapped at
the producer, which forces `Ui.Doc` into
`scdRequested card == scdModule card && defPkg == pkg` — comparisons
nothing stops from being written with swapped operands. Carry
`ModulePath` / `ComponentKey` and unwrap in the Lucid call. Same for
`App.scHumanSearch`'s scope parameter and `Fuzzy.scopeRows`, which take a
`Maybe Text` that is really a `ComponentKey` — and where
`Fuzzy.entityComponent` currently flattens `PackageName` and
`ComponentKey` into `Text` so they can be compared, which happens to work
only because `componentKeyOf p MainLib` renders as the bare package name.
