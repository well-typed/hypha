# Make CLI `source` (and `symbol`) follow cross-package re-exports using the build plan

**Status:** todo
**Type:** enhancement
**Found by:** dogfooding the announcing-hypha blog post tour (`hypha source base/Data.List/sortOn`)

## Problem

On a GHC 9.10.3 plan (base-4.20.2.0, ghc-internal-9.1003.0):

```
$ hypha source base/Data.List/sortOn
hypha: Data.List re-exports sortOn, and none of GHC.Internal.Data.List
both supplied a source and declared it
error:
  code: NOT_FOUND
  exit_code: 3
  message: "symbol 'sortOn' not found in base-4.20.2.0/Data.List"
```

`Data.List` re-exports `sortOn` from `GHC.Internal.Data.List` (which in
turn gets it from `GHC.Internal.Data.OldList`), both hops crossing the
`base` -> `ghc-internal` package boundary. The CLI gives up at the
boundary; `hypha server`, facing the same query, resolves it, because it
has the plan.

Since GHC 9.10 turned `base` into a facade over `ghc-internal`, this is
not an edge case: it is the common case for every `base` symbol a user
asks about. `hypha source base/<Module>/<sym>` failing on `base` is a
bad look for a tool whose pitch is "lands on the real definition".

## Root cause

`Hypha.Command.Source.locateSymbolLoc` calls
`locateDefinitionInComponent` with `noImportedDefinitions`. The comment
above it justifies this with "a bare package directory comes with no
build plan" — but that is only true of the resolver fallback path. In
the normal case the CLI has already located and decoded `plan.json`
(`runSymbol` takes a `BuildPlan`; `Hypha.Project.Plan.loadBuildPlan`
exists precisely for this), and `Hypha.Hackage.Source` already
materialises boot-library sources into the source cache for other
commands.

The resolution machinery itself does not need to change:
`Hypha.Source.Locate.locateDefinitionInComponent` already has the
`DefinedOutside` path that looks the candidate modules up in
`idSources imported` and scans them in rank order. We simply never hand
it any imported sources from the CLI.

## Fix sketch

- When a `BuildPlan` is available (the common case), build the
  `ImportedDefinitions` for `locateDefinitionInComponent` from the
  plan: for each dependency unit of the package being queried, locate
  its source dir (cabal store, or the `source/<pkg>-<ver>/` cache,
  fetching the tarball if needed) and enumerate its component sources,
  the same way `Indexer.packageSources` does for the target package.
- Keep the current report-rather-than-guess behaviour only for the
  genuinely plan-less path (bare directory with no cabal/project
  context), which is what the existing comment describes.
- `hypha symbol` on a facade module (see the bare-card behaviour for
  `hypha symbol base/Data.List/sortOn`) could use the same imported
  environment to annotate the card, e.g. `reexported_from:
  ghc-internal/GHC.Internal.Data.OldList`, instead of silently omitting
  `signature`/`haddock_raw`. Worth doing in the same pass; split into a
  follow-up issue if it grows.

Note the multi-hop case: `Data.List` -> `GHC.Internal.Data.List` ->
`GHC.Internal.Data.OldList`. The first hop lands in another facade
module, so the imported environment must be built per-package, not just
for one hop, or the fix stops one facade early.

## Acceptance criteria

- `hypha source base/Data.List/sortOn` on a GHC 9.10.x plan lands in
  `GHC/Internal/Data/OldList.hs` at the `sortOn` declaration.
- `hypha source base/Data.Traversable/mapAccumL` (the other facade case
  from the search work) lands at its `ghc-internal` declaration.
- A plan-less invocation (store path without project context) keeps the
  current honest `NOT_FOUND` report.
- Golden test covering the `base` facade case; the `DefinedOutside`
  rank-order comment mentions `Control.Concurrent` /
  `isCurrentThreadBound`, which is a good second fixture.
