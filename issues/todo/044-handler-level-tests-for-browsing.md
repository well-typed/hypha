# Test the browsing paths through the real handlers

**Status:** todo
**Type:** test
**Found by:** code review of `adinapoli/more-server-improvements`

## Problem

Two browsing releases shipped green and broken, because every test of the
fix handed the consumer a hand-written definition-site map:

```haskell
-- test/Unit/SourceLocate.hs and test/Unit/SourceExtract.hs
idSites = Map.fromList
  [ (SymbolName "depThing", DefinitionRef (ComponentKey "reexport-dep") ...) ]
```

So the suite proves "locate works *given the right map*" and never that the
server produces the right map. The producer —
`PackageCache.lookupInModule` plus `Command.Server.importedSourcesFor` —
has no test at all, and both consumer suites would pass with
`importedSourcesFor` returning `mempty`, which is the shape of both broken
releases.

There is also no handler-level test anywhere: `grep -r "appWith\|
buildServerConfig" test/` finds nothing, and the test stanza has no
`wai-extra` / `hspec-wai`.

Recorded in the 07-28 design spec as the process failure: pure tests cannot
verify a path whose IO they replace.

## Fix

1. A producer test: write rows into a temp `PackageCache`, call
   `lookupInModule` and `importedSourcesFor` with a stub `PackageResolver`
   pointing at `test/fixtures/reexport-dep`, and assert the resulting
   `ImportedDefinitions`. Then feed *that* — not a literal — into
   `locateDefinitionInComponent`.
2. One `Network.Wai.Test` request through `App.appWith` for
   `/pkg/<c>/<m>/<s>` and one for `/pkg/<c>/<m>`, asserting the card and
   the entry list are not empty. This needs `wai-extra` in the test stanza
   (update the plan/issue per `CLAUDE.md` before adding it).
3. A golden case for a module page whose entry is an `EntryReexport` from
   a *different* component: `Golden/Server.hs` currently only pins
   `EntryLocal`.

## Acceptance criteria

- Reverting either half of the browsing fix (the index lookup, or the
  cross-component source load) turns a test red.

## Progress

**Item 1 done.** `test/Unit/ImportedSources.hs` drives the producer end to
end: rows into a real `PackageCache`, out through the real
`lookupInModule` + `importedSourcesFor` with a stub resolver on the
`reexport-dep` fixture, and the result fed to the real
`locateDefinitionInComponent`.  Verified by mutation — stubbing the row
lookup to `[]` fails it.

Items 2 and 3 remain: a `Network.Wai.Test` request through `App.appWith`
(needs `wai-extra`), and goldens for the `EntryReexport` /
`EntryUnplaced` page shapes.

Separately, the browsing paths were driven manually against this project
in a `cabal repl lib:hypha` session over `buildServerConfig` — 252
packages indexed, operator hrefs escaped, `+N` disclosures rendered,
`base/Prelude` free of bogus `#v:` anchors, cross-package cards resolving,
and a second start rebuilding only the package whose source had changed.
That is a check someone has to remember to run; items 2 and 3 are how it
stops being one.
