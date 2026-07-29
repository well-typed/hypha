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
