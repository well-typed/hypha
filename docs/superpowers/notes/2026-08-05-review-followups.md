# Issue drafts from the #20 re-review (2026-08-05)

Two criticals were fixed in the commit that carries this file. Everything
below is what the same review turned up and we chose not to act on today.
Each block is ready to paste into `glab issue create`.

Note on the review itself: the first pass ran with a sandbox that denied
reads under `/home/alfredo`, so every probe of `~/.cabal` returned ENOENT
and the reviewer concluded the store did not exist. Re-run with the sandbox
off, the substance held — no store entry has a `src` directory (0 of 1871,
in both store roots) — but the *reason* had been half-guessed. Measure
before believing a review's causal claim.

---

## 1. Reuse the tarballs cabal has already downloaded, instead of fetching

**Labels:** enhancement, performance

`~/.cabal/packages/hackage.haskell.org` holds 883 entries on a developer
machine: cabal downloads a tarball for every plan dependency it builds.
`resolveSrcLocal` ignores them, so hypha's own source cache was built on
the assumption that those tarballs are not there. They are.

Extracting one is a local operation — no network, no Hackage round trip.
Adding `locateRepoTarball` + extract to the speculative probe would make
every ordinary Hackage dependency resolvable offline, and would shrink the
owner-fetch path introduced for #20 to the boot libraries alone
(`ghc-internal`, `ghc-prim`, `rts` have no tarball there, since they ship
with GHC).

**Acceptance criteria**

- With an empty hypha source cache and a populated `~/.cabal/packages`, a
  cross-package chain into a Hackage dependency resolves with zero HTTP
  requests.
- The extraction is charged to the probe budget in the same way a local
  directory hit is, so a wide ring cannot turn into hundreds of extractions.
- A boot library still goes through the owner-fetch path.

**Notes:** raised independently by a colleague. Deliberately out of scope
for the #20 fix, which needed ownership decided *before* any materialising
at all.

---

## 2. `search: exhausted` is claimed for searches that were blocked

**Labels:** bug

`searchOutcome` (`src/Hypha/Error.hs`) maps every non-bound failure to
`"exhausted"` and never looks at the gaps. So a chain that stopped because
a dependency was unreadable, or because a module would not parse, reports
the same verdict as one that looked everywhere and established the symbol
is absent. `test/Golden/golden/source-facade-no-plan.compact.json` pins the
contradiction: `"search": "exhausted"` beside
`"unreadable_dependencies": "the build plan has no unit for 'reexport' …"`.

This is the same defect the typed failure was introduced to remove — "we
stopped looking" must not read as "it is not there".

**Fix sketch:** make the verdict gap-aware (a third value, `blocked` or
`incomplete`, when the failure is a non-bound case and the gap list is
non-empty). `searchOutcome` is already called from a branch that holds the
gaps.

**Acceptance criteria**

- A query blocked by an unreadable dependency reports a verdict distinct
  from `exhausted`.
- The `source-facade-no-plan` golden is regenerated and no longer pairs
  `exhausted` with a non-empty gap list.

---

## 3. An unparseable module in the chain reports as absence

**Labels:** bug

`Hypha.Source.Locate` returns `ProbeUnreadable` both when nothing within
reach declares the name and when the module will not parse. Neither reaches
the typed failure: the frontier drains and the caller gets
`SearchNoSupplier`, i.e. `search: exhausted`. The parse error does go to
stderr, so it is not silently swallowed, but the structured answer an agent
branches on says the opposite of the truth.

This is the dominant real-world case in this codebase: unparseable CPP
modules are what most unresolved-export reports cascade from. A search
blocked by one has established nothing.

**Fix sketch:** give `Probe` a distinct `ProbeUnparsed !ModulePath` arm and
carry those modules into the failure (`SearchNoSupplier` gaining an
unparsed-modules field), classified with issue 2's new verdict rather than
`exhausted`.

**Acceptance criteria**

- A fixture whose chain passes through a module that will not parse reports
  a verdict distinct from `exhausted` and names the module.

---

## 4. `stopped_at_bound` reaches the envelope untested

**Labels:** test

`SearchHopLimit` / `SearchParseBudget` are asserted only as locator return
values (`test/Unit/SourceLocate.hs`). Nothing asserts they reach the
envelope. Mutation that should fail and does not: make `searchOutcome`
return `"exhausted"` unconditionally and delete `searchDetail`'s
`hop_limit` / `stopped_at` / `parse_budget` rows — the suite stays green.
`SearchNotExported`, `SearchNotDeclared` and `SearchSweptPackage` have no
envelope coverage in any form either.

**Acceptance criteria**

- A test drives `errorActions` over a `SearchHopLimit` failure and asserts
  `search: stopped_at_bound` plus the bound.

---

## 5. `SearchModuleUnparsed` conflates two facts and renders one of them wrong

**Labels:** bug

The constructor is produced both for the *asking* module failing to parse
and for a resolved local target missing from the parses, but
`renderSymbolSearchFailure` always prefixes with "re-exports", so the
message claims to know a module re-exports a symbol while saying we could
not read that module. `searchDetail` also reports `resolved_to` for a module
nothing resolved to.

**Fix sketch:** split into `SearchAskingUnparsed` (no argument — the caller
has `asking`) and `SearchTargetUnparsed !ModulePath`, each with its own
sentence.

---

## 6. The homonym fixture passes with the fix reverted

**Labels:** test

`test/Unit/SourceLocate.hs`'s private-homonym case targets the
`| exports iface` guard in `Locate.verdict`. Deleting that guard leaves the
suite green: `Dep.WideFacade` imports the declaring module *explicitly*, so
it lands in the `explicit` group and is probed before `Dep.Homonym` is ever
reached. The same fixture cannot detect a flattened explicit/open split
either — one parent in the frontier makes both orderings identical.

**Fix sketch:** add a facade that imports both modules openly
(`Dep.Homonym` sorts before `Dep.Internal`, so `rankAround` probes the
homonym first and the guard becomes load-bearing); for the split, the
frontier needs two parents that supply candidates by different kinds.

---

## 7. The gap list drains the whole closure into one message

**Labels:** bug, ux

`lookupModule` walks one dependency per miss, so an unresolvable module
name records a `GapNoLocalSource` for every dependency without unpacked
source, and `renderGaps` concatenates all of them into the error message.
Measured before the #20 fix: `hypha source base/Data.List/sortOn` on a cold
cache listed `ghc-internal`, `ghc-prim`, `ghc-bignum` and `rts` for a chain
that needed only the first. `puDeps` also unions the lib deps of every
component including test-suites, so a local package's closure pulls in
`tasty`, `hspec` and friends.

The owner-fetch path added for #20 reduces how often this is reached, but
does not bound the list.

**Fix sketch:** only report gaps for dependencies a candidate module name
actually sent us to, and cap or summarise the rendered list.

---

## 8. Own-component modules are re-parsed and charged the parse budget

**Labels:** performance

`localSources` (`Hypha.Source.Locate`) discards the `ModuleInterface` it
already holds, so `probe` pays a `spend` and a fresh parse for a module
`byResolution` parsed moments earlier. `supplierCandidates` does not filter
by component membership, so `base:Prelude`'s ring includes base's own
`Data.List`, `Data.Maybe`, … Each costs one parse of bytes we hold and one
unit of a 64-parse budget, which can turn a reachable answer into a
spurious `SearchParseBudget`.

**Fix sketch:** keep the interface in `localSources` and skip
`spend`/`parsedOf` when it is already in hand.

---

## 9. The swept path never reports `defined_in`

**Labels:** bug

`definedElsewhere` returns `Nothing` for `SweptSite` unconditionally, and
`cardFor` does the same — but the scan is precisely the path that lands in
another module. `hypha source containers/Data.Map.Strict/lookup` reports
`module: Data.Map.Strict` beside a path in `Data/Map/Internal.hs` with no
annotation, which is the case `defined_in` was added for.

**Fix sketch:** `SweptSite` needs to carry the module it landed in (or a
`resolution: swept` marker), which means `locateSymbolDefinitionInDir`
returning more than a `SourceLocation`.

---

## 10. `DefinedIn` and its encoder are declared twice

**Labels:** refactor

Identical type, fields and JSON encoder in `Hypha.Command.Source` and
`Hypha.Command.Symbol`, which already imports the former. The duplicated
`diModule d /= modPath` filter and the four-line `case located of` block go
with it. Two declarations of one wire shape, free to disagree between two
commands an agent reads interchangeably.

---

## 11. Smaller items

- `unreadable_dependencies` is the wrong wire key for `GapUnitNotInPlan`,
  which is a missing plan entry, not an unreadable dependency.
- `candidates_considered` carries only the first ring even after a
  three-level descent; rename or accumulate.
- A `stopped_at_bound` answer offers no remedy: the bounds are injectable
  in code but there is no `--hop-limit` / `--parse-budget` flag and no
  `retry_deeper` action, so the agent the type doc says can retry has
  nothing to retry with. `sbHopLimit = 3` is tight for parts of `base`.
- `Hypha.Source.Dependencies` imports `Hypha.Search.Indexer` for three
  cabal/filesystem helpers (`chooseSourceRoots`, `enumModuleFilesIn`,
  `stanzaModules`), so answering a source query pulls in the whole
  search-index subsystem. They belong in `Hypha.Project.Components`.
- Sibling sub-libraries of the asking package are unreachable:
  `dependencyClosure` seeds the visited set with the package's own name and
  the plan holds one unit per package name, so `pkg:internal-lib` cannot be
  followed and the failure is reported as plain `SearchNoSupplier`.
- `Hypha.Source.Extract.resolveModuleEntries` still probes a single ring,
  so `hypha module base/Data.List` emits `EntryUnplaced` placeholders for
  two-hop symbols — the third consumer of the same question and now the
  only one that stops at the first ring.
- Reading a dependency's module uses a bare `TIO.readFile`, so invalid
  UTF-8 in a third-party source surfaces as `INTERNAL_ERROR` rather than a
  `HyphaError`.
- The new e2e cases in `test/Golden/Cli.hs` do not assert empty stderr and
  omit `--cache-dir`, so they write the developer's XDG cache root.
- `test/fixtures/reexport/src/Fixture/ViaWide.hs` is not listed in
  `reexport.cabal`, so no cabal-driven query reaches the wide-ring shape.
- New wire fields (`defined_in`, `search`, `hop_limit`, `parse_budget`,
  `candidates_considered`, `unreadable_dependencies`, `module_absent`) are
  undocumented in `docs/` and `README.md`.
