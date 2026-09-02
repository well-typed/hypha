# Changelog

All notable changes to `hypha` are documented in this file. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/); the project
loosely follows [Semantic Versioning](https://semver.org/).

## [Unreleased]

### Changed

- **Default output format is now YAML** instead of JSON. YAML is
  token-leaner for LLM consumption (no brace/quote overhead), has a
  perfect isomorphism with JSON, and renders multi-line strings
  (Haddock, source snippets) as literal block scalars instead of
  `\n`-escaped single lines. Use `--json` to opt into the previous
  JSON envelope format for machine pipelines.
- **Removed `schema`, `command`, and `ok` from the output envelope.**
  The consumer always knows what they invoked, and the presence of
  `result` vs `error` is the success/failure signal — the boolean was
  redundant. The envelope is now purely structural.
- **Renamed `--human` to `--json`** (inverted semantics). The old
  `--human` flag was a misleading name for what was actually plain
  text output; the new default is YAML via `aeson-yaml`, and `--json`
  opts into the compact JSON envelope.
- **Removed `in_plan` from `package` command output.** The field was
  hardcoded to `true` and redundant with the envelope-level
  `outside_plan` flag.
- Added `aeson-yaml` dependency (BSD3, pure Haskell, no C FFI).

### Added

- **`hypha --version` / `-V`.** There was no way to ask a hypha binary what
  it was; `hypha --version` exited `1` with a usage message. The version
  comes from cabal's `CURRENT_PACKAGE_VERSION`, so it cannot drift from
  the build.
- **`--hoogle-timeout SECONDS`**, and hypha no longer advertises
  environment variables it does not read. `HYPHA_OFFLINE` and
  `HYPHA_HOOGLE_TIMEOUT` both appeared in hypha's *own* error output — the
  first in the message for a suppressed remote tier, the second as the
  suggested retry after a remote failure — and neither was ever looked up,
  so a user who followed hypha's advice still hit the network and the
  recommended retry failed identically. `HYPHA_OFFLINE` is now simply gone
  from the error text and the docs: `--offline` already exists, and
  hypha's own MCP server passes it explicitly, so the variable only
  shadowed a flag. `HYPHA_HOOGLE_TIMEOUT` had no flag to shadow, so it
  became one — it appears in `--help`, which no environment variable does,
  and a value that is not a positive whole number of seconds is rejected
  by the parser rather than silently ignored. The remote-failure action
  now suggests `hypha lookup <query> --hoogle-timeout 30`.
- **The compiler answers where a re-export comes from.** An export list
  says *which* names a module exports and never *whence*, so a syntactic
  pass has to guess between the imports that could plausibly supply one —
  and `base`'s `Control.Concurrent` lists `Prelude` first, which is how
  `isCurrentThreadBound` came to be "re-exported, origin unresolved" while
  Hackage documents it fine. The index build now asks GHC, which already
  ran the renamer and wrote one fully-qualified origin per export into the
  `.hi` file: an export syntax could not place is repaired by reading
  `ghc --show-iface` for the module that exports it, and by trying every
  ranked candidate import rather than committing to the first.

  This needs the compiler the plan was solved with — `ghc-<version>` or a
  bare `ghc` reporting that version, alongside its `ghc-pkg`; interface
  files are patch-exact and a mismatched compiler reads nothing at all.
  When there is no such compiler, or a cabal store cannot be listed, the
  reason is reported **once** and re-exports are resolved from source
  alone, as before. The project's own packages are read from the build
  tree the plan names, so a local façade is repaired like any dependency.
- **Cross-package re-exports are indexed.** A façade module now
  contributes rows for what it re-exports from a dependency, resolved
  transitively through that dependency's own already-resolved rows.
  `base` went from 308 index rows to 1450; `Data.Traversable`,
  `Control.Monad`, `Data.Foldable`, `Data.List`, `Data.Maybe` and
  `Prelude` had contributed none.
- **Class methods, data constructors and record fields are indexed.** The
  parser now emits them as declarations in their own right
  (`DkClassMethod` / `DkConstructor` / `DkRecordField`, parented on the
  enclosing class or type), and the resolver expands `T(..)` wildcard
  exports to them. A constructor carries the declaration text as its
  signature rather than an empty one, and a field is anchored on its own
  `field :: Type` entry, so `getSum`, `appEndo` and `runReaderT` are
  searchable. The index had no row for a class method —
  `foldMap`, `traverse`, `fmap`, `Just`, `mempty` all came up empty in
  the server's search — and `hypha source`/`hypha symbol` could not
  resolve a method (issue 12). A method now resolves to the module that
  declares it with the signature GHC attaches to it, and `lookup`'s
  cache tier surfaces the canonical package instead of only fringe
  packages that happened to declare a top-level function of the same
  name.
- **Search results collapse to one hit per definition**, with the most
  public presentation winning. A `+N` disclosure names every package and
  module folded in, each a link, with the defining one tagged; scoping to
  a package applies before the fold, so a definition two packages present
  still appears under either. Package and module names are results in
  their own right, ranked above the symbols beneath them.
- **Module pages list re-exported entries** with real signatures and
  Haddock, tagged with the defining module — and the defining package
  when it differs. Source links follow the definition across the package
  boundary.
- **Language extensions are read, not guessed.** A module is parsed under
  its own `{-# LANGUAGE #-}` pragmas plus its cabal stanza's
  `default-extensions` / `default-language`, resolved through GHC's own
  flag table, instead of a hand-written whitelist that failed on the
  first construct nobody had thought to add (`type role`, `MagicHash`).
- Boot libraries shipped with GHC now get a "view on Hackage" link; they
  are published there at the version the plan pins.

- **`hypha server` module pages now show real documentation** instead
  of a bare export list, resolved through a typed priority chain:
  prebuilt Haddock (hypha cache → local dist-dir → cabal store) is
  embedded and restyled inside the hypha shell; otherwise the docs are
  **rendered on the fly from source** (module header prose, per-decl
  signatures, haddock comments, kind badges, a sticky "On this page"
  rail, haddock-compatible `v:`/`t:` anchors) with zero extra disk;
  the export-only fallback states the reason for the degradation.
- `Hypha.Source.Parser` classifies declarations (`DeclKind`) and now
  covers `data`/`newtype`/`class`/`type`/`type family`/`pattern`/
  `foreign` decls — they were previously invisible to the search
  index and symbol pages.
- Server shell overhaul: two-state theme toggle (light/dark),
  filterable Project/Dependencies sidebar groups with active-entry
  highlight, home stat cards, symbol card copy-signature button
  and kind badge, source-view header with a docs backlink.
- `/haddock/:pkgver/*` serves non-HTML assets (CSS/JS/fonts/images)
  with correct MIME types and rejects path traversal, so raw Haddock
  pages finally render styled.
- Fuzzy search can be restricted per-package by pressing `Tab`.

### Changed

- `hypha-mcp`'s `hypha.exec` tool description spells out the argv-array
  shape with examples and lists the available subcommands, so smaller
  / less obedient MCP clients (GLM, Llama, …) don't invent calling
  conventions or non-existent subcommands like `hypha search`.
- SKILL.md gains a "Commands that do NOT exist" subsection and an
  explicit calling-convention block for `hypha.exec`.
- README sandbox troubleshooting now mentions `/etc/ssl/certs` (TLS to
  the remote Hoogle) and includes a generic `sbox` recipe for non
  Claude-Code harnesses.

### Fixed

- **Two projects no longer evict each other's index rows.** The cache held
  one row set per `(component, version)` and treated the per-component
  fingerprint as a validity check, so a project whose plan resolved a
  shared package differently rebuilt it — and the rebuild deleted the
  other project's rows. Switching between a GHC 9.2.8 project and a GHC
  9.10.3 one re-indexed the 47 `(package, version)` pairs their plans
  share, every time, in both directions. Rows are now keyed on cabal's
  unit-id as well, so both configurations coexist and each project sees
  only its own. Keying on the compiler alone would not have been enough:
  the synthesised `cabal_macros.h` defines `MIN_VERSION_<dep>` for every
  dependency, and 18 modules of `attoparsec` alone choose their imports
  from those gates, so the same version resolved against `text-1.2.5` and
  against `text-2.1` is genuinely two row sets. The unit-id is cabal's own
  hash of the compiler, the resolved dependency unit-ids and the flags,
  read straight from `plan.json` — which keeps it free on the tier-1
  lookup path, where the cheap plan reader is what makes `hypha lookup`
  cost 90ms rather than 505ms. Configurations are pruned to the three most
  recently indexed per `(component, version)`; a pruned one costs a
  re-index, never a wrong answer. `--package-override PKG=VER` names a
  version no plan resolved, so those reads match on the version instead.
  The index format generation goes to `6`: the primary key itself moved,
  which SQLite cannot alter in place, so the tables are dropped and
  rebuilt once.
- **The cheap plan reader picks the same unit as the full one.** For a
  local package with an executable, `loadPlanVersions` pinned
  `mylib-0.1.0-inplace-myexe` while the indexer wrote under
  `mylib-0.1.0-inplace`, so every local package would have looked stale to
  the tier it warms. Both readers now share the ordering that decides
  which unit represents a package, and a test pins them to agreement on
  the unit-id as well as the version.
- **`--offline` in `hypha lookup` now serves remote-tier answers that are
  already cached.** The remote Hoogle tier's `kv` cache was only consulted
  *after* the `--offline` check, so an offline run refused queries whose
  answer was sitting on disk. `--offline` now means what every document
  says — "do not use the network": a previously cached answer is still
  served (tagged `remote-hoogle`), and `HOOGLE_OFFLINE` henceforth means
  "offline *and* nothing cached". A corrupt cached blob is reported as
  `HOOGLE_REMOTE_ERROR` rather than misdiagnosed as offline. (#39)
- **The preprocessor gets GHC's own headers, and reads only the branches
  this platform builds.** `MachDeps.h` and `ghcplatform.h` ship with the
  compiler rather than with the packages that `#include` them, and cabal
  puts the compiler's include directory on every CPP invocation; hypha did
  not, so every module asking for `WORD_SIZE_IN_BITS` or
  `<arch>_HOST_ARCH` walked into its `#error` arm — 21 of
  `base-4.16.4.0`'s modules, 20 of `ghc-internal-9.1003.0`'s, including
  the ones a `Data.List` descent passes through. The directory is now
  located from the *plan's* compiler (`ghc --print-libdir`, then a probe
  for `MachDeps.h`, which covers both the pre-9.6 and the 9.6-and-later
  layouts). Separately, a package's `os()` and `arch()` stanzas are now
  resolved for the platform the plan was solved for instead of having
  every branch unioned: `base`'s Windows-only modules are on disk in the
  sdist and cannot preprocess off Windows, so reading them reported four
  parse failures per descent for modules this platform never builds.
  Conditions the plan does not settle — `flag()`, `impl()` — are still
  unioned, since guessing a flag assignment would silently index a module
  list the package was never built with. Measured: `hypha source
  base/Data.List/sortOn` on a GHC 9.2.8 project went from ten
  `could not be parsed` lines to two, and parse failures across this
  repo's 252-unit plan went from 46 modules to 25.
- **A `#error` in a module now reports as a parse failure rather than
  escaping.** cpphs raises `#error` by calling `error` from pure code, and
  the handler meant to catch it wrapped a thunk — `Text.pack <$> runCpphs`
  is not forced inside the `try`, so the failure fired wherever the text
  was first demanded. `Hypha.Source.Parser` forces it inside the handler,
  which makes its pure entry points (`parseModuleWith` and friends) total
  as their documentation already claimed.
- **Type-signature queries reach the remote Hoogle tier again.** The
  query string was percent-encoded by a hand-rolled escaper that only
  knew about `&?#=` and the space, so `>`, `[` and `]` went out raw.
  http-client parses a URL before it opens a socket, so
  `hypha lookup 'Ord b => (a -> b) -> [a] -> [a]'` never left the
  machine: it failed as `HOOGLE_REMOTE_ERROR` /
  `InvalidUrlException … "Invalid URL"` whenever the cache and local
  tiers missed. The query string is now rendered by
  `Network.HTTP.Types.URI.renderQuery`.
- **`hypha lookup`'s suggested retry commands are shell-quoted.** The
  `retry_offline`, `raise_timeout`, `retry_online` and
  `retry_with_prefix` hints interpolated the raw query, so the
  suggestion printed for a type-signature query
  (`hypha lookup Ord b => (a -> b) -> [a] -> [a] --offline`) was not
  the command it looked like — pasted into a shell, `=>` truncates a
  file named `b`. The query is single-quoted when it needs it; the
  `query` field itself stays raw, since it is data rather than a
  command.
- **`hypha lookup`'s cache tier is pinned to the build plan.** The
  package cache is keyed on `(package, version)` and shared by every
  project on the host, and a write for a new version does not evict the
  old one — so "what is indexed here" is the machine's history, which is
  wider than "what this project builds against". Tier 1 answered from all
  of it: in a project pinning `base-compat-0.15.0`, `hypha lookup fmap`
  also returned a row from `base-compat-0.14.1` that some other project
  had indexed, labelled `tier: cache` as though it came from the plan.
  The same query on a colleague's machine gave a different answer.
  Reaching past the plan remains the remote Hoogle tier's job, and the
  tier label is now what tells you how far an answer reached. Outside a
  project there is no plan to pin to and the whole cache still answers; a
  plan that cannot be read is reported on stderr rather than silently
  treated as the same case.
- **A `lookup` provider carries the version it was indexed under.** The
  version is a property of the cache entry rather than of a row, so two
  rows for the same symbol, module and package differed in nothing the
  renderer could see and printed as an unexplained duplicate. Emitted on
  the cache tier only: a Hoogle hit carries a package name and no version.
- **CPP conditionals are evaluated against the plan's macros.** cabal
  generates a `cabal_macros.h` for every build — `__GLASGOW_HASKELL__`,
  and a `MIN_VERSION_<pkg>` per dependency — and passes it to every CPP
  invocation. hypha read the same sources without it, and in CPP an
  undefined macro is `0`, so a module written the way most of Hackage is
  written:

  ```haskell
  #if __GLASGOW_HASKELL__ >= 710
  modern :: Int -> Int
  #else
  ancient :: Int -> Int
  #endif
  ```

  was indexed as `ancient` — on a plan pinning **GHC 9.10.3**. Likewise
  `MIN_VERSION_base(4,18,0)` was false against **base 4.20.2.0**. The
  module parses, contributes rows, and nothing is reported, so unlike a
  skipped module there was no stderr line to notice. **2,684 modules
  across 559 packages** — 15.3% of a real source cache — carry such a
  gate.

  hypha now synthesises the header from the build plan, which already
  holds the compiler and a version per package, so the macros cannot
  drift from the answers they describe; it is written once per plan,
  content-addressed under `<cache>/cpp-macros/`. A package's
  `include-dirs` are passed as the `#include` search path too, so a
  module including its own package's header is no longer dropped
  outright.

  The index format generation goes to `5`: the per-component fingerprint
  cannot notice this, because the source did not change — the macros did
  — so nothing else would force the affected rows to be rebuilt.
- **A signature is a type again.** Signatures were sliced out of the
  declaration's source span, so any comment inside that span came with
  them and the newlines were collapsed on the way — `hypha symbol
  text/Data.Text/splitOn` answered `splitOn :: HasCallStack => Text -- ^
  String to split on. If this string is empty, an error -- will occur. ->
  Text -- ^ Input text. -> [Text]`, in which the second line of the first
  comment reads as part of the type. Measured on a real cache: **9,365
  rows, 5.27%, across 262 packages** including `base`, `Cabal`, `text` and
  `primitive`. A data constructor had the same problem from the other
  direction, carrying the `=` or `|` and the trailing `-- ^` of the line
  it shared, and a record field inherited the opening brace.

  Signatures now come from the parse tree, with GHC's `HsDocTy` nodes
  removed, the way `declDoc` already did. That is exact where a textual
  strip is not: `arrow :: (a --> b) -> Int` is an operator, and everything
  after its `--` is the rest of the type. The index format generation is
  bumped to `4`, because a stored signature cannot be repaired after the
  fact — telling a comment from an operator needs the parse tree the row
  no longer has — so without it every existing cache would keep serving
  the mangled text. Expect one background re-index.
- **User-facing docs corrected against measured behaviour.** Exit code `1`
  (argument-parser failures — unknown subcommand, unknown flag, missing
  argument) was undocumented despite covering the most common way to
  misuse the CLI, and `9` (`INTERNAL_ERROR`) was reachable but unlisted;
  both are now in the table, with the `1`-versus-`2` distinction spelled
  out. `lookup.md` claimed class methods and data constructors are not
  indexed — they are; the determinant is whether a module survives CPP.
  The Hoogle freshness stamp is `hoogle-stamp`, not `plan-hash`
  (`caching.md` used both names, in the same file). `server/index.md`
  illustrated sublibraries with a `hypha:lib-breakdown` that does not
  exist. `flags.md` described `--full` as "every field", when it adds
  `source` and `tiers_consulted` to output that already carries the whole
  Haddock body. `quick-start.md`'s worked example predated three schema
  changes. Seven "Placeholder" comments still told readers to overwrite
  screenshots that are real captures. `tested-with` claimed GHC 9.8 (no
  project file) and omitted 9.12 (which has one, and a green CI job).
  Finally, `Hypha.Exit` and `Hypha.Cli.Run` both attributed exit `9` to a
  `catchAny` in `app/hypha/Main.hs`; there is none — it comes from the
  `tryAny` in `runClientMain`.
- **`hypha doctor` no longer reports itself outside a plan.** `Outcome`
  is built positionally in one place and takes a `Bool` third, so
  `all_pass` was filed as `outside_plan` — a healthy doctor printed
  `outside_plan: true` directly above its own passing `plan_json` check.

- **`hypha source` and `hypha symbol` follow a re-export out of the
  package.** `hypha source base/Data.List/sortOn` reported `NOT_FOUND`:
  `Data.List` re-exports `sortOn` from `GHC.Internal.Data.List`, which
  re-exports it from `GHC.Internal.Data.OldList`, and both hops cross into
  `ghc-internal`. Since GHC 9.10 made `base` a facade that is the shape of
  nearly every `base` symbol, not an edge case. The CLI now builds the
  dependency graph a chain is followed through from the build plan it had
  already loaded, and the descent walks as many hops as the chain crosses
  rather than probing a single ring of candidates. `hypha symbol` resolves
  through the same path, so a card for a re-exported symbol carries its
  signature, Haddock and source instead of nothing.
- **A dependency's modules come from its cabal file, not a directory
  walk.** The enumeration guessed six conventional `hs-source-dirs` names
  and stopped four directories deep, which lost fourteen of
  `ghc-internal`'s modules — `base/Control.Monad.ST.Lazy/strictToLazyST`
  among them — and every module of a dependency with an unconventional
  source layout, local packages in a multi-package repo included. None of
  those losses were distinguishable from "the symbol is not there". Reading
  the stanza also settles which component owns a module, whether it is
  exposed, and which extensions it parses under: the last of these
  reintroduced across a package boundary a bug fixed one hop earlier.
- **Following a chain no longer reaches for the network speculatively.** A
  single missed module name walked the whole dependency closure through the
  full resolution chain — tarball extraction, then Hackage over HTTP — so
  `hypha source base/Prelude/lines` answered correctly and printed
  `Hackage HTTP 404 for 'rts'` on the way, for a package the query never
  needed. Speculative probes now consult only what is already unpacked, and
  what was skipped is reported where it could actually explain a failure.
- **`--offline` no longer downloads.** `fetchAndExtractSource` took a
  `HackageClient`, bound it to `_hclient`, and built its own connection
  manager, so the source path fetched whatever it wanted: `hypha --offline
  --cache-dir <empty> source base/Data.List/sortOn` answered having pulled
  both `base` and `ghc-internal` from Hackage, neither of which has a
  tarball under `~/.cabal`. Offline mode is expressed by *which client was
  built* and nothing else, so the tarball fetch is now a field of the
  client: the offline one has no way to reach the network rather than a
  branch that can be forgotten. Offline with a warm cache still answers
  from it.
- **A cross-package chain resolves on a machine that has never run
  hypha.** Making the probe local-only left nothing on this path able to
  put a dependency's source on disk: no cabal store entry ships a `src`
  directory and `cabal build` unpacks nothing a query can read, so with an
  empty source cache `hypha source base/Data.List/sortOn` — the case the
  feature exists for — answered `NOT_FOUND` with `search: exhausted` and
  advised `cabal build`, which does not help. Ownership is now decided
  before anything is fetched: `ghc-pkg` names the one unit that exposes the
  module, from the package databases, without reading a source file, and
  that unit alone is materialised. A cold `base/Data.List/sortOn` fetches
  `ghc-internal` and nothing else — not `ghc-prim`, `ghc-bignum` or `rts`,
  which the previous gap report listed for a chain that never needed them.
  A fetch that fails, and a machine whose compiler cannot be asked, are
  each reported as themselves.
- **`hypha symbol` and `hypha source` no longer answer for a module the
  package does not have.** Routing both through the shared locator made a
  misspelled module fall through to the package-wide scan, which ranks
  every same-named binding by shared path suffix and so always finds
  something: `hypha symbol containers/Data.Map.Strct/insertWith` (one
  letter missing) exited 0 with the `IntMap` function's signature and
  Haddock, under `module: Data.Map.Strct`. The scan is now reached only
  when the package has no readable library stanza — the case it was added
  for — or when the module's file exists but no stanza lists it (generated
  modules, CPP-selected variants). Otherwise the envelope says
  `search: module_absent`.
- **A search that stopped early no longer reports the symbol as absent.**
  Hitting the hop limit or the parse budget produced the same `NOT_FOUND`
  as a name that does not exist, plus a stderr line contradicting it.
  The envelope now carries which of the two happened (`search:
  stopped_at_bound` vs `exhausted`), the bound that was hit, the candidate
  modules considered, and any dependency whose source could not be read.
  A third verdict, `blocked`, covers the case the other two hid: a chain
  that drained while something in it could not be read has established no
  absence, and calling that `exhausted` told an agent the search had ruled
  out a symbol it never managed to look at. Verdicts established from the
  asking module's own parse (`does not export it`) stay `exhausted` however
  many gaps there are, because nothing outside the component could have
  changed them.
- **A module reached through an open import cannot win on a private
  homonym.** The descent took "declares the name" as "defines it", so a
  local helper called `lines` or `null` in a module that does not export it
  could be reported as the definition site.
- **`hypha source` says where the definition actually landed.** The output
  echoed the module the caller named beside a path in another package, so a
  follow-up query on that module went nowhere. A `defined_in` field names
  the declaring module and component when they differ from the request.
- **`hypha symbol`'s `kind` is no longer the constant `"function"`.** The
  card asserted the one thing it had not learned; it now reports what the
  parser classified, and omits the field when there is nothing to say.
- **`--select sig,haddock` no longer returns an empty result.** The
  documented short spellings are aliases for the wire field names
  (`sig` → `signature`, `haddock` → `haddock_raw`), so the invocation
  the skill docs and the MCP schema recommend now keeps the signature
  and the haddock instead of projecting a key that does not exist
  (issue 6). Both spellings work.
- **`hypha lookup` answers with a stable candidate order.** The cache
  query is now ordered by `(pkg, mod, version DESC)`, so a name several
  packages declare comes back the same way every time rather than in
  insertion order. The order is alphabetical by package, not a relevance
  ranking — `HTTP` precedes `aeson` — so it buys reproducibility, not a
  better first candidate (issue 6).
- **`--select` says so when it cannot answer.** A field name the command
  does not produce — a typo, a field of a different command, or one that
  only exists under `--full` — used to be dropped in silence, leaving
  `result: {}` and exit 0. It now warns on stderr, naming the fields the
  command *does* answer. Exit code and output shape are unchanged
  (issue 6).

- **Conditional cabal stanzas are read.** `if`/`elif`/`else` branches were
  discarded, so `base` contributed no `GHC.Event` and no
  `System.CPUTime.Posix.*` on any non-Windows machine — both are declared
  only in an `else` branch — and around 25 other cached packages lost
  modules the same way.
- **A facade over a facade resolves.** A re-export was matched against the
  *definition's* package rather than the module's own; since GHC 9.10 a
  package re-exporting `Data.Foldable.foldl'` from `base` was told the
  definition lives in `ghc-internal`, which it does not depend on, and the
  row was dropped.
- **The index expires.** Cache warmth was "a row exists for this
  `(package, version)`", so editing your own package and restarting the
  server served the previous run's index forever, a component that indexed
  to zero rows stayed warm, and the host-wide cache served one project's
  rows to another. Entries now carry a fingerprint over the source bytes,
  the compiler, the language settings, whether the cabal stanzas were
  readable, and the resolved dependency versions.
- **The parser no longer throws.** `cpphs` reports `#error` by calling
  `error` from pure code, which escaped the `Either`: `hypha symbol` on a
  module guarded by `#error "CURRENT_PACKAGE_KEY undefined"` aborted with a
  raw `ErrorCall`. Module parsing also no longer swallows the server's
  request-timeout cancellation and reports it as a parse failure.
- **An unplaced entry is not filed as a function.** It carried a
  fabricated `DkFunction`, which put `Bool`, `Maybe` and `Functor` under
  "Values" in `base/Prelude`'s rail with `#v:` anchors no `#t:` link
  resolves. Such entries now carry no kind and get their own rail group.
- **A `module M` re-export of a module outside the component is
  reported.** Those names cannot be expanded, so they reached neither the
  index nor the unresolved report — `mtl`'s `Control.Monad.State` exports
  `module Control.Monad` and contributed none of its names, silently.

- **A symbol card or module page resolves a re-export through the index**
  rather than one hop of imports, so a two-hop chain (`Data.List` →
  `GHC.Internal.Data.List` → `GHC.Internal.Data.Traversable`) no longer
  reports "symbol not found". `base/Data.List` renders 121 entries where
  it rendered none.
- **Links to symbols with operator characters work.** `#`, `/` and `?` in
  a symbol name are percent-encoded, so `unpackCString#` reaches its card
  instead of silently landing on the module page, and `System.FilePath.</>`
  no longer 404s.
- **The `+N` disclosure appears for a re-exported definition.** It was
  gated on the folded-in presentations, so a result whose definition module
  is no presentation at all — the re-export case the affordance exists for
  — showed no badge and left the definition unreachable.
- **`--quiet` does something.** It was parsed and read nowhere; it now
  overrides `--verbose`. It still does not silence the indexer's stderr
  diagnostics.
- **One unparseable module no longer degrades a whole component.** Every
  other module page keeps its entries, and the skipped module is named on
  stderr. A module `cpphs` cannot preprocess — an `#error` guarded on a
  macro only a real compiler defines — is skipped rather than answering
  the request with a 500.
- **A module page no longer presents a guess as a definition site.** An
  entry whose definition could not be resolved is listed as "re-exported,
  origin unresolved" rather than attributed to the nearest candidate
  import: `base/Prelude` had been telling the reader that `Bool`, `True`,
  `Just` and `map` are all defined in `GHC.Internal.Control.Monad`, with a
  link there, while the index knew `Bool` is `ghc-prim`'s.
- **Signatures are read at the definition site**, never matched by name,
  so `Data.IntMap` symbols no longer show `Map k a` signatures.
- **Indexed module names come from the parse tree**, not from file paths,
  so a stray `examples/race.hs` no longer becomes a module called `race`,
  and module enumeration comes from the cabal stanza. The package
  overview page still enumerates by walking the source tree, so it can
  still list a name a file path suggested.
- `hypha source pkg/Mod/sym` resolves through the component's exports
  instead of scanning the package for a same-named binding;
  `Data.Map.Strict.insertWith` used to resolve to
  `Data/IntMap/Internal.hs`.
- Haddock now comes from the GHC parse tree rather than a line scanner:
  `haddock_raw` in `hypha symbol` output no longer carries `-- |` comment
  markers, and a doc block separated from its declaration by a blank line
  binds correctly.
- `hypha lookup` no longer aborts with a misleading `NETWORK_ERROR`
  when the local Hoogle tier cannot run `haddock` (e.g. when the
  binary is genuinely missing, or when Claude Code's sandbox hides
  `~/.ghcup` from the spawned process). The cascade now falls through
  to remote Hoogle, and any genuine `ENOENT` from a child-process
  spawn is classified as `TOOL_MISSING` (exit `8`).
- `defaultHaddockRunner` catches `IOException` around the `haddock`
  invocation and returns a structured `HaddockError` instead of
  propagating the exception.
- `ensureProjectHoogle` is now actually best-effort (matching its
  docstring): any failure regenerating the project Hoogle DB is
  swallowed so the lookup cascade continues to the remote tier.

### Known limitations

- **An existing `hypha.db` is cleared on first open** and rebuilt in the
  background: index rows now carry a definition site and a visibility, and
  old rows may hold path-derived module names or signatures matched by
  name — defects that are not detectable row by row. See
  [Caching](website/src/guide/caching.md) for what to expect.
- **A member of a type declared in an unparseable module still has no
  row.** Class methods, constructors and record fields are indexed now,
  but only from source the parser can read: `GHC.Internal.Base` needs CPP
  (see the next entry), so `liftA2`, `pure` and `mappend` have no `base`
  row while `fmap` and `mempty` reach one through a re-exporter that does
  parse. `hypha lookup` still answers for all of them through Hoogle.
- **159 modules of a 283-package plan do not parse without CPP
  preprocessing**, and every module re-exporting from one loses exactly
  what it re-exported — which is why `Prelude` is sparse. Each is
  reported on stderr with GHC's own message.

## [0.2.0] — unreleased

### Breaking

- `hypha search` removed.  Use `hypha lookup`.
- `hypha whatprovides` removed.  Use `hypha lookup`.
- `--global` flag removed.  The new `lookup` cascade decides for
  itself which tier (cache → local Hoogle → remote Hoogle) answers
  the query.

### Added

- `TOOL_MISSING` error variant (exit code `8`) for the case where a
  required external binary (`haddock`, `cabal`, `ghc`, …) is not on
  `$PATH` — distinguishes this from `NETWORK_ERROR` (was previously
  misclassified) and from `ENV_ERROR` (broader environment problems).
  Updated SKILL.md failure-modes table and added a "Running under
  Claude Code's sandbox" troubleshooting section to the README.
- Claude Code plugin scaffold (`.claude-plugin/plugin.json`) with a
  `hypha-haskell` skill (`skills/hypha-haskell/SKILL.md`) that auto-loads
  on Haskell projects and teaches the agent to prefer the `hypha` CLI
  over `WebFetch` on hackage.haskell.org/hoogle.haskell.org and over
  ad-hoc grepping of `~/.cabal/store`. Includes a `/hypha-lookup` slash
  command shortcut.
- `hypha lookup <query>` — single tiered symbol-resolution command.
  Short-circuiting cascade with a structured `OutcomeEnvelope` on
  every result, including failures (`NOT_FOUND`, `HOOGLE_OFFLINE`,
  `HOOGLE_REMOTE_ERROR`).
- `--offline` flag (and `HYPHA_OFFLINE=1`) skips the remote Hoogle
  tier; cache + local Hoogle still consulted.
- `HYPHA_HOOGLE_TIMEOUT=<seconds>` overrides the remote timeout
  (default 10s).
- Source-tree fingerprint invalidation for local-package cache rows
  (`Hypha.Project.Fingerprint`).
- Project Hoogle DB lifecycle (`Hypha.Hoogle.Local`): scavenges
  `<pkg>.txt` from the cabal store when present, falls back to
  `haddock --hoogle` for local packages.  Regeneration gated by a
  plan-hash + aggregate-fingerprint stamp; concurrent searches
  serialised behind a single MVar.
- `Hypha.Hoogle.Remote`: HTTP client to `hoogle.haskell.org` with
  injectable transport and 24-hour `kv`-table caching.

### Internal

- Schema migration: `pkg_index_meta` gains a `fingerprint TEXT`
  column; idempotent so existing user DBs upgrade in place on next
  open.
- Modules removed: `Hypha.Command.Search`, `Hypha.Command.WhatProvides`,
  `Hypha.Hoogle.Query`, `Hypha.Hoogle.Database`.

## [0.1.0] — 2026-05-18

Initial public release. Bundles Plan A (CLI alpha), Plan B (local doc
browser), and Plan C (MCP stdio shim).

### Added — Plan A: CLI alpha

- Project resolution via `cabal-plan` discovery walking up to the project root.
- Build-plan resolution from `dist-newstyle/cache/plan.json`, with
  `--package-override PKG=VER` for transient overrides.
- `BuildEnv` records-of-functions over the cabal store and (optionally) the
  nix store, composed into a single backend.
- Hackage JSON client with SHA-256 keyed XDG cache, ETag and
  `If-Modified-Since` revalidation, throttling, and 429/503 backoff.
- Per-project Hoogle DB build, hashed against the plan.
- Output envelope (`OutcomeEnvelope`) with compact / full field sets,
  `--select` post-filtering, and `--human` ANSI rendering via
  `prettyprinter-ansi-terminal`.
- Subcommands: `search`, `package`, `module`, `symbol`, `source`, `versions`,
  `deps`, `whatprovides`, `doctor`.
- Typed `HyphaError` totality-checked against `ExitCode` (0/2/3/4/5/7).
- Seamless fallback chain (build plan → cabal store → Hackage); no `--any`
  flag, widening is automatic.

### Added — Plan B: local doc browser

- `hypha server` Warp-based HTMX UI bound to loopback only.
- `--prebuild` worker pool for warming the Haddock cache.
- Strict Content-Security-Policy middleware.
- Haddock cross-package URL rewriting (`../<pkg>-<ver>/...` →
  `/haddock/<pkg>-<ver>/...`).
- De-duplicated lazy Haddock build slots (one build per package, even under
  concurrent requests).
- Keymap (`s`/`/`/`Ctrl-K` search; `j`/`k`/arrows cursor; `gp`/`gh` go;
  `h`/`l`/arrows history; `?` help overlay; `Esc` dismiss).

### Added — Plan C: MCP

- `hypha-mcp` executable: JSON-RPC 2.0 stdio bridge that re-exports every
  CLI subcommand as an MCP tool.
- Startup banner emitted on stderr (keeps stdout pure for the protocol).
- Pattern B implementation: shells out to the `hypha` binary rather than
  duplicating the library API.

### Conventions

- Strict record fields (`!`) by default; lazy fields justified inline.
- No `error` / `undefined` in production paths.
- Imports minimal and sorted; shared shorthands in `Hypha.Prelude`.
- Property tests via `Test.Tasty.Falsify`; golden tests via
  `Test.Tasty.Golden`; unit tests via `Test.Tasty.HUnit`.

[Unreleased]: https://github.com/well-typed/hypha/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/well-typed/hypha/releases/tag/v0.1.0
