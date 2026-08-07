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
