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
