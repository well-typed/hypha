# Changelog

All notable changes to `hypha` are documented in this file. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/); the project
loosely follows [Semantic Versioning](https://semver.org/).

## [Unreleased]

## [0.2.0] — unreleased

### Breaking

- `hypha search` removed.  Use `hypha lookup`.
- `hypha whatprovides` removed.  Use `hypha lookup`.
- `--global` flag removed.  The new `lookup` cascade decides for
  itself which tier (cache → local Hoogle → remote Hoogle) answers
  the query.

### Added

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
