# Route the indexer's and the browser's diagnostics through the tracer

**Status:** todo
**Type:** bug
**Found by:** code review of `adinapoli/more-server-improvements`

## Problem

`Hypha.Logging` already defines `Tracer`, `LogEvent (LogInfo | LogDebug |
LogWarning)`, and `silentTracer` / `verboseTracer`, wired in
`Hypha.Types` from `hoVerbose`. Fifteen diagnostic sites added by the
index-correctness work bypass all of it and write to `stderr`
unconditionally:

- `Hypha.Search.Indexer`: no source for a package, the four per-item
  reporters in `reportComponentIndex`, no cabal module list
- `Hypha.Command.Server`: four sites around module docs and imported
  sources
- `Hypha.Source.Locate`: four sites around parse failures and absent
  sources
- `Hypha.Search.Cache`: the row-anomaly report
- `Hypha.Command.Source`: the sweep-fallback announcement
- `Hypha.Project.Components`: the cabal read/parse and unknown-extension
  reports (added by the same review)

Consequences:

1. `--quiet` and `--verbose` have no effect on any of it, which
   `website/src/guide/flags.md` now has to admit in prose.
2. `reportUnresolved` is `mapM_`-ed over every unresolved export with no
   bound. On this project's 283-package plan that is **7217 lines** on any
   cold or format-bumped start.

Reporting these is right — the ethos forbids swallowing them — so this is
about the channel and the volume, not about going quiet.

## Fix

- Thread `heTracer` (or an explicit `Tracer m LogEvent` parameter, since
  `Indexer` and `Locate` are not in `Hypha`) into the reporting sites, and
  classify: a skipped module and an unreadable cabal file are
  `LogWarning`, per-export detail is `LogDebug`.
- Summarise the per-item reports at `LogInfo`/`LogWarning` and keep the
  enumeration for `LogDebug`:
  `base: 1142 exports unresolved through 47 unparseable modules (--verbose for each)`.
- Once done, drop the caveat sentence from `website/src/guide/flags.md`.

## Acceptance criteria

- `hypha server --quiet` on a cold cache prints no per-export lines.
- `--verbose` prints them.
- A test pins the summary line's shape for a component with unresolved
  exports.

## Note

`--quiet` now overrides `--verbose` when the tracer is chosen, so the flag
is no longer a complete no-op.  It still cannot silence the sites listed
above, because they do not go through the tracer at all — that is what
this issue is for.
