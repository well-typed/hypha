<p align="center">
  <img src="./images/hypha-logo.png" width="200" alt="hypha logo" />
</p>

# Hypha

*A Haskell-aware code/doc browser tuned for AI agents and humans alike.*

AI agents don't browse Hackage. They fetch HTML pages and burn input tokens
parsing chrome, navigation, and boilerplate just to find a signature or a
Haddock paragraph. They also re-grep the local source tree on every
follow-up question. Both are expensive.

`hypha` exists to make Haskell knowledge **cheap to consume**:

- **Token economy.** Every command emits compact, structured **YAML** by
  default (or `--json` for machine pipelines). No HTML. Use `--select` to
  drop the fields you don't need — `hypha symbol … --select sig,haddock`
  for just the type and the docs — and `--full` only when you do.
- **Cache-aggressive, Hackage-friendly.** Network responses are cached on
  disk with ETag + `If-Modified-Since` revalidation. The same project
  re-queried a thousand times produces a small handful of HTTP requests.
  The search index is persisted in SQLite and shared across every project
  on your machine — if two projects depend on `containers-0.6.7`, the
  second inherits the first's work.
- **Plan-aware, source-faithful.** Reads your
  `dist-newstyle/cache/plan.json` so answers reflect the exact versions
  you're building against — including your **local project**, and (in the
  doc-browser server) every cabal `library NAME` sub-library of every
  package in the plan. Symbols point to the `file:line` where they're
  actually defined, not the re-export module — following a chain of
  re-exports across module *and* package boundaries, so `base`'s façades
  resolve into `ghc-internal`. Modules that need CPP preprocessing are
  reported as skipped rather than guessed at.
- **One tool, two surfaces.** The same library powers the CLI and the
  local doc-browser server, so agents and humans see the same data.

<p align="center">
  <img src="./images/hero-cli.png" width="47%" alt="hypha CLI — default YAML output" />
  &nbsp;
  <img src="./images/hero-server.png" width="47%" alt="hypha server — doc browser UI" />
</p>
<!-- These currently show labelled placeholders. Overwrite images/hero-cli.png
     and images/hero-server.png in place with real captures — no edits needed
     here. See images/CAPTURE-LIST.md. -->

## Where to next

- **[Installation](getting-started/installation.md)** — build from source or Nix.
- **[Quick Start](getting-started/quick-start.md)** — materialise a plan and run your first query.
- **[Claude Code Plugin](getting-started/claude-plugin.md)** — teach Claude to prefer `hypha`.
- **[Guide](guide/identifiers.md)** — identifier syntax, subcommands, flags, caching.
- **[Doc Browser Server](server/index.md)** — the visual surface.
- **[Design](design/philosophy.md)** — the `cli-printing-press` ethos and architecture.

---

Licensed under BSD-3-Clause.
