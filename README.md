<p align="center">
  <img src="logo/hypha.png" width="240" alt="hypha logo" />
</p>

<h1 align="center">hypha</h1>

<p align="center">
  <em>A Haskell-aware code/doc browser tuned for AI agents and humans alike.</em>
</p>

<p align="center">
  <a href="#why-hypha">Why</a> ·
  <a href="#features">Features</a> ·
  <a href="#installation">Installation</a> ·
  <a href="#quick-start">Quick Start</a> ·
  <a href="#local-doc-browser-hypha-server">Server</a> ·
  <a href="#mantra">Design</a>
</p>

---

## Why hypha?

AI agents don't browse Hackage. They `WebFetch` HTML pages and burn input
tokens parsing chrome, navigation, and boilerplate just to find a signature
or a Haddock paragraph. They also re-grep the local source tree on every
follow-up question. Both are expensive.

`hypha` exists to make Haskell knowledge **cheap to consume**:

- **Token economy.** Every command emits compact, structured JSON (or
  `--human` ANSI prose). No HTML. Use `--select sig,haddock` to drop the
  fields you don't need; use `--full` only when you do.
- **Cache-aggressive, Hackage-friendly.** Network responses are cached
  on disk with ETag + `If-Modified-Since` revalidation. The same project
  re-queried a thousand times produces a small handful of HTTP requests.
  The search index is persisted in SQLite and shared across every project
  on your machine — if two projects depend on `containers-0.6.7`, the
  second one inherits the first one's work.
- **Plan-aware, source-faithful.** Reads your `dist-newstyle/cache/plan.json`
  so answers reflect the exact versions you're building against — including
  your **local project and its private libraries** — and points symbols to
  the file:line where they're actually defined, not the re-export module.
- **One tool, two surfaces.** Same code powers the CLI/MCP shim and the
  local doc-browser server, so agents and humans see the same data.

## Features

| Feature | Description |
|---------|-------------|
| **Project-aware queries** | Defaults to the versions pinned in `dist-newstyle/cache/plan.json`. No version guessing. |
| **Local package + private libraries** | Indexes the package at the project root *and* every cabal `library NAME` sublib of every package in the plan. Sublibs surface as `pkg:sublib` entries everywhere. |
| **Token-efficient JSON** | Compact JSON by default; opt into more fields with `--full`, opt out with `--select`. No HTML noise. |
| **Aggressive caching** | ETag-revalidated Hackage cache, persistent SQLite search index, on-disk source + Haddock caches. Drastically reduces HTTP traffic and repeat work. |
| **Faithful source pointers** | Signatures + Haddock are re-extracted at the re-export target. Source links land on the canonical declaration, even across CPP `#ifdef` branches. |
| **MCP server** | Ships `hypha-mcp` as an stdio MCP shim for Claude Code, opencode, and any MCP client. Exposes every CLI subcommand as a tool. |
| **Local doc-browser server** | Optional HTTP server with a command-palette fuzzy search (FZF / Telescope style), shimmering "Building docs…" placeholder, top progress bar, and Haddock prose rendered to clean HTML. |

## Installation

### From source (requires GHC ≥ 9.6)

```bash
git clone https://gitlab.well-typed.com/well-typed/hypha.git
cd hypha
cabal build all
cabal install exe:hypha
cabal install exe:hypha-mcp
```

### Nix (flakes)

```bash
nix run gitlab:well-typed/hypha#hypha -- --help
```

## Quick Start

### 1. Materialise a build plan

```bash
cd /path/to/your-cabal-project
cabal build --dry-run        # writes dist-newstyle/cache/plan.json
```

### 2. Query a symbol — compact JSON

```bash
hypha symbol async/Control.Concurrent.Async/concurrently
```

```json
{
  "schema": "hypha/v0",
  "command": "symbol",
  "ok": true,
  "result": {
    "name": "concurrently",
    "package": "async",
    "version": "2.2.5",
    "module": "Control.Concurrent.Async",
    "signature": "IO a -> IO b -> IO (a, b)"
  },
  "actions": {
    "view_source": "hypha source async/Control.Concurrent.Async/concurrently",
    "module_index": "hypha module async/Control.Concurrent.Async"
  }
}
```

### 3. Project only the fields you need

```bash
hypha symbol async/Control.Concurrent.Async/concurrently --select signature,haddock
```

### 4. Human-readable output

```bash
hypha symbol async/Control.Concurrent.Async/concurrently --human
```

## Identifier Syntax

```
<pkg>[:<sublib>][@<version>][/<Module.Path>][/<symbol>]
```

Examples:

- `async` — package
- `async@2.2.5` — version-pinned package
- `happy-lib:frontend` — sub-library
- `async/Control.Concurrent.Async` — module
- `async/Control.Concurrent.Async/concurrently` — symbol
- `my-project/MyProject.Internal/helper` — a symbol from the **local** project

## Global Flags

| Flag | Description |
|------|-------------|
| `--project-dir DIR` | Override project root |
| `--package-override PKG=VER` | Replace a plan entry (repeatable) |
| `--any` | Widen query outside build plan |
| `--global` | Use global Hoogle DB instead of per-project |
| `--offline` | No network; fail closed |
| `--human` | Pretty ANSI text instead of JSON |
| `--pretty-json` | Indent JSON output |
| `--full` | Include all fields (default: compact) |
| `--select f1,f2,...` | Project only listed JSON fields |

## Subcommands

| Command | Args | Purpose |
|---------|------|---------|
| `search` | `QUERY` | Hoogle search scoped to build plan |
| `package` | `<pkg>[@ver]` | Package metadata (latest, deprecation, license) |
| `module` | `<pkg>/<Mod>` | Exported symbols with signatures |
| `symbol` | `<pkg>/<Mod>/<sym>` | Full info: signature, Haddock, source coords |
| `source` | `<pkg>/<Mod>/<sym>` or `<pkg>/<Mod>` | Source slice |
| `versions` | `<pkg>` | Version history, plan-pinned marker |
| `deps` | `<pkg> [--reverse] [--depth N]` | Forward/reverse deps within the plan |
| `whatprovides` | `<symbol>` | Packages exporting that symbol |
| `doctor` | — | Environment health check |
| `server` | `[--port N] [--prebuild]` | Doc-browser HTTP server |
| `mcp` | — | MCP stdio shim |

## Local Doc Browser (`hypha server`)

A loopback-only doc browser built for the same data as the CLI, but with a
visual surface humans can scan quickly. The first run pays the indexing
cost; every subsequent run hits the SQLite cache and renders results on the
first keystroke.

```bash
$ hypha server --port 4287
hypha server listening on http://127.0.0.1:4287
```

Highlights:

- **Command-palette fuzzy search.** Type `Data.Map lookup` or
  `Data.Map.Strict.lookup` — FZF/Telescope-style tokenised matching ranks
  the canonical symbol first. The dropdown is centered under the search
  bar and works the same on every page.
- **Live build-progress feedback.** Slim accent-coloured progress bar at
  the top of the page shows how many packages remain to index. A
  shimmering "Building the docs…" placeholder fills the dropdown until
  the index is warm.
- **Faithful symbol cards.** Multi-line signatures are joined, Haddock
  prose is parsed and rendered to HTML (paragraphs, `<code>`, `<pre>`
  code blocks, lists, links), and the source link points at the canonical
  declaration — even when the symbol is re-exported.
- **Skylighting-rendered source view** with `?line=N` scroll target.
- **Private libraries.** Sublibs appear as separate sidebar entries
  (`nike`, `nike:lib-breakdown`), each with their own pages and search
  scope.

| Flag | Default | Purpose |
|------|---------|---------|
| `--port N` | `4287` | Loopback port to bind |
| `--bind HOST:PORT` | `127.0.0.1:<port>` | Explicit loopback bind (`localhost`, `127.0.0.1`, or `::1`) |
| `--prebuild` | off | Render Haddocks for every plan package up front |
| `--prebuild-jobs N` | `4` | Maximum concurrent prebuild workers |

Non-loopback binds (e.g. `0.0.0.0:4287`) are refused with exit code `2`.
There is no remote-access flag — sharing is out of scope on purpose.

Endpoints:

| Path | Returns |
|------|---------|
| `/` | HTML shell with sidebar + search |
| `/search?q=...` | HTMX results fragment (fuzzy ranked) |
| `/progress` | HTMX progress-bar fragment (self-polling) |
| `/pkg/<pkg>` or `/pkg/<pkg>:<sublib>` | Package / sublib overview |
| `/pkg/<pkg>/<Mod>` | Module page |
| `/pkg/<pkg>/<Mod>/<sym>` | Symbol card |
| `/source/<pkg>/<Mod>` | Highlighted source |
| `/haddock/<pkg>-<ver>/...` | Rewritten Haddock HTML |
| `/healthz` | `ok` (plain text) |

## Mantra

> An LLM doesn't need or care about fancy Haddock HTML pages — it cares
> about the source code, which also contains the comments (the
> documentation). That is what an LLM needs to learn the shape of a
> project. Humans need visuals. `hypha` gives both surfaces the same
> data through the same code.

## MCP Host Integration

Add `hypha-mcp` to your MCP client:

**Claude Code** (`~/.claude.json`):
```json
{
  "mcpServers": {
    "hypha": {
      "command": "hypha-mcp",
      "args": []
    }
  }
}
```

**opencode** (`opencode.json`):
```json
{
  "mcp": {
    "hypha": {
      "type": "local",
      "command": ["hypha-mcp"]
    }
  }
}
```

**Generic MCP client** — point any MCP-compatible host at the `hypha-mcp`
executable over stdio. It speaks JSON-RPC 2.0 and exposes every `hypha`
subcommand as an MCP tool.

## Exit Codes

| Code | Meaning |
|------|---------|
| `0` | Success |
| `2` | User error (bad args, malformed path, non-loopback bind) |
| `3` | Not found (symbol/package absent across plan → store → Hackage) |
| `4` | Network error (offline cache miss, HTTP 429/503, transport failure) |
| `5` | Cache / on-disk corruption |
| `7` | Environment error (no `plan.json`, missing GHC/Haddock, unreachable store) |

## Caching

`hypha` caches everything network-shaped — and as much of the
plan-shaped derived state as possible — under `$XDG_CACHE_HOME/hypha/`
(defaults to `~/.cache/hypha/`):

| Cache | Layout | Freshness |
|-------|--------|-----------|
| Search index | `hypha.db` (SQLite, WAL) | Keyed on `(pkg, version)` and shared across every project on the host |
| Hackage HTTP responses | `hackage/<sha256>.json` | ETag + `If-Modified-Since` revalidation; 15 min TTL for mutable resources, immutable bodies cached forever |
| Source tarballs | `source/<pkg>-<ver>/` | Immutable once extracted |
| Haddock HTML | `haddock/<pkg>-<ver>/` | Built on demand, reused across runs |
| Hoogle DB | `hoogle/<plan-hash>.hoo` | Rebuilt when `plan.json` changes |

The fallback chain is automatic for network reads: **local HTTP cache →
build plan → cabal store → Hackage**.

## Architecture

```
hypha .............. CLI entry point
hypha-mcp .......... MCP/stdio shim (Pattern B: shells out to hypha CLI)
library: hypha
  ├── BuildEnv ....... Cabal store + Nix store + composition
  ├── Project ........ plan.json → BuildPlan + per-package components
  ├── Hoogle ......... Per-project DB + freshness via plan-hash
  ├── Hackage ........ JSON API + ETag/Last-Modified cache
  ├── Search ......... SQLite-backed fuzzy index + FZF-style scorer
  ├── Output ......... Compact/full JSON, envelope, --select
  └── Server ......... HTMX-driven doc browser with command-palette UX
```

For full details see [`docs/superpowers/specs/2026-05-18-hypha-design.md`](docs/superpowers/specs/2026-05-18-hypha-design.md).

## Development

```bash
git clone https://gitlab.well-typed.com/well-typed/hypha.git
cd hypha
cabal build all
cabal test all
```

Tests use `falsify` for property testing, `tasty-golden` for JSON output
regression testing, and `tasty-hunit` for specific edge cases.

## Status

Pre-alpha. Work is tracked in `issues/todo/` and `issues/done/`, and in
`docs/superpowers/{specs,plans}/` for design + implementation plans.

## Etymology

*Hypha* (plural *hyphae*) — the branching, threadlike cell of a fungus
that probes through soil, wood, and leaf litter seeking nutrients. The
metaphor is deliberate: `hypha` probes through the Hackage / cabal-store
/ source-tree substrate of a Haskell project, finding the symbols,
packages, types, and documentation your agent (or you) needs.

## License

BSD-3-Clause. See [`LICENSE`](LICENSE).

---

<p align="center">
  <em>Built with <code>λ</code> by <a href="https://well-typed.com">Well-Typed LLP</a></em>
</p>
