<p align="center">
  <img src="logo/hypha.png" width="240" alt="hypha logo" />
</p>

<h1 align="center">hypha</h1>

<p align="center">
  <em>An agent-first CLI that probes Hackage, Hoogle, and your cabal build plan.</em>
</p>

<p align="center">
  <a href="#features">Features</a> ·
  <a href="#installation">Installation</a> ·
  <a href="#quick-start">Quick Start</a> ·
  <a href="#output-schema">Output</a> ·
  <a href="#mantra">Design</a>
</p>

---

## Why hypha?

When an AI agent needs to answer a Haskell question — "does `async` expose a
`concurrentlyE`?" — it usually opens a browser, manually hunts for the right
version on Hackage, and clicks through pages of HTML documentation.

**`hypha` closes that loop.**

It reads your project's `plan.json` to know exactly which versions you're
building against, and provides structured JSON on every command — first for
agents, with `--human` when humans need to read it.

## Features

| Feature | Description |
|---------|-------------|
| **Project-aware queries** | Defaults to the versions pinned in `dist-newstyle/cache/plan.json`. No version guessing. |
| **Hoogle search** | Per-project Hoogle DB built from your build plan. Scoped to your actual dependencies. |
| **Hackage metadata** | JSON-first access to package metadata, versions, and docs — with aggressive ETag caching. |
| **Agent-first JSON** | Every command emits structured JSON. `--human` opts into pretty ANSI text. |
| **Cross-recursion** | Every response includes `actions` and `related` fields containing more `hypha` invocations — *never* raw URLs. |
| **Source & Haddock** | Locates source from the cabal store and lazily builds Haddock docs when needed. |
| **MCP server** | Ships `hypha-mcp` as an stdio MCP shim for Claude Code, opencode, and generic MCP clients. |
| **Doc browser server** | Optional HTTP server with HTMX-driven live search, dark/light mode, and keyboard navigation. |

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

### 1. Make sure you have a build plan

```bash
cd /path/to/your-cabal-project
cabal build --dry-run
# this materialises dist-newstyle/cache/plan.json
```

### 2. Query a symbol

```bash
hypha symbol async/Control.Concurrent.Async/concurrently
```

```json
{
  "schema": "hypha/v0",
  "command": "symbol",
  "ok": true,
  "outside_plan": false,
  "overrides": [],
  "result": {
    "name": "concurrently",
    "kind": "function",
    "package": "async",
    "version": "2.2.5",
    "module": "Control.Concurrent.Async",
    "signature": "IO a -> IO b -> IO (a, b)",
    "source": { "path": ".../Async.hs", "line": 234 }
  },
  "actions": {
    "view_source": "hypha source async/Control.Concurrent.Async/concurrently",
    "module_index": "hypha module async/Control.Concurrent.Async",
    "package_info": "hypha package async"
  },
  "related": [
    { "label": "race", "fetch": "hypha symbol async/Control.Concurrent.Async/race" },
    { "label": "withAsync", "fetch": "hypha symbol async/Control.Concurrent.Async/withAsync" }
  ]
}
```

### 3. Search with Hoogle

```bash
hypha search "IO a -> IO b -> IO (a, b)"
```

### 4. Human-readable output

```bash
hypha symbol async/Control.Concurrent.Async/concurrently --human
```

## Identifier Syntax

```
<pkg>[@<version>][/<Module.Path>][/<symbol>]
```

Examples:

- `async` — package
- `async@2.2.5` — version-pinned package
- `async/Control.Concurrent.Async` — module
- `async/Control.Concurrent.Async/concurrently` — specific symbol

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

Launch a local doc browser bound to loopback only.  Pairs nicely with
`--prebuild` to warm the Haddock cache before you hit the page:

```bash
$ hypha server --port 4287
hypha server listening on http://127.0.0.1:4287
```

| Flag | Default | Purpose |
|------|---------|---------|
| `--port N` | `4287` | Loopback port to bind |
| `--bind HOST:PORT` | `127.0.0.1:<port>` | Explicit loopback bind (`localhost`, `127.0.0.1`, or `::1`) |
| `--prebuild` | off | Render Haddocks for every plan package up front |
| `--prebuild-jobs N` | `4` | Maximum concurrent prebuild workers |

Non-loopback binds (e.g. `0.0.0.0:4287`) are refused with exit code `2`.
There is no remote-access flag — sharing is out-of-scope on purpose.

Endpoints:

| Path | Returns |
|------|---------|
| `/` | HTML shell with sidebar + search |
| `/search?q=...` | HTMX results fragment |
| `/pkg/<pkg>` | Package overview |
| `/pkg/<pkg>/<Mod>` | Module page |
| `/pkg/<pkg>/<Mod>/<sym>` | Symbol card |
| `/source/<pkg>/<Mod>` | Highlighted source |
| `/haddock/<pkg>-<ver>/...` | Rewritten Haddock HTML |
| `/healthz` | `ok` (plain text) |

## Mantra

> An LLM doesn't need or care about fancy Haddock HTML pages — it cares about
> the source code, which also contains the comments (the documentation). That is
> what an LLM needs in order to learn knowledge of a project. Humans need
> visuals.

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

`hypha` aggressively caches everything network-shaped under
`$XDG_CACHE_HOME/hypha/` (defaults to `~/.cache/hypha/`):

| Cache | Layout | Freshness |
|-------|--------|-----------|
| Hackage HTTP responses | `hackage/<sha256>.json` | ETag + `If-Modified-Since` revalidation; 15 min TTL for mutable resources, immutable bodies cached forever |
| Source tarballs | `source/<pkg>-<ver>/` | Immutable once extracted |
| Haddock HTML | `haddock/<pkg>-<ver>/` | Built on demand, reused across runs |
| Hoogle DB | `hoogle/<plan-hash>.hoo` | Rebuilt when `plan.json` changes |

The fallback chain is automatic: **local HTTP cache → build plan → cabal
store → Hackage**.  There is no `--any` flag — widening is seamless.

## Architecture

```
hypha .............. CLI entry point
hypha-mcp .......... MCP/stdio shim (Pattern B: shells out to hypha CLI)
library: hypha
  ├── BuildEnv ....... Cabal store + Nix store + composition
  ├── Hoogle ......... Per-project DB + freshness via plan-hash
  ├── Hackage ........ JSON API + ETag/Last-Modified cache
  ├── Output ......... Compact/full JSON, envelope, --select
  └── Server ......... HTMX-driven doc browser (optional, Plan B scope)
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

Pre-alpha. The project is tracking issues in `issues/todo/` and `issues/done/`.

## Etymology

*Hypha* (plural *hyphae*) — the branching, threadlike cell of a fungus that
probes through soil, wood, and leaf litter seeking nutrients. The metaphor is
deliberate: `hypha` probes through the Hackage / cabal-store / source-tree
substrate of a Haskell project, finding the symbols, packages, types, and
documentation your agent needs.

## License

BSD-3-Clause. See [`LICENSE`](LICENSE).

---

<p align="center">
  <em>Built with <code>λ</code> by <a href="https://well-typed.com">Well-Typed LLP</a></em>
</p>
