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
  your **local project**, and (in the doc-browser server) every cabal
  `library NAME` sub-library of every package in the plan. Symbols point
  to the file:line where they're actually defined, not the re-export
  module — even across CPP `#ifdef` branches.
- **One tool, two surfaces.** Same code powers the CLI/MCP shim and the
  local doc-browser server, so agents and humans see the same data.

## Design philosophy: cli-printing-press

`hypha` is built to the principles laid out in
[cli-printing-press](https://github.com/mvanhorn/cli-printing-press) —
a manifesto for *agent-native* CLIs.  The relevant tenets and how
`hypha` honours them:

| cli-pp principle | hypha |
|---|---|
| **Agent-native by default** | Compact JSON is the default; `--human` is opt-in. |
| **Typed exit codes** | `0` success, `2` user error, `3` not found, `4` network, `5` cache corruption, `7` environment, `8` tool missing — every failure is classifiable without parsing error text. |
| **Local-first data layer** | SQLite caches (per-project + shared global), an on-disk Hoogle DB, ETag-revalidated Hackage HTTP cache, and a fuzzy index — all built so repeat queries stay off the network. |
| **Compact mode for tokens** | Compact JSON is the *default*; `--select f1,f2` projects fields; `--full` is opt-in. No HTML noise. |
| **Human + machine output modes** | `--human` for terminals, JSON for pipelines, HTMX-rendered HTML for the `server` UI — same data, three surfaces. |
| **Actionable errors** | Every `OutcomeEnvelope` failure carries a stable `code` and an `actions` map suggesting the next command to try. |
| **Verified, not vibes** | Property tests via `falsify`, golden JSON regressions via `tasty-golden`, edge cases via `tasty-hunit`. CI gates merges. |
| **Non-obvious insight** | Symbols resolve to the canonical declaration even across re-exports and CPP `#ifdef` branches — the value that raw Hackage HTML cannot give you. |
| **Dual interface from one spec** | The `hypha` CLI, the `hypha-mcp` JSON-RPC shim, and the `hypha server` HTML UI share one library — no duplicated client/store code. |

**CLI vs MCP, the cli-pp split.**  cli-printing-press is explicit that
*CLIs win for agents* (cheaper tokens, native to shell-trained LLMs)
and *MCP wins for IDE auto-discovery*.  `hypha` follows that split:

- **Agents should call `hypha` directly** through a shell tool.  The
  Claude Code skill (`skills/hypha-haskell/SKILL.md`) tells the model
  to prefer `Bash hypha …` over the MCP tools.
- **`hypha-mcp` exists for IDE/MCP-only harnesses** (Claude Desktop,
  Cursor, opencode without a shell).  It exposes one MCP tool per CLI
  subcommand (`hypha.lookup`, `hypha.symbol`, …) so IDE auto-discovery
  surfaces structured arguments; the generic `hypha.exec` remains as
  an escape hatch.

## Features

| Feature | Description |
|---------|-------------|
| **Project-aware queries** | Defaults to the versions pinned in `dist-newstyle/cache/plan.json`. No version guessing. |
| **Local package + private libraries** | The doc-browser server indexes the package at the project root *and* every cabal `library NAME` sublib of every package in the plan. Sublibs surface as `pkg:sublib` entries in the sidebar, URLs, and search index. (CLI/MCP sublib addressing is planned — see Identifier Syntax.) |
| **Token-efficient JSON** | Compact JSON by default; opt into more fields with `--full`, opt out with `--select`. No HTML noise. |
| **Aggressive caching** | ETag-revalidated Hackage cache, persistent SQLite search index, on-disk source + Haddock caches. Drastically reduces HTTP traffic and repeat work. |
| **Faithful source pointers** | Signatures + Haddock are re-extracted at the re-export target. Source links land on the canonical declaration, even across CPP `#ifdef` branches. |
| **MCP server** | Ships `hypha-mcp` as an stdio MCP shim for Claude Code, opencode, and any MCP client. Exposes one MCP tool per CLI subcommand (`hypha.lookup`, `hypha.symbol`, …) so IDEs see structured arguments, plus a generic `hypha.exec` escape hatch. |
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

### Claude Code plugin

`hypha` ships as a [Claude Code plugin](https://docs.claude.com/en/docs/claude-code/plugins)
that auto-loads a skill teaching Claude to prefer the `hypha` CLI over
`WebFetch` on hackage.haskell.org / hoogle.haskell.org and over ad-hoc
grepping of `~/.cabal/store`. The plugin lives at the root of this repo
(`.claude-plugin/{plugin,marketplace}.json` + `skills/hypha-haskell/SKILL.md`).

**Prerequisite:** the `hypha` and `hypha-mcp` binaries must already be on
`$PATH` — install per the [From source](#from-source-requires-ghc--96)
section above first. The plugin ships skill content, slash commands, and
an `mcpServers` declaration that auto-registers `hypha-mcp` with Claude
Code on install; it does **not** vendor the binaries themselves.

#### Option A — install from the Well-Typed marketplace (recommended)

Inside a Claude Code session:

```
/plugin marketplace add https://gitlab.well-typed.com/well-typed/hypha.git
/plugin install hypha@well-typed
```

> The first command opens an interactive TUI prompting you to confirm
> the marketplace add. Accept it, then run the second command.

#### Option B — install from a local clone

If you already have the repo checked out (e.g. for development):

```
/plugin marketplace add /absolute/path/to/hypha
/plugin install hypha@well-typed
```

Use the absolute path; Claude Code resolves the marketplace from the
directory's `.claude-plugin/marketplace.json`.

#### Verify the install

```
/plugin list
```

You should see `hypha@well-typed` enabled. Open any `.hs` or `.cabal`
file and Claude will auto-trigger the `hypha-haskell` skill on the next
Haskell question. The `/hypha-lookup <symbol-or-signature>` slash command
becomes available too.

#### Uninstall

```
/plugin uninstall hypha@well-typed
/plugin marketplace remove well-typed
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
<pkg>[@<version>][/<Module.Path>][/<symbol>]
```

Examples:

- `async` — package
- `async@2.2.5` — version-pinned package
- `async/Control.Concurrent.Async` — module
- `async/Control.Concurrent.Async/concurrently` — symbol
- `my-project/MyProject.Internal/helper` — a symbol from the **local** project

> **Sub-libraries:** The doc-browser server addresses sublibs as
> `pkg:sublib` in URLs (e.g. `/pkg/happy-lib:frontend`). The CLI/MCP
> path does **not** yet handle the `:<sublib>` suffix in identifier
> arguments — that's planned. For now, query a sublib by browsing it
> in the server UI.

## Global Flags

| Flag | Description |
|------|-------------|
| `--project-dir DIR` | Override project root |
| `--package-override PKG=VER` | Replace a plan entry (repeatable) |
| `--offline` | No network; skip the remote Hoogle tier in `lookup` |
| `--human` | Pretty ANSI text instead of JSON |
| `--pretty-json` | Indent JSON output |
| `--full` | Include all fields (default: compact) |
| `--select f1,f2,...` | Project only listed JSON fields |
| `--quiet` / `-q` | Suppress informational output |
| `--verbose` / `-v` | Show debug output |

## Subcommands

| Command | Args | Purpose |
|---------|------|---------|
| `lookup` | `QUERY` | Tiered symbol resolution (see below) |
| `package` | `<pkg>[@ver]` | Package metadata (latest, deprecation, license) |
| `module` | `<pkg>/<Mod>` | Exported symbols with signatures |
| `symbol` | `<pkg>/<Mod>/<sym>` | Full info: signature, Haddock, source coords |
| `source` | `<pkg>/<Mod>/<sym>` or `<pkg>/<Mod>` | Source slice |
| `versions` | `<pkg>` | Version history, plan-pinned marker |
| `deps` | `<pkg> [--reverse] [--depth N]` | Forward/reverse deps within the plan |
| `doctor` | — | Environment health check |
| `server` | `[--port N] [--prebuild]` | Doc-browser HTTP server |
| `mcp` | — | MCP stdio shim |

### Looking up symbols (`hypha lookup`)

Single entry point for the question *"which package/module provides
this?"*.  Runs a three-tier short-circuit cascade and returns at the
first hit:

1. **`PackageCache`** (SQLite): exact-name + qualified-name lookup
   (e.g. both `lookup` and `Data.Map.lookup`).
2. **Local Hoogle DB** at `<project>/.hypha/hoogle.hoo`: built lazily
   from scavenged store `*.txt` files plus on-demand `haddock --hoogle`
   for local packages.  Handles type-signature queries.
3. **Remote Hoogle** at `hoogle.haskell.org`: HTTP fallback.  Cached in
   the global `kv` table; skipped under `--offline` / `HYPHA_OFFLINE=1`.

`hypha lookup` always emits a structured `OutcomeEnvelope`.  Failures
carry a `code` (`NOT_FOUND`, `HOOGLE_OFFLINE`, `HOOGLE_REMOTE_ERROR`)
and `actions` suggesting how to retry.

### Cache layout

| Path | Purpose |
|------|---------|
| `~/.cache/hypha/hypha.db` | Global SQLite cache: store-package symbol index + remote-Hoogle KV cache |
| `~/.cache/hypha/hoogle-txt/` | Scratch dir for `haddock --hoogle` outputs |
| `<project>/.hypha/cache.db` | Project SQLite cache: local + SRP package symbol index |
| `<project>/.hypha/hoogle.hoo` | Project Hoogle DB |
| `<project>/.hypha/hoogle-stamp` | Plan-hash + aggregate-fingerprint stamp |
| `<project>/.hypha/hoogle-input/` | Symlinks / copies of the `.txt` files fed to `hoogle generate` |

No `invalidate` subcommand is shipped (agents would footgun).  To
force a rebuild: `rm -rf <project>/.hypha` (project-only) or
`rm -rf ~/.cache/hypha` (global).

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

## MCP Host Integration

`hypha-mcp` is a thin JSON-RPC 2.0 stdio shim. It exposes one MCP
tool per CLI subcommand — `hypha.lookup`, `hypha.package`,
`hypha.module`, `hypha.symbol`, `hypha.source`, `hypha.versions`,
`hypha.deps`, `hypha.doctor` — each with a structured input schema
that IDE clients can render as a form. A generic `hypha.exec` tool
remains as an escape hatch for argv-level invocation.

Per cli-printing-press: **agents in a shell-capable harness should
call `hypha` directly via `Bash` / equivalent**, not via MCP. The
MCP surface is here for IDE auto-discovery (Claude Desktop, Cursor)
and for harnesses without a shell.

In case your AI harness of choice doesn't support Claude plugins,
you can still add `hypha-mcp` as an MCP client:

**Claude Code** — if you installed the [Claude Code plugin](#claude-code-plugin),
`hypha-mcp` is registered for you on `/plugin install`; no manual config
needed. To wire it up manually, add to `~/.claude.json`:

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

**Generic MCP client** — point any MCP-compatible host at the
`hypha-mcp` executable over stdio. It speaks JSON-RPC 2.0 and exposes
the `hypha.exec` tool described above.

## Exit Codes

| Code | Meaning |
|------|---------|
| `0` | Success |
| `2` | User error (bad args, malformed path, non-loopback bind) |
| `3` | Not found (symbol/package absent across plan → store → Hackage) |
| `4` | Network error (offline cache miss, HTTP 429/503, transport failure) |
| `5` | Cache / on-disk corruption |
| `7` | Environment error (no `plan.json`, unreachable store) |
| `8` | Tool missing — required external binary (`haddock`, `cabal`, `ghc`) not on `$PATH` |

## Troubleshooting

### Running under Claude Code's sandbox

Claude Code runs `Bash` commands inside a filesystem sandbox that may
hide your toolchain directories from the spawned process. Symptoms:

- `hypha lookup` reports `TOOL_MISSING` for `haddock` (or `cabal`,
  `ghc`) even though those binaries work in your terminal.
- `hypha source` returns empty results that you can reproduce
  manually.

Cause: `~/.ghcup`, `~/.cabal/store`, and similar paths are not in the
sandbox's read allowlist. From the sandboxed process's view they
return `ENOENT`, so PATH-resolved binaries appear missing and
cabal-store reads find nothing.

Fix: widen the sandbox's read allowlist in your Claude Code
`settings.json`. The exact key depends on your Claude Code version,
but the directories `hypha` needs to see are typically:

- `~/.ghcup/**` — required if hypha needs to spawn `haddock`
- `~/.cabal/store/**` — required for `hypha source` / `hypha symbol`
- `~/.cache/cabal/**` — speeds up the Hackage HTTP cache
- `/etc/ssl/certs/**` (or `$SSL_CERT_FILE`) — required for TLS to
  hackage.haskell.org / hoogle.haskell.org; without it the remote
  tier fails with `HandshakeFailed ... certificate has unknown CA`

`hypha` is designed to degrade gracefully here: the `lookup` cascade
falls through to remote Hoogle when the local Hoogle tier cannot run
`haddock`, and reports `TOOL_MISSING` (exit `8`) rather than the
misleading `NETWORK_ERROR`. Widening the sandbox just restores the
local-fast path.

### Running under other harnesses (pi, sbox, …)

The same class of failures hits any sandboxed harness driving hypha,
not just Claude Code. If you wrap `hypha` (or an agent that calls it)
in `sbox`, `bwrap`, `firejail`, or similar, expose the same paths as
read-only mounts. A minimal recipe for `sbox`:

```bash
sbox \
  --rw  /path/to/your-project \
  --ro  ~/.ghcup \
  --ro  ~/.cabal/store \
  --ro  /etc/ssl/certs \
  -- <your-agent> --provider … --model …
```

Plus whatever paths the harness itself needs (e.g. `~/.pi` for the
`pi` harness's model config). Without these, hypha sees `TOOL_MISSING`
for the toolchain and TLS failures for the remote tier.

## Caching

`hypha` caches everything network-shaped — and as much of the
plan-shaped derived state as possible — under `$XDG_CACHE_HOME/hypha/`
(defaults to `~/.cache/hypha/`):

| Cache | Layout | Freshness |
|-------|--------|-----------|
| Search index | `hypha.db` (SQLite, WAL) | Keyed on `(pkg, version)` and shared across every project on the host |
| Hackage HTTP responses | `hackage/<sha256>.json` | ETag + `If-Modified-Since` revalidation; 15 min TTL on every cached entry |
| Source tarballs | `source/<pkg>-<ver>/` | Immutable once extracted |
| Haddock HTML | `haddock/<pkg>-<ver>/` | Built on demand, reused across runs |
| Hoogle DB | `<projectRoot>/.hypha/hoogle.hoo` (with `.hypha/plan-hash` sibling) | Rebuilt when `plan.json` changes |

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
