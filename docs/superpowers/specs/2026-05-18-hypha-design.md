# hypha — design

| Field    | Value                                              |
|----------|----------------------------------------------------|
| Status   | Draft v0 (post-brainstorm)                         |
| Date     | 2026-05-18                                         |
| Authors  | Alfredo (Well-Typed) + Claude (brainstorming pair) |
| Audience | WT engineers, AI agents (Claude Code, opencode, Pi, …) |
| License  | BSD-3-Clause                                       |

## 1. Motivation

When working on a cabal project, valuable information already lives on disk: `dist-newstyle/cache/plan.json` pins every package version we build against, `~/.cabal/store/` holds the built packages and (often) their pre-rendered Haddock HTML, `dist-newstyle/src/` holds `source-repository-package` checkouts, and the GHC toolchain is known from the environment. Today, answering a simple question — "does `async` expose a `concurrentlyE` variant in the version this project pins?" — still requires opening `hackage.haskell.org`, manually selecting the right version, fuzzy-searching, and clicking through to read Haddock or source.

`hypha` is a Haskell CLI that closes this loop for both human engineers and AI agents. It is **agent-first**: structured JSON by default, every result decorated with `hypha`-invocation hints that keep the agent inside the tool rather than fetching Hackage URLs. It is **project-aware**: queries default to the versions the current build plan actually uses, with explicit escape hatches when the user wants to widen. It bundles **Hoogle** as a library, generates per-project search databases that match the pinned plan, and exposes a local **doc-browser HTTP server** for humans who want to read fully-rendered Haddock with a fuzzy search bar and cross-package navigation.

`hypha` co-exists with [`haskell-docs-cli`](https://github.com/lazamar/haskell-docs-cli); they serve different audiences and have non-overlapping interaction models. `hypha` does not aim to replace that tool.

## 2. Name and etymology (for README)

`hypha` (plural *hyphae*) is the branching threadlike cell of a fungus that probes through substrate — soil, wood, leaf litter — seeking and absorbing nutrients. The metaphor is exact: `hypha` probes through the Hackage / cabal-store / source-tree substrate of a Haskell project, finding the symbols, packages, types, and documentation an agent or human needs. The name begins with `h` (free Haskell flavour), is five letters, and has no significant Google collisions in software.

## 3. Goals and non-goals

### 3.1 In scope (v0.1 MVP)

- **Project resolution** from `dist-newstyle/cache/plan.json`, the cabal store, and nix-store paths; `source-repository-package` handled shallowly (sources from disk, Haddock generated on demand).
- **Subcommands**: `search`, `package`, `module`, `symbol`, `source`, `versions`, `deps`, `whatprovides`, `doctor`, `server`, `mcp`.
- **Output**: compressed structured JSON by default, including for terminals; `--human` opts into pretty ANSI text; `--pretty-json` indents JSON; `--full` opts into the full field set; `--select` projects fields.
- **Cross-recursion principle**: every JSON response includes `actions` and `related` fields whose values are `hypha` invocation strings — never Hackage URLs.
- **Hoogle** integrated as a library; per-project DB by default; `--global` widens to a global (Stackage-built) DB.
- **Hackage** access only via the JSON API; aggressive ETag/Last-Modified cache; gentle rate limits; `--offline` enforces no network.
- **Server mode**: lazy on-demand Haddock generation per package; `--prebuild` flag; custom UI shell with fzf/telescope-style live search, keyboard navigation, package/module tree, cross-package link rewriting.
- **MCP server**: a `hypha-mcp` stdio binary exposing one tool, `hypha.exec`, that shells out to the `hypha` CLI (Pattern B per `cli-printing-press` doctrine).
- **`BuildEnv` abstraction** with Cabal and Nix implementations; Stack reserved for later.
- **`doctor`** subcommand that diagnoses the environment.
- **Typed exit codes** (0, 2, 3, 4, 5, 7) shared across CLI and MCP envelopes.

### 3.2 Out of scope (post-MVP roadmap)

- Stack `BuildEnv` (deferred behind the same record interface).
- `diff <pkg> v1 v2` (API diffs between versions).
- `compat <pkg> --ghc X.Y.Z` checks.
- Authenticated / private Hackage mirrors.
- A `hypha install-mcp <host>` auto-configurator that edits client config files.
- "Rung-5" behavioural analytics: deprecated symbols in plan, packages without recent activity, etc.
- Full-fat custom Haddock renderer — the server embeds Haddock-generated HTML inside our shell rather than re-rendering.
- HTML scraping of hackage.haskell.org. JSON API only.
- Writing to the user's project. `hypha` never edits `cabal.project`, never installs, never fires builds outside its own cache.

### 3.3 Explicit non-goal

Replacing `haskell-docs-cli`. That tool is an interactive human-only REPL; `hypha` is an agent-first one-shot CLI plus optional doc browser.

## 4. Design principles

1. **JSON-first.** Default output is compressed JSON, on every command, whether stdout is a TTY or a pipe. Humans opt in with `--human`. Agents pay no flag tax.
2. **Compact by default.** The default field set is the "high-gravity" subset (60–80% token reduction). `--full` opts into every field. The compact field set is a strict subset of the full field set; this is verified by a property test.
3. **`hypha` is the agent's only doorway.** All cross-references in JSON output are `hypha`-invocation strings. JSON output never emits Hackage URLs. Human-facing renderings may; the server obviously does.
4. **Project-aware.** Queries are scoped to the active build plan. Widening outside the plan requires an explicit `--any` (and is loudly annotated in output).
5. **An LLM doesn't need fancy Haddock HTML.** Agents want source code (which contains the inline Haddock comments) and structured metadata. Humans want visuals. The two-path architecture (`--human` / `server` for visuals; JSON for everything else) follows from this.
6. **Stand on shoulders.** Prefer existing ecosystem packages over custom code: `cabal-plan`, `cabal-install-parsers`, `Cabal-syntax`, `hoogle`, `haddock-library`, `mcp` (DPella), `falsify`, `contra-tracer`.
7. **WT production-quality Haskell.** Strong types over boolean blindness, polymorphic abstractions, records-of-functions for effects (not tagless final, not mtl effect classes, not effect libraries), manual `!` bangs on record fields (not `StrictData`), no `error` / `undefined` in production code, property-based testing with `falsify`.

## 5. Architecture

### 5.1 Top-level shape

One cabal package, one library, two executables:

```
library         hypha
executable      hypha       (CLI; depends on hypha library)
executable      hypha-mcp   (MCP stdio shim; depends on hypha library; minimal)
```

### 5.2 Effect carrier

`ReaderT Env IO`, no effect library. `Env` is a record holding records-of-functions and configuration:

```haskell
data Env = Env
  { envBuildEnv  :: !(BuildEnv IO)
  , envHackage   :: !(HackageClient IO)
  , envHoogle    :: !(Hoogle IO)
  , envCache     :: !(Cache IO)
  , envTracer    :: !(Tracer IO LogEvent)
  , envFlags     :: !GlobalFlags
  , envPlan      :: !BuildPlan
  }

newtype App a = App { runApp :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)
```

### 5.3 Records of functions (polymorphic over `m`)

```haskell
data BuildEnv m = BuildEnv
  { discoverInstalledPackages :: m (Set PackageId)
  , locatePackageSource       :: PackageId -> m (Maybe FilePath)
  , locateHaddockHtml         :: PackageId -> m (Maybe FilePath)
  , ghcVersion                :: m Version
  }

data HackageClient m = HackageClient
  { fetchPackageJson :: PackageName -> m PackageJson
  , fetchVersions    :: PackageName -> m [Version]
  }

data Hoogle m = Hoogle
  { searchHoogle  :: HoogleQuery -> m [HoogleHit]
  , ensureFreshDb :: BuildPlan   -> m ()
  }

data Cache m = Cache { ... }
```

Production smart constructors return `BuildEnv IO` etc. Tests construct `BuildEnv Identity` or `BuildEnv (State MockState)` from pure fixtures. Decorators (retry, log, throttle) are plain `BuildEnv m -> BuildEnv m` transformers.

### 5.4 Module tree (library)

```
Hypha.Prelude
Hypha.Types.PackageId           PackageName, Version, PackageId (newtypes)
Hypha.Types.SymbolPath          pkg[/Mod[.Submod]][/sym] ADT + parser + pretty
Hypha.Types.BuildPlan           shadow of plan.json
Hypha.Types.Doc                 re-exports haddock-library DocH

Hypha.BuildEnv.Type             record definition
Hypha.BuildEnv.Cabal            mkCabalBuildEnv
Hypha.BuildEnv.Nix              mkNixBuildEnv
Hypha.BuildEnv.Compose          composeBuildEnv :: BuildEnv IO -> BuildEnv IO -> BuildEnv IO

Hypha.Project.Discovery         walk-up + --project-dir
Hypha.Project.Plan              parse plan.json (cabal-plan)
Hypha.Project.Overrides         --package-override

Hypha.Hackage.Type              HackageClient record
Hypha.Hackage.Api               mkHackageClient (IO impl)
Hypha.Hackage.Cache             ETag/Last-Modified store under XDG_CACHE_HOME
Hypha.Hackage.Types             API response types

Hypha.Hoogle.Type
Hypha.Hoogle.Database           per-project DB build / freshness via plan-hash
Hypha.Hoogle.Query

Hypha.Haddock.Parse             haddock-library AST helpers
Hypha.Haddock.Interface         .haddock file reading (if needed)
Hypha.Haddock.Generate          on-demand cabal/haddock build pipeline

Hypha.Source.Locate
Hypha.Source.Extract

Hypha.Output.Outcome            Outcome a w/ actions + related
Hypha.Output.Json               compact and full ToJSON variants; envelope
Hypha.Output.Actions            builder for next_actions / related
Hypha.Output.Human              DocH -> ANSI; signature syntax highlight (skylighting)

Hypha.Command.Search
Hypha.Command.Package
Hypha.Command.Module
Hypha.Command.Symbol
Hypha.Command.Source
Hypha.Command.Versions
Hypha.Command.Deps
Hypha.Command.WhatProvides
Hypha.Command.Doctor
Hypha.Command.Server

Hypha.Server.App                WAI app, routes
Hypha.Server.Ui.Layout          lucid2 component functions
Hypha.Server.Ui.Search
Hypha.Server.Ui.Tree
Hypha.Server.Ui.Doc
Hypha.Server.Ui.Source
Hypha.Server.Haddock.Rewrite    pure Html -> Html link rewriter
Hypha.Server.Haddock.Slots      Map PackageId (MVar BuildState)

Hypha.Mcp.Server                JSON-RPC stdio handler (built on `mcp` library)

Hypha.Cli.Parser                optparse-applicative; per-command Args ADT
Hypha.Cli.Run                   dispatcher

Hypha.Exit                      typed ExitCode newtype
Hypha.Error                     HyphaError sum; mapping to ExitCode
Hypha.Logging                   LogEvent ADT + tracer interpreters
```

## 6. Project resolution

```
1. Discover ProjectRoot.
   --project-dir flag wins; otherwise walk up from CWD for the first directory
   containing `cabal.project` or any `*.cabal` file.

2. Detect BuildEnv backend(s).
   - dist-newstyle/cache/plan.json present  -> CabalBuildEnv primary.
   - shell.nix / flake.nix / default.nix present -> NixBuildEnv composed as fallback.
   - .stack-work present and nothing else    -> exit 7 with a "Stack not supported in MVP" hint.

3. Parse BuildPlan via `cabal-plan`.
   If plan.json is missing, refuse non-trivial queries: most commands need pinned
   versions to be meaningful. The `doctor` command will diagnose this and suggest
   `cabal build --dry-run` to materialise the plan.

4. Apply overrides in memory only.
   --package-override pkg=ver  (repeatable) replaces plan entries.
   Applied overrides are surfaced in the top-level `overrides` field of every
   output envelope so JSON consumers can see when results were not from the
   original plan.

5. Construct Env.
   Build the BuildEnv IO record (Cabal, optionally composed with Nix), the
   HackageClient IO, Hoogle IO, Cache IO, Tracer, and pass GlobalFlags through.
```

`--ghc-override` is **not** offered: plan.json already pins the GHC, and the
rare case of wanting a different toolchain is better solved by editing
`cabal.project`.

## 7. Data sources and outside-plan policy

For any "look up package/module/symbol info" query:

```
1. Plan check.
   - in plan                     -> proceed against pinned version.
   - not in plan + --offline     -> exit 3 ("not in plan and --offline forbids widening").
   - not in plan + no --any      -> exit 3 with hint "use --any to widen".
   - not in plan + --any         -> widen to Hackage latest; tag output outside_plan: true.

2. Source lookup.
   BuildEnv.locatePackageSource pkg-ver
   - present  -> parse on demand (haddock-library on .hs files).
   - absent   -> exit 7 with hint "run `cabal build` first; hypha does not fire builds".

3. Haddock lookup (only when needed: --human / server / when JSON output requires
   rendered prose, which it generally does not since the mantra is "JSON returns
   raw markup, agents render or not").
   BuildEnv.locateHaddockHtml pkg-ver
   - present  -> use it directly.
   - absent   -> for --human / server, invoke `cabal haddock` lazily and cache in
     ~/.cache/hypha/haddock/<pkg>-<ver>/. For JSON paths, skip — source comments
     suffice.

4. Hackage JSON.
   For metadata that is genuinely network-only (latest version, deprecation,
   maintainer list). Always cache-first.
```

## 8. Caching

Two cache tiers.

### 8.1 Per-user, cross-project

`${XDG_CACHE_HOME:-~/.cache}/hypha/`:

```
hackage/                       HTTP cache; key = SHA-256 of URL; body + headers stored
haddock/<pkg>-<ver>/           lazy-built docs
hoogle-global/default.hoo      if --global ever used
```

### 8.2 Per-project, ephemeral

`<project>/.hypha/`:

```
hoogle.hoo                     per-project Hoogle DB
plan-hash                      hash of plan.json; invalidates hoogle.hoo
state.json                     misc bookkeeping
```

README will recommend adding `.hypha/` to `.gitignore`. `hypha` does not edit the user's `.gitignore` itself.

### 8.3 HTTP cache semantics

- Immutable resources (specific package version metadata) — TTL infinite, but ETag still respected.
- Mutable resources (package latest meta) — short TTL (15 min default) with ETag / Last-Modified conditional revalidation.
- `--offline` => cache-only; cache miss => typed exit 4.
- **User-Agent**: every request sends `hypha/<version> (+https://github.com/well-typed/hypha; contact: info@well-typed.com)`.
- **Rate limit**: 1 RPS soft, 5 RPS burst, configurable internally (not via CLI); back-off on HTTP 429/503.

### 8.4 Cache record

```haskell
data CachedResponse = CachedResponse
  { crEtag         :: !(Maybe ByteString)
  , crLastModified :: !(Maybe UTCTime)
  , crStoredAt     :: !UTCTime
  , crBody         :: !ByteString
  , crKind         :: !CacheKind
  }

data CacheKind = Immutable | TtlMutable !NominalDiffTime
```

## 9. Identifier syntax

One canonical path format used everywhere — CLI arguments, JSON `fetch` strings, MCP `args`:

```
<pkg>[@<version>][/<Module.Path>][/<symbol>]
```

The `@version` qualifier attaches to the package segment so a single token unambiguously identifies a version-pinned root. Examples:

- `async`
- `async/Control.Concurrent.Async`
- `async/Control.Concurrent.Async/concurrently`
- `async@2.2.5/Control.Concurrent.Async/concurrently`

Defined in `Hypha.Types.SymbolPath`; parser + pretty are inverse; verified by a `falsify` roundtrip property.

## 10. Global flags

```
--project-dir DIR              override project root
--package-override PKG=VER     replace plan entry (repeatable)
--any                          allow widening outside plan
--global                       widen Hoogle to global stackage DB
--offline                      no network, fail closed
--human                        pretty ANSI text instead of JSON
--pretty-json                  indent JSON output
--full                         include all fields (default is compact)
--select f1,f2,...             post-filter JSON output to listed fields
--quiet | --verbose            tracer level
```

Explicitly omitted: `--no-cache`, `--user-agent`, `--ghc-override`.

## 11. Subcommands

| Cmd             | Args                                  | Purpose                                                                  |
|-----------------|---------------------------------------|--------------------------------------------------------------------------|
| `search`        | `QUERY [+pkg ...]`                    | Hoogle hits scoped to plan; `--global` widens                            |
| `package`       | `<pkg>[@ver]`                         | Metadata: pinned ver, latest, deprecation, license, maintainers, repo    |
| `module`        | `<pkg>/<Mod>`                         | Exported symbols list with signatures                                    |
| `symbol`        | `<pkg>/<Mod>/<sym>`                   | Signature + raw Haddock + source coords + related                        |
| `source`        | `<pkg>/<Mod>/<sym>` or `<pkg>/<Mod>`  | Source slice                                                             |
| `versions`      | `<pkg>`                               | Version history, revisions, plan-pinned marker                           |
| `deps`          | `<pkg> [--reverse] [--depth N]`       | Forward / reverse deps within the plan                                   |
| `whatprovides`  | `<symbol>`                            | Packages exporting that symbol                                           |
| `doctor`        | —                                     | Environment health: ghc, haddock, hoogle, store, plan.json               |
| `server`        | `[--port N] [--prebuild] [--bind ...]`| Start HTTP doc-browser                                                   |
| `mcp`           | —                                     | Run MCP stdio shim (alias for `hypha-mcp` exe entry)                     |

Per-command flag specs are kept under each `Hypha.Command.*` module, defined as a single `Args` ADT that the optparse-applicative parser and (later, if Pattern B doesn't suffice) the MCP tool schema both derive from.

## 12. Output schema

Every command emits exactly one JSON document on stdout.

### 12.1 Success envelope

```json
{
  "schema": "hypha/v0",
  "command": "symbol",
  "ok": true,
  "outside_plan": false,
  "overrides": [],
  "result": { ... command-specific ... },
  "actions": {
    "<name>": "hypha <args>"
  },
  "related": [
    { "label": "...", "fetch": "hypha ..." }
  ]
}
```

### 12.2 Error envelope

```json
{
  "schema": "hypha/v0",
  "command": "symbol",
  "ok": false,
  "error": {
    "code": "NOT_IN_PLAN",
    "message": "package 'foo' not in build plan",
    "exit_code": 3
  },
  "actions": {
    "retry_with_any": "hypha symbol foo/Bar/baz --any"
  }
}
```

The envelope is emitted regardless of exit code. The schema string `hypha/v0` lets agents key on it; we bump on breaking changes.

### 12.3 Field-set policy

Each command defines two field sets — `compact` and `full` — in `Hypha.Output.Json`. `compact` is a strict subset of `full`; a property test enforces this. `--full` opts into `full`; `--select` post-filters either set.

### 12.4 Worked example — `symbol`

`hypha symbol async/Control.Concurrent.Async/concurrently` (compact, default):

```json
{
  "schema":"hypha/v0","command":"symbol","ok":true,"outside_plan":false,"overrides":[],
  "result":{
    "kind":"function","name":"concurrently","package":"async","version":"2.2.5",
    "module":"Control.Concurrent.Async",
    "signature":"concurrently :: IO a -> IO b -> IO (a, b)",
    "haddock_raw":"Run two @IO@ actions concurrently...",
    "source":{"path":"/.../Control/Concurrent/Async.hs","line":234}
  },
  "actions":{
    "view_source":"hypha source async/Control.Concurrent.Async/concurrently",
    "module_index":"hypha module async/Control.Concurrent.Async",
    "package_info":"hypha package async",
    "reverse_deps":"hypha deps async --reverse",
    "version_history":"hypha versions async"
  },
  "related":[
    {"label":"race","fetch":"hypha symbol async/Control.Concurrent.Async/race"},
    {"label":"withAsync","fetch":"hypha symbol async/Control.Concurrent.Async/withAsync"}
  ]
}
```

## 13. Typed exit codes

| Code | Meaning                                                                          |
|------|----------------------------------------------------------------------------------|
| 0    | OK                                                                               |
| 2    | User error — bad CLI args, malformed path, conflicting flags                     |
| 3    | Not found — symbol/pkg not in plan (and no `--any`), or absent from Hackage      |
| 4    | Network error — including `--offline` with cache miss                            |
| 5    | Cache / parse / on-disk corruption                                               |
| 7    | Environment error — no plan.json, missing ghc/haddock, store unreachable, Stack  |

`Hypha.Exit` is a smart-ctor newtype. `Hypha.Error` is a sum type whose mapping to `ExitCode` is total — verified by a property test that enumerates constructors via `Generic`.

## 14. `--human` rendering pipeline

When `--human` is set, `Outcome` is rendered to ANSI text via `Hypha.Output.Human`:

1. Parse `haddock_raw` field with `haddock-library` → `DocH`.
2. Pretty-print `DocH` directly to ANSI via `prettyprinter` + `prettyprinter-ansi-terminal`.
3. Highlight the signature line using `skylighting`'s Haskell lexer (TTY ANSI theme).
4. Lay out: name + kind on first line, signature on second, prose paragraph, "See also" list, source coords at bottom.

We render `DocH` → ANSI **directly**, not via Markdown. Going through Markdown is a lossy intermediate hop.

`--human` and JSON output go through the same `Outcome` value; only the renderer differs.

## 15. MCP

### 15.1 Pattern

Pattern B per the `cli-printing-press` doctrine: one MCP tool that shells out to the CLI.

- Tool name: `hypha.exec`.
- Argument schema: `{ "args": string[] }`.
- Handler: spawn the `hypha` binary with those args via `typed-process`, capture stdout, return as the tool's content.
- The JSON envelope is emitted on stdout regardless of CLI exit code, so the MCP tool returns the envelope as content. Exit code is surfaced inside the envelope (`error.exit_code`) and as MCP tool metadata.

### 15.2 Implementation

`Hypha.Mcp.Server` uses the `mcp` package (DPella, v0.3.x) via its `MCP.Server.Stdio` transport. Total shim size budget: ~150 LOC.

The MCP tool's description (sent to the model) documents:

- The list of CLI subcommands with one-line descriptions each.
- The identifier syntax (`pkg[/Mod[.Submod]][/sym][@ver]`).
- The relevant global flags (explicitly omitting `--no-cache` and `--user-agent` from the doc; they are not flags `hypha` even accepts).
- A pointer to the output schema and the cross-recursion mantra (results contain `actions` and `related` containing further `hypha …` strings — feed those back as `hypha.exec args`).

### 15.3 Host integration

We ship `hypha-mcp` as a binary installable to `PATH`. README documents copy-paste snippets per host:

- **Claude Code** — `.mcp.json` at project root or `~/.claude.json` `mcpServers`:
  ```json
  { "mcpServers": { "hypha": { "command": "hypha-mcp", "args": [] } } }
  ```
- **opencode** — `~/.config/opencode/opencode.json` `mcp` block, same shape.
- **Generic MCP client** — any client speaking MCP/stdio can spawn `hypha-mcp`.

A future `hypha install-mcp <host>` subcommand may idempotently edit these configs; out of MVP scope.

## 16. Server mode

### 16.1 Routes

```
GET  /                              landing
GET  /search?q=...                  HTMX fragment (live results)
GET  /pkg/<pkg>                     package view
GET  /pkg/<pkg>/<Mod>               module view
GET  /pkg/<pkg>/<Mod>/<sym>         symbol view
GET  /haddock/<pkg>-<ver>/<path...> rewritten Haddock HTML
GET  /source/<pkg>/<Mod>            syntax-highlighted source
GET  /api/<cmd>?...                 same JSON envelope as CLI; in-process cache reuse
GET  /assets/<file>                 embedded CSS / JS / icons
GET  /healthz                       liveness probe
```

Built on `warp` + `wai` + `servant-server`. Views written with `lucid2`.

### 16.2 UI architecture

- Server-rendered HTML; no SPA.
- HTMX vendored locally for live search and pane swaps.
- ~50 LOC of vanilla JS for keybindings only, in a named file, organised by handler.
- CSS split by concern: `base.css`, `layout.css`, `components/*.css`. No Tailwind, no build step.
- Dark / light via `prefers-color-scheme`.
- Layout: top search bar always visible, left pane collapsible package/module tree, main pane current view, breadcrumb above main.

### 16.3 Keymap

| Key                                | Action                                |
|------------------------------------|---------------------------------------|
| `s` / `/` / `Ctrl-K`               | focus search                          |
| `↑` / `↓` / `j` / `k`              | navigate result list                  |
| `Enter`                            | open selected                         |
| `Esc`                              | dismiss overlay / unfocus search      |
| `←` / `→` / `h` / `l`              | breadcrumb back / forward             |
| `gp`                               | packages index                        |
| `gh`                               | home                                  |
| `?`                                | help overlay                          |

`s` is for Hackage muscle-memory parity.

### 16.4 Haddock build pipeline

Per-package, lazy:

1. Request `/haddock/<pkg>-<ver>/...`.
2. Check `~/.cache/hypha/haddock/<pkg>-<ver>/index.html`.
3. If missing, check the cabal store's `share/doc/...` — if present, symlink-mirror into the cache.
4. Otherwise spawn a build via `typed-process`: `cabal haddock --package <pkg>-<ver> --builddir <tmp>`, falling back to direct `haddock` invocation on installed source dirs when needed; for `source-repository-package` checkouts, run `haddock` against the dist-newstyle source dir.
5. HTML rewrite via `tagsoup` rewrites all relative cross-package `href`s to `/haddock/<other-pkg>-<ver>/…` server routes. Anchor links are preserved. The rewrite is a pure `Html -> Html` pass with golden tests.
6. The first HTMX request that triggers a build receives a "building docs…" placeholder; subsequent polls swap in the result on completion.

Concurrency model (per-package locks, not a global mutex):

```haskell
type BuildSlots = Map PackageId (MVar BuildState)

data BuildState
  = NotStarted
  | Building !(Async FilePath)
  | Done     !FilePath
  | Failed   !SomeException
```

The outer `Map` is built once at server startup from the BuildPlan and is immutable afterwards. Each per-package `MVar` is an independent lock — concurrent requests for *different* packages never contend.

`--prebuild` at startup: enumerate all plan packages, spawn a worker pool of size `--prebuild-jobs N` (default `getNumCapabilities`), build them all up front with tracer progress lines.

### 16.5 Search

- Backed by the per-project Hoogle DB.
- HTMX-driven: `hx-get="/search"` `hx-trigger="keyup changed delay:120ms"` `hx-target="#results"`.
- Three result lanes interleaved by score:
  - Hoogle hits (signature / name).
  - Package-name fuzzy hits via `text-metrics`.
  - Module-name fuzzy hits.
- Rows reachable via cursor (`↑` / `↓` / `j` / `k`); `Enter` navigates.
- `Ctrl-K` overlay variant summonable from any view.

### 16.6 Security / binding

- Default bind: `127.0.0.1:4287`.
- `--bind HOST:PORT` permitted, but if HOST is not one of `127.0.0.1` / `::1` / `localhost` the server refuses to start (exit 2).
- No `--allow-public` escape hatch in MVP. If users need it post-MVP it goes behind a long flag with an interactive confirmation.
- No auth in MVP. Read-only server: no write endpoints, builds confined to the cache dir.
- CSP: `default-src 'self'; img-src 'self' data:; style-src 'self' 'unsafe-inline'`. `'unsafe-inline'` is the minimum required for HTMX's inline `hx-on` attributes and is harmless given no external network.

### 16.7 Asset packaging

UI files in `ui/` are embedded into the executable at compile time via `file-embed`. The result is a single static binary serving zero-network out of the box.

## 17. Code quality conventions

- Manual `!` bangs on record fields; do not enable `StrictData`. Lazy fields are explicit and require a one-line justification comment.
- No `error` / `undefined` in production code paths. `HyphaError` is the boundary type; `throwIO` for `IO`-effecting failures; total functions everywhere else.
- Strong types over boolean blindness: newtypes for `PackageName`, `Version`, `PackageId`, `ProjectRoot`, `PortNumber`, `EtagBytes`, etc.
- Prefer polymorphic abstractions over concrete types when an interface works.
- Allergic to duplication: extract shared abstractions early.
- Frontend assets follow the same hygiene: `lucid2` component functions for repeated HTML, modular CSS, named JS functions.
- Tests: property-based via `falsify` (with `Test.Tasty.Falsify`) where invariants exist; golden tests via `tasty-golden` for JSON and ANSI output; HUnit only sparingly for specific parser cases.

## 18. Dependencies

**Library — core:**
`base`, `bytestring`, `text`, `containers`, `unordered-containers`, `time`, `filepath`, `directory`, `mtl`, `transformers`, `stm`, `async`.

**Library — domain:**
`cabal-plan`, `cabal-install-parsers`, `Cabal-syntax`, `hoogle`, `haddock-library`.

**Library — IO / formats:**
`aeson`, `aeson-pretty`, `http-client`, `http-client-tls`, `tls`, `typed-process`, `tagsoup`, `text-metrics`, `skylighting`, `contra-tracer`, `optparse-applicative`, `prettyprinter`, `prettyprinter-ansi-terminal`.

**Library — server:**
`warp`, `wai`, `wai-extra`, `servant-server`, `lucid2`, `file-embed`.

**Library — MCP:**
`mcp` (DPella).

**Tests:**
`tasty`, `falsify`, `tasty-falsify`, `tasty-quickcheck`, `tasty-golden`, `tasty-hunit`, `tasty-expected-failure`.

`cabal.project.freeze` is committed. CI matrix: GHC 9.6, 9.8, 9.10. Final versions are pinned during the dependency-audit step of Phase 1 of delivery; this list is the design-level set, not the build-level set.

## 19. Testing posture

| Tier            | Tooling                              | Scope                                                                                  |
|-----------------|--------------------------------------|----------------------------------------------------------------------------------------|
| Property        | `falsify` via `Test.Tasty.Falsify`   | `SymbolPath` parse/pretty roundtrip; `compact` ⊆ `full` field sets; `HyphaError → ExitCode` totality; cache write/read fidelity; Haddock HTML rewrite idempotence. |
| Unit            | `tasty-hunit`                        | Specific edge cases in plan.json / cabal.project parsing.                              |
| Golden          | `tasty-golden`                       | Compact JSON, full JSON, `--human` ANSI per representative command. Regenerable via `--accept`. |
| Integration     | tasty `--integration` flag           | Run `hypha` against `test/fixtures/sample-project/`; requires a real GHC; opt-in.      |

No network calls in unit/property/golden tiers. Integration tier is gated by `HYPHA_INTEGRATION=1` in CI.

## 20. Repo layout

```
hackage-ai-cli/                       (repo root)
├── README.md
├── CHANGELOG.md
├── LICENSE
├── cabal.project
├── cabal.project.freeze
├── hypha.cabal
├── flake.nix
├── docs/superpowers/specs/
│   └── 2026-05-18-hypha-design.md   (this document)
├── src/Hypha/...                     (library)
├── app/hypha/Main.hs
├── app/hypha-mcp/Main.hs
├── ui/
│   ├── css/base.css
│   ├── css/layout.css
│   ├── css/components/*.css
│   ├── js/keybindings.js
│   ├── js/htmx.min.js               (vendored)
│   └── icons/                       (inline SVG)
├── test/
│   ├── Unit/
│   ├── Property/
│   ├── Golden/
│   ├── Integration/
│   └── fixtures/sample-project/
└── .github/workflows/ci.yml
```

## 21. Phased delivery (v0.1 MVP)

Each item is one PR-sized chunk; build order recommended.

1. Project skeleton: cabal manifest, CI, devshell, license, README stub. `doctor` printing `hypha 0.0`.
2. Types: `PackageId`, `SymbolPath` + parser + property tests.
3. `BuildPlan` ingestion via `cabal-plan`; `Project.Discovery` + `--project-dir`; `Project.Overrides`.
4. `BuildEnv IO` interface + `Cabal` implementation. Mock impl for tests.
5. `BuildEnv` `Nix` implementation + composition.
6. `HackageClient IO` + ETag/Last-Modified cache.
7. `Hoogle` per-project DB builder + freshness via plan-hash; `--global` fallback.
8. `Output.Outcome` + `Output.Actions` + `Output.Json` (compact + full); `--select`.
9. `Command.Search` end-to-end; CLI dispatcher; typed exit codes; first golden tests.
10. `Command.Package`, `Versions`, `Module`, `Symbol`, `Source`, `Deps`, `WhatProvides`.
11. `--human` renderer: DocH → ANSI; skylighting for signatures.
12. `Doctor`.
13. `Haddock.Generate` lazy + on-demand build pipeline; cache layout.
14. `Server` (servant routes, lucid2 views, HTMX search, keymap); `--prebuild` worker pool; HTML rewrite.
15. `Hypha.Mcp.Server` (Pattern B `hypha.exec` shells to CLI); `hypha-mcp` shim binary; README MCP-host snippets.
16. README polish: etymology, install, MCP configs, mantra.
17. Release v0.1.

## 22. Open risks

- **`mcp` library maturity.** Pick DPella's `mcp`. If it proves unstable, fallback is hand-rolling JSON-RPC-over-stdio (~300 LOC). Pattern-B keeps our coupling minimal — we touch the library at exactly one boundary.
- **`source-repository-package` corner cases.** Source dir layout in `dist-newstyle/src/` has shifted between cabal-install versions. Mitigation: per-cabal-install-minor fixture suite.
- **Haddock cross-package link rewriting.** Haddock HTML is stable in practice but not specified. Mitigation: write the rewrite as a pure `Html -> Html` pass; golden tests catch regressions.
- **Hoogle library API drift.** The programmatic API has historically been less polished than the `hoogle` CLI. Mitigation: thin facade in `Hypha.Hoogle.Query` so that a future shell-out is a non-breaking internal refactor.

## 23. Mantra (for the README, the team, and future contributors)

> An LLM doesn't need or care about fancy Haddock HTML pages — it cares about the source code, which also contains the comments (the documentation). That is what an LLM needs in order to learn knowledge of a project. Humans need visuals.
