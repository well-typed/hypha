# hypha — Plan C: MCP shim + README polish + v0.1 release (phases 15–17)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship the `hypha-mcp` executable (Pattern B per `cli-printing-press` doctrine — one MCP tool, `hypha.exec`, that shells out to the `hypha` CLI), polish the README with the etymology, install instructions, mantra, and per-host MCP snippets, and cut version v0.1.0.

**Architecture:** A thin `MCP.Server.Stdio` shim (~150 LOC) that registers one tool whose handler invokes `typed-process` to run the `hypha` binary with the provided argv and returns the stdout JSON envelope verbatim. Adds CSP header to the server, the `gp`/`gh`/`?` keymap stubs from Plan B, and a `--port`-conflict-friendly bind error. README documents how to install for Claude Code, opencode, and any MCP-capable client.

**Tech Stack:** `mcp` (DPella, v0.3.x), `typed-process`, `aeson`. Existing Plan A/B deps.

**Spec:** `docs/superpowers/specs/2026-05-18-hypha-design.md` §15 (MCP), §16.3 (keymap polish), §16.6 (CSP). Prerequisite: Plans A and B merged.

**Reading order:** Tasks 1 → 5. One PR per task. Final task tags v0.1.0.

---

## File structure (added by this plan)

```
src/Hypha/Mcp/Server.hs
app/hypha-mcp/Main.hs

test/Unit/Mcp.hs
test/fixtures/mcp/hello-search.json     (sample MCP request input)
test/fixtures/mcp/hello-search.out.json (golden response)

README.md                               (rewritten / extended)
CHANGELOG.md                            (created)
```

## Conventions

Same as Plans A and B. Manual `!` bangs on record fields, no `StrictData`, no `error`/`undefined`, polymorphic where natural, records-of-functions for effects, `falsify` + `tasty-golden` testing.

---

## Task 1: `Hypha.Mcp.Server` — Pattern B shim

**Files:**
- Create: `src/Hypha/Mcp/Server.hs`
- Create: `test/Unit/Mcp.hs`
- Modify: `hypha.cabal`

- [ ] **Step 1: Extend `hypha.cabal`**

Extend `library` `build-depends`:

```cabal
    , mcp >= 0.3 && < 0.4
```

Extend `library` `exposed-modules`:

```cabal
    Hypha.Mcp.Server
```

Extend `test-suite` `other-modules`:

```cabal
    Unit.Mcp
```

- [ ] **Step 2: Write `Hypha.Mcp.Server`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Mcp.Server
  ( runMcpStdio
  , ExecArgs (..)
  , execHypha
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=), (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as Text
import Data.Text (Text)
import qualified System.Process.Typed as TP
import System.Environment (lookupEnv)
import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))

import qualified MCP.Server.Stdio as Stdio
import qualified MCP.Server.Common as MCP

newtype ExecArgs = ExecArgs { eaArgs :: [Text] }
  deriving stock (Show, Eq)

instance Aeson.FromJSON ExecArgs where
  parseJSON = Aeson.withObject "ExecArgs" $ \o -> ExecArgs <$> o .: "args"

-- | The single MCP tool we expose.
toolDescription :: Text
toolDescription = Text.unlines
  [ "Probe Hackage / Hoogle / cabal build plans by invoking the `hypha` CLI."
  , ""
  , "Argument schema: { \"args\": string[] }"
  , ""
  , "Subcommands:"
  , "  search QUERY                 — Hoogle search scoped to the current build plan"
  , "  package PKG                  — pinned-version metadata"
  , "  module  PKG/MOD              — module exports"
  , "  symbol  PKG/MOD/SYM          — signature + Haddock + source coords"
  , "  source  PKG/MOD[/SYM]        — source slice"
  , "  versions PKG                 — version history"
  , "  deps     PKG [--reverse]     — forward or reverse plan deps"
  , "  whatprovides SYM             — which packages export the symbol"
  , "  doctor                       — diagnose environment"
  , ""
  , "Identifier syntax: pkg[@ver][/Mod[.Path]][/symbol]"
  , ""
  , "Default output is compact JSON. The response envelope contains an"
  , "`actions` map and a `related` list whose values are further `hypha ...`"
  , "invocations — feed them back as { \"args\": [...] } to navigate without"
  , "ever fetching Hackage URLs."
  ]

runMcpStdio :: IO ()
runMcpStdio = do
  hSetBuffering stdout LineBuffering
  Stdio.runServer Stdio.ServerSpec
    { Stdio.serverName        = "hypha"
    , Stdio.serverVersion     = "0.1.0"
    , Stdio.serverDescription = "hypha — agent-first probe for Hackage and your cabal plan"
    , Stdio.serverTools       =
        [ MCP.Tool
            { MCP.toolName        = "hypha.exec"
            , MCP.toolDescription = toolDescription
            , MCP.toolHandler     = handle
            }
        ]
    }
  where
    handle :: Value -> IO MCP.ToolResult
    handle v =
      case Aeson.fromJSON v :: Aeson.Result ExecArgs of
        Aeson.Error e -> pure (MCP.ToolError (Text.pack e))
        Aeson.Success (ExecArgs xs) -> do
          mBin <- lookupEnv "HYPHA_BIN"
          let bin = maybe "hypha" id mBin
          (rc, out, err) <- execHypha bin xs
          pure (MCP.ToolContent
                  (Text.decodeUtf8 (BL.toStrict out))
                  (object [ "exit_code" .= rc
                          , "stderr"    .= Text.decodeUtf8 (BL.toStrict err)
                          ]))

-- | Spawn the @hypha@ binary, return @(exit-code, stdout, stderr)@.
execHypha :: FilePath -> [Text] -> IO (Int, BL.ByteString, BL.ByteString)
execHypha bin args = do
  let proc' = TP.setStdin TP.nullStream
            $ TP.proc bin (map Text.unpack args)
  (ec, sout, serr) <- TP.readProcess proc'
  pure (case ec of
          TP.ExitSuccess   -> 0
          TP.ExitFailure c -> c
       , sout, serr)
```

> Notes on `mcp` library shape: the exact module path for the stdio server (`MCP.Server.Stdio`), the tool record (`MCP`-prefixed types), and the result type may differ slightly between minor releases. The intent of this module is fixed: register one tool, decode `{"args":...}`, shell out to `hypha`, return the stdout envelope verbatim and stuff exit code + stderr into metadata. Adjust constructor names as needed to compile.

- [ ] **Step 3: Write unit test for `execHypha` against a deterministic fake binary**

Create `test/Unit/Mcp.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Unit.Mcp (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as Text

import Hypha.Mcp.Server (execHypha)

tests :: TestTree
tests = testGroup "Mcp"
  [ testCase "execHypha echoes via /bin/echo" $ do
      (rc, sout, _serr) <- execHypha "/bin/echo" ["hello"]
      rc @?= 0
      BL.toStrict sout @?= "hello\n"

  , testCase "execHypha propagates non-zero exit" $ do
      (rc, _, _) <- execHypha "/bin/sh" ["-c", "exit 3"]
      rc @?= 3
  ]
```

Register in `test/Main.hs`:

```haskell
import qualified Unit.Mcp
-- ...
  , Unit.Mcp.tests
```

- [ ] **Step 4: Run + commit**

Run: `cabal test`
Expected: pass (skip on Windows; the `/bin/echo` test is POSIX-only — fence with `#if !defined(mingw32_HOST_OS)` if needed).

```bash
git add hypha.cabal src/Hypha/Mcp/Server.hs test/Unit/Mcp.hs test/Main.hs
git commit -m "feat(mcp): Pattern-B stdio shim — single hypha.exec tool that shells out to CLI"
```

---

## Task 2: `hypha-mcp` executable

**Files:**
- Create: `app/hypha-mcp/Main.hs`
- Modify: `hypha.cabal`

- [ ] **Step 1: Add the executable stanza to `hypha.cabal`**

```cabal
executable hypha-mcp
  import:           warnings, deps
  hs-source-dirs:   app/hypha-mcp
  main-is:          Main.hs
  build-depends:    hypha
```

- [ ] **Step 2: Write `app/hypha-mcp/Main.hs`**

```haskell
module Main (main) where

import qualified Hypha.Mcp.Server as Mcp

main :: IO ()
main = Mcp.runMcpStdio
```

- [ ] **Step 3: Build and smoke**

Run: `cabal build hypha-mcp`
Expected: builds.

Run:

```sh
printf '%s\n' \
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26"}}' \
  | cabal run hypha-mcp
```

Expected: a JSON-RPC `initialize` response on stdout. (Exact framing depends on the `mcp` library's handling of the JSON-RPC initialize handshake; adjust the request shape per the library's docs if needed.)

- [ ] **Step 4: Commit**

```bash
git add hypha.cabal app/hypha-mcp/Main.hs
git commit -m "feat(mcp): hypha-mcp executable entry point"
```

---

## Task 3: Server polish — CSP header, `gp`/`gh`/`?` keymap, breadcrumb nav

**Files:**
- Modify: `src/Hypha/Server/App.hs`
- Modify: `ui/js/keybindings.js`
- Modify: `src/Hypha/Server/Ui/Layout.hs` (help overlay)

- [ ] **Step 1: Add Content-Security-Policy header**

Wrap the `Application` in `Hypha.Server.App` with a WAI middleware that adds the header:

```haskell
import Network.Wai (Middleware, mapResponseHeaders)
import qualified Network.HTTP.Types.Header as H

cspMiddleware :: Middleware
cspMiddleware app req respond =
  app req $ \resp -> respond (mapResponseHeaders addCsp resp)
  where
    addCsp hs = (H.HeaderName "Content-Security-Policy", csp) : hs
    csp = "default-src 'self'; img-src 'self' data:; style-src 'self' 'unsafe-inline'"

appWith :: ServerConfig -> Application
appWith cfg = cspMiddleware (serve api (server cfg))
```

> Note: `mapResponseHeaders` exists in `wai >= 3.2.4`; if pinned version lacks it, replace with manual response interception in a custom `Middleware`.

- [ ] **Step 2: Extend `ui/js/keybindings.js` with `gp`, `gh`, `?` and breadcrumb keys**

Append before the final `)();`:

```javascript
(function bindNav() {
  let pending = null;
  function clearPending() { pending = null; }

  document.addEventListener('keydown', function (ev) {
    if (document.activeElement && document.activeElement.tagName === 'INPUT') return;
    if (ev.metaKey || ev.ctrlKey || ev.altKey) return;
    if (pending === 'g') {
      clearPending();
      if (ev.key === 'p') { ev.preventDefault(); window.location.href = '/'; return; }
      if (ev.key === 'h') { ev.preventDefault(); window.location.href = '/'; return; }
    } else if (ev.key === 'g') {
      pending = 'g';
      setTimeout(clearPending, 600);
    } else if (ev.key === '?') {
      ev.preventDefault();
      const overlay = document.getElementById('help-overlay');
      if (overlay) overlay.classList.toggle('open');
    } else if (ev.key === 'h' || ev.key === 'ArrowLeft') {
      if (document.referrer) { ev.preventDefault(); history.back(); }
    } else if (ev.key === 'l' || ev.key === 'ArrowRight') {
      ev.preventDefault(); history.forward();
    }
  });
})();
```

- [ ] **Step 3: Add help overlay in `Hypha.Server.Ui.Layout`**

Insert at the top of the `body_` in `shellPage`:

```haskell
    div_ [id_ "help-overlay", class_ "help-overlay"] $ do
      div_ [class_ "help-overlay-card"] $ do
        h3_ "Keyboard shortcuts"
        table_ $ do
          tr_ (td_ "s, /, Ctrl-K"  >> td_ "focus search")
          tr_ (td_ "↑ ↓ / j k"     >> td_ "navigate results")
          tr_ (td_ "Enter"          >> td_ "open")
          tr_ (td_ "Esc"            >> td_ "dismiss")
          tr_ (td_ "← → / h l"     >> td_ "history back / forward")
          tr_ (td_ "gp"             >> td_ "packages")
          tr_ (td_ "gh"             >> td_ "home")
          tr_ (td_ "?"              >> td_ "this overlay")
```

Add a CSS rule in `ui/css/components/doc.css`:

```css
.help-overlay { display: none; position: fixed; inset: 0; background: rgba(0,0,0,0.4);
                z-index: 10; align-items: center; justify-content: center; }
.help-overlay.open { display: flex; }
.help-overlay-card { background: var(--bg); padding: 1rem 1.4rem; border-radius: 6px;
                     border: 1px solid var(--border); min-width: 300px; }
.help-overlay-card table td { padding: 0.2rem 0.6rem; }
```

- [ ] **Step 4: Build + manual smoke + commit**

Run: `cabal build`

Manual smoke (against the fixture project from Plan B):

```sh
HYPHA_FIXTURE_STORE=test/fixtures/fake-cabal-store/ghc-9.6.6/async-2.2.5-deadbeef \
  cabal run hypha -- --project-dir test/fixtures/tiny-project server --port 4287
```

Then `curl -sI http://127.0.0.1:4287/` should show `Content-Security-Policy: default-src 'self'; ...`. Open the page in a browser and press `?` to verify the help overlay appears.

```bash
git add src/Hypha/Server/App.hs ui/js/keybindings.js src/Hypha/Server/Ui/Layout.hs \
        ui/css/components/doc.css
git commit -m "feat(server): CSP header, gp/gh/? keymap, breadcrumb nav, help overlay"
```

---

## Task 4: README polish

**Files:**
- Modify: `README.md`
- Create: `CHANGELOG.md`

- [ ] **Step 1: Replace `README.md`**

Replace the entire contents of `README.md` with:

```markdown
# hypha

> An agent-first Haskell CLI that probes Hackage / Hoogle / your cabal build plan.

`hypha` (Greek *hyphē*: the branching threadlike cell of a fungus that probes
through substrate seeking nutrients) is a command-line tool for browsing
Hackage and Hoogle without leaving the terminal — designed first for AI
agents (Claude Code, opencode, Pi, …) and second for the humans they help.

It is **project-aware**: queries default to the versions your `cabal.project`
actually builds against (parsed from `dist-newstyle/cache/plan.json`), with
`--any` to widen and `--package-override` to escape-hatch.

## Mantra

> An LLM doesn't need or care about fancy Haddock HTML pages — it cares about
> the source code, which also contains the comments (the documentation). That
> is what an LLM needs in order to learn knowledge of a project. Humans need
> visuals.

## Install

### From source

```sh
git clone https://github.com/well-typed/hypha
cd hypha
cabal install hypha hypha-mcp
```

Two binaries are installed: `hypha` (the CLI) and `hypha-mcp` (the MCP stdio
server). Place both on your `PATH`.

### With Nix

```sh
nix develop      # devshell with ghc + cabal + hls
cabal build all  # then build inside the shell
```

## Quickstart

From inside any cabal project:

```sh
hypha doctor                       # diagnose the environment
hypha search 'Map.insert'          # Hoogle search scoped to the plan
hypha package async                # metadata
hypha module  async/Control.Concurrent.Async
hypha symbol  async/Control.Concurrent.Async/concurrently
hypha source  async/Control.Concurrent.Async/concurrently
hypha versions async
hypha deps async --reverse
hypha whatprovides concurrently
```

Default output is compact JSON. Add `--human` for ANSI-coloured text.
Add `--full` for the unabbreviated field set. Add `--pretty-json` for
indented JSON. Add `--select name,signature` to project fields.

## Local doc browser

```sh
hypha server --port 4287
# open http://127.0.0.1:4287
```

Press `s` (or `/`, `Ctrl-K`) to focus search. Arrow keys (or `j`/`k`) move
the cursor. `Enter` opens. `?` shows the full keymap.

Pass `--prebuild` to generate every package's Haddock up-front. The server
binds only to localhost; passing `--bind` with a non-localhost host exits
with code 2.

## MCP integration

`hypha` ships an MCP stdio server (`hypha-mcp`) that exposes a single tool,
`hypha.exec`, accepting an `args: string[]` argument and shelling out to
the `hypha` CLI. Output is the same JSON envelope, including the
`actions` and `related` cross-references that point back into further
`hypha …` invocations — so agents recurse through the tool rather than
fetching Hackage URLs.

### Claude Code

In a project's `.mcp.json` (or in `~/.claude.json` under `mcpServers`):

```json
{
  "mcpServers": {
    "hypha": { "command": "hypha-mcp", "args": [] }
  }
}
```

### opencode

In `~/.config/opencode/opencode.json` under `mcp`:

```json
{
  "mcp": {
    "hypha": { "command": "hypha-mcp", "args": [] }
  }
}
```

### Any MCP-capable client

Spawn `hypha-mcp` as a subprocess; the client talks JSON-RPC over its
stdio. No HTTP port is opened. The MCP server inherits the host's
working directory, so it auto-detects your cabal project the same way
the CLI does.

## Exit codes

| Code | Meaning |
|------|---------|
| 0    | OK |
| 2    | Bad CLI arguments |
| 3    | Not in plan (use `--any` to widen) or not on Hackage |
| 4    | Network failure, or `--offline` with cache miss |
| 5    | Cache or parse corruption |
| 7    | Environment error (no plan.json, missing ghc/haddock, Stack not supported) |

## Caching

- HTTP cache at `${XDG_CACHE_HOME:-~/.cache}/hypha/hackage/`. ETag/Last-Modified honoured. Be gentle on Hackage.
- On-demand Haddock at `~/.cache/hypha/haddock/<pkg>-<ver>/`.
- Per-project Hoogle DB at `<project>/.hypha/hoogle.hoo`; invalidated when `plan.json` changes.

Add `.hypha/` to your project's `.gitignore`.

## Status

Pre-1.0. Spec in `docs/superpowers/specs/2026-05-18-hypha-design.md`. Plans
for the three implementation phases live in `docs/superpowers/plans/`.

## License

BSD-3-Clause. See `LICENSE`. HTMX is vendored (Zero-clause BSD).
```

- [ ] **Step 2: Create `CHANGELOG.md`**

```markdown
# Changelog

All notable changes to this project are documented here. The format is loosely
based on Keep a Changelog; the project follows semantic versioning.

## [Unreleased]

## [0.1.0] — TBD

### Added

- Plan-aware CLI subcommands: `search`, `package`, `module`, `symbol`,
  `source`, `versions`, `deps`, `whatprovides`, `doctor`.
- Compact-JSON-by-default output with `--full`, `--pretty-json`, `--select`,
  `--human` modes; typed exit codes; envelope schema `hypha/v0`.
- Cross-recursion principle: every JSON response includes `actions` and
  `related` strings of further `hypha …` invocations; no Hackage URLs.
- `--project-dir`, `--package-override`, `--any`, `--global`, `--offline`
  global flags.
- `BuildEnv` records-of-functions abstraction with Cabal and Nix
  implementations and a composer.
- Hackage JSON-API client with ETag/Last-Modified filesystem cache.
- Hoogle library integration with per-project DB (plan-hash staleness)
  and `--global` fallback.
- Local doc-browser at `hypha server`: warp + servant + lucid2, vendored
  HTMX, modular CSS, embedded assets, lazy Haddock generation with
  per-package locks, `--prebuild` worker pool, localhost-only bind.
- MCP stdio server `hypha-mcp` exposing the single tool `hypha.exec`
  (Pattern B per `cli-printing-press` doctrine).

[Unreleased]: https://github.com/well-typed/hypha/compare/v0.1.0...HEAD
[0.1.0]:      https://github.com/well-typed/hypha/releases/tag/v0.1.0
```

- [ ] **Step 3: Commit**

```bash
git add README.md CHANGELOG.md
git commit -m "docs: README polish (install, MCP host snippets, mantra, exit codes) + CHANGELOG"
```

---

## Task 5: v0.1.0 release

**Files:**
- Modify: `hypha.cabal` (bump version)
- Modify: `src/Hypha/Prelude.hs` (bump version string)
- Modify: `CHANGELOG.md` (replace "TBD" with date)

- [ ] **Step 1: Bump version**

In `hypha.cabal`:

```cabal
version: 0.1.0
```

In `src/Hypha/Prelude.hs`:

```haskell
version :: String
version = "0.1.0"
```

In `CHANGELOG.md`, replace `## [0.1.0] — TBD` with `## [0.1.0] — 2026-05-18` (or the actual release date).

- [ ] **Step 2: Run the full test suite**

Run:
```
cabal build all
cabal test
cabal run hypha -- doctor
cabal run hypha -- --human search Map.insert
```

Expected: clean build, all tests pass, `doctor` reports a working environment, `search --human` produces a coloured "doc card" listing.

- [ ] **Step 3: Tag and push (manual)**

```bash
git add hypha.cabal src/Hypha/Prelude.hs CHANGELOG.md
git commit -m "release: v0.1.0"
git tag -a v0.1.0 -m "hypha v0.1.0 — initial release"
# Push only when ready, with the user's explicit approval:
# git push origin main --follow-tags
```

- [ ] **Step 4: Cut a Hackage candidate (optional)**

```bash
cabal sdist
ls dist-newstyle/sdist/hypha-0.1.0.tar.gz
# `cabal upload --publish=false dist-newstyle/sdist/hypha-0.1.0.tar.gz`
```

Do not publish without explicit user approval.

---

## Self-review

| Spec §15 / §16 item | Covered in |
|---|---|
| Pattern-B MCP shim (single `hypha.exec` tool, shells out) | Task 1 |
| `hypha-mcp` binary on `PATH` | Task 2 |
| Per-host MCP snippets (Claude Code, opencode, generic) | Task 4 (README) |
| CSP header on server responses | Task 3 |
| `gp` / `gh` / `?` / breadcrumb keymap | Task 3 |
| README mantra, etymology, exit codes, caches, install | Task 4 |
| v0.1.0 release tag | Task 5 |

Placeholder scan: no "TBD" survives once Task 5 step 1 is performed. The
`mcp` library API shapes flagged inline (Task 1 step 2) are intentional —
the constructor names may differ across the library's minor releases and
must be adapted at implementation time. Behaviour and types are fixed.

Type / name consistency: `ExecArgs`, `execHypha`, `runMcpStdio`, and the
single tool name `hypha.exec` are referenced identically across `Hypha.Mcp.Server`,
`app/hypha-mcp/Main.hs`, the README, and the CHANGELOG.

---

## Execution handoff

Plan C complete and saved to `docs/superpowers/plans/2026-05-18-hypha-plan-c-mcp-release.md`.

Two execution options:

**1. Subagent-Driven (recommended)** — fresh subagent per task, review between.
**2. Inline Execution** — `superpowers:executing-plans` with checkpoints.

Which approach?
