---
name: hypha-haskell
description: Use whenever working in a Haskell/cabal project — looking up a function, exploring a package, reading Haddock, or finding a symbol's source. Replaces WebFetch on hackage.haskell.org/hoogle.haskell.org and ad-hoc grepping of `.cabal-store` with the project-aware `hypha` CLI, which emits compact JSON pinned to the project's `plan.json`. Triggers on `.hs`/`.cabal`/`cabal.project` edits, Haskell library or module mentions, and any "what's the type of …", "where is … defined", "which version of … are we using" question.
---

# hypha — Haskell-aware code/doc browser

`hypha` is a CLI that answers Hackage/Hoogle/source questions against the
**exact versions** pinned in the current cabal project's
`dist-newstyle/cache/plan.json`. Output is compact JSON by default — much
cheaper than parsing rendered Haddock HTML.

## When to use

Use `hypha` — **not** `WebFetch` on hackage.haskell.org / hoogle.haskell.org,
**not** grepping `~/.cabal/store` or `dist-newstyle/` — for any of:

- Looking up a function/type signature or its Haddock.
- Listing a module's exports.
- Reading the source of a symbol (canonical declaration, not the re-export).
- Checking which version of a package the project depends on.
- Inspecting a package's metadata, dependencies, or version history.
- Diagnosing why a Haskell tool can't find something.

## Invariant: run from the project root

`hypha` reads `dist-newstyle/cache/plan.json` to pin versions. Always invoke
it from a directory that has `cabal.project` (or pass `--project-dir`).
If `plan.json` is missing, suggest `cabal build` first or run
`hypha doctor`.

## Command surface

| Command | Purpose | Example |
|---------|---------|---------|
| `hypha lookup <name-or-sig>` | Tiered resolution: cache → local Hoogle → remote. Takes a symbol name or a type signature. | `hypha lookup lookup` · `hypha lookup 'a -> Maybe a'` |
| `hypha package <pkg>` | Package metadata: name, version, exposed modules. | `hypha package async` |
| `hypha module <pkg/Module>` | List a module's exports. | `hypha module async/Control.Concurrent.Async` |
| `hypha symbol <pkg/Module/Sym>` | Signature + Haddock for one symbol. | `hypha symbol async/Control.Concurrent.Async/concurrently` |
| `hypha source <pkg/Module[/Sym]>` | Source snippet at the canonical declaration. | `hypha source containers/Data.Map.Strict/insert` |
| `hypha versions <pkg>` | Version history on Hackage. | `hypha versions text` |
| `hypha deps <pkg> [--reverse] [--depth N]` | Dependencies (forward or reverse). | `hypha deps aeson --reverse` |
| `hypha doctor` | Diagnose the environment (plan, store, cache, Hoogle DB). | `hypha doctor` |

`hypha server` exists too but is for humans (browser UI); skip it from agent
flows.

## Global flags — token economy

- **Default output is compact JSON.** Parse it directly; do not pipe to
  `jq` unless you genuinely need a sub-selection.
- `--select sig,haddock` — return only the listed fields. Use this when
  you only want the type signature, or only the docs.
- `--full` — opt into the heavier payload (full Haddock prose, all
  fields). Use sparingly; only when `--select` cannot express what you
  need.
- `--human` — ANSI prose. Only when showing output to the user, never
  for parsing.
- `--pretty-json` — pretty-printed JSON. Debugging only; wastes tokens.
- `--offline` — skip network. Use when the user is offline or wants
  pinned-only answers.
- `--project-dir DIR` — explicit project root. Use only when not already
  in one.
- `--quiet` / `--verbose` — log verbosity. Default is fine.

## Identifier syntax

Hypha uses `PKG/MOD/SYM` triples (slash-separated) for symbols and
`PKG/MOD` for modules. Examples:

- `aeson/Data.Aeson/encode`
- `containers/Data.Map.Strict/insert`
- `text/Data.Text`

If the user says "the `lookup` in `Data.Map`", resolve it via
`hypha lookup lookup` first to get the fully-qualified id, then pass that to
`hypha symbol` / `hypha source`.

## Workflow recipes

**"What's the type of X?"**

```bash
hypha lookup X --select sig
```

**"Show me the Haddock for `aeson`'s `encode`."**

```bash
hypha symbol aeson/Data.Aeson/encode --select sig,haddock
```

**"Where is `Data.Map.insert` actually defined?"**

```bash
hypha source containers/Data.Map.Strict/insert
```

**"Which version of `text` are we building against?"**

```bash
hypha package text --select name,version
```

**"What depends on `mtl` in our plan?"**

```bash
hypha deps mtl --reverse
```

**"Is there a function with type `a -> Maybe a`?"**

```bash
hypha lookup 'a -> Maybe a'
```

## CLI first, MCP only if no shell

Follow the [cli-printing-press](https://github.com/mvanhorn/cli-printing-press#why-clis-plus-mcp)
guidance: **CLIs win for agents** (100x fewer tokens than MCP tool
schemas, native to LLM training distribution); **MCP wins for IDE
auto-discovery**. So:

- **Default to `Bash hypha …`.** Zero schema-token tax, one fewer
  process hop (`hypha-mcp` shells to the same binary anyway), closer
  to the shell-interaction patterns the model was trained on.
- **Use the MCP tools only when no Bash is available** (e.g. a harness
  without a shell tool, or an IDE driving `hypha-mcp` directly).

When the MCP tools *are* the right call, prefer the **per-command
tools** (`hypha.lookup`, `hypha.symbol`, `hypha.source`, …) over the
generic `hypha.exec`. They take structured arguments instead of an
argv array, so the model does not have to spell out flag plumbing.
Reach for `hypha.exec` only as an escape hatch when no per-command
tool fits.

**`hypha.exec` calling convention** (escape hatch). Single field
`args` = argv array passed to the `hypha` binary. Not a bare query
string.

```json
{"args": ["lookup", "filterM"]}
{"args": ["lookup", "a -> Maybe a", "--select", "sig"]}
{"args": ["symbol", "aeson/Data.Aeson/encode", "--select", "sig,haddock"]}
{"args": ["source", "containers/Data.Map.Strict/insert"]}
```

## Commands that do NOT exist

Do not invent these — they have been removed or never existed:

- `hypha search` — removed in 0.2.0. Use `hypha lookup` for tiered
  symbol/type-signature resolution.
- `hypha whatprovides` — removed in 0.2.0. Use `hypha lookup`.
- `hypha install`, `hypha update` — `hypha` does not manage packages;
  use `cabal` for that.
- `--global` — flag removed in 0.2.0; the cascade decides which tier
  answers the query.

## Failure modes

- **`plan.json not found`** → user hasn't run `cabal build` yet, or you're
  not in a cabal project. Suggest `cabal build` or check `cabal.project`.
- **`unknown package: …`** → package isn't in the project's build plan.
  Confirm spelling, or check `hypha deps` from a package that depends on it.
- **Hoogle results stale** → run `hypha doctor`; the local Hoogle DB
  regenerates lazily.
- **`TOOL_MISSING`** (exit `8`, e.g. `haddock binary not found on PATH`) →
  required external binary is unavailable to `hypha`. Usually means one
  of (a) the user hasn't installed the GHC toolchain in this shell, or
  (b) Claude Code's sandbox is hiding `~/.ghcup` / `~/.cabal` from the
  spawned process even though the binary exists in the user's terminal.
  **Do not** hallucinate the answer from memory. Instead:
    1. Run `hypha doctor` and report what it found.
    2. Re-run the original query — `hypha lookup` already falls through
       to remote Hoogle on `TOOL_MISSING`; the failure usually only
       affects `hypha source` / `hypha symbol`, which need the cabal
       store.
    3. Tell the user the binary is missing and (if relevant) point
       them at the "Running under Claude Code's sandbox" section in
       the hypha README.

## Anti-patterns

- ❌ `WebFetch https://hackage.haskell.org/package/…` — use `hypha package`.
- ❌ `WebFetch https://hoogle.haskell.org/?hoogle=…` — use `hypha lookup`.
- ❌ `grep -r "foo ::" ~/.cabal/store` — use `hypha symbol` or
  `hypha source`.
- ❌ Asking the user which version of a dep they're on — use `hypha package`.
- ❌ `hypha … --pretty-json` for parsing — wastes tokens.
- ❌ `hypha … --full` when `--select` would do.
