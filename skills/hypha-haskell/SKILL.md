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

## MCP alternative

If `hypha-mcp` is configured as an MCP server in the client, prefer the
`hypha.exec` tool call over a `Bash` invocation — it avoids re-parsing
JSON through the shell.

## Failure modes

- **`plan.json not found`** → user hasn't run `cabal build` yet, or you're
  not in a cabal project. Suggest `cabal build` or check `cabal.project`.
- **`unknown package: …`** → package isn't in the project's build plan.
  Confirm spelling, or check `hypha deps` from a package that depends on it.
- **Hoogle results stale** → run `hypha doctor`; the local Hoogle DB
  regenerates lazily.

## Anti-patterns

- ❌ `WebFetch https://hackage.haskell.org/package/…` — use `hypha package`.
- ❌ `WebFetch https://hoogle.haskell.org/?hoogle=…` — use `hypha lookup`.
- ❌ `grep -r "foo ::" ~/.cabal/store` — use `hypha symbol` or
  `hypha source`.
- ❌ Asking the user which version of a dep they're on — use `hypha package`.
- ❌ `hypha … --pretty-json` for parsing — wastes tokens.
- ❌ `hypha … --full` when `--select` would do.
