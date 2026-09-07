# Claude Code Plugin

`hypha` ships as a [Claude Code plugin](https://docs.claude.com/en/docs/claude-code/plugins)
that auto-loads a skill teaching Claude to prefer the `hypha` CLI over
`WebFetch` on hackage.haskell.org / hoogle.haskell.org and over ad-hoc
grepping of `~/.cabal/store`. The plugin lives at the root of this repo
(`.claude-plugin/{plugin,marketplace}.json` + `skills/hypha-haskell/SKILL.md`).

**Prerequisite:** the `hypha` and `hypha-mcp` binaries must already be on
`$PATH` — install per **[Installation](installation.md)** first. The plugin
ships skill content, slash commands, and an `mcpServers` declaration that
auto-registers `hypha-mcp` with Claude Code on install; it does **not**
vendor the binaries themselves.

## Option A — install from the Well-Typed marketplace (recommended)

Inside a Claude Code session:

```
/plugin marketplace add https://github.com/well-typed/hypha.git
/plugin install hypha@well-typed
```

> The first command opens an interactive TUI prompting you to confirm the
> marketplace add. Accept it, then run the second command.

## Option B — install from a local clone

If you already have the repo checked out (e.g. for development):

```
/plugin marketplace add /absolute/path/to/hypha
/plugin install hypha@well-typed
```

Use the absolute path; Claude Code resolves the marketplace from the
directory's `.claude-plugin/marketplace.json`.

## Verify the install

```
/plugin list
```

You should see `hypha@well-typed` enabled. Open any `.hs` or `.cabal` file
and Claude will auto-trigger the `hypha-haskell` skill on the next Haskell
question. The `/hypha-lookup <symbol-or-signature>` slash command becomes
available too.

## Uninstall

```
/plugin uninstall hypha@well-typed
/plugin marketplace remove well-typed
```
