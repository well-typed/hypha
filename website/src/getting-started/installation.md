# Installation

`hypha` is a single cabal package producing two executables: `hypha` (the
CLI) and `hypha-mcp` (the [MCP](../mcp/index.md) stdio shim).

## From source (requires GHC ≥ 9.6)

```bash
git clone https://gitlab.well-typed.com/well-typed/hypha.git
cd hypha
cabal build all
cabal install exe:hypha
cabal install exe:hypha-mcp
```

## Nix (flakes)

```bash
nix run gitlab:well-typed/hypha#hypha -- --help
```

## Claude Code plugin

If you drive Claude Code, install the bundled plugin so Claude prefers
`hypha` over `WebFetch` and ad-hoc `grep` of `~/.cabal/store`. It ships the
skill, slash commands, and an auto-registered `hypha-mcp` MCP server — see
**[Claude Code Plugin](claude-plugin.md)**.

> The plugin does **not** vendor the binaries. Install `hypha` and
> `hypha-mcp` per *From source* (or Nix) first, and make sure both are on
> your `$PATH`.
