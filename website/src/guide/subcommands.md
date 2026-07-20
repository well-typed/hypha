# Subcommands

Every subcommand takes an [identifier](identifiers.md) and honours the
[global flags](flags.md).

| Command | Args | Purpose |
|---------|------|---------|
| `lookup` | `QUERY` | Tiered symbol resolution — see [Looking Up Symbols](lookup.md) |
| `package` | `<pkg>[-ver]` | Package metadata (name, version, exposed modules) |
| `module` | `<pkg>/<Mod>` | Exported symbols with signatures |
| `symbol` | `<pkg>/<Mod>/<sym>` | Full info: signature, Haddock, source coords |
| `source` | `<pkg>/<Mod>/<sym>` or `<pkg>/<Mod>` | Source slice |
| `versions` | `<pkg>` | Version history on Hackage, plan-pinned marker |
| `deps` | `<pkg> [--reverse] [--depth N]` | Forward/reverse deps within the plan |
| `doctor` | — | Environment health check |
| `server` | `[--port N] [--bind H:P] [--prebuild]` | [Doc-browser HTTP server](../server/index.md) |

> **MCP.** There is no `hypha mcp` subcommand. The MCP surface is a
> separate binary, `hypha-mcp` — see **[MCP Host Integration](../mcp/index.md)**.

Each command emits a structured envelope (`result:` + `actions:`) in YAML by
default. See [Global Flags](flags.md) for `--json`, `--select`, and `--full`.
