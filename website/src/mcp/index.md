# MCP Host Integration

`hypha-mcp` is a thin JSON-RPC 2.0 stdio shim (a separate binary from the
`hypha` CLI). It exposes one MCP tool per CLI subcommand — `hypha.lookup`,
`hypha.package`, `hypha.module`, `hypha.symbol`, `hypha.source`,
`hypha.versions`, `hypha.deps`, `hypha.doctor` — each with a structured
input schema that IDE clients can render as a form. A generic `hypha.exec`
tool remains as an escape hatch for argv-level invocation.

> **Prefer the CLI where you can.** Per
> [cli-printing-press](../design/philosophy.md), agents in a shell-capable
> harness should call `hypha` directly via `Bash` / equivalent, not via
> MCP. The MCP surface exists for IDE auto-discovery (Claude Desktop,
> Cursor) and for harnesses without a shell.

If your AI harness doesn't support Claude plugins, add `hypha-mcp` as an MCP
client directly.

## Claude Code

If you installed the [Claude Code plugin](../getting-started/claude-plugin.md),
`hypha-mcp` is registered for you on `/plugin install` — no manual config
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

## opencode

In `opencode.json`:

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

## Generic MCP client

Point any MCP-compatible host at the `hypha-mcp` executable over stdio. It
speaks JSON-RPC 2.0 and exposes the tools described above, including the
`hypha.exec` escape hatch.
