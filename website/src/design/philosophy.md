# Philosophy: cli-printing-press

`hypha` is built to the principles laid out in
[cli-printing-press](https://github.com/mvanhorn/cli-printing-press) — a
manifesto for *agent-native* CLIs. The relevant tenets and how `hypha`
honours them:

| cli-pp principle | hypha |
|---|---|
| **Agent-native by default** | Compact YAML is the default; `--json` is opt-in for pipelines. |
| **Typed exit codes** | `0` success, `2` user error, `3` not found, `4` network, `5` cache corruption, `7` environment, `8` tool missing — every failure is classifiable without parsing error text. |
| **Local-first data layer** | SQLite caches (per-project + shared global), an on-disk Hoogle DB, ETag-revalidated Hackage HTTP cache, and a fuzzy index — all built so repeat queries stay off the network. |
| **Compact mode for tokens** | Compact YAML is the *default*; `--select f1,f2` projects fields; `--full` is opt-in. `--json` for machine pipelines. No HTML noise. |
| **Human + machine output modes** | YAML for agents/terminals, `--json` for pipelines, HTMX-rendered HTML for the `server` UI — same data, three surfaces. |
| **Actionable errors** | Every failure envelope carries a stable `code` and an `actions` map suggesting the next command to try. |
| **Verified, not vibes** | Property tests via `falsify`, golden output regressions via `tasty-golden`, edge cases via `tasty-hunit`. CI gates merges. |
| **Non-obvious insight** | Symbols resolve to the canonical declaration even across re-exports and CPP `#ifdef` branches — the value that raw Hackage HTML cannot give you. |
| **Dual interface from one spec** | The `hypha` CLI, the `hypha-mcp` JSON-RPC shim, and the `hypha server` HTML UI share one library — no duplicated client/store code. |

## CLI vs MCP — the cli-pp split

cli-printing-press is explicit that *CLIs win for agents* (cheaper tokens,
native to shell-trained LLMs) and *MCP wins for IDE auto-discovery*.
`hypha` follows that split:

- **Agents should call `hypha` directly** through a shell tool. The Claude
  Code skill (`skills/hypha-haskell/SKILL.md`) tells the model to prefer
  `Bash hypha …` over the MCP tools.
- **`hypha-mcp` exists for IDE/MCP-only harnesses** (Claude Desktop, Cursor,
  opencode without a shell). It exposes one MCP tool per CLI subcommand
  (`hypha.lookup`, `hypha.symbol`, …) so IDE auto-discovery surfaces
  structured arguments; the generic `hypha.exec` remains as an escape
  hatch. See [MCP Host Integration](../mcp/index.md).
