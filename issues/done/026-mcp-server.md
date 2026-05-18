# Task 26: MCP Server — Pattern B Shim

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 25 (server-smoke-test)  
**PR:** One PR  
**Commit:** `feat(mcp): Pattern-B stdio shim — single hypha.exec tool that shells out to CLI`

## Goal
Implement `Hypha.Mcp.Server` — a thin MCP stdio server that registers one tool (`hypha.exec`) which shells out to the `hypha` CLI and returns the stdout JSON envelope verbatim.

## Files to Create
- `src/Hypha/Mcp/Server.hs` — `runMcpStdio`, `ExecArgs`, `execHypha`
- `test/Unit/Mcp.hs` — unit tests for `execHypha`

## Files to Modify
- `hypha.cabal` — add `mcp`, `typed-process`; expose `Hypha.Mcp.Server`
- `test/Main.hs` — register `Unit.Mcp`

## Design
- Single tool: `hypha.exec` with `{ "args": string[] }` argument schema
- Handler decodes `ExecArgs`, spawns `hypha` binary via `typed-process`, returns stdout verbatim
- Exit code + stderr stuffed into tool result metadata
- `HYPHA_BIN` env var overrides binary path (for testing)

## Acceptance Criteria
- [ ] `runMcpStdio` registers one tool named `hypha.exec`
- [ ] `execHypha` spawns subprocess, returns `(exitCode, stdout, stderr)`
- [ ] `ExecArgs` JSON decoding works (`{"args": ["search", "Map.insert"]}`)
- [ ] Unit test: `execHypha "/bin/echo" ["hello"]` returns exit 0 + "hello\n"
- [ ] Unit test: `execHypha "/bin/sh" ["-c", "exit 3"]` returns exit 3
- [ ] Build succeeds
