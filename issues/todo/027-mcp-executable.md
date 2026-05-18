# Task 27: `hypha-mcp` Executable

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 26  
**PR:** One PR  
**Commit:** `feat(mcp): hypha-mcp executable entry point`

## Goal
Create the `hypha-mcp` executable that runs the MCP stdio server.

## Files to Create
- `app/hypha-mcp/Main.hs` — thin wrapper calling `Mcp.runMcpStdio`

## Files to Modify
- `hypha.cabal` — add `executable hypha-mcp` stanza

## Acceptance Criteria
- [ ] `cabal build hypha-mcp` succeeds
- [ ] `hypha-mcp` binary runs `runMcpStdio`
- [ ] JSON-RPC initialize handshake returns valid response
- [ ] Build succeeds
