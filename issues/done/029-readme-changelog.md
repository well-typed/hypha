# Task 29: README Polish + CHANGELOG

**Status:** todo  
**Priority:** P2  
**Blocked by:** Task 28  
**PR:** One PR  
**Commit:** `docs: README polish (install, MCP host snippets, mantra, exit codes) + CHANGELOG`

## Goal
Rewrite README with etymology, install instructions, mantra, quickstart, MCP integration snippets, exit codes, caching docs. Create CHANGELOG.

## Files to Modify
- `README.md` — full rewrite
- `CHANGELOG.md` — create with v0.1.0 section

## README Sections
1. Title + tagline + etymology (Greek *hyphē*)
2. Mantra quote
3. Install (from source, with Nix)
4. Quickstart (all subcommands with examples)
5. Local doc browser (`hypha server`)
6. MCP integration (Claude Code `.mcp.json`, opencode, generic)
7. Exit codes table (0, 2, 3, 4, 5, 7)
8. Caching (HTTP, Haddock, Hoogle)
9. Status + spec links
10. License

## CHANGELOG Sections
- `[Unreleased]`
- `[0.1.0] — TBD` with all features from Plans A, B, C

## Acceptance Criteria
- [ ] README has etymology, mantra, install, quickstart, MCP snippets, exit codes
- [ ] MCP snippets for Claude Code, opencode, and generic client
- [ ] CHANGELOG created with v0.1.0 section
- [ ] No TBD placeholders remain (filled in Task 30)
