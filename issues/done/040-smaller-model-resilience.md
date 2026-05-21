# Task 040: Smaller-model resilience for hypha-mcp + SKILL.md

**Status:** in_progress
**Priority:** P2 (quality-of-life for non-Claude agents)
**PR:** One PR
**Commit:** `feat(plugin): tighten MCP tool + SKILL.md for non-Claude models`

## Goal

Make `hypha` behave well when driven by smaller / less obedient models
(GLM-4.7-Flash, Llama, etc.) routed through harnesses like `pi`. Today
those models recall the skill loosely and invent calling conventions
that don't exist.

## Motivation

A live `pi` + LM-Studio (GLM-4.7-Flash) session shows the symptoms:

- Model tried `mcp` with a bare query string `"ToJSON"` instead of the
  argv array `["lookup", "ToJSON"]` that `hypha-mcp.hypha.exec` expects.
- Model invented `hypha search "ToJSON"` (removed in 0.2.0; only
  `lookup` exists).
- Skill content is currently written assuming Claude-level instruction
  following; small models drift.

## Scope

### F1 — Tighten the `hypha.exec` MCP tool definition

In `Hypha.Mcp.Server`:

- Expand the tool `description` from
  `"Run a hypha CLI command and return its JSON envelope."` to spell
  out the argv shape, with an example. Models that scan the
  `tools/list` description for calling guidance will get the shape
  right on first try.
- Tighten the `args` property `description`: emphasise it is the
  command's argv (e.g. `["lookup","ToJSON","--select","sig"]`), NOT a
  bare query string.
- Add a small `examples` block (free-form, MCP allows extra fields) so
  hosts that surface examples show the canonical shape.

### F2 — Add a "Removed commands" line to SKILL.md

One short subsection or bullet that says: `hypha search` no longer
exists; the only resolution command is `hypha lookup`. Repeat the
argv-array shape for MCP callers (mirrors F1) and the project-root
invariant in plain prose so a small model doesn't have to chain three
inferences to use the tool right.

### F3 — Document sbox/sandbox mount requirements (out of scope; tracked here)

Add a note to README's existing sandbox troubleshooting section that
the same class of failures bites the `pi` harness too (CA bundle, cabal
store, ghcup root). Quick win, no code.

## Acceptance Criteria

- [ ] `tools/list` describes `hypha.exec`'s argv array clearly enough
      that an MCP client logs the right shape unaided.
- [ ] SKILL.md mentions removed commands and reiterates the argv shape
      for MCP callers.
- [ ] README sandbox section explicitly lists `/etc/ssl/certs`,
      `~/.cabal/store`, and `~/.ghcup` as paths that typically need
      read access for hypha's local-fast path to work under a sandbox.
- [ ] `cabal build all && cabal test all` green.
- [ ] CHANGELOG entry under `[Unreleased]`.

## Out of Scope

- Per-subcommand MCP tools (planned follow-up, separate issue).
- Adapting skill text per model family — we keep one SKILL.md.
