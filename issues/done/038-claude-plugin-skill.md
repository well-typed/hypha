# Task 038: Claude Code Plugin + Skill

**Status:** in_progress
**Priority:** P1 (ship internally to WT)
**PR:** One PR
**Commit:** `feat(plugin): claude-code plugin with hypha-haskell skill`

## Goal

Ship hypha as installable Claude Code plugin so any WT engineer can `/plugin
install` it and have Claude automatically prefer `hypha` over `WebFetch`/grep
when working in Haskell projects.

## Why

`hypha` only saves tokens if agents actually call it. Without a skill, Claude
keeps `WebFetch`-ing hackage.haskell.org. A SKILL.md auto-loaded by Claude
Code teaches the model when + how to invoke `hypha`.

## Design

Path A from design discussion: in-repo plugin scaffold. Single source of
truth; CLI + skill version always match.

## Files

- `.claude-plugin/plugin.json` — plugin manifest (name, version, description).
- `skills/hypha-haskell/SKILL.md` — main skill with frontmatter trigger
  conditions + body teaching the command surface.
- `commands/hypha-lookup.md` — slash command shortcut (optional, nice-to-have).
- `README.md` — add "For Claude Code users" section with install instructions.
- `CHANGELOG.md` — record plugin addition.

## Acceptance Criteria

- [ ] `.claude-plugin/plugin.json` validates (has `name`, `version`,
      `description`).
- [ ] `skills/hypha-haskell/SKILL.md` has valid YAML frontmatter with
      `name` and `description` fields.
- [ ] SKILL.md body covers: when to use, every `hypha` subcommand with
      one-line description, `--select`/`--full`/`--human` flag rules,
      project-root invariant, "never WebFetch hackage" rule, MCP fallback.
- [ ] README documents install via `/plugin marketplace add` or git clone.
- [ ] `cabal build all && cabal test all` still green (no code changes
      expected; sanity check).

## Out of Scope

- Publishing to a public marketplace.
- MCP tool changes.
- FTS5/raw-SQL surface (separate issue if we want cli-pp parity).
