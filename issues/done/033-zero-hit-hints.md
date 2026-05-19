# Issue 033: 0-hit hints for plan-scoped queries

**Status:** todo
**Priority:** P3
**Blocked by:** —
**Commit:** `feat(output): hint on empty results for plan-scoped queries`

## Goal
When `search` or `whatprovides` returns 0 hits because the query symbol/package isn't in the build plan, the output gives no indication that plan-scoping might be the reason. The agent wasted calls trying to figure out why `race` — a well-known symbol — returned nothing.

## Fix
When a plan-scoped query returns 0 results, include a `hint` field (or add an `actions` entry) suggesting `--global` to widen.

- `search`: if 0 hits and `--global` not set, add `hint: "no results in plan, try --global to widen to stackage"`
- `whatprovides`: same pattern

## Files to Modify
- `src/Hypha/Command/Search.hs` — add hint on 0 results
- `src/Hypha/Command/WhatProvides.hs` — same

## Acceptance Criteria
- [ ] `hypha search race` without `--global` returns 0 results + hint in envelope
- [ ] `hypha whatprovides race` without `--global` returns 0 providers + hint
- [ ] Hints don't appear when `--global` is set (no point hinting at what's already active)
