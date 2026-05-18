# Hypha — LLM Agent Instructions

> **Purpose:** This file tells Claude Code, GitHub Copilot, Pi, or any other AI agent how to behave when working on the `hypha` codebase.

## Project Overview

`hypha` is an **agent-first CLI** for browsing Hackage and Hoogle, scoped to a local `cabal` project's actual build plan. It emits compact JSON by default (`--human` for ANSI text). It is a single cabal package with one library + one executable. Effects are records-of-functions parameterized over `m`, wired into `ReaderT Env IO`.

**Key docs:**
- Master spec: `docs/superpowers/specs/2026-05-18-hypha-design.md`
- Design plans:
    * `docs/superpowers/plans/2026-05-18-hypha-plan-a-cli-alpha.md`
    * `docs/superpowers/plans/2026-05-18-hypha-plan-b-server.md`

## Multi-Agent Workflow

Up to 3 agents can work concurrently, each in its own git worktree:

| Agent | Worktree | Branch |
|-------|----------|--------|
| 1 | `.worktrees/agent-1/` | `agent-1/main` |
| 2 | `.worktrees/agent-2/` | `agent-2/main` |
| 3 | `.worktrees/agent-3/` | `agent-3/main` |

**CRITICAL: You MUST work inside your assigned worktree.** Your `$PWD` must be `.worktrees/agent-N/` — never the main repo.

### Issue Assignment

The human assigns issues manually to avoid race conditions. **Never pick an issue yourself.** Wait for the human to say: *"Agent N, work on issue XXX"*.

### Per-Task Workflow

When assigned an issue:

```
1. cd .worktrees/agent-N/                           # your worktree
2. git fetch origin && git merge origin/main        # stay current
3. git checkout -b agent-N/feat-<short-description>  # feature branch
4. Move issue: issues/todo/XXX → issues/in_progress/XXX
5. git commit -m "chore: claim issue XXX"
6. Implement exactly what the issue says
7. Run: cabal build all && cabal test all
8. Move issue: issues/in_progress/XXX → issues/done/XXX
9. git commit -m "feat(<scope>): <description>"      # conventional commit
10. Tell human: "Agent N ready to merge agent-N/feat-<desc>"
11. Wait for human to merge into main
12. git checkout agent-N/main && git pull origin main
13. Rinse and repeat from step 1
```

### Blocked?

If you cannot proceed (missing dependency, unclear spec, failing test you can't fix):
1. Add a comment to the issue file explaining the blocker
2. Move the issue back to `issues/todo/`
3. Tell the human what's blocking you

## Issue Board

We track work in `issues/todo/`, `issues/in_progress/`, and `issues/done/`.

- **To start work:** Move an issue from `issues/todo/` → `issues/in_progress/`
- **To finish work:** Move it from `issues/in_progress/` → `issues/done/`
- **Blocked?** Leave a comment in the issue file and move to `issues/todo/`

Never work on an issue without moving it to `in_progress` first.

## How to Pick Up Work (if human doesn't assign)

1. Read the master spec (`docs/superpowers/specs/2026-05-18-hypha-design.md`) for context.
2. Pick the next unblocked issue in `issues/todo/`.
3. Move the file to `issues/in_progress/`.
4. Implement exactly what the issue says.
5. Run `cabal build all && cabal test all` before declaring done.
6. Move the issue to `issues/done/`.
7. There might be multiple agents working concurrently on the codebase, so pick one
   unclaimed issue but stop and escalate to the user if you notice that the issue you have
   picked has a direct dependency on an issue currently "in progress".

## Hard Conventions (Follow Religiously)

1. **Strict bangs:** Every strict field in `data`/`newtype` gets `!`. Lazy fields get a one-line comment explaining why.
2. **No `error`/`undefined` in production:** Boundary failures go through `Hypha.Error.HyphaError`. Logic errors should be unreachable by construction.
3. **Imports:** Minimal and sorted. Use `Hypha.Prelude` for shared shorthands.
4. **Effects:** Records-of-functions over `m`. No effect library. No typeclass effect machinery.
5. **Types:** Prefer `newtype` with `deriving stock` + `deriving newtype`. Avoid partial records.
6. **Output:** Every command produces an `Outcome Value` wrapped in `OutcomeEnvelope`.

## Testing Discipline

- **Every step that changes code** → run tests for that area.
- **Every task ends with a commit.**
- **Property tests** use `Test.Tasty.Falsify`.
- **Golden tests** use `Test.Tasty.Golden`.
- **Unit tests** use `Test.Tasty.HUnit`.

## Commit Messages

Follow [Conventional Commits](https://www.conventionalcommits.org/):

- `feat:` new feature
- `test:` adding or correcting tests
- `docs:` documentation only
- `chore:` build/tooling changes

## What NOT to Do

- Do NOT add new dependencies without updating the plan and the issue.
- Do NOT implement `--server` or `mcp` — those are Plan B and Plan C.
- Do NOT push between tasks unless explicitly told to.
- Do NOT skip tests.
- Do NOT refactor unrelated modules while working on an issue.
- Do NOT work outside your assigned worktree.
