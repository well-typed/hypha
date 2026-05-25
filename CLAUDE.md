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

## Well-Typed Ethos

This codebase is held to Well-Typed quality standards. Internalise these before touching code:

- **Make impossible states unrepresentable** (Yaron Minsky). If a runtime branch ends in `error "unreachable"` or `case _ of _ -> error ...`, the type is not precise enough. Refine the type (e.g. split `Command` into `ClientCommand` / `ServerCommand`) so the impossible branch cannot be written.
- **Types over strings.** Stringly-typed APIs are banished. A `Text` that "is really" a package name, error code, or command tag must become a `newtype` (or a sum type) at the earliest boundary. `case err of NotFound m -> m; _ -> show err` is a code smell that proves the type was too loose.
- **`mtl` / `transformers` over zig-zags.** Long cascades of `case eX of Left e -> pure (Left e); Right x -> case eY of ...` are banished. Use `ExceptT` / `MaybeT` / `ReaderT` and let `do`-notation linearise the happy path. Typed sub-errors compose under `withExceptT`; `liftMaybe :: HyphaError -> Maybe a -> ExceptT HyphaError m a` beats nested case-of.
- **No duplication.** If two functions share a body, factor the shared body into a helper (e.g. `sourceFromDirE` shared by `runSource` and `runSourceFromDir`). DRY at the function level, not via macro-style copy-paste.
- **Errors are first-class.** All boundary failures funnel through `Hypha.Error.HyphaError`, which embeds typed sub-errors (`DiscoveryError`, `PlanError`, ...). Never wrap them back into `Text` and lose the structure.
- **Render at the edge, not at the source.** Domain types (`HoogleQuery`, `Tier`, `RemoteError`, `PackageId`, `BindError`, ...) MUST be carried as themselves through the program. Stringifying them inside an error constructor — `Text.pack (show e)`, `Text.intercalate "," . map tierToText`, `T.pack . renderFoo` at the call site — destroys structure, forces every downstream consumer to re-parse what was already known, and is the canonical sloppy refactor. Conversion to `Text` belongs in the rendering layer (`errorMessage`, `errorActions`, JSON encoders, `--human` formatter, `renderXxx` helpers) — at the wire boundary, not at the call site. Correct: `HoogleRemoteError !HoogleQuery ![Tier] !RemoteError`. Sloppy: `HoogleRemoteError !Text !Text !Text`. If you find yourself writing a `renderTiers q` call to feed an error constructor, stop — the constructor's type is wrong.
- **Never round-trip your own output.** Decoding bytes you just encoded is always a bug. The `Either` / `Maybe` that appears on the parse side is a phantom — a "failure mode" the code manufactured for itself and then has to "handle" with a silent fallback (typically a `Left _ -> dump raw bytes` that lies to the user). If you need both the structured form and the serialised form, build the structured form first (e.g. `encodeOutcomeEnvelope :: ... -> Value`) and serialise it last (`encodeEnvelopeValue :: Value -> ByteString`). The JSON path and the `--human` path share the same `Value`; neither parses anything. Symptom to grep for: `Aeson.eitherDecode` / `Aeson.decode` applied to a `ByteString` produced anywhere in our own codebase. If you see one, the design is doing work in the wrong order.
- **Never ignore an error branch silently.** `Left _ -> pure fallback`, `either (const fallback) id`, `fromRight fallback`, and friends are banished when applied to a real failure. Even when a degraded fallback is *intentional* (e.g. `hypha lookup` outside a cabal project), the underlying error MUST be surfaced — traced to stderr, attached to the outcome envelope, or propagated. Silent swallowing turns "the tool degraded gracefully" into "the user has no idea what went wrong." If the variable name is `_err`, you're doing it wrong. Use a helper like `warnOnLeft` that makes the fallback explicit and reports the cause.

## Hard Conventions (Follow Religiously)

1. **Strict bangs:** Every strict field in `data`/`newtype` gets `!`. Lazy fields get a one-line comment explaining why.
2. **No `error`/`undefined` in production:** Boundary failures go through `Hypha.Error.HyphaError`. Logic errors should be unreachable by construction (refine the type — see ethos above).
3. **Imports:** Minimal and sorted. Use `Hypha.Prelude` for shared shorthands.
4. **Effects:** Records-of-functions over `m`. No effect library. No typeclass effect machinery.
5. **Types:** Prefer `newtype` with `deriving stock` + `deriving newtype`. Avoid partial records. No stringly-typed parameters.
6. **Output:** Every command produces an `Outcome Value` wrapped in `OutcomeEnvelope`.
7. **Control flow:** Cascading `case eX of Left _ -> ...; Right _ -> case eY of ...` is banned. Reach for `ExceptT` / `MaybeT` instead.

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
