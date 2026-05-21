# Task 039: Sandbox & TOOL_MISSING resilience

**Status:** in_progress
**Priority:** P1 (blocks effective Claude Code plugin usage)
**PR:** One PR
**Commit:** `feat(error): TOOL_MISSING classification + cascade resilience`

## Goal

Make hypha usable from inside Claude Code's sandboxed Bash, where
`~/.ghcup`, `~/.cabal/store`, and friends may be invisible (ENOENT) even
though the user has them installed. A `hypha lookup` for a stdlib symbol
should still return a remote-Hoogle answer instead of failing with a
misclassified `NETWORK_ERROR`.

## Motivation

Live repro from a Claude Code session: user asked for source of `filterM`.
Plugin's skill invoked `hypha lookup filterM --select sig,haddock`. Hypha
shelled to `haddock` to regenerate the local Hoogle DB, got ENOENT on the
spawn (sandbox hides `~/.ghcup/bin`), and the CLI bubbled this up as:

```
NETWORK_ERROR: haddock: readCreateProcessWithExitCode:
posix_spawnp: does not exist (No such file or directory)
```

Two failures compounded:

1. **Misclassification.** ENOENT on a child process is not a network
   error. The agent had no way to know the local Hoogle tier just lacked
   a tool; it gave up on `hypha` and hallucinated source from memory.
2. **Cascade aborted on tier failure.** `hypha lookup` was designed as a
   cache → local Hoogle → remote Hoogle cascade. A `TOOL_MISSING` at the
   local Hoogle tier should not poison the cascade — remote Hoogle could
   have answered `filterM` immediately.

## Scope

### F1 — Classify `TOOL_MISSING`

- New `HyphaError` constructor `ToolMissing { tool :: !Text, attemptedPath :: !(Maybe FilePath) }`.
- New JSON `code = "TOOL_MISSING"`, new exit code (propose `5`).
- Update the exit-code table in README.
- Catch `IOError` with `isDoesNotExistError` (or `ioe_type == NoSuchThing`)
  in every `readCreateProcessWithExitCode` / `createProcess` call site
  and rewrap as `ToolMissing`. Audit:
    - `Hypha.Hoogle.Local` (haddock invocation)
    - any cabal/ghc-pkg shell-outs
    - any `hoogle` binary spawn (we use the library, but double-check)

### F2 — Cascade continuation on `TOOL_MISSING`

- `Hypha.Command.Lookup`: when the local Hoogle tier returns
  `Left ToolMissing{}`, log it into the `actions` object (so the JSON
  envelope tells the caller "skipped local tier: haddock missing") and
  fall through to the remote Hoogle tier instead of short-circuiting.
- Preserve the existing `--offline` behaviour: if offline, a
  `ToolMissing` at the local tier becomes the terminal error (no remote
  fallback to try).

### F2b — `hypha source` / `hypha symbol` fallback

- Today both read cabal-store. Under sandbox they hit ENOENT on store
  paths.
- When the store read fails with ENOENT for a path under a known
  toolchain root (`~/.cabal`, `~/.ghcup`, store path resolved from
  `plan.json`), classify as `ToolMissing { tool = "cabal-store" }` (or a
  dedicated `StoreUnavailable`) and attempt a remote fallback: fetch
  the source/Haddock from Hackage instead.
- Out of scope for this issue if it bloats too much — split into 039b.
  Decide during implementation.

### F5 — Plugin-side sandbox guidance

- Add `.claude-plugin/settings.json` (or a documented snippet) listing
  the read-allow paths Claude Code's sandbox needs to grant for hypha to
  use its local-fast path:
    - `~/.ghcup/**`
    - `~/.cabal/store/**`
    - `~/.cache/cabal/**`
- If Claude Code's plugin spec does not let a plugin widen the FS
  sandbox unilaterally (it shouldn't — security boundary belongs to the
  user), ship the snippet as documentation only and reference it from
  the plugin install section of the README.

### F6 — README troubleshooting

- Add a "Claude Code sandbox" subsection under Troubleshooting / FAQ
  (create if absent). Symptoms (`TOOL_MISSING` on haddock/cabal/ghc,
  empty `hypha source` results), cause, fix (the allowlist).

## Acceptance Criteria

- [ ] `hypha lookup <stdlib-symbol>` succeeds via remote Hoogle when
      `haddock` is unavailable on PATH; output JSON has the cascade
      hint in `actions`.
- [ ] Exit code `5` reserved for `TOOL_MISSING` and documented.
- [ ] `cabal build all && cabal test all` green.
- [ ] At least one unit test for the new error variant + at least one
      property/unit test that the lookup cascade continues past a
      simulated `ToolMissing` at the local tier.
- [ ] `.claude-plugin/settings.json` (or documented snippet) committed.
- [ ] README updated: exit-code table, sandbox troubleshooting,
      plugin-install note pointing at the snippet.
- [ ] CHANGELOG entry under `[0.2.0]`.

## Out of Scope

- Auto-detecting the sandbox state and adapting behaviour (we just
  classify errors and cascade; user controls their sandbox).
- Per-subcommand MCP tools.
- F2b may be split into a follow-up issue if it grows.
