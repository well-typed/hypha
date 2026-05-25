# Hypha `--prebuild` Gating (Phase 1)

> **Status:** design — ready for implementation plan.
> **Date:** 2026-05-25.
> **Author:** Alfredo Di Napoli (with Claude Opus 4.7).

## 1. Context

`hypha server --prebuild` walks every package in the loaded `BuildPlan`
and calls `ensureHaddockFor` on it.  That function looks in three
places for pre-rendered Haddock HTML:

1. The hypha cache (`~/.cache/hypha/haddock/<pid>/index.html`).
2. The build plan's `puDistDir/doc/html/<pkg>/index.html` (set by
   `cabal build --enable-documentation`).
3. The cabal store's `<store>/<pkg-ver-hash>/share/doc/<pkg-ver>/index.html`
   (populated only when the user installs with documentation on).

When `hypha server` runs **outside** a cabal project, the recently
introduced `enrichPlanFromStore` synthesises a plan from
`BuildEnv.discoverInstalledPackages` — typically several hundred
packages.  Almost none of those carry a `puDistDir`, and almost none
of the store entries ship `share/doc` HTML, so `--prebuild` produces
nothing useful and emits hundreds of `note: prebuild produced no
haddock for <pkg-ver>` lines.

The flag's `--help` description ("Pre-render Haddocks for every
package in the build plan") implies a project-scoped operation.
Outside a project the operation is meaningless: there is no plan and
no realistic path to render docs for every store entry on the fly.

## 2. Goal (Phase 1)

Make `--prebuild` semantically honest:

- When a cabal project is present, behaviour is unchanged: walk the
  plan, probe the three locations, emit a per-package note when no
  HTML is found.  These notes are informational and expected, given
  the user opted in.
- When no project is present, `--prebuild` is a no-op.  A single
  `warning:` line on stderr explains why; the server then starts
  normally and serves the synthesised store plan without docs.

Real on-demand Haddock generation for project dependencies is the
subject of Phase 2 (a separate spec).  Phase 1 is the smallest
possible step that removes the misleading noise and pins the
semantic for Phase 2 to build on.

## 3. Non-goals

- Generating Haddock HTML for any package.  No `haddock` invocations
  added by this spec.
- Caching, parallelism, or interface-file plumbing for cross-package
  links.
- Changing `ensureHaddockFor`, `prebuildAll`, or the layout of
  `~/.cache/hypha/haddock`.
- Changing behaviour for any subcommand other than `server`.

## 4. Design

### 4.1 Gating point

The gate lives in `Hypha.Cli.Run.runServerInteractive` — the same
function that already calls `loadProjectAndPlan` and knows whether
`mRoot :: Maybe ProjectRoot` resolved.  `Hypha.Command.Server.runServer`
and `prebuildAll` keep their current contract: "if I am asked to
prebuild, prebuild."  Layering the gate at the dispatcher boundary
keeps the inner command ignorant of project-root semantics.

### 4.2 Helper

Extract the gating logic into a small pure helper for testability:

```haskell
-- | Decide whether prebuild should fire, given the user's request
-- and whether a cabal project was discovered.  Returns the
-- effective flag and an optional warning to surface on stderr.
gatePrebuild :: Bool -> Maybe ProjectRoot -> (Bool, Maybe Text)
gatePrebuild False _        = (False, Nothing)
gatePrebuild True  (Just _) = (True,  Nothing)
gatePrebuild True  Nothing  =
  ( False
  , Just "--prebuild requires an active cabal project; ignoring"
  )
```

The helper has a tabular shape and is trivially unit-testable
(four cases, three of which collapse to "no change").

### 4.3 Call-site change

In `runServerInteractive`, after `loadProjectAndPlan`:

```haskell
let (prebuildEffective, mWarn) = gatePrebuild prebuild mRoot
for_ mWarn $ \w -> liftIO (hPutStrLn stderr ("warning: " <> Text.unpack w))
let opts = Server.ServerOpts ba prebuildEffective
                              (fromIntegral (max 1 jobs))
```

Everything downstream stays unchanged.  When `prebuildEffective` is
`False`, `Server.runServer`'s existing `when (soPrebuild opts) ...`
skips the walk; the synthetic store plan is still used to populate
the package list and the server still listens.

### 4.4 Help text

`Hypha.Cli.Parser.serverParser` updates the `--prebuild` flag help
from

> "Pre-render Haddocks for every package in the build plan"

to

> "Pre-render Haddocks for project dependencies (requires an active
> cabal project; ignored otherwise)"

## 5. Testing

A new unit test module (or a section of `Unit.Server`) covers
`gatePrebuild` exhaustively:

| `prebuild` | `mRoot`   | Expected flag | Expected warning |
|-----------:|-----------|--------------:|------------------|
| `False`    | `Nothing` | `False`       | `Nothing`        |
| `False`    | `Just _`  | `False`       | `Nothing`        |
| `True`     | `Just _`  | `True`        | `Nothing`        |
| `True`     | `Nothing` | `False`       | `Just "--prebuild requires an active cabal project; ignoring"` |

No integration test is required because the change is mechanical
and the unit-tested helper is the only logic.  Existing
`Unit.Server` parseBind tests remain green.

## 6. Backwards compatibility

- Project-rooted invocations: zero behavioural change.
- Project-less invocations with `--prebuild`: previously walked the
  store plan and printed hundreds of notes; now prints one warning
  and starts the server faster.  No JSON envelope changes — the
  warning is plain stderr text.
- Exit codes unchanged in every scenario.

## 7. Future work (Phase 2 preview)

Phase 2 will introduce real Haddock HTML generation for project
dependencies, in parallel, caching under
`~/.cache/hypha/haddock/<pkg-ver>/`.  Open questions for Phase 2:

- Use `cabal haddock --enable-documentation <pkg>` (heavier, fully
  correct, slow) or direct `haddock --html` against unpacked
  sources (lean, requires us to wire interface files)?
- Cross-package interface linking via `--read-interface`?
- How to surface progress in `hypha server`'s UI rather than only
  on stderr?

Phase 1 deliberately gates the flag now so that, when Phase 2 lands,
the existing `--prebuild` users get the new behaviour transparently
inside a project, while project-less users keep the no-op.
