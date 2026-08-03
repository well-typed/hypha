# Ask the compiler where an export comes from

**Status:** done
**Type:** bug
**Found by:** `base/Control.Concurrent#isCurrentThreadBound` rendering as
"re-exported, origin unresolved" in `hypha server` while Hackage documents
it fine.

## Problem

`Reexport.outsideFor` resolved a re-export from syntax alone and kept the
**first** import that could plausibly supply the name, in source order.
`Control.Concurrent` starts with `import Prelude`, an open import that
"plausibly supplies" everything, so `isCurrentThreadBound` was attributed
to `Prelude`, `lookupExport` found nothing there, and the row was dropped.

An export list says *which* names a module exports and never *whence*.
Only a renamer knows — which is why Haddock, running inside GHC after the
renamer, gets it right on Hackage and a parse-tree pass cannot.

Measured on `base` alone (260 modules, guesses joined against the index):

| | count |
|---|---|
| exports resolving outside the component | 4343 |
| first-import guess correct | 2661 (61%) |
| recoverable by probing every import | **625 (14%)** |
| no import supplies it in the index at all | 1057 (class methods → 043) |

## Fix

Two halves, both shipped:

1. **Probe, do not guess.** `DefinedOutside` carries a
   `NonEmpty ModulePath` of ranked candidates (explicit imports first,
   `Prelude` last); the indexer and `Hypha.Source.Locate` try each until
   one actually supplies — or declares — the name. `NoSupplier` replaces
   the old "defined outside, in the asking module" sentinel.
2. **Ask GHC for what syntax cannot see.** `Hypha.Source.Origins` reads
   the `exports:` section of `ghc --show-iface` on the already-built `.hi`
   file, where every export carries its fully-qualified origin, and the
   indexer uses it to repair what the syntactic pass left unresolved. No
   Haddock build: rendering documentation for every dependency would
   answer the same question for minutes-to-hours of build time and
   gigabytes of output, and the interface files are already on disk.

Notes on the second half, all of which the review pushed on:

- The toolchain is **selected**, not merely checked: `ghc-<version>` from
  the plan's `CompilerId` first, bare `ghc` second, each verified with
  `--numeric-version`, and `ghc-pkg-<version>` paired with it so a skewed
  pair cannot be assembled. Interface files are patch-exact — `ghc-9.12.4`
  on a 9.10.3 `.hi` exits 1 — so equality is the right predicate. Without
  this the feature was inert under this repo's own multi-GHC workflow.
- A missing toolchain or an unlistable store root is reported **once**, at
  construction, not once per module (`base` alone would have printed it a
  hundred times), and the oracle is then `Nothing` rather than a thing
  that refuses.
- Dumps are **streamed** and reading stops at the end of the exports
  section: `ghc-internal:GHC.Internal.ClosureTypes` prints 12 MB, and the
  server process holding an index does not need it as a `String`.
- Local packages use the build tree the plan already names (`puDistDir`),
  since a project's own packages have no store entry for `ghc-pkg`.
- One name can have **two** origins (`GHC` exports `XFixitySig` from both
  `…Syntax.Binds` and `…Syntax.Extension` — 58 such cases in a 400-dump
  sample), so `ModuleOrigins` keeps them all and the indexer probes them
  in order, same as everywhere else.
- "No `exports:` section" and "unreadable store root" are typed errors,
  never a silently empty module or an empty database list.

## Verification

- `cabal build all && cabal test all` → 358 tests pass.
- `Unit.SourceOrigins` runs the **real** oracle against a real `.hi` on
  disk, not only the stub, since this repo has shipped browsing releases
  that were green and broken in the IO.
- End to end: after a rebuild, `pkg_index` holds
  `base|Control.Concurrent|isCurrentThreadBound` with `def_mod =
  GHC.Internal.Conc.Bound`, and the server log carries no per-module
  origin diagnostics.

## Deferred

See `issues/todo/047-origin-oracle-follow-ups.md`.
