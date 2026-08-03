# Synthesise CPP macros so modules needing preprocessing get indexed

**Status:** todo
**Type:** bug
**Found by:** verification during issue 12 / 043 (index class methods and
constructors)
**Designed in:** `docs/superpowers/specs/2026-07-27-hypha-index-correctness-design.md`
§7, conditional on §1.4's measurement — which has since come back
warranted. Also open in the 07-28 cross-package design, §"still open".

## Problem

`Hypha.Source.Parser` runs `cpphs` when a module says
`{-# LANGUAGE CPP #-}`, but with no symbol table and no include path
(`cpphsOpts`, `Parser.hs`). Macros that only a real GHC invocation
defines are therefore undefined, `#include` resolves against nothing, and
the module fails to parse. It contributes no rows, and every module that
re-exports from it loses exactly what it re-exported.

The 07-27 spec measured 159 such modules in a 283-package plan, all
CPP-shaped:

```
48  parse error on input `#'            (unexpanded directives)
 6  #  error This code isn't being built with GHC
 4  parse error on input `CALLCONV'     (undefined macro)
 5  #s'   4  #s1   5  parse error on input `$'
```

It was deferred as a tail: 159 modules against ~62 000 rows, each one
reported on stderr rather than aborting the index.

**Issue 12 changed that calculus.** Now that a class's methods, a type's
constructors and its record fields are declarations in their own right,
an unparseable module costs its *members* too, not just its top-level
names. `GHC.Internal.Base` needs CPP, so on the current cache:

| symbol | rows in `base` + `ghc-internal` |
|---|---|
| `liftA2` | 0 |
| `pure` | 0 |
| `mappend` | 1 |
| `GHC.Internal.Base` (whole module) | 5 rows |

`liftA2` was named in issue 043's own bug table and is still zero. The
methods that *do* resolve (`fmap`, `mempty`) only do so through a
re-exporter that happens to parse, which is luck rather than design.

## Fix

Implement §7's `CppEnv`:

```haskell
data CppEnv = CppEnv
  { ceIncludeDirs :: ![FilePath]        -- ^ component @include-dirs@
  , ceDefines     :: ![(Text, Text)]
  }
```

- `#include` resolves against the component's own `include-dirs`, so
  `containers.h`, `HsBaseConfig.h` and friends are found.
- `MIN_VERSION_pkg(a,b,c)` synthesised from the build plan's resolved
  dependency versions — the same information cabal uses to generate
  them. Likewise `__GLASGOW_HASKELL__` from the plan's compiler,
  `CALLCONV` and `CURRENT_PACKAGE_KEY`.
- Where cabal has already written the macros, prefer them over
  synthesising: `dist-newstyle/build/<arch>/<ghc>/<pkg>/build/autogen/cabal_macros.h`
  exists for every local component. It is only the *dependency* packages
  (read from the Hackage source cache) that need synthesis.
- cpphs then runs in honest `IO`, which retires the `unsafePerformIO` in
  `Parser.parseDecls` — its only justification today is hiding that `IO`.

`#error This code isn't being built with GHC` disappears on its own once
`__GLASGOW_HASKELL__` is defined.

## Acceptance criteria

- Re-measure the failure count on a full plan index first, so the
  before/after is a number and not a claim. The 159 above is from the
  07-27 measurement and predates several parser changes.
- `GHC.Internal.Base` gets rows for what it declares; `liftA2`, `pure`
  and `mappend` each have a `base` row attributed to it.
- The remaining failures are named and classified on stderr as they are
  today — a module we still cannot preprocess must stay reported, not
  silently empty.
- `unsafePerformIO` is gone from `Hypha.Source.Parser`.
- A fixture pins a module that needs an `#include` from the component's
  include dir and one that needs `MIN_VERSION_*`, both indexed.
- Update the CPP entry in `CHANGELOG.md`'s Known limitations and the
  matching section of `website/src/troubleshooting.md` — both currently
  cite this as the reason `liftA2` and `pure` have no `base` row.

## Notes

Not blocked by anything in flight. Touches `Hypha.Source.Parser`,
`Hypha.Source.Extensions` and whatever hands the parser its per-component
settings; the plan already carries the dependency versions the macros
need.
