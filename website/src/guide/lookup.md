# Looking Up Symbols (`hypha lookup`)

`hypha lookup` is the single entry point for the question *"which
package/module provides this?"* It runs a three-tier short-circuit cascade
and returns at the first hit:

1. **`PackageCache`** (SQLite): exact-name + qualified-name lookup — e.g.
   both `lookup` and `Data.Map.lookup`. Indexes top-level declarations,
   class methods, data constructors and record fields alike. What it does
   not cover is a module that will not parse even after preprocessing:
   such a module contributes no rows, so a symbol only that module
   presents falls through to the tiers below — see
   [Troubleshooting](../troubleshooting.md). Tier 1 is also empty for one
   background pass after an index-format upgrade — see
   [Caching](caching.md#index-format-generations).

   **This tier is pinned to your build plan.** The SQLite cache is keyed on
   `(package, version)` and shared by every project on the host, so it
   accumulates versions no single project builds against. Tier 1 answers
   only at the versions your `plan.json` pins — or that
   `--package-override` sets — and every provider reports the version it
   came from. Outside a cabal project there is no plan to pin to and the
   whole cache answers instead; hypha says so on stderr.
2. **Local Hoogle DB** at `<project>/.hypha/hoogle.hoo`: built lazily from
   scavenged store `*.txt` files plus on-demand `haddock --hoogle` for
   local packages. Handles type-signature queries such as
   `a -> Maybe a`.
3. **Remote Hoogle** at `hoogle.haskell.org`: HTTP fallback. Cached in the
   global `kv` table; under `--offline` a cached answer is still served, only
   the network call is skipped. Its request timeout is
   10s, raisable with `--hoogle-timeout SECONDS`.

   **This is the only tier that reaches beyond your build plan**, and it is
   how you find a package you do not yet depend on. The `tier` field on
   each provider is what tells you how far an answer had to reach: `cache`
   and `local-hoogle` are your project, `remote-hoogle` is all of Hackage.

```bash
hypha lookup lookup
hypha lookup 'a -> Maybe a'
```

`hypha lookup` always emits a structured envelope. Failures carry a `code`
(`NOT_FOUND`, `HOOGLE_OFFLINE`, `HOOGLE_REMOTE_ERROR`) and an `actions` map
suggesting how to retry. See [Exit Codes](exit-codes.md) for how those map
to process exit status.
