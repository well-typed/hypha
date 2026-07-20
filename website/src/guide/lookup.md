# Looking Up Symbols (`hypha lookup`)

`hypha lookup` is the single entry point for the question *"which
package/module provides this?"* It runs a three-tier short-circuit cascade
and returns at the first hit:

1. **`PackageCache`** (SQLite): exact-name + qualified-name lookup — e.g.
   both `lookup` and `Data.Map.lookup`.
2. **Local Hoogle DB** at `<project>/.hypha/hoogle.hoo`: built lazily from
   scavenged store `*.txt` files plus on-demand `haddock --hoogle` for
   local packages. Handles type-signature queries such as
   `a -> Maybe a`.
3. **Remote Hoogle** at `hoogle.haskell.org`: HTTP fallback. Cached in the
   global `kv` table; skipped under `--offline` / `HYPHA_OFFLINE=1`.

```bash
hypha lookup lookup
hypha lookup 'a -> Maybe a'
```

`hypha lookup` always emits a structured envelope. Failures carry a `code`
(`NOT_FOUND`, `HOOGLE_OFFLINE`, `HOOGLE_REMOTE_ERROR`) and an `actions` map
suggesting how to retry. See [Exit Codes](exit-codes.md) for how those map
to process exit status.
