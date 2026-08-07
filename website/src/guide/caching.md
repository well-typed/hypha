# Caching

`hypha` caches everything network-shaped — and as much of the plan-shaped
derived state as possible — under `$XDG_CACHE_HOME/hypha/` (defaults to
`~/.cache/hypha/`, overridable with [`--cache-dir`](flags.md)).

| Cache | Layout | Freshness |
|-------|--------|-----------|
| Search index | `hypha.db` (SQLite, WAL) | Keyed on `(pkg, version)` and on a row-format generation (`index_format`, currently `3`), shared across every project on the host |
| Hackage HTTP responses | `hackage/<sha256>.json` | ETag + `If-Modified-Since` revalidation; 15 min TTL per entry |
| Source tarballs | `source/<pkg>-<ver>/` | Immutable once extracted |
| Haddock HTML | `haddock/<pkg>-<ver>/` | Built on demand, reused across runs |
| Hoogle DB | `<projectRoot>/.hypha/hoogle.hoo` (with `.hypha/hoogle-stamp` sibling) | Rebuilt when `plan.json` changes |

The fallback chain is automatic for network reads:
**local HTTP cache → build plan → cabal store → Hackage**.

## Project-local cache layout

| Path | Purpose |
|------|---------|
| `~/.cache/hypha/hypha.db` | Global SQLite cache: store-package symbol index + remote-Hoogle KV cache |
| `~/.cache/hypha/hoogle-txt/` | Scratch dir for `haddock --hoogle` outputs |
| `<project>/.hypha/cache.db` | Project SQLite cache: local + SRP package symbol index |
| `<project>/.hypha/hoogle.hoo` | Project Hoogle DB |
| `<project>/.hypha/hoogle-stamp` | Plan-hash + aggregate-fingerprint stamp |
| `<project>/.hypha/hoogle-input/` | Symlinks / copies of the `.txt` files fed to `hoogle generate` |

## Index format generations

The row format is versioned. When `hypha` opens a `hypha.db` written by an
older build it clears the index outright rather than migrating it: an old
row's module name may have been derived from a file path, and its
signature may have been matched by name rather than read at the
definition site — and neither defect is detectable row by row, so the
choice is re-index or lie.

Expect one full background re-index the first time you run a new `hypha`
version (a few minutes for a large plan). While it runs, `hypha server`'s
search is briefly empty and `hypha lookup` falls through to Hoogle.
Nothing needs to be deleted by hand.

## Forcing a rebuild

No `invalidate` subcommand is shipped (agents would footgun). To force a
rebuild, delete the cache directory:

```bash
rm -rf <project>/.hypha     # project-only
rm -rf ~/.cache/hypha       # global
```
