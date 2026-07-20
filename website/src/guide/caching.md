# Caching

`hypha` caches everything network-shaped — and as much of the plan-shaped
derived state as possible — under `$XDG_CACHE_HOME/hypha/` (defaults to
`~/.cache/hypha/`, overridable with [`--cache-dir`](flags.md)).

| Cache | Layout | Freshness |
|-------|--------|-----------|
| Search index | `hypha.db` (SQLite, WAL) | Keyed on `(pkg, version)`, shared across every project on the host |
| Hackage HTTP responses | `hackage/<sha256>.json` | ETag + `If-Modified-Since` revalidation; 15 min TTL per entry |
| Source tarballs | `source/<pkg>-<ver>/` | Immutable once extracted |
| Haddock HTML | `haddock/<pkg>-<ver>/` | Built on demand, reused across runs |
| Hoogle DB | `<projectRoot>/.hypha/hoogle.hoo` (with `.hypha/plan-hash` sibling) | Rebuilt when `plan.json` changes |

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

## Forcing a rebuild

No `invalidate` subcommand is shipped (agents would footgun). To force a
rebuild, delete the cache directory:

```bash
rm -rf <project>/.hypha     # project-only
rm -rf ~/.cache/hypha       # global
```
