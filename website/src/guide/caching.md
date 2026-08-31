# Caching

`hypha` caches everything network-shaped — and as much of the plan-shaped
derived state as possible — under `$XDG_CACHE_HOME/hypha/` (defaults to
`~/.cache/hypha/`, overridable with [`--cache-dir`](flags.md)).

| Cache | Layout | Freshness |
|-------|--------|-----------|
| Search index | `hypha.db` (SQLite, WAL) | Keyed on `(component, version, unit-id)` and on a row-format generation (`index_format`, currently `6`), shared across every project on the host |
| Hackage HTTP responses | `hackage/<sha256>.json` | ETag + `If-Modified-Since` revalidation; 15 min TTL per entry |
| Source tarballs | `source/<pkg>-<ver>/` | Immutable once extracted |
| Haddock HTML | `haddock/<pkg>-<ver>/` | Built on demand, reused across runs |
| Hoogle DB | `<projectRoot>/.hypha/hoogle.hoo` (with `.hypha/hoogle-stamp` sibling) | Rebuilt when `plan.json` changes |
| Remote-Hoogle query bodies | `kv` table of `~/.cache/hypha/hypha.db` | No TTL: every successful remote lookup is kept and served even under `--offline`; cleared only by deleting `~/.cache/hypha` (negative-caching/TTL policy tracked in issue #40) |

The fallback chain is automatic for network reads:
**local HTTP cache → build plan → cabal store → Hackage**.

## What "the same package" means

The index is shared across every project on the host, so it has to be
precise about when two projects are talking about the same thing. Sharing
is keyed on cabal's **unit-id** — its hash of the compiler, the resolved
dependency versions and the flag assignment — not on the package version
alone.

That distinction is not pedantry. A package's own source decides what it
exports by asking about its dependencies:

```haskell
#if MIN_VERSION_text(2,0,0)
import Data.Text.Internal.Encoding.Utf8 (utf8LengthByLeader)
#else
import Data.Text.Internal.Encoding.Utf16 (chr2)
#endif
```

`attoparsec-0.14.4` built against `text-2.1` and the same version built
against `text-1.2.5` are different row sets, and 18 of its modules gate on
`MIN_VERSION_*` like this. So:

- Two projects that resolve a package **identically** share the indexed
  rows: the second one starts warm.
- Two projects that resolve it **differently** each keep their own rows,
  and each sees only its own. Neither re-indexes because of the other.
- Up to three configurations of one `(component, version)` are kept; the
  least recently indexed is dropped beyond that. A dropped configuration
  costs one re-index, never a wrong answer.

A `--package-override PKG=VER` names a version your plan does not build,
so cabal never resolved a unit-id for it; those reads match on the version
instead.

## Project-local cache layout

| Path | Purpose |
|------|---------|
| `~/.cache/hypha/hypha.db` | Global SQLite cache: store-package symbol index + remote-Hoogle KV cache |
| `~/.cache/hypha/hoogle-txt/` | Scratch dir for `haddock --hoogle` outputs |
| `~/.cache/hypha/cpp-macros/` | Synthesised `cabal_macros.h` per plan, named by content hash |
| `<project>/.hypha/cache.db` | Project SQLite cache: local + SRP package symbol index |
| `<project>/.hypha/hoogle.hoo` | Project Hoogle DB |
| `<project>/.hypha/hoogle-stamp` | Plan-hash + aggregate-fingerprint stamp |
| `<project>/.hypha/hoogle-input/` | Symlinks / copies of the `.txt` files fed to `hoogle generate` |

## Index format generations

The row format is versioned. When `hypha` opens a `hypha.db` written by an
older build it clears the index outright rather than migrating it: an old
row's module name may have been derived from a file path, its signature
may have been matched by name rather than read at the definition site, or
that signature may have been sliced out of the source span and so carry a
per-argument Haddock comment as though it were part of the type — and
or it may have been read from the wrong side of a
`#if __GLASGOW_HASKELL__` gate, because the macros cabal defines for a
build were not supplied — and none of those defects is detectable row by
row, so the choice is re-index or lie. Generation `6` is the exception
that proves the rule: nothing was wrong with generation `5`'s rows, but
the tables gained the `unit_id` column that keys them, and SQLite cannot
alter a primary key in place.

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
