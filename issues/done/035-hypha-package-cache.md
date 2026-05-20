# 035: Split package cache into global + project DBs behind `HyphaPackageCache`

## Motivation

Today every indexed package goes into a single SQLite DB at
`$XDG_CACHE_HOME/hypha/hypha.db`, keyed by `(pkg, version)`.  Local
packages (`packages:` entries, `source-repository-package` checkouts)
collide with store packages of the same name/version and pollute the
global cache.

## Design

Introduce an opaque wrapper:

```haskell
data HyphaPackageCache = HyphaPackageCache
  { hpcGlobal  :: !IndexCache
  , hpcProject :: !(Maybe IndexCache)   -- Nothing when no project root
  }

data CacheOrigin = OriginGlobal | OriginProject
```

- Global DB stays at `$XDG_CACHE_HOME/hypha/hypha.db` — store packages.
- Project DB at `$PROJECT_ROOT/.hypha/cache.db` — local + SRP packages.
- Reads merge with **project shadows global** (project hit wins).
- Writes are routed by `CacheOrigin`, derived from `puIsLocal`.

Parallel reads are deferred: a single SQLite SELECT is sub-millisecond
and the `concurrently` overhead is likely net negative.  We can revisit
under bench.

## Tasks

- [ ] New module `Hypha/Search/PackageCache.hs` with the wrapper, an
      `openPackageCache :: Maybe ProjectRoot -> IO HyphaPackageCache`,
      and `withPackageCache` bracket.
- [ ] Adapt `hydrateFromCache` + `buildAndCacheIndex` in
      `Hypha/Command/Server.hs` to take `HyphaPackageCache` and pick
      origin per planned unit (`puIsLocal`).
- [ ] Thread `ProjectRoot` from `Cli/Run.hs` into `Server.runServer`.
- [ ] Unit test: project entry shadows global entry for same
      `(pkg, version)`.
- [ ] `cabal build all && cabal test all` passes.

## Out of scope

- Schema changes (no `source_id` column — separation is by DB file).
- Parallel reads.
- CLI surface to wipe the project cache (`.hypha/` is git-ignorable).
