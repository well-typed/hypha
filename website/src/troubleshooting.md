# Troubleshooting

## Non-UTF-8 locale

GHC derives every `Handle`'s encoding from the process locale. Under `C` or
`POSIX` — the default in bare containers, where `LANG` is simply unset — that
encoding is ASCII, and any non-ASCII character becomes a hard `IOError`. This
bites when *building* hypha; hypha itself is immune (see below).

**Building hypha fails in `ghc-lib-parser`:**

```
happy: compiler/GHC/Parser.y: hGetContents: invalid argument (cannot decode byte sequence starting from 226)
```

`happy` cannot *decode* the grammar. Byte 226 is `0xE2`, the first byte of
`∷` (U+2237), which appears in the GHC 9.12 series' `Parser.y` via
`EpUniToken "::" "∷"`. Nothing is wrong with the download — the file is valid
UTF-8 that an ASCII decoder refuses to read.

**Fix.** Give the *build* a UTF-8 locale:

```bash
export LANG=C.UTF-8
```

`C.UTF-8` is available in every modern glibc image and needs no `locale-gen`.
Prefer setting `LANG` over `LC_ALL`, so you don't clobber your other locale
categories. hypha's own CI and its Nix devshell both set it for exactly this
reason.

**Running hypha needs no locale setup.** Up to 0.2.0 it inherited the same
problem and died on its own output:

```
hypha: <stdout>: commitBuffer: invalid argument (cannot encode character '\8212')
```

Since then both binaries pin UTF-8 on their handles, on the filesystem
encoding, and on every handle they open, before printing anything
([issue #9](https://github.com/well-typed/hypha/issues/9)). The
locale was never the right authority: Haskell sources, `.cabal` files, JSON and
Haddock HTML are all UTF-8 by their own specs. The one thing that now fails
loudly instead of quietly producing mojibake is a genuinely Latin-1 `.hs` or
`.cabal` file.

## Running under Claude Code's sandbox

Claude Code runs `Bash` commands inside a filesystem sandbox that may hide
your toolchain directories from the spawned process. Symptoms:

- `hypha lookup` reports `TOOL_MISSING` for `haddock` (or `cabal`, `ghc`)
  even though those binaries work in your terminal.
- `hypha source` returns empty results that you can reproduce manually.

**Cause:** `~/.ghcup`, `~/.cabal/store`, and similar paths are not in the
sandbox's read allowlist. From the sandboxed process's view they return
`ENOENT`, so PATH-resolved binaries appear missing and cabal-store reads
find nothing.

**Fix:** widen the sandbox's read allowlist in your Claude Code
`settings.json`. The exact key depends on your Claude Code version, but the
directories `hypha` needs to see are typically:

- `~/.ghcup/**` — required if hypha needs to spawn `haddock`
- `~/.cabal/store/**` — required for `hypha source` / `hypha symbol`
- `~/.cache/cabal/**` — speeds up the Hackage HTTP cache
- `/etc/ssl/certs/**` (or `$SSL_CERT_FILE`) — required for TLS to
  hackage.haskell.org / hoogle.haskell.org; without it the remote tier
  fails with `HandshakeFailed ... certificate has unknown CA`

`hypha` is designed to degrade gracefully here: the `lookup` cascade falls
through to remote Hoogle when the local Hoogle tier cannot run `haddock`,
and reports `TOOL_MISSING` ([exit `8`](guide/exit-codes.md)) rather than the
misleading `NETWORK_ERROR`. Widening the sandbox just restores the
local-fast path.

## Running under other harnesses (pi, sbox, …)

The same class of failures hits any sandboxed harness driving hypha, not
just Claude Code. If you wrap `hypha` (or an agent that calls it) in `sbox`,
`bwrap`, `firejail`, or similar, expose the same paths as read-only mounts.
A minimal recipe for `sbox`:

```bash
sbox \
  --rw  /path/to/your-project \
  --ro  ~/.ghcup \
  --ro  ~/.cabal/store \
  --ro  /etc/ssl/certs \
  -- <your-agent> --provider … --model …
```

Plus whatever paths the harness itself needs (e.g. `~/.pi` for the `pi`
harness's model config). Without these, hypha sees `TOOL_MISSING` for the
toolchain and TLS failures for the remote tier.

## Search is empty right after upgrading hypha

Expected, once. The search index is versioned, and a format change clears
it on first open — see [Index format
generations](guide/caching.md#index-format-generations). Start `hypha
server` and let the background index finish; `hypha lookup` keeps working
in the meantime by falling through to Hoogle.

## A symbol I know exists is not in the index

One known gap, reported on stderr as it happens:

- **A few modules still will not parse.** CPP itself is not the problem:
  `hypha` runs the preprocessor with the macros your plan implies
  (`__GLASGOW_HASKELL__`, `MIN_VERSION_<pkg>`) and with your compiler's own
  header directory on the include path (`MachDeps.h`, `ghcplatform.h`), and
  it resolves each package's `os()` and `arch()` stanzas for the platform
  your plan was solved for — so `#if`-guarded code is read from the branch
  your compiler would actually compile, and a module your platform never
  builds is not read at all. What remains are modules that do not parse
  even then: a Template Haskell quotation the parser cannot take
  standalone, or an `#include` of a header `configure` generates at build
  time (`HsBaseConfig.h`), which is not in the released tarball. Measured
  on this repo's 252-unit plan: **25 modules across 13 packages**, and each
  one is named on stderr with GHC's own message.

  A module that contributes nothing takes with it whatever it re-exported,
  and that covers class methods and constructors too — a member presented
  only by a skipped module is unreachable, while one that is *also*
  re-exported by a module that parses is found. `mempty` is a current
  example: it resolves to `ghc-internal` but has no `base` row. Which
  symbols fall on which side shifts as packages and GHC change, so treat
  any specific example as a snapshot — the stderr list is the authority
  for your plan.
