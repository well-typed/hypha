# Troubleshooting

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
