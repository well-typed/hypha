# Exit Codes

Every failure hypha itself reports is classifiable from the process exit
status alone — no need to parse error text. Success envelopes exit `0`;
failures carry a stable `code` in the envelope *and* a typed exit status:

| Code | Meaning |
|------|---------|
| `0` | Success |
| `1` | Argument parsing failed before hypha ran — unknown subcommand, unknown flag, missing required argument. See below. |
| `2` | User error (bad args, malformed path, non-loopback bind) |
| `3` | Not found (symbol/package absent across plan → store → Hackage) |
| `4` | Network error (offline cache miss, HTTP 429/503, transport failure) |
| `5` | Cache / on-disk corruption |
| `7` | Environment error (no `plan.json`, unreachable store) |
| `8` | Tool missing — a required external binary (`haddock`, `cabal`, `ghc`) is not on `$PATH` |
| `9` | Internal error — an exception escaped hypha's own error handling. A bug; the envelope carries `INTERNAL_ERROR`. |

## `1` versus `2`

`1` comes from the argument parser, before any command runs, and so is the
one code with **no envelope on stdout** — the usage message goes to stderr
instead. `2` is hypha's own validation of an argument it did parse:

```console
$ hypha nosuchcommand      # unknown subcommand      -> 1, usage on stderr
$ hypha --nosuchflag       # unknown flag            -> 1, usage on stderr
$ hypha package            # missing required arg    -> 1, usage on stderr
$ hypha symbol not-a-path  # malformed identifier    -> 2, error envelope
```

An agent that branches on the envelope should treat `1` as "I called hypha
wrong" and re-read `--help`, not as a failure of the query.

If you are seeing `8` (`TOOL_MISSING`) unexpectedly, you are probably
running under a sandbox that hides the toolchain — see
[Troubleshooting](../troubleshooting.md).
