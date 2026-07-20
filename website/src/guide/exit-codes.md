# Exit Codes

Every failure is classifiable from the process exit status alone — no need
to parse error text. Success envelopes exit `0`; failures carry a stable
`code` in the envelope *and* a typed exit status:

| Code | Meaning |
|------|---------|
| `0` | Success |
| `2` | User error (bad args, malformed path, non-loopback bind) |
| `3` | Not found (symbol/package absent across plan → store → Hackage) |
| `4` | Network error (offline cache miss, HTTP 429/503, transport failure) |
| `5` | Cache / on-disk corruption |
| `7` | Environment error (no `plan.json`, unreachable store) |
| `8` | Tool missing — a required external binary (`haddock`, `cabal`, `ghc`) is not on `$PATH` |

If you are seeing `8` (`TOOL_MISSING`) unexpectedly, you are probably
running under a sandbox that hides the toolchain — see
[Troubleshooting](../troubleshooting.md).
