# Endpoints & Flags

## Command-line flags

| Flag | Default | Purpose |
|------|---------|---------|
| `--port N` | `4287` | Loopback port to bind |
| `--bind HOST:PORT` | `127.0.0.1:<port>` | Explicit loopback bind (`localhost`, `127.0.0.1`, or `::1`) |
| `--prebuild` | off | Render Haddocks for every plan package up front |
| `--prebuild-jobs N` | `4` | Maximum concurrent prebuild workers |

Non-loopback binds (e.g. `0.0.0.0:4287`) are refused with
[exit code `2`](../guide/exit-codes.md). There is no remote-access flag —
sharing is out of scope on purpose.

## Endpoints

| Path | Returns |
|------|---------|
| `/` | HTML shell with sidebar + search |
| `/search?q=...[&pkg=<component>]` | HTMX results fragment: fuzzy ranked, and collapsed to one hit per definition. `pkg` scopes to a single component and is applied *before* the fold, so a symbol two packages present still appears under either |
| `/progress` | HTMX progress-bar fragment (self-polling) |
| `/pkg/<pkg>` or `/pkg/<pkg>:<sublib>` | Package / sublib overview |
| `/pkg/<pkg>/<Mod>` | Module page, re-exported entries included |
| `/pkg/<pkg>/<Mod>/<sym>` | Symbol card |
| `/source/<pkg>/<Mod>` | Highlighted source |
| `/haddock/<pkg>-<ver>/...` | Rewritten Haddock HTML |
| `/healthz` | `ok` (plain text) |

Links out of a module page or a symbol card may name a different component
from the one in the URL: a re-exported symbol is documented where it is
*defined*, and that can be another package (`base`'s `Data.Traversable`
resolves into `ghc-internal`).
