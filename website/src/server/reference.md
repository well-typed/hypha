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
| `/search?q=...` | HTMX results fragment (fuzzy ranked) |
| `/progress` | HTMX progress-bar fragment (self-polling) |
| `/pkg/<pkg>` or `/pkg/<pkg>:<sublib>` | Package / sublib overview |
| `/pkg/<pkg>/<Mod>` | Module page |
| `/pkg/<pkg>/<Mod>/<sym>` | Symbol card |
| `/source/<pkg>/<Mod>` | Highlighted source |
| `/haddock/<pkg>-<ver>/...` | Rewritten Haddock HTML |
| `/healthz` | `ok` (plain text) |
