# Global Flags

These apply to every subcommand and are given before or after the command.

| Flag | Description |
|------|-------------|
| `--project-dir DIR` | Override project root |
| `--package-override PKG=VER` | Replace a plan entry (repeatable) |
| `--offline` | Completely disable network access; work only with local data (skips the remote Hoogle tier in `lookup`) |
| `--json` | Emit the JSON envelope instead of YAML (the default) |
| `--pretty-json` | Indent JSON output |
| `--full` | Include all fields (default: compact) |
| `--select f1,f2,...` | Post-filter output to the listed fields |
| `--cache-dir DIR` | Override the cache root (default: XDG, `~/.cache/hypha`) |
| `--quiet` / `-q` | Suppress informational output |
| `--verbose` / `-v` | Show debug output |

## Output format

YAML is the default — compact, terminal-readable, and cheap for an agent to
parse. `--json` switches to a JSON envelope for machine pipelines;
`--pretty-json` indents it.

- **`--full`** includes every field the command can produce.
- **`--select`** trims the output to just the fields you name, e.g.
  `--select signature,haddock`. Great for keeping token cost down.

See [Caching](caching.md) for what `--cache-dir` and `--offline` control.
