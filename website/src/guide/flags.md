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
| `--quiet` / `-q` | Suppress informational output; overrides `--verbose`. Does not suppress the indexer's and the browser's diagnostics — a skipped module, an export it could not resolve, a cabal file it could not read — which always go to stderr |
| `--verbose` / `-v` | Show debug output |

## Output format

YAML is the default — compact, terminal-readable, and cheap for an agent to
parse. `--json` switches to a JSON envelope for machine pipelines;
`--pretty-json` indents it.

- **`--full`** includes every field the command can produce.
- **`--select`** trims the output to just the fields you name, e.g.
  `hypha symbol … --select signature,haddock`. Great for keeping token
  cost down. The short spellings `sig` and `haddock` mean the same fields
  (they are aliases for `signature` and `haddock_raw`), so
  `--select sig,haddock` works too.

  It names *top-level result* fields, so which names are valid depends on
  the command: `signature` and `haddock_raw` are `hypha symbol`'s. A name
  no command produces selects nothing — `hypha lookup … --select sig`
  answers with an empty result, because `lookup`'s signatures live one
  level down, inside each entry of `providers`. `lookup`'s own top-level
  fields are `query` and `providers` (plus `tiers_consulted` under
  `--full`), and its default output is already compact.

  You do not have to guess: a name the command cannot answer is reported
  on stderr, along with the ones it can.

  ```console
  $ hypha lookup encode --select sig
  warning: --select names no field of 'lookup': signature; this command
  answers with providers, query (more under --full)
  ```

See [Caching](caching.md) for what `--cache-dir` and `--offline` control.
