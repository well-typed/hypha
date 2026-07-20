# Quick Start

## 1. Materialise a build plan

`hypha` reads `dist-newstyle/cache/plan.json` to learn which versions your
project builds against. Generate it with a dry-run build:

```bash
cd /path/to/your-cabal-project
cabal build --dry-run        # writes dist-newstyle/cache/plan.json
```

## 2. Query a symbol — compact YAML

```bash
hypha symbol async/Control.Concurrent.Async/concurrently
```

```yaml
result:
  name: concurrently
  package: async
  version: '2.2.5'
  module: Control.Concurrent.Async
  signature: IO a -> IO b -> IO (a, b)
actions:
  view_source: hypha source async/Control.Concurrent.Async/concurrently
  module_index: hypha module async/Control.Concurrent.Async
```

YAML is the **default** output: compact, readable in a terminal, and cheap
for an agent to parse. The top level is just `result:` (the answer) and
`actions:` (suggested follow-up commands).

<p align="center">
  <img src="../images/placeholder.svg" width="70%" alt="hypha symbol — compact YAML output" />
</p>
<!-- SCREENSHOT: replace with images/cli-yaml.png. See images/CAPTURE-LIST.md. -->

## 3. Project only the fields you need

```bash
hypha symbol async/Control.Concurrent.Async/concurrently --select signature,haddock
```

`--select` post-filters the output to the listed fields; `--full` opts into
every field. See **[Global Flags](../guide/flags.md)**.

## 4. JSON for machine pipelines

When a downstream tool wants JSON, opt in with `--json`:

```bash
hypha symbol async/Control.Concurrent.Async/concurrently --json
```

## Next steps

- Learn the **[Identifier Syntax](../guide/identifiers.md)** (`pkg`,
  `pkg-version`, module, symbol).
- Browse all **[Subcommands](../guide/subcommands.md)**.
- Use **[`hypha lookup`](../guide/lookup.md)** when you don't know which
  package provides a name or type.
