# Architecture

`hypha` is a single cabal package with one library plus two executables.
Effects are records-of-functions parameterized over `m`, wired into
`ReaderT Env IO` — no effect library, no typeclass effect machinery.

```
hypha .............. CLI entry point
hypha-mcp .......... MCP/stdio shim (Pattern B: shells out to hypha CLI)
library: hypha
  ├── BuildEnv ....... Cabal store + Nix store + composition
  ├── Project ........ plan.json → BuildPlan + per-package components
  ├── Hoogle ......... Per-project DB + freshness via plan-hash
  ├── Hackage ........ JSON API + ETag/Last-Modified cache
  ├── Search ......... Index (typed rows carrying a definition site),
  │                    Indexer (the build pass), Reexport (resolution
  │                    within a component), Exports (the cross-package
  │                    environment), Collapse (one result per definition),
  │                    Cache / PackageCache (SQLite, WAL), Fuzzy (FZF-style
  │                    scorer)
  ├── Source ......... Extensions (language settings from pragmas + cabal),
  │                    Parser / Interface (GHC parse tree → declarations,
  │                    exports, imports), Extract (docs and entries),
  │                    Locate (definition sites)
  ├── Output ......... Compact/full YAML (default) + JSON envelope, --select
  └── Server ......... HTMX-driven doc browser with command-palette UX
```

For full details see the design specs in the repository, under
[`docs/superpowers/`](https://gitlab.well-typed.com/well-typed/hypha/-/tree/main/docs/superpowers).

## Development

```bash
git clone https://gitlab.well-typed.com/well-typed/hypha.git
cd hypha
cabal build all
cabal test all
```

Tests use `falsify` for property testing, `tasty-golden` for output
regression testing, and `tasty-hunit` for specific edge cases.
