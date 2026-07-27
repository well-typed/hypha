<p align="center">
  <img src="logo/hypha.png" width="220" alt="hypha logo" />
</p>

<h1 align="center">hypha</h1>

<p align="center">
  <em>A Haskell-aware code/doc browser tuned for AI agents and humans alike.</em>
</p>

---

AI agents burn tokens fetching Hackage HTML and re-grepping the source tree
just to find a signature or a Haddock paragraph. **`hypha` makes Haskell
knowledge cheap to consume:** every command emits compact, structured
**YAML** by default (`--json` for machine pipelines) — no HTML noise — and
you can trim it further with `--select`.

It is **plan-aware**: `hypha` reads your `dist-newstyle/cache/plan.json`, so
answers reflect the exact versions you build against — including your local
project — and symbols point to the `file:line` where they are actually
defined, even across re-exports and CPP `#ifdef` branches. One library
powers both the CLI and a local doc-browser server, so agents and humans see
the same data.

<p align="center">
  <img src="website/src/images/hero-cli.png" width="46%" alt="hypha CLI — default YAML output" />
  &nbsp;
  <img src="website/src/images/hero-server.png" width="46%" alt="hypha doc-browser server" />
</p>
<!-- Placeholders until captured; overwrite website/src/images/hero-cli.png
     and hero-server.png in place — see website/src/images/CAPTURE-LIST.md. -->

## Install

```bash
git clone https://gitlab.well-typed.com/well-typed/hypha.git
cd hypha && cabal install exe:hypha exe:hypha-mcp
```

## Try it

```bash
cd your-cabal-project && cabal build --dry-run   # writes plan.json
hypha symbol async/Control.Concurrent.Async/concurrently
```

## Troubleshooting

**Set a UTF-8 locale.** hypha and its build both handle non-ASCII text, and GHC
derives every `Handle`'s encoding from the locale. Under `C`/`POSIX` — the
default in bare containers, where `LANG` is unset — you will hit one of:

```
happy: compiler/GHC/Parser.y: hGetContents: invalid argument (cannot decode byte sequence starting from 226)
hypha: <stdout>: commitBuffer: invalid argument (cannot encode character '\8212')
```

The first is `happy` failing to read `ghc-lib-parser`'s grammar, which contains
a `∷` (U+2237) in the GHC 9.12 series; the second is hypha failing to write an
em-dash. Both are fixed by giving the process a UTF-8 locale:

```bash
export LANG=C.UTF-8
```

## 📖 Documentation

The full guide — installation, subcommands, the doc-browser server, MCP
integration, caching, troubleshooting, and design — lives on the
**[hypha docs site](https://well-typed.pages.well-typed.com/hypha/)**.

## License

BSD-3-Clause. See [`LICENSE`](LICENSE).
