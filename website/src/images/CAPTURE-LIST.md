# Screenshots to capture

Each filename below already exists as a **labelled placeholder PNG** and is
wired into its page. To ship a real screenshot, just **overwrite the file in
place** with your capture under the same name — no markdown edits, and the
build stays green throughout. Any file you haven't replaced keeps showing a
"screenshot coming soon" tile.

| Filename | Referenced by | Should show |
|----------|---------------|-------------|
| `hero-cli.png` | `README.md` (Introduction) + repo-root `README.md` | `hypha symbol …` default **YAML** output in a terminal |
| `hero-server.png` | `README.md` (Introduction), `server/index.md`, repo-root `README.md` | server command palette + a symbol card |
| `cli-yaml.png` | `getting-started/quick-start.md` | compact YAML output of a `symbol` query |
| `server-search.png` | `server/index.md` | fuzzy-search dropdown mid-query |
| `server-symbol-card.png` | `server/index.md` | a rendered symbol card with Haddock prose |
| `server-source-view.png` | `server/index.md` | skylighting source view with a `?line=N` target |

## Capturing

The `hypha` binary is built at
`dist-newstyle/build/*/ghc-*/hypha-*/x/hypha/build/hypha/hypha`
(or use `cabal run hypha --`). For the CLI shots, pipe a `symbol`/`lookup`
query in a terminal. For the server shots, run `hypha server` and open the
loopback URL it prints.

- **hero-cli / cli-yaml:** terminal running e.g.
  `hypha symbol async/Control.Concurrent.Async/concurrently`.
- **hero-server / server-*:** `hypha server`, then screenshot the palette,
  a symbol card, and a `?line=` source view.

## Tips

- Aim for a 16:9-ish crop (the placeholders are 1280×720) so pages don't
  jump when the real image lands.
- The site defaults to the **dark** (navy) theme — dark captures will blend
  in best, though mdBook's theme toggle means either works.
- `placeholder.svg` is the source tile these PNGs were rasterised from; keep
  it around if you want to regenerate a placeholder
  (`rsvg-convert -w 1280 -h 720 placeholder.svg -o NAME.png`).
