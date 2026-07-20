# Screenshots to capture

The pages below currently show `placeholder.svg`. Capture each real
screenshot, drop the PNG into this `images/` directory under the exact
filename listed, then edit the referencing page to point its `<img src>` at
the new file (search the repo for `placeholder.svg` to find each spot).

| Filename | Referenced by | Should show |
|----------|---------------|-------------|
| `hero-cli.png` | `README.md` (Introduction) | `hypha symbol …` default **YAML** output in a terminal |
| `hero-server.png` | `README.md` (Introduction), `server/index.md` | server command palette + a symbol card |
| `cli-yaml.png` | `getting-started/quick-start.md` | compact YAML output of a `symbol` query |
| `server-search.png` | `server/index.md` | fuzzy-search dropdown mid-query |
| `server-symbol-card.png` | `server/index.md` | a rendered symbol card with Haddock prose |
| `server-source-view.png` | `server/index.md` | skylighting source view with a `?line=N` target |

## Why placeholders instead of pre-stamped PNGs

The docs were scaffolded in an environment without an image rasteriser, so
each screenshot slot references the shared `placeholder.svg` rather than a
per-name placeholder PNG. Once you add the real PNGs, swap the one `<img
src>` on each page — the placeholder can then be deleted if no slot still
uses it.

## Tips for consistent shots

- Use the **light** theme for a clean look that matches the site default (or
  capture both and we can offer a toggle later).
- Crop tightly; aim for a 16:9-ish aspect so pages don't jump.
- Terminal shots: a dark terminal is fine, but keep the font legible at
  ~1280px wide.
