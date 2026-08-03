# Doc Browser Server

`hypha server` is a loopback-only doc browser built on the same data as the
CLI, but with a visual surface humans can scan quickly. The first run pays
the indexing cost; every subsequent run hits the SQLite cache and renders
results on the first keystroke.

```bash
$ hypha server --port 4287
hypha server listening on http://127.0.0.1:4287
```

<p align="center">
  <img src="../images/hero-server.png" width="80%" alt="hypha server — command palette and symbol card" />
</p>
<!-- Placeholder; overwrite images/hero-server.png in place. -->

## Highlights

- **Command-palette fuzzy search.** Type `Data.Map lookup` or
  `Data.Map.Strict.lookup` — FZF/Telescope-style tokenised matching ranks
  the canonical symbol first. The dropdown is centered under the search bar
  and works the same on every page.

  <p align="center">
    <img src="../images/server-search.png" width="80%" alt="fuzzy search dropdown mid-query" />
  </p>
  <!-- Placeholder; overwrite images/server-search.png in place. -->

- **One result per definition.** Rows that share a definition site collapse
  into a single hit — `Data.Traversable.mapAccumL` and
  `GHC.Internal.Data.Traversable.mapAccumL` are one result, not two — with
  the most public presentation winning. Nothing is hidden: a `+N` badge
  opens a list naming every package and module that was folded in, each a
  link, with the defining one tagged. `Data.Map.Strict.insertWith` and
  `Data.Map.Lazy.insertWith` stay two results, because they have different
  definitions. Press `Tab` to scope to one package; the scope applies
  before the fold, so a definition two packages present still appears
  under either.
- **Packages and modules are results too**, ranked above the symbols
  beneath them: typing `containers` lands the package page, typing
  `Data.Map.Strict` lands the module page.

- **Live build-progress feedback.** A slim accent-coloured progress bar at
  the top of the page shows how many packages remain to index. A shimmering
  "Building the docs…" placeholder fills the dropdown until the index is
  warm.
- **Façade modules are no longer empty.** A module that only re-exports —
  `Data.Traversable`, `Prelude`, `Data.Map.Strict` — lists its entries with
  real signatures and Haddock, each tagged `from <Module>` (or
  `from <pkg>:<Module>` when the definition lives in a dependency) and
  linking there. Resolution goes through the search index, so a symbol
  re-exported through two or more modules still lands on its declaration
  rather than on the module that merely passes it along.
- **Faithful symbol cards.** Multi-line signatures are joined, Haddock
  comes from the GHC parse tree — so a doc block separated from its
  declaration by a blank line still binds correctly — and is rendered to
  HTML (paragraphs, `<code>`, `<pre>` code blocks, lists, links). Every
  link is built from the component that *defines* the symbol, which may be
  a different package from the one in the URL. A card that could not read a
  signature says so instead of rendering an empty box.

  <p align="center">
    <img src="../images/server-symbol-card.png" width="80%" alt="rendered symbol card with Haddock" />
  </p>
  <!-- Placeholder; overwrite images/server-symbol-card.png in place. -->

- **Skylighting-rendered source view** with a `?line=N` scroll target.

  <p align="center">
    <img src="../images/server-source-view.png" width="80%" alt="highlighted source view" />
  </p>
  <!-- Placeholder; overwrite images/server-source-view.png in place. -->

- **Private libraries.** Sublibs appear as separate sidebar entries (`hypha`,
  `hypha:lib-breakdown`), each with their own pages and search scope.

See **[Endpoints & Flags](reference.md)** for the full HTTP surface and
command-line options.
