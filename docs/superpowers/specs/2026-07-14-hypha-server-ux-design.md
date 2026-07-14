# Hypha Server UX Overhaul — Design

**Date:** 2026-07-14
**Status:** Approved (autonomous goal session; decisions documented inline)
**Branch:** `adinapoli/hypha-server-overhaul`

## 1. Goals

From the user's directive:

1. **Module pages must show documentation, not a bland export list.**
   When Haddock documentation has been built for the package, show it.
2. **Render Haddock on the fly from source** when we have the source at our
   disposal, so docs stay useful without extra disk space.
3. **Make the UI captivating** — modern, slick, Dash-class graphics and
   usability.

## 2. Non-goals

- No external network assets. The CSP stays `default-src 'self'`; fonts remain
  system stacks; all CSS/JS is embedded via `Hypha.Server.Assets`.
- No JS framework. HTMX + small vanilla JS files, as today.
- No invoking GHC/haddock to *build* docs at request time. "Built docs" means
  what `ensureHaddockFor` already locates (hypha cache → local dist-dir →
  store). On-the-fly rendering means parsing source with the existing
  `ghc-lib-parser`/`haddock-library` pipeline, not compiling.
- No new package dependencies. Everything needed (`tagsoup`, `lucid2`,
  `haddock-library`, `skylighting`, `file-embed`, `servant-server`) is already
  a dependency.

## 3. Current state (summary)

- `modPage` renders bare export names from `scModuleExports` — the complaint.
- A full prebuilt-Haddock pipeline exists but is *unreachable from the UI*:
  `/haddock/:pkgver/*` serves rewritten HTML, `ensureHaddockFor` resolves
  cache/dist/store copies, `--prebuild` warms the cache. Nothing links to it,
  and `scHaddockHtml` only serves the hypha cache dir (never dist/store).
- On-the-fly primitives exist for *single symbols*: `Hypha.Source.Extract`
  slices signature + Haddock prose; `Hypha.Server.Ui.Haddock` renders prose
  via `haddock-library`. There is no batch per-module variant — calling
  `extractSymbolInfo` per export would re-parse the module N times.
- The shell (topbar/sidebar/search) is functional but visually plain;
  `keybindings.js` only handles Esc-blur.

## 4. Approaches considered for module documentation

**A. On-the-fly source rendering only.** Consistent look, zero disk, but
ignores richer prebuilt docs (instances, constructor/field/method docs,
cross-links) when they exist. Rejected: contradicts goal 1.

**B. Embed prebuilt Haddock content only.** Highest fidelity, but most store
packages have no built docs unless `--prebuild` ran, and local modules under
active development rarely have fresh Haddock. Rejected: module pages would
stay bland in the most common case.

**C. Hybrid (chosen).** Priority per module:

1. **Prebuilt Haddock** found for the package *and* the module's HTML file
   exists → extract its content regions, rewrite links, embed restyled in the
   hypha shell. Badge: `prebuilt haddock`, plus an "open raw" escape hatch.
2. Else **render from source**: one-parse batch extraction of module header
   prose + per-declaration signature/prose/kind, rendered natively. Badge:
   `rendered from source`.
3. Else (source unresolvable too) the current export list, with the reason
   surfaced (e.g. "package source could not be resolved"), never silently.

This mirrors the user's two bullets exactly and keeps a total order of
fallbacks with no silent degradation.

## 5. Design

### 5.1 Data layer — batch module extraction

`Hypha.Source.Parser` gains a declaration-kind field:

```haskell
data DeclKind
  = DkFunction | DkData | DkNewtype | DkClass
  | DkTypeSyn  | DkTypeFamily | DkPatternSyn | DkForeign
  deriving stock (Show, Eq)

data Decl = Decl { ..., declKind :: !DeclKind }
```

Classification happens in `declsFromTop` from the `HsDecl` constructor
(`TyClD` data/newtype/class/syn/fam, `ValD`/`SigD` → function, pattern
synonym sigs/binds → `DkPatternSyn`, `ForD` → `DkForeign`). `mergeByName`
prefers the non-`DkFunction` kind when merging a signature into a type decl
(cannot happen today, but the merge must be total).

`Hypha.Source.Extract` gains a batch API sharing the existing internal
helpers (`sigText`, `haddockBefore`) over a **single** `parseDecls` pass:

```haskell
data ModuleDocInfo = ModuleDocInfo
  { mdiHeader  :: !(Maybe DocText)  -- module-level '-- |' block
  , mdiEntries :: ![DocEntry]       -- source order
  }

data DocEntry = DocEntry
  { deName      :: !Text
  , deKind      :: !DeclKind
  , deSignature :: !(Maybe Text)
  , deHaddock   :: !(Maybe DocText)
  , deSigLine   :: !(Maybe Int)
  , deDefLine   :: !(Maybe Int)
  }

extractModuleDoc :: FilePath -> Text -> Either Parser.ParseError ModuleDocInfo
```

Module header extraction is line-based like the rest of Extract: the
contiguous `-- |`/`--` comment block immediately above the first line
beginning with `module ` (pragmas and blank lines between block and keyword
are tolerated). Block-style `{-| … -}` headers are out of scope for this
iteration (noted limitation; they render as absent, not as garbage).

### 5.2 Server plumbing — a typed module-doc view

`ServerConfig.scModuleExports` is **replaced** by:

```haskell
data ModuleDocView
  = ViewPrebuilt   !PrebuiltDoc        -- extracted + rewritten fragments
  | ViewFromSource !SourceDoc          -- batch-extracted, natively rendered
  | ViewExportsOnly ![Text] !Text      -- names + human-readable reason

data PrebuiltDoc = PrebuiltDoc
  { pdPkgVer      :: !Text            -- "<pkg>-<ver>" for raw links
  , pdDescription :: !(Maybe Text)    -- inner HTML of #description
  , pdInterface   :: !Text            -- inner HTML of #interface
  , pdContents    :: !(Maybe Text)    -- inner HTML of #table-of-contents list
  }

data SourceDoc = SourceDoc
  { sdInfo    :: !ModuleDocInfo
  , sdExports :: !(Maybe [Text])      -- explicit export list, when parsed
  }
```

`scModuleDoc :: Text -> Text -> IO ModuleDocView` decides the priority chain
(§4C) in `Hypha.Command.Server`. `buildServerConfig` gains the `BuildEnv IO`
parameter (its caller `runServer` already holds it) so it can call
`ensureHaddockFor`. Any `Left` from source parsing or a missing prebuilt file
degrades one level and carries the reason into `ViewExportsOnly` / a stderr
trace — never `Left _ -> pure fallback`.

Export filtering for `ViewFromSource` reuses `Locate.parseExports`: with an
explicit export list, entries are filtered and ordered by it; without one,
all top-level decls appear in source order (same over-inclusion contract as
the search index).

### 5.3 Prebuilt Haddock extraction + link rewriting

New module `Hypha.Server.Haddock.Extract`:

```haskell
extractModuleDocHtml :: Text -> Maybe PrebuiltParts
```

Tagsoup-based: slice the inner HTML of `div#description`,
`div#interface`, and `div#table-of-contents` (all stable Haddock ids across
the versions we care about, ≥ 2.20). Returns `Nothing` when `#interface` is
absent (ancient/unknown markup) — caller degrades to source rendering.

`Hypha.Server.Haddock.Rewrite` gains module-context rewriting applied to the
extracted fragments:

- `../<pkg>-<ver>/<File>.html[#frag]` → `/haddock/<pkg>-<ver>/<File>.html[#frag]`
  (existing rule, kept).
- Same-package relative `<Mod-Name>.html[#frag]` (no slash in path) →
  `/pkg/<component>/<Mod.Name>[#frag]` so navigation stays inside the shell.
- `src/…` → `/haddock/<pkg>-<ver>/src/…`.
- Bare `#frag`, absolute `http(s)`, and `mailto:` untouched.
- Applied to `href` on `<a>` and `src` on `<img>`.

The source-rendered view emits Haddock-compatible anchors (`v:name` for
values, `t:Name` for types) so `#frag` links resolve identically whichever
view a target module renders with.

### 5.4 `/haddock` route: correct resolution + MIME

The `/haddock/:pkgver/*path` route becomes a `Raw` sub-application:

- Resolves the package's Haddock directory via `ensureHaddockFor`
  (cache → dist-dir → store) instead of assuming the hypha cache.
- Path segments are sanitised: reject `..`, empty segments, and absolute
  components (loopback-only server, but path traversal is still a bug).
- Serves `.html` through `rewriteHaddockHtml` as today; other files
  (`ocean.css`, `haddock-bundle.min.js`, fonts, images) are served raw with
  a MIME type inferred from the extension, fixing the currently-broken
  stylesheet/script loading on raw Haddock pages.

### 5.5 Module page UI

`modPage` renders by view:

- **Header (all views):** module name, package link, provenance chip, doc-source
  badge (`prebuilt haddock` / `rendered from source` / `exports only`),
  actions: *View source*, *Open raw haddock ↗* (only when prebuilt exists).
- **ViewPrebuilt:** description + interface fragments injected via
  `toHtmlRaw` inside a `.haddock-embed` wrapper; new
  `ui/css/components/haddock.css` maps Haddock's classes (`.top`, `p.src`,
  `.doc`, `.subs`, `.arguments`, `.methods`, `.instances`, `table.info`) onto
  hypha design tokens. Haddock's own contents list feeds the right rail.
- **ViewFromSource:** module header prose (via `renderHaddockHtml`), then one
  section per `DocEntry`: anchor id, kind badge, name, signature block,
  prose. Entries without prose still show the signature.
- **Right rail ("On this page"):** sticky TOC listing entries (or Haddock's
  contents), hidden below 1200px viewport width.
- **ViewExportsOnly:** today's list plus the visible reason line.

### 5.6 Shell + search UX overhaul

- **Design tokens:** spacing/radius/elevation scale in `base.css`; refreshed
  light/dark palettes (keep the warm-cream identity, increase contrast and
  depth); improved focus-visible states.
- **Theme toggle:** topbar button cycling auto → light → dark; persisted in
  `localStorage`; new `ui/js/theme.js` loaded synchronously in `<head>`
  (self-origin, CSP-safe) sets `data-theme` on `<html>` pre-paint; CSS
  overrides via `:root[data-theme="…"]`.
- **Sidebar:** client-side filter input; two collapsible `<details>` groups —
  *Project* (`OriginLocal`) and *Dependencies* (everything else) — with count
  badges; active entry highlighted by `keybindings.js` from
  `location.pathname`.
- **Search:** keyboard navigation (↑/↓ select, Enter navigate, Esc close);
  `/` and `Ctrl-K`/`⌘K` focus the input from anywhere; server-side `<mark>`
  highlighting of query tokens inside the result name; result rows restyled
  (name + mono signature, pkg·module subline).
- **Keybindings.js** grows from Esc-blur to the above; stays dependency-free.

### 5.7 Page polish

- **Home:** hero with project name; stat chips (packages, project components);
  grid of project-package cards; keyboard-hint chips (`/`, `Ctrl-K`, `Esc`).
- **Symbol card:** kind badge, copy-signature button (Clipboard API), *view in
  module* link alongside the source link.
- **Source view:** sticky header (module path, copy-path button, *back to
  docs*), row hover highlight, pulse animation on the `?line=` target.

## 6. Error handling

All degradations surface their cause: `ViewExportsOnly` carries the reason
text rendered in the page; prebuilt-lookup IO failures trace to stderr via
the existing patterns; parse failures from `extractModuleDoc` propagate as
`ParseError` and degrade the view rather than 500ing. No `Left _ ->`
discards.

## 7. Testing

- **Unit (HUnit):** `extractModuleDoc` (header block, entry kinds, sigs,
  ordering, export filter); `extractModuleDocHtml` region slicing on a small
  fixture captured from real Haddock output; new rewrite rules; anchor id
  generation; MIME inference; path sanitisation.
- **Golden (tasty-golden):** source-rendered module page; prebuilt-embedded
  module page; updated `server-home.html`; search fragment with `<mark>`.
- **Property (falsify):** extend `Property.HaddockRewrite` to the
  same-package and `src/` rules (idempotence, anchor preservation).
- Every phase ends with `cabal build all && cabal test all` and a commit.

## 8. Implementation phases

1. **Data layer:** `DeclKind`, `extractModuleDoc`, `ModuleDocView` +
   `scModuleDoc`, `ensureHaddockFor`-backed `/haddock` Raw route + MIME +
   sanitisation, `Haddock.Extract`, rewrite extensions. Unit + property tests.
2. **Module page UI:** the three views, TOC rail, badges, `haddock.css`.
   Golden tests.
3. **Shell + search:** tokens, theme toggle, sidebar groups/filter, search
   keyboard nav + highlighting, `keybindings.js`/`theme.js`.
4. **Polish + verification:** home/symbol/source upgrades, live end-to-end
   drive of the server, golden regeneration, final review.
