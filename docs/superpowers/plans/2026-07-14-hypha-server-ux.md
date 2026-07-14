# Hypha Server UX Overhaul Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Module pages show real documentation (prebuilt Haddock when built, on-the-fly source rendering otherwise), inside a modernised Dash-class shell.

**Architecture:** Extend the existing `ghc-lib-parser` pipeline to classify and batch-extract per-module declarations; add a typed `ModuleDocView` chain (prebuilt → source → exports-only) wired through `ServerConfig`; convert `/haddock` to a MIME-correct `Raw` route resolved via `ensureHaddockFor`; overhaul CSS/JS assets (tokens, theme toggle, sidebar groups, keyboard search).

**Tech Stack:** servant-server, lucid2, HTMX, tagsoup, haddock-library, ghc-lib-parser (9.10–9.12), skylighting, file-embed, vanilla JS.

**Spec:** `docs/superpowers/specs/2026-07-14-hypha-server-ux-design.md`

## Global Constraints

- No new package dependencies.
- CSP stays `default-src 'self'; img-src 'self' data:; style-src 'self' 'unsafe-inline'` — no inline `<script>`, no remote assets, system font stacks only.
- Strict bangs on every strict field; no `error`/`undefined`; typed sums over strings; degradations always carry and surface their reason.
- Every task: `cabal build all && cabal test all` green, then a conventional commit.
- Existing golden files change only in steps that explicitly regenerate them.
- Haddock anchor scheme: values `v:<name>`, types `t:<Name>` (mirrors haddock's own anchors so `#frag` links work across both views).

---

### Task 1: `DeclKind` classification in `Hypha.Source.Parser`

**Files:**
- Modify: `src/Hypha/Source/Parser.hs`
- Test: `test/Unit/SourceParser.hs` (extend existing if present, else create + register in `test/Main.hs`)

**Interfaces:**
- Produces: `data DeclKind = DkFunction | DkData | DkNewtype | DkClass | DkTypeSyn | DkTypeFamily | DkPatternSyn | DkForeign` (exported, `deriving stock (Show, Eq)`), new field `declKind :: !DeclKind` on `Decl`, and `declSpan :: Decl -> (Maybe Int, Maybe Int)` is NOT added — instead `declDefEndLine :: !(Maybe Int)` new field (end line of the definition span, for slicing type/class bodies).
- `parseDecls` now also returns type/class/pattern-synonym/foreign decls (previously functions only). Search index and symbol lookup pick these up for free.

- [ ] **Step 1: Write failing tests** — parse a fixture module containing `data`, `newtype`, `class`, `type`, `type family`, `pattern`, `foreign import`, and plain functions; assert names + kinds + def spans:

```haskell
  , testCase "parseDecls classifies declaration kinds" $ do
      let src = Text.unlines
            [ "{-# LANGUAGE TypeFamilies, PatternSynonyms #-}"
            , "module Fixture where"
            , "data Colour = Red | Green"
            , "newtype Wrap = Wrap Int"
            , "class Pretty a where"
            , "  pretty :: a -> String"
            , "type Alias = Int"
            , "type family Elem c"
            , "pattern None :: Maybe a"
            , "pattern None = Nothing"
            , "run :: Int -> Int"
            , "run x = x"
            ]
          decls = either (const []) id (Parser.parseDecls "Fixture.hs" src)
          kindOf n = Parser.declKind <$> Parser.findDecl n decls
      kindOf "Colour" @?= Just Parser.DkData
      kindOf "Wrap"   @?= Just Parser.DkNewtype
      kindOf "Pretty" @?= Just Parser.DkClass
      kindOf "Alias"  @?= Just Parser.DkTypeSyn
      kindOf "Elem"   @?= Just Parser.DkTypeFamily
      kindOf "None"   @?= Just Parser.DkPatternSyn
      kindOf "run"    @?= Just Parser.DkFunction
      -- span slicing support for multi-line type decls
      (Parser.declDefLine =<< Parser.findDecl "Colour" decls) @?= Just 3
```

- [ ] **Step 2: Run, verify FAIL** (`declKind` not in scope).
- [ ] **Step 3: Implement.** Add to `Decl`:

```haskell
data DeclKind
  = DkFunction | DkData | DkNewtype | DkClass
  | DkTypeSyn  | DkTypeFamily | DkPatternSyn | DkForeign
  deriving stock (Show, Eq)

data Decl = Decl
  { declName       :: !Text
  , declSiblings   :: ![Text]
  , declKind       :: !DeclKind
  , declSigLine    :: !(Maybe Int)
  , declSigEndLine :: !(Maybe Int)
  , declDefLine    :: !(Maybe Int)
  , declDefEndLine :: !(Maybe Int)
    -- ^ End line of the definition span (inclusive).  For data/class
    -- declarations this delimits the whole body so callers can slice
    -- the constructor/method block out of the source.
  }
```

Extend `declsFromTop`:

```haskell
declsFromTop :: LHsDecl GhcPs -> [Decl]
declsFromTop ld = case unLoc ld of
  SigD _ (TypeSig _ lnames _ty) ->
    let names    = map (rdrText . unLoc) lnames
        (mS, mE) = locLines ld
    in [ sigDecl nm names mS mE DkFunction | nm <- names ]
  SigD _ (PatSynSig _ lnames _ty) ->
    let names    = map (rdrText . unLoc) lnames
        (mS, mE) = locLines ld
    in [ sigDecl nm names mS mE DkPatternSyn | nm <- names ]
  ValD _ (FunBind { fun_id = L _ rn }) ->
    let (mS, mE) = locLines ld
    in [ defDecl (rdrText rn) mS mE DkFunction ]
  ValD _ (PatSynBind _ (PSB { psb_id = L _ rn })) ->
    let (mS, mE) = locLines ld
    in [ defDecl (rdrText rn) mS mE DkPatternSyn ]
  TyClD _ tc ->
    let (mS, mE) = locLines ld
        mk nm k  = [ defDecl nm mS mE k ]
    in case tc of
         SynDecl  { tcdLName = L _ rn } -> mk (rdrText rn) DkTypeSyn
         FamDecl  { tcdFam = FamilyDecl { fdLName = L _ rn } } ->
           mk (rdrText rn) DkTypeFamily
         ClassDecl { tcdLName = L _ rn } -> mk (rdrText rn) DkClass
         DataDecl { tcdLName = L _ rn, tcdDataDefn = defn } ->
           mk (rdrText rn) (case dd_cons defn of
                              NewTypeCon {} -> DkNewtype
                              _             -> DkData)
  ForD _ (ForeignImport { fd_name = L _ rn }) ->
    let (mS, mE) = locLines ld
    in [ defDecl (rdrText rn) mS mE DkForeign ]
  _ -> []
  where
    sigDecl nm names mS mE k = Decl nm (filter (/= nm) names) k mS mE Nothing Nothing
    defDecl nm mS mE k       = Decl nm [] k Nothing Nothing mS mE
```

`mergeByName.merge` keeps the more specific kind and the new field:

```haskell
    merge a b = Decl
      { declName       = declName a
      , declSiblings   = declSiblings a `orEmpty` declSiblings b
      , declKind       = if declKind a == DkFunction then declKind b else declKind a
      , declSigLine    = declSigLine a    `orFirst` declSigLine b
      , declSigEndLine = declSigEndLine a `orFirst` declSigEndLine b
      , declDefLine    = declDefLine a    `orFirst` declDefLine b
      , declDefEndLine = declDefEndLine a `orFirst` declDefEndLine b
      }
```

(Exact record-pattern field names may need adjusting against ghc-lib-parser 9.12 — compile drives the final form; positional patterns are forbidden, record patterns only.)

- [ ] **Step 4: `cabal build all && cabal test all`** — expect PASS; fix any pre-existing tests that pinned functions-only behaviour by updating their expectations (types now indexed is the desired behaviour).
- [ ] **Step 5: Commit** `feat(parser): classify top-level declarations and cover type/class/pattern/foreign decls`

---

### Task 2: batch module extraction in `Hypha.Source.Extract`

**Files:**
- Modify: `src/Hypha/Source/Extract.hs`
- Modify: `src/Hypha/Source/Locate.hs` (drop the `take 100` cap in `parseExports`)
- Test: `test/Unit/SourceExtract.hs` (extend or create + register)

**Interfaces:**
- Produces:

```haskell
data ModuleDocInfo = ModuleDocInfo
  { mdiHeader  :: !(Maybe DocText)
  , mdiEntries :: ![DocEntry]        -- source order
  } deriving stock (Show, Eq)

data DocEntry = DocEntry
  { deName      :: !Text
  , deKind      :: !Parser.DeclKind
  , deSignature :: !(Maybe Text)     -- sig text, or clamped decl slice for types
  , deHaddock   :: !(Maybe DocText)
  , deSigLine   :: !(Maybe Int)
  , deDefLine   :: !(Maybe Int)
  } deriving stock (Show, Eq)

extractModuleDoc :: FilePath -> Text -> Either Parser.ParseError ModuleDocInfo
```

- Consumes: Task 1's `declKind`/`declDefEndLine`.
- Single `parseDecls` pass; per-entry haddock via the existing `haddockBefore`; signatures via existing `sigText`; for `DkData/DkNewtype/DkClass/DkTypeSyn/DkTypeFamily` with no `::` sig, `deSignature` is the raw source slice `declDefLine..declDefEndLine` clamped to 40 lines (append `"…"` line when clamped).
- Module header: contiguous `-- |`-initiated comment block (plus following `--` continuation lines) ending on the last comment line above the first line starting with `module `; blank lines and `{-# … #-}` pragma lines between block and keyword are skipped when scanning upward.

- [ ] **Step 1: Failing tests** — fixture module with `-- |` module header, documented + undocumented exports, a multi-line data decl; assert header text, entry order, kinds, clamped slice, haddock prose presence.

```haskell
  , testCase "extractModuleDoc returns header and entries in source order" $ do
      let src = Text.unlines
            [ "-- | Fixture module header."
            , "--"
            , "-- Second paragraph."
            , "{-# LANGUAGE BangPatterns #-}"
            , "module Fixture (Colour (..), run) where"
            , ""
            , "-- | A colour."
            , "data Colour = Red | Green"
            , ""
            , "-- | Run it."
            , "run :: Int -> Int"
            , "run x = x"
            ]
      case Extract.extractModuleDoc "Fixture.hs" src of
        Left e  -> assertFailure (show e)
        Right d -> do
          fmap unDocText (Extract.mdiHeader d)
            @?= Just "-- | Fixture module header.\n--\n-- Second paragraph."
          map Extract.deName (Extract.mdiEntries d) @?= ["Colour", "run"]
          map Extract.deKind (Extract.mdiEntries d)
            @?= [Parser.DkData, Parser.DkFunction]
          Extract.deSignature (head (Extract.mdiEntries d))
            @?= Just "data Colour = Red | Green"
```

- [ ] **Step 2: Run, verify FAIL.**
- [ ] **Step 3: Implement** `extractModuleDoc` reusing `numberedLines`/`sigText`/`haddockBefore`; add private `moduleHeaderBlock :: [(Int, Text)] -> Maybe DocText` and `declSlice :: [(Int, Text)] -> Int -> Int -> Text` (clamp 40). Remove `take 100` in `Locate.parseExports` (cap truncated real export lists — e.g. `Data.Map` — and silently dropped docs downstream).
- [ ] **Step 4: Build + test all, PASS.**
- [ ] **Step 5: Commit** `feat(extract): one-pass module documentation extraction`

---

### Task 3: prebuilt-Haddock content extraction (`Hypha.Server.Haddock.Extract`)

**Files:**
- Create: `src/Hypha/Server/Haddock/Extract.hs`
- Create: `test/fixtures/haddock/module-fixture.html` (trimmed real Haddock module page: `#package-header`, `#module-header`, `#table-of-contents`, `#description`, `#synopsis`, `#interface` divs)
- Test: `test/Unit/HaddockExtract.hs` (+ register in `test/Main.hs` and `hypha.cabal` test module list)

**Interfaces:**
- Produces:

```haskell
data PrebuiltParts = PrebuiltParts
  { ppDescription :: !(Maybe Text)  -- inner HTML of div#description
  , ppInterface   :: !Text          -- inner HTML of div#interface
  , ppContents    :: !(Maybe Text)  -- inner HTML of div#table-of-contents
  } deriving stock (Show, Eq)

extractModuleDocHtml :: Text -> Maybe PrebuiltParts
```

- `Nothing` iff `div#interface` absent. Implementation: tagsoup `parseTags`, take the tag stream between `TagOpen "div" [("id", target)]` and its *matching* close (track div nesting depth), `renderTags` the slice.

- [ ] **Step 1: Failing test** — extraction from the fixture returns interface containing a known symbol anchor, description containing a known phrase, contents non-empty; and `extractModuleDocHtml "<p>no interface</p>" == Nothing`.
- [ ] **Step 2: FAIL.**
- [ ] **Step 3: Implement** with a depth-tracking `sliceDiv :: Text -> [Tag Text] -> Maybe [Tag Text]` helper shared by all three ids.
- [ ] **Step 4: PASS.**
- [ ] **Step 5: Commit** `feat(server): extract content regions from prebuilt haddock pages`

---

### Task 4: link rewriting for embedded module docs

**Files:**
- Modify: `src/Hypha/Server/Haddock/Rewrite.hs`
- Test: `test/Property/HaddockRewrite.hs` + unit cases in `test/Unit/Server.hs`

**Interfaces:**
- Produces:

```haskell
-- existing, kept:
rewriteHaddockHtml :: Text -> Text
-- new: rewriting for fragments embedded at /pkg/<component>/<mod>
data EmbedContext = EmbedContext
  { ecComponent :: !Text   -- URL component name, e.g. "containers"
  , ecPkgVer    :: !Text   -- "containers-0.7"
  }
rewriteEmbeddedDocHtml :: EmbedContext -> Text -> Text
```

- Rules for `rewriteEmbeddedDocHtml` (applied to `href` on `<a>`, `src` on `<img>`):
  - `../<pkg>-<ver>/<path>[#f]` → `/haddock/<pkg>-<ver>/<path>[#f]` (reuse existing `fixUrl`).
  - `src/<path>` → `/haddock/<ecPkgVer>/src/<path>`.
  - Sibling module page `<Mod-Name>.html[#f]` (no `/`, `.html` suffix, first char uppercase) → `/pkg/<ecComponent>/<Mod.Name>[#f]` (hyphens→dots on the stem).
  - Untouched: bare `#f`, `http://`/`https://`/`mailto:`, absolute `/…`.
- Haddock-to-dotted module name: stem `Data-Map-Strict` → `Data.Map.Strict`.

- [ ] **Step 1: Failing unit cases** for each rule (five positive, four leave-alone).
- [ ] **Step 2: FAIL.**
- [ ] **Step 3: Implement**; factor the shared attr-walking so `rewriteHaddockHtml` and `rewriteEmbeddedDocHtml` share one tag traversal parameterised by URL-fixing function.
- [ ] **Step 4: Property test** — `rewriteEmbeddedDocHtml` is idempotent on its own output; fragment suffixes are preserved verbatim. Run all, PASS.
- [ ] **Step 5: Commit** `feat(server): rewrite links inside embedded haddock fragments`

---

### Task 5: `/haddock` becomes a MIME-correct Raw route resolved via `ensureHaddockFor`

**Files:**
- Modify: `src/Hypha/Server/Api.hs` (haddock branch → `"haddock" :> Capture "pkgver" String :> Raw`… — Servant `Raw` under two captures; implement as `CaptureAll` + custom handler returning `WithStatus`-free raw WAI app: use `Tagged Handler Application`)
- Modify: `src/Hypha/Server/App.hs`, `src/Hypha/Command/Server.hs`
- Test: `test/Unit/Server.hs`

**Interfaces:**
- `ServerConfig.scHaddockFile :: !(Text -> [Text] -> IO (Maybe (FilePath, BL.ByteString)))` replaces `scHaddockHtml` — returns resolved absolute path (for MIME sniffing by extension) + bytes with `.html` payloads already passed through `rewriteHaddockHtml`.
- Produces pure helpers (exported for tests):

```haskell
-- Hypha.Server.App
sanitizeSegments :: [Text] -> Maybe [Text]  -- Nothing on "..", "", '/'-containing, or leading '.' segments
mimeFor :: FilePath -> ByteString           -- by extension: html/css/js/png/svg/gif/woff/woff2/json; default application/octet-stream
```

- Route serves 404 (`text/plain`) when unresolved. Implementation in `Command/Server.hs`: `parsePkgVer` → `ensureHaddockFor cacheRoot plan env pid` → `takeDirectory` of returned index → join sanitised segments → read.
- `buildServerConfig` gains the `BuildEnv IO` argument (caller `runServer` already holds one; mock env in tests).

- [ ] **Step 1: Failing tests** for `sanitizeSegments` (rejects `["..",  "x"]`, `["a/b"]`, `[""]`, `[".hidden"]`; accepts `["Data-Map.html"]`, `["src","Foo.html"]`) and `mimeFor` (`"a.css"` → `"text/css"`, `"b.min.js"` → `"application/javascript"`, `"c.html"` → `"text/html; charset=utf-8"`, `"d"` → `"application/octet-stream"`).
- [ ] **Step 2: FAIL.**
- [ ] **Step 3: Implement** helpers + route swap + config change; wire `Tagged Handler Application` handler that consults `scHaddockFile` and emits `responseLBS` with the right `Content-Type`.
- [ ] **Step 4: Build + test all, PASS** (existing golden/unit servers compile against new `ServerConfig` — update `test/Unit/Server.hs` mock configs).
- [ ] **Step 5: Commit** `feat(server): serve haddock assets with correct MIME from cache/dist/store`

---

### Task 6: `ModuleDocView` + `scModuleDoc` priority chain

**Files:**
- Create: `src/Hypha/Server/ModuleDoc.hs` (types only — view sum consumed by App/UI)
- Modify: `src/Hypha/Server/App.hs` (`ServerConfig`: replace `scModuleExports` with `scModuleDoc`), `src/Hypha/Command/Server.hs` (implementation), `hypha.cabal`
- Test: `test/Unit/Server.hs`

**Interfaces:**

```haskell
-- Hypha.Server.ModuleDoc
data ModuleDocView
  = ViewPrebuilt   !PrebuiltDoc
  | ViewFromSource !SourceDoc
  | ViewExportsOnly ![Text] !Text          -- names + reason
data PrebuiltDoc = PrebuiltDoc
  { pdPkgVer      :: !Text
  , pdDescription :: !(Maybe Text)
  , pdInterface   :: !Text
  , pdContents    :: !(Maybe Text)
  }
data SourceDoc = SourceDoc
  { sdInfo      :: !Extract.ModuleDocInfo  -- export-filtered entries
  , sdHasHaddock :: !(Maybe Text)          -- Just pkgVer when raw haddock exists (for the ↗ link)
  }
```

- `scModuleDoc :: !(Text -> Text -> IO ModuleDocView)`.
- Chain in `Command/Server.hs`: resolve pid; `ensureHaddockFor` → module file `<dir>/<Data-Mod>.html` exists → `Extract.extractModuleDocHtml` → `rewriteEmbeddedDocHtml` each part → `ViewPrebuilt`. Else locate source file → `extractModuleDoc` → filter+order entries by `Locate.parseExports` when non-empty (preserving export-list order; entries not found in decls are skipped) → `ViewFromSource`. Parse failure or missing source → `ViewExportsOnly` with reason (`"module source could not be parsed: …"` / `"package source could not be resolved"`), reason also traced to stderr.
- Module-file name: `Text.replace "." "-" modPath <> ".html"`.

- [ ] **Step 1: Failing unit test** — drive `scModuleDoc` from a `buildServerConfig` over the existing fake-cabal-store fixtures: module with source but no haddock → `ViewFromSource`; unresolvable package → `ViewExportsOnly` with non-empty reason.
- [ ] **Step 2: FAIL.**
- [ ] **Step 3: Implement**; delete `scModuleExports`.
- [ ] **Step 4: Build + test all, PASS.**
- [ ] **Step 5: Commit** `feat(server): typed module documentation view with prebuilt/source/exports chain`

---

### Task 7: module page UI — three views + TOC rail

**Files:**
- Create: `src/Hypha/Server/Ui/ModuleDoc.hs`
- Create: `ui/css/components/haddock.css` (haddock-embed restyle: `.haddock-embed .top`, `p.src`, `.doc`, `.subs`, `.arguments`, `.methods`, `.instances`, `table.info` mapped to tokens)
- Modify: `src/Hypha/Server/App.hs` (`modPage`), `src/Hypha/Server/Assets.hs` + `hypha.cabal` (embed haddock.css), `ui/css/components/doc.css`, `ui/css/layout.css` (right rail grid)
- Test: `test/Golden/Server.hs` + `test/Golden/golden/server-module-source.html`, `test/Golden/golden/server-module-prebuilt.html`

**Interfaces:**
- Produces:

```haskell
-- Hypha.Server.Ui.ModuleDoc
modulePage
  :: Text            -- component (URL name)
  -> Text            -- module dotted path
  -> ModuleDocView
  -> Html ()
anchorFor :: DeclKind -> Text -> Text  -- "v:run" / "t:Colour"
kindBadge :: DeclKind -> Html ()
```

- Layout: `.mod-doc` header (h1 module, package link, doc-source badge, actions: View source, Open raw haddock ↗ when available) + `.doc-body` + `.toc-rail` (sticky `<nav>`, hidden `@media (max-width: 1200px)`).
- `ViewFromSource`: header prose via `Haddock.renderHaddockHtml . unDocText`; per entry `section.decl` with `id=anchorFor`, kind badge, `h3` name, `pre.signature` when present, prose. TOC = entries grouped by kind (`Types` for Dk{Data,Newtype,TypeSyn,TypeFamily,Class}, `Values` for the rest).
- `ViewPrebuilt`: badge `prebuilt haddock`; `toHtmlRaw` description/interface inside `div.haddock-embed`; TOC rail = `toHtmlRaw` pdContents when present.
- `ViewExportsOnly`: current list + visible `.warn` reason.

- [ ] **Step 1: Golden tests first** — render `modulePage` for (a) a `ViewFromSource` built inline from a fixture `ModuleDocInfo`, (b) a `ViewPrebuilt` from fixture parts; `goldenVsString` against new files (generate with `--accept` after implementing, review by eye before committing).
- [ ] **Step 2: Implement page + CSS**; wire `modPage` in `App.hs` to `scModuleDoc`.
- [ ] **Step 3: Build + test all; `--accept` new goldens; eyeball the HTML.**
- [ ] **Step 4: Commit** `feat(server): module pages render prebuilt or source-extracted documentation`

---

### Task 8: shell design tokens + theme toggle

**Files:**
- Modify: `ui/css/base.css` (token block below), `ui/css/layout.css`
- Create: `ui/js/theme.js`
- Modify: `src/Hypha/Server/Ui/Layout.hs` (head: sync `<script src="/assets/theme.js">`; topbar: theme button), `src/Hypha/Server/Api.hs` + `App.hs` + `Assets.hs` (serve `/assets/theme.js`)
- Test: golden `server-home.html` regenerated deliberately

**Token block (replaces the current `:root`):**

```css
:root {
  color-scheme: light dark;
  --bg: #faf9f7; --panel: #ffffff; --panel-2: #f3f1ec;
  --fg: #1c1b1a; --muted: #6d7078; --faint: #9a9da6;
  --accent: #b0413e; --accent-2: #5a3a86; --accent-soft: #b0413e1a;
  --ok: #2e7d4f; --warn-c: #b3730f;
  --code-bg: #f4f1ea; --code-fg: #1c1b1a;
  --border: #e6e3dc; --border-strong: #d5d1c8;
  --shadow-1: 0 1px 2px rgba(28,27,26,.05);
  --shadow-2: 0 4px 16px rgba(28,27,26,.08);
  --r-s: 6px; --r-m: 10px; --r-l: 14px;
  --sp-1: .25rem; --sp-2: .5rem; --sp-3: .75rem; --sp-4: 1rem; --sp-6: 1.5rem; --sp-8: 2rem;
  --mono: ui-monospace, "JetBrains Mono", "SF Mono", Menlo, monospace;
  --sans: ui-sans-serif, system-ui, -apple-system, "Segoe UI", Roboto, sans-serif;
}
```

Dark palette under both `@media (prefers-color-scheme: dark)` (guarded by `:root:not([data-theme=light])`) and `:root[data-theme=dark]`:

```css
  --bg: #0e1014; --panel: #151821; --panel-2: #1b1f2a;
  --fg: #e9eaef; --muted: #8b909d; --faint: #5d6270;
  --accent: #f08c84; --accent-2: #b3a1e5; --accent-soft: #f08c8426;
  --ok: #6fbf92; --warn-c: #d9a555;
  --code-bg: #1a1d26; --code-fg: #e9eaef;
  --border: #262a36; --border-strong: #333849;
  --shadow-1: 0 1px 2px rgba(0,0,0,.4); --shadow-2: 0 6px 24px rgba(0,0,0,.45);
```

**theme.js (complete):**

```js
(function () {
  'use strict';
  var KEY = 'hypha-theme'; // 'auto' | 'light' | 'dark'
  function apply(mode) {
    if (mode === 'light' || mode === 'dark') {
      document.documentElement.setAttribute('data-theme', mode);
    } else {
      document.documentElement.removeAttribute('data-theme');
    }
  }
  apply(localStorage.getItem(KEY) || 'auto');
  window.hyphaTheme = {
    cycle: function () {
      var order = ['auto', 'light', 'dark'];
      var cur = localStorage.getItem(KEY) || 'auto';
      var next = order[(order.indexOf(cur) + 1) % order.length];
      localStorage.setItem(KEY, next);
      apply(next);
      return next;
    },
    current: function () { return localStorage.getItem(KEY) || 'auto'; }
  };
})();
```

Topbar button `button#theme-toggle.theme-toggle` (label updated by keybindings.js on click via `hyphaTheme.cycle()`); focus-visible ring `outline: 2px solid var(--accent-2)` globally.

- [ ] **Step 1: Implement CSS/JS/Haskell changes.**
- [ ] **Step 2: Build; regenerate `server-home.html` golden with `--accept`; diff-review the golden by eye.**
- [ ] **Step 3: Test all, PASS. Commit** `feat(ui): design tokens and three-state theme toggle`

---

### Task 9: sidebar — Project/Dependencies groups, filter, active state

**Files:**
- Modify: `src/Hypha/Server/Ui/Tree.hs`, `src/Hypha/Server/Ui/Layout.hs`, `ui/css/components/tree.css`, `ui/js/keybindings.js`
- Test: golden `server-home.html` regeneration; unit for the grouping function

**Interfaces:**

```haskell
-- Tree.hs
packageTree :: [(Text, PackageOrigin)] -> Html ()   -- signature unchanged
splitByOrigin :: [(Text, PackageOrigin)] -> ([(Text, PackageOrigin)], [(Text, PackageOrigin)])
-- fst = Project (OriginLocal), snd = Dependencies (everything else); exported for tests
```

- Two `<details open>` groups with `<summary>Project <span class="count">N</span></summary>`; filter `<input class="tree-filter" placeholder="Filter packages…">` above them; keybindings.js: (a) filter rows by substring on input, (b) mark the `a[href]` whose pathname prefixes `location.pathname` with `.active`.

- [ ] **Step 1: Unit test `splitByOrigin`** (Local → fst; Hackage/Distribution/SRP/Tarball → snd). FAIL → implement → PASS.
- [ ] **Step 2: Implement UI + JS + CSS** (row hover, active left-border accent, sticky filter).
- [ ] **Step 3: Build + test; regenerate home golden; commit** `feat(ui): grouped, filterable sidebar with active highlighting`

---

### Task 10: search — keyboard navigation + match highlighting

**Files:**
- Modify: `src/Hypha/Server/Ui/Search.hs`, `ui/js/keybindings.js`, `ui/css/components/search.css`
- Test: `test/Golden/golden/server-search-fragment.html` (new), unit for highlighter

**Interfaces:**

```haskell
-- Search.hs
highlightTokens :: [Text] -> Text -> Html ()
-- case-insensitive; wraps each first occurrence of every token in <mark>;
-- non-overlapping, left-to-right; exported for tests
resultsFragment :: [Text] -> [(Text, Text, Text, Text)] -> Html ()
-- NEW first arg: query tokens (callers: App.searchPage passes Fuzzy.tokenize q)
```

- keybindings.js additions (complete behaviours): `/` or `Ctrl/⌘-K` anywhere → focus `.search-input`; with results open: `ArrowDown`/`ArrowUp` move `li.selected`, `Enter` follows the selected `a`, `Escape` empties `#results` then blurs. Rows get `.selected` styling.

- [ ] **Step 1: Unit tests for `highlightTokens`** (single hit, multi-token, case-insensitive, no-hit passthrough, no nested marks). FAIL → implement → PASS.
- [ ] **Step 2: Golden for `resultsFragment ["ma"]` with two fixture rows.** Accept + eyeball.
- [ ] **Step 3: JS + CSS.** Build + test all.
- [ ] **Step 4: Commit** `feat(search): keyboard-first navigation and match highlighting`

---

### Task 11: home, symbol card, source view polish

**Files:**
- Modify: `src/Hypha/Server/App.hs` (homePage: stats chips + project-package card grid), `src/Hypha/Server/Ui/Doc.hs` (kind badge slot, copy-signature button, view-in-module link), `src/Hypha/Server/Ui/Source.hs` (sticky head, copy-path button, back-to-docs link), `ui/css/*` accordingly, `ui/js/keybindings.js` (clipboard handler for `button[data-copy]`)
- Modify: `src/Hypha/Server/App.hs` symPage — pass the defining module + kind (extend `scSymbolLookup` result tuple with `!(Maybe DeclKind)` via a small record if tuple grows past 5 fields: introduce `data SymbolCardData` in `Hypha.Server.ModuleDoc`)
- Test: goldens for home + symbol card; build + test all

**Interfaces:**

```haskell
data SymbolCardData = SymbolCardData
  { scdSignature :: !Text
  , scdHaddock   :: !Text
  , scdModule    :: !Text
  , scdLine      :: !(Maybe Int)
  , scdKind      :: !(Maybe DeclKind)
  }
-- ServerConfig: scSymbolLookup :: !(Text -> Text -> Text -> IO (Maybe SymbolCardData))
```

- Copy buttons: `<button class="copy-btn" data-copy="...">` + JS `navigator.clipboard.writeText`, transient "Copied" state.
- Home stats: total packages, project components, `hint` chips for `/`, `Ctrl-K`, `Esc`.

- [ ] **Step 1: Refactor `scSymbolLookup` to `SymbolCardData`** (mechanical); build.
- [ ] **Step 2: Implement pages + CSS + JS.**
- [ ] **Step 3: Regenerate goldens deliberately; test all PASS.**
- [ ] **Step 4: Commit** `feat(ui): home dashboard, richer symbol card, source view header`

---

### Task 12: end-to-end verification

- [ ] **Step 1:** `cabal build all && cabal test all` — all green.
- [ ] **Step 2:** Live drive: `cabal run hypha -- server` inside the hypha repo itself; `curl` `/`, `/pkg/hypha`, `/pkg/hypha/Hypha.Server.App` (source-rendered view expected), a store package module with prebuilt haddock (run with `--prebuild` for one package first) → prebuilt view; `/haddock/<pkg-ver>/ocean.css` → `text/css`. Check the browser paths render (screenshot via `/run` skill if available).
- [ ] **Step 3:** Fix anything found; final commit `feat(server): UX overhaul — module docs + Dash-class shell`.
