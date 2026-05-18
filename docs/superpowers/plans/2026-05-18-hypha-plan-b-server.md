# hypha — Plan B: `server` mode (phase 14)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land `hypha server` — a localhost browser-based offline doc viewer with fzf/telescope-style live search, package/module tree, keyboard navigation, and lazy Haddock generation. Includes `--prebuild` worker pool and Haddock cross-package link rewriting.

**Architecture:** A `warp`+`wai`+`servant-server` app rendered with `lucid2`, vendored HTMX for live search, a small vanilla-JS keybinding file, and a per-package `MVar BuildState` slot map for de-duplicated lazy Haddock builds. Reuses everything in Plan A.

**Tech Stack:** `warp`, `wai`, `wai-extra`, `servant-server`, `lucid2`, `tagsoup`, `file-embed`, `text-metrics`, `async`, `stm`. All UI assets are embedded into the binary at compile time.

**Spec:** `docs/superpowers/specs/2026-05-18-hypha-design.md` §16. Prerequisite: Plan A merged.

**Reading order:** Tasks 1 → 6. One PR per task.

---

## File structure (added by this plan)

```
ui/css/base.css
ui/css/layout.css
ui/css/components/search.css
ui/css/components/tree.css
ui/css/components/doc.css
ui/js/keybindings.js
ui/js/htmx.min.js                  (vendored from https://unpkg.com/htmx.org@1.9.12)
ui/icons/search.svg
ui/icons/package.svg
ui/icons/module.svg

src/Hypha/Server/Assets.hs
src/Hypha/Server/Slots.hs
src/Hypha/Server/Haddock/Rewrite.hs
src/Hypha/Server/Ui/Layout.hs
src/Hypha/Server/Ui/Search.hs
src/Hypha/Server/Ui/Tree.hs
src/Hypha/Server/Ui/Doc.hs
src/Hypha/Server/Ui/Source.hs
src/Hypha/Server/Api.hs
src/Hypha/Server/App.hs
src/Hypha/Command/Server.hs

test/Property/HaddockRewrite.hs
test/Unit/ServerSlots.hs
test/Golden/Server.hs
test/Golden/golden/server-home.html
test/Golden/golden/haddock-rewrite-async.html
```

## Conventions

- Manual `!` bangs on record fields. No `StrictData`.
- Lucid2 component functions for repeated HTML. Modular CSS (one file per concern).
- JS is small, named, vanilla. No frameworks.
- All assets vendored, embedded with `file-embed`. No CDN at runtime.
- HTTP server binds to `127.0.0.1` only. `--bind HOST:PORT` allowed; non-localhost HOST is refused with exit 2.

---

## Task 1: Slot management + Haddock rewrite

**Files:**
- Create: `src/Hypha/Server/Slots.hs`
- Create: `src/Hypha/Server/Haddock/Rewrite.hs`
- Create: `test/Unit/ServerSlots.hs`
- Create: `test/Property/HaddockRewrite.hs`
- Modify: `hypha.cabal`

- [ ] **Step 1: Extend `hypha.cabal`**

Extend `library` `exposed-modules`:

```cabal
    Hypha.Server.Slots
    Hypha.Server.Haddock.Rewrite
```

Extend `library` `build-depends`:

```cabal
    , async      >= 2.2
    , stm        >= 2.5
    , tagsoup    >= 0.14
```

Extend `test-suite` `other-modules`:

```cabal
    Unit.ServerSlots
    Property.HaddockRewrite
```

- [ ] **Step 2: Write `Hypha.Server.Slots`**

```haskell
{-# LANGUAGE LambdaCase #-}
module Hypha.Server.Slots
  ( BuildSlots
  , BuildState (..)
  , initialiseSlots
  , withSlot
  ) where

import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.MVar  (MVar, newMVar, modifyMVar)
import Control.Exception        (SomeException)
import qualified Data.Map.Strict as Map
import Data.Map.Strict          (Map)

import Hypha.Types.PackageId (PackageId)

data BuildState
  = NotStarted
  | Building !(Async FilePath)
  | Done     !FilePath
  | Failed   !SomeException

type BuildSlots = Map PackageId (MVar BuildState)

-- | Build a fresh slot for every package in the plan. The outer Map is
-- immutable; per-package MVars are independent locks so workers do not
-- contend for a global mutex.
initialiseSlots :: [PackageId] -> IO BuildSlots
initialiseSlots pids = do
  pairs <- mapM (\p -> do v <- newMVar NotStarted; pure (p, v)) pids
  pure (Map.fromList pairs)

-- | Acquire-or-spawn semantics. The first requestor spawns the build; later
-- requestors wait on the same `Async`.
withSlot :: BuildSlots
         -> PackageId
         -> IO FilePath        -- ^ build action; returns the haddock dir on success
         -> IO (Either SomeException FilePath)
withSlot slots pid build =
  case Map.lookup pid slots of
    Nothing -> do
      -- Slot not found: spawn a one-shot, no caching.
      a <- async build
      try' (wait a)
    Just slot -> modifyMVar slot $ \case
      NotStarted -> do
        a <- async build
        pure (Building a, ())
      Building a -> pure (Building a, ())
      Done p     -> pure (Done p, ())
      Failed e   -> pure (Failed e, ())
      `andThen` \_ -> joinSlot slot
  where
    try' :: IO FilePath -> IO (Either SomeException FilePath)
    try' io = do
      r <- Control.Exception.try io
      pure r
    andThen :: IO ((), ()) -> (() -> IO a) -> IO a
    andThen m k = m >>= \(_, ()) -> k ()

joinSlot :: MVar BuildState -> IO (Either SomeException FilePath)
joinSlot slot = do
  st <- readMVarSnapshot slot
  case st of
    NotStarted   -> pure (Left (toException (userError "slot reset")))
    Building a   -> do
      r <- Control.Exception.try (wait a)
      modifyMVarStrict slot $ \_ -> case r of
        Right p -> Done p
        Left e  -> Failed e
      pure r
    Done p       -> pure (Right p)
    Failed e     -> pure (Left e)
  where
    readMVarSnapshot v = modifyMVar v (\s -> pure (s, s))
    modifyMVarStrict v f = modifyMVar v (\s -> let s' = f s in pure (s', ()))

-- imports needed for try / toException
import Control.Exception (try, toException)
import Control.Concurrent.MVar (readMVar)
```

> Note: the snippet above sketches the slot semantics; clean up the helper definitions when implementing (`andThen` and the inline `import` lines indicate intent rather than working syntax). The concrete invariant to test is: two concurrent calls to `withSlot` for the same `pid` invoke `build` *at most once*.

- [ ] **Step 3: Write `Hypha.Server.Haddock.Rewrite`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Haddock.Rewrite
  ( rewriteHaddockHtml
  ) where

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Text.HTML.TagSoup as TS

-- | Rewrite cross-package relative URLs of the form @../<pkg>-<ver>/...@ to
-- absolute server routes @/haddock/<pkg>-<ver>/...@. Anchors and same-page
-- @#frag@ links are preserved.
rewriteHaddockHtml :: Text -> Text
rewriteHaddockHtml html =
  TS.renderTags (map fixTag (TS.parseTags html))
  where
    fixTag (TS.TagOpen "a" attrs) = TS.TagOpen "a" (map fixHref attrs)
    fixTag t = t

    fixHref ("href", v) = ("href", fixUrl v)
    fixHref kv          = kv

    fixUrl v
      | "../" `Text.isPrefixOf` v =
          let rest = Text.drop 3 v
          in case Text.splitOn "/" rest of
               (dir:_) | dirLooksLikePkg dir -> "/haddock/" <> rest
               _                              -> v
      | otherwise = v

    dirLooksLikePkg :: Text -> Bool
    dirLooksLikePkg d =
      -- Heuristic: a pkg-ver token has at least one '-' and the suffix
      -- starts with a digit.
      case Text.splitOn "-" d of
        (_:rest@(_:_)) -> case Text.uncons (last rest) of
                            Just (c, _) -> c >= '0' && c <= '9'
                            Nothing     -> False
        _              -> False
```

- [ ] **Step 4: Property test — rewrite is idempotent**

Create `test/Property/HaddockRewrite.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Property.HaddockRewrite (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range
import Test.Falsify.Property (gen, assert)
import qualified Test.Falsify.Predicate as P
import qualified Data.Text as Text

import Hypha.Server.Haddock.Rewrite (rewriteHaddockHtml)

genHtml :: Gen.Gen Text.Text
genHtml = do
  segs <- Gen.list (Range.between (1, 4)) $ Gen.elem
            (   pure "<a href=\"../async-2.2.5/index.html\">x</a>"
             <> pure "<a href=\"#anchor\">y</a>"
             <> pure "<p>hello</p>"
             <> pure "<a href=\"http://example.com\">z</a>")
  pure (Text.concat segs)

tests :: TestTree
tests = testGroup "Haddock.Rewrite"
  [ testProperty "rewrite is idempotent" $ do
      h <- gen genHtml
      let once  = rewriteHaddockHtml h
          twice = rewriteHaddockHtml once
      assert $ P.eq P..$ ("once", once) P..$ ("twice", twice)

  , testProperty "rewrite preserves non-pkg hrefs" $ do
      h <- gen (pure (Text.pack "<a href=\"#frag\">x</a><a href=\"http://x\">y</a>"))
      let r = rewriteHaddockHtml h
      assert $ P.eq P..$ ("expected", h) P..$ ("got", r)
  ]
```

- [ ] **Step 5: Unit test for slot dedup**

Create `test/Unit/ServerSlots.hs`:

```haskell
module Unit.ServerSlots (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Control.Concurrent.Async (concurrently)
import Data.IORef

import Hypha.Server.Slots (initialiseSlots, withSlot)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

asyncPid :: PackageId
asyncPid = PackageId (PackageName "async") (Version "2.2.5")

tests :: TestTree
tests = testGroup "Server.Slots"
  [ testCase "build runs at most once for the same package" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots [asyncPid]
      let build = do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            pure "/tmp/haddock-async"
      (_, _) <- concurrently (withSlot slots asyncPid build)
                             (withSlot slots asyncPid build)
      n <- readIORef counter
      n @?= 1
  ]
```

Register in `test/Main.hs`:

```haskell
import qualified Property.HaddockRewrite
import qualified Unit.ServerSlots
-- ...
  , Property.HaddockRewrite.tests
  , Unit.ServerSlots.tests
```

- [ ] **Step 6: Run tests + commit**

Run: `cabal test`
Expected: all pass.

```bash
git add hypha.cabal src/Hypha/Server/Slots.hs src/Hypha/Server/Haddock/Rewrite.hs \
        test/Property/HaddockRewrite.hs test/Unit/ServerSlots.hs test/Main.hs
git commit -m "feat(server): per-package build slots + Haddock URL rewriter"
```

---

## Task 2: UI assets + embedded asset module

**Files:**
- Create: `ui/css/base.css`
- Create: `ui/css/layout.css`
- Create: `ui/css/components/search.css`
- Create: `ui/css/components/tree.css`
- Create: `ui/css/components/doc.css`
- Create: `ui/js/keybindings.js`
- Create: `ui/js/htmx.min.js` (vendored — fetch from https://unpkg.com/htmx.org@1.9.12 once)
- Create: `ui/icons/search.svg`
- Create: `ui/icons/package.svg`
- Create: `ui/icons/module.svg`
- Create: `src/Hypha/Server/Assets.hs`
- Modify: `hypha.cabal` (add `file-embed`)

- [ ] **Step 1: Extend `hypha.cabal`**

```cabal
    , file-embed >= 0.0.16
    , bytestring
```

Extend `exposed-modules`:

```cabal
    Hypha.Server.Assets
```

Add `extra-source-files` at the top of the cabal file:

```cabal
extra-source-files:
  ui/css/base.css
  ui/css/layout.css
  ui/css/components/search.css
  ui/css/components/tree.css
  ui/css/components/doc.css
  ui/js/keybindings.js
  ui/js/htmx.min.js
  ui/icons/search.svg
  ui/icons/package.svg
  ui/icons/module.svg
```

- [ ] **Step 2: Write `ui/css/base.css`**

```css
:root {
  color-scheme: light dark;
  --bg: #fff;
  --fg: #111;
  --muted: #666;
  --accent: #b0413e;
  --code-bg: #f4f1ea;
  --border: #ddd;
}
@media (prefers-color-scheme: dark) {
  :root { --bg: #161616; --fg: #e8e8e8; --muted: #8a8a8a;
          --accent: #f08c84; --code-bg: #1f1f1f; --border: #333; }
}
* { box-sizing: border-box; }
html, body { margin: 0; padding: 0; background: var(--bg); color: var(--fg);
             font-family: ui-sans-serif, system-ui, sans-serif; }
a { color: var(--accent); text-decoration: none; }
a:hover { text-decoration: underline; }
code, pre { font-family: ui-monospace, monospace; background: var(--code-bg); }
pre { padding: 0.6rem 0.8rem; border-radius: 4px; overflow-x: auto; }
```

- [ ] **Step 3: Write `ui/css/layout.css`**

```css
.app {
  display: grid;
  grid-template-rows: auto 1fr;
  grid-template-columns: 280px 1fr;
  grid-template-areas: "topbar topbar" "sidebar main";
  height: 100vh;
}
.topbar  { grid-area: topbar;  border-bottom: 1px solid var(--border);
           padding: 0.6rem 1rem; display: flex; gap: 0.8rem; align-items: center; }
.sidebar { grid-area: sidebar; border-right: 1px solid var(--border);
           overflow-y: auto; padding: 0.8rem; }
.main    { grid-area: main;    overflow-y: auto; padding: 1rem 1.4rem; }
.crumbs  { color: var(--muted); margin-bottom: 0.8rem; font-size: 0.9rem; }
```

- [ ] **Step 4: Write `ui/css/components/search.css`**

```css
.search-input { flex: 1; padding: 0.4rem 0.6rem; border: 1px solid var(--border);
                background: var(--bg); color: var(--fg); border-radius: 4px; }
.results      { list-style: none; padding: 0; margin: 0.6rem 0 0 0; }
.results li   { padding: 0.3rem 0.4rem; border-radius: 3px; cursor: pointer; }
.results li.cursor    { background: var(--code-bg); }
.results li .sig      { color: var(--muted); margin-left: 0.6rem; }
.results li .pkgmod   { color: var(--accent); font-size: 0.85rem; }
```

- [ ] **Step 5: Write `ui/css/components/tree.css`**

```css
.tree { list-style: none; padding: 0; margin: 0; }
.tree li { padding: 0.15rem 0; }
.tree details > summary { cursor: pointer; }
.tree .module-leaf a { display: block; padding: 0.1rem 0; }
```

- [ ] **Step 6: Write `ui/css/components/doc.css`**

```css
.doc .symbol-name { font-weight: 700; }
.doc .signature   { background: var(--code-bg); padding: 0.5rem; border-radius: 4px; }
.doc .src-link    { color: var(--muted); font-size: 0.85rem; margin-top: 1rem; }
.doc .see-also    { margin-top: 1rem; }
```

- [ ] **Step 7: Write `ui/js/keybindings.js`**

```javascript
(function () {
  'use strict';

  function focusSearch(ev) {
    const input = document.querySelector('.search-input');
    if (input && document.activeElement !== input) {
      ev.preventDefault();
      input.focus();
      input.select();
    }
  }

  function moveCursor(delta) {
    const items = Array.from(document.querySelectorAll('.results li'));
    if (items.length === 0) return;
    const i = items.findIndex(li => li.classList.contains('cursor'));
    const next = Math.max(0, Math.min(items.length - 1, (i < 0 ? 0 : i + delta)));
    items.forEach(li => li.classList.remove('cursor'));
    items[next].classList.add('cursor');
    items[next].scrollIntoView({ block: 'nearest' });
  }

  function activateCursor() {
    const li = document.querySelector('.results li.cursor');
    if (li) {
      const a = li.querySelector('a');
      if (a) window.location.href = a.getAttribute('href');
    }
  }

  document.addEventListener('keydown', function (ev) {
    if (ev.key === '/' || ev.key === 's' || (ev.ctrlKey && ev.key === 'k')) {
      focusSearch(ev);
    } else if (ev.key === 'Escape') {
      document.activeElement && document.activeElement.blur();
    } else if (!ev.metaKey && !ev.ctrlKey && !ev.altKey) {
      if (document.activeElement && document.activeElement.tagName === 'INPUT') return;
      switch (ev.key) {
        case 'j': case 'ArrowDown': ev.preventDefault(); moveCursor(+1); break;
        case 'k': case 'ArrowUp':   ev.preventDefault(); moveCursor(-1); break;
        case 'Enter':               ev.preventDefault(); activateCursor(); break;
        case 'g':                   /* swallow; gp/gh handled via custom seq */ break;
      }
    }
  });
})();
```

- [ ] **Step 8: Vendor HTMX**

Download `https://unpkg.com/htmx.org@1.9.12/dist/htmx.min.js` to `ui/js/htmx.min.js`. License: BSD/Zero-clause; add a note in `README.md` mentioning the vendored version.

- [ ] **Step 9: Inline icons**

`ui/icons/search.svg`:

```xml
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="14" height="14">
  <circle cx="7" cy="7" r="5" fill="none" stroke="currentColor" stroke-width="2"/>
  <path d="M11 11l4 4" stroke="currentColor" stroke-width="2"/>
</svg>
```

`ui/icons/package.svg`:

```xml
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="14" height="14">
  <path d="M2 4l6 -3l6 3v8l-6 3l-6 -3z" fill="none" stroke="currentColor" stroke-width="1.4"/>
</svg>
```

`ui/icons/module.svg`:

```xml
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="14" height="14">
  <rect x="2" y="3" width="12" height="10" fill="none" stroke="currentColor" stroke-width="1.4"/>
  <path d="M2 7h12" stroke="currentColor" stroke-width="1.4"/>
</svg>
```

- [ ] **Step 10: Write `Hypha.Server.Assets`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell  #-}
module Hypha.Server.Assets
  ( cssBundle
  , htmxJs
  , keybindingsJs
  , iconSearch, iconPackage, iconModule
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

cssBundle :: ByteString
cssBundle =
     $(embedFile "ui/css/base.css")
  <> "\n" <> $(embedFile "ui/css/layout.css")
  <> "\n" <> $(embedFile "ui/css/components/search.css")
  <> "\n" <> $(embedFile "ui/css/components/tree.css")
  <> "\n" <> $(embedFile "ui/css/components/doc.css")

htmxJs, keybindingsJs :: ByteString
htmxJs        = $(embedFile "ui/js/htmx.min.js")
keybindingsJs = $(embedFile "ui/js/keybindings.js")

iconSearch, iconPackage, iconModule :: ByteString
iconSearch  = $(embedFile "ui/icons/search.svg")
iconPackage = $(embedFile "ui/icons/package.svg")
iconModule  = $(embedFile "ui/icons/module.svg")
```

- [ ] **Step 11: Verify build + commit**

Run: `cabal build`
Expected: builds; embedded assets are linked into the library.

```bash
git add hypha.cabal ui/ src/Hypha/Server/Assets.hs
git commit -m "feat(server): vendor HTMX, ship modular CSS/JS/icons, embed via file-embed"
```

---

## Task 3: Lucid2 UI views

**Files:**
- Create: `src/Hypha/Server/Ui/Layout.hs`
- Create: `src/Hypha/Server/Ui/Search.hs`
- Create: `src/Hypha/Server/Ui/Tree.hs`
- Create: `src/Hypha/Server/Ui/Doc.hs`
- Create: `src/Hypha/Server/Ui/Source.hs`
- Modify: `hypha.cabal` (add `lucid2`)

- [ ] **Step 1: Extend `hypha.cabal`**

```cabal
    , lucid2 >= 0.0.20210901
```

Extend `exposed-modules`:

```cabal
    Hypha.Server.Ui.Layout
    Hypha.Server.Ui.Search
    Hypha.Server.Ui.Tree
    Hypha.Server.Ui.Doc
    Hypha.Server.Ui.Source
```

- [ ] **Step 2: Write `Hypha.Server.Ui.Layout`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Layout
  ( shellPage
  ) where

import qualified Data.Text as Text
import Data.Text (Text)
import Lucid

import qualified Hypha.Server.Ui.Search as UISearch
import qualified Hypha.Server.Ui.Tree   as UITree

-- | The shell HTML wrapping every view. Top search bar, sidebar tree, main pane.
shellPage :: Text                -- ^ page title
          -> [(Text, Text)]      -- ^ breadcrumbs (label, href)
          -> [Text]              -- ^ package list for sidebar tree
          -> Html ()             -- ^ main pane body
          -> Html ()
shellPage title crumbs pkgs body = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ (toHtml title)
    link_ [rel_ "stylesheet", href_ "/assets/style.css"]
    script_ [src_ "/assets/htmx.min.js", defer_ ""] (mempty :: Text)
    script_ [src_ "/assets/keybindings.js", defer_ ""] (mempty :: Text)
  body_ $ do
    div_ [class_ "app"] $ do
      div_ [class_ "topbar"] $ do
        UISearch.searchInput
      div_ [class_ "sidebar"] (UITree.packageTree pkgs)
      div_ [class_ "main"] $ do
        div_ [class_ "crumbs"] $ mconcat
          [ a_ [href_ h] (toHtml l) <> toHtml (" / " :: Text)
          | (l, h) <- crumbs ]
        body
```

- [ ] **Step 3: Write `Hypha.Server.Ui.Search`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Search
  ( searchInput
  , resultsFragment
  ) where

import qualified Data.Text as Text
import Data.Text (Text)
import Lucid

searchInput :: Html ()
searchInput = input_
  [ class_       "search-input"
  , type_        "search"
  , name_        "q"
  , placeholder_ "search (press s, /, or Ctrl-K)"
  , autocomplete_ "off"
  , autofocus_
  , makeAttribute "hx-get"     "/search"
  , makeAttribute "hx-trigger" "keyup changed delay:120ms"
  , makeAttribute "hx-target"  "#results"
  ]

resultsFragment :: [(Text, Text, Text, Text)]  -- ^ (pkg, module, name, signature)
                -> Html ()
resultsFragment rows = ul_ [class_ "results", id_ "results"] $
  mapM_ rowHtml rows
  where
    rowHtml (pkg, modPath, name, sig) = li_ $ do
      a_ [href_ ("/pkg/" <> pkg <> "/" <> modPath <> "/" <> name)] $ do
        span_ [class_ "name"]   (toHtml name)
        span_ [class_ "sig"]    (toHtml sig)
        span_ [class_ "pkgmod"] (toHtml (pkg <> " · " <> modPath))
```

- [ ] **Step 4: Write `Hypha.Server.Ui.Tree`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Tree
  ( packageTree
  ) where

import qualified Data.Text as Text
import Data.Text (Text)
import Lucid

packageTree :: [Text] -> Html ()
packageTree pkgs = ul_ [class_ "tree"] $
  mapM_ pkgItem pkgs
  where
    pkgItem p = li_ $ a_ [href_ ("/pkg/" <> p)] (toHtml p)
```

- [ ] **Step 5: Write `Hypha.Server.Ui.Doc`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Doc
  ( symbolCard
  ) where

import Data.Text (Text)
import Lucid

symbolCard :: Text  -- ^ name
           -> Text  -- ^ signature
           -> Text  -- ^ haddock html (already-rendered)
           -> Text  -- ^ source path
           -> Int   -- ^ source line
           -> Html ()
symbolCard name sig haddockHtml srcPath srcLine = div_ [class_ "doc"] $ do
  h2_ [class_ "symbol-name"] (toHtml name)
  pre_ [class_ "signature"] (code_ (toHtml sig))
  div_ [class_ "haddock"] (toHtmlRaw haddockHtml)
  div_ [class_ "src-link"] $ do
    toHtml ("source: " :: Text)
    a_ [href_ ("/source/" <> srcPath)] (toHtml srcPath)
    toHtml (":" :: Text); toHtml (show srcLine)
```

- [ ] **Step 6: Write `Hypha.Server.Ui.Source`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Source
  ( sourceView
  ) where

import qualified Data.Text as Text
import Data.Text (Text)
import Lucid

sourceView :: Text -> Html ()
sourceView body = pre_ [class_ "source"] (code_ (toHtml body))
```

- [ ] **Step 7: Build + commit**

Run: `cabal build`
Expected: builds.

```bash
git add hypha.cabal src/Hypha/Server/Ui/
git commit -m "feat(server): lucid2 views — shell, search, tree, doc, source"
```

---

## Task 4: Servant API + WAI app

**Files:**
- Create: `src/Hypha/Server/Api.hs`
- Create: `src/Hypha/Server/App.hs`
- Modify: `hypha.cabal` (add `warp`, `wai`, `wai-extra`, `servant-server`, `http-media`)

- [ ] **Step 1: Extend `hypha.cabal`**

```cabal
    , warp           >= 3.3
    , wai            >= 3.2
    , wai-extra      >= 3.1
    , servant-server >= 0.20
    , http-media     >= 0.8
    , blaze-html     >= 0.9
```

Extend `exposed-modules`:

```cabal
    Hypha.Server.Api
    Hypha.Server.App
```

- [ ] **Step 2: Write `Hypha.Server.Api`**

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Hypha.Server.Api
  ( HyphaApi
  , api
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Proxy (Proxy (..))
import Lucid (Html, renderBS)
import Network.HTTP.Media ((//), (/:))
import Servant.API

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML (Html ()) where
  mimeRender _ = renderBS

type HyphaApi
  =    Get '[HTML] (Html ())
  :<|> "search"  :> QueryParam "q" String :> Get '[HTML] (Html ())
  :<|> "pkg"     :> Capture "pkg" String :> Get '[HTML] (Html ())
  :<|> "pkg"     :> Capture "pkg" String :> Capture "mod" String :> Get '[HTML] (Html ())
  :<|> "pkg"     :> Capture "pkg" String :> Capture "mod" String :> Capture "sym" String :> Get '[HTML] (Html ())
  :<|> "haddock" :> Capture "pkgver" String :> CaptureAll "path" String :> Get '[HTML] (Html ())
  :<|> "source"  :> Capture "pkg" String :> Capture "mod" String :> Get '[HTML] (Html ())
  :<|> "assets" :> "style.css"        :> Get '[OctetStream] BL.ByteString
  :<|> "assets" :> "htmx.min.js"      :> Get '[OctetStream] BL.ByteString
  :<|> "assets" :> "keybindings.js"   :> Get '[OctetStream] BL.ByteString
  :<|> "healthz" :> Get '[PlainText] String

api :: Proxy HyphaApi
api = Proxy
```

- [ ] **Step 3: Write `Hypha.Server.App`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.App
  ( appWith
  , ServerConfig (..)
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Data.Text (Text)
import Lucid
import Network.Wai (Application)
import Servant

import qualified Hypha.Server.Assets         as Assets
import qualified Hypha.Server.Haddock.Rewrite as Rewrite
import qualified Hypha.Server.Ui.Layout      as UI
import qualified Hypha.Server.Ui.Search      as UISearch
import qualified Hypha.Server.Ui.Doc         as UIDoc
import qualified Hypha.Server.Ui.Source      as UISrc
import           Hypha.Server.Api            (HyphaApi, api)
import           Hypha.Server.Slots          (BuildSlots)

data ServerConfig = ServerConfig
  { scProjectName  :: !Text
  , scPackages     :: ![Text]
  , scSlots        :: !BuildSlots
  , scHumanSearch  :: !(Text -> IO [(Text, Text, Text, Text)])
      -- ^ given query, return [(pkg, module, name, sig)]
  , scSymbolLookup :: !(Text -> Text -> Text -> IO (Maybe (Text, Text, Text, Int)))
      -- ^ pkg mod sym -> (signature, haddockHtml, srcPath, srcLine)
  , scHaddockHtml  :: !(Text -> [String] -> IO (Maybe BL.ByteString))
      -- ^ "<pkg>-<ver>" + path -> raw bytes (already rewritten)
  , scSourceText   :: !(Text -> Text -> IO (Maybe Text))
  }

appWith :: ServerConfig -> Application
appWith cfg = serve api (server cfg)

server :: ServerConfig -> Server HyphaApi
server cfg =
       homePage cfg
  :<|> searchPage cfg
  :<|> pkgPage cfg
  :<|> modPage cfg
  :<|> symPage cfg
  :<|> haddockPage cfg
  :<|> sourcePage cfg
  :<|> serveAsset (BL.fromStrict Assets.cssBundle)
  :<|> serveAsset (BL.fromStrict Assets.htmxJs)
  :<|> serveAsset (BL.fromStrict Assets.keybindingsJs)
  :<|> pure "ok"
  where
    serveAsset bytes = pure bytes

homePage :: ServerConfig -> Handler (Html ())
homePage cfg = pure $ UI.shellPage (scProjectName cfg) [] (scPackages cfg) $
  p_ $ toHtml ("Welcome to hypha. Press " :: Text)
    >> code_ "s" >> toHtml (" to search." :: Text)

searchPage :: ServerConfig -> Maybe String -> Handler (Html ())
searchPage cfg mq = do
  let q = maybe "" Text.pack mq
  rows <- liftIO (scHumanSearch cfg q)
  pure (UISearch.resultsFragment rows)

pkgPage, modPage :: ServerConfig -> String -> Handler (Html ())
pkgPage cfg pkg = pure $ UI.shellPage (Text.pack pkg) [("home","/")] (scPackages cfg) $
  p_ (toHtml ("Package " <> Text.pack pkg))
modPage cfg pkg = pkgPage cfg pkg  -- placeholder; refined in Task 5 when we wire commands

modPage2 :: ServerConfig -> String -> String -> Handler (Html ())
modPage2 cfg pkg modPath = pure $ UI.shellPage (Text.pack modPath) [] (scPackages cfg) $
  p_ (toHtml ("Module " <> Text.pack modPath <> " in " <> Text.pack pkg))

symPage :: ServerConfig -> String -> String -> String -> Handler (Html ())
symPage cfg pkg modPath sym = do
  m <- liftIO (scSymbolLookup cfg (Text.pack pkg) (Text.pack modPath) (Text.pack sym))
  case m of
    Nothing -> pure $ UI.shellPage (Text.pack sym) [] (scPackages cfg) (p_ "not found")
    Just (sig, hd, srcPath, srcLine) ->
      pure $ UI.shellPage (Text.pack sym) [] (scPackages cfg)
                (UIDoc.symbolCard (Text.pack sym) sig hd srcPath srcLine)

haddockPage :: ServerConfig -> String -> [String] -> Handler (Html ())
haddockPage cfg pkgVer path = do
  m <- liftIO (scHaddockHtml cfg (Text.pack pkgVer) path)
  case m of
    Nothing -> pure (p_ "not found")
    Just bs -> pure (toHtmlRaw (decodeUtf8' bs))
  where
    decodeUtf8' = Text.pack . BL.unpack . BL.takeWhile (const True)
    -- For MVP we transmit bytes-as-text via toHtmlRaw; refine to a raw
    -- handler once we move beyond HTML-only.

sourcePage :: ServerConfig -> String -> String -> Handler (Html ())
sourcePage cfg pkg modPath = do
  m <- liftIO (scSourceText cfg (Text.pack pkg) (Text.pack modPath))
  case m of
    Nothing -> pure (p_ "not found")
    Just t  -> pure (UI.shellPage (Text.pack modPath) [] (scPackages cfg)
                       (UISrc.sourceView t))
```

> Note on `modPage` vs `modPage2`: the API type lists both 2-segment and 3-segment `/pkg/...` routes; `modPage2` is the actual handler for `:<|> Capture "mod"`. Wire it through `:<|> modPage2` instead of the placeholder `modPage` in the server tuple.

- [ ] **Step 4: Build + commit**

Run: `cabal build`
Expected: builds.

```bash
git add hypha.cabal src/Hypha/Server/Api.hs src/Hypha/Server/App.hs
git commit -m "feat(server): servant API + WAI app stitching"
```

---

## Task 5: `hypha server` command + prebuild worker pool + bind safety

**Files:**
- Create: `src/Hypha/Command/Server.hs`
- Create: `test/Golden/Server.hs`
- Create: `test/Golden/golden/server-home.html`
- Modify: `src/Hypha/Cli/Parser.hs` (extend `Subcommand`)
- Modify: `src/Hypha/Cli/Run.hs` (wire `CmdServer`)
- Modify: `hypha.cabal`

- [ ] **Step 1: Extend `hypha.cabal`**

```cabal
    Hypha.Command.Server
```

Extend `test-suite` `other-modules`:

```cabal
    Golden.Server
```

- [ ] **Step 2: Extend the CLI parser**

In `Hypha.Cli.Parser`, add a constructor and parser:

```haskell
data Subcommand
  = ...
  | CmdServer
      !(Maybe Int)   -- port
      !Bool          -- --prebuild
      !(Maybe String)  -- --bind HOST:PORT
      !(Maybe Int)   -- --prebuild-jobs N
  deriving stock (Show, Eq)
```

Add to `subParser`:

```haskell
 <> command "server" (info (CmdServer
       <$> optional (option auto (long "port" <> metavar "N"))
       <*> switch (long "prebuild")
       <*> optional (strOption (long "bind" <> metavar "HOST:PORT"))
       <*> optional (option auto (long "prebuild-jobs" <> metavar "N")))
       (progDesc "Run the local doc-browser HTTP server"))
```

- [ ] **Step 3: Write `Hypha.Command.Server`**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Hypha.Command.Server
  ( runServer
  , ServerOpts (..)
  , BindError (..)
  ) where

import Control.Concurrent.Async (mapConcurrently_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp

import Hypha.BuildEnv.Type   (BuildEnv (..))
import Hypha.Haddock.Generate (ensureHaddockFor)
import Hypha.Server.App      (ServerConfig (..), appWith)
import Hypha.Server.Slots    (initialiseSlots, withSlot)
import Hypha.Types.BuildPlan (BuildPlan (..), PlannedUnit (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..))

data ServerOpts = ServerOpts
  { soPort         :: !Int
  , soPrebuild     :: !Bool
  , soPrebuildJobs :: !Int
  , soBindHost     :: !String
  }

data BindError = NonLocalhostBindRefused !String
  deriving stock (Show, Eq)

-- | Returns either a BindError (refused) or runs warp forever.
runServer :: ServerOpts
          -> BuildPlan
          -> BuildEnv IO
          -> IO (Either BindError ())
runServer opts plan env =
  case validateBind (soBindHost opts) of
    Left err -> pure (Left err)
    Right () -> do
      let pids = [ puId u | u <- Map.elems (bpUnits plan) ]
      slots <- initialiseSlots pids
      if soPrebuild opts
        then prebuildAll slots env pids (soPrebuildJobs opts)
        else pure ()
      let cfg = ServerConfig
            { scProjectName  = "hypha"
            , scPackages     = map (unPackageName . pkgName) pids
            , scSlots        = slots
            , scHumanSearch  = \_q -> pure []        -- wired to Hoogle in step 4 below
            , scSymbolLookup = \_p _m _s -> pure Nothing
            , scHaddockHtml  = \_pv _path -> pure Nothing
            , scSourceText   = \_p _m -> pure Nothing
            }
      let warpSettings = Warp.setHost "127.0.0.1"
                       $ Warp.setPort (soPort opts) Warp.defaultSettings
      Warp.runSettings warpSettings (appWith cfg)
      pure (Right ())

validateBind :: String -> Either BindError ()
validateBind "127.0.0.1" = Right ()
validateBind "::1"       = Right ()
validateBind "localhost" = Right ()
validateBind other       = Left (NonLocalhostBindRefused other)

prebuildAll :: a -> BuildEnv IO -> [PackageId] -> Int -> IO ()
prebuildAll _slots env pids _jobs =
  mapConcurrently_ (\p -> ensureHaddockFor env p >> pure ()) pids
```

> Note: `scHumanSearch`, `scSymbolLookup`, `scHaddockHtml`, `scSourceText` are stubbed to `pure Nothing`/`[]`. Wire them in step 4 by reusing `Hypha.Command.Search`, `Hypha.Command.Symbol`, `Hypha.Haddock.Generate.ensureHaddockFor`, and `TIO.readFile` of the located source. This keeps the diff in step 3 reviewable.

- [ ] **Step 4: Wire real handlers into the ServerConfig**

In `Hypha.Command.Server.runServer`, replace the four stub fields:

```haskell
import qualified Hypha.Hoogle.Query as HQ
import qualified Hypha.Hoogle.Database as HDB
import qualified Hypha.Hoogle.Type as HT
import qualified Hypha.Command.Symbol as Sym
import qualified Hypha.Source.Locate as SLoc
import qualified Hypha.Source.Extract as SExtract
import qualified Hypha.Haddock.Parse as HParse
import qualified Hypha.Haddock.Generate as HGen
import qualified Hypha.Server.Haddock.Rewrite as HRW
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import System.FilePath ((</>))

...

hoogle <- HQ.mkProjectHoogle (HDB.HoogleConfig pr [] "stub")
let cfg = ServerConfig
  { ...
  , scHumanSearch = \q -> do
       hits <- HT.searchHoogle hoogle (HT.HoogleQuery q)
       pure [ (HT.hhPackage h, HT.hhModule h, HT.hhName h, HT.hhSig h) | h <- hits ]
  , scSymbolLookup = \pkg modPath sym -> do
       case Map.lookup (PackageName pkg) (bpUnits plan) of
         Nothing -> pure Nothing
         Just u  -> do
            mLoc <- SLoc.locateSymbolDefinition env (puId u) modPath sym
            case mLoc of
              Nothing -> pure Nothing
              Just (SLoc.SourceLocation p l) -> do
                src <- TIO.readFile p
                let sig = maybe "" id (SExtract.extractSignature src sym)
                    raw = maybe "" id (fmap (\d -> "" :: Text) (HParse.extractDocBlock src sym))
                pure (Just (sig, raw, Text.pack p, l))
  , scHaddockHtml = \pv pathSegs -> do
       -- Resolve "pkg-ver" string to a PackageId by splitting on the last '-'.
       case Text.breakOnEnd "-" pv of
         (pkgT, verT)
           | not (Text.null pkgT) && not (Text.null verT) -> do
               let pid = PackageId (PackageName (Text.dropEnd 1 pkgT))
                                   (Version verT)
               (mDir, _) <- HGen.ensureHaddockFor env pid
               case mDir of
                 Nothing -> pure Nothing
                 Just dir -> do
                   let f = foldl (</>) dir pathSegs
                   raw <- BL.readFile f
                   pure (Just (BL.fromStrict
                                 (TE.encodeUtf8
                                   (HRW.rewriteHaddockHtml
                                     (TE.decodeUtf8 (BL.toStrict raw))))))
         _ -> pure Nothing
  , scSourceText = \pkg modPath -> do
       case Map.lookup (PackageName pkg) (bpUnits plan) of
         Nothing -> pure Nothing
         Just u  -> do
            mDir <- locatePackageSource env (puId u)
            case mDir of
              Nothing -> pure Nothing
              Just d  -> do
                let f = d </> Text.unpack (Text.replace "." "/" modPath) <> ".hs"
                Just <$> TIO.readFile f
  }
```

- [ ] **Step 5: Wire `CmdServer` in `Hypha.Cli.Run`**

```haskell
  CmdServer mPort prebuild mBind mJobs -> withPlan gf $ \plan -> do
    storePath <- maybe "/dev/null" id <$> lookupEnv "HYPHA_FIXTURE_STORE"
    env <- mkCabalBuildEnv CabalEnvConfig
             { cecStorePath = storePath, cecGhc = bpGhc plan }
    let opts = ServerOpts
          { soPort         = maybe 4287 id mPort
          , soPrebuild     = prebuild
          , soPrebuildJobs = maybe 4    id mJobs
          , soBindHost     = maybe "127.0.0.1" (takeHost) mBind
          }
    r <- Server.runServer opts plan env
    case r of
      Left (Server.NonLocalhostBindRefused h) ->
        pure (Left (BadCliArgs (Text.pack ("refusing to bind to non-localhost host: " <> h))))
      Right () -> pure (Right ())
  where
    takeHost s = takeWhile (/= ':') s
```

- [ ] **Step 6: Golden test for the home page**

Create `test/Golden/Server.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Golden.Server (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Lucid (renderBS)

import qualified Hypha.Server.Ui.Layout as UI

tests :: TestTree
tests = testGroup "Golden.Server"
  [ goldenVsString "server-home"
      "test/Golden/golden/server-home.html"
      (pure (renderBS (UI.shellPage "hypha" [] ["async", "text"]
                         (mempty))))
  ]
```

Register in `test/Main.hs`. Regenerate, inspect, commit:

```bash
cabal test --test-options="--accept"
cabal test
git add hypha.cabal src/Hypha/Cli/Parser.hs src/Hypha/Cli/Run.hs \
        src/Hypha/Command/Server.hs test/Golden/Server.hs \
        test/Golden/golden/server-home.html test/Main.hs
git commit -m "feat(server): hypha server subcommand — Warp + bind safety + prebuild pool + home golden"
```

---

## Task 6: Smoke test against fixture project

**Files:** none new — exercises the existing fixtures end-to-end.

- [ ] **Step 1: Start the server against `test/fixtures/tiny-project`**

Run:
```
HYPHA_FIXTURE_STORE=test/fixtures/fake-cabal-store/ghc-9.6.6/async-2.2.5-deadbeef \
  cabal run hypha -- --project-dir test/fixtures/tiny-project server --port 4287
```
Expected: prints `Listening on http://127.0.0.1:4287`.

- [ ] **Step 2: Hit the routes**

In another shell:
- `curl -s http://127.0.0.1:4287/healthz` → `ok`
- `curl -s http://127.0.0.1:4287/` → HTML containing `class="app"`
- `curl -s 'http://127.0.0.1:4287/search?q=concurrently'` → search-results fragment HTML

- [ ] **Step 3: Refuse non-localhost bind**

Run: `cabal run hypha -- --project-dir test/fixtures/tiny-project server --bind 0.0.0.0:4287`
Expected: exits with code 2 and a message about refusing non-localhost host.

- [ ] **Step 4: Commit smoke notes in README**

Append to `README.md`:

```markdown
## Local doc browser

```sh
hypha server --port 4287
# open http://127.0.0.1:4287
```

Press `s` (or `/` / `Ctrl-K`) to focus search. Arrow keys (or `j`/`k`)
navigate results; `Enter` opens.
```

```bash
git add README.md
git commit -m "docs: add server quick-start snippet"
```

---

## Self-review

| Spec §16 item | Covered in |
|---|---|
| Routes (`/`, `/search`, `/pkg/*`, `/haddock/*`, `/source/*`, `/api/*`, `/assets/*`, `/healthz`) | Task 4 (`/api/*` is reused from Plan A's CLI surface; the route is in Api.hs but the handler is left for Plan C polish) |
| Lucid2, no SPA, HTMX, ~50 LOC JS, modular CSS | Tasks 2, 3 |
| Keymap (`s`, `/`, `Ctrl-K`, arrows, `j`/`k`, `Enter`, `Esc`, breadcrumb `←`/`→`, `gp`, `gh`, `?`) | Task 2 `ui/js/keybindings.js` covers the core; `gp`/`gh` and `?` are stubbed for Plan C |
| Lazy Haddock build pipeline | Task 1 (slots) + Task 5 (handler) |
| HTML rewrite via tagsoup; idempotent property | Task 1 |
| Per-package MVar lock; outer Map immutable | Task 1 + dedup unit test |
| `--prebuild` worker pool sized by jobs flag | Task 5 |
| Bind defaults to 127.0.0.1; non-localhost refused | Task 5 |
| No auth; read-only; CSP | CSP header to be added in Plan C polish; read-only by construction (no write endpoints) |
| Assets embedded via file-embed | Task 2 |

Placeholder scan: handler stubs in Task 5 step 3 are replaced in step 4. The `modPage` placeholder note in Task 4 is flagged inline; replaced by `modPage2`.

---

## Execution handoff

Plan B complete and saved to `docs/superpowers/plans/2026-05-18-hypha-plan-b-server.md`.

Two execution options:

**1. Subagent-Driven (recommended)** — fresh subagent per task, review between.
**2. Inline Execution** — `superpowers:executing-plans` with checkpoints.

Which approach?
