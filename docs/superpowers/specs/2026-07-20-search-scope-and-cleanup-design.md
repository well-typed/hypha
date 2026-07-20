# Search Cleanup + Package-Scoped Search — Design

## Context

The `hypha server` home page currently shows two "hint chips" claiming keybindings that don't exist:

```
/ or Ctrl-K to search
↑↓ to pick, Enter to jump
```

None of this is real. `ui/js/keybindings.js` implements exactly one behavior — `Escape` blurs the search input — and nothing else. There is no `/`-to-focus shortcut, no `Ctrl-K` handler, no arrow-key result navigation, no `Enter`-to-jump. Worse, the search input already carries the HTML `autofocus_` attribute (`src/Hypha/Server/Ui/Search.hs:38`), so it's already focused on page load — the hints aren't just unimplemented, they're actively misleading (the first one describes a keybinding to reach a state the page is already in).

Investigating the codebase for what backs these claims turned up a second piece of dead code: `ui/css/components/search.css` has a `.results li.cursor` rule with the comment "Keyboard cursor row (keybindings.js moves the .cursor class)" — but `keybindings.js` never sets that class anywhere. Arrow-key result navigation was never built; only its CSS scaffolding exists.

Separately, the user wants a genuinely new capability: restrict the fuzzy search to just the package currently being viewed (à la Hackage's per-package search), analogous to a token/filter-chip search UI (select2-style): press `Tab` while the search box is focused to add a small pill showing the current package's name; while it's present, search results are filtered to that package; click its `×` (or press Backspace when the query is empty) to remove it and return to global search.

## Scope

Two pieces of work, bundled because they touch the same UI surface:

1. **Cleanup (no design ambiguity):** delete the fake hint chips and the dead `.cursor` CSS + its misleading comment.
2. **New feature:** package-scoped search via a Tab-triggered chip.

## Part 1: Cleanup

- Delete the `div_ [class_ "hint-chips"] $ ...` block in `homePage` (`src/Hypha/Server/App.hs:114-124`).
- Delete the now-orphaned `.hint-chips` / `.hint-chip` rules in `ui/css/layout.css:198-212`. (Verified via grep: these classes and the `kbd_` Lucid calls appear nowhere else in the codebase.)
- Delete the `.results li.cursor { ... }` rule and its preceding comment in `ui/css/components/search.css` (dead — no JS ever applies `.cursor`).
- **Explicitly not doing:** implementing real arrow-key result navigation. The CSS scaffolding being deleted proves someone once intended it, but it was never finished, and it wasn't asked for here. A future request can revisit it as its own feature.
- **Explicitly not touching:** the generic `kbd { ... }` element style in `ui/css/base.css:121-131`. It's a reusable base-typography primitive for the standard `<kbd>` tag, not itself tied to the fake feature — leaving it in place even though nothing currently renders a `<kbd>` element.

## Part 2: Package-Scoped Search

### Detecting the current package

No server-side changes are needed to know "what package is this page about" — every relevant route already encodes it in the URL (`/pkg/:pkg`, `/pkg/:pkg/:mod`, `/pkg/:pkg/:mod/:sym`, `/source/:pkg/:mod`). A small client-side helper parses `location.pathname` against `/^\/(?:pkg|source)\/([^/]+)/`, URL-decodes the captured group, and returns `null` on `/` (home) or any path that doesn't match (there is currently no other page shape).

### The chip lifecycle

Lives in `ui/js/keybindings.js` (already owns keydown handling; no new JS file).

- **Add (Tab):** when the search input is focused, a current package is detected from the URL, and no chip is currently shown → `preventDefault()`, build the pill (package name text + a `×` button) and insert it into `.search-wrap` immediately before the `<input class="search-input">`; set the hidden scope input's (see below) value to the package name; move focus back to the search input if the browser had started to move it.
- **Fall-through (Tab, dead-end cases):** on the home page (no package to scope to), or when a chip already exists, Tab is **not** intercepted — normal focus navigation proceeds, so keyboard users are never trapped in the search box.
- **Remove:** clicking the pill's `×`, or pressing Backspace while the query is empty and the cursor is at position 0 — either clears the pill from the DOM and resets the hidden scope input's value to empty.
- **No persistence across navigation.** Every page in this app is a full browser navigation (no `hx-boost` on nav links), so client-side DOM/JS state doesn't survive a page load unless deliberately persisted — and it deliberately isn't here. Landing on a new page always starts unscoped; press Tab again on that page to scope it. This sidesteps any "stale scope on an unrelated package" problem by construction, at the cost of not being sticky across a session.

### Wiring the filter into the live-search request

- `Hypha.Server.Ui.Search.searchInput` (`src/Hypha/Server/Ui/Search.hs:26-53`) gains one new element: `input_ [type_ "hidden", name_ "pkg", class_ "search-scope-value"]` (empty value by default), placed inside `.search-wrap`.
- The visible `<input class="search-input">` gains `hx-include=".search-scope-value"` alongside its existing `hx-get`/`hx-trigger`/`hx-target`/`hx-swap`, so htmx serializes both `q` and `pkg` on every keyup-triggered request.
- `Hypha.Server.Api`'s `/search` route (`Api.hs:60`) gains a second query parameter: `"search" :> QueryParam "q" String :> QueryParam "pkg" String :> Get '[HTML] (Html ())`.
- `Hypha.Server.App.searchPage` (`App.hs:143-154`) takes the extra `Maybe String` parameter. After `rows <- liftIO (scHumanSearch cfg q)`, apply a filter: when the `pkg` parameter is `Just p` and `p` is non-empty, keep only rows whose package component equals `Text.pack p`; when it's `Nothing` or `Just ""`, keep all rows unfiltered. (The empty-string case matters and is easy to get wrong: Servant parses a present-but-valueless `?pkg=` query key as `Just ""`, not `Nothing` — that's exactly what happens when the chip has been removed and the hidden input's now-empty value still gets included in the request. Both `Nothing` and `Just ""` must mean "no scope".)

### Styling

- New `.search-scope` (the pill) and `.search-scope .remove` (the `×`) rules in `ui/css/components/search.css`, visually modeled on the existing `.origin-pill` pattern (rounded pill, muted background, small font) rather than introducing a new visual language.
- `.search-wrap` is already `display: flex; align-items: center;`, so the chip becomes another flex child ahead of the input — no layout restructuring needed.

## Testing

- **Unit test:** the pkg-filter predicate used by `searchPage` should be small and pure enough to extract and unit-test directly (e.g. `scopeFilter :: Maybe String -> (Text,Text,Text,Text) -> Bool` or equivalent) — covering `Nothing`, `Just ""`, and `Just "somepkg"` against matching/non-matching rows. Exact extraction shape is left to the implementation plan.
- **Golden test:** `test/Golden/golden/server-home.html` will change (hint chips removed) and must be regenerated via `cabal test all --test-options="--accept"`, diff-reviewed by eye (established convention in this repo).
- **Manual/unverifiable-here:** the Tab-to-add-chip, `×`-to-remove, Backspace-to-remove, and fall-through-on-dead-ends interactions all require a real browser with JS execution and are not verifiable by an automated check in this environment. Flagged as an open item for manual verification, same as the last two features.

## Out of Scope

- Real arrow-key (`↑`/`↓`) result navigation and `Enter`-to-jump. The dead CSS being removed hints this was once planned, but it isn't part of this request.
- Persisting scope across page navigation (sessionStorage-based stickiness) — considered and explicitly rejected in favor of the simpler per-page-load reset.
- Any change to the Fuzzy search backend (`Hypha.Search.Fuzzy`) — scoping is a pure post-filter on already-returned rows, not a change to how matches are ranked or found.
