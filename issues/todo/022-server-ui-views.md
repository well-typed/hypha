# Task 22: Lucid2 UI Views

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 21  
**PR:** One PR  
**Commit:** `feat(server): lucid2 views — shell, search, tree, doc, source`

## Goal
Create Lucid2 HTML view components for the server UI: shell layout, search results, package tree, symbol card, and source view.

## Files to Create
- `src/Hypha/Server/Ui/Layout.hs` — `shellPage`: topbar + sidebar + main pane
- `src/Hypha/Server/Ui/Search.hs` — `searchInput` (with HTMX attributes), `resultsFragment`
- `src/Hypha/Server/Ui/Tree.hs` — `packageTree`: sidebar package list
- `src/Hypha/Server/Ui/Doc.hs` — `symbolCard`: name + signature + haddock + source link
- `src/Hypha/Server/Ui/Source.hs` — `sourceView`: preformatted source code

## Files to Modify
- `hypha.cabal` — add `lucid2`; expose new modules

## Acceptance Criteria
- [ ] `shellPage` renders full HTML document with CSS/JS links, topbar, sidebar, main
- [ ] `searchInput` has HTMX attributes for live search (`hx-get`, `hx-trigger`, `hx-target`)
- [ ] `resultsFragment` renders search results as `<ul>` with links
- [ ] `packageTree` renders sidebar list of packages
- [ ] `symbolCard` renders name, signature, haddock HTML, source link
- [ ] `sourceView` renders preformatted code
- [ ] Build succeeds
