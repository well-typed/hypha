# Task 21: UI Assets + Embedded Asset Module

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 20  
**PR:** One PR  
**Commit:** `feat(server): vendor HTMX, ship modular CSS/JS/icons, embed via file-embed`

## Goal
Create all UI assets (CSS, JS, SVG icons) and embed them into the binary at compile time using `file-embed`.

## Files to Create
- `ui/css/base.css` — CSS variables, light/dark theme, typography
- `ui/css/layout.css` — grid layout (topbar, sidebar, main)
- `ui/css/components/search.css` — search input + results list
- `ui/css/components/tree.css` — package/module tree
- `ui/css/components/doc.css` — symbol card styling
- `ui/js/keybindings.js` — vanilla JS keybindings (`s`, `/`, `Ctrl-K`, `j`/`k`, arrows, `Enter`, `Esc`)
- `ui/js/htmx.min.js` — vendored HTMX 1.9.12 (BSD/Zero-clause)
- `ui/icons/search.svg`
- `ui/icons/package.svg`
- `ui/icons/module.svg`
- `src/Hypha/Server/Assets.hs` — `file-embed` Template Haskell module

## Files to Modify
- `hypha.cabal` — add `file-embed`; add `extra-source-files` for UI assets

## Acceptance Criteria
- [ ] `cssBundle` concatenates all CSS files
- [ ] `htmxJs`, `keybindingsJs` embed JS
- [ ] `iconSearch`, `iconPackage`, `iconModule` embed SVGs
- [ ] All assets embedded at compile time (no CDN at runtime)
- [ ] Build succeeds with embedded assets
