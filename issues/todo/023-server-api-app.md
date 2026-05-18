# Task 23: Servant API + WAI App

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 22  
**PR:** One PR  
**Commit:** `feat(server): servant API + WAI app stitching`

## Goal
Define the Servant API type and wire all UI views into a WAI application with route handlers.

## Files to Create
- `src/Hypha/Server/Api.hs` — `HyphaApi` type with all routes, `HTML` content type
- `src/Hypha/Server/App.hs` — `ServerConfig` record, `appWith` function, route handlers

## Files to Modify
- `hypha.cabal` — add `warp`, `wai`, `wai-extra`, `servant-server`, `http-media`, `blaze-html`

## Routes
- `GET /` — home page
- `GET /search?q=` — search results fragment (HTMX target)
- `GET /pkg/:pkg` — package page
- `GET /pkg/:pkg/:mod` — module page
- `GET /pkg/:pkg/:mod/:sym` — symbol card
- `GET /haddock/:pkgver/*path` — rewritten Haddock HTML
- `GET /source/:pkg/:mod` — source view
- `GET /assets/style.css` — embedded CSS
- `GET /assets/htmx.min.js` — embedded HTMX
- `GET /assets/keybindings.js` — embedded keybindings
- `GET /healthz` — health check

## Acceptance Criteria
- [ ] `HyphaApi` type-level API compiles
- [ ] `HTML` content type wraps Lucid `Html ()`
- [ ] `ServerConfig` holds callbacks for search, symbol lookup, haddock, source
- [ ] `appWith` produces a `Wai.Application`
- [ ] All route handlers wired
- [ ] Build succeeds
