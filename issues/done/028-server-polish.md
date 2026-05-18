# Task 28: Server Polish — CSP Header, Keymap, Help Overlay

**Status:** todo  
**Priority:** P2  
**Blocked by:** Task 27  
**PR:** One PR  
**Commit:** `feat(server): CSP header, gp/gh/? keymap, breadcrumb nav, help overlay`

## Goal
Add Content-Security-Policy header, extend keybindings with `gp`/`gh`/`?`/breadcrumb navigation, and add a help overlay.

## Files to Modify
- `src/Hypha/Server/App.hs` — add `cspMiddleware` wrapping the WAI app
- `ui/js/keybindings.js` — add `gp` (packages), `gh` (home), `?` (help overlay), `h`/`l`/arrows (history)
- `src/Hypha/Server/Ui/Layout.hs` — add help overlay `<div>` with keyboard shortcut table
- `ui/css/components/doc.css` — add `.help-overlay` CSS rules

## Acceptance Criteria
- [ ] CSP header present: `default-src 'self'; img-src 'self' data:; style-src 'self' 'unsafe-inline'`
- [ ] `?` key toggles help overlay
- [ ] `gp` navigates to packages
- [ ] `gh` navigates to home
- [ ] `h`/`ArrowLeft` = history back, `l`/`ArrowRight` = history forward
- [ ] Help overlay shows keyboard shortcut table
- [ ] Build succeeds
