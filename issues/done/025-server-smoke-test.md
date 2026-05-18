# Task 25: Smoke Test Against Fixture Project

**Status:** todo  
**Priority:** P2  
**Blocked by:** Task 24  
**PR:** One PR  
**Commit:** `docs: add server quick-start snippet`

## Goal
End-to-end smoke test of the server against the fixture project, and document the server in README.

## Steps
1. Start server against `test/fixtures/tiny-project` with `HYPHA_FIXTURE_STORE` env var
2. Verify `curl http://127.0.0.1:4287/healthz` returns `ok`
3. Verify `curl http://127.0.0.1:4287/` returns HTML with `class="app"`
4. Verify `curl 'http://127.0.0.1:4287/search?q=concurrently'` returns search results
5. Verify `--bind 0.0.0.0:4287` exits with code 2
6. Add server quick-start snippet to README.md

## Acceptance Criteria
- [ ] Server starts and listens on localhost
- [ ] `/healthz` returns `ok`
- [ ] `/` returns valid HTML shell
- [ ] `/search?q=` returns search results fragment
- [ ] Non-localhost bind refused with exit 2
- [ ] README documents `hypha server` usage
