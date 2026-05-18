# Task 24: `hypha server` Command + Prebuild Worker Pool + Bind Safety

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 23  
**PR:** One PR  
**Commit:** `feat(server): hypha server subcommand — Warp + bind safety + prebuild pool + home golden`

## Goal
Implement the `hypha server` CLI subcommand with `--port`, `--prebuild`, `--bind`, `--prebuild-jobs` flags. Bind to localhost only; refuse non-localhost with exit 2.

## Files to Create
- `src/Hypha/Command/Server.hs` — `runServer`, `ServerOpts`, `BindError`
- `test/Golden/Server.hs` — golden test for home page HTML
- `test/Golden/golden/server-home.html` — golden output

## Files to Modify
- `src/Hypha/Cli/Parser.hs` — add `CmdServer` constructor
- `src/Hypha/Cli/Run.hs` — wire `CmdServer` arm
- `hypha.cabal` — expose `Hypha.Command.Server`
- `test/Main.hs` — register `Golden.Server`

## Acceptance Criteria
- [ ] `hypha server --port 4287` starts Warp on 127.0.0.1:4287
- [ ] `--prebuild` triggers `mapConcurrently_` Haddock generation for all plan packages
- [ ] `--prebuild-jobs N` controls concurrency
- [ ] `--bind localhost:4287` accepted
- [ ] `--bind 0.0.0.0:4287` refused with exit 2
- [ ] Default bind is 127.0.0.1
- [ ] Home page golden test passes
- [ ] `ServerConfig` callbacks wired to Hoogle, Symbol, Haddock, Source commands
