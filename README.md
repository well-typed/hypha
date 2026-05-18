# hypha

> An agent-first Haskell CLI that probes Hackage / Hoogle / your cabal build plan.

`hypha` (Greek *hyphē*: the branching threadlike cell of a fungus that probes
through substrate seeking nutrients) is a command-line tool for browsing
Hackage and Hoogle without leaving the terminal — designed first for AI agents
(Claude Code, opencode, Pi, …) and second for the humans they help.

It is **project-aware**: queries default to the versions your `cabal.project`
actually builds against (parsed from `dist-newstyle/cache/plan.json`), with
`--any` to widen and `--package-override` to escape-hatch.

## Mantra

> An LLM doesn't need or care about fancy Haddock HTML pages — it cares about
> the source code, which also contains the comments (the documentation). That
> is what an LLM needs in order to learn knowledge of a project. Humans need
> visuals.

## Status

Pre-alpha. See `docs/superpowers/specs/2026-05-18-hypha-design.md` for the
design.

## License

BSD-3-Clause. See `LICENSE`.
