# 037: `hypha lookup` — single tiered symbol-resolution command

Collapse `hypha search` and `hypha whatprovides` into a single tiered
command `hypha lookup`, with a working local Hoogle DB + remote Hoogle
fallback.  Drops `--global`.  Breaking 0.2.0 bump.

- Spec: [`docs/superpowers/specs/2026-05-20-hypha-lookup-design.md`](../../docs/superpowers/specs/2026-05-20-hypha-lookup-design.md)
- Plan: [`docs/superpowers/plans/2026-05-20-hypha-lookup.md`](../../docs/superpowers/plans/2026-05-20-hypha-lookup.md)
