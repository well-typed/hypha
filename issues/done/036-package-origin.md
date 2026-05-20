# 036: Surface package origin (Hackage / SRP / local) in server UI + JSON

## Motivation

A planned unit pinned via @source-repository-package@ at the same
`(pkg, ver)` as the Hackage copy is currently invisible.  Following
Option A from the design chat: show **one** version per plan (the
pinned one) but tag its provenance so users and agents can tell a
fork apart.

## Done

- `PackageOrigin` sum in `Hypha.Types.BuildPlan`: Hackage / SRP /
  Local / LocalTarball / RemoteTarball / Unknown.
- `puOrigin` field on `PlannedUnit`, populated from `plan.json`'s
  `pkg-src` block in `Hypha.Project.Plan.toPlannedUnit`.
- `rpOrigin` on `ResolvedPackage`; threaded through
  `Hypha.Command.Package.mkSuccessOutcome`.
- JSON outcome carries `"origin": { "kind": ..., ... }` with stable
  discriminator + optional URL/ref/path/subdir.
- Sidebar entries now `(Text, PackageOrigin)`; non-Hackage origins
  render a tiny pill (`srp@<sha>`, `local`, `tarball`).
- CSS additions for `.origin-tag.{srp,local,tarball}`.

## Out of scope

- Per-component origin differentiation (a library and its exe always
  share the same source, so the unit-level origin suffices).
- Project-cache key extension with commit hash (edge case: two projects
  pinning the same SRP `(pkg, ver)` at different commits).  Flagged in
  the package-cache work; not blocking.
