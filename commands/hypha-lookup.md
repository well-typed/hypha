---
description: Resolve a Haskell symbol or type signature via the hypha CLI (cache → local Hoogle → remote).
argument-hint: <symbol-or-signature>
---

Run `hypha lookup $ARGUMENTS` from the current project root and report the
result. If the user passed a type signature (contains `->` or `=>`), quote
it. The default output is already compact — `query` plus `providers`, each
candidate carrying its `pkg`, `mod`, `name`, `sig` and `tier` — so pass
`--select providers` only if you want to drop `query`, and never
`--select sig` (that is a top-level field of `hypha symbol`; here the
signatures sit inside each provider, and selecting it yields an empty
result).
