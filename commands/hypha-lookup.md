---
description: Resolve a Haskell symbol or type signature via the hypha CLI (cache → local Hoogle → remote).
argument-hint: <symbol-or-signature>
---

Run `hypha lookup $ARGUMENTS` from the current project root and report the
result. If the user passed a type signature (contains `->` or `=>`), quote
it. Use `--select sig,haddock` to keep output compact unless the user asks
for more.
