# Index class methods and data constructors

**Status:** todo
**Type:** bug
**Found by:** verification during `adinapoli/more-server-improvements`

## Problem

The search index has no row for a class method or a data constructor.

| symbol | rows in the whole index |
|---|---|
| `traverse` | 4 (none in `base` or `ghc-internal`) |
| `fmap`, `Just`, `mempty`, `liftA2` | **0** |

`GHC.Internal.Data.Traversable` yields a row for `Traversable` — the class
— and none for `traverse` or `sequenceA`. `Parser.findDecl` matches
top-level declarations, and a class's methods and a data type's
constructors are not top-level declarations, so the indexer has no
declaration to read a signature from and writes no row.

The export side already knows about them: `Traversable(..)` is recorded
with its subordinates, so resolution reaches the right module and then
finds nothing to describe.

`traverse` is the clean demonstration — its module parses and the class row
exists, so this is the method gap alone, not the CPP tail (issue: see the
07-27 design spec's §7).

## Fix

Teach `Hypha.Source.Parser` to emit class methods and data constructors as
declarations in their own right, with the signatures GHC already attaches
to them, and give them a `DeclKind` that says which they are so a card can
render "class method of `Traversable`".

The indexer then writes rows for them with no change: the resolution and
collapse layers already key on `(definition, name)`.

## Note (from issue 046)

The interface dump `Hypha.Source.Origins` now reads carries class methods
and constructors with a per-method origin — the `Data.Bits` fixture pins
`.&.` and `shiftL` — so the origin side of this is already answered. What
is still missing is a *declaration* to read a signature from, which is
what `Parser.findDecl` has to learn.

## Acceptance criteria

- `fmap`, `Just`, `mempty`, `traverse` all have rows, attributed to the
  module that declares the class or type.
- `hypha server` search finds `traverse`; the card shows its signature.
- A fixture pins a class with methods and a data type with constructors,
  including a record field.
- Remove the corresponding entry from `website/src/troubleshooting.md` and
  from `CHANGELOG.md`'s Known limitations.
