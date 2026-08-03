# Origin oracle follow-ups

**Status:** todo
**Type:** refactor
**Found by:** code review of `adinapoli/more-server-improvements` (see
`issues/done/046-ask-the-compiler-where-an-export-comes-from.md`)

Three things the review raised that the branch deliberately did not take.
None of them produces a wrong answer today; each removes a way to get one.

## 1. Take import directories from `plan.json`, not from `ghc-pkg`

`plan.json` records the exact unit id per package
(`aeson-2.2.5.0-e939f13b06…`), and `PlannedUnit` already carries
`puDistDir` for local ones. `<store>/<abi-tag>/<unit-id>/lib` *is* the
import directory, exactly — no subprocess, no ambiguity.

`ghc-pkg field <pkg>-<ver> import-dirs` answers a *version*-level
question, so it prints two lines when the store holds one version under
two hashes (confirmed here for `aeson-2.2.4.1`). `firstExisting` then
takes whichever exists first, and both do. Exports can be flag-dependent,
so that can be the wrong build.

Doing this removes one subprocess per package and makes the
caller-supplied hook (`known`) the only path rather than the local-package
special case.

## 2. `ciOriginFailures` is a staging field

`ComponentIndex.ciOriginFailures` is documented as "empty until
`repairUnresolved` has run" — a representable state that means nothing. A
distinct return type from `repairUnresolved`, or a stage tag on the
component index, makes "not yet repaired" unrepresentable, per the ethos
section of `CLAUDE.md`.

## 3. `Fixture.Bystander` does not exist

`test/fixtures/reexport/src/Fixture/{Blind,Shadowed}.hs` import
`Fixture.Bystander`, which is nowhere on disk. The tests pass and prove
the right thing — an absent module resolves through `lookupExport` exactly
like a real-but-non-supplying one — but a reader cannot tell which
property is under test. `Fixture.ViaFacade` shows the shape to copy: a
losing candidate that genuinely exists and simply lacks the name.
