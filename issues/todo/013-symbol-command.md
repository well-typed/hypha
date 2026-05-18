# Task 10.4: `symbol` Command

**Status:** todo  
**Priority:** P1  
**Blocked by:** Task 10.3  
**Commit:** `feat(cmd): symbol command — signature, haddock_raw, source coords; golden`

## Goal
Implement `hypha symbol <PKG/MOD/SYM>` returning signature, haddock raw text, and source coordinates.

## Files to Create
- `src/Hypha/Command/Symbol.hs`
- `src/Hypha/Types/Doc.hs` (DocText newtype)
- `src/Hypha/Haddock/Parse.hs` (extractDocBlock)
- `src/Hypha/Haddock/Interface.hs` (stub hasInterfaceFile)
- `src/Hypha/Source/Extract.hs` (extractSignature)

## Files to Modify
- `src/Hypha/Cli/Run.hs` — add `CmdSymbol` arm
- `test/Golden/Commands.hs` — add `symbol-async-concurrently` golden
- `test/Golden/golden/symbol-async-concurrently.compact.json`
- `hypha.cabal` — expose `haddock-library`, new modules
- `test/Main.hs`

## Acceptance Criteria
- [ ] Extracts `::` signature line
- [ ] Extracts Haddock doc block preceding definition
- [ ] Returns source path + line number
- [ ] Golden test passes with concurrently fixture
