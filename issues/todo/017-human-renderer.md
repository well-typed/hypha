# Task 11: `--human` Renderer (DocH → ANSI, Signature Syntax Highlighting)

**Status:** todo  
**Priority:** P2  
**Blocked by:** Task 9 (dispatcher), Task 10.4 (symbol card data)  
**PR:** One PR  
**Commit:** `feat(human): DocH→ANSI renderer + skylighting signatures + symbol-card golden`

## Goal
Add `--human` output mode: render symbol cards with ANSI colours, Haddock DocH parsing, and skylighting syntax highlighting for signatures.

## Files to Create
- `src/Hypha/Output/Human.hs`
- `test/Golden/Human.hs`
- `test/Golden/golden/human-symbol-async-concurrently.ansi`

## Files to Modify
- `src/Hypha/Cli/Run.hs` — fork `emit` on `gfHuman`
- `hypha.cabal` — add `skylighting`, `skylighting-core`, `prettyprinter-ansi-terminal`, `haddock-library`
- `test/Main.hs`

## Acceptance Criteria
- [ ] `renderSymbolCard` produces a Prettyprinter `Doc AnsiStyle`
- [ ] `renderHaddock` parses DocH and outputs ANSI
- [ ] `renderSignature` uses skylighting for Haskell syntax highlighting
- [ ] `--human` flag routes through human renderer
- [ ] Human golden test passes
