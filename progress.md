# Progress

## Status
QA in Progress (TICKET-mqr-02 signed off PASS with 2 advisories)

## Tasks
- TICKET-mqr-02 — QA re-derivation (v2 R4) complete; verdict PASS, see `reports/TICKET-mqr-02-qa.md`
  - 11/26 PASS rows re-checked (42% sample) — all survived
  - 0/0 FIX rows trivially passes (0 FIXes in ticket)
  - R1: 0 net new `hPutStrLn stderr "warning: "` sites (verified: 1 strict, 3 wide, before==after)
  - R2: 0 `Text.pack . show` on typed sub-errors (2 hits are on `Integer`/`Int` numbers, R2 N/A)
  - R3: 26-row verdict table with rubric enumeration; 0 `PASS / No issues found`; no REVIEW_SUMMARY.md
  - Build: green, 157/157 tests pass (run by QA in throwaway worktree)
- TICKET-mqr-03 — QA re-derivation (v2 R4) complete; verdict PASS, see `reports/TICKET-mqr-03-qa.md`
  - 6/15 PASS rows re-checked (40% sample) — all survived
  - 3/3 FIX rows re-checked — all consistent with dev claims
  - R1: -2 inline warning call sites (verified)
  - R2: FIX 5 (displayException swap) verified; no remaining `Text.pack (show err)` on exceptions
  - R3: 18-row verdict table with rubric enumeration; no REVIEW_SUMMARY.md
  - Build: green, 157/157 tests pass

## Files Changed
- reports/TICKET-mqr-02-qa.md (new)
- reports/TICKET-mqr-03-qa.md (new)

## Notes
- Build/test evidence (TICKET-mqr-02): `cabal build all` exit 0; `cabal test all` exit 0 (157 tests)
- TICKET-mqr-02 advisories for lead's design review:
  - Source/Extract.hs:46's "snippet parse" rationale is factually wrong (all 4 callers pass full file source) — site should be added to follow-up "6 sites" -> "7 sites"
  - BuildEnv/Cabal.hs has a small in-file `listDirectory + find (isPrefixOf prefix)` duplication between `findInStoreEntry` and `locateHaddock` that the dev could have extracted as a 4-line helper without violating any layering
- Dev's "0 FIXes" verdict on TICKET-mqr-02 is principled: 7 silent-swallow sites + 2 bytestring round-trip sites all require either a `Hypha.Logging` move (cycle-breaking) or a cache-schema refactor (out of small-fix budget)
- Build/test evidence (TICKET-mqr-03): `cabal build all` exit 0; `cabal test all` exit 0 (157 tests)
- Dev's local `warnOnLeft` copies (3-line duplication) flagged for lead decision (advisory)
- Strict-vs-substantive line count interpretation flagged for lead decision (advisory)
- No regression test for progress-bar / stderr interaction (coverage gap, not a defect)

## TICKET-mqr-01 QA (2026-06-02)

**Verdict: PASS**

- Throwaway worktree: `.worktrees/qa-mqr-01` (detached HEAD at 4e4d9fa); removed after verdict
- Build/test: `cabal build all` exit 0; `cabal test all` exit 0 (157 tests)
- R4 sample: 3/3 FIX rows re-checked (100%); 7/13 PASS rows sampled (54%) — rows 2, 5, 7, 8, 11, 13, 16
- R1: `"warning: "` site count -1 (accurate); all Helper Discovery citations real
- R2: 0 new `Text.pack . show` sites in dev's diff
- R3: 16-row verdict table, no `REVIEW_SUMMARY.md`, all PASS rows enumerated
- Adversarial: no caller breakage; `restrictByKeys` preserves key order (golden tests pass); `mkAction`/`restrictByKeys` are file-local (not exported)
- Defects NOT bounced on (with rationale): (1) line-number drift in verdict citations; (2) `Output/Actions.hs` functions are dead code in public API (FOLLUPMQR-03); (3) `VersionsCommand` warning lost the `"hackage availability lookup failed: "` prefix when converted to `warnOnLeft` (informational, not a regression)

## Files Changed
- reports/TICKET-mqr-01-qa.md (new)
