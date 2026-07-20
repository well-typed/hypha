# Hypha Docs Site — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans
> to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax.
> **Adaptation:** this is a static content site (no runtime logic). Per the
> spec, "the build is the test." Each task ends with a structure/link check
> (`website/check-book.py`, no network) + commit, not a red-green TDD cycle.

**Goal:** Split the 21 KB README into a lean what/why README plus an mdBook
documentation site published via GitLab Pages.

**Architecture:** All docs prose migrates from the current (YAML-updated)
README into an mdBook project under `website/`. A lean README links to it.
GitLab CI builds the book (downloaded mdBook binary, no Rust toolchain) and
publishes `public/` to GitLab Pages on `main`.

**Tech Stack:** mdBook (Rust, prebuilt binary), Markdown, custom CSS theme,
GitLab CI/Pages. Python 3 (stdlib only) for the local structure checker.

## Global Constraints

- Design spec: `docs/superpowers/specs/2026-07-20-hypha-docs-site-design.md`.
- **Output reality (verified against code):** default output is compact
  **YAML** (`result:` + `actions:`, no `schema`/`command`/`ok`/`in_plan`);
  `--json` is opt-in. `--human` does **not** exist. `--cache-dir DIR` is a
  global flag. `mcp` is **not** a subcommand — only the `hypha-mcp` binary.
  Identifier versions use a **hyphen** (`async-2.2.6`), never `@`.
- Authoritative sources: current `README.md`, `src/Hypha/Cli/Parser.hs`,
  `src/Hypha/Types/PackageId.hs`.
- No new Haskell deps; docs-only change. GHC CI jobs untouched.
- mdBook version pinned in CI via `MDBOOK_VERSION`.
- Commit style: Conventional Commits; `docs:` scope. Co-author trailer.
- No `theme/index.hbs` override (full-template override is unverifiable
  without the binary and would break the site). Theme via `additional-css`
  + favicon files only.

---

### Task 1: Scaffold the mdBook project (book.toml + SUMMARY + checker)

**Files:**
- Create: `website/book.toml`
- Create: `website/src/SUMMARY.md` (full chapter tree; stub files follow)
- Create: `website/check-book.py` (stdlib-only structure/link checker)
- Create: `website/.gitignore` (ignore `book/` build output)

**Interfaces:**
- Produces: `check-book.py` — run as `python3 website/check-book.py`;
  exits non-zero if any `SUMMARY.md` link, image, or intra-doc link points
  at a missing file, or if `book.toml` is invalid TOML.

- [ ] **Step 1:** Write `book.toml`: `[book] title="Hypha"`, authors,
  `src="src"`; `[output.html]` with `default-theme="light"`,
  `preferred-dark-theme="navy"`, `git-repository-url` → GitLab repo,
  `additional-css=["hypha.css"]`, `[output.html.fold] enable=true`,
  `[output.html.search] enable=true`. No `site-url` yet (relative links
  work; set once Pages URL known).
- [ ] **Step 2:** Write `SUMMARY.md` with the full tree (see spec layout):
  Introduction; Getting Started (installation, quick-start, claude-plugin);
  Guide (identifiers, subcommands, lookup, flags, exit-codes, caching);
  Doc Browser Server (index, reference); MCP Integration (index); Design
  (philosophy, architecture); Troubleshooting; Etymology.
- [ ] **Step 3:** Write `check-book.py`: parse `SUMMARY.md` for
  `](path.md)` links; assert each file exists; scan every `src/**/*.md`
  for `](relative)` and `<img src="...">` local refs and assert targets
  exist; parse `book.toml` with `tomllib`.
- [ ] **Step 4 (test):** `python3 website/check-book.py` → expected FAIL
  (chapter files not created yet) listing the missing files.
- [ ] **Step 5:** Do not commit yet (checker fails by design). Proceed to
  Task 2 which creates the files; commit at end of Task 2.

### Task 2: Migrate content into chapter files

**Files (all Create under `website/src/`):**
- `README.md` (Introduction / landing), `getting-started/installation.md`,
  `getting-started/quick-start.md`, `getting-started/claude-plugin.md`,
  `guide/identifiers.md`, `guide/subcommands.md`, `guide/lookup.md`,
  `guide/flags.md`, `guide/exit-codes.md`, `guide/caching.md`,
  `server/index.md`, `server/reference.md`, `mcp/index.md`,
  `design/philosophy.md`, `design/architecture.md`, `troubleshooting.md`,
  `etymology.md`

**Interfaces:**
- Consumes: current `README.md` section text (migration source), corrected
  per Global Constraints.
- Produces: every `SUMMARY.md` target exists.

- [ ] **Step 1:** Copy each README section into its destination file per
  the spec's content-split table. Correct all output examples to YAML,
  flags to the verified set, identifier syntax to hyphen, drop the `mcp`
  subcommand row, drop `--human`, add `--cache-dir`.
- [ ] **Step 2:** Introduction (`src/README.md`): logo `<img>`, tagline,
  the two why-paragraphs (token-economy YAML + plan-aware), a "Full guide"
  nav nudge, side-by-side hero images (placeholder.svg for now).
- [ ] **Step 3 (test):** `python3 website/check-book.py` → expected PASS
  for `.md` links (images added in Task 3 may still be missing — checker
  reports them; that's fine until Task 3).
- [ ] **Step 4: Commit** `git add website/ && git commit -m "docs: scaffold mdBook site + migrate README content"`.

### Task 3: Theme + logo + images

**Files:**
- Create: `website/hypha.css` (palette from `ui/css/base.css`)
- Create: `website/theme/favicon.png` (copy of `logo/hypha.png`)
- Create: `website/src/images/hypha-logo.png` (copy of `logo/hypha.png`)
- Create: `website/src/images/placeholder.svg`
- Create: `website/src/images/CAPTURE-LIST.md`

**Interfaces:**
- Consumes: `logo/hypha.png`, color tokens from `ui/css/base.css`.
- Produces: `hypha.css` referenced by `book.toml`; all `<img>` refs resolve.

- [ ] **Step 1:** Write `hypha.css` overriding mdBook CSS vars for the
  `light` and `navy` themes with hypha tokens (light accent `#b0413e`,
  dark `#f08c84`, bg `#faf9f7`/`#0e1014`, code-bg, borders, links,
  sidebar-active, table stripes).
- [ ] **Step 2:** `cp logo/hypha.png website/theme/favicon.png` and
  `cp logo/hypha.png website/src/images/hypha-logo.png`.
- [ ] **Step 3:** Write `placeholder.svg` (a "screenshot coming soon" tile,
  ~1280×720). Reference it wherever a screenshot goes (README + Intro +
  server pages), each with an HTML comment naming the intended real file.
- [ ] **Step 4:** Write `CAPTURE-LIST.md`: table of intended filename →
  page → what it shows, plus instructions (drop PNG into `images/`, swap
  the one `<img src>`). Note placeholder mechanism (SVG reference, since no
  rasterizer is available here to pre-stamp PNGs).
- [ ] **Step 5 (test):** `python3 website/check-book.py` → expected PASS
  (all `.md` and image refs resolve).
- [ ] **Step 6: Commit** `git add website/ && git commit -m "docs: add hypha theme, logo, and screenshot placeholders"`.

### Task 4: Rewrite the lean README

**Files:**
- Modify: `README.md` (repo root) — replace with lean version.

**Interfaces:**
- Consumes: `website/src/images/*` (hero) — but README hero can reference
  `logo/hypha.png` + placeholder(s) at repo root paths.

- [ ] **Step 1:** Replace `README.md` with ~40–60 lines: centered logo,
  tagline, two why-paragraphs (YAML token-economy + plan-aware), side-by-
  side hero (CLI YAML output + server UI, placeholders for now), install
  one-liner, `📖 Full documentation →` link (URL TBD → note as
  `<PAGES_URL>` placeholder with a comment), License + Well-Typed footer.
- [ ] **Step 2 (test):** `wc -l README.md` → expected ≤ ~65; grep for
  `--human`/`schema: hypha/v0` → expected none.
- [ ] **Step 3: Commit** `git add README.md && git commit -m "docs: slim README to lean what/why + link to docs site"`.

### Task 5: GitLab CI — build gate + Pages deploy

**Files:**
- Modify: `.gitlab-ci.yml` — add `deploy` stage + `docs-build` + `pages`.

**Interfaces:**
- Consumes: `website/` mdBook project.
- Produces: `public/` artifact on `main`.

- [ ] **Step 1:** Add `deploy` to `stages`. Add `docs-build` (test stage,
  MR+push): `debian:bookworm-slim`, install `curl ca-certificates`,
  download mdBook release binary, `./mdbook build website`. Add `pages`
  (deploy stage, `main` only): same setup, `./mdbook build website -d
  "$CI_PROJECT_DIR/public"`, artifacts `public/`. Pin `MDBOOK_VERSION`.
- [ ] **Step 2 (test):** validate YAML — `python3 -c "import yaml,sys;
  yaml.safe_load(open('.gitlab-ci.yml'))"` (if PyYAML present) OR a
  structural grep for the two new job names + `stages:` containing
  `deploy`. Confirm existing GHC jobs unchanged (`git diff` shows only
  additions).
- [ ] **Step 3: Commit** `git add .gitlab-ci.yml && git commit -m "ci: build docs on MRs and publish to GitLab Pages on main"`.

### Task 6: Final verification + spec/plan commit

- [ ] **Step 1:** Run `python3 website/check-book.py` → PASS.
- [ ] **Step 2:** Grep the whole `website/` + `README.md` for banned
  strings: `--human`, `schema: hypha/v0`, `"ok": true`, `in_plan`,
  `@<version>`, `hypha mcp` (as a subcommand). Expected: none (config JSON
  blocks and prose mentioning the `hypha-mcp` binary are fine).
- [ ] **Step 3:** Commit the updated spec + this plan if not already.
- [ ] **Step 4:** Summarize deliverables + the human follow-ups (enable
  Pages, set `site-url`/README URL, capture screenshots).

## Self-Review

**Spec coverage:** repo layout (T1–T3), content split (T2), theme (T3),
screenshots (T3), CI/Pages (T5), lean README (T4), external Pages item
(T4/T6 notes). ✔ All spec sections have a task.

**Placeholder scan:** the only intentional placeholders are the screenshot
SVG and the `<PAGES_URL>` link — both documented, both required because the
assets/URL come from the human. No "TBD/implement later" in logic.

**Type consistency:** checker name `check-book.py` used consistently; CI
job names `docs-build`/`pages` consistent with spec.
