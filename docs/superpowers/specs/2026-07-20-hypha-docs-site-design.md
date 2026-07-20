# Hypha Documentation Split — Lean README + mdBook Docs Site

- **Date:** 2026-07-20
- **Status:** Approved (design) — **implementation BLOCKED, see below**
- **Author:** Alfredo Di Napoli (with Claude Code)

> **⚠️ Blocked on the YAML-default output change.**
> A separate workstream is flipping the CLI's default output from
> **compact JSON** to **YAML** (JSON becomes opt-in via `--json`). Docs
> implementation is paused until that lands on `main`. When resumed, the
> output-format framing throughout this spec and the migrated docs MUST be
> rewritten accordingly:
> - The pitch "compact JSON is the default" → "YAML is the default; JSON
>   via `--json`". Affects the Introduction, the cli-printing-press table
>   ("Compact JSON is the default"), and the "Human + machine output
>   modes" framing.
> - Every quick-start / reference **example** currently showing a JSON
>   block must show YAML (with a `--json` variant where useful).
> - Examples should be captured from the real YAML-default binary once it
>   is buildable, not hand-written.

## Problem

The current `README.md` is ~21 KB / 530 lines. It doubles as a landing
page, a full reference manual, a troubleshooting guide, and a design
essay. The signal — *what hypha is and why it exists* — is buried under
exhaustive detail. A newcomer cannot answer "what is this?" in the first
screenful.

## Goal

Split the documentation in two:

1. **A lean `README.md`** (~40–60 lines) that answers *what* and *why*
   fast, shows one hero image, and points to the full docs.
2. **A dedicated documentation website** built with **mdBook**, hosting
   the extensive reference, guides, screenshots, and design material,
   published via **GitLab Pages**.

## Non-Goals

- No custom domain at launch (use the GitLab-Pages-assigned URL).
- No docs versioning, blog, or search-engine beyond mdBook's built-in
  client-side search.
- No Node.js toolchain — the site must build from a single static
  binary in CI.
- No change to hypha's code, CLI, or server behaviour. This is docs-only.

## Tooling Decision: mdBook

mdBook (the Rust project's documentation tool) was chosen over MkDocs,
Docusaurus, and hand-rolled HTML because:

- **Single static binary**, zero runtime dependencies. CI downloads the
  prebuilt release binary from GitHub — no Rust toolchain, no `npm
  install`.
- **Built-in** client-side search, syntax highlighting, and light/dark
  themes.
- **Markdown source**, so migrating the existing README prose is
  near-copy-paste.
- Book-style sidebar navigation suits prose-heavy technical docs.
- Trivial GitLab Pages integration: build to `public/`, done.

Trade-off accepted: mdBook is "book"-shaped rather than
"marketing-landing"-shaped. We compensate with a custom-themed landing
page (`src/README.md`) and the hero image.

## Repository Layout

```
README.md                       # LEAN. what/why + hero + install one-liner + link
website/
  book.toml                     # mdBook config; custom theme wired in
  src/
    SUMMARY.md                  # chapter tree == site navigation
    README.md                   # site landing page (Introduction)
    getting-started/
      installation.md
      quick-start.md
      claude-plugin.md
    guide/
      identifiers.md
      subcommands.md
      lookup.md
      flags.md
      exit-codes.md
      caching.md
    server/
      index.md                  # doc-browser server overview
      reference.md              # endpoints + flags tables
    mcp/
      index.md                  # MCP host integration
    design/
      philosophy.md             # cli-printing-press tenets + CLI-vs-MCP split
      architecture.md
    troubleshooting.md
    etymology.md
    images/
      CAPTURE-LIST.md           # names + descriptions of every screenshot the site expects
      placeholder.svg           # neutral placeholder shown until real shots land
  theme/
    hypha.css                   # accent + light/dark palette lifted from ui/css
    index.hbs                   # light override: logo atop the sidebar
```

- The mdBook project lives entirely under `website/`, kept separate from
  `docs/superpowers/` (which holds specs and plans — unrelated to the
  published site).
- Build output (`public/`) is **generated in CI, never committed**.

## Content Split

The current README's sections are migrated as follows:

| Current README section                        | Destination                          |
|------------------------------------------------|--------------------------------------|
| Why hypha?                                     | `src/README.md` (Introduction) + condensed 2-para version in lean README |
| Design philosophy: cli-printing-press          | `design/philosophy.md`               |
| CLI vs MCP split                               | `design/philosophy.md`               |
| Features                                       | `src/README.md` (short) + `guide/*`  |
| Installation (source, Nix, plugin)             | `getting-started/installation.md` + `getting-started/claude-plugin.md` |
| Quick Start                                    | `getting-started/quick-start.md`     |
| Identifier Syntax                              | `guide/identifiers.md`               |
| Global Flags                                   | `guide/flags.md`                     |
| Subcommands (+ lookup deep-dive)               | `guide/subcommands.md`, `guide/lookup.md` |
| Cache layout / Caching                         | `guide/caching.md`                   |
| Local Doc Browser (server)                     | `server/index.md`, `server/reference.md` |
| MCP Host Integration                           | `mcp/index.md`                       |
| Exit Codes                                     | `guide/exit-codes.md`                |
| Troubleshooting                                | `troubleshooting.md`                 |
| Architecture                                   | `design/architecture.md`             |
| Etymology                                      | `etymology.md`                       |
| License / footer                               | stays in lean README                 |

### Lean README contents (final shape)

1. Centered logo.
2. Tagline (one line).
3. Two short paragraphs: (a) the token-economy pitch, (b) plan-aware /
   source-faithful pitch.
4. **Hero image**: CLI `--human` output *and* the server UI, side by
   side ("one tool, two surfaces").
5. Install one-liner + `Full documentation → <site URL>` link.
6. License + Well-Typed footer.

Target: 40–60 lines. Everything else lives on the site.

## Theme

- `theme/hypha.css` referenced via `book.toml`'s
  `output.html.additional-css`. It overrides mdBook's CSS custom
  properties so the site adopts hypha's palette (values lifted verbatim
  from `ui/css/base.css`):

  | Token   | Light     | Dark      |
  |---------|-----------|-----------|
  | bg      | `#faf9f7` | `#0e1014` |
  | fg      | `#1c1b1a` | `#e9eaef` |
  | accent  | `#b0413e` | `#f08c84` |
  | accent-2| `#5a3a86` | `#b3a1e5` |
  | code-bg | `#f4f1ea` | `#1a1d26` |
  | border  | `#e6e3dc` | `#262a36` |

  These map onto mdBook theme variables (`--links`, `--sidebar-active`,
  `--inline-code-color`, `--table-*`, etc.) for both the light theme and
  a dark theme.
- `book.toml`: `default-theme = "light"`, `preferred-dark-theme` set to
  the hypha dark theme; `git-repository-url` → the GitLab repo;
  `edit-url-template` enabled so pages have an "edit on GitLab" link.
- `theme/index.hbs`: a **minimal** override of the stock template that
  injects the logo above the sidebar table of contents. Kept as small a
  diff from the shipped `index.hbs` as possible to ease future mdBook
  upgrades. Favicon derived from the logo.

## Screenshots

- All images live in `website/src/images/`.
- The user captures the PNGs. To keep the site building green before
  they exist, the spec ships:
  - `images/placeholder.svg` — a neutral "screenshot coming soon" tile.
  - `images/CAPTURE-LIST.md` — an explicit, ordered list naming each
    expected screenshot, the page that references it, and what it should
    show. Initial list (subject to refinement during implementation):

    | Filename                | Referenced by                | Shows |
    |-------------------------|------------------------------|-------|
    | `hero-cli.png`          | lean README, Introduction    | `hypha symbol … --human` terminal output |
    | `hero-server.png`       | lean README, Introduction    | server command-palette + symbol card |
    | `server-search.png`     | `server/index.md`            | fuzzy search dropdown mid-query |
    | `server-symbol-card.png`| `server/index.md`            | a rendered symbol card with Haddock |
    | `server-source-view.png`| `server/index.md`            | skylighting source view with `?line=` |
    | `cli-json.png`          | `getting-started/quick-start.md` | compact JSON output |

- Markdown references the real filenames from day one
  (`images/hero-cli.png`, …). To keep the site looking complete and the
  build green before real shots exist, **each expected filename is
  committed as a copy of a shared placeholder image** (a small "screenshot
  coming soon" tile). The user overwrites each file in place with the real
  capture — no markdown edits needed, and no build ever fails for a
  missing image.

## CI / Deployment (GitLab Pages)

GitLab Pages is the gh-pages equivalent: a CI job that publishes a
`public/` artifact, which GitLab then serves statically. Two jobs are
added to `.gitlab-ci.yml`. Neither needs a Rust toolchain — both fetch
the prebuilt mdBook release binary from GitHub.

A pinned `MDBOOK_VERSION` variable controls the downloaded binary.

```yaml
stages:
  - test
  - deploy            # NEW

# NEW — build the docs on every MR/push; gates broken builds, no deploy
docs-build:
  stage: test
  image: debian:bookworm-slim
  before_script:
    - apt-get update && apt-get install -y curl
    - curl -fsSL "https://github.com/rust-lang/mdBook/releases/download/v${MDBOOK_VERSION}/mdbook-v${MDBOOK_VERSION}-x86_64-unknown-linux-gnu.tar.gz" | tar -xz
  script:
    - ./mdbook build website
  variables:
    MDBOOK_VERSION: "0.4.40"      # pinned; bump deliberately
  rules:
    - if: $CI_PIPELINE_SOURCE == "push"
    - if: $CI_PIPELINE_SOURCE == "merge_request_event"

# NEW — deploy to GitLab Pages, main branch only
pages:
  stage: deploy
  image: debian:bookworm-slim
  before_script:
    - apt-get update && apt-get install -y curl
    - curl -fsSL "https://github.com/rust-lang/mdBook/releases/download/v${MDBOOK_VERSION}/mdbook-v${MDBOOK_VERSION}-x86_64-unknown-linux-gnu.tar.gz" | tar -xz
  script:
    - ./mdbook build website -d "$CI_PROJECT_DIR/public"
  artifacts:
    paths:
      - public
  variables:
    MDBOOK_VERSION: "0.4.40"
  rules:
    - if: $CI_COMMIT_BRANCH == "main"
```

Notes:
- The existing GHC build/test jobs are untouched; only the `stages` list
  gains `deploy` and two jobs are appended.
- The exact mdBook version and the release asset name are verified during
  implementation against the current mdBook release.
- Download host `github.com` / `objects.githubusercontent.com` must be
  reachable from the GitLab runners (true for standard shared runners).

## External Dependency / Open Item

**GitLab Pages must be enabled** for `well-typed/hypha` on
`gitlab.well-typed.com`, and the served URL is assigned by that instance
(commonly `https://<namespace>.<pages-domain>/<project>`). This cannot be
verified from the development sandbox. Action for the user: confirm Pages
is enabled and note the assigned URL. Once known:

- Set `book.toml`'s `site-url` to the path prefix (e.g. `/hypha/`) so
  absolute asset links resolve. Internal relative links work regardless,
  so the site is functional before this is set.
- Update the lean README's `Full documentation →` link to the live URL.

A custom domain (e.g. `hypha.well-typed.com`) is explicitly out of scope
for launch but the design does not preclude it later.

## Testing Strategy

- **Primary gate:** the CI `docs-build` job — the site must build
  cleanly (mdBook fails on malformed `SUMMARY.md` and, with the linkcheck
  backend if enabled, on broken internal links).
- **Local preview:** `mdbook serve website` renders with live reload.
- No unit/property/golden tests: this is a static content site with no
  runtime logic. The successful build is the test.

## Migration / Rollout Order (for the implementation plan)

1. Scaffold `website/` (`book.toml`, `SUMMARY.md`, empty chapter stubs).
2. Add custom theme (`hypha.css`, `index.hbs`, favicon).
3. Migrate README prose into chapters, section by section.
4. Add `images/` placeholder + `CAPTURE-LIST.md`; wire image references.
5. Rewrite `README.md` into the lean form.
6. Add the `docs-build` + `pages` CI jobs.
7. Verify `mdbook build website` succeeds locally and in CI.
8. User enables Pages, captures screenshots, sets final `site-url` + link.

## Success Criteria

- `README.md` ≤ ~60 lines and answers what/why on the first screen.
- `mdbook build website` succeeds locally and in CI.
- Every current README section has a home on the site (nothing lost).
- The site is themed to match hypha's palette in both light and dark.
- The `pages` job publishes on merge to `main`; the URL is live.
