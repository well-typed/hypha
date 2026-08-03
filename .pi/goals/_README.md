# Goal Template Reference

`_GOAL_TEMPLATE.md` is a generic blueprint for kicking off the multi-agent team
via the `/goal` skill. The team itself (`.pi/agents/team-lead.md`,
`team-senior-dev.md`, `team-qa.md`) is **project-agnostic** — it learns the
project's domain, work-tracking mechanism, build/verify commands, and constraints
by reading `CLAUDE.md` / `AGENT.md` at boot, then persists that to
`.pi/team-context.md` for the dev/QA instances to inherit.

Because of that, a goal file carries far less than it used to: most context comes
from the repo's own docs, not the goal.

## How the team operates (the recyclable "modus operandi")

- **team-lead** (strong model, 1×): boots from `CLAUDE.md`, discovers usable
  skills, writes the sentinel, decomposes the tracking unit into tickets with
  acceptance criteria, dispatches devs, runs the QA gate, does the design/quality
  review, and is the ONLY agent that touches trunk.
- **team-senior-dev** (cheap model, N×): one ticket each, own feature branch,
  owns its ticket's state transitions, reports to the lead only.
- **team-qa** (cheap model, N×): adversarial gate BEFORE the lead — tries to break
  the branch and proves every acceptance criterion. A QA FAIL bounces straight back
  to the dev; only QA-passed branches reach the lead's review.

Lenses are split: **QA = behavior/acceptance**, **lead = design/architecture**.
Comms are hub-and-spoke: devs and QA talk ONLY to the lead, never to each other.

**Branch model.** `main`/`master` is sacred — the team never merges or pushes there.
At boot the lead cuts an **epic integration branch** (`epic/<label>`) off base; that
branch is the team's trunk. Devs cut `feat/<ticket>` branches off the epic branch,
and the lead merges QA-passed work back into the epic branch. When the epic is done
the lead hands the branch to the stakeholder for the PR into base — it never merges
into base on its own.

## Creating a New Goal

Copy `_GOAL_TEMPLATE.md` and fill in the placeholders below. Anything the team can
read from `CLAUDE.md` is NOT a placeholder anymore — leave it to the boot sequence.

| Placeholder | Description | Example |
|---|---|---|
| `MISSION_SUMMARY` | One-line mission | `harden error handling across the source-resolution layer` |
| `TRACKING_UNIT` | What you track work in (matches the project's mechanism) | `epic` / `milestone` / `issue` / `board` |
| `TRACKING_REF` | The reference to the tracking unit | `#81`, `%3` (GitLab milestone) |
| `TRACKING_LABEL` | Short human name for it | `source-hardening` |
| `N_DEVS` | How many senior-dev instances may run | `2` |
| `N_QA` | How many QA instances may run | `1` |
| `VERIFY_COMMAND` | Only if `CLAUDE.md` doesn't state how to verify | `cabal build all && cabal test all` |
| `EXTRA_CONSTRAINTS` | Hard constraints beyond what `CLAUDE.md` declares | `do not bump dependency bounds` |
| `CLOSING_ARTIFACT` | Final summary doc to write | `docs/closing/source-hardening.md` |
| `STAKEHOLDER` | Who can unblock scope decisions | `Alfredo` |

The `{{#IF_...}}` blocks are optional — drop them entirely if `CLAUDE.md` already
covers verification / constraints.

## Naming Convention

Name goal files descriptively: `<project>-<task>-<date>.md` — e.g.
`hypha-source-hardening-2026-06-01.md`. The template is prefixed with `_` so it
sorts first and signals "not a real goal."
