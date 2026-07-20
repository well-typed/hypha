#!/usr/bin/env python3
"""No-network sanity checker for the hypha mdBook site.

Catches the failure modes an mdBook build would reject, without needing the
mdBook binary (unavailable in the sandbox):

  * book.toml is not valid TOML;
  * a chapter linked from SUMMARY.md does not exist;
  * a relative markdown link or <img>/![]() target points at a missing file.

External links (http/https/mailto), in-page anchors (#...), and the
placeholder are ignored. Exits non-zero on any problem so CI/pre-commit can
gate on it. The authoritative gate is still the CI `docs-build` job.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent
SRC = ROOT / "src"
SUMMARY = SRC / "SUMMARY.md"

MD_LINK = re.compile(r"(?<!!)\[[^\]]*\]\(([^)]+)\)")   # [text](target)
MD_IMG = re.compile(r"!\[[^\]]*\]\(([^)]+)\)")          # ![alt](target)
HTML_IMG = re.compile(r"""<img[^>]*\bsrc\s*=\s*["']([^"']+)["']""", re.I)

problems: list[str] = []


def is_external(target: str) -> bool:
    t = target.strip()
    return (
        t.startswith(("http://", "https://", "mailto:", "//", "#"))
        or t == ""
    )


def local_part(target: str) -> str:
    """Strip #anchor and ?query, leaving the file path."""
    return target.split("#", 1)[0].split("?", 1)[0].strip()


def check_toml() -> None:
    try:
        import tomllib
    except ModuleNotFoundError:
        print("note: tomllib unavailable (Python < 3.11); skipping TOML parse")
        return
    toml = ROOT / "book.toml"
    try:
        with toml.open("rb") as fh:
            tomllib.load(fh)
    except Exception as exc:  # noqa: BLE001 - report any parse failure
        problems.append(f"book.toml: invalid TOML: {exc}")


def check_summary_chapters() -> list[Path]:
    if not SUMMARY.exists():
        problems.append("src/SUMMARY.md: missing")
        return []
    chapters: list[Path] = []
    for target in MD_LINK.findall(SUMMARY.read_text(encoding="utf-8")):
        if is_external(target):
            continue
        rel = local_part(target)
        if not rel.endswith(".md"):
            continue
        path = (SRC / rel).resolve()
        if not path.exists():
            problems.append(f"SUMMARY.md -> {rel}: chapter file missing")
        else:
            chapters.append(path)
    return chapters


def check_links_in(md: Path) -> None:
    text = md.read_text(encoding="utf-8")
    targets = MD_LINK.findall(text) + MD_IMG.findall(text) + HTML_IMG.findall(text)
    for target in targets:
        if is_external(target):
            continue
        rel = local_part(target)
        if not rel:
            continue
        resolved = (md.parent / rel).resolve()
        if not resolved.exists():
            problems.append(f"{md.relative_to(ROOT)} -> {rel}: target missing")


def main() -> int:
    check_toml()
    check_summary_chapters()
    for md in sorted(SRC.rglob("*.md")):
        check_links_in(md)

    if problems:
        print(f"FAIL: {len(problems)} problem(s):")
        for p in problems:
            print(f"  - {p}")
        return 1
    print("OK: book.toml valid; all SUMMARY chapters and local links resolve.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
