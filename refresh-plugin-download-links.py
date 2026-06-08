#!/usr/bin/env python3
"""Refresh plugin download-link tables in plugin-catalog docs.

Starting with EMQX 5.10 (enterprise) and 6.2 (unified), plugins live in
the emqx.git monorepo under `plugins/<name>/` and each has a `VERSION`
file. For every stable release tag in the chosen series, the CI
publishes a tarball at:

    https://packages.emqx.io/emqx-plugins/<tag>/<plugin>-<plugin-ver>.tar.gz

where `<tag>` is the literal git tag — bare semver for 6.x (e.g. `6.2.0`)
and `e`-prefixed for 5.x enterprise (e.g. `e5.10.4`).

This script walks the stable tags of one series (e.g. 5.10 or 6.2),
reads each plugin's VERSION file, and regenerates a "Download" table
inside every matching markdown file under
`en_US|zh_CN|ja_JP/extensions/plugin-catalog/`. The series defaults
to the current docs-root branch (`release-N.M`); override with
`--series N.M`.

Markdown filenames use dashes (e.g. `emqx-bridge-mqtt-dq.md`) whereas
plugin directory names use underscores (e.g. `emqx_bridge_mqtt_dq`);
the script maps between them by replacing `-` with `_`.

Generated content is bounded by HTML-comment markers so the script is
idempotent. If the markers are missing, a new Download section is
appended at the end of the file.
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

DEFAULT_EMQX_REMOTE = "https://github.com/emqx/emqx.git"
PACKAGE_URL_TEMPLATE = (
    "https://packages.emqx.io/emqx-plugins/{tag}/{plugin}-{plugin_ver}.tar.gz"
)

# Stable release tags. Two conventions are in use:
#   - 6.x:  bare semver, e.g. "6.2.0"
#   - 5.x:  enterprise prefix, e.g. "e5.10.4"
# Pre-release suffixes (-rc.1 / -alpha.1 / -M2) are excluded.
STABLE_TAG_RE = re.compile(r"^(?P<prefix>e)?(?P<major>\d+)\.(?P<minor>\d+)\.(?P<patch>\d+)$")
SERIES_RE = re.compile(r"^(\d+)\.(\d+)$")
BRANCH_RE = re.compile(r"^release-(\d+)\.?(\d+)$")

BEGIN_MARKER = "<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->"
END_MARKER = "<!-- PLUGIN-DOWNLOADS:END -->"

LOCALES: dict[str, dict[str, str]] = {
    "en_US": {
        "heading": "## Download",
        "intro": "Tarballs for each EMQX release:",
        "col_emqx": "EMQX Version",
        "col_plugin": "Plugin Version",
        "col_link": "Package",
    },
    "zh_CN": {
        "heading": "## 下载",
        "intro": "各 EMQX 版本对应的插件安装包：",
        "col_emqx": "EMQX 版本",
        "col_plugin": "插件版本",
        "col_link": "安装包",
    },
    "ja_JP": {
        "heading": "## ダウンロード",
        "intro": "各 EMQX リリースに対応するプラグインパッケージ:",
        "col_emqx": "EMQX バージョン",
        "col_plugin": "プラグインバージョン",
        "col_link": "パッケージ",
    },
}

CATALOG_SUBPATH = Path("extensions/plugin-catalog")


@dataclass(frozen=True)
class ReleaseTag:
    raw: str  # original tag, used verbatim in URL: "6.2.0" or "e5.10.4"
    major: int
    minor: int
    patch: int

    @property
    def display_version(self) -> str:
        """Human-readable semver shown in the table (no 'e' prefix)."""
        return f"{self.major}.{self.minor}.{self.patch}"

    @property
    def sort_key(self) -> tuple[int, int, int]:
        return (self.major, self.minor, self.patch)


def run_git(repo: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", "-C", str(repo), *args],
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout


def try_git(repo: Path, *args: str) -> str | None:
    """Run git; return None if it fails (e.g. file missing at tag)."""
    result = subprocess.run(
        ["git", "-C", str(repo), *args],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return None
    return result.stdout


def ensure_repo(supplied: Path | None, workdir: Path) -> Path:
    """Return a usable emqx repo; clone into workdir if none supplied."""
    if supplied is not None:
        if not (supplied / ".git").exists():
            sys.exit(f"error: {supplied} is not a git repository")
        print(f"Using existing emqx clone: {supplied}", file=sys.stderr)
        run_git(supplied, "fetch", "--tags", "--quiet")
        return supplied

    target = workdir / "emqx"
    print(f"Cloning {DEFAULT_EMQX_REMOTE} into {target} ...", file=sys.stderr)
    subprocess.run(
        ["git", "clone", "--filter=blob:none", "--no-checkout", DEFAULT_EMQX_REMOTE, str(target)],
        check=True,
    )
    return target


def list_release_tags(repo: Path, series: tuple[int, int]) -> list[ReleaseTag]:
    """Return stable tags matching the given (major, minor) series.

    `series` is e.g. (5, 10) or (6, 2). Only tags using the conventional
    prefix for that major are accepted (5.x → 'e' prefix; 6.x → no prefix).
    """
    out = run_git(repo, "tag", "--list")
    want_major, want_minor = series
    tags: list[ReleaseTag] = []
    for line in out.splitlines():
        raw = line.strip()
        m = STABLE_TAG_RE.match(raw)
        if not m:
            continue
        has_prefix = bool(m.group("prefix"))
        major = int(m.group("major"))
        minor = int(m.group("minor"))
        patch = int(m.group("patch"))
        if (major, minor) != (want_major, want_minor):
            continue
        # Enforce prefix convention: 5.x uses 'e', 6.x uses none.
        if major == 5 and not has_prefix:
            continue
        if major >= 6 and has_prefix:
            continue
        tags.append(ReleaseTag(raw=raw, major=major, minor=minor, patch=patch))
    tags.sort(key=lambda t: t.sort_key)
    return tags


def list_plugins_at(repo: Path, tag: str) -> list[str]:
    """Return plugin directory names present under `plugins/` at the tag."""
    out = try_git(repo, "ls-tree", "--name-only", f"{tag}:plugins")
    if out is None:
        return []
    return sorted(p.strip() for p in out.splitlines() if p.strip())


def read_plugin_version(repo: Path, tag: str, plugin: str) -> str | None:
    out = try_git(repo, "show", f"{tag}:plugins/{plugin}/VERSION")
    if out is None:
        return None
    version = out.strip()
    return version or None


def build_rows(
    repo: Path, tags: Iterable[ReleaseTag]
) -> dict[str, list[tuple[ReleaseTag, str]]]:
    """Return plugin -> [(tag, plugin_ver), ...] sorted by tag asc."""
    rows: dict[str, list[tuple[ReleaseTag, str]]] = {}
    for tag in tags:
        plugins = list_plugins_at(repo, tag.raw)
        for plugin in plugins:
            pver = read_plugin_version(repo, tag.raw, plugin)
            if not pver:
                continue
            rows.setdefault(plugin, []).append((tag, pver))
    return rows


def render_section(
    locale: str, plugin: str, entries: list[tuple[ReleaseTag, str]]
) -> str:
    labels = LOCALES[locale]
    lines: list[str] = []
    lines.append(BEGIN_MARKER)
    lines.append("")
    lines.append(labels["heading"])
    lines.append("")
    lines.append(labels["intro"])
    lines.append("")
    lines.append(
        f"| {labels['col_emqx']} | {labels['col_plugin']} | {labels['col_link']} |"
    )
    lines.append("|---|---|---|")
    for tag, plugin_ver in entries:
        url = PACKAGE_URL_TEMPLATE.format(
            tag=tag.raw, plugin=plugin, plugin_ver=plugin_ver
        )
        filename = f"{plugin}-{plugin_ver}.tar.gz"
        lines.append(
            f"| {tag.display_version} | {plugin_ver} | [{filename}]({url}) |"
        )
    lines.append("")
    lines.append(END_MARKER)
    return "\n".join(lines)


def splice_section(original: str, new_section: str) -> str:
    """Replace an existing marker block, or append one at end of file."""
    begin_idx = original.find(BEGIN_MARKER)
    end_idx = original.find(END_MARKER)
    if begin_idx != -1 and end_idx != -1 and end_idx > begin_idx:
        after = original[end_idx + len(END_MARKER):]
        return original[:begin_idx] + new_section + after

    # Append; ensure exactly one blank line before the new block.
    trimmed = original.rstrip() + "\n\n"
    return trimmed + new_section + "\n"


def md_filename_for(plugin: str) -> str:
    return plugin.replace("_", "-") + ".md"


def update_catalog_files(
    docs_root: Path,
    rows: dict[str, list[tuple[ReleaseTag, str]]],
    locales: list[str],
    dry_run: bool,
) -> int:
    changed = 0
    for plugin, entries in sorted(rows.items()):
        fname = md_filename_for(plugin)
        for locale in locales:
            path = docs_root / locale / CATALOG_SUBPATH / fname
            if not path.exists():
                print(
                    f"skip: {path.relative_to(docs_root)} (no such doc for plugin {plugin})",
                    file=sys.stderr,
                )
                continue
            new_section = render_section(locale, plugin, entries)
            original = path.read_text(encoding="utf-8")
            updated = splice_section(original, new_section)
            if updated == original:
                print(f"unchanged: {path.relative_to(docs_root)}", file=sys.stderr)
                continue
            changed += 1
            if dry_run:
                print(f"would update: {path.relative_to(docs_root)}", file=sys.stderr)
            else:
                path.write_text(updated, encoding="utf-8")
                print(f"updated: {path.relative_to(docs_root)}", file=sys.stderr)
    return changed


def detect_series_from_branch(docs_root: Path) -> tuple[int, int] | None:
    """Read the docs-root git branch and parse 'release-N.M' / 'release-NM'."""
    out = try_git(docs_root, "rev-parse", "--abbrev-ref", "HEAD")
    if out is None:
        return None
    branch = out.strip()
    m = BRANCH_RE.match(branch)
    if not m:
        return None
    return int(m.group(1)), int(m.group(2))


def resolve_series(flag: str | None, docs_root: Path) -> tuple[int, int]:
    if flag is not None:
        m = SERIES_RE.match(flag)
        if not m:
            sys.exit(f"error: --series must look like 'N.M' (got {flag!r})")
        return int(m.group(1)), int(m.group(2))
    detected = detect_series_from_branch(docs_root)
    if detected is None:
        sys.exit(
            "error: could not auto-detect release series from docs-root branch; "
            "pass --series N.M (e.g. --series 5.10)"
        )
    return detected


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Refresh plugin download-link tables in plugin-catalog docs.",
    )
    parser.add_argument(
        "--emqx-repo",
        type=Path,
        default=None,
        help="Path to an existing emqx.git clone (tags will be fetched). "
             "Default: clone a fresh blobless copy into a temp dir.",
    )
    parser.add_argument(
        "--docs-root",
        type=Path,
        default=Path(__file__).resolve().parent,
        help="Root of emqx-docs (must contain en_US/zh_CN/ja_JP dirs). "
             "Default: directory containing this script.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show which files would change without writing.",
    )
    parser.add_argument(
        "--series",
        default=None,
        help="Release series to scan, e.g. '5.10' or '6.2'. "
             "Default: auto-detect from the docs-root branch name (release-N.M).",
    )
    args = parser.parse_args()

    docs_root: Path = args.docs_root.resolve()
    present_locales = [
        loc for loc in LOCALES if (docs_root / loc / CATALOG_SUBPATH).is_dir()
    ]
    if not present_locales:
        sys.exit(
            f"error: no plugin-catalog dirs found under {docs_root} "
            f"(looked for {'/'.join(LOCALES)}/{CATALOG_SUBPATH})"
        )
    missing = sorted(set(LOCALES) - set(present_locales))
    if missing:
        print(
            f"note: skipping locale(s) without plugin-catalog dir: {', '.join(missing)}",
            file=sys.stderr,
        )

    series = resolve_series(args.series, docs_root)
    print(f"Targeting release series {series[0]}.{series[1]}", file=sys.stderr)

    tmp_ctx: tempfile.TemporaryDirectory | None = None
    try:
        if args.emqx_repo is None:
            tmp_ctx = tempfile.TemporaryDirectory(prefix="emqx-plugins-")
            workdir = Path(tmp_ctx.name)
        else:
            workdir = Path(".")
        repo = ensure_repo(args.emqx_repo.resolve() if args.emqx_repo else None, workdir)

        tags = list_release_tags(repo, series)
        if not tags:
            print(
                f"warning: no stable tags found for series {series[0]}.{series[1]}",
                file=sys.stderr,
            )
            return 0
        print(
            f"Found {len(tags)} stable tag(s): "
            f"{tags[0].raw} ... {tags[-1].raw}",
            file=sys.stderr,
        )

        rows = build_rows(repo, tags)
        if not rows:
            print("warning: no plugins discovered in any tag", file=sys.stderr)
            return 0
        print(f"Discovered {len(rows)} plugin(s): {', '.join(sorted(rows))}", file=sys.stderr)

        changed = update_catalog_files(docs_root, rows, present_locales, args.dry_run)
        print(f"Done. {changed} file(s) {'to update' if args.dry_run else 'updated'}.", file=sys.stderr)
        return 0
    finally:
        if tmp_ctx is not None:
            tmp_ctx.cleanup()


if __name__ == "__main__":
    raise SystemExit(main())
