#!/usr/bin/env python3
"""Open a draft catalog PR for a GA whose plugin packages are in S3."""

import argparse
import importlib.util
import json
import os
import re
import subprocess
import sys
import tempfile
import urllib.request
from pathlib import Path

GENERATOR = Path(__file__).resolve().parents[2] / "scripts" / "refresh-plugin-download-links.py"
spec = importlib.util.spec_from_file_location("plugin_catalog", GENERATOR)
catalog = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = catalog
spec.loader.exec_module(catalog)


def command(*args, cwd=None):
    return subprocess.check_output(args, cwd=cwd, text=True).strip()


def git(root, *args):
    return command("git", *args, cwd=root)


def metadata(tag):
    release = catalog.parse_release_tag(tag)
    if release.major < 5 or (release.major == 5 and release.minor < 10):
        raise ValueError("plugin catalogs require EMQX 5.10 or later")
    series = f"{release.major}.{release.minor}"
    return dict(
        tag=tag,
        version=release.display_version,
        series=series,
        base=f"release-{series}",
        branch=f"update-plugin-catalog-{tag}",
    )


def verify_packages(source, tag):
    plugins = catalog.list_plugins_at(source, tag)
    for plugin in plugins:
        version = catalog.read_plugin_version(source, tag, plugin)
        if not version:
            raise ValueError(f"{plugin} has no VERSION at {tag}")
        base = f"https://packages.emqx.io/emqx-plugins/{tag}/{plugin}-{version}"
        with urllib.request.urlopen(
            urllib.request.Request(base + ".tar.gz", method="HEAD"), timeout=30
        ) as response:
            if int(response.headers.get("Content-Length", "0")) <= 0:
                raise ValueError(f"empty plugin package: {base}.tar.gz")
        with urllib.request.urlopen(base + ".sha256", timeout=30) as response:
            digest = response.read(256).decode().strip()
        if not re.fullmatch(r"[0-9a-f]{64}", digest):
            raise ValueError(f"invalid build-time checksum: {base}.sha256")
    return bool(plugins)


class GitHub:
    def __init__(self, repository):
        self.repository = repository

    def pull_request(self, branch, base):
        prs = json.loads(
            command(
                "gh",
                "pr",
                "list",
                "--repo",
                self.repository,
                "--head",
                branch,
                "--base",
                base,
                "--state",
                "all",
                "--json",
                "number,state,url,milestone",
            )
        )
        return next((pr for pr in prs if pr["state"] == "OPEN"), prs[0] if prs else None)

    def milestone(self, version):
        pages = json.loads(
            command(
                "gh",
                "api",
                "--paginate",
                "--slurp",
                f"repos/{self.repository}/milestones?state=all&per_page=100",
            )
        )
        if any(m["title"] == version for page in pages for m in page):
            return version
        command(
            "gh",
            "api",
            "--method",
            "POST",
            f"repos/{self.repository}/milestones",
            "-f",
            f"title={version}",
        )
        return version

    def set_milestone(self, number, version):
        command(
            "gh",
            "pr",
            "edit",
            str(number),
            "--repo",
            self.repository,
            "--milestone",
            self.milestone(version),
        )

    def create(self, info, run_url):
        body = (
            f'Refresh plugin download tables through EMQX {info["version"]}.\n\n'
            f'Source tag `{info["tag"]}`: https://github.com/emqx/emqx/tree/{info["source_commit"]}\n'
        )
        if run_url:
            body += f"\nWorkflow: {run_url}\n"
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md") as file:
            file.write(body)
            file.flush()
            return command(
                "gh",
                "pr",
                "create",
                "--repo",
                self.repository,
                "--draft",
                "--base",
                info["base"],
                "--head",
                info["branch"],
                "--milestone",
                self.milestone(info["version"]),
                "--title",
                f'docs: refresh plugin catalog for {info["version"]}',
                "--body-file",
                file.name,
            )


def refresh(tag, source, docs, github, source_sha=None, run_url=None):
    info = metadata(tag)
    commit = git(source, "rev-parse", "--verify", f"refs/tags/{tag}^{{commit}}")
    if source_sha and commit != source_sha:
        raise ValueError("release tag no longer matches the package source commit")
    info["source_commit"] = commit
    if not verify_packages(source, tag):
        print(f"{tag} has no bundled plugins; no PR needed.")
        return
    pr = github.pull_request(info["branch"], info["base"])
    if pr and pr["state"] != "OPEN":
        print(f'Existing PR is {pr["state"]}: {pr["url"]}')
        return
    if git(docs, "status", "--porcelain"):
        raise ValueError("docs checkout must be clean")
    if pr and not pr.get("milestone"):
        github.set_milestone(pr["number"], info["version"])
    git(docs, "config", "user.name", "github-actions[bot]")
    git(docs, "config", "user.email", "41898282+github-actions[bot]@users.noreply.github.com")
    git(docs, "fetch", "origin", info["base"])
    remote_exists = bool(
        git(docs, "ls-remote", "--heads", "origin", f'refs/heads/{info["branch"]}')
    )
    local_exists = (
        subprocess.run(
            ["git", "show-ref", "--verify", "--quiet", f'refs/heads/{info["branch"]}'], cwd=docs
        ).returncode
        == 0
    )
    if remote_exists:
        git(docs, "fetch", "origin", info["branch"])
    if local_exists:
        git(docs, "switch", info["branch"])
        if remote_exists:
            git(docs, "merge", "--ff-only", f'origin/{info["branch"]}')
    elif remote_exists:
        git(docs, "switch", "-c", info["branch"], f'origin/{info["branch"]}')
    else:
        git(docs, "switch", "-c", info["branch"], f'origin/{info["base"]}')
    git(docs, "merge", "--no-edit", f'origin/{info["base"]}')
    command(
        sys.executable,
        str(GENERATOR),
        "--docs-root",
        str(docs),
        "--emqx-repo",
        str(source),
        "--series",
        info["series"],
        "--through-tag",
        tag,
        "--no-fetch",
    )
    changed = set(filter(None, git(docs, "diff", "--name-only", "-z").split("\0")))
    changed.update(
        filter(None, git(docs, "ls-files", "--others", "--exclude-standard", "-z").split("\0"))
    )
    for path in changed:
        if not re.fullmatch(
            r"(en_US|zh_CN|ja_JP)/guides/extensions/plugin-catalog/(?:[0-9]+\.[0-9]+/)?[^/]+\.md",
            path,
        ):
            raise ValueError(f"unexpected generated path: {path}")
        parent = Path(path).parent.name
        if parent != "plugin-catalog" and parent != info["series"]:
            raise ValueError(f"catalog belongs to another series: {path}")
    if changed:
        git(docs, "add", "--", *sorted(changed))
        git(docs, "commit", "-m", f'docs: refresh plugin catalog for {info["version"]}')
    if not git(docs, "diff", "--name-only", f'origin/{info["base"]}...HEAD'):
        print("Catalog is already current; no PR needed.")
        return
    git(docs, "push", "origin", f'HEAD:refs/heads/{info["branch"]}')
    if pr:
        print(pr["url"])
    else:
        print(github.create(info, run_url))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tag", required=True)
    parser.add_argument("--source-sha", default="")
    parser.add_argument("--metadata-only", action="store_true")
    parser.add_argument("--emqx-repo", type=Path)
    parser.add_argument("--docs-root", type=Path)
    parser.add_argument(
        "--repository", default=os.environ.get("GITHUB_REPOSITORY", "emqx/emqx-docs")
    )
    parser.add_argument("--run-url")
    args = parser.parse_args()
    try:
        info = metadata(args.tag)
        if args.source_sha and not re.fullmatch(r"[0-9a-f]{40}", args.source_sha):
            raise ValueError("--source-sha must be a full commit SHA")
        if args.metadata_only:
            for key, value in info.items():
                print(f"{key}={value}")
            return
        if args.docs_root is None or args.emqx_repo is None:
            raise ValueError("--docs-root and --emqx-repo are required")
        refresh(
            args.tag,
            args.emqx_repo.resolve(),
            args.docs_root.resolve(),
            GitHub(args.repository),
            args.source_sha,
            args.run_url,
        )
    except (ValueError, subprocess.CalledProcessError, OSError) as error:
        parser.exit(1, f"error: {error}\n")


if __name__ == "__main__":
    main()
