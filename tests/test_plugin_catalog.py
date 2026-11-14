import importlib.util
import io
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
GENERATOR = Path(os.environ.get("CATALOG_GENERATOR", ROOT / "scripts" / "refresh-plugin-download-links.py"))
spec = importlib.util.spec_from_file_location(
    "catalog_pr", ROOT / ".github/scripts/refresh_plugin_catalog.py"
)
helper = importlib.util.module_from_spec(spec)
spec.loader.exec_module(helper)
catalog = helper.catalog


def git(root, *args):
    return subprocess.check_output(
        ["git", "-C", str(root), *args], text=True, stderr=subprocess.PIPE
    ).strip()


def initialize(root):
    root.mkdir()
    git(root, "init", "-q")
    git(root, "config", "user.name", "test")
    git(root, "config", "user.email", "test@example.com")
    git(root, "config", "commit.gpgsign", "false")
    git(root, "config", "tag.gpgsign", "false")


class FakeGitHub:
    def __init__(self):
        self.pr = None
        self.creates = 0
        self.fail_create = False

    def pull_request(self, branch, base):
        return self.pr

    def set_milestone(self, number, version):
        self.pr["milestone"] = {"title": version}

    def create(self, info, run_url):
        self.creates += 1
        if self.fail_create:
            raise OSError("temporary API failure")
        self.pr = dict(
            state="OPEN",
            number=1,
            url="https://example.com/pull/1",
            milestone={"title": info["version"]},
        )
        return self.pr["url"]


class CatalogTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.source = self.root / "source"
        initialize(self.source)
        plugin = self.source / "plugins/emqx_demo"
        plugin.mkdir(parents=True)
        (plugin / "README.md").write_text("# Demo\n\nPlugin documentation.\n")
        for patch_number in range(1, 4):
            (plugin / "VERSION").write_text(f"1.0.{patch_number}\n")
            git(self.source, "add", "plugins/emqx_demo/README.md", "plugins/emqx_demo/VERSION")
            git(self.source, "commit", "-qm", f"release {patch_number}")
            git(self.source, "tag", f"6.3.{patch_number}")
        self.docs = self.root / "docs"
        initialize(self.docs)
        git(self.docs, "switch", "-c", "release-6.3")
        for locale in catalog.LOCALES:
            page = self.docs / locale / catalog.CATALOG_BASE / "6.3/emqx-demo.md"
            page.parent.mkdir(parents=True)
            page.write_text(f"# Demo {locale}\n\nTranslated introduction {locale}.\n")
        untouched = self.docs / "en_US" / catalog.CATALOG_BASE / "6.2/keep.md"
        untouched.parent.mkdir(parents=True)
        untouched.write_text("Another series.\n")
        git(
            self.docs,
            "add",
            "--",
            *[str(p.relative_to(self.docs)) for p in self.docs.rglob("*.md")],
        )
        git(self.docs, "commit", "-qm", "initial docs")
        self.remote = self.root / "remote.git"
        subprocess.run(["git", "init", "-q", "--bare", str(self.remote)], check=True)
        git(self.docs, "remote", "add", "origin", str(self.remote))
        git(self.docs, "push", "-u", "origin", "release-6.3")
        self.github = FakeGitHub()

    def generate(self, *extra):
        subprocess.run(
            [
                sys.executable,
                str(GENERATOR),
                "--docs-root",
                str(self.docs),
                "--emqx-repo",
                str(self.source),
                "--series",
                "6.3",
                "--no-fetch",
                *extra,
            ],
            check=True,
            capture_output=True,
            text=True,
        )

    def refresh(self):
        with patch.object(helper, "verify_packages", return_value=True):
            helper.refresh(
                "6.3.2", self.source, self.docs, self.github, git(self.source, "rev-parse", "6.3.2")
            )

    def test_generator_limits_tags_and_preserves_locales(self):
        self.generate("--through-tag", "6.3.2")
        for locale in catalog.LOCALES:
            text = (self.docs / locale / catalog.CATALOG_BASE / "6.3/emqx-demo.md").read_text()
            self.assertIn(f"Translated introduction {locale}.", text)
            self.assertIn("| 6.3.1 | 1.0.1 |", text)
            self.assertIn("| 6.3.2 | 1.0.2 |", text)
            self.assertNotIn("| 6.3.3 |", text)
        self.assertEqual(
            (self.docs / "en_US" / catalog.CATALOG_BASE / "6.2/keep.md").read_text(),
            "Another series.\n",
        )
        before = git(self.docs, "diff")
        self.generate("--through-tag", "6.3.2")
        self.assertEqual(git(self.docs, "diff"), before)

    def test_stable_release_corrects_upcoming_row(self):
        page = self.docs / "ja_JP" / catalog.CATALOG_BASE / "6.3/emqx-demo.md"
        upcoming = catalog.ReleaseTag(raw="6.3.2", major=6, minor=3, patch=2)
        page.write_text(
            "Reviewed Japanese prose.\n\n"
            + catalog.render_section("ja_JP", "emqx_demo", [(upcoming, "0.9.0")])
            + "\n"
        )
        self.generate("--through-tag", "6.3.2")
        text = page.read_text()
        self.assertIn("Reviewed Japanese prose.", text)
        self.assertIn("| 6.3.2 | 1.0.2 |", text)
        self.assertNotIn("0.9.0", text)

    def test_invalid_release_bounds(self):
        for extra in [
            ("--through-tag", "6.2.1"),
            ("--through-tag", "6.3.4"),
            ("--through-tag", "6.3.2-rc.1"),
            ("--through-tag", "6.3.2", "--upcoming", "6.3.4"),
        ]:
            with self.subTest(extra=extra), self.assertRaises(subprocess.CalledProcessError):
                self.generate(*extra)

    def test_refresh_is_idempotent(self):
        self.refresh()
        first = git(self.docs, "rev-parse", "HEAD")
        self.refresh()
        self.assertEqual(self.github.creates, 1)
        self.assertEqual(git(self.docs, "rev-parse", "HEAD"), first)
        self.assertEqual(git(self.docs, "status", "--porcelain"), "")

    def test_retry_preserves_reviewer_edits_and_new_base_commits(self):
        self.refresh()
        page = self.docs / "ja_JP" / catalog.CATALOG_BASE / "6.3/emqx-demo.md"
        page.write_text(page.read_text().replace("Translated introduction", "Reviewed translation"))
        git(self.docs, "add", str(page.relative_to(self.docs)))
        git(self.docs, "commit", "-qm", "reviewer edit")
        git(self.docs, "push", "origin", "HEAD")
        git(self.docs, "switch", "release-6.3")
        (self.docs / "other.txt").write_text("new base change")
        git(self.docs, "add", "other.txt")
        git(self.docs, "commit", "-qm", "base update")
        git(self.docs, "push", "origin", "HEAD")
        self.refresh()
        self.assertIn("Reviewed translation", page.read_text())
        self.assertEqual((self.docs / "other.txt").read_text(), "new base change")
        self.assertEqual(self.github.creates, 1)

    def test_retry_after_push_before_pr_creation(self):
        self.github.fail_create = True
        with self.assertRaises(OSError):
            self.refresh()
        first = git(self.docs, "rev-parse", "HEAD")
        self.github.fail_create = False
        self.refresh()
        self.assertEqual(git(self.docs, "rev-parse", "HEAD"), first)
        self.assertEqual(self.github.pr["state"], "OPEN")

    def test_closed_or_merged_pr_is_respected(self):
        first = git(self.docs, "rev-parse", "HEAD")
        for state in ["CLOSED", "MERGED"]:
            self.github.pr = dict(state=state, url="https://example.com/pull/1")
            self.refresh()
            self.assertEqual(git(self.docs, "rev-parse", "HEAD"), first)
            self.assertEqual(self.github.creates, 0)

    def test_retry_repairs_missing_milestone(self):
        self.refresh()
        self.github.pr["milestone"] = None
        self.refresh()
        self.assertEqual(self.github.pr["milestone"]["title"], "6.3.2")
        self.assertEqual(self.github.creates, 1)

    def test_wrong_source_commit_is_rejected(self):
        with patch.object(helper, "verify_packages") as verify, self.assertRaises(ValueError):
            helper.refresh("6.3.2", self.source, self.docs, self.github, "0" * 40)
        verify.assert_not_called()

    def test_missing_packages_do_not_create_pr(self):
        with patch.object(helper, "verify_packages", side_effect=OSError("404")), self.assertRaises(
            OSError
        ):
            helper.refresh("6.3.2", self.source, self.docs, self.github)
        self.assertEqual(self.github.creates, 0)


class MetadataTests(unittest.TestCase):
    def test_release_metadata(self):
        for tag, series in [
            ("6.3.3", "6.3"),
            ("6.0.5", "6.0"),
            ("e5.10.6", "5.10"),
            ("7.0.0", "7.0"),
        ]:
            with self.subTest(tag=tag):
                self.assertEqual(helper.metadata(tag)["base"], f"release-{series}")
        for tag in ["6.3.4-rc.1", "6.3.4-preview.1", "e6.3.4", "5.10.6", "e5.8.12", "x;echo bad"]:
            with self.subTest(tag=tag), self.assertRaises(ValueError):
                helper.metadata(tag)

    def test_pr_is_draft_and_has_release_milestone(self):
        github = helper.GitHub("emqx/emqx-docs")
        calls = []
        with patch.object(github, "milestone", return_value="6.3.3"), patch.object(
            helper, "command", side_effect=lambda *args: calls.append(args) or "PR"
        ):
            github.create(
                {**helper.metadata("6.3.3"), "source_commit": "a" * 40}, "https://example.com/run/1"
            )
        self.assertIn("--draft", calls[0])
        self.assertEqual(calls[0][calls[0].index("--milestone") + 1], "6.3.3")

    def test_milestone_is_reused_or_created(self):
        github = helper.GitHub("emqx/emqx-docs")
        for pages, expected_calls in [('[[{"title":"6.3.3"}]]', 1), ("[[]]", 2)]:
            with patch.object(helper, "command", side_effect=[pages, "{}"]) as command:
                self.assertEqual(github.milestone("6.3.3"), "6.3.3")
                self.assertEqual(command.call_count, expected_calls)

    def test_s3_package_and_checksum_checks(self):
        class Response(io.BytesIO):
            headers = {"Content-Length": "7"}

        for digest, size, accepted in [
            ("a" * 64, "7", True),
            ("invalid", "7", False),
            ("a" * 64, "0", False),
        ]:

            def fetch(url, timeout):
                response = Response(digest.encode())
                response.headers = {"Content-Length": size}
                return response

            with self.subTest(digest=digest, size=size), patch.object(
                catalog, "list_plugins_at", return_value=["emqx_demo"]
            ), patch.object(catalog, "read_plugin_version", return_value="1.0.0"), patch.object(
                helper.urllib.request, "urlopen", side_effect=fetch
            ):
                if accepted:
                    self.assertTrue(helper.verify_packages(Path("."), "6.3.3"))
                else:
                    with self.assertRaises(ValueError):
                        helper.verify_packages(Path("."), "6.3.3")


if __name__ == "__main__":
    unittest.main()
