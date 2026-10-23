"""Check the isolated migration fixture and its safety boundaries."""
from pathlib import Path
import sys
import tempfile
import subprocess
import json
import unittest

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / ".github/scripts"))
from urllib.parse import unquote, urlsplit
from markdown_sections import _headings, _rewrite_links
from prepare_migration_pilot import prepare


class PilotTests(unittest.TestCase):
    def test_html_checker_rejects_empty_dist_in_optimized_mode(self):
        with tempfile.TemporaryDirectory() as tmp:
            result = subprocess.run(
                [sys.executable, "-O", str(REPO / ".github/scripts/check_migration_pilot_html.py"), "--dist", tmp],
                capture_output=True, text=True,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("Expected 6 rendered fixture pages", result.stderr)

    def test_output_is_closed_and_navigation_generated(self):
        with tempfile.TemporaryDirectory() as tmp:
            output = prepare(REPO, Path(tmp) / "output")
            directory = json.loads((output / "directory.json").read_text())
            self.assertEqual([r["title"] for r in directory["nav"]["en"]], ["Get Started", "Develop", "Operate", "Reference"])
            self.assertEqual(directory["en"]["/"][0]["title"], "EMQX Documentation Migration Sample")
            self.assertEqual(directory["en"]["/"][0]["path"], "./")
            self.assertEqual(directory["en"]["/reference/"][0]["path"], "reference/")
            self.assertEqual(len(list((output / "en_US").rglob("*.md"))), 6)
            for page in (output / "en_US").rglob("*.md"):
                text = page.read_text()
                self.assertNotIn("<!-- include:", text)
                def check(url, *, image=False):
                    parts = urlsplit(url)
                    if parts.scheme or parts.netloc:
                        return url
                    target = (page.parent / unquote(parts.path)).resolve() if parts.path else page
                    self.assertTrue(target.is_file(), (page, url))
                    if parts.fragment and target.suffix == ".md":
                        anchors = {h.slug for h in _headings(target.read_text())}
                        self.assertIn(unquote(parts.fragment), anchors, (page, url))
                    return url
                _rewrite_links(text, text, check)
            included = (output / "en_US/get-started/publish-subscribe.md").read_text()
            self.assertIn("| Observation | Check Next |", included)
            self.assertIn("## Prerequisites", included)
            self.assertIn("../reference/index.md", included)
            self.assertNotIn("../../reference/index.md", included)

    def test_refuses_existing_output(self):
        with tempfile.TemporaryDirectory() as tmp:
            with self.assertRaises(ValueError):
                prepare(REPO, Path(tmp))

    def test_refuses_repository_output(self):
        with self.assertRaises(ValueError):
            prepare(REPO, REPO / "must-not-create-pilot-output")

    def test_production_navigation_is_not_changed(self):
        before = [(REPO / p).read_bytes() for p in ("dir.yaml", "nav.yaml")]
        with tempfile.TemporaryDirectory() as tmp:
            prepare(REPO, Path(tmp) / "output")
        self.assertEqual(before, [(REPO / p).read_bytes() for p in ("dir.yaml", "nav.yaml")])


if __name__ == "__main__":
    unittest.main()
