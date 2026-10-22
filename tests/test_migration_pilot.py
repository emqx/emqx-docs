"""Regression checks for opt-in section reuse and navigation generation."""
from pathlib import Path
import json
import re
import subprocess
import sys
import tempfile
import unittest
from urllib.parse import unquote, urlsplit

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / ".github/scripts"))
from markdown_title import read_markdown_title
from markdown_sections import IncludeError, _headings, _rewrite_links, expand_markdown
from prepare_migration_pilot import prepare


class TitleTests(unittest.TestCase):
    def title(self, text):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "page.md"
            p.write_text(text, encoding="utf-8")
            return read_markdown_title(p)

    def test_plain_h1(self):
        self.assertEqual(self.title("\n# Publish and Subscribe\n"), "Publish and Subscribe")

    def test_front_matter_title(self):
        self.assertEqual(self.title('---\ntitle: "Quick Start: Send Your First MQTT Message"\n---\n# Other\n'), "Quick Start: Send Your First MQTT Message")

    def test_description_only(self):
        self.assertEqual(self.title("---\ndescription: Example\n---\n\n# Publish and Subscribe\n"), "Publish and Subscribe")

    def test_bom_and_crlf(self):
        self.assertEqual(self.title("\ufeff---\r\ndescription: Example\r\n---\r\n# Reference\r\n"), "Reference")

    def test_unclosed_metadata_fails(self):
        with self.assertRaises(ValueError):
            self.title("---\ntitle: Broken\n")

    def test_non_mapping_metadata_fails(self):
        with self.assertRaises(ValueError):
            self.title("---\n- item\n---\n# Reference\n")

    def test_non_string_title_falls_back(self):
        self.assertEqual(self.title("---\ntitle: 123\n---\n# Reference\n"), "Reference")


class SectionTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)

    def write(self, name, text):
        p = self.root / name
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text, encoding="utf-8")
        return p

    def expand(self, page):
        return expand_markdown(page, self.root, project_root=self.root)

    def test_section_stops_before_next_peer(self):
        self.write("source.md", "# Source\n## Reused Section\nKeep.\n### Child\nAlso keep.\n## Next\nExclude.\n")
        p = self.write("task.md", "# Task\n<!-- include: source.md#reused-section -->\n")
        text = self.expand(p).text
        self.assertIn("### Child", text)
        self.assertNotIn("Exclude", text)
        self.assertNotIn("# Source", text)

    def test_rebases_links_and_images(self):
        self.write("develop/assets/a.svg", "<svg/>")
        self.write("reference/index.md", "# Reference\n")
        self.write("develop/source.md", "# Source\n## Section\n[Reference](../reference/index.md) ![Image](assets/a.svg)\n")
        p = self.write("get-started/task.md", "# Task\n<!-- include: ../develop/source.md#section -->\n")
        text = self.expand(p).text
        self.assertIn("../reference/index.md", text)
        self.assertIn("../develop/assets/a.svg", text)

    def test_local_fragment_stays_local(self):
        self.write("source.md", "# Source\n## Section\n[Child](#child)\n### Child\nText.\n")
        p = self.write("task.md", "# Task\n<!-- include: source.md#section -->\n")
        self.assertIn("<#child>", self.expand(p).text)

    def test_missing_section_fails(self):
        self.write("source.md", "# Source\n")
        p = self.write("task.md", "<!-- include: source.md#missing -->\n")
        with self.assertRaises(IncludeError):
            self.expand(p)

    def test_ambiguous_section_fails(self):
        self.write("source.md", "# Source\n## Same\nA\n## Same\nB\n")
        p = self.write("task.md", "<!-- include: source.md#same -->\n")
        with self.assertRaises(IncludeError):
            self.expand(p)

    def test_missing_file_fails(self):
        p = self.write("task.md", "<!-- include: absent.md#body -->\n")
        with self.assertRaises(IncludeError):
            self.expand(p)

    def test_cycle_fails(self):
        p = self.write("a.md", "<!-- include: b.md#body -->\n")
        self.write("b.md", "<!-- include: a.md#body -->\n")
        with self.assertRaises(IncludeError):
            self.expand(p)

    def test_outside_root_fails(self):
        p = self.write("task.md", "<!-- include: ../outside.md#body -->\n")
        with self.assertRaises(IncludeError):
            self.expand(p)

    def test_symlink_outside_root_fails(self):
        with tempfile.TemporaryDirectory() as outside:
            dest = Path(outside) / "source.md"
            dest.write_text("# Outside\n", encoding="utf-8")
            (self.root / "source.md").symlink_to(dest)
            p = self.write("task.md", "<!-- include: source.md#body -->\n")
            with self.assertRaises(IncludeError):
                self.expand(p)

    def test_code_bytes_preserved(self):
        code = "```bash\necho '${payload}'\n<!-- include: absent.md#body -->\n```"
        self.write("source.md", "# Source\n## Section\n" + code + "\n")
        p = self.write("task.md", "<!-- include: source.md#section -->\n")
        self.assertIn(code, self.expand(p).text)

    def test_inline_code_is_not_a_directive(self):
        p = self.write("task.md", "# Task\nUse `<!-- include: absent.md#body -->` as an example.\n")
        self.assertIn("`<!-- include: absent.md#body -->`", self.expand(p).text)

    def test_reference_link_definition_outside_section(self):
        self.write("reference.md", "# Reference\n")
        self.write("source.md", "# Source\n## Section\n[Read][ref]\n## Next\n[ref]: reference.md\n")
        p = self.write("tasks/task.md", "<!-- include: ../source.md#section -->\n")
        self.assertIn("../reference.md", self.expand(p).text)

    def test_title_case_preserves_selector(self):
        self.write("source.md", "# Source\n## If the Message Does Not Arrive\nText.\n")
        p = self.write("task.md", "<!-- include: source.md#if-the-message-does-not-arrive -->\n")
        self.assertIn("## If the Message Does Not Arrive", self.expand(p).text)


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
