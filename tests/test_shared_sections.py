"""Check shared section expansion and safe build-copy processing."""
from pathlib import Path
import sys
import tempfile
import unittest

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / ".github/scripts"))
from markdown_sections import IncludeError, expand_markdown
from expand_shared_sections import expand_build_copy


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
        self.assertIn("](#child)", self.expand(p).text)

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


class BuildCopyTests(unittest.TestCase):
    def test_build_expansion_is_repeatable(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "source.md").write_text("# Source\n## Shared\nText.\n")
            task = root / "task.md"
            task.write_text("# Task\n<!-- include: source.md#shared -->\n")
            self.assertEqual(expand_build_copy(root), (task.resolve(),))
            self.assertIn("## Shared", task.read_text())
            self.assertEqual(expand_build_copy(root), ())

    def test_invalid_dependency_does_not_partially_write(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "source.md").write_text("# Source\n## Shared\nText.\n")
            task = root / "a.md"
            original = "# Task\n<!-- include: source.md#shared -->\n"
            task.write_text(original)
            (root / "b.md").write_text("<!-- include: missing.md#body -->\n")
            with self.assertRaises(IncludeError):
                expand_build_copy(root)
            self.assertEqual(task.read_text(), original)


if __name__ == "__main__":
    unittest.main()
