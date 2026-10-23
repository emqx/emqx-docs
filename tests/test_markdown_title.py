"""Check Markdown titles and front matter."""
from pathlib import Path
import sys
import tempfile
import unittest

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / ".github/scripts"))
from markdown_title import read_markdown_title


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


if __name__ == "__main__":
    unittest.main()
