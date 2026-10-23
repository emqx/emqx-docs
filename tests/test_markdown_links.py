"""Check Markdown link destination rewriting."""
from pathlib import Path
import sys
import unittest

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / ".github/scripts"))
from markdown_sections import _rewrite_links


class LinkDestinationTests(unittest.TestCase):
    def test_plain_destinations_do_not_gain_angles(self):
        text = '[Page](<../page.md#section> "Title") ![Image](assets/a.svg) [Web](https://example.com/a?q=1&b=2)'
        expected = '[Page](../page.md#section "Title") ![Image](assets/a.svg) [Web](https://example.com/a?q=1&b=2)'
        self.assertEqual(_rewrite_links(text, text, lambda url, **kw: url), expected)

    def test_sensitive_destinations_keep_angles_and_titles(self):
        for url in ('assets/my image.svg', 'page(1).md', 'page).md', ''):
            with self.subTest(url=url):
                text = '[Read](source.md "Title")'
                result = _rewrite_links(text, text, lambda value, **kw: url)
                self.assertEqual(result, f'[Read](<{url}> "Title")')

    def test_reference_and_image_links_preserve_sensitive_targets(self):
        text = '[Read][ref] ![Image](<assets/my image.svg>)\n\n[ref]: ../page.md "Title"\n'
        result = _rewrite_links(text, text, lambda url, **kw: url)
        self.assertIn('[Read](../page.md "Title")', result)
        self.assertIn('![Image](<assets/my image.svg>)', result)

    def test_code_and_escaped_links_are_unchanged(self):
        text = '`[Code](<page.md>)`\n```md\n[Code](<page.md>)\n```\n'
        self.assertEqual(_rewrite_links(text, text, lambda url, **kw: url), text)


if __name__ == "__main__":
    unittest.main()
