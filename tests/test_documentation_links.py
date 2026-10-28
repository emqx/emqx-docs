"""Check expanded documentation links, anchors, and source preservation."""
from pathlib import Path
import sys
import unittest

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / ".github/scripts"))
from urllib.parse import unquote, urlsplit
from markdown_sections import _headings, _rewrite_links, expand_markdown


PAGES = (
    "index.md", "get-started/quick-start.md", "get-started/connect-client.md",
    "get-started/publish-subscribe.md", "develop/clients/test-clients.md",
    "develop/mqtt/basics.md", "get-started/topics-and-wildcards.md",
    "get-started/qos.md", "develop/mqtt/wildcard-subscriptions.md",
    "get-started/password-authentication.md", "get-started/topic-permissions.md",
    "develop/authentication/index.md", "develop/authentication/password-backends.md",
    "develop/authentication/built-in-database.md",
    "develop/authorization/index.md", "develop/authorization/built-in-database.md",
    "get-started/connect-over-tls.md", "develop/tls/index.md",
    "develop/tls/enable.md", "develop/tls/client-examples.md",
    "develop/tls/certificates.md", "develop/tls/crl.md",
    "develop/tls/ocsp.md", "develop/tls/psk.md",
)


class DocumentationLinkTests(unittest.TestCase):
    def test_batch_pages_expand_and_local_links_resolve(self):
        root = REPO / "en_US"
        for name in PAGES:
            with self.subTest(page=name):
                path = root / name
                original = path.read_bytes()
                text = expand_markdown(path, root, project_root=root).text
                self.assertNotIn("<!-- include:", text)
                self.assertEqual(len([h for h in _headings(text) if h.level == 1]), 1)
                # Preserved overview content predates this batch and is not
                # silently repaired by the new-content link check.
                text = text.split("::: details EMQX Overview and Use Cases", 1)[0]
                def check(url, **kwargs):
                    parts = urlsplit(url)
                    if parts.scheme or parts.netloc:
                        return url
                    target = (path.parent / unquote(parts.path)).resolve() if parts.path else path
                    self.assertTrue(target.is_relative_to(root.resolve()), (name, url))
                    self.assertTrue(target.is_file(), (name, url))
                    if parts.fragment and target.suffix == ".md":
                        body = expand_markdown(target, root, project_root=root).text
                        self.assertIn(unquote(parts.fragment), {h.slug for h in _headings(body)}, (name, url))
                    return url
                _rewrite_links(text, text, check)
                self.assertEqual(path.read_bytes(), original)


if __name__ == "__main__":
    unittest.main()
