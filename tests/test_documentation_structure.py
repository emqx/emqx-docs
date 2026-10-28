"""Check documentation roles, navigation, and learning-path continuity."""
from pathlib import Path
import sys
import unittest

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / ".github/scripts"))
import yaml


class DocumentationStructureTests(unittest.TestCase):
    def test_migrated_pages_have_descriptions_without_redundant_titles(self):
        from test_documentation_links import PAGES
        from markdown_sections import expand_markdown
        root = REPO / "en_US"
        for name in PAGES:
            with self.subTest(page=name):
                path = root / name
                text = path.read_text()
                metadata = yaml.safe_load(text.split("---", 2)[1])
                self.assertIsInstance(metadata["description"], str)
                self.assertTrue(metadata["description"].strip())
                self.assertNotIn("title", metadata)
                self.assertFalse(set(metadata) - {
                    "description", "prev", "next", "aside", "pageClass",
                })
                expanded = expand_markdown(path, root, project_root=root).text
                self.assertEqual(expanded.count("\ndescription:"), 1)

    def test_wildcard_tutorial_and_reference_have_distinct_roles(self):
        tutorial = (REPO / "en_US/get-started/topics-and-wildcards.md").read_text()
        reference = (REPO / "en_US/develop/mqtt/wildcard-subscriptions.md").read_text()
        self.assertNotIn("<!-- include:", tutorial)
        self.assertIn("## Wildcard Basics", tutorial)
        self.assertIn("## Verify Single-Level Matching", tutorial)
        self.assertIn("## Verify Multi-Level Matching", tutorial)
        self.assertIn("reading-003", tutorial)
        self.assertNotIn("```bash", reference)
        self.assertIn("## Choose a Filter", reference)
        self.assertIn("## Design and Troubleshoot Filters", reference)
        self.assertIn("topics-and-wildcards.md#verify-single-level-matching", reference)
        self.assertIn("topics-and-wildcards.md#verify-multi-level-matching", reference)

    def test_develop_pages_use_canonical_navigation_groups(self):
        tree = yaml.safe_load((REPO / "dir.yaml").read_text())
        groups = {x.get("title_en"): x for x in tree["/develop/"] if isinstance(x, dict)}
        self.assertNotIn("Client Testing and MQTT Basics", groups)
        self.assertNotIn("TLS and Certificates", groups)
        self.assertEqual(groups["Client Connections"]["children"][0], "develop/clients/test-clients")
        tls = groups["Client Connections"]["children"][1]
        self.assertEqual(tls["title_en"], "MQTT over TLS")
        self.assertEqual([entry["path"] for entry in tls["children"]], [
            "develop/tls/" + name for name in (
                "index", "enable", "client-examples", "certificates", "crl", "ocsp", "psk",
            )
        ])
        self.assertEqual(groups["MQTT Messaging"]["children"], [
            "develop/mqtt/basics",
            {"title_en": "Wildcard Subscriptions", "path": "develop/mqtt/wildcard-subscriptions"},
        ])
        for label in ("Client Connections", "MQTT Messaging"):
            self.assertEqual(groups[label]["lang"], "en")
        for name, title, heading in (
            ("basics", "MQTT Basics", "# MQTT Basics"),
            ("wildcard-subscriptions", "Wildcard Subscriptions", "# Subscribe to Multiple Topics"),
        ):
            text = (REPO / "en_US/develop/mqtt" / (name + ".md")).read_text()
            body = text.split("---", 2)[2].lstrip()
            self.assertTrue(body.startswith(heading + "\n"))

    def test_essentials_navigation_and_task_sequence(self):
        tree = yaml.safe_load((REPO / "dir.yaml").read_text())
        groups = {x.get("title_en"): x for x in tree["/"] if isinstance(x, dict)}
        self.assertEqual(groups["First Steps"]["children"], [
            {"title_en": "Quick Start", "path": "get-started/quick-start"},
        ])
        quick_start = (REPO / "en_US/get-started/quick-start.md").read_text()
        metadata = yaml.safe_load(quick_start.split("---", 2)[1])
        self.assertNotIn("title", metadata)
        self.assertEqual(metadata["description"], "Run EMQX Enterprise locally, publish a message, and receive it with an MQTT client.")
        self.assertIn("# Quick Start: Send Your First MQTT Message\n", quick_start)
        self.assertEqual(groups["MQTT Essentials"]["lang"], "en")
        self.assertEqual(groups["MQTT Essentials"]["children"], [
            "get-started/connect-client", "get-started/publish-subscribe",
            "get-started/topics-and-wildcards", "get-started/qos",
        ])
        self.assertIn("(topics-and-wildcards.md)", (REPO / "en_US/get-started/publish-subscribe.md").read_text())
        self.assertIn("(qos.md)", (REPO / "en_US/get-started/topics-and-wildcards.md").read_text())

    def test_learning_path_pagers_do_not_return_to_legacy_quick_start(self):
        def metadata(name):
            text = (REPO / "en_US" / name).read_text()
            return yaml.safe_load(text.split("---", 2)[1])
        self.assertIs(metadata("index.md")["next"], False)
        self.assertIs(metadata("get-started/quick-start.md")["prev"], False)
        self.assertNotIn("next: false", (REPO / "en_US/get-started/qos.md").read_text())


if __name__ == "__main__":
    unittest.main()
