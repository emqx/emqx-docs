"""Check documentation roles, navigation, and learning-path continuity."""
from pathlib import Path
import sys
import unittest

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / ".github/scripts"))
import yaml


class DocumentationStructureTests(unittest.TestCase):
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
        self.assertEqual(groups["Client Connections"]["children"], ["develop/clients/test-clients"])
        self.assertEqual(groups["MQTT Messaging"]["children"], [
            "develop/mqtt/basics", "develop/mqtt/wildcard-subscriptions",
        ])
        for label in ("Client Connections", "MQTT Messaging"):
            self.assertEqual(groups[label]["lang"], "en")
        for name, title, heading in (
            ("basics", "MQTT Basics", "# MQTT Basics"),
            ("wildcard-subscriptions", "Wildcard Subscriptions", "# Subscribe to Multiple Topics"),
        ):
            text = (REPO / "en_US/develop/mqtt" / (name + ".md")).read_text()
            self.assertEqual(yaml.safe_load(text.split("---", 2)[1])["title"], title)
            self.assertIn(heading, text)

    def test_essentials_navigation_and_task_sequence(self):
        tree = yaml.safe_load((REPO / "dir.yaml").read_text())
        groups = {x.get("title_en"): x for x in tree["/"] if isinstance(x, dict)}
        self.assertEqual(groups["First Steps"]["children"], ["get-started/quick-start"])
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
        self.assertIs(metadata("get-started/qos.md")["next"], False)


if __name__ == "__main__":
    unittest.main()
