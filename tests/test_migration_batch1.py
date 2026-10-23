"""Check legacy entry retention during the first migration batch."""
from pathlib import Path
import sys
import unittest

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / ".github/scripts"))
import yaml


class MigrationCompatibilityTests(unittest.TestCase):
    def test_old_pages_and_navigation_are_retained(self):
        tree = yaml.safe_load((REPO / "dir.yaml").read_text())
        def paths(value):
            if isinstance(value, str):
                yield value
            elif isinstance(value, list):
                for item in value:
                    yield from paths(item)
            elif isinstance(value, dict):
                if "path" in value:
                    yield value["path"]
                for key, item in value.items():
                    if key != "path":
                        if isinstance(item, (dict, list)):
                            yield from paths(item)
        entries = set(paths(tree))
        for name in ("get-started/getting-started", "get-started/messaging/publish-and-subscribe", "get-started/mqtt-basics"):
            self.assertTrue((REPO / "en_US" / (name + ".md")).is_file())
            self.assertIn(name, entries)
        self.assertIn("## EMQX Overview", (REPO / "en_US/index.md").read_text())


if __name__ == "__main__":
    unittest.main()
