"""Protect authentication and authorization learning paths and maintained guides."""
from pathlib import Path
import subprocess
import sys
import unittest

import yaml

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / ".github/scripts"))
from markdown_sections import expand_markdown


class AccessControlTests(unittest.TestCase):
    def expanded(self, name):
        root = REPO / "en_US"
        return expand_markdown(root / name, root, project_root=root).text

    def test_tasks_keep_the_cli_verification_path(self):
        authn = self.expanded("get-started/password-authentication.md")
        authz = self.expanded("get-started/topic-permissions.md")
        self.assertIn("mqttx conn", authn)
        self.assertIn("wrong-password", authn)
        self.assertIn("No credentials", authn)
        self.assertIn("exit with code `0`", authn)
        self.assertNotIn("## Optional: Verify with MQTTX Desktop", authn)
        self.assertIn("mqttx sub", authz)
        self.assertIn("mqttx pub", authz)
        self.assertIn("authz-allowed-001", authz)
        self.assertIn("authz-allowed-002", authz)
        self.assertIn("authz-denied-001", authz)
        self.assertNotIn("**Received**", authz)
        self.assertIn("## Verify Authorization Rules", authz)
        self.assertIn("### Verify with MQTTX CLI", authz)
        self.assertNotIn("### Verify with MQTTX Desktop", authz)
        for text in (authn, authz):
            self.assertNotIn("<!-- include:", text)
            self.assertIn("-u mqtt-user -P 'Example-only-password!'", text)
            self.assertNotIn("docs-device", text)
            self.assertNotIn("Docs-test-only-2026!", text)

    def test_policy_setup_and_restoration_are_explicit(self):
        text = self.expanded("get-started/topic-permissions.md")
        for value in ("**No Match Action**", "**Deny Action**", "`deny`", "`ignore`",
                      "cache", "Disable unrelated", "legacy", "**Is superuser**",
                      "Restore the Local Learning Policy", "recorded", "127.0.0.1"):
            self.assertIn(value, text)
        self.assertIn("## Create the Authorizer in the Dashboard\n", text)
        self.assertNotIn("## Create the Authorizer\n", text)
        self.assertIn("## Configure Authorization Rules", text)
        self.assertIn("### Add a Username Rule", text)
        for heading in ("Authorizer Options", "Common Rule Fields", "Scope-Specific Fields", "Order Permissions"):
            self.assertNotIn("## " + heading, text)
        self.assertIn("../develop/authorization/assets/built-in-database-username-rule.jpg", text)
        self.assertNotIn("## Create the Source", text)
        self.assertNotIn("**Superuser**", text)
        self.assertIn("Keep the authentication account and its password", text)
        self.assertIn("not a production authorization configuration", text)

    def test_learning_path_links_and_credentials_transition(self):
        qos = (REPO / "en_US/get-started/qos.md").read_text()
        authn = (REPO / "en_US/get-started/password-authentication.md").read_text()
        authz = (REPO / "en_US/get-started/topic-permissions.md").read_text()
        self.assertIn("(password-authentication.md)", qos)
        self.assertNotIn("next: false", qos)
        self.assertIn("(topic-permissions.md)", authn)
        self.assertIn("anonymous commands", authn)
        self.assertIn("no longer apply unchanged", authn)
        self.assertIn("Removing the disposable container deletes", authn)
        self.assertIn("next: false", authz)
        self.assertNotIn("](connect-over-tls.md)", authz)

    def test_navigation_uses_reader_topics(self):
        tree = yaml.safe_load((REPO / "dir.yaml").read_text())
        def groups(key):
            return {x.get("title_en"): x for x in tree[key] if isinstance(x, dict)}
        start, develop = groups("/"), groups("/develop/")
        self.assertEqual(start["Secure Your Clients"]["children"], [
            "get-started/password-authentication", "get-started/topic-permissions",
        ])
        self.assertEqual(develop["Client Authentication"]["children"], [
            {"title_en": "Choose a Method", "path": "develop/authentication/index"},
            {"title_en": "Choose a Password Backend", "path": "develop/authentication/password-backends"},
            {"title_en": "Built-in Database", "path": "develop/authentication/built-in-database"},
        ])
        self.assertEqual(develop["Client Authorization"]["children"], [
            {"title_en": "MQTT Authorization", "path": "develop/authorization/index"},
            {"title_en": "Built-in Database", "path": "develop/authorization/built-in-database"},
        ])
        for group in (start["Secure Your Clients"], develop["Client Authentication"],
                      develop["Client Authorization"]):
            self.assertEqual(group["lang"], "en")

    def test_shared_expansion_does_not_modify_sources(self):
        for name in ("get-started/password-authentication.md", "get-started/topic-permissions.md"):
            path = REPO / "en_US" / name
            before = path.read_bytes()
            self.expanded(name)
            self.assertEqual(path.read_bytes(), before)

    def test_backend_guides_keep_operational_details(self):
        overview = (REPO / "en_US/develop/authentication/index.md").read_text()
        backends = (REPO / "en_US/develop/authentication/password-backends.md").read_text()
        self.assertIn("[Password-based authentication](password-backends.md)", overview)
        for backend in ("mysql", "postgresql", "mongodb", "redis", "ldap", "http"):
            link = "../../guides/access-control/authn/" + backend + ".md"
            self.assertNotIn("](" + link + ")", overview)
            self.assertIn("](" + link + ")", backends)
        authn = (REPO / "en_US/develop/authentication/built-in-database.md").read_text()
        authz = (REPO / "en_US/develop/authorization/built-in-database.md").read_text()
        overview = (REPO / "en_US/develop/authorization/index.md").read_text()
        for text in (authn, authz, overview):
            self.assertIn("mqtt-user", text)
            self.assertNotIn("docs-device", text)
            self.assertNotIn("Docs-test-only-2026!", text)
        self.assertIn("mqtt-user,Example-only-password!,false", authn)
        self.assertIn('"username": "mqtt-user"', authz)
        for value in ("bcrypt", "salt_rounds = 10", "bootstrap_file", "bootstrap_type",
                      "password_hash", "is_superuser", "existing user IDs", "Namespace"):
            self.assertIn(value, authn)
        for value in ("/api/v5/authorization/sources", "rules/users", "rules/clients",
                      "rules/all", "username_re", "clientid_re", "listener_re", "zone_re"):
            self.assertIn(value, authz)
        for value in ("0x87", "135", "**No match**", "**Deny**", "**Published**",
                      "same connection surviving a denial", "MQTT 3.1.1", "QoS 0"):
            self.assertIn(value, overview)

    def test_ci_triggers_include_tasks_backends_and_legacy_dependencies(self):
        config = yaml.load((REPO / ".github/workflows/migration-pilot.yaml").read_text(),
                           Loader=yaml.BaseLoader)
        paths = config["on"]["pull_request"]["paths"]
        for path in ("en_US/get-started/password-authentication.md",
                     "en_US/get-started/topic-permissions.md",
                     "en_US/develop/authentication/**", "en_US/develop/authorization/**",
                     "en_US/guides/access-control/**"):
            self.assertIn(path, paths)


if __name__ == "__main__":
    unittest.main()
