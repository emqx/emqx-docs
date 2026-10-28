"""Protect TLS task continuity, certificate trust, and revocation boundaries."""
from pathlib import Path
import json
import subprocess
import sys
import unittest

import yaml

REPO = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO / ".github/scripts"))
from markdown_sections import expand_markdown


class TLSTests(unittest.TestCase):
    def guide(self, name):
        return (REPO / "en_US/develop/tls" / (name + ".md")).read_text()

    def test_intro_shares_verification_without_advanced_configuration(self):
        root = REPO / "en_US"
        path = root / "get-started/connect-over-tls.md"
        original = path.read_bytes()
        text = expand_markdown(path, root, project_root=root).text
        self.assertIn("## Verify a Message Exchange", text)
        self.assertIn("## Troubleshoot and Clean Up", text)
        self.assertNotIn("## Require Mutual TLS", text)
        self.assertNotIn("**Certificate Source**", text)
        self.assertNotIn("<!-- include:", text)
        self.assertIn("-u mqtt-user -P 'Example-only-password!'", text)
        for value in ("127.0.0.1:8883", "Subject Alternative Name", "replacement container",
                      "rootCA.crt", "docs/tls", "hello over TLS", "tls-sub", "tls-pub"):
            self.assertIn(value, text)
        self.assertNotIn("--insecure", text)
        self.assertNotIn("docs-device", text)
        self.assertEqual(path.read_bytes(), original)

    def test_listener_verification_and_resumption_conditions(self):
        text = self.guide("enable")
        for value in ("verify = verify_none", "fail_if_no_peer_cert = false",
                      "verify = verify_peer", "fail_if_no_peer_cert = true",
                      "--ca rootCA.crt", "--cert client.crt --key client.key",
                      "managed_certs", "namespace", "stateless_with_cert",
                      "node.tls_stateless_tickets_seed", "0-RTT", "MQTT credentials"):
            self.assertIn(value, text)
        for value in ("**Certificate Source**", "**Enter Manually**", "**TLS Cert**",
                      "**TLS Key**", "**Select from Managed Certs**", "**Namespace**",
                      "**Managed Cert Bundle Name**", "**CA Cert**", "**Update**",
                      "certificates.md#create-a-bundle-in-the-dashboard",
                      "../../guides/configuration/secret-from-file.md"):
            self.assertIn(value, text)
        self.assertNotIn("--insecure", text)
        self.assertIn("the TLS connection must fail", text)
        tickets = text.split("### TLS Session Tickets", 1)[1].split("## Troubleshoot", 1)[0]
        self.assertIn("These ticket modes apply only to TLS 1.3", tickets)
        self.assertIn("TLS 1.2 session reuse is controlled separately by `ssl_options.reuse_sessions`", tickets)
        self.assertIn("disabled by default (`false`)", tickets)
        self.assertNotIn("does not support stateful TLS 1.2 session resumption", text)

    def test_revocation_and_psk_boundaries(self):
        crl, ocsp, psk = (self.guide(name) for name in ("crl", "ocsp", "psk"))
        for value in ("Issuing Distribution Point", "15m", "15s", "WSS or QUIC",
                      "already-connected", "enable_crl_check = true",
                      "HTTP or HTTPS CRL Distribution Point", "other CDP protocols are not supported",
                      "crl_cache.failure_threshold", "No cached CRL", "still-valid cached CRL",
                      "cache entry is evicted", "Cached CRL has expired"):
            self.assertIn(value, crl)
        for value in ("explicit server `certfile`", "refresh_interval = 5m", "refresh_http_timeout = 15s",
                      "WSS and QUIC", "client enforces OCSP",
                      "does not by itself validate the OCSP response's signature",
                      "A failed refresh does not immediately remove the cached response",
                      "the node's OCSP response cache is empty",
                      "Validate the response's signature, certificate match, status, and validity times",
                      "without a stapled response", "response is missing or expired",
                      "reports an `unknown` or `revoked` certificate status",
                      "does not configure that client policy"):
            self.assertIn(value, ocsp)
        self.assertNotIn("refresh_interval = 15m", ocsp)
        self.assertNotIn("no response is cached for the server certificate", ocsp)
        for value in ("RSA-PSK-AES128-GCM-SHA256", 'versions = ["tlsv1.2"]',
                      "rather than interpreting", "Unknown PSK identity", "incorrect key",
                      "does not automatically create", "Deleting the initialization file alone",
                      "does not replace the complete database", "does not delete the stored identity",
                      "Restarting EMQX does not clear", "restricts the listener to TLS 1.2"):
            self.assertIn(value, psk)
        self.assertNotIn("suppresses the external PSK cipher suites", psk)
        headings = [line for line in psk.splitlines() if line.startswith("## ")]
        self.assertEqual(headings, ["## " + title for title in (
            "Before You Begin", "Create the PSK File", "Enable PSK Authentication",
            "Configure the TLS Listener", "Verify the Connection", "Update PSK Data",
            "Troubleshooting and Cleanup",
        )])
        maintenance = psk.split("## Update PSK Data", 1)[1].split("## Troubleshooting", 1)[0]
        self.assertIn("does not replace the complete database", maintenance)
        self.assertIn("#create-the-psk-file", psk)
        self.assertNotIn('"tlsv1.1"', psk)

    def test_certificate_management_preserves_operations_and_key_safety(self):
        text = self.guide("certificates")
        for value in ("OpenSSL 3", "subjectAltName=DNS:localhost,IP:127.0.0.1",
                      "extendedKeyUsage=clientAuth", "-purpose sslclient", "-verify_hostname",
                      "POST /certs/global/name/{name}", "POST /certs/ns/{namespace}/name/{name}",
                      "GET /certs/global/list", "DELETE /certs/global/name/{name}",
                      "JSON fields containing PEM strings", "key_password", "acc_key",
                      "referencing_configs", "force_delete=true", "not copy it to EMQX",
                      "every resource", "existing MQTT connection"):
            self.assertIn(value, text)

    def test_tls_navigation_and_task_pagination_generate(self):
        overview = (REPO / "en_US/develop/authentication/index.md").read_text()
        self.assertIn("[TLS-PSK](../tls/psk.md)", overview)
        self.assertIn("| [TLS-PSK](../tls/psk.md) | TLS handshake, before MQTT authentication |", overview)
        for edition in ("ce", "ee"):
            generated = json.loads(subprocess.check_output([sys.executable, "-B", "gen.py", edition], cwd=REPO))
            entries = {}
            def visit(value):
                if isinstance(value, dict):
                    if "path" in value:
                        entries[value["path"]] = value["title"]
                    for item in value.values():
                        visit(item)
                elif isinstance(value, list):
                    for item in value:
                        visit(item)
            groups = {entry["title"]: entry for entry in generated["en"]["/develop/"]}
            self.assertNotIn("TLS and Certificates", groups)
            tls = groups["Client Connections"]["children"][1]
            self.assertEqual(tls["title"], "MQTT over TLS")
            self.assertEqual([entry["path"] for entry in tls["children"]], [
                "develop/tls/" + name for name in (
                    "index", "enable", "client-examples", "certificates", "crl", "ocsp", "psk",
                )
            ])
            visit(generated["en"])
            self.assertEqual(entries["develop/tls/index"], "Overview")
            self.assertEqual(entries["develop/tls/certificates"], "Certificates")
            self.assertEqual(entries["get-started/connect-over-tls"], "Connect over TLS")
            for name in ("enable", "client-examples", "certificates", "crl", "ocsp", "psk"):
                self.assertIn("develop/tls/" + name, entries)
        self.assertFalse((REPO / "en_US/develop/clients/tls").exists())
        task = (REPO / "en_US/get-started/topic-permissions.md").read_text()
        self.assertIn("](connect-over-tls.md)", task)
        self.assertNotIn("next: false", task)

    def test_ci_triggers_include_tls_sources_and_dependencies(self):
        config = yaml.load((REPO / ".github/workflows/migration-pilot.yaml").read_text(), Loader=yaml.BaseLoader)
        paths = config["on"]["pull_request"]["paths"]
        for value in ("en_US/develop/tls/**", "en_US/get-started/connect-over-tls.md",
                      "en_US/guides/network/**", "en_US/guides/configuration/listener.md",
                      "en_US/get-started/deploy/install-docker.md", "en_US/develop/connect-emqx/**"):
            self.assertIn(value, paths)


if __name__ == "__main__":
    unittest.main()
