"""Retain legacy TLS routes unchanged during additive batch3 migration."""
from pathlib import Path
import hashlib
import unittest

REPO = Path(__file__).resolve().parents[1]
LEGACY_HASHES = {'overview': '8457b551603a0e13b540a93e4aa3f46c8069acfb61c04f453941e7fcf09768d3', 'emqx-mqtt-tls': 'f154477df5d8c9cdd2766e428dab46662f8528722413500cbca07143e73bf792', 'mqtt-client-tls': '339096fa2483bfd62e7711cc89566398684a8c8c04be83b54e9b3781034a8869', 'tls-certificate': '02bd5e2b4f4ec44ddc5ea51225071d0f343c5f682a90d97dbf445904139381b9', 'crl': '32d6a60cd13880ffb280abba84167cf5bd2c5c38d499e1ecae829098dd63909f', 'ocsp': '238048d429132bc793fb61b252d7139b8380e8ac97e112d061cecabb3229fab4', 'psk-authentication': 'ab160d1c0b9bef62392516d690a1118b080379e7e0c8cb51c16529d641d35619'}


class TLSMigrationTests(unittest.TestCase):
    def test_legacy_tls_sources_are_preserved(self):
        for name, expected in LEGACY_HASHES.items():
            path = REPO / "en_US/guides/network" / (name + ".md")
            self.assertEqual(hashlib.sha256(path.read_bytes()).hexdigest(), expected, name)


if __name__ == "__main__":
    unittest.main()
