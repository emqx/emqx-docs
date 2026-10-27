"""Keep legacy access-control sources unchanged during additive migration."""
from pathlib import Path
import hashlib
import unittest


REPO = Path(__file__).resolve().parents[1]
LEGACY_HASHES = {'authn/authn': 'be079030ef4674e09a07f6459c92e9950777477692307ba8478dc772451a706d', 'authn/mnesia': '645b0dc5b276495b554726662ee941f047c2d6f6a02f7f073b19f5fd42c6e713', 'authn/user_management': '0f5daf9d9b9e574360914a6b2f4be0184a16031d7385dc9c81d916c5fda8b9da', 'authz/authz': '7872e4869f1c660b9a75e06a87ecef84aa7cd69e8d949587dbe7bb255c9aa52b', 'authz/mnesia': '53d9420ee7bf1754aff0b91af5270f7fa932567852ec283d2e1515cd99167746'}


class AccessControlMigrationTests(unittest.TestCase):
    def test_legacy_access_control_pages_are_preserved(self):
        for name, expected in LEGACY_HASHES.items():
            path = REPO / "en_US/guides/access-control" / (name + ".md")
            self.assertEqual(hashlib.sha256(path.read_bytes()).hexdigest(), expected, name)



if __name__ == "__main__":
    unittest.main()
