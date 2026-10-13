from __future__ import annotations
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "proxy"))
from audit_policy import AuditWriter, destination_denial, domain_allowed, normalise_host, safe_path


class PolicyTests(unittest.TestCase):
    def denied(self, host, port=443, scheme="https", connect=False, blocked=(), allowed=()):
        return destination_denial(host, port, scheme, connect=connect,
                                  blocked_networks=blocked, allowed_domains=allowed)

    def test_public_https(self):
        self.assertIsNone(self.denied("api.github.com"))
        self.assertIsNone(self.denied("1.1.1.1"))

    def test_public_http(self):
        self.assertIsNone(self.denied("example.com", 80, "http"))

    def test_lan_and_special_ranges(self):
        for host in ("172.16.1.1", "10.0.0.1", "192.168.2.3", "100.64.0.1", "169.254.169.254",
                     "127.0.0.1", "0.0.0.0", "192.0.2.1", "198.18.0.1", "224.0.0.1", "240.0.0.1"):
            with self.subTest(host=host):
                self.assertIsNotNone(self.denied(host))

    def test_ipv6(self):
        self.assertEqual(self.denied("[2606:4700:4700::1111]"), "ipv6_not_allowed")
        self.assertEqual(self.denied("::ffff:172.16.1.1"), "ipv6_not_allowed")

    def test_public_hairpin_deny(self):
        self.assertEqual(self.denied("8.8.8.8", blocked=["8.8.8.8/32"]), "non_public_destination")

    def test_connect_only_443(self):
        self.assertIsNone(self.denied("github.com", 443, "", True))
        for port in (22, 53, 80, 8080):
            self.assertEqual(self.denied("github.com", port, "", True), "connect_port_not_allowed")

    def test_scheme_and_port_pairs(self):
        for scheme, port in (("ftp", 21), ("https", 80), ("http", 443), ("https", 8443)):
            self.assertEqual(self.denied("github.com", port, scheme), "scheme_or_port_not_allowed")

    def test_local_hostnames(self):
        for host in ("localhost", "printer.local", "nas.lan", "service.internal", "router.home.arpa"):
            self.assertEqual(self.denied(host), "local_hostname")

    def test_malformed_authorities(self):
        for host in ("", "user:secret@example.com", "https://example.com", "evil\nexample.com", "-bad.example", "bad..example"):
            self.assertEqual(self.denied(host), "invalid_host")

    def test_legacy_numeric_addresses(self):
        for host in ("127.1", "0177.0.0.1", "2130706433", "0x7f000001"):
            self.assertEqual(self.denied(host), "invalid_host")

    def test_domain_allowlist_boundaries(self):
        self.assertTrue(domain_allowed("github.com", ["github.com"]))
        self.assertFalse(domain_allowed("evilgithub.com", ["*.github.com"]))
        self.assertFalse(domain_allowed("github.com.evil.example", ["*.github.com"]))
        self.assertFalse(domain_allowed("github.com", ["*.github.com"]))
        self.assertTrue(domain_allowed("api.github.com", ["*.github.com"]))
        self.assertTrue(domain_allowed("a.b.github.com", ["*.github.com"]))
        self.assertFalse(domain_allowed("8.8.8.8", ["github.com"]))

    def test_case_and_root_dot(self):
        self.assertEqual(normalise_host("API.GitHub.Com."), "api.github.com")

    def test_query_fragment_redaction(self):
        self.assertEqual(safe_path("/artifact?token=secret#password"), "/artifact")
        self.assertEqual(safe_path("/artifact#secret"), "/artifact")
        self.assertLessEqual(len(safe_path("/" * 5000)), 2048)


class WriterTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.path = Path(self.tmp.name) / "audit.jsonl"

    def tearDown(self):
        self.tmp.cleanup()

    def test_json_and_permissions(self):
        writer = AuditWriter(str(self.path), 4096, 2, True)
        writer.write({"event": "request", "host": "test\ninjected"})
        writer.close()
        lines = self.path.read_text().splitlines()
        self.assertEqual(len(lines), 1)
        obj = json.loads(lines[0])
        self.assertEqual(obj["schema_version"], 1)
        self.assertEqual(obj["host"], "test\ninjected")
        self.assertEqual(self.path.stat().st_mode & 0o777, 0o640)

    def test_rotation_retention(self):
        writer = AuditWriter(str(self.path), 100, 2, True)
        for n in range(8):
            writer.write({"event": "test", "n": n, "padding": "x" * 40})
        writer.close()
        self.assertEqual(len(list(self.path.parent.glob("audit.jsonl*"))), 3)
        self.assertEqual(json.loads(self.path.read_text())["n"], 7)
        self.assertEqual(json.loads(Path(f"{self.path}.1").read_text())["n"], 6)

    def test_disk_error_propagates(self):
        writer = AuditWriter(str(self.path), 4096, 2, False)
        with patch("audit_policy.os.write", side_effect=OSError("disk full")):
            with self.assertRaises(OSError):
                writer.write({"event": "request"})
        writer.close()

    def test_partial_write_propagates(self):
        writer = AuditWriter(str(self.path), 4096, 2, False)
        with patch("audit_policy.os.write", return_value=1):
            with self.assertRaises(OSError):
                writer.write({"event": "request"})
        writer.close()

    def test_fsync_error_propagates(self):
        writer = AuditWriter(str(self.path), 4096, 2, True)
        with patch("audit_policy.os.fsync", side_effect=OSError("I/O error")):
            with self.assertRaises(OSError):
                writer.write({"event": "request"})
        writer.close()

    def test_symlink_refused(self):
        target = Path(self.tmp.name) / "other"
        target.write_text("do not overwrite")
        self.path.symlink_to(target)
        with self.assertRaises(OSError):
            AuditWriter(str(self.path), 4096, 2)
        self.assertEqual(target.read_text(), "do not overwrite")

    def test_closed_writer_refused(self):
        writer = AuditWriter(str(self.path), 4096, 2)
        writer.close()
        with self.assertRaises(OSError):
            writer.write({"event": "request"})


if __name__ == "__main__":
    unittest.main()
