"""Contract tests using a small HTTPFlow double, NOT mitmproxy integration tests."""
from __future__ import annotations
import importlib.util
import json
import sys
import tempfile
import types
import unittest
from pathlib import Path
from unittest.mock import patch

PROXY = Path(__file__).resolve().parents[1] / "proxy"
sys.path.insert(0, str(PROXY))

# Keep test doubles isolated: neither install nor import real mitmproxy here.
class FakeResponse:
    @staticmethod
    def make(status, body, headers):
        return types.SimpleNamespace(status_code=status, content=body, headers=headers, stream=False)

spec = importlib.util.spec_from_file_location("ci_audit_under_test", PROXY / "audit.py")
module = importlib.util.module_from_spec(spec)
with patch.dict(sys.modules, {"mitmproxy": types.SimpleNamespace(http=types.SimpleNamespace(Response=FakeResponse))}):
    spec.loader.exec_module(module)


def flow(host="github.com", method="GET", scheme="https", port=443, client="10.233.0.2", headers=None):
    return types.SimpleNamespace(
        id="test-flow", metadata={},
        client_conn=types.SimpleNamespace(peername=(client, 45678)),
        server_conn=types.SimpleNamespace(peername=("140.82.112.4", port)),
        request=types.SimpleNamespace(host=host, method=method, scheme=scheme, port=port,
                                      path="/repos/a/b?token=SECRET#fragment", headers=headers or {}, stream=False),
        response=None, websocket=None,
    )


class AddonTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.path = Path(self.tmp.name) / "audit.jsonl"
        self.addon = module.CIAudit()
        self.addon.settings = {
            "clients": {"10.233.0.2": "ghr-proj-a"}, "blocked_networks": [],
            "allowed_domains": [], "log_paths": False, "allow_websockets": True,
        }
        self.addon.writer = module.AuditWriter(str(self.path), 65536, 2, False)

    def tearDown(self):
        self.addon.writer.close()
        self.tmp.cleanup()

    def records(self):
        return [json.loads(line) for line in self.path.read_text().splitlines()]

    def test_request_response_streaming_and_redaction(self):
        f = flow(headers={"authorization": "Bearer SECRET", "cookie": "SECRET", "host": "misleading.example"})
        self.addon.requestheaders(f)
        self.assertEqual(f.request.stream(b"request-body"), b"request-body")
        f.response = FakeResponse.make(200, b"", {})
        self.addon.responseheaders(f)
        f.response.stream(b"response-body")
        self.addon.response(f)
        records = self.records()
        self.assertEqual([r["event"] for r in records], ["request", "response"])
        self.assertEqual(records[0]["host"], "github.com")
        self.assertEqual(records[0]["runner"], "ghr-proj-a")
        self.assertEqual(records[1]["request_body_bytes"], len(b"request-body"))
        self.assertEqual(records[1]["response_body_bytes"], len(b"response-body"))
        text = self.path.read_text()
        for forbidden in ("SECRET", "misleading.example", "request-body", "response-body", "/repos/a/b"):
            self.assertNotIn(forbidden, text)

    def test_optional_path_excludes_query(self):
        self.addon.settings["log_paths"] = True
        self.addon.requestheaders(flow())
        self.assertEqual(self.records()[0]["path"], "/repos/a/b")
        self.assertNotIn("SECRET", self.path.read_text())

    def test_unknown_client(self):
        f = flow(client="172.16.1.2")
        self.addon.requestheaders(f)
        self.assertEqual(f.response.status_code, 403)
        self.assertEqual(self.records()[0]["reason"], "unrecognized_client")

    def test_connect_intent_is_logged(self):
        self.addon.http_connect(flow(method="CONNECT", scheme=""))
        self.assertEqual(self.records()[0]["event"], "connect_requested")

    def test_connect_lan_is_denied(self):
        f = flow(host="172.16.1.1", method="CONNECT", scheme="")
        self.addon.http_connect(f)
        self.assertEqual(f.response.status_code, 403)

    def test_nested_connect_is_denied(self):
        f = flow(method="CONNECT")
        self.addon.requestheaders(f)
        self.assertEqual(self.records()[0]["reason"], "nested_or_extended_connect_not_allowed")

    def test_unknown_upgrade_is_denied(self):
        self.addon.requestheaders(flow(headers={"upgrade": "other-protocol"}))
        self.assertEqual(self.records()[0]["reason"], "unsupported_upgrade")

    def test_websocket_optout(self):
        self.addon.settings["allow_websockets"] = False
        self.addon.requestheaders(flow(headers={"upgrade": "websocket"}))
        self.assertEqual(self.records()[0]["reason"], "websockets_disabled")

    def test_websocket_content_not_retained(self):
        f = flow()
        self.addon.websocket_start(f)
        f.websocket = types.SimpleNamespace(messages=[types.SimpleNamespace(from_client=True, content=b"SECRET")])
        self.addon.websocket_message(f)
        self.assertEqual(f.websocket.messages, [])
        self.addon.websocket_end(f)
        self.assertEqual(self.records()[-1]["client_message_bytes"], 6)
        self.assertNotIn("SECRET", self.path.read_text())

    def test_error_details_not_logged(self):
        f = flow()
        f.error = types.SimpleNamespace(msg="https://github.com/?token=SECRET")
        self.addon.error(f)
        self.assertNotIn("SECRET", self.path.read_text())

    def test_unexpected_hook_error_terminates(self):
        with patch.object(self.addon, "_denial", side_effect=ValueError("unexpected")), \
             patch.object(self.addon, "_fatal", side_effect=SystemExit(74)):
            with self.assertRaises(SystemExit) as caught:
                self.addon.requestheaders(flow())
        self.assertEqual(caught.exception.code, 74)

    def test_writer_failure_terminates(self):
        with patch.object(self.addon.writer, "write", side_effect=OSError("disk full")), \
             patch.object(self.addon, "_fatal", side_effect=SystemExit(74)):
            with self.assertRaises(SystemExit) as caught:
                self.addon.requestheaders(flow())
        self.assertEqual(caught.exception.code, 74)


if __name__ == "__main__":
    unittest.main()
