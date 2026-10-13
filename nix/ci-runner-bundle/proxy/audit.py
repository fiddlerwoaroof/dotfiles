"""mitmdump addon: explicit-proxy policy, streamed bodies and redacted audit records.

The host nftables rules are the LAN/route boundary. This addon does not replace
resolved-IP filtering and does not purport to decode arbitrary nested tunnels.
"""
from __future__ import annotations

import json
import os
import sys
from functools import wraps
from pathlib import Path
from typing import Any

from mitmproxy import http

# Nix copies the entire adjacent proxy directory into the store.
sys.path.insert(0, str(Path(__file__).resolve().parent))
from audit_policy import AuditWriter, destination_denial, normalise_host, safe_path


def fail_closed(method):
    """Mitmproxy normally logs addon exceptions and continues: do not allow that."""
    @wraps(method)
    def guarded(self, *args, **kwargs):
        try:
            return method(self, *args, **kwargs)
        except Exception:
            self._fatal()
    return guarded


class CIAudit:
    def __init__(self) -> None:
        self.settings: dict[str, Any] = {}
        self.writer: AuditWriter | None = None

    @staticmethod
    def _fatal() -> None:
        # Never include an exception string: it can contain a URL or secret.
        try:
            os.write(2, b"CI audit unavailable; terminating proxy (fail closed).\n")
        finally:
            os._exit(74)

    def load(self, loader: Any) -> None:
        try:
            self.settings = json.loads(Path(os.environ["CI_AUDIT_CONFIG"]).read_text())
            self.writer = AuditWriter(
                self.settings["log_file"], self.settings["max_bytes"],
                self.settings["backups"], self.settings["fsync"],
            )
            self._write({"event": "proxy_start"})
        except Exception:
            self._fatal()

    def _write(self, event: dict[str, Any]) -> None:
        try:
            if self.writer is None:
                raise OSError("audit writer not initialized")
            self.writer.write(event)
        except Exception:
            self._fatal()

    def _client(self, flow: http.HTTPFlow) -> tuple[str, str | None]:
        peer = flow.client_conn.peername
        address = str(peer[0]) if peer else "unknown"
        return address, self.settings["clients"].get(address)

    def _record(self, flow: http.HTTPFlow, event: str, **extra: Any) -> None:
        address, runner = self._client(flow)
        try:
            host = normalise_host(flow.request.host)
        except ValueError:
            host = "<invalid>"
        peer = flow.server_conn.peername
        record: dict[str, Any] = {
            "event": event,
            "flow_id": flow.id,
            "runner": runner,
            "runner_ip": address,
            "method": flow.request.method[:32],
            "scheme": flow.request.scheme,
            "host": host,
            "port": flow.request.port,
            "destination_ip": str(peer[0]) if peer else None,
            **extra,
        }
        if self.settings["log_paths"] and flow.request.method != "CONNECT":
            record["path"] = safe_path(flow.request.path)
        self._write(record)

    def _denial(self, flow: http.HTTPFlow, *, connect: bool) -> str | None:
        if self._client(flow)[1] is None:
            return "unrecognized_client"
        return destination_denial(
            flow.request.host, flow.request.port, flow.request.scheme,
            connect=connect,
            blocked_networks=self.settings["blocked_networks"],
            allowed_domains=self.settings["allowed_domains"],
        )

    def _deny(self, flow: http.HTTPFlow, reason: str) -> None:
        self._record(flow, "denied", reason=reason)
        flow.metadata["ci_denied"] = True
        flow.response = http.Response.make(
            403, b"Destination or protocol denied by CI egress policy.\n",
            {"content-type": "text/plain; charset=utf-8", "cache-control": "no-store"},
        )

    @fail_closed
    def http_connect(self, flow: http.HTTPFlow) -> None:
        reason = self._denial(flow, connect=True)
        if reason:
            self._deny(flow, reason)
        else:
            # CONNECT itself does not receive normal request/response hooks.
            self._record(flow, "connect_requested")

    @fail_closed
    def http_connect_error(self, flow: http.HTTPFlow) -> None:
        self._record(flow, "connect_error", error_class="connect_failed")

    @fail_closed
    def requestheaders(self, flow: http.HTTPFlow) -> None:
        reason = self._denial(flow, connect=False)
        upgrade = flow.request.headers.get("upgrade", "").lower()
        if flow.request.method == "CONNECT":
            reason = "nested_or_extended_connect_not_allowed"
        elif upgrade and upgrade != "websocket":
            reason = "unsupported_upgrade"
        elif upgrade == "websocket" and not self.settings["allow_websockets"]:
            reason = "websockets_disabled"
        if reason:
            self._deny(flow, reason)
            return
        flow.metadata["ci_request_bytes"] = 0
        flow.metadata["ci_response_bytes"] = 0
        # Record intent before streaming anything to the upstream server.
        self._record(flow, "request")

        def count(chunk: bytes) -> bytes:
            flow.metadata["ci_request_bytes"] += len(chunk)
            return chunk

        flow.request.stream = count

    @fail_closed
    def responseheaders(self, flow: http.HTTPFlow) -> None:
        if flow.metadata.get("ci_denied"):
            return
        assert flow.response is not None

        def count(chunk: bytes) -> bytes:
            flow.metadata["ci_response_bytes"] = flow.metadata.get("ci_response_bytes", 0) + len(chunk)
            return chunk

        flow.response.stream = count

    @fail_closed
    def response(self, flow: http.HTTPFlow) -> None:
        if flow.metadata.get("ci_denied"):
            return
        assert flow.response is not None
        self._record(
            flow, "response", status=flow.response.status_code,
            request_body_bytes=flow.metadata.get("ci_request_bytes", 0),
            response_body_bytes=flow.metadata.get("ci_response_bytes", 0),
        )

    @fail_closed
    def error(self, flow: http.HTTPFlow) -> None:
        self._record(
            flow, "http_error", error_class="transport_or_upstream_error",
            request_body_bytes=flow.metadata.get("ci_request_bytes", 0),
            response_body_bytes=flow.metadata.get("ci_response_bytes", 0),
        )

    @fail_closed
    def websocket_start(self, flow: http.HTTPFlow) -> None:
        flow.metadata["ci_ws_client_bytes"] = 0
        flow.metadata["ci_ws_server_bytes"] = 0
        self._record(flow, "websocket_start")

    @fail_closed
    def websocket_message(self, flow: http.HTTPFlow) -> None:
        assert flow.websocket is not None
        message = flow.websocket.messages[-1]
        key = "ci_ws_client_bytes" if message.from_client else "ci_ws_server_bytes"
        flow.metadata[key] = flow.metadata.get(key, 0) + len(message.content)
        # The proxy layer keeps its own reference to the message being forwarded.
        # We do not retain a history of potentially sensitive message payloads.
        flow.websocket.messages.clear()

    @fail_closed
    def websocket_end(self, flow: http.HTTPFlow) -> None:
        self._record(
            flow, "websocket_end",
            client_message_bytes=flow.metadata.get("ci_ws_client_bytes", 0),
            server_message_bytes=flow.metadata.get("ci_ws_server_bytes", 0),
        )

    @fail_closed
    def done(self) -> None:
        if self.writer is not None:
            self._write({"event": "proxy_stop"})
            self.writer.close()


addons = [CIAudit()]
