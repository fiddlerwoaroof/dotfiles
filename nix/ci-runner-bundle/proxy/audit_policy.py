"""Pure policy and synchronous JSONL writer; intentionally no mitmproxy dependency."""
from __future__ import annotations

import ipaddress
import json
import os
import re
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterable

_HOST_LABEL = re.compile(r"[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?\Z")


def normalise_host(value: str) -> str:
    """Validate an authority host, never a Host header or an entire URL."""
    host = value.lower().rstrip(".")
    if host.startswith("[") and host.endswith("]"):
        host = host[1:-1]
    try:
        return str(ipaddress.ip_address(host))
    except ValueError:
        pass
    if not host or len(host) > 253 or not all(_HOST_LABEL.fullmatch(label) for label in host.split(".")):
        raise ValueError("invalid_host")
    # Avoid legacy inet_aton numeric shorthand such as 127.1 or 0177.0.0.1.
    if all(label.isdigit() for label in host.split(".")) or host.startswith("0x"):
        raise ValueError("ambiguous_numeric_host")
    return host


def domain_allowed(host: str, allowed: Iterable[str]) -> bool:
    entries = tuple(allowed)
    if not entries:
        return True
    for entry in entries:
        pattern = entry.lower().rstrip(".")
        if pattern.startswith("*."):
            suffix = pattern[1:]
            if host.endswith(suffix) and host != pattern[2:]:
                return True
        elif host == pattern:
            return True
    return False


def destination_denial(host: str, port: int, scheme: str, *, connect: bool,
                       blocked_networks: Iterable[str], allowed_domains: Iterable[str]) -> str | None:
    """Early application policy. Kernel filtering of actual resolved IPs is authoritative."""
    try:
        host = normalise_host(host)
    except ValueError:
        return "invalid_host"
    if connect:
        if port != 443:
            return "connect_port_not_allowed"
    elif (scheme, port) not in (("http", 80), ("https", 443)):
        return "scheme_or_port_not_allowed"
    if host == "localhost" or host.endswith((".localhost", ".local", ".lan", ".internal", ".home.arpa")):
        return "local_hostname"
    try:
        address = ipaddress.ip_address(host)
    except ValueError:
        address = None
    if address is not None:
        if address.version != 4:
            return "ipv6_not_allowed"
        if not address.is_global or address.is_multicast or any(address in ipaddress.ip_network(n, strict=False) for n in blocked_networks):
            return "non_public_destination"
    if not domain_allowed(host, allowed_domains):
        return "domain_not_allowed"
    return None


def safe_path(value: str) -> str:
    """Never include a query or fragment; still unsafe for secrets embedded in paths."""
    return value.split("?", 1)[0].split("#", 1)[0][:2048]


class AuditWriter:
    """Single-event-loop writer. Errors propagate so the proxy can terminate closed.

    No logging.Handler: Python's ordinary logging handlers may swallow disk errors.
    Rotation is internal; do not run logrotate/copytruncate against these files.
    """
    def __init__(self, path: str, max_bytes: int, backups: int, fsync: bool = True):
        if max_bytes < 1 or backups < 1:
            raise ValueError("max_bytes and backups must be positive")
        self.path = Path(path)
        self.max_bytes = max_bytes
        self.backups = backups
        self.fsync = fsync
        self.fd: int | None = None
        self._open()

    def _open(self) -> None:
        self.fd = os.open(self.path, os.O_WRONLY | os.O_CREAT | os.O_APPEND | os.O_CLOEXEC | os.O_NOFOLLOW, 0o640)
        os.fchmod(self.fd, 0o640)

    def _rotate(self) -> None:
        self.close()
        oldest = Path(f"{self.path}.{self.backups}")
        oldest.unlink(missing_ok=True)
        for index in range(self.backups - 1, 0, -1):
            src = Path(f"{self.path}.{index}")
            if src.exists():
                os.replace(src, Path(f"{self.path}.{index + 1}"))
        os.replace(self.path, Path(f"{self.path}.1"))
        self._open()
        if self.fsync:
            directory = os.open(self.path.parent, os.O_RDONLY | os.O_DIRECTORY | os.O_CLOEXEC)
            try:
                os.fsync(directory)
            finally:
                os.close(directory)

    def write(self, record: dict[str, Any]) -> None:
        event = {
            "schema_version": 1,
            "timestamp": datetime.now(timezone.utc).isoformat(timespec="milliseconds"),
            **record,
        }
        data = (json.dumps(event, ensure_ascii=True, separators=(",", ":"), allow_nan=False) + "\n").encode("ascii")
        if self.fd is None:
            raise OSError("audit writer is closed")
        size = os.fstat(self.fd).st_size
        if size and size + len(data) > self.max_bytes:
            self._rotate()
        assert self.fd is not None
        # A partial write is an audit failure, not silently retried as another line.
        if os.write(self.fd, data) != len(data):
            raise OSError("partial audit write")
        if self.fsync:
            os.fsync(self.fd)

    def close(self) -> None:
        if self.fd is not None:
            fd, self.fd = self.fd, None
            os.close(fd)
