# Audit records and retention

## Location and defaults

Structured records are written to `/var/log/ci-mitmproxy/audit.jsonl` on the host.
The directory is private to the proxy service/root. The writer uses mode 0640 for
its files, rejects a symlink as the active log file, writes one JSON object per
line, and calls `fsync` on each record by default.

It rotates internally at approximately 50 MiB, retaining `.1` through `.10` plus
the active file: approximately 550 MiB total, with minor per-record overhead. Old
records are deliberately discarded at the retention limit. Do not add external
logrotate/copytruncate rules against this path. Increase limits or forward logs to
a separately controlled collector for longer retention. Collector integration,
acknowledged remote durability and remote tamper resistance are not implemented.

Configuration:

```nix
services.ci-mitm-proxy.audit = {
  logPaths = false;
  maxFileBytes = 50 * 1024 * 1024;
  backups = 10;
  fsync = true;
};
```

`fsync = false` reduces I/O overhead but increases the chance of losing recently
written records after a crash. Even `true` is not a guarantee against every storage,
rotation, power-loss or hardware failure.

## Schema

Example (illustrative, not a captured request):

```json
{"schema_version":1,"timestamp":"2026-09-23T19:00:00.000+00:00","event":"response","flow_id":"example-flow-id","runner":"ghr-proj-a","runner_ip":"10.233.0.2","method":"GET","scheme":"https","host":"api.github.com","port":443,"destination_ip":"140.82.112.6","status":200,"request_body_bytes":0,"response_body_bytes":128}
```

Common fields:

| Field | Meaning |
|---|---|
| `schema_version` | Currently 1. |
| `timestamp` | Host wall-clock UTC when the audit record was emitted. |
| `event` | Event name described below. |
| `flow_id` | Mitmproxy flow identifier; HTTP request/completion records can be correlated. |
| `runner`, `runner_ip` | Configured client name and observed source address. Not a GitHub job/run ID. |
| `method`, `scheme`, `host`, `port` | Request target metadata. Host is `request.host`, not the displayed/override Host header. |
| `destination_ip` | Actual server peer IP when available; null before connection or on early rejection. |
| `status` | HTTP response code, only on response events. |
| `request_body_bytes`, `response_body_bytes` | Body chunk bytes observed by the proxy, not TCP/TLS wire-byte counts. Partial on failure. |
| `reason`, `error_class` | Fixed policy/error classifications, not raw exceptions or URLs. |
| `path` | Present only with `logPaths = true`; query/fragment removed, length bounded. |

Events include `proxy_start`, `proxy_stop`, `connect_requested`, `connect_error`,
`request`, `response`, `http_error`, `denied`, `websocket_start`, `websocket_end`.
Service lifecycle events do not carry HTTP fields. An unrecognized client can have
`runner: null` on a denial record. The firewall and source map are the identity
boundary, not a supplied proxy username/header.

CONNECT is a separate proxy instruction, not an upstream HTTP request. Its intent
is logged before tunnel establishment. It can fail before an HTTP request exists,
including during TLS negotiation. Do not treat `connect_requested` as proof of a
successful TLS connection or website access. CONNECT and inner HTTP flows are not
guaranteed to share a flow ID. TLS errors before an HTTP flow may have only a
CONNECT intent record plus service diagnostics.

`request` records are emitted before forwarding the request body. `response`
records are emitted after the HTTP response finishes. A missing completion record
means unknown/interrupted completion, not necessarily that no data was sent.
Streaming counters are counts of body chunks at the proxy layer, not packet-level
accounting or guaranteed whole-transaction totals during interruption.

## What is deliberately omitted

The addon never records request/response bodies, Cookie/Authorization headers,
query strings, raw mitmproxy flow captures, or raw transport-error strings. Paths
are off by default because they can contain tokens or private repository names.
Enabling paths does not guarantee that secrets embedded in path components are
redacted. Hostnames and methods are also application-controlled metadata and can
contain sensitive identifiers; no field-level policy can prevent a job from
intentionally embedding data in a hostname.

HTTP request/response bodies are streamed without retained flow bodies. WebSocket
messages are not persisted or retained as a history by this addon. Their opening
handshake metadata and cumulative per-direction payload byte counts on close are
logged. An abrupt termination may lack the final byte-count record. This is not
content inspection of WebSocket messages, application-layer encryption, nested
relays, or every site ultimately accessed by a remote service.

Stock flow printing is disabled (`flow_detail=0`) and ordinary terminal logging is
limited to errors. Service diagnostics and kernel logs are separate from the
redacted JSONL format; do not assume debugging output has the same redaction
contract, and do not enable verbose/full-flow logging around real secrets.

No GitHub job ID is attached automatically. Correlate runner and timestamps with
GitHub workflow records. A reused runner name identifies that runner configuration,
not cryptographic proof of which commit/job generated a request.

## Failure behavior

Audit open/write/rotation/fsync failures and unexpected addon hook exceptions
terminate the proxy with exit code 74. Python's normal logging handlers, which can
swallow write errors, are not used for structured records. The service restarts on
failure, subject to systemd's restart limits. It cannot serve requests while the
writer cannot initialize.

There is no alternate direct runner egress rule when the proxy dies. The runner
container units also bind their lifetime to the proxy/firewall units. After fixing
the fault, explicitly restart stopped runners as described in `DEPLOYMENT.md`.

This behavior is intended to prefer stopping jobs over silently continuing without
audit. It is not an exactly-once transaction protocol: a request can be sent before
recording its response, and a process/host/storage failure can truncate the most
recent record. Record parsing should tolerate and report a truncated final line
rather than silently discarding evidence of an incomplete log.
