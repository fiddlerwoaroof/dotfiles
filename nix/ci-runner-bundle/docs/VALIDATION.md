# Validation performed for this bundle

Prepared September 23, 2026.

## Completed in the packaging environment

- **32 Python unit/contract tests passed.** These cover public/private destination
  policy, IPv6 rejection, CONNECT ports, domain allowlist boundaries, local and
  malformed/numeric hostnames, query/fragment redaction, JSONL escaping, file modes,
  rotation/retention, symlink refusal, closed-writer/disk/partial-write/fsync errors,
  observed client identity, streamed body counters, WebSocket metadata/content
  handling, unknown upgrades, and fail-closed writer/hook behavior.
- Every Python source compiled successfully with the local Python interpreter.
- Both deployment helper scripts passed `bash -n` syntax checking.
- The **actual CA bootstrap shell body extracted from the Nix module** was run
  twice with the store executable path and state directories substituted for local
  test equivalents. RSA-3072 key/certificate matching, CA basic constraints,
  private/public file separation, private mode 0600, public mode 0644, and preservation
  of the same private key on the second run were verified. Temporary private keys
  were deleted and are not part of this archive.
- Source-file layout, LF line endings and final newlines were checked.
- Both archives were reopened and their file contents compared with the bundle;
  per-file and archive SHA-256 checksums were generated.

`python-test-output.txt` contains the unit-test output. To repeat locally:

```sh
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s tests -v
bash -n scripts/inspect-host.sh scripts/runner-smoke-test.sh
```

The addon contract tests use small HTTPFlow test doubles; they are not tests of
mitmproxy internals or real HTTP/TLS traffic. The bootstrap test does not exercise
systemd StateDirectory ownership, namespace setup, credential handoff, or mitmproxy's
CA loader. An isolated unit test is not proof of a complete deployment.

## NOT completed here

Nix, nftables and mitmproxy were not installed in this environment, network retrieval
of a Nix executable was unavailable, and creating a Linux network namespace was
not permitted. Therefore none of these has been validated:

- Nix parsing/evaluation, flake locking, NixOS module merging or the system build.
- The fully evaluated nftables ruleset parser/checker, kernel rule installation,
  routing/NAT, source validation, IPv6 blocking or router hairpin exclusions.
- systemd service startup, the proxy's filesystem/network sandbox, nspawn startup,
  private-user mapping, masking the shared daemon socket, and credential handoff.
- A live mitmproxy TLS exchange, upstream verification or GitHub runner registration.
- Actual GitHub actions, Docker workflows, custom Java/Node/application trust stores,
  certificate pinning, DNS rebinding or fault/restart behavior on a NixOS host.

Do not treat this bundle as already production-tested or as a security certification.
The lack of runtime validation is the reason the sample leaves runner auto-start
**disabled**. It is also why a dependency lock is not fabricated or implied.

## Required target-system validation

After editing the sample host and generating/reviewing `flake.lock`:

```sh
nix flake check --no-build
nix build .#checks.x86_64-linux.audit-unit-tests
sudo nixos-rebuild build --flake .#ci-host
```

The full build retains NixOS's nftables ruleset check. Then follow the disposable-host
and disposable-runner acceptance checklist in `DEPLOYMENT.md`, using positive
controls, firewall counters and real application tests. Repeat after dependency or
network-policy updates. Report build/runtime failures without weakening the egress
policy, disabling upstream TLS verification, or exposing host execution sockets.
