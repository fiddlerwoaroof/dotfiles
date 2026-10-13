# Isolated GitHub Actions runners (cloud-hypervisor microVMs) with a host MITM audit proxy

This bundle contains two reusable NixOS modules, an example host flake, a streaming
mitmproxy audit addon, tests, and deployment/maintenance instructions.

**Validation status:** 32 Python unit/contract tests passed in the packaging
environment. Nix, nftables and mitmproxy were not installed there; no Nix evaluation,
NixOS build, live TLS interception or network-isolation integration test has been
completed. Build and run the acceptance checks on a disposable host/runner before
putting real jobs or secrets on this setup. See `docs/VALIDATION.md`.

## Architecture

```text
runner VM ghr-1: 10.233.0.2 -- tap ci-ghr-1 ------+
                                             | trusted host nftables
runner VM ghr-4: 10.233.0.5 -- tap ci-ghr-4 ------+ guard and routing
                                             |
                                   ci-mitm-host: 10.234.0.1
                                             |
                       host systemd service, netns "ci-mitm"
                       mitmdump on 10.234.0.2:8080
                       private CA + logs live on the host
                                             |
                       public HTTP(S), controlled proxy DNS
                                             |
                           host uplink enp1s0, with next hop
                                    172.16.1.1
                                             |
                                           WAN
```

Runners can connect only to the proxy's TCP listener. They have no direct WAN,
LAN, host-service, other-runner, DNS, IPv6, or QUIC path. The proxy's actual outbound
IP packets are filtered separately: public IPv4 TCP 80/443 and TCP/UDP 53 to the
configured public resolvers, through the selected uplink and router. The proxy is
**a host systemd service with a network namespace, not another NixOS container**.

`172.16.1.1` is an allowed next hop, **not an allowed destination**. Router
administration and router DNS are not exceptions. Add router-public-address
hairpin destinations and public IP ranges used inside your network to
`extraBlockedNetworks`; see `docs/SECURITY.md`.

The private CA key never enters the runner or the Nix store. A bootstrap unit
creates a persistent CA on first deployment. a per-boot read-only secrets disk delivers only the
public CA to the guest; a guest oneshot service builds a runtime trust bundle
before the GitHub runner starts. The host's global trust store is not modified.

## Contents

- `modules/microvm-runners.nix`: one cloud-hypervisor microVM per runner (via
  microvm.nix), pinned host cores, fixed RAM with free page reporting, a sparse
  scratch disk recreated every boot, one-job-per-VM lifecycle, proxy environment,
  runtime CA installation, GitHub registration.
- `modules/mitm-proxy.nix`: host proxy service, namespace, nftables, NAT, CA,
  restricted resolvers, structured audit configuration.
- `flake.nix` and `examples/`: sample `nixosConfigurations.ci-host` plus reusable
  module exports. Hardware, interface, host address, GitHub URLs and secrets are
  placeholders; runner auto-start is deliberately off in the sample.
- `proxy/`: request policy and JSONL audit addon, with internal size-based rotation
  and fail-closed handling of audit write/hook failures.
- `docs/DEPLOYMENT.md`: installation, validation, rollout, maintenance, CA rotation,
  upgrade and rollback instructions.
- `docs/SECURITY.md`, `docs/AUDIT.md`, `docs/VALIDATION.md`, `docs/REFERENCES.md`:
  trust boundaries, audit fields/limitations, test status, and primary references.
- `scripts/`: read-only host inspection and disposable-runner smoke checks.
- `tests/`: Python unit tests and addon contract tests (HTTPFlow doubles, not a
  live mitmproxy instance).

## Starting point

Read **`docs/DEPLOYMENT.md` before switching a host**. After replacing placeholders:

```sh
nix flake lock
nix flake check --no-build
nix build .#checks.x86_64-linux.audit-unit-tests
sudo nixos-rebuild build --flake .#ci-host
```

The archive does not include a fabricated `flake.lock`. Generate, review, and
commit one; use that same lock for checks and deployment. The input follows
`nixos-unstable` until locked. The modules were written against the upstream
interfaces referenced in `docs/REFERENCES.md`, not runtime-certified against a
particular locked revision.

To consume the modules from an existing flake, import
`inputs.ci-runners.nixosModules.default`, or copy the directories and import
`./modules/microvm-runners.nix` (plus microvm.nix's host module). The runner
module imports the proxy module;
`nixosModules.mitm-proxy` is also exported separately.

## Scope

This is an **audited HTTP(S) egress policy**, not unrestricted Internet access and
not a complete content/DLP audit. It cannot make arbitrary HTTP payloads or nested
tunnels semantically visible. WebSocket payloads are not logged. Certificate-pinned
clients and tools that ignore proxy/trust settings can fail rather than bypassing
the proxy. Docker actions, host Docker sockets, the shared host Nix daemon and
privileged job environments are not provisioned.

The host kernel, KVM, cloud-hypervisor, the proxy and the router are trusted. Each
job runs in its own guest kernel; with `ephemeralRegistration` (the default) the
guest powers off after one job and the host boots a fresh VM with an empty scratch
disk. Guests boot from an erofs image of their own closure, not the host's
`/nix/store`. KSM is forced off by default because guest RAM is mergeable.

The docs under `docs/` were written for the earlier nspawn variant. The egress
policy, proxy, CA and audit material still applies; unit names and in-guest
commands differ (`microvm@NAME` instead of `container@NAME`, and there is no
`nixos-container run` equivalent: run `scripts/runner-smoke-test.sh` as a
workflow job on the runner instead).
