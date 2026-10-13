# Deployment and operations

## 1. Prerequisites and scope

Deploy on a trusted Linux NixOS host with root/console access, IPv4 connectivity
through router `172.16.1.1`, and working systemd network/user namespaces. The sample
is x86_64-linux. The configuration targets the upstream NixOS interfaces referenced
in `REFERENCES.md`; there is no supplied, tested dependency lock. Do not assume it
works on an older stable release without evaluating it there.

Read `SECURITY.md` and `VALIDATION.md`. This bundle changes host forwarding/firewall
configuration and provisions host services. It must be integrated with any existing
firewall, NAT, Docker/libvirt networking, systemd-networkd, NetworkManager, VPN,
policy routing, and flow-offload configuration. Do not flush nftables while runners
or the proxy are running. Keep a console or out-of-band recovery path.

Runners cannot resolve hostnames directly, use arbitrary ports, access LAN caches,
use SSH-based Git transports, or download through the host Nix daemon. Provision
job tools through `extraPackages` and use HTTPS package/Git endpoints that support
an explicit proxy. This is not a Docker-capable runner template.

## 2. Unpack and replace the placeholders

```sh
tar -xzf ci-runner-bundle.tar.gz
cd ci-runner-bundle
# Alternatively: unzip ci-runner-bundle.zip
```

Edit `examples/ci-host.nix`. Replace `enp1s0` in **both** the host network settings
and `services.ci-mitm-proxy.externalInterface`. Replace `172.16.1.50/24` with an
unused, appropriate host address and prefix on your router's actual network.
The router address alone does not establish your LAN's subnet mask.

Replace both GitHub URLs and token paths. Preserve the real host's existing
`system.stateVersion`; `26.05` is an example, not an instruction to upgrade that
setting. Replace `examples/hardware-configuration.nix` with the actual host hardware
configuration, and review the bootloader and `/boot` filesystem. The supplied
hardware file is only a build-oriented placeholder and may not boot your machine.

Choose unused addresses for both virtual networks. Default runner links use host
`10.233.0.1` and guests `.2`/`.3`; the proxy link uses `10.234.0.1`/`.2`. These are
independent /32 point-to-point routes, not a bridge exposing guests to the LAN.
Do not overlap an existing LAN, VPN, container, or tunnel network.

Populate `services.ci-mitm-proxy.extraBlockedNetworks` with your router's **actual
public WAN IPv4 address(es)** when those addresses can hairpin to internal services,
and all publicly numbered internal subnets. Example syntax only:

```nix
extraBlockedNetworks = [
  # "YOUR.ACTUAL.WAN.IP/32"
  # "YOUR.PUBLIC.INTERNAL.SUBNET/PREFIX"
];
```

Do not insert these literal placeholders. Where public addresses change, maintain
this configuration or enforce CI-to-WAN-only routing on a dedicated router VLAN.
The module cannot observe NAT translation performed later by your router. Do not
mark deployment accepted until this part of the network policy is resolved.

Leave `services.nspawn-github-runners.autoStart = false` for first deployment.
The proxy will start; the runner containers will not automatically register.

## 3. Create token files outside the repository and Nix store

Use a dedicated test repository/organization initially, with no sensitive jobs
queued. Obtain a PAT with the permissions required by the NixOS GitHub runner
module for your repository or organization. Consult the current upstream option
reference linked in `REFERENCES.md`; avoid broad shared credentials.

Example, with no token in shell history, command arguments, or the Nix expression:

```sh
sudo install -d -m 0700 /var/secrets
sudo bash -c '
  umask 077
  read -r -s -p "Project runner PAT: " token
  printf "\n" >&2
  printf %s "$token" > /var/secrets/gh-pat-project-a
  unset token
'
sudo bash -c '
  umask 077
  read -r -s -p "Organization runner PAT: " token
  printf "\n" >&2
  printf %s "$token" > /var/secrets/gh-pat-org
  unset token
'
```

An existing secret manager can supply these runtime paths instead. Order secret
provisioning before the corresponding `container@NAME.service`. Paths are strings,
not Nix path literals; the module restricts characters to avoid nspawn argument
parsing ambiguity. A PAT is copied into the guest as a system credential for
registration; guest root must therefore be treated as able to access it.

## 4. Lock, check, build, then deploy

No lock file is included because the configuration has not been evaluated against
a fetched/pinned nixpkgs revision in the packaging environment.

```sh
nix flake lock
# Review flake.lock, then commit it with all module/addon/example files.
# Git flakes exclude untracked files: git add the source files before building.
nix flake check --no-build
nix build .#checks.x86_64-linux.audit-unit-tests
sudo nixos-rebuild build --flake .#ci-host
```

The full host build has `networking.nftables.checkRuleset = true`; leave it enabled.
A successful evaluation is not a successful system build, and a successful build
is not a live isolation test. Inspect the generated unit definitions and rules.

Stop any old runner containers before switching, especially when replacing the
previous WAN-only module. Do not import that old monolithic flake alongside this
replacement: option locations and egress policy have changed.

```sh
sudo systemctl stop container@ghr-proj-a.service container@ghr-org.service
# Do this only after your edited configuration built successfully.
sudo nixos-rebuild switch --flake .#ci-host
sudo ./scripts/inspect-host.sh
```

A `systemctl stop` error because an old runner does not exist can be ignored after
confirming no old runner is running. Other stop/switch errors require inspection.
The inspection script assumes the default proxy addressing; adjust it when using
custom addresses.

The proxy startup sequence is:

1. The host loads the independent `inet ci_egress` guard table.
2. `ci-mitm-netns.service` creates the network namespace and /32 veth routes.
3. `ci-mitm-ca.service` generates a private persistent CA if absent, verifies it,
   and exports the public certificate.
4. `ci-mitmproxy.service` starts mitmdump, opens the audit log, and checks that its
   TCP listener is available inside the namespace.
5. A started runner receives the public CA and PAT as separate nspawn credentials.
   `ci-proxy-trust.service` creates the guest runtime trust bundle before its GitHub
   runner registration/service starts.

The CA and namespace services are dependencies; they do not need to be enabled
manually. There is no additional DNS daemon, proxy web UI, or port opened on the
host LAN address.

## 5. Validate a disposable runner

Use a dedicated test repository with no real jobs pending. Starting the container
starts the GitHub runner service, which can accept a job once registered.

```sh
sudo systemctl start container@ghr-proj-a.service
sudo nixos-container run ghr-proj-a -- systemctl status ci-proxy-trust github-runner-ghr-proj-a
sudo nixos-container run ghr-proj-a -- bash -s < scripts/runner-smoke-test.sh
sudo nft list table inet ci_egress
sudo journalctl -k -g 'ci-egress-denied:' --since '10 minutes ago'
sudo tail -n 20 /var/log/ci-mitmproxy/audit.jsonl | jq .
```

For custom proxy addresses, invoke the smoke script through `env`, for example:

```sh
sudo nixos-container run ghr-proj-a -- env CI_PROXY_URL=http://10.234.0.2:8080 bash -s \
  < scripts/runner-smoke-test.sh
```

Acceptance must cover **all** of the following, not just HTTP timeouts:

| Check | Expected result / evidence |
|---|---|
| GitHub HTTPS through the proxy | Success with normal certificate verification; correlated `connect_requested`, `request`, `response` records naming the runner. |
| Direct public TCP 80/443 and public DNS UDP/TCP 53 | Blocked even after clearing proxy variables; guard counters increase. Use IP literals so lack of DNS is not mistaken for filtering. |
| Direct host, other runner, listening LAN endpoint, and router administration | Blocked with guard evidence. A service that was never listening is not a meaningful positive control. |
| Those destinations requested through the proxy | Literal destinations produce a policy 403; a controlled domain resolving to a private IP is blocked by the host guard. |
| Public DNS record rebound to a private address | The subsequent connection is blocked at the actual destination IP; verify counters, not only hostname policy. |
| Cloud metadata `169.254.169.254` | Blocked directly and through the proxy. |
| IPv6, source spoofing, UDP/443, unsolicited LAN-to-proxy/runner connections | Blocked; test privileged spoofing only in a disposable environment. |
| Uplink or next hop differs from the configured route | Proxy upstream traffic is blocked. Exercise route changes only on a disposable host/network. |
| Router WAN address with hairpin NAT and public internal subnets | Blocked by your reviewed site-specific policy, not only the private-IP baseline. |
| Host Nix daemon socket in guest | No usable `/nix/var/nix/daemon-socket/socket`; no other host execution/socket escape hatch is mounted. |
| Guest trust and proxy private key | HTTPS works; only the public CA is in the guest; `/var/lib/ci-mitmproxy` and audit logs are not guest mounts. |
| Proxy stopped / audit disk write failure | No direct runner fallback. Runners bound to the proxy stop; proxy cannot process requests without its audit writer. |
| Redaction | Test fake query/header/body secrets do not appear in structured audit records. Do not test with real credentials. |
| Guest runtime NSS/tool trust | Curl, checkout, required Node/Python tools and job downloads work with the supplied trust/proxy setup. |

Also inspect `systemctl cat container@ghr-proj-a.service` to confirm the dependency
and daemon-socket mask overrides are present on the actual instance unit. Confirm
namespace and private credential mount behavior on your kernel/systemd versions.

After successful acceptance, change `autoStart = true`, build/switch again, and
start any containers not already running. Test an actual disposable workflow on
both configured runner sizes before moving production jobs.

## 6. Audit operations

```sh
sudo tail -F /var/log/ci-mitmproxy/audit.jsonl | jq --unbuffered .
sudo jq -r 'select(.event == "request") | [.timestamp,.runner,.method,.host,.port] | @tsv' \
  /var/log/ci-mitmproxy/audit.jsonl
sudo journalctl -u ci-mitmproxy -u ci-mitm-ca -u ci-mitm-netns
sudo journalctl -k -f -g 'ci-egress-denied:'
```

Log rotation is internal: 50 MiB per active file, ten backups by default. Do not
apply `copytruncate` or another logrotate policy to the same files. See `AUDIT.md`
for schema, retention, omissions, failures, and forwarding considerations.

The proxy exposes HTTP(S) on upstream ports 80/443 only. `CONNECT` is restricted to
443, raw TCP fallback and HTTP/3 are disabled. WebSockets are allowed by default
for compatibility; handshake metadata and final message-byte totals are logged,
not message contents. Disable with `allowWebSockets = false` only after testing
required job/runner functionality.

## 7. Restart, update, and recover

Pause/drain jobs first. Stopping the proxy, namespace, or nftables service stops
dependent runner containers. A restarted dependency does not necessarily restart
containers stopped by `BindsTo`; explicitly start them after recovery.

```sh
sudo systemctl stop container@ghr-proj-a container@ghr-org
sudo systemctl stop ci-mitmproxy
# Apply an already-built configuration, or restart the namespace when necessary.
sudo systemctl restart ci-mitm-netns
sudo systemctl start ci-mitmproxy
sudo systemctl start container@ghr-proj-a container@ghr-org
```

Keep nftables running during normal maintenance. If it failed, fix and start it
first, then start namespace, proxy, and runners in that order. Do not remove rules
as a workaround. After changing addresses, routes, firewall policy, or namespace
configuration, drain and stop runners first and repeat the acceptance tests.

For a nixpkgs upgrade, update/review the lock, run unit tests and full builds, then
repeat disposable-runner tests. Proxy options, runner module behavior and unit
sandboxing can change between revisions.

After an audit/storage failure, correct permissions/free space, inspect diagnostic
logs, then recover explicitly:

```sh
sudo systemctl reset-failed ci-mitmproxy
sudo systemctl start ci-mitmproxy
sudo systemctl start container@ghr-proj-a container@ghr-org
```

A completed HTTP transaction may have a request record but no response record if
the proxy crashes or fails while recording completion. Never classify all such
requests as successfully completed or as never sent.

## 8. CA lifecycle

Private CA state is `/var/lib/ci-mitmproxy/mitmproxy-ca.pem` (contains the private
key). Public CA is `/var/lib/ci-mitmproxy-public/ca.crt`. Back up private state only
to protected storage; the archive you received contains **no key or certificate**.
The bootstrap refuses a corrupt/mismatched CA and a CA expiring within seven days;
it does not silently replace one and break guest trust.

To deliberately rotate, drain jobs and stop all managed runner containers, then:

```sh
sudo systemctl stop ci-mitmproxy ci-mitm-ca
# Choose a unique, protected backup name; this directory contains a private key.
sudo mv /var/lib/ci-mitmproxy /var/lib/ci-mitmproxy.retired-$(date -u +%Y%m%dT%H%M%SZ)
sudo systemctl start ci-mitm-ca
sudo systemctl start ci-mitmproxy
sudo systemctl start container@ghr-proj-a container@ghr-org
```

Restart the **containers**, not merely the inner GitHub runner process, so nspawn
loads the new public credential. Verify the public certificate fingerprint and
verified GitHub HTTPS after rotation. Protect/delete retired keys according to
your retention policy. Do not install the MITM CA into the host's global trust store
or your personal browser just to make this setup work.

Java and other applications with private trust stores may need their own CA import
and proxy configuration. Use a job-specific trust store containing the public CA,
not `ssl_insecure`, `NODE_TLS_REJECT_UNAUTHORIZED=0`, `GIT_SSL_NO_VERIFY`, or `curl -k`.
Certificate-pinned or mutual-TLS applications may not be compatible with MITM.

## 9. Rollback and removal

Drain and stop runners **before** rolling back or disabling these modules. A prior
host generation might have weaker egress rules. Never continue jobs under an older
WAN-only configuration by accident.

```sh
sudo systemctl stop container@ghr-proj-a container@ghr-org
sudo systemctl stop ci-mitmproxy ci-mitm-netns
sudo nixos-rebuild switch --rollback
```

Review what that generation will start automatically; disable old runners as
needed before rollback. For complete removal, remove the module imports/settings,
build/switch the resulting configuration, unregister retired GitHub runners, revoke
unused PATs, and retain or securely delete proxy keys/logs according to policy.
Do not blindly delete unrelated nftables tables or flush the host ruleset.

## 10. Integrating into an existing flake

Preserve the directory layout because the modules refer to `../lib` and `../proxy`.
For a local copied directory:

```nix
# In your real host's module list:
modules = [
  ./hardware-configuration.nix
  ./configuration.nix
  ./ci-runner-bundle/modules/github-runners.nix
  # Put the services.* blocks from examples/ci-host.nix into a reviewed local module.
];
```

As a flake input, use `nixosModules.default` (runners plus proxy),
`nixosModules.github-runners`, or `nixosModules.mitm-proxy` (proxy alone). The proxy
alone accepts explicit `clients.NAME = { interface = "ve-NAME"; address = "..."; };`
entries and applies the same proxy-only network policy to those interfaces. You are
responsible for provisioning those clients, source routes, public CA trust and
proxy variables. Do not register a physical LAN interface as a client interface.
