# Security boundaries and limitations

## What the configuration is intended to enforce

The host `inet ci_egress` nftables table is the network authority. Runner
configuration/environment variables are conveniences, not the enforcement boundary.
Each managed client is tied to an exact host-side interface and IPv4 source address.
IPv6 and mismatched sources are dropped before host input/forward routing decisions.
There is no wildcard granting access to every nspawn container.

The forwarding policy permits identified runners to reach only the proxy address
and TCP port. It permits only connection-tracked proxy replies back to those
runners, not proxy-initiated connections. Neither runners nor proxy namespace can
reach host network services through the host INPUT path. Runner packets are not
NATed to the WAN. The proxy is NATed only when leaving through the configured uplink.

Proxy upstream traffic is restricted to public IPv4 TCP ports 80/443 and TCP/UDP
53 to the configured public resolvers. The route must have both the configured
outgoing interface and router next hop. The firewall applies to **resolved IP
packets**, not merely the hostname that appeared in a request; it is intended to
cover DNS changes/rebinding into private networks. Host DNAT of protected outbound
traffic is rejected. Only tracked upstream replies/related ICMP return to the proxy.

The separate early guard chain drops forbidden traffic before NixOS's ordinary
forward firewall can accept established or broadly permitted traffic. The NixOS
forward allow rules are only interoperability rules for traffic that already passed
the guard; do not remove or disable the guard table while leaving those rules active.
Do not bypass it with offload, another interface, an additional namespace uplink,
manual nft flushes, or host execution channels.

`172.16.1.1` is a routing next hop, not a service allowlist entry. Ethernet/ARP
resolution of a next-hop router is not permission to request its HTTP or DNS server.

## Conditions this cannot infer from your router

A packet to a public address can be translated into a LAN destination by a router
later in its path. Blocking RFC1918 on the host does not observe that later NAT.
You MUST identify and block hairpin-capable router public addresses and any public
address ranges used internally, or enforce this at the router on a separate CI
network. Dynamic WAN changes require maintaining this policy. Other router-side
redirects, transparent proxies and tunnels are also outside this host's visibility.

A compromised remote public service can relay traffic elsewhere. Blocking the local
LAN is not proof that an arbitrary public endpoint is harmless. The optional domain
allowlist narrows destinations but is not a complete exfiltration-prevention system.

## Trust and isolation

The host administrator/kernel, router, proxy executable, proxy addon, nixpkgs input,
and the deployed rules are trusted. nspawn provides namespaces and cgroups but
shares the host kernel. This bundle is not a VM-equivalent sandbox for hostile
multi-tenant code or a claim of resistance to kernel/container escapes.

The proxy is a non-root host service in a dedicated network namespace, without
capabilities. Its private mount namespace hides host `/run` sockets, the host Nix
daemon socket and personal home directories; resolver/NSS files are pinned inside
that service. The namespace's abstract Unix sockets are also separate. Host root
can inspect, modify or bypass all of this by design.

No host Docker socket, host execution agent, host networking, writable host mounts,
or shared host Nix daemon is provided to runners. NixOS normally shares its host
daemon socket with nspawn guests; the outer instance unit masks that directory,
and the guest has Nix disabled. Confirm that mount/credential behavior on your
actual kernel/systemd revision. Do not re-enable a host-side download/build service
as a convenience: its requests would not originate inside the protected guest path.

The long-lived PAT used for runner registration is inside the guest as a root
credential. Root in that guest is not a boundary protecting that token. Use narrow
credentials and trusted/private workflows appropriate to your threat model. GitHub
registration mode is not a substitute for a clean machine.

## Ephemeral means two different things here

`containers.NAME.ephemeral = true` discards the ephemeral guest filesystem when
the whole container is stopped/recreated. It does NOT create a new container for
every GitHub job.

The default `ephemeralRegistration = true` tells the NixOS GitHub runner service
to register for a single job and re-register after its service restart. That does
not prove that background processes or arbitrary guest state from a previous job
were removed. For strict one-job/one-machine isolation, add an external lifecycle
controller or disposable VM setup. This bundle does not include such a controller.

## TLS interception and auditing

The proxy is an intentional TLS interception point. It can read/modify plaintext
requests and responses in memory even though the addon does not persist payloads.
Its CA key is sensitive. Distribute only the public certificate to the managed CI
clients; the host and personal devices should not trust this CA globally.

The proxy verifies upstream certificates. Certificate-pinned, custom-trust-store,
mutual-TLS and non-proxy-aware clients may fail. There is no insecure fallback or
pass-through exception configured. Raw TCP fallback, QUIC/HTTP3 and non-web CONNECT
ports are disabled; arbitrary application data inside legitimate HTTPS/WebSocket
messages is still not semantically understood. Blocking a raw protocol does not
prove that no tunnel can be encoded in HTTP payloads.

Logs are outside guest filesystems and associated with client source/IP/interface,
not a job-supplied identity. They are NOT tamper-proof against host root or a
compromised proxy. The writer terminates the proxy on audit errors, but service or
host crashes can leave incomplete request/completion pairs. Bounded local retention
also intentionally removes old records. Forward records to separately controlled
storage for stronger retention and tamper resistance; no collector is included.

Direct denied-packet logs are rate-limited diagnostics, not exhaustive audit records.
The explicit proxy does not provide a direct runner DNS service, and the addon is
not a DNS-query capture tool. Legitimate public HTTP(S) access can carry encrypted
application data or DNS-over-HTTPS; this is not a guarantee to log every ultimate
site an application might reach through an application-layer relay.

## Deployment criteria

Build/evaluate the actual locked configuration, inspect generated units, then pass
the `DEPLOYMENT.md` acceptance tests with known listening test destinations and
firewall counters. Test negative and positive paths, failures, restart ordering,
source spoofing, private DNS answers, hairpin addressing, secret redaction and
certificate validation. The supplied test report is not a network security audit.
