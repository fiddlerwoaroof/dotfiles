# Primary references

These upstream references were consulted while preparing this bundle on
September 23, 2026. Branch documentation is not a tested dependency lock; generate
and preserve a real `flake.lock`, and validate against the resulting versions.

- NixOS nspawn module: interface names, /32 links, namespace/credential flags,
  automatic host-store/daemon mounts, lifecycle.
  https://raw.githubusercontent.com/NixOS/nixpkgs/nixos-unstable/nixos/modules/virtualisation/nixos-containers.nix
- NixOS GitHub runner option and service implementations: token files, extra
  environment, service dependencies, ephemeral registration semantics.
  https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/continuous-integration/github-runner/options.nix
  https://raw.githubusercontent.com/NixOS/nixpkgs/nixos-unstable/nixos/modules/services/continuous-integration/github-runner/service.nix
- NixOS CA implementation: build-time certificateFiles versus the generated CA
  bundle and /etc compatibility paths.
  https://raw.githubusercontent.com/NixOS/nixpkgs/nixos-unstable/nixos/modules/security/ca.nix
- NixOS nftables/firewall/NAT implementations: independent tables, forward-allow
  extension point, NAT interfaces, build-time ruleset checking.
  https://raw.githubusercontent.com/NixOS/nixpkgs/nixos-unstable/nixos/modules/services/networking/nftables.nix
  https://raw.githubusercontent.com/NixOS/nixpkgs/nixos-unstable/nixos/modules/services/networking/firewall-nftables.nix
  https://raw.githubusercontent.com/NixOS/nixpkgs/nixos-unstable/nixos/modules/services/networking/nat-nftables.nix
- nftables manual: routing-next-hop expression, hook priorities, counters,
  connection-tracking expressions and verdict behavior.
  https://netfilter.org/projects/nftables/manpage.html
- systemd execution settings: NetworkNamespacePath, filesystem protection,
  capabilities, and service execution namespaces.
  https://raw.githubusercontent.com/systemd/systemd/main/man/systemd.exec.xml
- GitHub runner proxy settings: startup environment and tool/Docker caveats.
  https://docs.github.com/en/actions/how-tos/manage-runners/use-proxy-servers
- mitmproxy explicit proxy mode and transport tradeoffs.
  https://docs.mitmproxy.org/stable/concepts/modes/
- mitmproxy options: upstream verification, lazy connections, raw TCP, HTTP/3,
  streaming, onboarding, output verbosity.
  https://docs.mitmproxy.org/stable/concepts/options/
- mitmproxy CA and client-trust model, private/public PEM distinction, certificate
  pinning limitations.
  https://docs.mitmproxy.org/stable/concepts/certificates/
- mitmproxy event API and streaming examples: CONNECT, request/response/error,
  WebSocket lifecycle, body streaming callbacks.
  https://docs.mitmproxy.org/stable/api/events.html
  https://docs.mitmproxy.org/stable/addons/examples/
- mitmproxy CA loader and WebSocket handling implementations.
  https://raw.githubusercontent.com/mitmproxy/mitmproxy/main/mitmproxy/certs.py
  https://raw.githubusercontent.com/mitmproxy/mitmproxy/main/mitmproxy/proxy/layers/websocket.py
- RFC 4787, section 6: hairpinning terminology and behavior.
  https://www.rfc-editor.org/rfc/rfc4787.html#section-6
