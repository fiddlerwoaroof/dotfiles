{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkOption types;
  net = import ../lib/network.nix {inherit lib;};
  cfg = config.services.ci-mitm-proxy;
  ns = "ci-mitm";
  hostVeth = "ci-mitm-host";
  peerVeth = "ci-mitm-peer";
  stateDir = "/var/lib/ci-mitmproxy";
  publicDir = "/var/lib/ci-mitmproxy-public";
  clients = lib.attrValues cfg.clients;
  clientInterfaces = map (client: client.interface) clients;
  clientAddresses = map (client: client.address) clients;
  protectedInterfaces = [hostVeth] ++ clientInterfaces;
  hostAccess = cfg.clientHostAccess;
  pairs = net.elements (map (client: ''"${client.interface}" . ${client.address}'') clients);
  blockedNetworks = lib.unique (
    net.blockedNetworks
    ++ cfg.extraBlockedNetworks
    ++ map (address: "${address}/32") ([cfg.address cfg.hostAddress cfg.gateway] ++ clientAddresses)
  );
  policyFile = pkgs.writeText "ci-mitm-policy.json" (builtins.toJSON {
    clients = lib.mapAttrs' (name: client: lib.nameValuePair client.address name) cfg.clients;
    blocked_networks = blockedNetworks;
    allowed_domains = cfg.allowedDomains;
    log_paths = cfg.audit.logPaths;
    allow_websockets = cfg.allowWebSockets;
    log_file = "/var/log/ci-mitmproxy/audit.jsonl";
    max_bytes = cfg.audit.maxFileBytes;
    backups = cfg.audit.backups;
    fsync = cfg.audit.fsync;
  });
  resolverFile = pkgs.writeText "ci-mitm-resolv.conf" (
    lib.concatMapStringsSep "\n" (address: "nameserver ${address}") cfg.dnsServers
    + "\noptions timeout:2 attempts:2\n"
  );
  # Avoid host NSS modules (for example nss-resolve, which talks to host sockets).
  nssFile = pkgs.writeText "ci-mitm-nsswitch.conf" ''
    passwd: files
    group: files
    shadow: files
    hosts: files dns
    networks: files
  '';
  rules = ''
    set clients {
      type ifname
      ${lib.optionalString (clients != []) "elements = { ${net.quotedElements clientInterfaces} }"}
    }
    set client_sources {
      type ifname . ipv4_addr
      ${lib.optionalString (clients != []) "elements = { ${pairs} }"}
    }
    set protected_interfaces {
      type ifname
      elements = { ${net.quotedElements protectedInterfaces} }
    }
    set blocked_v4 {
      type ipv4_addr
      flags interval
      auto-merge
      elements = { ${net.elements blockedNetworks} }
    }
    set resolvers {
      type ipv4_addr
      elements = { ${net.elements cfg.dnsServers} }
    }
    chain denied {
      ${lib.optionalString cfg.logDenied ''
      limit rate 10/second burst 50 packets log prefix "ci-egress-denied: " level info
    ''}
      counter drop
    }
    chain validate_client {
      meta nfproto ipv4 iifname . ip saddr @client_sources return
      jump denied
    }
    chain validate_proxy {
      meta nfproto ipv4 ip saddr ${cfg.address} return
      jump denied
    }
    chain prerouting {
      type filter hook prerouting priority raw; policy accept;
      iifname @clients jump validate_client
      iifname "${hostVeth}" jump validate_proxy
    }
    chain input {
      type filter hook input priority filter - 10; policy accept;
      ${lib.optionalString (hostAccess != null) ''
      # The one host service clients may reach directly (e.g. a Nix cache).
      iifname @clients ip daddr ${hostAccess.address} tcp dport { ${lib.concatMapStringsSep ", " toString hostAccess.ports} } ct state { new, established } counter accept
    ''}
      # Otherwise neither the proxy namespace nor a runner can call host network services.
      iifname @protected_interfaces jump denied
    }
    chain from_client {
      ct state { invalid, untracked } jump denied
      ct status dnat jump denied
      # The ONLY network destination available to runners. No runner DNS path.
      oifname "${hostVeth}" ip daddr ${cfg.address} tcp dport ${toString cfg.port} ct direction original ct state { new, established } counter accept
      jump denied
    }
    chain from_proxy {
      ct state { invalid, untracked } jump denied
      ct status dnat jump denied
      # Responses, not new proxy-initiated connections, back to identified clients.
      oifname . ip daddr @client_sources tcp sport ${toString cfg.port} ct direction reply ct state established counter accept
      # DNS to the configured resolvers only, checked before the private-address
      # denial so a LAN resolver (e.g. the router) can be used. Port 53 only.
      oifname "${cfg.externalInterface}" ct direction original ct state { new, established } ip daddr @resolvers udp dport 53 counter accept
      oifname "${cfg.externalInterface}" ct direction original ct state { new, established } ip daddr @resolvers tcp dport 53 counter accept
      ip daddr @blocked_v4 jump denied
      # The LAN router is checked as the real NEXT HOP, never whitelisted as a target.
      oifname "${cfg.externalInterface}" rt ip nexthop ${cfg.gateway} ct direction original ct state { new, established } tcp dport { 80, 443 } counter accept
      jump denied
    }
    chain to_proxy {
      # Public upstream replies and related ICMP (including PMTU errors).
      meta nfproto ipv4 ip daddr ${cfg.address} iifname "${cfg.externalInterface}" ct direction reply ct state { established, related } counter accept
      jump denied
    }
    chain forward {
      type filter hook forward priority filter - 10; policy accept;
      iifname @clients jump from_client
      iifname "${hostVeth}" jump from_proxy
      oifname @clients jump denied
      oifname "${hostVeth}" jump to_proxy
    }
  '';
  caBootstrap = pkgs.writeShellScript "ci-mitm-ca-bootstrap" ''
    set -euo pipefail
    umask 077
    chmod 0700 ${stateDir}
    chmod 0755 ${publicDir}
    tmp=$(mktemp -d ${stateDir}/.bootstrap.XXXXXX)
    trap 'rm -rf "$tmp"' EXIT
    if [ ! -e ${stateDir}/mitmproxy-ca.pem ]; then
      ${pkgs.openssl}/bin/openssl req -x509 -newkey rsa:3072 -nodes -sha256 \
        -days 3650 -subj '/O=Isolated CI/CN=CI audit proxy CA' \
        -addext 'basicConstraints=critical,CA:TRUE' \
        -addext 'keyUsage=critical,keyCertSign,cRLSign' \
        -keyout "$tmp/key.pem" -out "$tmp/cert.pem"
      cat "$tmp/key.pem" "$tmp/cert.pem" > "$tmp/mitmproxy-ca.pem"
      mv "$tmp/mitmproxy-ca.pem" ${stateDir}/mitmproxy-ca.pem
    fi
    # Never silently rotate an existing, invalid, or nearly expired CA.
    ${pkgs.openssl}/bin/openssl pkey -in ${stateDir}/mitmproxy-ca.pem -check -noout
    ${pkgs.openssl}/bin/openssl x509 -in ${stateDir}/mitmproxy-ca.pem -checkend 604800 -noout
    ${pkgs.openssl}/bin/openssl pkey -in ${stateDir}/mitmproxy-ca.pem -pubout > "$tmp/key.pub"
    ${pkgs.openssl}/bin/openssl x509 -in ${stateDir}/mitmproxy-ca.pem -pubkey -noout > "$tmp/cert.pub"
    cmp "$tmp/key.pub" "$tmp/cert.pub"
    ${pkgs.openssl}/bin/openssl x509 -in ${stateDir}/mitmproxy-ca.pem -out "$tmp/ca.crt"
    install -m 0644 "$tmp/ca.crt" ${publicDir}/.ca.crt.new
    mv ${publicDir}/.ca.crt.new ${publicDir}/ca.crt
    install -m 0600 "$tmp/ca.crt" ${stateDir}/mitmproxy-ca-cert.pem
  '';
  netnsStart = pkgs.writeShellScript "ci-mitm-netns-start" ''
    set -euo pipefail
    # Names are reserved by this module. Clean up a failed previous invocation.
    ${pkgs.iproute2}/bin/ip link del ${hostVeth} 2>/dev/null || true
    ${pkgs.iproute2}/bin/ip netns del ${ns} 2>/dev/null || true
    ${pkgs.iproute2}/bin/ip netns add ${ns}
    ${pkgs.iproute2}/bin/ip link add ${hostVeth} type veth peer name ${peerVeth}
    ${pkgs.iproute2}/bin/ip link set ${peerVeth} netns ${ns}
    ${pkgs.iproute2}/bin/ip -n ${ns} link set ${peerVeth} name eth0
    ${pkgs.iproute2}/bin/ip addr add ${cfg.hostAddress}/32 dev ${hostVeth}
    ${pkgs.iproute2}/bin/ip link set ${hostVeth} up
    ${pkgs.iproute2}/bin/ip route add ${cfg.address}/32 dev ${hostVeth}
    ${pkgs.iproute2}/bin/ip -n ${ns} addr add ${cfg.address}/32 dev eth0
    ${pkgs.iproute2}/bin/ip -n ${ns} link set lo up
    ${pkgs.iproute2}/bin/ip -n ${ns} link set eth0 up
    ${pkgs.iproute2}/bin/ip -n ${ns} route add ${cfg.hostAddress}/32 dev eth0
    ${pkgs.iproute2}/bin/ip -n ${ns} route add default via ${cfg.hostAddress}
    ${pkgs.iproute2}/bin/ip netns exec ${ns} ${pkgs.procps}/bin/sysctl -qw \
      net.ipv6.conf.all.disable_ipv6=1 net.ipv6.conf.default.disable_ipv6=1 \
      net.ipv4.conf.all.accept_redirects=0 net.ipv4.conf.default.accept_redirects=0
  '';
  netnsStop = pkgs.writeShellScript "ci-mitm-netns-stop" ''
    ${pkgs.iproute2}/bin/ip link del ${hostVeth} 2>/dev/null || true
    ${pkgs.iproute2}/bin/ip netns del ${ns} 2>/dev/null || true
  '';
  waitForProxy = pkgs.writeShellScript "ci-mitm-wait" ''
    exec ${pkgs.python3}/bin/python - ${cfg.address} ${toString cfg.port} <<'PY'
    import socket, sys, time
    deadline = time.monotonic() + 40
    while time.monotonic() < deadline:
        try:
            with socket.create_connection((sys.argv[1], int(sys.argv[2])), timeout=1):
                break
        except OSError:
            time.sleep(0.2)
    else:
        raise SystemExit("ci-mitmproxy did not open its listening socket")
    PY
  '';
in {
  options.services.ci-mitm-proxy = {
    enable = mkEnableOption "a host-managed, isolated CI HTTPS audit proxy";
    externalInterface = mkOption {
      type = net.interface;
      description = "Host uplink whose actual next hop must be gateway.";
    };
    gateway = mkOption {
      type = net.ipv4;
      default = "172.16.1.1";
      description = "Required IPv4 upstream next hop. Host routing is configured separately.";
    };
    address = mkOption {
      type = net.ipv4;
      default = "10.234.0.2";
      description = "Proxy address in its dedicated network namespace.";
    };
    hostAddress = mkOption {
      type = net.ipv4;
      default = "10.234.0.1";
      description = "Host end of the proxy's point-to-point veth.";
    };
    port = mkOption {
      type = types.port;
      default = 8080;
      description = "Explicit HTTP proxy listener port.";
    };
    publicCAFile = mkOption {
      type = types.str;
      readOnly = true;
      default = "${publicDir}/ca.crt";
      description = "Runtime public CA path; the private key is never exported.";
    };
    clients = mkOption {
      default = {};
      description = "Client identities; populated automatically by the runner module.";
      type = types.attrsOf (types.submodule {
        options = {
          interface = mkOption {
            type = net.interface;
            description = "Dedicated host-side client veth.";
          };
          address = mkOption {
            type = net.ipv4;
            description = "Only accepted source IPv4 for this interface.";
          };
        };
      });
    };
    clientHostAccess = mkOption {
      default = null;
      description = ''
        One host address and its TCP ports that clients may connect to directly,
        bypassing the proxy (set by the runner module for a host Nix cache).
        Null keeps every host service unreachable from clients.
      '';
      type = types.nullOr (types.submodule {
        options = {
          address = mkOption {
            type = net.ipv4;
            description = "Host address on the client links.";
          };
          ports = mkOption {
            type = types.nonEmptyListOf types.port;
            description = "Allowed TCP ports.";
          };
        };
      });
    };
    dnsServers = mkOption {
      type = types.listOf net.ipv4;
      default = ["1.1.1.1" "9.9.9.9"];
      description = ''
        Resolvers reachable only from the proxy namespace, on TCP/UDP 53 via
        externalInterface. May be a LAN resolver such as the router; it is
        exempt from blockedNetworks for port 53 only.
      '';
    };
    extraBlockedNetworks = mkOption {
      type = types.listOf net.ipv4Network;
      default = [];
      description = "Additional denials, including public internal networks and router WAN/hairpin addresses.";
    };
    allowedDomains = mkOption {
      type = types.listOf (types.strMatching "(\\*\\.)?[a-zA-Z0-9][a-zA-Z0-9.-]*");
      default = [];
      example = ["github.com" "*.github.com"];
      description = "Optional exact/suffix allowlist. Empty permits public HTTP(S) generally. A wildcard does not include its apex.";
    };
    allowWebSockets = mkOption {
      type = types.bool;
      default = true;
      description = "Allow audited WebSocket handshakes; log message byte counts, never message content.";
    };
    logDenied = mkOption {
      type = types.bool;
      default = true;
      description = "Rate-limited kernel diagnostics for denied packets, separate from request audit logs.";
    };
    audit = {
      logPaths = mkOption {
        type = types.bool;
        default = false;
        description = "Log URL paths without query/fragment. Paths can themselves contain secrets; off by default.";
      };
      maxFileBytes = mkOption {
        type = types.ints.positive;
        default = 50 * 1024 * 1024;
        description = "Rotate the JSONL log at approximately this size.";
      };
      backups = mkOption {
        type = types.ints.positive;
        default = 10;
        description = "Number of rotated JSONL files retained in addition to the active file.";
      };
      fsync = mkOption {
        type = types.bool;
        default = true;
        description = "fsync every record before continuing; stronger local durability at an I/O cost.";
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = clients == [] || builtins.length clientAddresses == builtins.length (lib.unique clientAddresses);
        message = "ci-mitm-proxy: client addresses must be unique.";
      }
      {
        assertion = builtins.length clientInterfaces == builtins.length (lib.unique clientInterfaces);
        message = "ci-mitm-proxy: each client requires a dedicated interface.";
      }
      {
        assertion = !(builtins.elem hostVeth clientInterfaces) && !(builtins.elem cfg.externalInterface protectedInterfaces);
        message = "ci-mitm-proxy: uplink, proxy veth and client veths must differ.";
      }
      {
        assertion =
          cfg.address
          != cfg.hostAddress
          && cfg.address != cfg.gateway
          && cfg.hostAddress != cfg.gateway
          && lib.all (address: !(builtins.elem address [cfg.address cfg.hostAddress cfg.gateway])) clientAddresses;
        message = "ci-mitm-proxy: gateway, proxy and client addresses must not collide.";
      }
      {
        assertion = cfg.dnsServers != [];
        message = "ci-mitm-proxy: at least one public DNS resolver is required.";
      }
    ];

    networking.nftables = {
      enable = true;
      # Keep NixOS's build-time nft syntax check enabled.
      checkRuleset = true;
      tables.ci_egress = {
        family = "inet";
        content = rules;
      };
    };
    networking.firewall = {
      enable = true;
      filterForward = true;
      # The earlier ci_egress base chain has already irrevocably dropped forbidden
      # traffic. These rules make its permitted traffic survive nixos-fw as well.
      extraForwardRules = ''
        iifname { ${net.quotedElements protectedInterfaces} } accept
        oifname { ${net.quotedElements protectedInterfaces} } accept
      '';
    };
    # nixos-fw must admit the same traffic on the client links.
    networking.firewall.interfaces =
      lib.mkIf (hostAccess != null)
      (lib.genAttrs clientInterfaces (_: {allowedTCPPorts = hostAccess.ports;}));
    networking.dhcpcd.denyInterfaces = [hostVeth peerVeth];
    networking.networkmanager.unmanaged = ["interface-name:${hostVeth}" "interface-name:${peerVeth}"];
    networking.nat = {
      enable = true;
      internalInterfaces = [hostVeth];
      inherit (cfg) externalInterface;
    };
    # IPv4 forwarding: networking.nat sets net.ipv4.conf.all.forwarding. Setting
    # net.ipv4.ip_forward here too collides with hosts that already set it.
    users.groups.ci-mitmproxy = {};
    users.users.ci-mitmproxy = {
      isSystemUser = true;
      group = "ci-mitmproxy";
      home = stateDir;
      description = "Isolated CI audit proxy";
    };
    environment.systemPackages = with pkgs; [nftables iproute2 jq openssl];
    # nixos-26.05 has no services.journald.settings; use the older options.
    services.journald.settings = {
      Journal = {
        Storage = lib.mkDefault "persistent";
        SystemMaxUse = lib.mkDefault "10G";
      };
    };

    systemd.services.ci-mitm-ca = {
      description = "Provision CI audit CA; export only its public certificate";
      before = ["ci-mitmproxy.service"];
      path = with pkgs; [coreutils diffutils];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "ci-mitmproxy";
        Group = "ci-mitmproxy";
        StateDirectory = ["ci-mitmproxy" "ci-mitmproxy-public"];
        StateDirectoryMode = "0700";
        UMask = "0077";
        ExecStart = caBootstrap;
        PrivateNetwork = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        NoNewPrivileges = true;
      };
    };
    systemd.services.ci-mitm-netns = {
      description = "Dedicated network namespace for the host CI audit proxy";
      after = ["nftables.service" "network-online.target"];
      wants = ["network-online.target"];
      bindsTo = ["nftables.service"];
      before = ["ci-mitmproxy.service"];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = netnsStart;
        # Also clean up after partial startup failure.
        ExecStopPost = netnsStop;
      };
    };
    systemd.services.ci-mitmproxy = {
      description = "MITM audit proxy for isolated GitHub Actions runners";
      wantedBy = ["multi-user.target"];
      after = ["nftables.service" "ci-mitm-netns.service" "ci-mitm-ca.service"];
      requires = ["ci-mitm-ca.service"];
      bindsTo = ["nftables.service" "ci-mitm-netns.service"];
      environment.CI_AUDIT_CONFIG = toString policyFile;
      serviceConfig = {
        Type = "exec";
        User = "ci-mitmproxy";
        Group = "ci-mitmproxy";
        NetworkNamespacePath = "/run/netns/${ns}";
        StateDirectory = "ci-mitmproxy";
        StateDirectoryMode = "0700";
        LogsDirectory = "ci-mitmproxy";
        LogsDirectoryMode = "0700";
        WorkingDirectory = stateDir;
        UMask = "0077";
        ExecStart = lib.escapeShellArgs [
          "${pkgs.mitmproxy}/bin/mitmdump"
          "--mode"
          "regular"
          "--listen-host"
          cfg.address
          "--listen-port"
          (toString cfg.port)
          "--set"
          "confdir=${stateDir}"
          "--set"
          "connection_strategy=lazy"
          "--set"
          "ssl_insecure=false"
          "--set"
          "ssl_verify_upstream_trusted_ca=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
          "--set"
          "rawtcp=false"
          "--set"
          "block_global=false"
          "--set"
          "http3=false"
          "--set"
          "onboarding=false"
          "--set"
          "flow_detail=0"
          "--set"
          "termlog_verbosity=error"
          "--set"
          "store_streamed_bodies=false"
          "--scripts"
          "${../proxy}/audit.py"
        ];
        ExecStartPost = waitForProxy;
        Restart = "on-failure";
        RestartSec = "5s";
        TimeoutStartSec = "60s";
        TimeoutStopSec = "30s";
        NoNewPrivileges = true;
        CapabilityBoundingSet = "";
        AmbientCapabilities = "";
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        RestrictRealtime = true;
        LockPersonality = true;
        RestrictNamespaces = true;
        PrivateIPC = true;
        SystemCallArchitectures = "native";
        RestrictAddressFamilies = ["AF_INET" "AF_UNIX"];
        LimitCORE = 0;
        MemoryMax = "1G";
        TasksMax = 256;
        # Hide host IPC sockets, including /run/systemd/resolve. Bind only the
        # local resolver/NSS files into the service's private mount namespace.
        # A private empty /run also permits resolver symlinks whose bind-mount
        # targets live below /run; InaccessiblePaths=/run would prevent that.
        TemporaryFileSystem = ["/run"];
        InaccessiblePaths = ["-/nix/var/nix/daemon-socket"];
        BindReadOnlyPaths = ["${resolverFile}:/etc/resolv.conf" "${nssFile}:/etc/nsswitch.conf"];
      };
    };
  };
}
