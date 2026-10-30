{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkOption types;
  net = import ../lib/network.nix {inherit lib;};
  cfg = config.services.microvm-github-runners;
  proxy = config.services.ci-mitm-proxy;
  hostStateVersion = config.system.stateVersion;
  names = lib.attrNames cfg.runners;
  addresses = map (runner: runner.localAddress) (lib.attrValues cfg.runners);
  stateDir = config.microvm.stateDir;
  proxyURL = "http://${proxy.address}:${toString proxy.port}";
  trustDir = "/run/ci-trust";
  # Delivered per boot as a small read-only ext4 disk (see mkHostUnit).
  secretsMount = "/ci-secrets";
  workMount = "/var/lib/ci-work";
  hostCache = cfg.nix.enable && cfg.nix.hostCache.enable;
  # Nix orders substituters by the Priority a cache advertises, not list
  # order. Harmonia advertises 50 and cache.nixos.org 40, so without this the
  # guests fetched from the internet (through the proxy) ahead of the host.
  hostCacheURL = "http://${cfg.hostAddress}:${toString cfg.nix.hostCache.port}?priority=10";
  # The host cache is reached directly over the tap, never through the proxy.
  noProxy = lib.concatStringsSep "," (["localhost" "127.0.0.1"] ++ lib.optional hostCache cfg.hostAddress);
  nixStoreMount = "/nix/.rw-store";
  proxyEnvironment = {
    http_proxy = proxyURL;
    https_proxy = proxyURL;
    HTTP_PROXY = proxyURL;
    HTTPS_PROXY = proxyURL;
    no_proxy = noProxy;
    NO_PROXY = noProxy;
    SSL_CERT_FILE = "${trustDir}/ca-bundle.crt";
    NIX_SSL_CERT_FILE = "${trustDir}/ca-bundle.crt";
    CURL_CA_BUNDLE = "${trustDir}/ca-bundle.crt";
    REQUESTS_CA_BUNDLE = "${trustDir}/ca-bundle.crt";
    GIT_SSL_CAINFO = "${trustDir}/ca-bundle.crt";
    NODE_EXTRA_CA_CERTS = "${trustDir}/proxy-ca.crt";
  };
  tapName = name: "ci-${name}";
  # Deterministic, locally administered unicast MAC per runner name.
  macFor = name: let
    h = builtins.hashString "sha256" name;
  in
    lib.concatStringsSep ":" (["02"] ++ map (i: builtins.substring (2 * i) 2 h) (lib.range 0 4));

  runnerOptions = {
    options = {
      url = mkOption {
        type = types.str;
        description = "GitHub repository or organization registration URL.";
      };
      tokenFile = mkOption {
        type = net.runtimePath;
        description = "Host runtime path to a PAT. Never use a Nix path literal for a secret.";
      };
      localAddress = mkOption {
        type = net.ipv4;
        description = "Unique runner IPv4 address, without a prefix.";
      };
      labels = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Additional GitHub runner labels.";
      };
      extraPackages = mkOption {
        type = types.listOf types.package;
        default = [];
        description = "Tools provisioned by the trusted host build, available to jobs.";
      };
      cpus = mkOption {
        type = types.str;
        example = "8,24";
        description = ''
          Host CPUs (cpuset syntax) the VMM process, and therefore every vCPU
          thread, is confined to. Give each runner whole physical cores (both
          SMT siblings) that nothing else uses; check with lscpu -e.
        '';
      };
      vcpu = mkOption {
        type = types.ints.positive;
        default = 2;
        description = "Guest vCPUs (2 = both threads of one core).";
      };
      memoryMiB = mkOption {
        type = types.ints.positive;
        default = 16384;
        description = "Guest RAM. Free page reporting returns unused pages to the host.";
      };
      diskGiB = mkOption {
        type = types.ints.positive;
        default = 100;
        description = "Sparse scratch disk, recreated empty on every VM start.";
      };
      ephemeralRegistration = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Register for exactly one job. When the runner exits the guest powers
          off and the host starts a fresh VM, so every job gets a clean disk.
        '';
      };
    };
  };

  mkHostUnit = name: runner:
    lib.nameValuePair "microvm@${name}" {
      # Template instance: add to microvm.nix's microvm@.service, don't replace it.
      overrideStrategy = "asDropin";
      after = ["nftables.service" "ci-mitmproxy.service" "ci-mitm-ca.service"];
      # Losing the guard table must stop the VM. Losing the proxy need not:
      # runner egress already fails closed at nftables while it restarts.
      bindsTo = ["nftables.service"];
      wants = ["ci-mitmproxy.service"];
      requires = ["ci-mitm-ca.service"];
      unitConfig.StartLimitIntervalSec = 0;
      serviceConfig = {
        AllowedCPUs = runner.cpus;
        RestartSec = "1s";
        RuntimeDirectory = "ci-runner-${name}";
        RuntimeDirectoryMode = "0700";
        ExecStartPre = [
          ("+"
            + pkgs.writeShellScript "ci-runner-prepare-${name}" ''
              set -euo pipefail
              PATH=${lib.makeBinPath [pkgs.coreutils pkgs.e2fsprogs pkgs.iproute2]}
              rundir=/run/ci-runner-${name}

              # Point-to-point addressing on the tap (idempotent across restarts).
              if [ -e /proc/sys/net/ipv6/conf/${tapName name}/disable_ipv6 ]; then
                echo 1 > /proc/sys/net/ipv6/conf/${tapName name}/disable_ipv6
              fi
              ip addr replace ${cfg.hostAddress}/32 dev ${tapName name}
              ip route replace ${runner.localAddress}/32 dev ${tapName name}

              # Fresh scratch disks every boot; microvm-run recreates them sparse.
              rm -f ${stateDir}/${name}/work.img ${stateDir}/${name}/nix-store.img

              # Secrets disk: PAT + public proxy CA, root-owned inside the guest.
              src=$(mktemp -d)
              trap 'rm -rf "$src"' EXIT
              install -m 0400 ${lib.escapeShellArg runner.tokenFile} "$src/gh-token"
              install -m 0444 ${proxy.publicCAFile} "$src/ca.crt"
              rm -f "$rundir/secrets.img"
              mke2fs -q -t ext4 -O ^has_journal -N 16 -d "$src" "$rundir/secrets.img" 1M
              chown microvm "$rundir/secrets.img"
              chmod 0400 "$rundir/secrets.img"
            '')
        ];
      };
    };

  mkVM = name: runner: {
    autostart = cfg.autoStart;
    config = {
      config,
      pkgs,
      ...
    }: let
      guestConfig = config;
      guestPkgs = pkgs;
    in {
      system.stateVersion = hostStateVersion;

      microvm = {
        hypervisor = "cloud-hypervisor";

        # Store disk lives on host ZFS: STATX_DIOALIGN there is the recordsize,
        # which cloud-hypervisor advertises as blk_size (>64K, guest rejects it).
        storeDiskDirect = false;

        inherit (runner) vcpu;
        mem = runner.memoryMiB;
        # virtio-balloon with free page reporting: idle guest RAM goes back
        # to the host (and ZFS ARC) instead of staying allocated.
        balloon = true;
        # Lets cloud-hypervisor report readiness to systemd (Type=notify).
        vsock.cid = 3 + lib.lists.findFirstIndex (n: n == name) 0 names;
        interfaces = [
          {
            type = "tap";
            id = tapName name;
            mac = macFor name;
          }
        ];
        # No virtiofs: the guest's own closure boots from an erofs store disk,
        # so the host /nix/store is not exposed and no virtiofsd runs.
        shares = [];
        volumes =
          [
            {
              image = "/run/ci-runner-${name}/secrets.img";
              mountPoint = secretsMount;
              autoCreate = false;
              readOnly = true;
            }
            {
              image = "${stateDir}/${name}/work.img";
              mountPoint = workMount;
              size = runner.diskGiB * 1024;
              fsType = "ext4";
              # Scratch that is discarded every boot needs no journal; skipping
              # it avoids writing ~512 MiB of journal per start.
              mkfsExtraArgs = ["-O" "^has_journal" "-E" "lazy_itable_init=1"];
            }
          ]
          ++ lib.optional cfg.nix.enable {
            # Writable upper layer over the read-only erofs store; discarded
            # every boot like the work disk.
            image = "${stateDir}/${name}/nix-store.img";
            mountPoint = nixStoreMount;
            size = cfg.nix.storeDiskGiB * 1024;
            fsType = "ext4";
            mkfsExtraArgs = ["-O" "^has_journal" "-E" "lazy_itable_init=1"];
          };
        writableStoreOverlay = lib.mkIf cfg.nix.enable nixStoreMount;
      };

      nix.enable = cfg.nix.enable;
      nix.settings = lib.mkIf cfg.nix.enable {
        experimental-features = ["nix-command" "flakes"];
        substituters = lib.mkForce (lib.optional hostCache hostCacheURL ++ ["https://cache.nixos.org"]);
        trusted-public-keys =
          lib.mkForce (["cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="]
            ++ lib.optional hostCache cfg.nix.hostCache.publicKey);
        # An unreachable cache should cost seconds, not the default minutes.
        connect-timeout = 5;
      };
      # The daemon does the downloads, so it needs the proxy and runtime trust.
      systemd.services.nix-daemon = lib.mkIf cfg.nix.enable {
        # mkForce: nixpkgs points the daemon's CA vars at the store bundle,
        # which lacks the proxy CA.
        environment = lib.mapAttrs (_: lib.mkForce) proxyEnvironment;
        after = ["ci-proxy-trust.service"];
        requires = ["ci-proxy-trust.service"];
      };
      fileSystems.${secretsMount}.options = ["ro" "nodev" "nosuid" "noexec"];

      # Opt-in host-only debug shell over vsock (`sudo microvm -s NAME`).
      # vsock is a host<->guest channel; it gives the guest no network path.
      microvm.vsock.ssh.enable = cfg.debugSshKeys != [];
      services.openssh = lib.mkIf (cfg.debugSshKeys != []) {
        openFirewall = false;
        settings.PasswordAuthentication = false;
        settings.PermitRootLogin = "prohibit-password";
      };
      users.users.root.openssh.authorizedKeys.keys = cfg.debugSshKeys;

      networking = {
        useDHCP = false;
        enableIPv6 = false;
        useHostResolvConf = false;
        # No runner DNS is needed for an explicit HTTP proxy. This local,
        # non-listening resolver fails fast for applications that ignore it.
        nameservers = lib.mkForce ["127.0.0.1"];
      };
      systemd.network.networks."10-ci" = {
        matchConfig.MACAddress = macFor name;
        address = ["${runner.localAddress}/32"];
        routes = [
          {Destination = "${cfg.hostAddress}/32";}
          {
            Gateway = cfg.hostAddress;
            GatewayOnLink = true;
          }
        ];
      };
      services.resolved.enable = false;
      # The guest journal is lost at every poweroff. Its serial console is
      # captured in the host's journal as microvm@NAME, so forward it there;
      # the runner's own logs then survive the VM. Job step output is not
      # journald's (the runner keeps it in _diag), so this is service logs only.
      services.journald.settings = {
        Journal = {
          ForwardToConsole = true;
          TTYPath = "/dev/ttyS0";
        };
      };

      environment.variables = lib.mapAttrs (_: value: lib.mkForce value) proxyEnvironment;
      environment.systemPackages = with guestPkgs; [curl git openssl python3 iproute2] ++ runner.extraPackages;
      # These are runtime symlinks. Do NOT put the CA into
      # security.pki.certificateFiles (which is a build-time input).
      environment.etc = {
        "ssl/certs/ca-certificates.crt".source = lib.mkForce "${trustDir}/ca-bundle.crt";
        "ssl/certs/ca-bundle.crt".source = lib.mkForce "${trustDir}/ca-bundle.crt";
        "pki/tls/certs/ca-bundle.crt".source = lib.mkForce "${trustDir}/ca-bundle.crt";
      };
      # Static runner user: the upstream DynamicUser cannot own a pre-created
      # workDir on the scratch disk.
      users.users.ci-runner = {
        isSystemUser = true;
        group = "ci-runner";
      };
      users.groups.ci-runner = {};
      systemd.tmpfiles.rules = [
        "d ${workMount}/work 0750 ci-runner ci-runner -"
        "d ${workMount}/tmp 1777 root root -"
      ];

      systemd.services.ci-proxy-trust = {
        description = "Install the host proxy's public CA into a runtime trust bundle";
        wantedBy = ["multi-user.target"];
        before = ["github-runner-${name}.service"];
        unitConfig.RequiresMountsFor = [secretsMount];
        path = with guestPkgs; [coreutils openssl];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          RuntimeDirectory = "ci-trust";
          RuntimeDirectoryMode = "0755";
          UMask = "0022";
        };
        script = ''
          set -euo pipefail
          source=${secretsMount}/ca.crt
          openssl x509 -in "$source" -checkend 0 -noout
          install -m 0644 "$source" ${trustDir}/proxy-ca.crt
          # caBundle is the normal store-built public-root bundle, independent
          # of the overridden /etc symlinks above.
          cat ${guestConfig.security.pki.caBundle} ${trustDir}/proxy-ca.crt > ${trustDir}/.bundle.new
          chmod 0644 ${trustDir}/.bundle.new
          mv ${trustDir}/.bundle.new ${trustDir}/ca-bundle.crt
        '';
      };
      services.github-runners.${name} = {
        enable = true;
        inherit (runner) url;
        name = name;
        tokenFile = "${secretsMount}/gh-token";
        replace = true;
        ephemeral = runner.ephemeralRegistration;
        workDir = "${workMount}/work";
        user = "ci-runner";
        group = "ci-runner";
        # ProtectSystem=strict leaves /var read-only; TMPDIR lives on the scratch disk.
        serviceOverrides = {
          ReadWritePaths = ["${workMount}/tmp"];
          # Browser sandboxes (Playwright's Firefox and WebKit) create user
          # namespaces. The one-job VM is the isolation boundary here.
          RestrictNamespaces = false;
        };
        extraLabels = ["nixos" "microvm" "audited"] ++ runner.labels;
        extraEnvironment = proxyEnvironment // {TMPDIR = "${workMount}/tmp";};
        extraPackages =
          [guestPkgs.curl guestPkgs.openssl guestPkgs.python3]
          ++ lib.optional cfg.nix.enable guestConfig.nix.package
          ++ runner.extraPackages;
      };
      # Keep the runner out of the boot transaction. The host's microvm@ unit
      # waits for the guest to report boot complete; with the runner inside
      # that transaction, a failed registration (sleep, then poweroff) meant
      # the guest never reported, every start failed, and anything waiting on
      # the VMs hung, `nixos-rebuild switch` included. Booting now finishes
      # first; this unit then queues the runner without waiting for it.
      systemd.services.ci-runner-start = {
        description = "Start the GitHub runner once boot has completed";
        wantedBy = ["multi-user.target"];
        after = ["ci-proxy-trust.service" "network-online.target"];
        wants = ["network-online.target"];
        serviceConfig.Type = "oneshot";
        script = "${guestPkgs.systemd}/bin/systemctl start --no-block github-runner-${name}.service";
      };
      systemd.services."github-runner-${name}" = {
        wantedBy = lib.mkForce [];
        after = ["ci-proxy-trust.service"];
        requires = ["ci-proxy-trust.service"];
        unitConfig.RequiresMountsFor = [secretsMount workMount];
        serviceConfig = lib.mkMerge [
          {
            # nixpkgs' filter minus `~capset` and `~@mount`, both of which kill
            # browser processes ("Target crashed" in Playwright): Chromium drops
            # capabilities with capset even under --no-sandbox, and Firefox's
            # content sandbox builds its own mount namespace. Found by bisecting
            # the filter under systemd-run. The list is appended to upstream, so
            # the whole of it is replaced rather than entries removed.
            SystemCallFilter = lib.mkForce [
              "~@clock"
              "~@cpu-emulation"
              "~@module"
              "~@obsolete"
              "~@raw-io"
              "~@reboot"
              "~setdomainname"
              "~sethostname"
            ];
          }
          (lib.mkIf runner.ephemeralRegistration {
            # One job per VM: power off when the runner exits and let the host
            # restart the VM with a fresh disk. On failure (e.g. GitHub or the
            # proxy unreachable) wait first, so four VMs don't hammer the API.
            Restart = lib.mkForce "no";
            ExecStopPost = [
              ("+"
                + guestPkgs.writeShellScript "ci-runner-poweroff" ''
                  if [ "$SERVICE_RESULT" != success ]; then ${guestPkgs.coreutils}/bin/sleep 30; fi
                  exec ${guestPkgs.systemd}/bin/systemctl poweroff --no-block
                '')
            ];
          })
        ];
      };
    };
  };
in {
  imports = [./mitm-proxy.nix];
  options.services.microvm-github-runners = {
    runners = mkOption {
      type = types.attrsOf (types.submodule runnerOptions);
      default = {};
      description = "One cloud-hypervisor microVM per runner. Names: 1-11 ASCII letters/digits/hyphens, starting with a letter or digit.";
    };
    autoStart = mkOption {
      type = types.bool;
      default = true;
      description = "Automatically start runner VMs. Use false for first-deployment firewall/trust tests.";
    };
    nix = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Run a Nix daemon in each guest so jobs can use `nix develop` / `nix build`.
          The store gets a writable overlay on a scratch disk recreated every boot.
          Downloads go through the audit proxy, except the optional host cache.
        '';
      };
      storeDiskGiB = mkOption {
        type = types.ints.positive;
        default = 60;
        description = "Sparse per-boot disk holding the guest's writable store overlay.";
      };
      hostCache = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Use a binary cache on the host (e.g. harmonia), reached directly at
            hostAddress:port over each runner's tap, bypassing the proxy. Jobs
            can then read anything that cache serves.
          '';
        };
        port = mkOption {
          type = types.port;
          default = 5000;
          description = "Host cache TCP port.";
        };
        publicKey = mkOption {
          type = types.str;
          description = "The host cache's signing public key (name:base64).";
        };
      };
    };
    debugSshKeys = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        SSH public keys allowed to log in as root in every runner guest over
        vsock only (`sudo microvm -s NAME` on the host). Empty disables sshd.
        Guest root can read the PAT; treat as debugging-only.
      '';
    };
    hostAddress = mkOption {
      type = net.ipv4;
      default = "10.233.0.1";
      description = "Host endpoint of each runner's independent /32 point-to-point link.";
    };
  };
  config = mkIf (cfg.runners != {}) {
    assertions = [
      {
        assertion = lib.all (name: builtins.match "[a-zA-Z0-9][a-zA-Z0-9-]{0,10}" name != null) names;
        message = "microvm-github-runners: names must be 1-11 alphanumeric/hyphen characters (tap is ci-<name>, max 15).";
      }
      {
        assertion = builtins.length addresses == builtins.length (lib.unique addresses);
        message = "microvm-github-runners: localAddress values must be unique.";
      }
      {
        assertion =
          lib.all (address: address != cfg.hostAddress) addresses
          && !(builtins.elem cfg.hostAddress [proxy.address proxy.hostAddress proxy.gateway]);
        message = "microvm-github-runners: runner hostAddress must not collide with runners or proxy addressing.";
      }
    ];
    # microvm.nix's host module turns KSM on with mkDefault. Without virtiofs,
    # guest RAM is registered as mergeable, so KSM would give concurrent jobs
    # a page-sharing side channel. Beat that default; an explicit host
    # setting still wins, with a warning.
    hardware.ksm.enable = lib.mkOverride 999 false;
    warnings =
      lib.optional config.hardware.ksm.enable
      "microvm-github-runners: KSM is enabled; runner guest RAM is mergeable, which gives jobs a cross-VM page-sharing side channel.";
    services.ci-mitm-proxy = {
      enable = true;
      clientHostAccess = lib.mkIf hostCache {
        address = cfg.hostAddress;
        ports = [cfg.nix.hostCache.port];
      };
      clients =
        lib.mapAttrs (name: runner: {
          interface = tapName name;
          address = runner.localAddress;
        })
        cfg.runners;
    };
    systemd.services = lib.mapAttrs' mkHostUnit cfg.runners;
    microvm.vms = lib.mapAttrs mkVM cfg.runners;
  };
}
