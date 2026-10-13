{pkgs, ...}: {
  nixpkgs.hostPlatform = "x86_64-linux";
  networking.hostName = "ci-host";
  # EXAMPLE ONLY: replace the interface/address with an unused address on your LAN.
  networking.useDHCP = false;
  networking.interfaces.enp1s0.ipv4.addresses = [
    {
      address = "172.16.1.50";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = {
    address = "172.16.1.1";
    interface = "enp1s0";
  };
  # These are HOST resolvers. The runners have no direct DNS access.
  networking.nameservers = ["1.1.1.1" "9.9.9.9"];
  nix.settings.experimental-features = ["nix-command" "flakes"];
  # Preserve your existing host's stateVersion; do not bump it merely to use this module.
  system.stateVersion = "26.05";

  services.ci-mitm-proxy = {
    enable = true;
    externalInterface = "enp1s0";
    gateway = "172.16.1.1";
    # REQUIRED site-specific review: add router PUBLIC WAN /32 addresses that
    # can hairpin to your LAN, and public IP networks routed internally.
    extraBlockedNetworks = [];
    # Empty means audited public HTTP(S), not a GitHub-only domain allowlist.
    allowedDomains = [];
    allowWebSockets = true;
    audit = {
      logPaths = false; # Paths can contain secrets; query strings are always omitted.
      maxFileBytes = 50 * 1024 * 1024;
      backups = 10;
      fsync = true;
    };
  };
  services.microvm-github-runners = {
    # Deliberately safe for first deployment. Start one VM manually
    # (systemctl start microvm@ghr-1) after reviewing the firewall, then change
    # this to true for normal operation.
    autoStart = false;
    # Four runners on CCD1 of a 5950X: one physical core (both SMT threads),
    # 16 GiB RAM and a 100 GiB scratch disk each. Verify the core/thread
    # numbering with `lscpu -e=CPU,CORE,CACHE` before using these cpusets.
    runners = {
      ghr-1 = {
        url = "https://github.com/example/project-a";
        tokenFile = "/var/secrets/gh-pat-project-a";
        localAddress = "10.233.0.2";
        cpus = "8,24";
      };
      ghr-2 = {
        url = "https://github.com/example/project-a";
        tokenFile = "/var/secrets/gh-pat-project-a";
        localAddress = "10.233.0.3";
        cpus = "9,25";
      };
      ghr-3 = {
        url = "https://github.com/example-org";
        tokenFile = "/var/secrets/gh-pat-org";
        localAddress = "10.233.0.4";
        cpus = "10,26";
        extraPackages = with pkgs; [gnumake gcc];
      };
      ghr-4 = {
        url = "https://github.com/example-org";
        tokenFile = "/var/secrets/gh-pat-org";
        localAddress = "10.233.0.5";
        cpus = "11,27";
      };
    };
  };
}
