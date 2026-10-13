{...}: let
  repoURL = "https://github.com/fiddlerwoaroof/improvise";
  # All four runners share one PAT; each registers under its own name.
  tokenFile = "/run/secrets/gh-runner-pat";
in {
  # GitHub PAT for runner registration, from titan's sops file.
  sops.secrets.gh-runner-pat = {};

  services.ci-mitm-proxy = {
    enable = true;
    externalInterface = "enp5s0";
    gateway = "172.16.1.1";
    # The router is the only DNS it allows; port 53 only.
    dnsServers = ["172.16.1.1"];
    # Add your router's public WAN IP as "x.x.x.x/32" if it does hairpin NAT.
    extraBlockedNetworks = [];
  };

  services.microvm-github-runners = {
    autoStart = true;

    nix = {
      enable = true;
      hostCache = {
        enable = true; # harmonia on titan:5000, reached directly at 10.233.0.1
        publicKey = "titan-1:4trfowu4WP6Hxb8LGB6zS5VRuDIu4hHHC4QIsfrI9WE=";
      };
    };

    # `sudo microvm -s ghr-N`; guest root, so debugging only.
    debugSshKeys = [
      "cert-authority ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH5XSJGJ5poII9t+FEnhqoNhtHtv0AqsgMJJJ1866lND 2020-11-23"
    ];

    # Two physical cores (four threads) per runner, together all of CCD1
    # (cores 8-15). CCD0 stays with Minecraft, ollama, harmonia and the host.
    runners = {
      ghr-1 = {
        url = repoURL;
        inherit tokenFile;
        localAddress = "10.233.0.2";
        cpus = "8-9,24-25";
        vcpu = 4;
        memoryMiB = 8192;
      };
      ghr-2 = {
        url = repoURL;
        inherit tokenFile;
        localAddress = "10.233.0.3";
        cpus = "10-11,26-27";
        vcpu = 4;
        memoryMiB = 8192;
      };
      ghr-3 = {
        url = repoURL;
        inherit tokenFile;
        localAddress = "10.233.0.4";
        cpus = "12-13,28-29";
        vcpu = 4;
        memoryMiB = 8192;
      };
      ghr-4 = {
        url = repoURL;
        inherit tokenFile;
        localAddress = "10.233.0.5";
        cpus = "14-15,30-31";
        vcpu = 4;
        memoryMiB = 8192;
      };
    };
  };
}
