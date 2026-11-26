# Base system profile for every machine, including the installer ISO: tooling,
# nix settings, nixpkgs pinned to the dotfiles input, the primary user and SSH
# setup, Tailscale and the node exporter. Nothing here is optional.
{nixpkgs}: {
  lib,
  pkgs,
  ...
}: let
  user = "edwlan";
  # SSH user certificate authorities trusted by sshd. Any user certificate
  # signed by one of these CAs with a matching principal may log in without its
  # public key being pre-registered; this is what lets the primary user reach a
  # freshly installed machine out of the box.
  trustedUserCAKeys = pkgs.writeText "fwoar-trusted-user-ca-keys.pub" ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH5XSJGJ5poII9t+FEnhqoNhtHtv0AqsgMJJJ1866lND 2020-11-23
  '';
in {
  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    substituters = [
      "https://cache.nixos.org"
      "https://fwoar-greenhouse.cachix.org"
      "https://hercules-ci.cachix.org"
      "https://latex-sample.cachix.org"
      "https://nix-community.cachix.org"
      "https://siuta.cachix.org"
    ];
    trusted-public-keys = [
      "fwoar-greenhouse.cachix.org-1:OSOY4mJ0TIscoZsGZjX0Y0IFQCjUoBYFQpQdDGGE/iM="
      "fwoar-greenhouse.cachix.org-1:iQ+rkqzBJOebBlwcRyvctq3oZHoDBz5x9qHqWC7n9rg="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
      "latex-sample.cachix.org-1:wyrXgr9qt1PcnWdBbmyMpkRvl0Z1doFe+/ssbzz3OJw="
      "siuta.cachix.org-1:aNWNdZez5khiQkgPeU64P9x4T3DHT5gw6VsPWvAYE20="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  # Keep the store from growing without bound: drop profile generations older
  # than 30 days and anything unreachable weekly, and hard-link duplicates.
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
  nix.optimise.automatic = true;

  # `<nixpkgs>` and `nixpkgs#…` both resolve to the dotfiles nixpkgs input.
  # Stateful channels are disabled so nothing else can shadow or drift from it.
  environment.etc."channels/nixpkgs".source = nixpkgs.outPath;
  nix.channel.enable = false;
  nix.nixPath = ["nixpkgs=/etc/channels/nixpkgs"];
  nix.registry.nixpkgs.flake = nixpkgs;

  documentation = {
    man.enable = true;
    info.enable = true;
  };

  environment.systemPackages = with pkgs; [
    alejandra
    dmidecode
    file
    findutils
    gitFull
    htop
    lm_sensors
    lsof
    man-pages
    man-pages-posix
    nvme-cli
    pciutils
    smartmontools
    sops
    vim
    wget
  ];

  programs = {
    nix-ld.enable = true;
    vim.enable = true;
    zsh.enable = true;
  };
  users.defaultUserShell = pkgs.zsh;

  time.timeZone = lib.mkDefault "America/Los_Angeles";
  i18n.defaultLocale = lib.mkDefault "en_US.UTF-8";

  services.openssh = {
    enable = lib.mkDefault true;
    settings = {
      PasswordAuthentication = lib.mkDefault false;
      KbdInteractiveAuthentication = lib.mkDefault false;
      PermitRootLogin = lib.mkDefault "no";
    };
    extraConfig = ''
      TrustedUserCAKeys ${trustedUserCAKeys}
    '';
  };

  users.users.${user} = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    shell = pkgs.zsh;
  };

  services.tailscale.enable = true;

  services.prometheus.exporters.node = {
    enable = true;
    port = 9100;
    openFirewall = true;
    enabledCollectors = ["hwmon" "drm" "systemd"];
  };
}
