# Base system profile abstracted from titan: a useful subset of the tooling,
# nix settings and SSH setup worth having on any new machine (including the
# installer ISO). Enabled by default; toggle individual pieces via options.
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.fwoar.base;
  caKeysFile =
    pkgs.writeText "fwoar-trusted-user-ca-keys.pub"
    (lib.concatStringsSep "\n" cfg.trustedUserCAKeys + "\n");
in {
  options.fwoar.base = {
    enable =
      lib.mkEnableOption "the fwoar base system profile"
      // {default = true;};

    user = lib.mkOption {
      type = lib.types.str;
      default = "edwlan";
      description = "Primary interactive user account to create.";
    };

    userGroups = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = ["wheel"];
      description = "Groups the primary user is a member of.";
    };

    authorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = "Raw authorized SSH public keys for the primary user.";
    };

    trustedUserCAKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH5XSJGJ5poII9t+FEnhqoNhtHtv0AqsgMJJJ1866lND 2020-11-23"
      ];
      description = ''
        SSH user certificate authorities trusted by sshd. Any user certificate
        signed by one of these CAs with a matching principal may log in without
        its public key being pre-registered — this is what lets the primary user
        reach a freshly installed machine out of the box.
      '';
    };

    extraSystemPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [];
      description = "Additional system packages to install beyond the base set.";
    };

    binaryCaches.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Configure the fwoar binary cache substituters and keys.";
    };

    nixpkgsFlake = lib.mkOption {
      type = lib.types.nullOr lib.types.raw;
      default = null;
      description = ''
        The nixpkgs flake input this system is built from. When set, pin it in
        the flake registry and expose it at /etc/channels/nixpkgs for NIX_PATH
        so `<nixpkgs>` and `nixpkgs#…` match the running system.
      '';
    };

    tailscale.enable =
      lib.mkEnableOption "Tailscale"
      // {default = true;};

    nodeExporter.enable = lib.mkEnableOption ''
      the Prometheus node exporter (hwmon/drm/systemd collectors) on port
      9100, opened in the firewall
    '';
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      nix.settings.experimental-features = ["nix-command" "flakes"];

      documentation = {
        man.enable = true;
        info.enable = true;
      };

      environment.systemPackages =
        (with pkgs; [
          alejandra
          dmidecode
          file
          findutils
          gitFull
          htop
          lsof
          man-pages
          man-pages-posix
          nvme-cli
          pciutils
          sops
          vim
          wget
        ])
        ++ cfg.extraSystemPackages;

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
        extraConfig = lib.mkIf (cfg.trustedUserCAKeys != []) ''
          TrustedUserCAKeys ${caKeysFile}
        '';
      };

      users.users.${cfg.user} = {
        isNormalUser = true;
        extraGroups = cfg.userGroups;
        shell = pkgs.zsh;
        openssh.authorizedKeys.keys = cfg.authorizedKeys;
      };
    }
    (lib.mkIf (cfg.nixpkgsFlake != null) {
      # `<nixpkgs>` and `nixpkgs#…` both resolve to the input the system was
      # built from. Stateful channels are disabled so nothing else can shadow
      # or drift from it.
      environment.etc."channels/nixpkgs".source = cfg.nixpkgsFlake.outPath;
      nix.channel.enable = false;
      nix.nixPath = ["nixpkgs=/etc/channels/nixpkgs"];
      nix.registry.nixpkgs.flake = cfg.nixpkgsFlake;
    })
    (lib.mkIf cfg.tailscale.enable {
      services.tailscale.enable = true;
    })
    (lib.mkIf cfg.nodeExporter.enable {
      services.prometheus.exporters.node = {
        enable = true;
        port = 9100;
        openFirewall = true;
        enabledCollectors = ["hwmon" "drm" "systemd"];
      };
    })
    (lib.mkIf cfg.binaryCaches.enable {
      nix.settings = {
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
    })
  ]);
}
