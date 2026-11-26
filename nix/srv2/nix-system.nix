{
  self,
  nixpkgs,
  nixpkgs-master,
  sops-nix,
}: let
  system = "x86_64-linux";
in
  nixpkgs.lib.nixosSystem {
    inherit system;
    specialArgs = {
      pkgs-unstable = import nixpkgs-master {inherit system;};
    };
    modules = [
      sops-nix.nixosModules.sops
      self.nixosModules.fwoar-grafana
      self.nixosModules.mpd-setup
      self.nixosModules.bincache-setup
      {
        environment.etc."channels/nixpkgs".source = nixpkgs.outPath;
        nix.nixPath = [
          "nixpkgs=/etc/channels/nixpkgs"
          "nixos-config=/etc/nixos/configuration.nix"
          "/nix/var/nix/profiles/per-user/root/channels"
        ];
        nix.registry.nixpkgs.flake = nixpkgs;
      }
      {programs.nix-ld.enable = true;}
      ({pkgs, ...}: {
        environment.systemPackages = [
          pkgs.alejandra
          pkgs.dmidecode
          pkgs.gitFull
          pkgs.htop
          pkgs.libdrm
          pkgs.lm_sensors
          pkgs.lsof
          pkgs.pkg-config
          pkgs.smartmontools
          pkgs.sops
          pkgs.vim
          pkgs.wget
        ];
      })
      self.nixosModules.srv2-sops
      {
        systemd.tmpfiles.rules = [
          "d /pool/dataset/nix-build 1777 root root -"
        ];

        nix.settings = {
          experimental-features = ["nix-command" "flakes"];
          build-dir = "/tank/nixbuild/";
          substituters = [
            "https://cache.nixos.org"
            "https://fwoar-greenhouse.cachix.org"
            "https://nix-community.cachix.org"
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
          trusted-users = ["root"];
        };
      }
      ./nixos/configuration.nix
      ./nixos/zfs.nix
      ./nixos/nginx.nix
      ./nixos/radicale.nix
      ({config, ...}: {
        sops.secrets.grafana_secret_key = {
          owner = "grafana";
          restartUnits = ["grafana.service"];
        };
        services.grafana.settings.security.secret_key = "$__file{${config.sops.secrets.grafana_secret_key.path}}";
      })
    ];
  }
