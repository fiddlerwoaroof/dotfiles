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
      self.nixosModules.base
      sops-nix.nixosModules.sops
      self.nixosModules.fwoar-grafana
      self.nixosModules.mpd-setup
      self.nixosModules.bincache-setup
      ({pkgs, ...}: {
        environment.systemPackages = [
          pkgs.libdrm
          pkgs.lm_sensors
          pkgs.pkg-config
          pkgs.smartmontools
        ];
      })
      self.nixosModules.srv2-sops
      {
        systemd.tmpfiles.rules = [
          "d /pool/dataset/nix-build 1777 root root -"
        ];
        nix.settings.build-dir = "/tank/nixbuild/";
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
