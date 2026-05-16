{
  inputs = {
    alejandra = {
      url = "github:kamadorueda/alejandra";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    claude-nixpkgs.url = "github:NixOS/nixpkgs/master";
    emacs-community = {url = "github:nix-community/emacs-overlay";};
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-editor = {url = "github:snowfallorg/nix-editor";};
    nixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };
    ollama-nixpkgs.url = "github:NixOS/nixpkgs/ac4dd85979ee6eeac9a5f7aa95534f667a26e980";
    sops-nix.url = "github:Mic92/sops-nix";
    titan-home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "titan-nixpkgs";
    };
    titan-nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = {
    self,
    alejandra,
    emacs-community,
    home-manager,
    nixpkgs,
    sops-nix,
    claude-nixpkgs,
    titan-nixpkgs,
    titan-home-manager,
    ...
  } @ inputs: let
    withSystem = system: attrSet: attrSet // {inherit system;};
    withAppleSilicon = withSystem "aarch64-darwin";
    withx8664Linux = withSystem "x86_64-linux";
  in {
    packages = import ./nix/packages inputs;
    homeManagerModules = {
      common = import ./nix/common-module.nix;
      personal-module-configs = {
        fwoar.info.fullName = "Edward Langley";
        fwoar.github.username = "fiddlerwoaroof";
        fwoar.git.email = "el-github@elangley.org";
      };
      emacs = {emacs-pkgs, ...}: {
        home.packages = [
          emacs-pkgs.emacs-git
        ];
        services.emacs = {
          enable = true;
          package = emacs-pkgs.emacs-git;
        };
      };
      fonts = {pkgs, ...}: {
        home.packages = [
          pkgs.lato
          pkgs.alegreya
          pkgs.source-code-pro
          pkgs.alegreya-sans
        ];
      };
      git-config = import ./nix/git-config.nix;
      mac-apps = import ./nix/mac-apps;
      main = import ./nix/personal-flake/home.nix;
      direnv = {
        programs.direnv = {
          enable = true;
          nix-direnv = {enable = true;};
        };
      };
      tmux = {
        programs.tmux = {
          enable = true;
          clock24 = true;
          escapeTime = 0;
          extraConfig = builtins.readFile ./tmux.conf;
          keyMode = "vi";
          newSession = true;
          terminal = "screen-256color";
        };
      };
    };
    homeConfigurations = {
      "ouranos" = import ./nix/ouranos/home.nix (withAppleSilicon {
        inherit self alejandra emacs-community home-manager nixpkgs sops-nix;
        nix-editor = inputs.nix-editor;
      });
      "titan" = import ./nix/titan/home.nix ((withx8664Linux inputs)
        // {
          nixpkgs = titan-nixpkgs;
          cc-pkgs = import claude-nixpkgs {
            system = "x86_64-linux";
            config.allowUnfreePredicate = pkg:
              builtins.elem (nixpkgs.lib.getName pkg) [
                "claude-code"
              ];
          };
          home-manager = titan-home-manager;
        });
      "srv2" = import ./nix/srv2/home.nix (withx8664Linux inputs);
    };
    apps.aarch64-darwin = let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
      self-pkgs = self.packages.${system};
      writeZsh = pkgs.writers.makeScriptWriter {interpreter = "${pkgs.zsh}/bin/zsh";};
    in {
      cls = {
        type = "app";
        buildInputs = [self-pkgs.cls];
        program = "${self-pkgs.cls}/bin/cls";
      };
    };
    nixosModules = {
      tailscale = import ./nix/nixos-modules/tailscale.nix;
      home-assistant = import ./nix/nixos-modules/home-assistant.nix;
      srv2-sops = import ./nix/srv2/sops.nix;
      fwoar-grafana = {
        config,
        pkgs,
        ...
      }: {
        # Node exporter - collects hwmon/drm metrics including MI100 temps
        services.prometheus.exporters.node = {
          enable = true;
          port = 9100;
          openFirewall = true;
          enabledCollectors = ["hwmon" "drm" "systemd"];
        };

        # Prometheus - scrapes and stores metrics
        services.prometheus = {
          enable = true;
          scrapeConfigs = [
            {
              job_name = "node";
              scrape_interval = "10s";
              static_configs = [
                {
                  targets = ["127.0.0.1:${toString config.services.prometheus.exporters.node.port}"];
                  labels = {host = "srv2";};
                }
              ];
            }
            {
              job_name = "titan";
              scrape_interval = "10s";
              static_configs = [
                {
                  targets = ["titan.h.elangley.org:9100"];
                  labels = {host = "titan";};
                }
              ];
            }
          ];
        };

        # Grafana - dashboards
        services.grafana = {
          enable = true;
          settings.server = {
            http_addr = "0.0.0.0";
            http_port = 3000;
          };
          provision = {
            datasources.settings.datasources = [
              {
                name = "Prometheus";
                type = "prometheus";
                url = "http://localhost:${toString config.services.prometheus.port}";
                isDefault = true;
              }
            ];
            dashboards.settings = {
              apiVersion = 1;
              providers = [
                {
                  name = "nixos-provisioned";
                  options.path = "/etc/grafana-dashboards";
                }
              ];
            };
          };
        };

        environment.etc."grafana-dashboards/node-overview.json" = {
          source =
            pkgs.writeText "node-overview.json" (builtins.toJSON
              (import ./nix/srv2/overview-dashboard.nix).dashboard);
          user = "grafana";
          group = "grafana";
        };

        sops.secrets.grafana_password = {
          owner = "grafana";
          group = "grafana";
        };
      };
      mpd-setup = {
        config,
        pkgs,
        ...
      }: {
        # AirPlay/RAOP discovery
        environment.systemPackages = [
          pkgs.mpc
          pkgs.ncmpcpp
        ];
        services = {
          avahi.enable = true;
          pipewire = {
            raopOpenFirewall = true; # opens UDP 6001-6002
            extraConfig.pipewire."10-airplay" = {
              "context.modules" = [
                {name = "libpipewire-module-raop-discover";}
              ];
            };
          };

          mpd = {
            enable = true;
            user = "edwlan";

            musicDirectory = "/home/edwlan/oldhome/sorted_music";
            extraConfig = ''
              audio_output {
                type "pipewire"
                name "PipeWire"
              }
            '';
            # Optional:
            network.listenAddress = "any"; # if you want to allow non-localhost connections
            #startWhenNeeded = true;
            # systemd feature: only start MPD service upon connection to its socket
          };
        };
        systemd.services.mpd.environment = {
          # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/609
          XDG_RUNTIME_DIR = "/run/user/1000"; # User-id must match above user. MPD will look inside this directory for the PipeWire socket.
        };
      };
    };
    nixosConfigurations = {
      titan = import ./nix/titan/nix-system.nix {
        nixpkgs = titan-nixpkgs;
        home-manager = titan-home-manager;
        inherit self sops-nix;
      };
    };
  };
}
