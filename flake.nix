{
  inputs = {
    titan-nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    claude-nixpkgs.url = "github:NixOS/nixpkgs/master";
    ollama-nixpkgs.url = "github:NixOS/nixpkgs/ac4dd85979ee6eeac9a5f7aa95534f667a26e980";
    alejandra = {
      url = "github:kamadorueda/alejandra";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-community = {url = "github:nix-community/emacs-overlay";};
    titan-home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "titan-nixpkgs";
    };
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
    emacs-hack = {url = "github:fiddlerwoaroof/emacs-nix-hack";};
    sops-nix.url = "github:Mic92/sops-nix";
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
      "ouranos" = import ./nix/ouranos/home.nix (withAppleSilicon inputs);
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
    nixosConfigurations.titan = titan-nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ({lib, ...}: {
          nix.registry.nixpkgs.flake = titan-nixpkgs;
          nix.nixPath = [
            "nixpkgs=/etc/channels/nixpkgs"
            "nixos-config=/etc/nixos/configuration.nix"
            "/nix/var/nix/profiles/per-user/root/channels"
          ];
          environment.etc."channels/nixpkgs".source = inputs.titan-nixpkgs.outPath;
          nixpkgs.config.allowUnfreePredicate = pkg:
            builtins.elem (lib.getName pkg) [
              "dropbox"
              "open-webui"
            ];
        })
        (import ./nix/nixos-modules/tailscale.nix)
        ({pkgs, ...}: {
          environment.systemPackages = [
            pkgs.alejandra
            pkgs.dmidecode
            pkgs.gitFull
            pkgs.htop
            pkgs.lsof
            pkgs.sops
            pkgs.vim
            pkgs.wget
          ];
        })
        ({pkgs, ...}: {
          # In configuration.nix or flake.nix

          environment.systemPackages = with pkgs; [motion];

          # Create motion config file
          environment.etc."motion/motion.conf" = {
            text = ''
              daemon on

              video_device /dev/video0
              video_params -1

              width 640
              height 480
              framerate 3

              picture_quality 80
              picture_output when-motion
              picture_type jpeg
              picture_filename %Y%m%d_%H%M%S-%q


              target_dir /var/lib/motion

              threshold 1500
              threshold_maximum 1800
              despeckle_filter EedDl

              # Video capture settings
              movie_output on
              movie_quality 60
              movie_max_time 0
              movie_timelapse 0

              # Pre/post capture (in frames)
              pre_capture 15
              post_capture 15

              stream_localhost off
              stream_port 8081
            '';
          };

          # Systemd service
          systemd.services.motion = {
            description = "Motion Detection";
            after = ["network.target"];
            wantedBy = ["multi-user.target"];

            preStart = ''
              mkdir -p /var/lib/motion /var/log/motion
              chown motion:motion /var/lib/motion /var/log/motion
            '';

            serviceConfig = {
              Type = "simple";
              ExecStart = "${pkgs.motion}/bin/motion -c /etc/motion/motion.conf -n";
              Restart = "on-failure";
              RestartSec = 10;
              User = "motion";
              Group = "motion";
              StandardOutput = "journal";
              StandardError = "journal";
            };
          };

          users.users.motion = {
            isSystemUser = true;
            group = "motion";
            extraGroups = ["video"];

            home = "/var/lib/motion";
          };

          users.groups.motion = {};

          # Dropbox sync timer
          systemd.timers.motion-to-dropbox = {
            wantedBy = ["timers.target"];
            timerConfig = {
              OnBootSec = "2min";
              OnUnitActiveSec = "5min";
              Unit = "motion-to-dropbox.service";
            };
          };

          systemd.services.motion-to-dropbox = {
            description = "Sync motion captures to Dropbox";
            serviceConfig = {
              Type = "oneshot";
              ExecStart = "${pkgs.rsync}/bin/rsync -rvh /var/lib/motion/ '/home/edwlan/.dropbox-hm/Langleys Dropbox/Edward Langley/motion/'";
              ExecStartPost = "${pkgs.coreutils}/bin/chown -R edwlan: '/home/edwlan/.dropbox-hm/Langleys Dropbox/Edward Langley/motion/'";
              User = "root";
            };
          };
        })
        ./nix/titan/nixos/ollama.nix
        ./nix/titan/nixos/configuration.nix
        sops-nix.nixosModules.sops
        titan-home-manager.nixosModules.home-manager
        ./nix/nixos-modules/home-assistant.nix
        {
          home-manager.useGlobalPkgs = false;
          home-manager.useUserPackages = false;
        }
      ];
    };
  };
}
