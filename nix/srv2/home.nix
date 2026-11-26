{
  self,
  mkHome,
  ...
}: let
  system = "x86_64-linux";
in
  mkHome {
    inherit system;
    modules = [
      self.homeManagerModules.emacs
      self.homeManagerModules.direnv
      self.homeManagerModules.tmux
      self.homeManagerModules.cli-tools
      self.homeManagerModules.shell-dotfiles
      ({
        config,
        lib,
        pkgs,
        ...
      }: let
        hub-repo = "${config.home.homeDirectory}/git_repos/git.fiddlerwoaroof.com/u/edwlan/fwoar-hub/";
      in {
        systemd.user.services.hub = {
          Unit.AssertPathExists = "${config.home.homeDirectory}/fwoar-hub";

          Service = {
            WorkingDirectory = "${config.home.homeDirectory}/fwoar-hub";
            Environment = [
              "LD_LIBRARY_PATH=${lib.makeLibraryPath [pkgs.zeromq pkgs.openssl pkgs.libev]}"
            ];
            ExecStart = "${hub-repo}/fwoar-hub";
            Restart = "always";
            PrivateTmp = false;
            NoNewPrivileges = true;
          };

          Install.WantedBy = ["default.target"];
        };
      })
      {
        home.packages = [self.packages.${system}.ddns-updater];
        systemd.user = {
          enable = true;
          timers.ddns-updater = {
            Unit = {
              Description = "Update DDNS occasionally";
              RefuseManualStart = false;
              RefuseManualStop = false;
            };

            Timer = {
              Persistent = false;
              OnBootSec = "300min";
              OnUnitActiveSec = "300min";
              Unit = "ddns-updater.service";
            };
          };
          services.ddns-updater = {
            Unit = {
              Description = "Sync DDNS records";
              RefuseManualStart = false;
              RefuseManualStop = true;
            };
            Service = {
              Type = "oneshot";
              ExecStart = "${self.packages.${system}.ddns-updater}/bin/ddns-updater";
            };
          };
        };
      }
      {home.stateVersion = "25.05";}
    ];
  }
