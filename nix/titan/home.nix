{
  alejandra,
  emacs-community,
  home-manager,
  nixpkgs,
  self,
  sops-nix,
  system,
  ...
}: let
  openssl = pkgs.openssl.overrideAttrs (oldAttrs: {meta = oldAttrs.meta // {outputsToInstall = oldAttrs.meta.outputsToInstall or ["out"] ++ ["dev"];};});
  username = "edwlan";
  common_home = import ../common.nix {inherit pkgs;};
  utils = common_home.utils;
  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      (import ../personal-flake/elangley-overlay)
    ];
  };
  emacs-pkgs = emacs-community.packages.${system};
in
  home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      sops-nix.homeManagerModules.sops
      self.homeManagerModules.common
      self.homeManagerModules.fonts
      (import ./legacy.nix)
      {
        sops = {
          age.keyFile = "/home/${username}/.age-key.txt";
          defaultSopsFile = ./secrets.json;
          secrets.mail-password = {
            path = "%r/mail-password.txt";
          };
        };
      }
      {
        home = {
          packages =
            [
              pkgs.ncdu
              (import ../lpass-nix {inherit pkgs;})
              emacs-pkgs.emacs-git
              openssl
              pkgs.awscli
              pkgs.cachix
              pkgs.curl
              pkgs.cvs
              pkgs.direnv
              pkgs.glibcLocales
              pkgs.gron
              pkgs.libssh2
              pkgs.lorri
              pkgs.nix
              pkgs.pass
              pkgs.sqlite
              pkgs.sqlite.dev
              pkgs.sqlite.out
              pkgs.visidata
              pkgs.zeromq
            ]
            ++ (with pkgs; [
              ccl
              ecl
              #gcl
              cmucl_binary
              nixpkgs-fmt
            ]);
        };
      }
      {
        systemd.user = {
          enable = true;
          timers = {
            nm-new = {
              Unit = {
                After = ["sops-nix.service"];
                Description = "Check Mail every fifteen minutes";
                RefuseManualStart = false;
                RefuseManualStop = false;
              };

              Timer = {
                Persistent = false;
                OnBootSec = "5min";
                OnUnitActiveSec = "5min";
                Unit = "nm-new.service";
              };

              Install = {
                WantedBy = ["timers.target"];
              };
            };
          };
          services = let
            writeZsh = pkgs.writers.makeScriptWriter {interpreter = "${pkgs.zsh}/bin/zsh";};
          in {
            nm-new = {
              Unit = {
                Description = "Sync mail with notmuch";
                RefuseManualStart = false;
                RefuseManualStop = true;
              };
              Service = {
                Type = "oneshot";
                ExecStart = writeZsh "nm-new" ''
                  set -eu -o pipefail
                  export PATH=$PATH:"$HOME"/bin
                  ${pkgs.notmuch}/bin/notmuch new
                '';
              };
            };
          };
        };
      }
      self.homeManagerModules.git-config
      (import ./email.nix)
      {
        home.file = {
          ".vimrc".source = ../../vimrc_work;
          ".vim/repos/github.com/Shougo/dein.vim".source =
            (pkgs.fetchFromGitHub {
              owner = "Shougo";
              repo = "dein.vim";
              rev = "master";
              hash = "sha256-whrWIex57PwSSIOViLby71slC6VXDOapdaICO7o6Oms";
            }).outPath;
          ".zshrc".source = ../../zsh/zshrc_work;
          ".zsh.d/nix-zsh-completions".source = ../../3dp/nix-zsh-completions;
          ".zsh.d" = {
            source = ../../zsh/zsh_plugins;
            recursive = true;
          };
        };
      }
      {
        targets.genericLinux.enable = true;
        home.stateVersion = "21.03";
      }
    ];
    extraSpecialArgs = {
      inherit system;
      fwoar-pkgs = self.packages.${system};
      emacs-pkgs = emacs-community.packages.${system};
      alejandra-pkgs = alejandra.packages.${system};
    };
  }
