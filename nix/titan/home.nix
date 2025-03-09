{
  self,
  system,
  nixpkgs,
  home-manager,
  alejandra,
  emacs-community,
  ...
}: let
  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      (import ../personal-flake/elangley-overlay)
    ];
  };
  emacs-pkgs = emacs-community.packages.${system};
  common_home = import ../common.nix {inherit pkgs;};
  dotfileDirectory = "${homeDirectory}/git_repos/dotfiles";
  homeDirectory = "/home/${username}";
  lisps = with pkgs; [
    ccl
    ecl
    #gcl
    cmucl_binary
    nixpkgs-fmt
  ];
  openssl = pkgs.openssl.overrideAttrs (oldAttrs: {meta = oldAttrs.meta // {outputsToInstall = oldAttrs.meta.outputsToInstall or ["out"] ++ ["dev"];};});
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
    ++ lisps;
  username = "edwlan";
  utils = common_home.utils;
in
  home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      self.homeManagerModules.common
      self.homeManagerModules.fonts
      {
        home = {
          inherit username homeDirectory packages;
          file = {
            "sbcl-source".source = utils.untar pkgs.sbcl.src;
          };
        };
        programs = {
          direnv = {
            enable = true;
            nix-direnv = {enable = true;};
          };
          password-store = {
            enable = false;
            settings = {
              PASSWORD_STORE_DIR = "/some/directory";
              PASSWORD_STORE_KEY = "12345678";
              PASSWORD_STORE_CLIP_TIME = "60";
            };
          };
          tmux = {
            enable = true;
            terminal = "screen-256color";
            escapeTime = 0;
            clock24 = true;
            newSession = true;
            keyMode = "vi";
            extraConfig = builtins.readFile ../../tmux.conf;
          };
        };
        targets.genericLinux.enable = true;
      }
      self.homeManagerModules.git-config
      (import ./email.nix)
      {home.stateVersion = "21.03";}
    ];
    extraSpecialArgs = {
      inherit system;
      fwoar-pkgs = self.packages.${system};
      emacs-pkgs = emacs-community.packages.${system};
      alejandra-pkgs = alejandra.packages.${system};
    };
  }
