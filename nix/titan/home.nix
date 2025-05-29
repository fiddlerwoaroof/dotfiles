{
  alejandra,
  emacs-community,
  home-manager,
  nixpkgs,
  self,
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
      self.homeManagerModules.common
      self.homeManagerModules.fonts
      (import ./legacy.nix)
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
      self.homeManagerModules.git-config
      (import ./email.nix)
      {
        home.file = {
          ".vimrc".source = ../../vimrc_work;
          ".vim/repos/github.com/Shougo/dein.vim".source = (pkgs.fetchFromGitHub {
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
