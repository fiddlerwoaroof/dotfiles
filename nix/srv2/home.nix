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
  username = "edwlan";
  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      (import ../personal-flake/elangley-overlay)
    ];
  };
in
  home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      sops-nix.homeManagerModules.sops
      self.homeManagerModules.common
      self.homeManagerModules.fonts
      self.homeManagerModules.emacs
      #{
      #  sops = {
      #    age.keyFile = "/home/${username}/.age-key.txt";
      #    defaultSopsFile = ./secrets.json;
      #    secrets.mail-password = {
      #      path = "%r/mail-password.txt";
      #    };
      #  };
      #}
      self.homeManagerModules.direnv
      self.homeManagerModules.tmux
      (import ./programs.nix)
      #(import ./sops.nix)
      self.homeManagerModules.git-config
      #(import ./email.nix)
      {
        home.file = {
          ".vimrc".source = ../../vimrc_work;
          ".vim/repos/github.com/Shougo/dein.vim".source =
            (pkgs.fetchFromGitHub {
              owner = "Shougo";
              repo = "dein.vim";
              rev = "master";
              hash = "sha256-/DmbdiFO1O/fz4biTAynRJ0JgAp8FbY7XMW1oO9kCnM=";
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
        home = {
          inherit username;
          homeDirectory = "/home/${username}";
        };
      }
      {
        #targets.genericLinux.enable = true;
        home.stateVersion = "25.05";
      }
    ];
    extraSpecialArgs = {
      inherit system;
      fwoar-pkgs = self.packages.${system};
      emacs-pkgs = emacs-community.packages.${system};
      alejandra-pkgs = alejandra.packages.${system};
    };
  }
