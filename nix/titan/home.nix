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
in
  home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      self.homeManagerModules.common
      self.homeManagerModules.fonts
      (import ./legacy.nix)
      self.homeManagerModules.git-config
      (import ./email.nix)
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
