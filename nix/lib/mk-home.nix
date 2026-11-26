# Shared home-manager configuration builder: one nixpkgs import with the
# personal overlay, the modules every machine uses, and the common special args.
{
  self,
  alejandra,
  emacs-community,
  home-manager,
  nixpkgs,
  sops-nix,
  ...
}: {
  system,
  modules,
  username ? "edwlan",
  # Package names allowed despite unfree licenses.
  unfree ? [],
  extraSpecialArgs ? {},
}: let
  lib = nixpkgs.lib;
  isDarwin = lib.hasSuffix "-darwin" system;
  pkgs = import nixpkgs ({
      inherit system;
      overlays = [(import ../personal-flake/elangley-overlay)];
    }
    // lib.optionalAttrs (unfree != []) {
      config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) unfree;
    });
in
  home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules =
      [
        sops-nix.homeManagerModules.sops
        self.homeManagerModules.common
        self.homeManagerModules.pin-nixpkgs
        self.homeManagerModules.git-config
        self.homeManagerModules.gpg
        self.homeManagerModules.fonts
        self.homeManagerModules.personal-module-configs
        {
          home = {
            inherit username;
            homeDirectory =
              if isDarwin
              then "/Users/${username}"
              else "/home/${username}";
          };
        }
      ]
      ++ modules;
    extraSpecialArgs =
      {
        inherit system;
        fwoar-pkgs = self.packages.${system};
        emacs-pkgs = import ../emacs-pkgs.nix {inherit emacs-community system;};
        alejandra-pkgs = alejandra.packages.${system};
      }
      // extraSpecialArgs;
  }
