{
  self,
  alejandra,
  emacs-community,
  home-manager,
  nix-editor,
  nixpkgs,
  emacs-hack,
  system,
  ...
}: let
  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      (import ../personal-flake/elangley-overlay)
    ];
  };
in
  home-manager.lib.homeManagerConfiguration {
    pkgs = pkgs;

    # Specify your home configuration modules here, for example,
    # the path to your home.nix.
    modules = [
      self.homeManagerModules.common
      self.homeManagerModules.main
      self.homeManagerModules.git-config
      self.homeManagerModules.fonts
      self.homeManagerModules.mac-apps
      (import ./email.nix)
      ({
        pkgs,
        nix-editor-pkgs,
        ...
      }: {
        home.packages = [
          nix-editor-pkgs.default
          pkgs.cmake
          pkgs.nasm
          pkgs.ninja
          pkgs.autoconf
          pkgs.automake
          pkgs.autoconf-archive
        ];
      })
      {
        # You can update Home Manager without changing this value. See
        # the Home Manager release notes for a list of state version
        # changes in each release.
        home.stateVersion = "22.05";
        home.packages = [pkgs.aria2];
      }
    ];

    # Optionally use extraSpecialArgs
    # to pass through arguments to home.nix
    extraSpecialArgs = {
      inherit system;
      fwoar-pkgs = self.packages.${system};
      nix-editor-pkgs = nix-editor.packages.${system};
      emacs-pkgs = emacs-community.packages.${system};
      intel-pkgs = nixpkgs.legacyPackages.x86_64-darwin;
      alejandra-pkgs = alejandra.packages.${system};
      emacs-hack-pkgs = emacs-hack.packages.${system};
    };
  }
