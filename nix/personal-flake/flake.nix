{
  description = "Home Manager configuration of edwlan";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    alejandra = {
      url = "github:kamadorueda/alejandra/3.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-community = {
      url = "github:nix-community/emacs-overlay";
    };
  };

  outputs = {
    nixpkgs,
    home-manager,
    alejandra,
    emacs-community,
    ...
  }: let
    system = "aarch64-darwin";
    common_home =
      import ./common.nix {inherit pkgs;};
    extraOverlay = self: super: {
      alejandra = alejandra.defaultPackage.${system};
    };
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        (import ./elangley-overlay)
        emacs-community.overlay
        extraOverlay
      ];
    };
  in {
    defaultPackage.aarch64-darwin = pkgs.mkShell {
      buildInputs = [
        pkgs.alejandra
      ];
    };
    homeConfigurations."edwlan" = home-manager.lib.homeManagerConfiguration {
      pkgs = pkgs;

      # Specify your home configuration modules here, for example,
      # the path to your home.nix.
      modules = [./home.nix];

      # Optionally use extraSpecialArgs
      # to pass through arguments to home.nix
    };
  };
}
