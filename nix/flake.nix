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
      url = "github:kamadorueda/alejandra";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-community = {
      url = "github:nix-community/emacs-overlay";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    alejandra,
    emacs-community,
    ...
  }: let
    system = "aarch64-darwin";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        (import ./personal-flake/elangley-overlay)
        emacs-community.overlay
      ];
    };
  in {
    defaultPackage.aarch64-darwin = pkgs.mkShell {
      buildInputs = [
        alejandra.outputs.packages.${system}.default
      ];
    };
  };
}
