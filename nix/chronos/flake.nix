{
  description = "Home Manager config for personal laptop";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
  }: let
    system = "aarch64-darwin";
  in {
    homeConfigurations = (
      import ./home.nix {
        inherit system home-manager;
        pkgs = nixpkgs;
      }
    );
  };
}
