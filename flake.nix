{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      ref = "nixos-24.11";
    };
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    mkDerivation = system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      zenburn = let
        sbcl = pkgs.sbcl.withPackages (ps:
          with ps; [
            alexandria
            dufy
            net_dot_didierverna_dot_clon
            net_dot_didierverna_dot_clon_dot_termio
            serapeum
            #uiop
          ]);
      in
        pkgs.stdenv.mkDerivation {
          inherit system;
          name = "zenburn";
          src = ./tools;
          builder = ./build.sh;
          dontStrip = true;
          buildInputs = [
            pkgs.makeWrapper
            pkgs.openssl.dev
            sbcl
            pkgs.which
            pkgs.zsh
          ];
        };
    };
  in {
    packages."aarch64-darwin" = mkDerivation "aarch64-darwin";
    packages."aarch64-linux" = mkDerivation "aarch64-linux";
    packages."x86_64-linux" = mkDerivation "x86_64-linux";
  };
}
