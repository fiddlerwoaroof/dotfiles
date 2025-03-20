{nixpkgs, ...}: let
  asdf-dependencies = builtins.fromJSON (builtins.readFile ./asdf-dependencies.json);
  getAsdfDependencies = s: p: builtins.map (s: builtins.getAttr s p) asdf-dependencies.${s};
  mkTool = {
    name,
    system,
  }: let
    pkgs = nixpkgs.legacyPackages.${system};
    sbcl = pkgs.sbcl.withPackages (getAsdfDependencies "fwoar-tools/${name}");
  in
    pkgs.stdenv.mkDerivation {
      inherit system name;
      src = ./.;
      builder = ../build.sh;
      dontStrip = true;
      buildInputs = [
        pkgs.makeWrapper
        pkgs.openssl.dev
        sbcl
        pkgs.which
        pkgs.zsh
      ];
    };
in
  system: {
    zenburn = mkTool rec {
      inherit system;
      name = "zenburn";
    };
    cls = mkTool rec {
      inherit system;
      name = "cls";
    };
    git-pick-patch = mkTool rec {
      inherit system;
      name = "git-pick-patch";
    };
    json-formatter = mkTool rec {
      inherit system;
      name = "json-formatter";
    };
  }
