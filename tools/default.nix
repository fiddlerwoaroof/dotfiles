{nixpkgs, ...}: let
  asdf-dependencies = builtins.fromJSON (builtins.readFile ./asdf-dependencies.json);
  getAsdfDependencies = s: p: builtins.map (s: builtins.getAttr s p) asdf-dependencies.${s};
  mkTool = {
    name,
    system-name,
    system,
  }: let
    pkgs = nixpkgs.legacyPackages.${system};
    sbcl = pkgs.sbcl.withPackages (getAsdfDependencies system-name);
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
      system-name = "fwoar-tools/${name}";
    };
    cls = mkTool rec {
      inherit system;
      name = "cls";
      system-name = "fwoar-tools/${name}";
    };
    git-pick-patch = mkTool rec {
      inherit system;
      name = "git-pick-patch";
      system-name = "fwoar-tools/${name}";
    };
    json-formatter = mkTool rec {
      inherit system;
      name = "json-formatter";
      system-name = "fwoar-tools/${name}";
    };
  }
