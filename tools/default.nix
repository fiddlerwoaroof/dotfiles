{nixpkgs, ...}: let
  asdf-dependencies = builtins.fromJSON (builtins.readFile ./asdf-dependencies.json);
  getAsdfDependencies = s: p: builtins.map (s: builtins.getAttr s p) asdf-dependencies.${s};
  mkTool = {
    name,
    system,
    ...
  } @ rest: let
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
        pkgs.openssl
        sbcl
        pkgs.which
        pkgs.sbclPackages.osicat
        pkgs.zsh
      ];
      postInstall = ''
        OSICAT_LIB="${pkgs.sbclPackages.osicat}/posix"

        wrapProgram $out/bin/${name} \
          --prefix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath [pkgs.openssl]}:$OSICAT_LIB"
      '';
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
    file-indexer = mkTool rec {
      inherit system;
      name = "file-indexer";
    };
    json-formatter = mkTool rec {
      inherit system;
      name = "json-formatter";
    };
    embedding-hash = mkTool rec {
      inherit system;
      name = "embedding-hash";
    };
    ddns-updater = mkTool rec {
      inherit system;
      name = "ddns-updater";
    };
  }
