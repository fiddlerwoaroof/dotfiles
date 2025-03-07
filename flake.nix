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
    mkTool = {
      name,
      lispDeps,
    }: system: let
      pkgs = nixpkgs.legacyPackages.${system};
      sbcl = pkgs.sbcl.withPackages lispDeps;
    in
      pkgs.stdenv.mkDerivation {
        inherit system name;
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
    mkZenburn = mkTool {
      name = "zenburn";
      lispDeps = ps:
        with ps; [
          alexandria
          dufy
          net_dot_didierverna_dot_clon
          net_dot_didierverna_dot_clon_dot_termio
          serapeum
          #uiop
        ];
    };
    mkCls = mkTool {
      name = "cls";
      lispDeps = ps:
        with ps; [
          alexandria
          data-lens
          local-time
          net_dot_didierverna_dot_clon
          net_dot_didierverna_dot_clon_dot_termio
          yason
          #uiop
        ];
    };
    mkGitPickPatch = mkTool {
      name = "git-pick-patch";
      lispDeps = ps:
        with ps; [
          alexandria
          serapeum
          cl-ppcre
        ];
    };
    mkTools = system: {
      zenburn = mkZenburn system;
      cls = mkCls system;
      git-pick-patch = mkGitPickPatch system;
    };
  in {
    packages = builtins.mapAttrs (system: f: f system) {
      "aarch64-darwin" = mkTools;
      "aarch64-linux" = mkTools;
      "x86_64-linux" = mkTools;
    };
  };
}
