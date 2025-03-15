{nixpkgs, ...}: let
  mkTool = {
    name,
    lispDeps,
    system,
  }: let
    pkgs = nixpkgs.legacyPackages.${system};
    sbcl = pkgs.sbcl.withPackages lispDeps;
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
    zenburn = mkTool {
      inherit system;
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
    cls = mkTool {
      inherit system;
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
    git-pick-patch = mkTool {
      inherit system;
      name = "git-pick-patch";
      lispDeps = ps:
        with ps; [
          alexandria
          serapeum
          cl-ppcre
        ];
    };
    json-formatter = mkTool {
      inherit system;
      name = "json-formatter";
      lispDeps = ps:
        with ps; [
          net_dot_didierverna_dot_clon
          net_dot_didierverna_dot_clon_dot_termio
          alexandria
          serapeum
          com_dot_inuoe_dot_jzon
        ];
    };
  }
