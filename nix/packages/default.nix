{nixpkgs, ...}: let
  mkTool = {
    name,
    lispDeps,
  }: system: let
    pkgs = nixpkgs.legacyPackages.${system};
    sbcl = pkgs.sbcl.withPackages lispDeps;
  in
    pkgs.stdenv.mkDerivation {
      inherit system name;
      src = ../../tools;
      builder = ../../build.sh;
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
  mkJsonFormatter = mkTool {
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
  mkPrefixedSed = system: let
    pkgs = nixpkgs.legacyPackages.${system};
  in
    pkgs.callPackage ./prefixed-gnused {};
  mkCurl = system: let
    pkgs = nixpkgs.legacyPackages.${system};
  in
    pkgs.curl.override {
      openssl = pkgs.quictls;
      http3Support = true;
    };
  mkTools = system: {
    zenburn = mkZenburn system;
    cls = mkCls system;
    git-pick-patch = mkGitPickPatch system;
    json-formatter = mkJsonFormatter system;
    mycurl = mkCurl system;
    gsed = mkPrefixedSed system;
  };
  darwinPackages = system:
    (mkTools system)
    // {
      iterm2 = let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        pkgs.callPackage ./iterm.nix {
          version = "3.5.11";
          hash = "sha256-vcZL74U9RNjhpIQRUUn6WueYhE/LfLqpb/JgWunY5dI=";
        };
    };
in
  builtins.mapAttrs (system: f: f system) {
    "aarch64-darwin" = darwinPackages;
    "aarch64-linux" = mkTools;
    "x86_64-linux" = mkTools;
  }
