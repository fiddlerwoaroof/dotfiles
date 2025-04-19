{nixpkgs, ...} @ inputs: let
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
  tools = import ../../tools inputs;
  mkTools = system:
    {
      mycurl = mkCurl system;
      gsed = mkPrefixedSed system;
    }
    // tools system;
  darwinPackages = system:
    (mkTools system)
    // {
      clasp-cl = nixpkgs.legacyPackages.x86_64-darwin.clasp-common-lisp;
      iterm2 = let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        pkgs.callPackage ./iterm.nix {
          version = "3.5.13";
          hash = "sha256-PoHgAE2yooQivAXfDweeQW0KmS32XuxrXjlLIsigh+o=";
        };
    };
in
  builtins.mapAttrs (system: f: f system) {
    "aarch64-darwin" = darwinPackages;
    "aarch64-linux" = mkTools;
    "x86_64-linux" = mkTools;
  }
