import ./default.nix {
  pkgs =
    import <nixpkgs> {crossSystem = {config = "aarch64-apple-darwin";};};
}
