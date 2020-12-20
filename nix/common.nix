{ pkgs }:
let
  packages = extras:
    (with pkgs; [ dtach nixpkgs-fmt nixfmt glibcLocales ]) ++ extras;

  utils = {
    untar = path:
      pkgs.runCommand "untar" { buildInputs = [ pkgs.gnutar ]; } ''
        mkdir -p "$out"
        cd "$out"
        tar --strip-components=1 -xf "${path}"
      '';
  };
in
{ inherit packages utils; }
