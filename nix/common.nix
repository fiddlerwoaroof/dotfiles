{pkgs}: rec {
  utils = {
    untar = path:
      pkgs.runCommand "untar" {buildInputs = [pkgs.gnutar];} ''
        mkdir -p "$out"
        cd "$out"
        tar --strip-components=1 -xf "${path}"
      '';
  };

  overlays = [(import ./elangley-overlay) (import ./emacs-overlay.nix)];
}
