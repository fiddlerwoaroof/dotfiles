{
  lib,
  pkgs,
  config,
  ...
}: let
  profileInfoDir = "${config.home.homeDirectory}/.nix-profile/share/info";
in {
  # ensure texinfo-interactive (provides install-info) is in the user profile
  home.packages = [
    pkgs.texinfoInteractive
  ];

  # # Ensure INFOPATH contains the profile info dir (useful if other tools read INFOPATH)
  # environment.variables.INFOPATH = ''
  #   ${pkgs.texinfo-interactive}/share/info:${profileInfoDir}:/usr/local/share/info:/usr/share/info:$INFOPATH
  # '';

  # Rebuild the "dir" index at activation time so Emacs/info can find the top-level node.
  home.activation.rebuildInfoDir = lib.hm.dag.entryAfter ["linkGeneration"] ''
    mkdir -p "$HOME"/info
    BIN_INSTALL_INFO=${pkgs.texinfoInteractive}/bin/install-info

    if [[ ! -x "$BIN_INSTALL_INFO" ]]; then
      echo "warning: install-info not found at $BIN_INSTALL_INFO"
    fi

    for f in "${profileInfoDir}"/*.info "${profileInfoDir}"/*.info.gz; do
      [[ -e "$f" ]] || continue
      "$BIN_INSTALL_INFO" --dir-file="$HOME/info/dir" "$f" || true
    done
    if [[ -e "$HOME/info/dir" ]]; then
      gzip -f "$HOME/info/dir"
    fi
  '';
}
