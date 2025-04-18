{
  lib,
  config,
  pkgs,
  emacs-pkgs,
  emacs-hack-pkgs,
  fwoar-pkgs,
  ...
}: let
  emacs = emacs-hack-pkgs.default;
in {
  home.packages = [
    pkgs.crawlTiles
    fwoar-pkgs.iterm2
    emacs
  ];
  home.activation.install-apps = lib.hm.dag.entryAfter ["linkGeneration"] ''
    new_nix_apps="${config.home.homeDirectory}/Applications/Nix"
    rm -rf "$new_nix_apps"
    mkdir -p "$new_nix_apps"
    find -H -L "$genProfilePath/home-files/Applications" -maxdepth 2 -name "*.app" -type d -print | while read -r app; do
      real_app=$(readlink -f "$app")
      app_name=$(basename "$app")
      target_app="$new_nix_apps/$app_name"
      echo "Link '$real_app' to '$target_app'"
      mkdir -p $target_app
      "${pkgs.xorg.lndir}"/bin/lndir "$real_app" "$target_app" 2>&1>/dev/null
      rm "$target_app/Contents/Info.plist"
      cp "$real_app/Contents/Info.plist" "$target_app/Contents/Info.plist"
    done
  '';
}
