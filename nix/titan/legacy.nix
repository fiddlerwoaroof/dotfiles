{
  pkgs,
  emacs-pkgs,
  ...
}: let
  openssl = pkgs.openssl.overrideAttrs (oldAttrs: {meta = oldAttrs.meta // {outputsToInstall = oldAttrs.meta.outputsToInstall or ["out"] ++ ["dev"];};});
  username = "edwlan";
  common_home = import ../common.nix {inherit pkgs;};
  utils = common_home.utils;
in {
  home = {
    inherit username;
    homeDirectory = "/home/${username}";

    file = {
      "sbcl-source".source = utils.untar pkgs.sbcl.src;
    };
  };
  programs = {
    direnv = {
      enable = true;
      nix-direnv = {enable = true;};
    };
    password-store = {
      enable = false;
      settings = {
        PASSWORD_STORE_DIR = "/some/directory";
        PASSWORD_STORE_KEY = "12345678";
        PASSWORD_STORE_CLIP_TIME = "60";
      };
    };
    tmux = {
      enable = true;
      terminal = "screen-256color";
      escapeTime = 0;
      clock24 = true;
      newSession = true;
      keyMode = "vi";
      extraConfig = builtins.readFile ../../tmux.conf;
    };
  };
}
