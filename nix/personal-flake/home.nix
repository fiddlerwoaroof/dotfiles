{
  config,
  pkgs,
  ...
}: let
  username = "edwlan";
  homeDirectory = "/Users/${username}";
  dotfileDirectory = "${config.home.homeDirectory}/git_repos/dotfiles";

  libpng =
    pkgs.libpng.overrideAttrs (_: {meta.outputsToInstall = _.outputs;});
in {
  home.packages = [
    #pkgs.gnumake.info
    #pkgs.ncdu
    libpng
    pkgs.SDL2
    pkgs.SDL2.dev
    pkgs.atuin
    pkgs.borgbackup
    pkgs.cargo
    pkgs.coreutils-prefixed
    pkgs.duckdb
    pkgs.graphviz
    pkgs.imagemagick
    pkgs.ispell
    pkgs.libffi.dev
    pkgs.libheif.dev
    pkgs.libssh2
    pkgs.mosh
    pkgs.nixfmt-classic
    pkgs.nodejs
    pkgs.openssl
    pkgs.pandoc
    pkgs.pdftk
    pkgs.pkg-config
    pkgs.python311
    pkgs.xmlstarlet
    pkgs.zeromq
    pkgs.zstd
    pkgs.zstd.dev
  ];

  programs = {
    tmux = {
      enable = true;
      terminal = "screen-256color";
      escapeTime = 0;
      clock24 = true;
      newSession = true;
      keyMode = "vi";
      extraConfig = builtins.readFile ./tmux.conf;
    };
  };

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = username;
  home.homeDirectory = homeDirectory;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
