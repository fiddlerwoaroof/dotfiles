{
  config,
  pkgs,
  ...
}: let
  username = "edwlan";
  homeDirectory = "/Users/${username}";
  dotfileDirectory = "${homeDirectory}/git_repos/dotfiles";

  libpng =
    pkgs.libpng.overrideAttrs (_: {meta.outputsToInstall = _.outputs;});

  zsh =
    pkgs.zsh
    // {
      meta =
        pkgs.zsh.meta
        // {
          outputsToInstall = pkgs.zsh.meta.outputsToInstall ++ ["info" "doc"];
        };
    };
in {
  home.packages = [
    #pkgs.gnumake.info
    libpng
    pkgs.atuin
    pkgs.borgbackup
    pkgs.SDL2
    pkgs.SDL2.dev
    pkgs.cargo
    pkgs.clojure
    pkgs.coreutils-prefixed
    pkgs.difftastic
    pkgs.direnv
    pkgs.dtach
    pkgs.duckdb
    pkgs.ecl
    pkgs.fwoar.gsed
    pkgs.gawk
    pkgs.gnumake
    pkgs.gnuplot
    pkgs.graphviz
    pkgs.groff
    pkgs.htop
    pkgs.imagemagick
    pkgs.texinfoInteractive
    pkgs.ispell
    pkgs.jq
    pkgs.libffi.dev
    pkgs.libheif.dev
    pkgs.libssh2
    pkgs.lorri
    pkgs.mosh
    #pkgs.ncdu
    pkgs.nixfmt-classic
    pkgs.nodejs
    pkgs.openssl
    pkgs.pandoc
    pkgs.pdftk
    pkgs.pkg-config
    pkgs.python311
    pkgs.ripgrep
    (pkgs.sbcl.overrideAttrs (_: {enableFeatures = ["sb-thread" "sb-core-compression" "sb-simd" "sb-xref-for-internals" "sb-after-xc-core" "sb-doc"];}))
    pkgs.shellcheck
    pkgs.texinfoInteractive
    pkgs.tree
    pkgs.vim
    pkgs.visidata
    pkgs.xmlstarlet
    pkgs.zeromq
    pkgs.zsh.doc
    pkgs.zstd
    pkgs.zstd.dev
    zsh
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
