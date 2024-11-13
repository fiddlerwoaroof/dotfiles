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
  home.file.".ssh/allowed_signers".text = "* ${builtins.readFile ./id_ed25519.pub}";

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
    pkgs.emacs-git
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
    git = {
      enable = true;
      userEmail = "el-github@elangley.org";
      userName = "Edward Langley";
      lfs.enable = true;
      difftastic.enable = true;
      extraConfig = {
        commit = {gpgsign = true;};
        github = {user = "fiddlerwoaroof";};
        gpg = {
          format = "ssh";
          allowedSignersFile = "${homeDirectory}/.ssh/allowed_signers";
        };
        init = {defaultBranch = "main";};
        merge = {autoStash = true;};
        pull = {rebase = false;};
        rebase = {autoStash = true;};
        user = {signingkey = "${homeDirectory}/.ssh/id_ed25519.pub";};
      };
    };
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
