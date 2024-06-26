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
  sbcl = pkgs.fwoar.sbcl_master;
in {
  home.file.".ssh/allowed_signers".text = "* ${builtins.readFile ./id_ed25519.pub}";

  home.packages = [
    #pkgs.gnumake.info
    libpng
    pkgs.alejandra
    pkgs.atuin
    pkgs.borgbackup
    pkgs.cargo
    pkgs.clojure
    pkgs.coreutils-prefixed
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
    pkgs.ispell
    pkgs.jq
    pkgs.libffi.dev
    pkgs.libheif.dev
    pkgs.libssh2
    pkgs.lorri
    pkgs.mosh
    pkgs.nixfmt-rfc-style
    pkgs.nodejs
    pkgs.openssl
    pkgs.pandoc
    pkgs.pkg-config
    pkgs.pdftk
    pkgs.python311
    pkgs.ripgrep
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
    sbcl
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
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
