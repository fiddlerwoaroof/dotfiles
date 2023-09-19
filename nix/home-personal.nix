{
  config,
  pkgs,
  ...
}: let
  libpng =
    pkgs.libpng.overrideAttrs (_: {meta.outputsToInstall = _.outputs;});
  username = "edwlan";
  homeDirectory = "/Users/${username}";
  dotfileDirectory = "${homeDirectory}/git_repos/dotfiles";

  alejandra =
    (import (builtins.fetchTarball {
      url = "https://github.com/kamadorueda/alejandra/tarball/3.0.0";
      sha256 = "18jm0d5xrxk38hw5sa470zgfz9xzdcyaskjhgjwhnmzd5fgacny4";
    }) {inherit pkgs;})
    .outPath;

  # zsh = pkgs.zsh.overrideAttrs ({postInstall}: {postInstall = ''
  #   make install.info install.html
  #   '' + postInstall})

  common_home =
    import "${dotfileDirectory}/nix/common.nix" {inherit homeDirectory pkgs;};

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
  home.file.".ssh/allowed_signers".text = "* ${builtins.readFile "${homeDirectory}/.ssh/id_ed25519.pub"}";

  nixpkgs.overlays = common_home.overlays;

  ## Doesn't work???
  #xdg.configFile."nixpkgs/overlays".source = ./elangley-overlay;

  home.packages =
    common_home.packages
    ++ [
      alejandra
      pkgs.cargo
      pkgs.clojure
      (pkgs.emacsGit.override {nativeComp = true;})
      pkgs.graphviz
      pkgs.ispell
      pkgs.libffi.dev
      pkgs.libheif.dev
      libpng
      pkgs.libssh2
      pkgs.mosh
      pkgs.nodejs
      pkgs.openssl
      pkgs.pandoc
      pkgs.pkg-config
      pkgs.shellcheck
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
      extraConfig = builtins.readFile (dotfileDirectory + "/tmux.conf");
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
