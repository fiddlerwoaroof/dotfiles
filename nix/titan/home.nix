{
  config,
  pkgs,
  ...
}: let
  common_home = import "${homeDirectory}/git_repos/dotfiles/nix/common.nix" {
    inherit homeDirectory pkgs;
  };

  lisps = with pkgs; [
    ccl
    #clisp-tip
    ecl
    gcl
    cmucl_binary
    nixpkgs-fmt
  ];

  openssl = pkgs.openssl.overrideAttrs (oldAttrs: {
    meta =
      oldAttrs.meta
      // {
        outputsToInstall =
          oldAttrs.meta.outputsToInstall
          or ["out"]
          ++ ["dev"];
      };
  });

  username = "edwlan";
  homeDirectory = "/home/${username}";
  dotfileDirectory = "${homeDirectory}/git_repos/dotfiles";
  utils = common_home.utils;

  packages =
    common_home.packages
    ++ ([
        pkgs.cachix
        pkgs.pass
        pkgs.direnv
        pkgs.emacs-git
        pkgs.glibcLocales
        pkgs.gron
        pkgs.libssh2
        pkgs.lorri
        pkgs.zeromq
        pkgs.sqlite.dev
        pkgs.sqlite
        openssl
        (import (dotfileDirectory + "/nix/lpass-nix") {inherit pkgs;})
      ]
      ++ lisps);

  syncMailNotArchive = pkgs.writeScript "sync-mail-not-archive" ''
    #!${pkgs.zsh}/bin/zsh

    mailboxes=()

    mbsync -l personal | grep -v '^Archive' | while read -r; do
      mailboxes=("''${mailboxes[@]}" "personal:$REPLY");
    done

    mbsync -L -H --full "''${mailboxes[@]}"
  '';

  notmuchTag = pkgs.writeScript "notmuch-tag" ''
    #!${pkgs.zsh}/bin/zsh
    PATH=${pkgs.notmuch}/bin:$PATH
    notmuch tag -inbox -- tag:inbox AND NOT 'folder:"personal/Inbox"'
    notmuch tag +attend -- "folder:\"personal/Inbox/Attention Needed\" and -tag:attend"
    notmuch tag +receipt -- "folder:\"personal/Inbox/Receipts\" and -tag:receipt"
    notmuch tag +main -- "folder:\"personal/Inbox\" and -tag:main"
    notmuch tag +wine -main -inbox -- "(from:\"wine enthusiast\" OR to:\"el-gordons@elangley.org\" OR from:\"garagiste\") and -tag:wine"
    notmuch tag +travel -main -inbox -- "from:\"priceline\" -tag:travel"
    notmuch tag +shopping -main -inbox -- "(from:nordstrom OR from:\"Julianna Rae\" OR from:\"D'artagnan\" OR from:\"brightcellars.com\" OR from:\"shoppremiumoutlets.com\") -tag:shopping"
    notmuch tag +clutter -main -inbox -- "(from:\"newsletter@reply.canvasonsale.com\" OR from:\"Adobe Special\") -tag:clutter"
    notmuch tag +news -main -inbox -- "(from:Bloomberg OR from:\"The Epoch Times\" OR from:\"Grassfire\") -tag:news"
    notmuch tag +archive -inbox -main -- "folder:/personal.Archive.*/ and -tag:archive"
    notmuch tag +cigars -inbox -- "folder:personal/cigars and -tag:cigars"
    notmuch tag +sunpower -inbox -- "from:sunpower and -tag:sunpower"
  '';
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  nixpkgs.overlays = common_home.overlays;

  home = {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    inherit username homeDirectory packages;

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    stateVersion = "21.03";

    file = {
      "sbcl-source".source = utils.untar pkgs.sbcl.src;
      ".ssh/allowed_signers".text = "* ${builtins.readFile ./id_ed25519.pub}";
    };
  };

  targets.genericLinux.enable = true;

  accounts = {
    email = {
      accounts = {
        personal = {
          address = "edward@elangley.org";
          imap = {host = "mb.elangley.org";};
          mbsync = {
            enable = true;
            create = "maildir";
            patterns = ["*" "!tmp" "!dovecot" "!dovecot/%"];
          };
          msmtp = {enable = true;};
          notmuch = {enable = true;};
          neomutt.enable = true;
          primary = true;
          realName = "Edward Langley";
          passwordCommand = "mail-password";
          smtp = {
            host = "mb.elangley.org";
            port = 587;
            tls.useStartTls = true;
          };
          userName = "edward@howit.is";
        };
      };
    };
  };
  programs = {
    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
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
    mbsync = {enable = true;};
    msmtp = {enable = true;};
    notmuch = {
      enable = true;
      hooks = {
        preNew = "${syncMailNotArchive}";
        postNew = "${notmuchTag}";
      };
      extraConfig = {index = {"header.dt" = "Delivered-To";};};
    };
    neomutt.enable = true;

    password-store = {
      enable = false;
      settings = {
        PASSWORD_STORE_DIR = "/some/directory";
        PASSWORD_STORE_KEY = "12345678";
        PASSWORD_STORE_CLIP_TIME = "60";
      };
    };
  };
}
