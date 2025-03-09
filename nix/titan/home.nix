{
  self,
  system,
  nixpkgs,
  home-manager,
  alejandra,
  emacs-community,
  ...
}: let
  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      (import ../personal-flake/elangley-overlay)
    ];
  };
  emacs-pkgs = emacs-community.packages.${system};
  common_home = import ../common.nix {inherit pkgs;};
  dotfileDirectory = "${homeDirectory}/git_repos/dotfiles";
  homeDirectory = "/home/${username}";
  lisps = with pkgs; [
    ccl
    ecl
    #gcl
    cmucl_binary
    nixpkgs-fmt
  ];
  notmuchTag = pkgs.writeScript "notmuch-tag" ''
    #!${pkgs.zsh}/bin/zsh
    PATH=${pkgs.notmuch}/bin:$PATH
    notmuch tag -inbox -- tag:inbox AND NOT 'folder:"personal/Inbox"'
    notmuch tag +attend -- "folder:\"personal/Inbox/Attention Needed\" and -tag:attend"
    notmuch tag +receipt -- "folder:\"personal/Inbox/Receipts\" and -tag:receipt"
    notmuch tag +main -- "folder:\"personal/Inbox\" and -tag:main"
    notmuch tag +gordon -- "to:\"el-gordons@elangley.org\""
    notmuch tag +klwine -- "to:\"el-klwines@elangley.org\""
    notmuch tag +wine -main -inbox -- "(from:\"wine enthusiast\" OR to:\"el-gordons@elangley.org\" OR from:\"garagiste\") and -tag:wine"
    notmuch tag +travel -main -inbox -- "from:\"priceline\" -tag:travel"
    notmuch tag +shopping -main -inbox -- "(from:nordstrom OR from:\"Julianna Rae\" OR from:\"D'artagnan\" OR from:\"brightcellars.com\" OR from:\"shoppremiumoutlets.com\") -tag:shopping"
    notmuch tag +clutter -main -inbox -- "(from:\"newsletter@reply.canvasonsale.com\" OR from:\"Adobe Special\") -tag:clutter"
    notmuch tag +news -main -inbox -- "(from:Bloomberg OR from:\"The Epoch Times\" OR from:\"Grassfire\") -tag:news"
    notmuch tag +archive -inbox -main -- "folder:/personal.Archive.*/ and -tag:archive"
    notmuch tag +cigars -inbox -- "folder:personal/cigars and -tag:cigars"
    notmuch tag +sunpower -inbox -- "from:sunpower and -tag:sunpower"
    notmuch search --output summary --format json date:-1week..today |
        jq -c '[.[] | select(.authors == "Cron Daemon" | not)]' > "$HOME"/public_html/recent.json
  '';
  openssl = pkgs.openssl.overrideAttrs (oldAttrs: {meta = oldAttrs.meta // {outputsToInstall = oldAttrs.meta.outputsToInstall or ["out"] ++ ["dev"];};});
  packages =
    [
      pkgs.ncdu
      (import ../lpass-nix {inherit pkgs;})
      emacs-pkgs.emacs-git
      openssl
      pkgs.awscli
      pkgs.cachix
      pkgs.curl
      pkgs.cvs
      pkgs.direnv
      pkgs.glibcLocales
      pkgs.gron
      pkgs.libssh2
      pkgs.lorri
      pkgs.nix
      pkgs.pass
      pkgs.sqlite
      pkgs.sqlite.dev
      pkgs.sqlite.out
      pkgs.visidata
      pkgs.zeromq
    ]
    ++ lisps;
  syncMailNotArchive = pkgs.writeScript "sync-mail-not-archive" ''
    #!${pkgs.zsh}/bin/zsh

    mailboxes=()

    mbsync -l personal | grep -v '^Archive' | while read -r; do
      mailboxes=("''${mailboxes[@]}" "personal:$REPLY");
    done

    mbsync -L -H --full "''${mailboxes[@]}"
  '';
  username = "edwlan";
  utils = common_home.utils;
in
  home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      self.homeManagerModules.common
      self.homeManagerModules.fonts
      {
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
        home = {
          inherit username homeDirectory packages;
          file = {
            "sbcl-source".source = utils.untar pkgs.sbcl.src;
            ".ssh/allowed_signers".text = "* ${builtins.readFile ./id_ed25519.pub}";
            #"lib/libsqlite.so" = "${pkgs.sqlite}/lib/libsqlite.so";
          };
        };
        programs = {
          direnv = {
            enable = true;
            nix-direnv = {enable = true;};
          };
          home-manager = {enable = true;};
          mbsync = {enable = true;};
          msmtp = {enable = true;};
          neomutt = {enable = true;};
          notmuch = {
            enable = true;
            hooks = {
              preNew = "${syncMailNotArchive}";
              postNew = "${notmuchTag}";
            };
            extraConfig = {index = {"header.dt" = "Delivered-To";};};
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
        targets.genericLinux.enable = true;
      }
      {
        programs.git = {
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
      }
      {home.stateVersion = "21.03";}
    ];
    extraSpecialArgs = {
      inherit system;
      fwoar-pkgs = self.packages.${system};
      emacs-pkgs = emacs-community.packages.${system};
      alejandra-pkgs = alejandra.packages.${system};
    };
  }
