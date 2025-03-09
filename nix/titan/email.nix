{pkgs, ...}: let
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
  syncMailNotArchive = pkgs.writeScript "sync-mail-not-archive" ''
    #!${pkgs.zsh}/bin/zsh

    mailboxes=()

    mbsync -l personal | grep -v '^Archive' | while read -r; do
      mailboxes=("''${mailboxes[@]}" "personal:$REPLY");
    done

    mbsync -L -H --full "''${mailboxes[@]}"
  '';
in {
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
  };
}
