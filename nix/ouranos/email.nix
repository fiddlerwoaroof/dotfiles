{pkgs, ...}: {
  accounts = {
    email = {
      accounts = {
        personal = {
          address = "edward@elangley.org";
          imap = {host = "mb.elangley.org";};
          #msmtp = {enable = true;};
          neomutt.enable = false;
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
    #msmtp = {enable = true;};
    neomutt = {
      package = pkgs.neomutt.override {withNotmuch = false;};
      enable = true;
    };
  };
}
