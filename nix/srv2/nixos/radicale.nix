{config, ...}: {
  sops.secrets.radicale_passwords = {
    mode = "400";
    owner = config.users.users.radicale.name;
    group = config.users.users.radicale.group;
    restartUnits = ["radicale.service"];
  };

  services.radicale = {
    enable = true;
    settings = {
      server.hosts = ["0.0.0.0:5232"]; # or 127.0.0.1 behind a TLS proxy
      auth = {
        type = "htpasswd";
        htpasswd_filename = config.sops.secrets.radicale_passwords.path;
        htpasswd_encryption = "plain";
      };
    };
    # Sections are emitted in key order; first match wins.
    rights = {
      "00-root" = {
        user = ".+";
        collection = "";
        permissions = "R";
      };
      "10-principal" = {
        user = ".+";
        collection = "{user}";
        permissions = "RW";
      };
      "20-calendars" = {
        user = ".+";
        collection = "{user}/[^/]+";
        permissions = "rw";
      };
      "30-concerts-public" = {
        user = ".*"; # matches "" (anonymous) too
        collection = "grok_bot/concerts";
        permissions = "r";
      };
    };
  };
}
