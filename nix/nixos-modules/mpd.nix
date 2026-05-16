{
  config,
  pkgs,
  ...
}: {
  # AirPlay/RAOP discovery
  environment.systemPackages = [
    pkgs.mpc
    pkgs.ncmpcpp
  ];
  services = {
    avahi.enable = true;
    pipewire = {
      raopOpenFirewall = true; # opens UDP 6001-6002
      extraConfig.pipewire."10-airplay" = {
        "context.modules" = [
          {name = "libpipewire-module-raop-discover";}
        ];
      };
    };

    mpd = {
      enable = true;
      user = "edwlan";

      musicDirectory = "/home/edwlan/oldhome/sorted_music";
      extraConfig = ''
        audio_output {
          type "pipewire"
          name "PipeWire"
        }
      '';
      # Optional:
      network.listenAddress = "any"; # if you want to allow non-localhost connections
      #startWhenNeeded = true;
      # systemd feature: only start MPD service upon connection to its socket
    };
  };
  systemd.services.mpd.environment = {
    # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/609
    XDG_RUNTIME_DIR = "/run/user/1000"; # User-id must match above user. MPD will look inside this directory for the PipeWire socket.
  };
}
