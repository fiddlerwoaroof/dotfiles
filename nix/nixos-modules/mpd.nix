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
      openFirewall = true;

      settings = {
        bind_to_address = "any";
        music_directory = "/home/edwlan/oldhome/sorted_music";

        audio_output = [
          {
            type = "pipewire";
            name = "PipeWire";
          }
        ];
      };
    };
  };
  systemd.services.mpd.environment = {
    # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/609
    XDG_RUNTIME_DIR = "/run/user/1000"; # User-id must match above user. MPD will look inside this directory for the PipeWire socket.
  };
}
