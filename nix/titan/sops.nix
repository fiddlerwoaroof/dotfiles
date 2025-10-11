{pkgs, ...}: let
  writeZsh = pkgs.writers.makeScriptWriter {interpreter = "${pkgs.zsh}/bin/zsh";};
in {
  systemd.user = {
    enable = true;
    timers = {
      nm-new = {
        Unit = {
          Description = "Check Mail every fifteen minutes";
          RefuseManualStart = false;
          RefuseManualStop = false;
        };

        Timer = {
          Persistent = false;
          OnBootSec = "5min";
          OnUnitActiveSec = "5min";
          Unit = "nm-new.service";
        };

        Install = {
          WantedBy = ["timers.target"];
        };
      };
    };
    services = {
      nm-new = {
        Unit = {
          Description = "Sync mail with notmuch";
          RefuseManualStart = false;
          RefuseManualStop = true;
        };
        Service = {
          Type = "oneshot";
          ExecStart = writeZsh "nm-new" ''
            set -eu -o pipefail
            export PATH=$PATH:"$HOME"/bin
            ${pkgs.notmuch}/bin/notmuch new
          '';
        };
      };
    };
  };
}
