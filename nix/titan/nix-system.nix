{
  self,
  nixpkgs,
  sops-nix,
  home-manager,
  nix-minecraft,
}:
nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    {documentation.info.enable = true;}
    ({lib, ...}: {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = [
        "nixpkgs=/etc/channels/nixpkgs"
        "nixos-config=/etc/nixos/configuration.nix"
        "/nix/var/nix/profiles/per-user/root/channels"
      ];
      environment.etc."channels/nixpkgs".source = nixpkgs.outPath;
      nixpkgs.config.allowUnfreePredicate = pkg:
        builtins.elem (lib.getName pkg) [
          "dropbox"
          "open-webui"
        ];
    })
    self.nixosModules.tailscale
    nix-minecraft.nixosModules.minecraft-servers
    ({pkgs, ...}: let
      mine-port = 25565;
    in {
      nixpkgs.overlays = [nix-minecraft.overlay];
      networking.firewall.allowedTCPPorts = [mine-port];
      services.minecraft-servers = {
        enable = true;
        eula = true;
        dataDir = "/world";
        servers.main = {
          enable = true;
          package = let
            jdk25 = pkgs.jdk25_headless;
          in
            pkgs.paperServers.paper-26_2.override {jre = jdk25;};
          jvmOpts = " -Xms30G -Xmx30G -XX:+UseZGC -XX:+ZGenerational -XX:+AlwaysPreTouch -XX:+DisableExplicitGC -XX:+UseNUMA -XX:+UseLargePages -XX:-OmitStackTraceInFastThrow -XX:+PerfDisableSharedMem -XX:+UseStringDeduplication -XX:+OptimizeStringConcat -XX:+UseCompressedOops -XX:+UseCompressedClassPointers -XX:MaxGCPauseMillis=50 -XX:SoftMaxHeapSize=3584M -XX:ConcGCThreads=2 -XX:ParallelGCThreads=4 -Dterminal.jline=false -Dlog4j2.formatMsgNoLookups=true --add-modules=jdk.incubator.vector -DPaper.IgnoreJavaVersion=true ";

          serverProperties = {
            server-port = mine-port;
            gamemode = "survival";
            motd = "NixOS Minecraft server on Tailscale!";
            max-players = 15;
            online-mode = false;
            enable-rcon = true;
            # This password can be used to administer your minecraft server.
            # Exact details as to how will be explained later. If you want
            # you can replace this with another password.
            "rcon.password" = "hunter2";
          };
        };
      };
    })

    ({pkgs, ...}: {
      services.prometheus.exporters.node = {
        enable = true;
        port = 9100;
        enabledCollectors = ["hwmon" "drm" "systemd"];
        # only allow srv2 to scrape
        openFirewall = true;
        # or manually:
        # firewallFilter = "-i eth0 -s srv2.h.elangley.org -p tcp -m tcp --dport 9100";
      };
    })
    ({pkgs, ...}: {
      environment.systemPackages = [
        pkgs.alejandra
        pkgs.dmidecode
        pkgs.gitFull
        pkgs.htop
        pkgs.lsof
        pkgs.sops
        pkgs.vim
        pkgs.wget
      ];
    })
    ({pkgs, ...}: {
      # In configuration.nix or flake.nix

      environment.systemPackages = with pkgs; [motion];

      # Create motion config file
      environment.etc."motion/motion.conf" = {
        text = ''
          daemon on

          video_device /dev/video0
          video_params -1

          width 640
          height 480
          framerate 3

          picture_quality 80
          picture_output when-motion
          picture_type jpeg
          picture_filename %Y%m%d_%H%M%S-%q


          target_dir /var/lib/motion

          threshold 1500
          threshold_maximum 1800
          despeckle_filter EedDl

          # Video capture settings
          movie_output on
          movie_quality 60
          movie_max_time 0
          movie_timelapse 0

          # Pre/post capture (in frames)
          pre_capture 15
          post_capture 15

          stream_localhost off
          stream_port 8081
        '';
      };

      # Systemd service
      systemd.services.motion = {
        description = "Motion Detection";
        after = ["network.target"];
        wantedBy = ["multi-user.target"];

        preStart = ''
          mkdir -p /var/lib/motion /var/log/motion
          chown motion:motion /var/lib/motion /var/log/motion
        '';

        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.motion}/bin/motion -c /etc/motion/motion.conf -n";
          Restart = "on-failure";
          RestartSec = 10;
          User = "motion";
          Group = "motion";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };

      users.users.motion = {
        isSystemUser = true;
        group = "motion";
        extraGroups = ["video"];

        home = "/var/lib/motion";
      };

      users.groups.motion = {};

      # Dropbox sync timer
      systemd.timers.motion-to-dropbox = {
        wantedBy = ["timers.target"];
        timerConfig = {
          OnBootSec = "2min";
          OnUnitActiveSec = "5min";
          Unit = "motion-to-dropbox.service";
        };
      };

      systemd.services.motion-to-dropbox = {
        description = "Sync motion captures to Dropbox";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.rsync}/bin/rsync -rvh /var/lib/motion/ '/home/edwlan/.dropbox-hm/Langleys Dropbox/Edward Langley/motion/'";
          ExecStartPost = "${pkgs.coreutils}/bin/chown -R edwlan: '/home/edwlan/.dropbox-hm/Langleys Dropbox/Edward Langley/motion/'";
          User = "root";
        };
      };
    })
    ./nixos/ollama.nix
    ./nixos/configuration.nix
    sops-nix.nixosModules.sops
    {
      sops.defaultSopsFile = ../../secrets/titan/main.yaml;
      sops.age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
    }
    {
      services.harmonia = {
        enable = true;
        signKeyPaths = ["/run/secrets/nix_cache_priv_key"];
      };
      networking.firewall.allowedTCPPorts = [5000];
      sops.secrets.nix_cache_priv_key = {
        owner = "edwlan";
      };
    }
    home-manager.nixosModules.home-manager
    self.nixosModules.home-assistant
    {
      home-manager.useGlobalPkgs = false;
      home-manager.useUserPackages = false;
    }
  ];
}
