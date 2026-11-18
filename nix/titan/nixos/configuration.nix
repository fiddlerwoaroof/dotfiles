# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # nix.settings, documentation, base packages, the edwlan user, zsh/vim/nix-ld,
  # the hardened sshd and locale/timezone all come from self.nixosModules.base.

  fileSystems = {
    "/" = {
      device = "zpool/root";
      fsType = "zfs";
      options = ["zfsutil"];
    };

    "/nix" = {
      device = "zpool/nix";
      fsType = "zfs";
      options = ["zfsutil"];
    };

    "/var" = {
      device = "zpool/var";
      fsType = "zfs";
      options = ["zfsutil"];
    };

    "/home" = {
      device = "zpool/home";
      fsType = "zfs";
      options = ["zfsutil"];
    };

    "/home/edwlan/Maildir" = {
      device = "zpool/home/edwlan/Maildir";
      fsType = "zfs";
      options = ["zfsutil"];
      depends = ["/home"];
    };

    #"/home/edwlan/oldhome" = {
    #  device = "/dev/disk/by-uuid/e99a31c0-e201-4f6f-8b9b-9d856841ae99";
    #  fsType = "ext4";
    #  options = ["discard"];
    #  depends = ["/home"];
    #};
    #
    #"/home/edwlan/Langleys\ Dropbox" = {
    #  device = "/home/edwlan/oldhome/edwlan/Langleys\040Dropbox";
    #  options = ["bind"];
    #  depends = ["/home/edwlan/oldhome" "/home"];
    #};

    "/boot" = {
      device = "/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_2TB_S7KHNJ0X715552N-part1";
      fsType = "vfat";
      options = ["fmask=0022" "dmask=0022"];
    };
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_6_18;
    kernelParams = [
      "zfs.zfs_arc_max=12884901888"
      "amdgpu.vm_update_mode=3"
      "amdgpu.ppfeaturemask=0xffffffff"
    ];
    loader.systemd-boot.enable = true;
    zfs = {
      devNodes = "/dev/disk/by-id";
      extraPools = ["zpool"];
      forceImportRoot = false;
    };
  };

  # Desktop/hardware packages specific to titan; the base tooling lives in
  # self.nixosModules.base.
  environment.systemPackages = with pkgs; [
    blueman
    bluez
  ];

  networking = {
    hostId = "8425e349";
    hostName = "titan"; # Define your hostname.
    domain = "h.elangley.org";
    # Open ports in the firewall.
    firewall.allowedTCPPorts = [22 11434];
  };

  services = {
    blueman = {enable = true;};
    desktopManager.plasma6.enable = true;
    displayManager = {
      defaultSession = "plasma";
      sddm.enable = true;
      sddm.wayland.enable = false;
    };
    gitolite = {
      enable = true;
      adminPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP3Dt99tfZLxBQcLa/shJA12+aw5fNClP0ZzwPCto3lU f@localhost";
      extraGitoliteRc = ''
        # setup gitolite mirror
        $RC{GIT_CONFIG_KEYS} = 'gitolite\.mirror\..*';
      '';
      commonHooks = [../gitolite-hooks/post-receive];
    };
    libinput.enable = true;
    # openssh (hardened, with the trusted SSH CA) is provided by
    # self.nixosModules.base.
    xserver.enable = true;
    zfs = {
      trim.enable = true;
      autoScrub.enable = true;
    };
  };

  programs = {
    firefox.enable = true;
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  #### expose tailscale
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
  };
  networking.firewall.extraForwardRules = ''
    iifname "enp5s0" oifname "tailscale0" ip saddr 172.16.0.0/16 accept
      iifname "tailscale0" oifname "enp5s0" ct state { established, related } accept
  '';
  networking.nftables.tables.lan-to-tailnet = {
    family = "ip";
    content = ''
      chain postrouting {
          type nat hook postrouting priority srcnat; policy accept;
          ip saddr 172.16.0.0/16 oifname "tailscale0" masquerade
      }
    '';
  };
  systemd.services.tailscaled.environment.TS_DEBUG_FIREWALL_MODE = "nftables";

  # This option defines the first version of NixOS you have installed on this
  # particular machine, and is used to maintain compatibility with application
  # data (e.g.  databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for
  # any reason, even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are
  # pulled from, so changing it will NOT upgrade your system - see
  # https://nixos.org/manual/nixos/stable/#sec-upgrading for how to actually do
  # that.
  #
  # This value being lower than the current NixOS release does NOT mean your
  # system is out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes
  # it would make to your configuration, and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or
  # https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.05"; # Did you read the comment?
}
