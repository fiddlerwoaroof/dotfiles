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

  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    substituters = [
      "https://cache.nixos.org"
      "https://fwoar-greenhouse.cachix.org"
      "https://hercules-ci.cachix.org"
      "https://latex-sample.cachix.org"
      "https://nix-community.cachix.org"
      "https://siuta.cachix.org"
    ];
    trusted-public-keys = [
      "fwoar-greenhouse.cachix.org-1:OSOY4mJ0TIscoZsGZjX0Y0IFQCjUoBYFQpQdDGGE/iM="
      "fwoar-greenhouse.cachix.org-1:iQ+rkqzBJOebBlwcRyvctq3oZHoDBz5x9qHqWC7n9rg="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
      "latex-sample.cachix.org-1:wyrXgr9qt1PcnWdBbmyMpkRvl0Z1doFe+/ssbzz3OJw="
      "siuta.cachix.org-1:aNWNdZez5khiQkgPeU64P9x4T3DHT5gw6VsPWvAYE20="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    trusted-users = ["root" "edwlan"];
  };

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

    "/home/edwlan/oldhome" = {
      device = "/dev/disk/by-uuid/e99a31c0-e201-4f6f-8b9b-9d856841ae99";
      fsType = "ext4";
      options = ["discard"];
      depends = ["/home"];
    };

    "/home/edwlan/Langleys\ Dropbox" = {
      device = "/home/edwlan/oldhome/edwlan/Langleys\040Dropbox";
      options = ["bind"];
      depends = ["/home/edwlan/oldhome"];
    };

    "/boot" = {
      device = "/dev/disk/by-id/nvme-Samsung_SSD_990_PRO_2TB_S7KHNJ0X715552N-part1";
      fsType = "vfat";
      options = ["fmask=0022" "dmask=0022"];
    };
  };

  boot = {
    loader.systemd-boot.enable = true;
    kernelParams = ["zfs.zfs_arc_max=12884901888"];
    zfs.devNodes = "/dev/disk/by-id";
    zfs.extraPools = ["zpool"];
  };

  services.zfs.trim.enable = true;
  services.zfs.autoScrub.enable = true;
  networking = {
    hostId = "8425e349";
    hostName = "titan"; # Define your hostname.
    domain = "h.elangley.org";
  };
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.edwlan = {
    isNormalUser = true;
    extraGroups = ["wheel"];
  };
  # users.users.alice = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  #   packages = with pkgs; [
  #     tree
  #   ];
  # };

  # programs.firefox.enable = true;

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    file
    git
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.displayManager.defaultSession = "plasmax11";
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = false;
  services.desktopManager.plasma6.enable = true;

  programs.vim.enable = true;
  programs.firefox.enable = true;
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [22];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.05"; # Did you read the comment?
}
