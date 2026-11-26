{pkgs, ...}: {
  boot = {
    supportedFilesystems = {
      zfs = true;
    };
    zfs = {
      extraPools = ["tank"];
      forceImportRoot = false;
    };
  };
  services.zfs.autoScrub.enable = true;

  # Use latest ZFS-supporting kernel.
  boot.kernelPackages = pkgs.linuxPackages_6_12;
}
