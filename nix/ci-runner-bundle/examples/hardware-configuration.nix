# PLACEHOLDER, NOT A HARDWARE CONFIGURATION FOR YOUR MACHINE.
# Replace with the host's existing hardware configuration. Review bootloader,
# disk UUIDs, filesystems, initrd modules, host platform and swap configuration.
{...}: {
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };
}
