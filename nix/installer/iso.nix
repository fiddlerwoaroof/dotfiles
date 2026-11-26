# A NixOS installer ISO for bringing up new machines like titan. It layers the
# fwoar base profile (tooling, nix settings, the edwlan user that trusts the SSH
# CA out of the box) on top of the minimal installation CD, and adds ZFS support
# since titan is ZFS-on-root.
{
  modulesPath,
  lib,
  pkgs,
  ...
}: {
  imports = [
    (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix")
  ];

  # nixpkgs comes from base (pinned to the dotfiles input, channels disabled);
  # don't also bundle and seed a stateful "nixos" channel.
  system.installer.channel.enable = false;

  # titan uses ZFS on root; the installer needs to be able to create/import it.
  boot.supportedFilesystems = ["zfs"];
  boot.zfs.forceImportRoot = false;
  # ZFS refuses to import without a hostId. This is only the live-image id; the
  # installed system sets its own in its configuration.
  networking.hostId = "0a1b2c3d";

  # Give the base user a password so you can reach a console on the live image.
  # (SSH is cert-only via the trusted CA from the base profile.)
  users.users.edwlan.initialPassword = lib.mkForce "nixos";

  # The installation-device profile and the base profile both set these at the
  # default priority; on the live image keep the installer's permissive values.
  services.openssh.settings.PermitRootLogin = lib.mkForce "yes";

  # Handy tools for partitioning and installing.
  environment.systemPackages = with pkgs; [
    parted
    gptfdisk
    cryptsetup
    rsync
    tmux
  ];

  # The written ISO is named "${image.baseName}.iso"; baseName is the knob that
  # actually renames the file (image.fileName/isoName do not drive it). Tag it
  # with the architecture so the x86_64 and aarch64 images are distinguishable.
  image.baseName = lib.mkForce "fwoar-installer-${pkgs.stdenv.hostPlatform.system}";
}
