{config, ...}: {
  sops.defaultSopsFile = ../../secrets/srv2/main.yml
  sops.age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
}
