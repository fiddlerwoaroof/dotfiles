{
  description = "Proxy-only GitHub Actions runners in cloud-hypervisor microVMs with host-side TLS audit logging";
  # Lock and review these inputs before deploying. No fabricated lock file is shipped.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    microvm = {
      url = "github:microvm-nix/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = {
    self,
    nixpkgs,
    microvm,
  }: {
    nixosModules.mitm-proxy = {imports = [./modules/mitm-proxy.nix];};
    nixosModules.microvm-runners = {
      imports = [microvm.nixosModules.host ./modules/microvm-runners.nix];
    };
    nixosModules.default = self.nixosModules.microvm-runners;

    nixosConfigurations.ci-host = nixpkgs.lib.nixosSystem {
      modules = [
        self.nixosModules.default
        ./examples/ci-host.nix
        ./examples/hardware-configuration.nix
      ];
    };

    checks.x86_64-linux = let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      # The sample host build also exercises NixOS's nftables ruleset checker.
      configuration = self.nixosConfigurations.ci-host.config.system.build.toplevel;
      audit-unit-tests =
        pkgs.runCommand "ci-audit-unit-tests" {
          nativeBuildInputs = [pkgs.python3];
        } ''
          cp -r ${./proxy} proxy
          cp -r ${./tests} tests
          export PYTHONDONTWRITEBYTECODE=1
          python -m unittest discover -s tests -v
          touch "$out"
        '';
    };
  };
}
