{
  nixpkgs,
  sops-nix,
  home-manager,
}:
nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = [
        "nixpkgs=/etc/channels/nixpkgs"
        "nixos-config=/etc/nixos/configuration.nix"
        "/nix/var/nix/profiles/per-user/root/channels"
      ];
      environment.etc."channels/nixpkgs".source = inputs.nixpkgs.outPath;
    }
    (let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      services.ollama = {
        enable = true;
        #package = pkgs.ollama.override {acceleration = "rocm";};
        acceleration = "rocm";
        openFirewall = true;
        host = "172.16.31.3";
        loadModels = ["gemma3:4b-it-qat" "nomic-embed-text"];
      };
    })
    ./nix/titan/nixos/configuration.nix
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    {
      home-manager.useGlobalPkgs = false;
      home-manager.useUserPackages = false;
    }
  ];
}
