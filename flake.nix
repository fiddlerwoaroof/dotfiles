{
  inputs = {
    titan-nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    ollama-nixpkgs.url = "github:NixOS/nixpkgs/ac4dd85979ee6eeac9a5f7aa95534f667a26e980";
    alejandra = {
      url = "github:kamadorueda/alejandra";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-community = {url = "github:nix-community/emacs-overlay";};
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-editor = {url = "github:snowfallorg/nix-editor";};
    nixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };
    emacs-hack = {url = "github:fiddlerwoaroof/emacs-nix-hack";};
    sops-nix.url = "github:Mic92/sops-nix";
  };

  outputs = {
    self,
    alejandra,
    emacs-community,
    home-manager,
    nixpkgs,
    sops-nix,
    titan-nixpkgs,
    ...
  } @ inputs: let
    withSystem = system: attrSet: attrSet // {inherit system;};
    withAppleSilicon = withSystem "aarch64-darwin";
    withx8664Linux = withSystem "x86_64-linux";
  in {
    packages = import ./nix/packages inputs;
    homeManagerModules = {
      common = import ./nix/common-module.nix;
      fonts = {pkgs, ...}: {
        home.packages = [
          pkgs.lato
          pkgs.alegreya
          pkgs.source-code-pro
          pkgs.alegreya-sans
        ];
      };
      git-config = import ./nix/git-config.nix;
      mac-apps = import ./nix/mac-apps;
      main = import ./nix/personal-flake/home.nix;
    };
    homeConfigurations = {
      "ouranos" = import ./nix/ouranos/home.nix (withAppleSilicon inputs);
      "titan" = import ./nix/titan/home.nix (withx8664Linux inputs);
    };
    apps.aarch64-darwin = let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
      self-pkgs = self.packages.${system};
      writeZsh = pkgs.writers.makeScriptWriter {interpreter = "${pkgs.zsh}/bin/zsh";};
    in {
      cls = {
        type = "app";
        buildInputs = [self-pkgs.cls];
        program = "${self-pkgs.cls}/bin/cls";
      };
    };
    nixosConfigurations.titan = titan-nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        {
          nix.registry.nixpkgs.flake = titan-nixpkgs;
          nix.nixPath = [
            "nixpkgs=/etc/channels/nixpkgs"
            "nixos-config=/etc/nixos/configuration.nix"
            "/nix/var/nix/profiles/per-user/root/channels"
          ];
          environment.etc."channels/nixpkgs".source = inputs.nixpkgs.outPath;
        }
        (let pkgs = nixpkgs.legacyPackages.x86_64-linux; in
        {
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
    };
  };
}
