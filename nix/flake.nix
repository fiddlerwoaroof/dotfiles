{
  description = "Home Manager configuration of edwlan";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    alejandra = {
      url = "github:kamadorueda/alejandra";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-community = {
      url = "github:nix-community/emacs-overlay";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    alejandra,
    emacs-community,
    ...
  }: let
    system = "aarch64-darwin";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        (import ./personal-flake/elangley-overlay)
        emacs-community.overlay
      ];
    };
  in {
    defaultPackage.aarch64-darwin = pkgs.mkShell {
      buildInputs = [
        alejandra.outputs.packages.${system}.default
      ];
    };
    packages.aarch64-darwin.mycurl = pkgs.curl.override {
      http3Support = true;
      rustlsSupport = true;
      gnutlsSupport = false;
      opensslSupport = false;
      wolfsslSupport = false;
    };
    homeManagerModules = {
      common = import ./common-module.nix;
      fonts = {
        home.packages = [
          pkgs.lato
          pkgs.alegreya
          pkgs.source-code-pro
          pkgs.alegreya-sans
        ];
      };
      git-config = import ./git-config.nix;
      mac-apps = import ./mac-apps;
      main = import ./personal-flake/home.nix;
    };
    homeConfigurations."ouranos" = home-manager.lib.homeManagerConfiguration {
      pkgs = pkgs;

      # Specify your home configuration modules here, for example,
      # the path to your home.nix.
      modules = [
        self.homeManagerModules.common
        self.homeManagerModules.main
        self.homeManagerModules.git-config
        self.homeManagerModules.fonts
        self.homeManagerModules.mac-apps
        {home.packages = [pkgs.cmake pkgs.nasm pkgs.ninja pkgs.autoconf pkgs.automake pkgs.autoconf-archive];}
        {
          # You can update Home Manager without changing this value. See
          # the Home Manager release notes for a list of state version
          # changes in each release.
          home.stateVersion = "22.05";
          home.packages = [pkgs.aria2];
        }
      ];

      # Optionally use extraSpecialArgs
      # to pass through arguments to home.nix
      extraSpecialArgs = {
        inherit system;
        extraFlakes = {
          inherit alejandra;
        };
      };
    };
  };
}
