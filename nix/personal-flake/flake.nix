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
    common_home =
      import ./common.nix {inherit pkgs;};
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        (import ./elangley-overlay)
        emacs-community.overlay
      ];
    };
  in {
    defaultPackage.aarch64-darwin = pkgs.mkShell {
      buildInputs = [
        pkgs.alejandra
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
      main = import ./home.nix;
      fonts = {
        home.packages = [
          pkgs.lato
          pkgs.alegreya
          pkgs.source-code-pro
          pkgs.alegreya-sans
        ];
      };
    };
    homeConfigurations."edwlan" = home-manager.lib.homeManagerConfiguration {
      pkgs = pkgs;

      # Specify your home configuration modules here, for example,
      # the path to your home.nix.
      modules = [
        {
          home.packages = [alejandra.defaultPackage.${system}];
        }
        {
          home.packages = [pkgs.aria2];
        }
        self.homeManagerModules.main
        self.homeManagerModules.fonts
        {
          # You can update Home Manager without changing this value. See
          # the Home Manager release notes for a list of state version
          # changes in each release.
          home.stateVersion = "22.05";
        }
      ];

      # Optionally use extraSpecialArgs
      # to pass through arguments to home.nix
    };
  };
}
