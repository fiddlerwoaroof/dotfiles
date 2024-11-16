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
      common = {
        home.packages = [
          alejandra.defaultPackage.${system}
          pkgs.clojure
          pkgs.coreutils-prefixed
          pkgs.difftastic
          pkgs.direnv
          pkgs.dtach
          pkgs.ecl
          pkgs.fwoar.gsed
          pkgs.gawk
          pkgs.gnumake
          pkgs.gnuplot
          pkgs.groff
          pkgs.htop
          pkgs.jq
          pkgs.lorri
          pkgs.mosh
          # pkgs.ncdu ## currently broken
          pkgs.nixfmt-classic
          pkgs.pandoc
          pkgs.pkg-config
          pkgs.ripgrep
          (pkgs.sbcl.overrideAttrs (_: {
            enableFeatures = [
              "sb-thread"
              "sb-core-compression"
              "sb-simd"
              "sb-xref-for-internals"
              "sb-after-xc-core"
              "sb-doc"
            ];
          }))
          pkgs.shellcheck
          pkgs.texinfoInteractive
          pkgs.tree
          pkgs.vim
          pkgs.visidata
          (pkgs.zsh
            // {
              meta =
                pkgs.zsh.meta
                // {
                  outputsToInstall = pkgs.zsh.meta.outputsToInstall ++ ["info" "doc"];
                };
            })
        ];
      };
      fonts = {
        home.packages = [
          pkgs.lato
          pkgs.alegreya
          pkgs.source-code-pro
          pkgs.alegreya-sans
        ];
      };
      git-config = import ./personal-flake/git-config.nix;
      mac-apps = import ./personal-flake/mac-apps;
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
    };
  };
}
