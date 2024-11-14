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
      git-config = {
        config,
        lib,
        ...
      }: {
        home.activation.setup-allowed-signers = lib.hm.dag.entryAfter ["install-apps"] ''
          echo "* $(cat "$HOME"/.ssh/id_ed25519.pub)" > "$HOME"/.ssh/allowed_signers
        '';
        programs = {
          git = {
            enable = true;
            userEmail = "el-github@elangley.org";
            userName = "Edward Langley";
            lfs.enable = true;
            difftastic.enable = true;
            extraConfig = {
              commit = {gpgsign = true;};
              github = {user = "fiddlerwoaroof";};
              gpg = {
                format = "ssh";
                allowedSignersFile = "${config.home.homeDirectory}/.ssh/allowed_signers";
              };
              init = {defaultBranch = "main";};
              merge = {autoStash = true;};
              pull = {rebase = false;};
              rebase = {autoStash = true;};
              user = {signingkey = "${config.home.homeDirectory}/.ssh/id_ed25519.pub";};
            };
          };
        };
      };
      fonts = {
        home.packages = [
          pkgs.lato
          pkgs.alegreya
          pkgs.source-code-pro
          pkgs.alegreya-sans
        ];
      };
      mac-apps = import ./mac-apps;
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
        self.homeManagerModules.git-config
        self.homeManagerModules.fonts
        self.homeManagerModules.mac-apps
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
