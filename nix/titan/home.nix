{
  self,
  mkHome,
  nixpkgs,
  nixpkgs-master,
  ...
}: let
  system = "x86_64-linux";
  cc-pkgs = import nixpkgs-master {
    inherit system;
    config.allowUnfreePredicate = pkg:
      builtins.elem (nixpkgs.lib.getName pkg) ["claude-code"];
  };
in
  mkHome {
    inherit system;
    unfree = [
      "dropbox"
      "firefox-bin"
      "firefox"
      "firefox-bin-unwrapped"
    ];
    modules = [
      self.homeManagerModules.emacs
      self.homeManagerModules.direnv
      self.homeManagerModules.tmux
      self.homeManagerModules.cli-tools
      self.homeManagerModules.shell-dotfiles
      ./improvise-ci-cache.nix
      ./sops.nix
      ./email.nix
      ({
        config,
        pkgs,
        ...
      }: let
        common_home = import ../common.nix {inherit pkgs;};
      in {
        home.packages = [
          cc-pkgs.claude-code
          pkgs.gcc
          pkgs.cmake
          pkgs.libtool
          pkgs.zeromq
          pkgs.pinentry-curses
          pkgs.git-lfs-transfer
        ];
        home.file."sbcl-source".source = common_home.utils.untar pkgs.sbcl.src;
        sops = {
          age.keyFile = "${config.home.homeDirectory}/.age-key.txt";
          defaultSopsFile = ./secrets.json;
          secrets.mail-password.path = "%r/mail-password.txt";
        };
        programs.gpg = {
          enable = true;
          mutableKeys = true;
          mutableTrust = true;
        };
        services = {
          gpg-agent = {
            enable = true;
            pinentry.package = pkgs.pinentry-curses;
          };
          dropbox.enable = true;
        };
        home.stateVersion = "21.03";
      })
    ];
  }
