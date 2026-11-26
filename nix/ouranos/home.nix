{
  self,
  mkHome,
  nix-editor,
  ...
}: let
  system = "aarch64-darwin";
in
  mkHome {
    inherit system;
    extraSpecialArgs.nix-editor-pkgs = nix-editor.packages.${system};
    modules = [
      self.homeManagerModules.beads
      self.homeManagerModules.main
      self.homeManagerModules.mac-apps
      ./info.nix
      ./email.nix
      ({
        config,
        pkgs,
        nix-editor-pkgs,
        ...
      }: {
        home.file."nginx-conf/ca.pem".source = ./ca.pem;
        sops = {
          # It's also possible to use a ssh key, but only when it has no password:
          age.sshKeyPaths = ["${config.home.homeDirectory}/.ssh/id_ed25519"];
          defaultSopsFile = ../../secrets/ouranos/secrets.yml;
          # %r gets replaced with a runtime directory, use %% to specify a '%'
          # sign. Runtime dir is $XDG_RUNTIME_DIR on linux and
          # $(getconf DARWIN_USER_TEMP_DIR) on darwin.
          secrets.nginx_local_key.path = "${config.home.homeDirectory}/nginx-conf/ca-key.pem";
        };
        home.packages = [
          nix-editor-pkgs.default
          pkgs.protobuf
          pkgs.cmake
          pkgs.nasm
          pkgs.ninja
          pkgs.autoconf
          pkgs.automake
          pkgs.autoconf-archive
          pkgs.openjdk8
          pkgs.cachix
          pkgs.libpq
          pkgs.libpg_query
          #pkgs.c2ffi
          pkgs.nginx
          pkgs.aria2
        ];
        # You can update Home Manager without changing this value. See
        # the Home Manager release notes for a list of state version
        # changes in each release.
        home.stateVersion = "22.05";
      })
    ];
  }
