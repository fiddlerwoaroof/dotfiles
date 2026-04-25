{
  config,
  lib,
  ...
}: {
  options = {
    fwoar = {
      info = {
        fullName = lib.mkOption {
          type = lib.types.str;
          default = "Bob Apple";
          description = ''
            Full Name
          '';
        };
      };
      github = {
        username = lib.mkOption {
          type = lib.types.str;
          default = "whatever";
          description = ''
            GitHub username
          '';
        };
      };
      git = {
        email = lib.mkOption {
          type = lib.types.str;
          default = "foo@example.com";
          description = ''
            Git Email
          '';
        };
      };
    };
  };
  config = {
  home.activation.setup-allowed-signers = lib.hm.dag.entryAfter ["install-apps"] ''
    echo "* $(cat "$HOME"/.ssh/id_ed25519.pub)" > "$HOME"/.ssh/allowed_signers
  '';
  programs = {
    difftastic = {
      enable = true;
      git.enable = true;
    };
    git = {
      enable = true;
      lfs = {
        enable = true;
      };
      settings = {
        commit = {
          gpgsign = true;
        };
        github = {
            user = config.fwoar.github.username;
        };
        gpg = {
          format = "ssh";
          allowedSignersFile = "${config.home.homeDirectory}/.ssh/allowed_signers";
        };
        init = {
          defaultBranch = "main";
        };
        merge = {
          autoStash = true;
        };
          push = {
            default = "upstream";
          };
        pull = {
          rebase = false;
        };
        rebase = {
          autoStash = true;
        };
          user = {
            email = config.fwoar.git.email;
            name = config.fwoar.info.fullName;
            signingkey = "${config.home.homeDirectory}/.ssh/id_ed25519.pub";
          };
        };
        signing = {
          format = "openpgp";
        };
      };
    };
  };
}
