{
  config,
  lib,
  ...
}: {
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
      settings = {
        user = {
          email = "el-github@elangley.org";
          name = "Edward Langley";
          signingkey = "${config.home.homeDirectory}/.ssh/id_ed25519.pub";
        };
        commit = {
          gpgsign = true;
        };
        github = {
          user = "fiddlerwoaroof";
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
        pull = {
          rebase = false;
        };
        rebase = {
          autoStash = true;
        };
      };
      lfs.enable = true;
    };
  };
}
