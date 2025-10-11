{
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
}
