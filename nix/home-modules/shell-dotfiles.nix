# zsh and vim configuration from this repo, plus the dein.vim plugin manager.
{pkgs, ...}: {
  home.file = {
    ".vimrc".source = ../../vimrc_work;
    ".vim/repos/github.com/Shougo/dein.vim".source =
      (pkgs.fetchFromGitHub {
        owner = "Shougo";
        repo = "dein.vim";
        rev = "c7333735af30cc06da5ee74b65ed7e85c8f7c283";
        hash = "sha256-mYrk0wYCUpx6Jr0R94QxfUjAgDLEXv71FLPQ551mnzo=";
      }).outPath;
    ".zshrc".source = ../../zsh/zshrc_work;
    ".zsh.d/nix-zsh-completions".source = ../../3dp/nix-zsh-completions;
    ".zsh.d" = {
      source = ../../zsh/zsh_plugins;
      recursive = true;
    };
  };
}
