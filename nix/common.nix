{
  pkgs,
  homeDirectory,
}: let
  alejandra =
    (import (builtins.fetchTarball {
      url = "https://github.com/kamadorueda/alejandra/tarball/1.1.0";
      sha256 = "0r8d4pllz3rar5b8xlk332mm395nj6w1zh6dnpbz7156fii4lhdy";
    }))
    # Pick one from: aarch64-darwin, aarch64-linux, i686-linux, x86_64-darwin, x86_64-linux
    ."${pkgs.system}";
in rec {
  packages = [
    alejandra
    pkgs.difftastic
    pkgs.direnv
    pkgs.dtach
    pkgs.ecl
    pkgs.gnuplot
    pkgs.texinfoInteractive
    pkgs.jq
    pkgs.lorri
    pkgs.ncdu
    pkgs.nixfmt
    pkgs.ripgrep
    (pkgs.sbcl.overrideAttrs (_: {propagatedBuildInputs = [pkgs.sqlite];}))
    pkgs.tree
  ];

  utils = {
    untar = path:
      pkgs.runCommand "untar" {buildInputs = [pkgs.gnutar];} ''
        mkdir -p "$out"
        cd "$out"
        tar --strip-components=1 -xf "${path}"
      '';
  };

  home-relative-git-repository = domain: owner: repo:
    homeDirectory + "/git_repos/" + domain + "/" + owner + "/" + repo;

  github-repo = home-relative-git-repository "github.com";
  gf-repo = home-relative-git-repository "git.fiddlerwoaroof.com";
  gitlab-repo = home-relative-git-repository "gitlab.com";

  overlays = [(import ./elangley-overlay) (import ./emacs-overlay.nix)];
}
