{ pkgs, homeDirectory }: rec {
  packages = [
    pkgs.direnv
    pkgs.dtach
    pkgs.ecl
    pkgs.gnuplot
    pkgs.jq
    pkgs.lorri
    pkgs.nixfmt
    pkgs.ripgrep
    pkgs.sbcl
    pkgs.tree
  ];

  utils = {
    untar = path:
      pkgs.runCommand "untar" { buildInputs = [ pkgs.gnutar ]; } ''
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

  overlays = [ (import ./elangley-overlay) (import ./emacs-overlay.nix) ];
}
