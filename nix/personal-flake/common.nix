{pkgs}: rec {
  utils = {
    untar = path:
      pkgs.runCommand "untar" {buildInputs = [pkgs.gnutar];} ''
        mkdir -p "$out"
        cd "$out"
        tar --strip-components=1 -xf "${path}"
      '';
  };

  home-relative-git-repository = domain: owner: repo: homeDirectory:
    homeDirectory + "/git_repos/" + domain + "/" + owner + "/" + repo;

  github-repo = home-relative-git-repository "github.com";
  gf-repo = home-relative-git-repository "git.fiddlerwoaroof.com";
  gitlab-repo = home-relative-git-repository "gitlab.com";
}
