# Keeps titan's harmonia cache warm for the improvise CI runners. Runner jobs
# can't push to titan's store, so titan builds gh/main's CI dev shell and
# crate2nix package itself; the out-links are GC roots. A user service so git
# uses ~/.ssh (the repo is private); linger keeps it running while logged out.
# It works in its own worktree, so no checkout of yours is touched.
{pkgs, ...}: let
  repo = "/home/edwlan/git_repos/git.fiddlerwoaroof.com/u/edwlan/improvise";
  script = pkgs.writeShellScript "improvise-ci-cache" ''
    set -euo pipefail
    wt="$STATE_DIRECTORY/worktree"
    git -C ${repo} fetch gh main
    if [ -e "$wt/.git" ]; then
      git -C "$wt" checkout --detach --force gh/main
    else
      git -C ${repo} worktree prune
      git -C ${repo} worktree add --detach "$wt" gh/main
    fi
    echo "building gh/main at $(git -C "$wt" rev-parse --short HEAD)"
    nix build --out-link "$STATE_DIRECTORY/ci-shell" "$wt#devShells.x86_64-linux.ci"
    # Every CI build (nix/ci.nix), which also roots the crane dependency
    # derivations PR jobs reuse. Not `site`: its stamp is the commit, so no PR
    # could ever reuse it. --keep-going so one failing check still caches the rest.
    nix build --keep-going --out-link "$STATE_DIRECTORY/main" \
      "$wt#improvise" "$wt#web" "$wt#ci-deps" \
      "$wt#checks.x86_64-linux.clippy" "$wt#checks.x86_64-linux.test" \
      "$wt#checks.x86_64-linux.wasm-check" "$wt#checks.x86_64-linux.harness-lint"
  '';
in {
  systemd.user.services.improvise-ci-cache = {
    Unit.Description = "Build improvise gh/main's CI outputs into titan's store for the runner cache";
    Service = {
      Type = "oneshot";
      ExecStart = "${script}";
      # nix from the system profile, so the client matches the daemon.
      Environment = "PATH=${pkgs.lib.makeBinPath [pkgs.git pkgs.openssh pkgs.coreutils]}:/run/current-system/sw/bin";
      StateDirectory = "improvise-ci";
      Nice = 10;
    };
  };
  systemd.user.timers.improvise-ci-cache = {
    Unit.Description = "Hourly improvise CI cache build";
    Timer = {
      OnCalendar = "hourly";
      RandomizedDelaySec = "5m";
      Persistent = true;
    };
    Install.WantedBy = ["timers.target"];
  };
}
