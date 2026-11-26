# gpg with a user agent; `pass` (in cli-tools) needs both.
{pkgs, ...}: {
  home.packages = [pkgs.pinentry-curses];
  programs.gpg = {
    enable = true;
    mutableKeys = true;
    mutableTrust = true;
  };
  services.gpg-agent = {
    enable = true;
    pinentry.package = pkgs.pinentry-curses;
  };
}
