{
  pkgs,
  ...
}: let
  openssl = pkgs.openssl.overrideAttrs (oldAttrs: {meta = oldAttrs.meta // {outputsToInstall = oldAttrs.meta.outputsToInstall or ["out"] ++ ["dev"];};});
in {
  home = {
    packages =
      [
        openssl
        pkgs.awscli
        pkgs.cachix
        pkgs.curl
        pkgs.cvs
        pkgs.direnv
        pkgs.glibcLocales
        pkgs.gron
        pkgs.libssh2
        pkgs.lorri
        pkgs.ncdu
        pkgs.nix
        pkgs.pass
        pkgs.sqlite
        pkgs.sqlite.dev
        pkgs.sqlite.out
        pkgs.visidata
        pkgs.zeromq
      ]
      ++ (with pkgs; [
        ccl
        ecl
        #gcl
        cmucl_binary
        nixpkgs-fmt
      ]);
  };
}
