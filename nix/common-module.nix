{
  alejandra-pkgs,
  fwoar-pkgs,
  pkgs,
  system,
  ...
}: {
  home.packages = [
    alejandra-pkgs.default
    fwoar-pkgs.zenburn
    pkgs.clojure
    pkgs.coreutils-prefixed
    pkgs.difftastic
    pkgs.direnv
    pkgs.dtach
    pkgs.ecl
    pkgs.fwoar.gsed
    pkgs.gawk
    pkgs.gnumake
    pkgs.gnuplot
    pkgs.groff
    pkgs.htop
    pkgs.jq
    pkgs.lorri
    pkgs.mosh
    # pkgs.ncdu ## currently broken
    pkgs.nixfmt-classic
    pkgs.pandoc
    pkgs.pkg-config
    pkgs.ripgrep
    (pkgs.sbcl.overrideAttrs (_: {
      enableFeatures = [
        "sb-thread"
        "sb-core-compression"
        "sb-simd"
        "sb-xref-for-internals"
        "sb-after-xc-core"
        "sb-doc"
      ];
    }))
    pkgs.shellcheck
    pkgs.texinfoInteractive
    pkgs.tree
    pkgs.vim
    pkgs.visidata
    (pkgs.zsh
      // {
        meta =
          pkgs.zsh.meta
          // {
            outputsToInstall = pkgs.zsh.meta.outputsToInstall ++ ["info" "doc"];
          };
      })
  ];
}
