# emacs-overlay's package set, with local fixes layered on top.
{
  emacs-community,
  system,
}: let
  upstream = emacs-community.packages.${system};

  # nixpkgs applies these to its emacs, but they are already on emacs master,
  # so they fail to apply to emacs-git.  Drop this once emacs-overlay filters
  # them itself (overlays/emacs.nix, inapplicablePatches).
  alreadyOnMaster = [
    "fix-arbitrary-code-execution-in-dot-dir-locals-dot-el-handling.patch"
  ];

  emacs-git = upstream.emacs-git.overrideAttrs (old: {
    patches =
      builtins.filter
      (patch: !(builtins.elem (patch.name or patch) alreadyOnMaster))
      old.patches;
    # ffap--darwin-bypass-xcode-p does (zerop (ignore-errors (call-process
    # "xcode-select" ...))).  xcode-select isn't on PATH in the build sandbox,
    # so that is (zerop nil) -> wrong-type-argument, which breaks the org.texi
    # export (org requires ffap).  Drop once fixed upstream.
    postPatch =
      (old.postPatch or "")
      + ''
        substituteInPlace lisp/ffap.el \
          --replace-fail '(not (zerop (ignore-errors' '(not (eql 0 (ignore-errors'
      '';
    # keep emacs-git.pkgs building against the patched emacs
    passthru =
      old.passthru
      // {
        pkgs = old.passthru.pkgs.overrideScope (_: _: {emacs = emacs-git;});
      };
  });
in
  upstream // {inherit emacs-git;}
