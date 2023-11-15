self: super: {
  fwoar = {
    gsed = super.callPackage ./prefixed-gnused {};
    sbcl_master = super.callPackage ./sbcl-master.nix {
      sbclBootstrap=super.sbcl;
      src = super.fetchFromGitHub {
        owner = "sbcl";
        repo = "sbcl";
        rev = "de68e71a81a5ca270ebb796e5d0bb1b47f1a609a";
        sha256 = "5iVDTXTCH+gxlVGJVqzN3WZ+/qvu0CGB1AbN1YtV1zs=";
      };
    };
    sbcl_2_3_10 = super.callPackage ./sbcl.nix {
      version = "2.3.10";
      sha = "sha256:0000000000000000000000000000000000000000000000000000";
    };

    sbcl_2_1_7 = super.callPackage ./sbcl.nix {
      version = "2.1.7";
      sha = "sha256:1ihxx8bjvcqg5f6m7xxvrilqjphshlx6nahns81j1bij70anyq0j";
    };
    sbcl_2_0_10 = super.callPackage ./sbcl.nix {
      version = "2.0.10";
      sha = "sha256:0mq5ga977hzsq4wgv31l8d6rpa186q8xc4x2awwcskf5nq842xai";
    };
  };
}
