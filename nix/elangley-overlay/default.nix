self: super: {
  fwoar = {
    gsed = super.callPackage ./prefixed-gnused {};
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
