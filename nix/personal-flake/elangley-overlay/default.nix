self: super: {
  fwoar = {
    gsed = super.callPackage ./prefixed-gnused {};
    sbcl_master = super.callPackage ./sbcl-master.nix {
      sbclBootstrap = super.sbcl;
      src = super.fetchFromGitHub {
        owner = "sbcl";
        repo = "sbcl";
        rev = "ccc6ce08ff4e6de764713acb315f24138e74dca3";
        sha256 = "aJQb1RENFRixBDx0aISd7Vb8mHP2UT5TDXepoLhIUhY=";
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
