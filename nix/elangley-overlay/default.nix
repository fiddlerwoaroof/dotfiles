self: super: 

{
  fwoar = {
    gsed = super.callPackage ./prefixed-gnused {};
    sbcl_2_0_10 = super.callPackage ./sbcl.nix {
      version = "2.0.10";
      sha = "sha256:0mq5ga977hzsq4wgv31l8d6rpa186q8xc4x2awwcskf5nq842xai";
    };
  };
}
