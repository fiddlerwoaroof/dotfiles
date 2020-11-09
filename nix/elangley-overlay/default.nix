self: super: 

{
  fwoar = {
    gsed = super.callPackage ./prefixed-gnused {};
  };
}
