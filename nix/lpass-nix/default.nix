{
  pkgs ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
}:
stdenv.mkDerivation {
  name = "lpass";
  nativeBuildInputs = [pkgs.cmake pkgs.pkg-config];
  buildInputs = [pkgs.libxml2 pkgs.openssl pkgs.curl];
  CFLAGS = " -fcommon ";
  src = pkgs.fetchFromGitHub {
    owner = "lastpass";
    repo = "lastpass-cli";
    rev = "v1.3.3";
    sha256 = "ChX6t62PdgveSyiJFhNr87TpF6XAsgipeo76JSd6Epk=";
  };
}
