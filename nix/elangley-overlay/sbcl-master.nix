{
  lib,
  stdenv,
  src,
  writeText,
  sbclBootstrap,
  zstd,
  sbclBootstrapHost ? "${sbclBootstrap}/bin/sbcl --disable-debugger --no-userinit --no-sysinit",
  texinfo,
}: let
  version = builtins.substring 0 7 src.rev;
in
  stdenv.mkDerivation rec {
    pname = "sbcl";
    inherit version;
    inherit src;

    nativeBuildInputs = [texinfo];
    buildInputs = [zstd];

    # There are no patches necessary for the currently enabled versions, but this
    # code is left in place for the next potential patch.
    postPatch = ''
      echo '"${version}.nixos"' > version.lisp-expr

      # SBCL checks whether files are up-to-date in many places..
      # Unfortunately, same timestamp is not good enough
      sed -e 's@> x y@>= x y@' -i contrib/sb-aclrepl/repl.lisp
      #sed -e '/(date)/i((= date 2208988801) 2208988800)' -i contrib/asdf/asdf.lisp
      sed -i src/cold/slam.lisp -e \
        '/file-write-date input/a)'
      sed -i src/cold/slam.lisp -e \
        '/file-write-date output/i(or (and (= 2208988801 (file-write-date output)) (= 2208988801 (file-write-date input)))'
      sed -i src/code/target-load.lisp -e \
        '/date defaulted-fasl/a)'
      sed -i src/code/target-load.lisp -e \
        '/date defaulted-source/i(or (and (= 2208988801 (file-write-date defaulted-source-truename)) (= 2208988801 (file-write-date defaulted-fasl-truename)))'

      # Fix the tests
      sed -e '5,$d' -i contrib/sb-bsd-sockets/tests.lisp
      sed -e '5,$d' -i contrib/sb-simple-streams/*test*.lisp
          sed -e "s@/bin/uname@$(command -v uname)@g" -i src/code/*-os.lisp \
            src/code/run-program.lisp
    '';

    preBuild = ''
      export INSTALL_ROOT=$out
      mkdir -p test-home
      export HOME=$PWD/test-home
    '';

    enableFeatures = ["sb-thread" "sb-core-compression"];

    # Fails to find `O_LARGEFILE` otherwise.
    env.NIX_CFLAGS_COMPILE = "-D_GNU_SOURCE";

    buildPhase = ''
      runHook preBuild

      sh make.sh --prefix=$out --xc-host="${sbclBootstrapHost}" --fancy ${lib.optionalString (stdenv.hostPlatform.system == "aarch64-darwin") "--arch=arm64"}
      (cd doc/manual ; make info)

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      INSTALL_ROOT=$out sh install.sh

      runHook postInstall
    '';

    meta = sbclBootstrap.meta;
  }
