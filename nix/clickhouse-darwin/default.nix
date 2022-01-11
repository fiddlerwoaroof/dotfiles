{ pkgs ? import <nixpkgs> {} } :

pkgs.llvmPackages_9.stdenv.mkDerivation rec {
  pname = "clickhouse";
  version = "20.8.11.17-lts";

  src = pkgs.fetchFromGitHub {
    owner  = "ClickHouse";
    repo   = "ClickHouse";
    rev    = "v${version}";
    fetchSubmodules = true;
    sha256 = "0c87k0xqwj9sc3xy2f3ngfszgjiz4rzd787bdg6fxp94w1adjhny";
  };

  nativeBuildInputs = with pkgs; [ cmake libtool llvmPackages_9.lldClang.bintools ninja ];
  buildInputs = with pkgs; [
    boost brotli capnproto cctz clang_9 double-conversion
    icu jemalloc libcpuid libxml2 lld_9 llvm_9 lz4 libmysqlclient openssl perl
    poco protobuf python3 rapidjson re2 rdkafka readline sparsehash unixODBC
    xxHash zstd
  ];

  patches = [
    # This patch is only required for 20.11.4.13 - it should be included in the
    # next stable release from upstream by default
    (pkgs.fetchpatch {
      url = "https://github.com/ClickHouse/ClickHouse/commit/e31753b4db7aa0a72a85757dc11fc403962e30db.patch";
      sha256 = "12ax02dh9y9k8smkj6v50yfr46iprscbrvd4bb9vfbx8xqgw7grb";
    })
  ];

  postPatch = ''
    patchShebangs src/

    substituteInPlace contrib/openssl-cmake/CMakeLists.txt \
      --replace '/usr/bin/env perl' perl
    substituteInPlace src/Storages/System/StorageSystemLicenses.sh \
      --replace 'git rev-parse --show-toplevel' '$src'
    substituteInPlace utils/check-style/check-duplicate-includes.sh \
      --replace 'git rev-parse --show-toplevel' '$src'
    substituteInPlace utils/check-style/check-ungrouped-includes.sh \
      --replace 'git rev-parse --show-toplevel' '$src'
    substituteInPlace utils/generate-ya-make/generate-ya-make.sh \
      --replace 'git rev-parse --show-toplevel' '$src'
    substituteInPlace utils/list-licenses/list-licenses.sh \
      --replace 'git rev-parse --show-toplevel' '$src'
    substituteInPlace utils/check-style/check-style \
      --replace 'git rev-parse --show-toplevel' '$src'

    echo $CC
  '';

  cmakeFlags = [
    "-DENABLE_TESTS=OFF"
    "-DENABLE_EMBEDDED_COMPILER=ON"
    "-USE_INTERNAL_LLVM_LIBRARY=OFF"
  ];

  postInstall = ''
    rm -rf $out/share/clickhouse-test

    sed -i -e '\!<log>/var/log/clickhouse-server/clickhouse-server\.log</log>!d' \
      $out/etc/clickhouse-server/config.xml
    substituteInPlace $out/etc/clickhouse-server/config.xml \
      --replace "<errorlog>/var/log/clickhouse-server/clickhouse-server.err.log</errorlog>" "<console>1</console>"
  '';

  hardeningDisable = [ "format" ];

}
