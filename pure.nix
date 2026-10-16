{
  lib,
  stdenv,
  llvmPackages,
  gmp,
  mpfr,
  readline,
  libffi,
  pkg-config,
  autoconf,
  automake,
  libtool,
  bison,
  flex,
  which,
}:

let
  llvm = llvmPackages.llvm;
  clang = llvmPackages.clang;
in
stdenv.mkDerivation {
  pname = "pure";
  version = "0.68";  # Keep in sync with AC_INIT in pure/configure.ac

  src = ./pure;

  nativeBuildInputs = [
    pkg-config
    autoconf
    automake
    libtool
    bison
    flex
    which
    llvmPackages.llvm.dev
    clang
  ];

  buildInputs = [
    llvm
    gmp
    mpfr
    readline
    libffi
  ];

  preConfigure = ''
    # Generate configure script and supporting files from configure.ac
    if [ ! -f configure ]; then
      echo "Running autoreconf to generate configure script..."
      autoreconf -fi
    fi
  '';

  configureFlags = [
    "--with-tool-prefix=${llvm}/bin"
    "--enable-release"
  ];

  # Set LLVM-related environment for the build
  env = {
    NIX_CFLAGS_COMPILE = "-I${llvm.dev}/include";
    NIX_LDFLAGS = "-L${llvm.lib or llvm}/lib";
  };

  # Install the standard library alongside the interpreter
  postInstall = ''
    # Ensure the lib directory is populated
    if [ -d lib ]; then
      mkdir -p $out/lib/pure
      cp -r lib/*.pure $out/lib/pure/ 2>/dev/null || true
    fi
  '';

  enableParallelBuilding = true;

  meta = with lib; {
    description =
      "Pure programming language interpreter "
      + "(functional, LLVM-based)";
    homepage = "https://agraef.github.io/pure-lang/";
    license = with licenses; [ gpl3Plus lgpl3Plus ];
    platforms =
      platforms.linux ++ platforms.darwin;
    # FreeBSD: Use native packages (nixpkgs has libiconv recursion bugs)
    maintainers = [ ];
  };
}
