{
  description = "Pure programming language - functional, LLVM-based";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    let
      # Systems for native builds
      # FreeBSD excluded: nixpkgs has libiconv infinite recursion on FreeBSD
      # FreeBSD users should use native packages (pkg install llvm21 gmp mpfr ...)
      nativeSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      # LLVM major version to use (LLVM 20.1+ required for ORC JIT v2)
      # Update to 22+ when available in nixpkgs
      llvmVersion = 21;

      # Helper to get the right LLVM packages
      getLlvmPackages = pkgs:
        pkgs."llvmPackages_${toString llvmVersion}";

      # Build pure for a given pkgs set, optionally overriding the LLVM version
      mkPureWith = pkgs: llvmMajor:
        pkgs.callPackage ./pure.nix {
          llvmPackages = pkgs."llvmPackages_${toString llvmMajor}";
        };

      # Build pure for a given pkgs set using the default LLVM version
      mkPure = pkgs: mkPureWith pkgs llvmVersion;

    in
    (flake-utils.lib.eachSystem nativeSystems (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;
        llvmPkgs = getLlvmPackages pkgs;
        isLinux = pkgs.stdenv.hostPlatform.isLinux;
      in
      {
        packages = {
          default = mkPure pkgs;
          pure = mkPure pkgs;
          pure-llvm22 = mkPureWith pkgs 22;
        };

        devShells.default = (pkgs.mkShell.override { stdenv = llvmPkgs.stdenv; }) {
          name = "pure-dev";

          packages = [
            # LLVM toolchain
            llvmPkgs.llvm
            llvmPkgs.llvm.dev
            llvmPkgs.clang

            # Build dependencies
            pkgs.gmp
            pkgs.gmp.dev
            pkgs.mpfr
            pkgs.mpfr.dev
            pkgs.readline
            pkgs.libffi
            pkgs.pkg-config

            # Shell tools
            pkgs.bash  # Bash 5.x for direnv (;& syntax not in bash 3.x)

            # Build tools
            pkgs.autoconf
            pkgs.automake
            pkgs.libtool
            pkgs.bison
            pkgs.flex
            pkgs.gnumake
            pkgs.which

            # Ecosystem library dependencies
            pkgs.gsl           # pure-gsl
            pkgs.glpk          # pure-glpk
            pkgs.zlib          # pure-glpk (indirect dep via -lz)
            pkgs.libxml2       # pure-xml
            pkgs.libxslt       # pure-xml
            pkgs.sqlite        # pure-sql3
            pkgs.unixodbc      # pure-odbc
            pkgs.fcgi          # pure-fastcgi
            pkgs.tcl           # pure-tk
            pkgs.tk            # pure-tk
            # pure-gen (Haskell toolchain + language-c build deps)
            (pkgs.haskellPackages.ghcWithPackages (hp: [ hp.syb ]))
            pkgs.haskellPackages.happy
            pkgs.haskellPackages.alex
          ] ++ lib.optionals (!pkgs.stdenv.hostPlatform.isDarwin) [
            pkgs.libiconv
          ] ++ lib.optionals isLinux [
            pkgs.gdb
            pkgs.valgrind
            pkgs.bear
            pkgs.xorg.libX11   # pure-tk (Tk requires X11)
          ];

          shellHook = ''
            echo "Pure development shell"
            echo "  LLVM:    $(llvm-config --version)"
            echo "  CC:      $CC"
            echo "  CXX:     $CXX"
            echo "  System:  ${system}"
            echo ""

            export LLVM_CONFIG="${llvmPkgs.llvm.dev}/bin/llvm-config"
          '';
        };
      }
    ))
    // {
      # Overlay for downstream consumers
      overlays.default = final: prev: {
        pure = mkPure final;
      };
    };
}
