
dnl To regenerate the configury after changes:
dnl autoconf -I config && autoheader -I config

AC_INIT(pure, 0.23)
AC_CONFIG_AUX_DIR(config)
dnl AC_CONFIG_MACRO_DIR(config)
AC_CONFIG_HEADERS(config.h)
dnl Determine host information.
AC_CANONICAL_HOST
if test -z "${host}"; then
  host=unknown
fi
AC_DEFINE_UNQUOTED(HOST, "${host}", [Define to the name of the host system.])
AC_SUBST(host)
dnl Figure out special programs and filename extensions for various systems.
dnl XXXFIXME: This is probably incomplete.
DIFF="diff"
DLLEXT=".so"
case "$host" in
    *-*-mingw*)       DLLEXT=".dll"; DIFF="diff --strip-trailing-cr";;
    *-*-darwin*)      DLLEXT=".dylib";;
    hppa*-hp-hpux*)   DLLEXT=".sl";;
esac
AC_SUBST(DIFF)
AC_SUBST(DLLEXT)
AC_DEFINE_UNQUOTED(DLLEXT, "${DLLEXT}", [Define to the filename extension for shared libraries.])
dnl Check for programs.
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_CXX
dnl Determine pointer sizes. This will be 8 on 64 bit systems.
AC_CHECK_SIZEOF(void *)
dnl Parse --enable options.
gsllib=yes
AC_ARG_ENABLE(gsl,
  [  --enable-gsl            build with GSL support (default, experimental)],
  [case "${enableval}" in
    no)   gsllib=no ;;
  esac])
AC_ARG_ENABLE(debug,
  [  --enable-debug          enable the debug build],
  [case "${enableval}" in
    yes)  CXXFLAGS="-g"; CFLAGS="-g" ;;
  esac])
AC_ARG_ENABLE(debug2,
  [  --enable-debug2         enable the maintenance build],
  [case "${enableval}" in
    yes)  CPPFLAGS="$CPPFLAGS -DDEBUG=2"; CXXFLAGS="-g"; CFLAGS="-g" ;;
  esac])
AC_ARG_ENABLE(release,
  [  --enable-release        enable the release build],
  [case "${enableval}" in
    yes)  CPPFLAGS="$CPPFLAGS -DNDEBUG -DDEBUG=0"; CXXFLAGS="-O3";
          CFLAGS="-O3" ;;
  esac])
sharedlib=yes
AC_ARG_ENABLE(shared,
  [  --enable-shared         build the shared runtime library (default)],
  [case "${enableval}" in
    no)   LDFLAGS="$LDFLAGS $rdynamic"; sharedlib=no ;;
  esac])
AC_SUBST(sharedlib)
versioned=no
AC_ARG_ENABLE(versioned,
  [  --enable-versioned      versioned install],
  [case "${enableval}" in
    yes)  versioned=yes ;;
  esac])
AC_SUBST(versioned)
AC_ARG_ENABLE(warnings,
  [  --enable-warnings       enable compiler warnings (-Wall)],
  [case "${enableval}" in
    yes)  CXXFLAGS="$CXXFLAGS -Wall"; CFLAGS="$CFLAGS -Wall" ;;
  esac])
dnl Check for libraries.
AC_CHECK_LIB(gmp, __gmpz_init)
AC_CHECK_LIB(m, cos)
dnl On some systems iconv is in a separate library, and may actually be named
dnl libiconv.
AC_CHECK_LIB(iconv, libiconv)
if test $ac_cv_lib_iconv_libiconv = no; then
  AC_CHECK_LIB(iconv, iconv)
fi
dnl On non-POSIX systems like Windows, we have to get the glob and regex
dnl functions from separate libraries, too.
AC_CHECK_LIB(glob, glob)
AC_CHECK_LIB(regex, regcomp)
dnl readline needs special treatment (macro by Ville Laurikari).
AC_CHECK_READLINE
dnl iconv and nl_langinfo need special treatment (macros by Bruno Haible).
AM_ICONV
AM_LANGINFO_CODESET
dnl Determine how to get alloca.
AC_FUNC_ALLOCA
dnl Platform-dependent time functions.
AC_CHECK_FUNCS(ftime gettimeofday nanosleep usleep)
dnl Platform specifics of signal handlers.
AC_REINSTALL_SIGHANDLERS
dnl Check to see whether we have POSIX/ISOC99 complex numbers.
AC_CHECK_TYPES([_Complex float, _Complex double])
dnl Check for GSL support.
if test $gsllib = yes; then
  AC_CHECK_LIB(gslcblas, cblas_dgemm)
  AC_CHECK_LIB(gsl, gsl_blas_dgemm)
  AC_CHECK_HEADERS([gsl/gsl_version.h])
  gsllib=no
  if test $ac_cv_lib_gsl_gsl_blas_dgemm = yes; then
    if test $ac_cv_header_gsl_gsl_version_h = yes; then
      AC_DEFINE(HAVE_GSL, 1, [Define when building with GSL vector/matrix support.])
      AC_MSG_RESULT([Building with GSL support.])
      gsllib=yes
    fi
  fi
fi
dnl Check for the new-style template arguments of the IRBuilder class.
AC_LANG_PUSH([C++])
save_CXXFLAGS=$CXXFLAGS
CXXFLAGS="`llvm-config --cppflags` $CXXFLAGS"
AC_CACHE_CHECK([for new LLVM IRBuilder class], [pure_cv_new_builder],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
       #include <llvm/Support/IRBuilder.h>
      ], [
        llvm::IRBuilder<> b;
      ])
    ], [pure_cv_new_builder=yes], [pure_cv_new_builder=no])
  ])
CXXFLAGS=$save_CXXFLAGS
if test "$pure_cv_new_builder" = yes; then
  # do whatever adjusting you need to do for presence of the library:
  AC_DEFINE(NEW_BUILDER, 1, [Define when building with new-style LLVM IRBuilder template class.])
fi
AC_LANG_POP([C++])
dnl Output files.
AC_CONFIG_FILES([Makefile])
AC_OUTPUT

llvm_version=`llvm-config --version`

AC_MSG_RESULT([
Pure ${PACKAGE_VERSION} is now configured for LLVM ${llvm_version} on ${host}.

  Source directory:     ${srcdir}
  Installation prefix:  ${prefix}
  Versioned install:    $versioned
  C compiler:           $CC $CFLAGS $CPPFLAGS
  C++ compiler:         $CXX $CXXFLAGS $CPPFLAGS
  Linker:               $CXX $LDFLAGS $LIBS
  Build libpure:        $sharedlib
  GSL support:          $gsllib

Now run 'make' to build everything, and 'make install' to install this
software on your system. To remove the installed software at a later
time use the 'make uninstall' command.
])
