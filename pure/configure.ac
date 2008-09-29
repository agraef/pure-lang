
dnl To regenerate the configury after changes:
dnl autoconf -I config && autoheader -I config

AC_INIT(pure, 0.8)
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
dnl Figure out extra build flags and filename extensions for various systems.
dnl XXXFIXME: This is probably incomplete. Please let me know about options
dnl needed for other Unix flavours.
shared="-shared"
rdynamic="-rdynamic"
PIC=
DLLEXT=".so"
AUXLIBS=
LD_LIB_PATH="LD_LIBRARY_PATH"
case "$host" in
    *-*-mingw*)       AUXLIBS="-DLIBGLOB='\"libglob.dll\"' -DLIBREGEX='\"libgnurx-0.dll\"'";
                      LIBS="$LIBS -limagehlp -lpsapi -lmingwex";
                      LDFLAGS="-Wl,--enable-auto-import"; DLLEXT=".dll";;
    x86_64-*-linux*)  PIC="-fPIC";;
    *-*-darwin*)      LD_LIB_PATH="DYLD_LIBRARY_PATH";
                      rdynamic=""; shared="-dynamiclib -install_name \$(libdir)/\$(libpure)"; DLLEXT=".dylib";;
    hppa*-hp-hpux*)   rdynamic=""; DLLEXT=".sl";;
esac
AC_SUBST(shared)
AC_SUBST(rdynamic)
AC_SUBST(PIC)
AC_SUBST(DLLEXT)
AC_SUBST(AUXLIBS)
AC_SUBST(LD_LIB_PATH)
AC_DEFINE_UNQUOTED(DLLEXT, "${DLLEXT}", [Define to the filename extension for shared libraries.])
dnl Check for programs.
AC_PROG_INSTALL
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
    yes)  CXXFLAGS="-g" ;;
  esac])
AC_ARG_ENABLE(debug2,
  [  --enable-debug2         enable the maintenance build],
  [case "${enableval}" in
    yes)  CPPFLAGS="-DDEBUG=2"; CXXFLAGS="-g" ;;
  esac])
AC_ARG_ENABLE(release,
  [  --enable-release        enable the release build],
  [case "${enableval}" in
    yes)  CPPFLAGS="-DNDEBUG -DDEBUG=0"; CXXFLAGS="-O3" ;;
  esac])
sharedlib=yes
AC_ARG_ENABLE(shared,
  [  --enable-shared         build the shared runtime library (default)],
  [case "${enableval}" in
    no)   LDFLAGS="$LDFLAGS $rdynamic"; sharedlib=no ;;
  esac])
AC_SUBST(sharedlib)
AC_ARG_ENABLE(warnings,
  [  --enable-warnings       enable compiler warnings (-Wall)],
  [case "${enableval}" in
    yes)  CXXFLAGS="$CXXFLAGS -Wall" ;;
  esac])
dnl Check for libraries.
AC_CHECK_LIB(gmp, __gmpz_init)
AC_CHECK_LIB(readline, readline)
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
AC_CONFIG_FILES([Makefile])
AC_OUTPUT

AC_MSG_RESULT([
Pure is now configured for ${host}.

  Source directory:     ${srcdir}
  Installation prefix:  ${prefix}
  Compiler:             $CXX $CXXFLAGS $CPPFLAGS
  Linker:               $CXX $LDFLAGS $LIBS
  Build libpure:        $sharedlib
  GSL support:          $gsllib

Now run 'make' to build everything, and 'make install' to install this
software on your system. To remove the installed software at a later
time use the 'make uninstall' command.
])
