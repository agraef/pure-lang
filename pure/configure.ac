
dnl To regenerate the configury after changes:
dnl aclocal && autoconf && autoheader  (or just: autoreconf)

AC_INIT(pure, 0.68)
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
EXEEXT=""
DLLEXT=".so"
LD_LIB_PATH="LD_LIBRARY_PATH"
rdynamic="-rdynamic"
shared="-shared"
case "$host" in
    *-*-mingw*)       EXEEXT=".exe"; DLLEXT=".dll";
    		      DIFF="diff --strip-trailing-cr";;
    *-*-darwin*)      rdynamic=""; shared="-dynamiclib"; DLLEXT=".dylib";
                      LD_LIB_PATH="DYLD_LIBRARY_PATH";
                      ;;
    hppa*-hp-hpux*)   rdynamic=""; DLLEXT=".sl";;
esac
AC_SUBST(shared)
AC_SUBST(DIFF)
AC_SUBST(DLLEXT)
AC_SUBST(EXEEXT)
AC_SUBST(LD_LIB_PATH)
AC_DEFINE_UNQUOTED(DLLEXT, "${DLLEXT}", [Define to the filename extension for shared libraries.])
AC_DEFINE_UNQUOTED(EXEEXT, "${EXEEXT}", [Define to the filename extension for executables.])
dnl Check for programs.
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_CXX
if test -z "$BISON"; then
  AC_CHECK_PROG(BISON,bison,bison,bison-missing)
fi
if test "$BISON" = bison-missing; then
  AC_MSG_WARN([Cannot find bison, won't be able to regenerate parser.cc.])
fi
AC_ARG_VAR(BISON, [how to invoke bison (version 2.3 or later required)])
if test -z "$FLEX"; then
  AC_CHECK_PROG(FLEX,flex,flex,flex-missing)
fi
if test "$FLEX" = flex-missing; then
  AC_MSG_WARN([Cannot find flex, won't be able to regenerate lexer.cc.])
else
  flexminor=`"$FLEX" --version 2>&1 | sed -e 's,^flex \(version \)*2\.5\.\(.*\),\2,'`
  if test "$flexminor" = "3" -o "$flexminor" = "4" -o "$flexminor" = "4a"; then
    AC_MSG_WARN([Your flex version is too old, consider installing flex 2.5.31 or newer.])
  fi
fi
AC_ARG_VAR(FLEX, [how to invoke flex (version 2.5.31 or later required)])
AC_PATH_PROG(EMACS,emacs)
if test -z "$EMACS"; then
  AC_MSG_WARN([Cannot find emacs, won't be able to byte-compile pure-mode.el.])
fi

dnl Check for C++17 support (required for LLVM 20+).
AC_MSG_CHECKING([whether $CXX supports C++17])
save_CXXFLAGS=$CXXFLAGS
CXXFLAGS="$CXXFLAGS -std=c++17"
AC_LANG_PUSH([C++])
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM([[
    #include <type_traits>
    static_assert(std::is_same_v<int,int>);
  ]], [])],
  [AC_MSG_RESULT([yes])
   CXX17FLAG="-std=c++17"],
  [AC_MSG_RESULT([no])
   AC_MSG_ERROR([C++17 support is required for LLVM 20+. Please use a compiler that supports -std=c++17.])])
AC_LANG_POP([C++])
CXXFLAGS="$save_CXXFLAGS $CXX17FLAG"

dnl Check for special linker options.
AX_CHECK_LINK_FLAG([-l:libm.a], [pure_cv_gnu_linker="yes"])
if test "$pure_cv_gnu_linker" = yes; then
  AC_DEFINE(GNU_LINKER, 1, [Define if your linker understands the '-l:' option.])
fi

dnl Install elisp files?
elcfiles="etc/pure-mode.elc etc/flycheck-pure.elc"
AC_ARG_WITH(elisp,
  [  --with-elisp            install elisp files (default if emacs is available)],
  [case "${withval}" in
     no)  elcfiles="" ;;
   esac])
AC_SUBST(elcfiles)
AC_PATH_PROG(TEXMACS,texmacs)
dnl Install texmacs files?
tmfiles='$(tmsrc)'
AC_ARG_WITH(texmacs,
  [  --with-texmacs          install texmacs files (default if it is available)],
  [case "${withval}" in
     no)  tmfiles="" ;;
   esac])
AC_SUBST(tmfiles)
dnl Determine compiler characteristics.
AC_C_BIGENDIAN
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(size_t)
dnl Parse --enable options.
fastcc=yes
CFLAGS_USER=`echo $CFLAGS     | sed -e 's/^  *//' -e 's/  *$//'`
CXXFLAGS_USER=`echo $CXXFLAGS | sed -e 's/^  *//' -e 's/  *$//'`
buildtype=no
AC_ARG_ENABLE(fastcc,
  [  --enable-fastcc         enable fast calls/tail call optimization (default)],
  [case "${enableval}" in
    no)   fastcc=no ;;
  esac])
AC_ARG_ENABLE(debug,
  [  --enable-debug          enable the debug build],
  [case "${enableval}" in
    yes)  CXXFLAGS="-g -O0"; CFLAGS="-g -O0"; buildtype=yes ;;
  esac])
AC_ARG_ENABLE(debug2,
  [  --enable-debug2         enable the maintenance build],
  [case "${enableval}" in
    yes)  CPPFLAGS="$CPPFLAGS -DDEBUG=2"; CXXFLAGS="-g -O0";
    	  CFLAGS="-g -O0"; buildtype=yes ;;
  esac])
AC_ARG_ENABLE(release,
  [  --enable-release        enable the release build],
  [case "${enableval}" in
    yes)  CPPFLAGS="$CPPFLAGS -DNDEBUG -DDEBUG=0"; CXXFLAGS="-g0 -O3";
          CFLAGS="-g0 -O3"; buildtype=yes ;;
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
dnl Add extra CFLAGS/CXXFLAGS specified by the user if a
dnl build type was specified.
if test $buildtype = yes; then
  if test -n "$CFLAGS_USER" -a "$CFLAGS_USER" != "-g -O2" ; then
    CFLAGS="$CFLAGS_USER $CFLAGS"
  fi
  if test -n "$CXXFLAGS_USER" -a "$CXXFLAGS_USER" != "-g -O2"; then
    CXXFLAGS="$CXXFLAGS_USER $CXXFLAGS"
  fi
fi
dnl These are just dummy options to make the Debian package builder happy.
AC_ARG_ENABLE(maintainer-mode, [  --enable-maintainer-mode (ignored)], [])
AC_ARG_ENABLE(dependency-tracking, [  --enable-dependency-tracking (ignored)], [])
dnl Readline/libedit support.
rl_use_readline="yes"
rl_use_editline="yes"
vl_readline_libs="readline"
vl_editline_libs="edit editline"
AC_ARG_WITH(readline,
  [  --with-readline         readline support, if available (default)],
  [case "${withval}" in
     no)  vl_readline_libs=""; rl_use_readline="no" ;;
   esac])
AC_ARG_WITH(editline,
  [  --with-editline         editline support, if available (default)],
  [case "${withval}" in
     no)  vl_editline_libs=""; rl_use_editline="no" ;;
   esac])
if test -z "$vl_readline_libs" -a -z "$vl_editline_libs"; then
  vl_readline_libs=""
else
  vl_readline_libs="$vl_readline_libs $vl_editline_libs"
fi
vl_editline_libs=""
dnl libpcre support (replaces GNU/POSIX regex if available).
pure_use_pcre="no"
pcrelib="no (use --with-pcre to enable)"
PCRE_CFLAGS=""
PCRE_LIBS=""
AC_ARG_WITH(pcre,
  [  --with-pcre             libpcre support (shared libraries)],
  [case "${withval}" in
     yes)  pure_use_pcre="yes"; pcrelib="" ;;
     no)  pure_use_pcre="no"; pcrelib="no (use --with-pcre to enable)" ;;
   esac])
dnl How to link LLVM.
static_llvm_libs="no"
AC_ARG_WITH(static-llvm,
  [  --with-static-llvm      link with the static LLVM libraries],
  [case "${withval}" in
     yes) static_llvm_libs="yes" ;;
   esac])
dnl Which LLVM version to link against (default: latest version). This
dnl requires that you have the llvm-config-<version> script on your PATH.
llvm_config_pre="llvm-config"
AC_ARG_WITH(llvm-version,
  [  --with-llvm-version     force LLVM version to link against],
  [case "${withval}" in
     no)  ;;
     yes) ;;
     *)   llvm_config_pre="llvm-config-${withval}" ;;
   esac])
dnl How to find the LLVM tools.
TOOL_PREFIX=""
AC_ARG_WITH(tool-prefix,
  [  --with-tool-prefix      prefix under which the LLVM toolchain is installed],
  [case "${withval}" in
     no)  ;;
     yes) ;;
     *)   TOOL_PREFIX="${withval}/";
     	  llvm_config_pre="${withval}/llvm-config ${llvm_config_pre}";
	  ;;
   esac])
dnl How to link GMP/MPIR.
pure_cv_gmp="gmp"
AC_ARG_WITH(mpir,
  [  --with-mpir             use MPIR as a GMP replacement],
  [case "${withval}" in
     yes) pure_cv_gmp="mpir" ;;
   esac])
PURE_LIBGMP_PREFIX
AC_CHECK_LIB(${pure_cv_gmp}, __gmpz_init, ,
[AC_MSG_ERROR([Cannot find the GMP library, please install libgmp or libmpir.])])
AC_CHECK_HEADERS(${pure_cv_gmp}.h, [],
[AC_MSG_ERROR([Cannot find the GMP header file, please install libgmp or libmpir.])])
dnl How to link MPFR.
PURE_LIBMPFR_PREFIX
AC_CHECK_LIB(mpfr, mpfr_init, ,
[AC_MSG_ERROR([Cannot find the MPFR library, please install libmpfr.])])
AC_CHECK_HEADERS(mpfr.h, [],
[AC_MSG_ERROR([Cannot find the MPFR header file, please install libmpfr.])])
dnl Other checks for libraries.
AC_CHECK_LIB(m, cos)
dnl On some systems iconv is in a separate library, and may actually be named
dnl libiconv.
AC_CHECK_LIB(iconv, libiconv)
if test $ac_cv_lib_iconv_libiconv = no; then
  AC_CHECK_LIB(iconv, iconv)
fi
dnl libpcre support (replaces GNU/POSIX regex if available).
if test $pure_use_pcre = yes; then
  AC_CHECK_LIB(pcreposix, regcomp, [pcrelib=-lpcreposix], [pure_use_pcre="no"; pcrelib="no (not available)"])
fi
if test $pure_use_pcre = yes; then
  AC_DEFINE(USE_PCRE, 1, [Define to use the pcre library.])
  PCRE_LIBS=$(pkg-config --libs libpcreposix 2>/dev/null)
  AC_DEFINE_UNQUOTED(PCRE_LIBS, "${PCRE_LIBS}", [Libraries to be linked for Perl regex support.])
  pcrelib="$PCRE_LIBS"
fi
AC_SUBST(PCRE_CFLAGS)
AC_SUBST(PCRE_LIBS)
dnl On non-POSIX systems like Windows, we have to get the glob and regex
dnl functions from separate libraries, too.
AC_CHECK_LIB(glob, glob)
if test $pure_use_pcre != yes; then
  AC_CHECK_LIB(regex, regcomp)
fi
dnl readline/editline support (macro by Ville Laurikari).
AC_CHECK_READLINE
if test $rl_use_readline = yes; then
  AC_DEFINE(USE_READLINE, 1, [Define to use the readline library.])
fi
if test $rl_use_editline = yes; then
  AC_DEFINE(USE_EDITLINE, 1, [Define to use the editline library.])
fi
dnl editline doesn't have this
save_LIBS="$LIBS"
LIBS="$LIBS $rllib"
AC_CHECK_FUNCS(history_set_history_state)
LIBS="$save_LIBS"
dnl How to get the pthread library.
AX_PTHREAD
dnl iconv and nl_langinfo need special treatment (macros by Bruno Haible).
AM_ICONV
if test "$am_cv_func_iconv" != yes; then
  AC_MSG_ERROR([Cannot find iconv, please install GNU libiconv.])
fi
AM_LANGINFO_CODESET
dnl Determine how to get alloca.
AC_FUNC_ALLOCA
dnl Platform-dependent time functions.
AC_CHECK_FUNCS(ftime gettimeofday nanosleep usleep)
PURE_CHECK_TIME_H_DECLARES_DAYLIGHT
PURE_CHECK_TM_INCLUDES_TM_GMTOFF
# We have a replacement for this one if it isn't available.
AC_REPLACE_FUNCS(strptime)
dnl Check for the dirent.h header and the readdir() function.
AC_CHECK_HEADERS([dirent.h])
AC_CHECK_FUNCS(readdir)
# Other platform-specific system functions.
AC_CHECK_HEADERS([sys/fcntl.h sys/wait.h])
AC_CHECK_FUNCS(mkstemp fstat _setjmp _longjmp)
AC_CHECK_MEMBERS([struct stat.st_rdev])
dnl Platform specifics of signal handlers.
AC_REINSTALL_SIGHANDLERS
dnl Check to see whether we have POSIX/ISOC99 complex numbers.
AC_CHECK_TYPES([_Complex float, _Complex double])
dnl fastcc/tail call support.
if test $fastcc = yes; then
  AC_DEFINE(HAVE_FASTCC, 1, [Define to build with fastcc/TCO support (broken on some systems).])
fi

dnl Check for LLVM (require LLVM 20.1 or later).
AC_CHECK_PROGS(LLVMCONF,[${llvm_config_pre} llvm-config-22 llvm-config-21 llvm-config-20 llvm-config],no,"$PATH:/")
if test "$LLVMCONF" = no; then
  AC_MSG_ERROR([Cannot find LLVM.])
fi

dnl Determine the LLVM version and check minimum requirement.
llvm_version=`$LLVMCONF --version`
AC_DEFINE_UNQUOTED(LLVM_VERSION, "${llvm_version}", [Define to the LLVM version string.])

dnl Require LLVM >= 20.1 (modern ORC JIT v2 with stable APIs).
llvm_major=`echo "$llvm_version" | sed -e 's/\..*//'`
llvm_minor=`echo "$llvm_version" | sed -e 's/^[0-9]*\.\([0-9]*\).*/\1/'`
if test "$llvm_major" -lt 20 2>/dev/null || \
   { test "$llvm_major" -eq 20 && test "$llvm_minor" -lt 1 2>/dev/null; }; then
  AC_MSG_ERROR([LLVM >= 20.1 is required, but found ${llvm_version}.])
fi

dnl Some systems have the LLVM tools under /usr/lib/llvm-${llvm_version}/bin.
if test -z "${TOOL_PREFIX}" -a -x "/usr/lib/llvm-${llvm_version}/bin/opt"; then
  TOOL_PREFIX="/usr/lib/llvm-${llvm_version}/bin/"
fi
AC_SUBST(TOOL_PREFIX)
AC_DEFINE_UNQUOTED(TOOL_PREFIX, "${TOOL_PREFIX}", [Define to the prefix under which the LLVM toolchain is installed.])

dnl LLVM lists extra system libraries with the --system-libs option.
dnl These are required when linking against the static LLVM libs. Note that
dnl we edit some of the linker options to prevent collisions with Pure's
dnl readline support.
LLVM_SYS_LIBS="`$LLVMCONF --system-libs 2>/dev/null | sed -e 's/-ledit[ ]*//g' -e 's/-lcurses/-lncurses/g'`"

dnl If we got LLVM installed as a shared library, use that unless the user
dnl specified --with-static-llvm.
if test "${static_llvm_libs}" != "yes"; then
save_LIBS="$LIBS"
llvm_lib_flags="-L`$LLVMCONF --libdir`"
LIBS="$LIBS ${llvm_lib_flags} -lLLVM-${llvm_version}"
AC_MSG_CHECKING([for shared LLVM library])
AC_TRY_LINK([#ifdef __cplusplus
extern "C"
#endif
char LLVMContextCreate ();],
[return LLVMContextCreate ();
 ;
 return 0;],
[LLVM_LIBS="${llvm_lib_flags} -lLLVM-${llvm_version}"; LLVM_EXE_LIBS="${LLVM_LIBS}"
AC_MSG_RESULT([$]LLVM_LIBS)],
[LLVM_LIBS="$($LLVMCONF --ldflags --libs core bitreader bitwriter linker orcjit passes support | tr '\r\n' '  ') ${LLVM_SYS_LIBS}"
LLVM_EXE_LIBS=${LLVM_SYS_LIBS}
AC_MSG_RESULT(no)])
LIBS="$save_LIBS"
else
LLVM_LIBS="$($LLVMCONF --ldflags --libs core bitreader bitwriter linker orcjit passes support | tr '\r\n' '  ') ${LLVM_SYS_LIBS}"
LLVM_EXE_LIBS=${LLVM_SYS_LIBS}
fi
AC_SUBST(LLVM_LIBS)
AC_SUBST(LLVM_EXE_LIBS)

LLVM_FLAGS=$($LLVMCONF --cppflags | sed -e s/-DNDEBUG//g)
AC_SUBST(LLVM_FLAGS)
LLVM_LDFLAGS=$($LLVMCONF --ldflags)
AC_SUBST(LLVM_LDFLAGS)

# 64 bit needs -fPIC flag (apparently not needed on OSX, though)
# FIXME: this should be set based on a check
PIC=
case $host in
*x86_64-*)
  case $host in
  *-darwin*) ;;
  *-*-mingw*) ;;
  *) PIC=-fPIC ;;
  esac
  ;;
esac
AC_SUBST(PIC)
AC_DEFINE_UNQUOTED(PURE_PIC, "${PIC}", [PIC compiler flag for shared libraries. Named PURE_PIC to avoid conflict with LLVM PassInstrumentationCallbacks parameter.])

dnl Verify LLVM headers are usable.
AC_LANG_PUSH([C++])
save_CPPFLAGS=$CPPFLAGS
CPPFLAGS="`$LLVMCONF --cppflags` $CXX17FLAG $CPPFLAGS"
AC_CHECK_HEADERS([llvm/IR/IRBuilder.h llvm/IR/Verifier.h llvm/Linker/Linker.h llvm/Support/raw_ostream.h llvm/Support/raw_os_ostream.h llvm/Support/TargetSelect.h llvm/Support/DynamicLibrary.h llvm/IR/DataLayout.h])
CPPFLAGS=$save_CPPFLAGS
AC_LANG_POP([C++])

dnl Output files.
AC_CONFIG_FILES([Makefile examples/Makefile pure.pc])
AC_CONFIG_FILES([run-test], [chmod +x run-test])
AC_CONFIG_FILES([run-tests], [chmod +x run-tests])
AC_OUTPUT

AC_MSG_RESULT([
Pure ${PACKAGE_VERSION} is now configured for LLVM ${llvm_version} on ${host}.

  Source directory:     ${srcdir}
  Installation prefix:  ${prefix}
  Versioned install:    $versioned
  C compiler:           $CC $CFLAGS $CPPFLAGS
  C++ compiler:         $CXX $CXXFLAGS $CPPFLAGS
  Linker:               $CXX $LDFLAGS $LIBS
  LLVM tool prefix:     ${TOOL_PREFIX}
  Readline support:	$rllib
  Perl regex support:	$pcrelib
  POSIX threads:	$PTHREAD_CFLAGS $PTHREAD_LIBS
  Build libpure:        $sharedlib
  fastcc/TCO support:   $fastcc

Now run 'make' to build everything, and 'make install' to install this
software on your system. To remove the installed software at a later
time use the 'make uninstall' command.
])
