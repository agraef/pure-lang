
dnl To regenerate the configury after changes:
dnl aclocal && autoconf && autoheader  (or just: autoreconf)

AC_INIT(pure, 0.59)
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
    *-*-mingw*)       EXEEXT = ".exe"; DLLEXT=".dll";
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
dnl Install elisp files?
elcfiles="etc/pure-mode.elc"
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
dnl On non-POSIX systems like Windows, we have to get the glob and regex
dnl functions from separate libraries, too.
AC_CHECK_LIB(glob, glob)
AC_CHECK_LIB(regex, regcomp)
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

dnl Check for LLVM (try various LLVM versions).
AC_CHECK_PROGS(LLVMCONF,[${llvm_config_pre} llvm-config-3.4 llvm-config-3.3 llvm-config-3.2 llvm-config-3.1 llvm-config-3.0 llvm-config-2.9 llvm-config-2.8 llvm-config-2.7 llvm-config-2.6 llvm-config-2.5],no,"$PATH:/")
if test "$LLVMCONF" = no; then
  AC_MSG_ERROR([Cannot find LLVM.])
fi

dnl Determine the LLVM version.
llvm_version=`$LLVMCONF --version`
AC_DEFINE_UNQUOTED(LLVM_VERSION, "${llvm_version}", [Define to the LLVM version string.])

dnl Some systems have the LLVM tools under /usr/lib/llvm-${llvm_version}/bin.
if test -z "${TOOL_PREFIX}" -a -x "/usr/lib/llvm-${llvm_version}/bin/opt"; then
  TOOL_PREFIX="/usr/lib/llvm-${llvm_version}/bin/"
fi
AC_SUBST(TOOL_PREFIX)
AC_DEFINE_UNQUOTED(TOOL_PREFIX, "${TOOL_PREFIX}", [Define to the prefix under which the LLVM toolchain is installed.])

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
char LLVMFreeMachineCodeForFunction ();],
[return LLVMFreeMachineCodeForFunction ();
 ;
 return 0;],
[LLVM_LIBS="${llvm_lib_flags} -lLLVM-${llvm_version}"; LLVM_EXE_LIBS="${LLVM_LIBS}"
AC_MSG_RESULT([$]LLVM_LIBS)],
[LLVM_LIBS=$($LLVMCONF --ldflags --libs core bitreader bitwriter linker jit native | tr '\r\n' '  ')
AC_MSG_RESULT(no)])
LIBS="$save_LIBS"
else
LLVM_LIBS=$($LLVMCONF --ldflags --libs core bitreader bitwriter linker jit native | tr '\r\n' '  ')
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

dnl Check for the new-style template arguments of the IRBuilder class.
AC_LANG_PUSH([C++])
save_CPPFLAGS=$CPPFLAGS
CPPFLAGS="`$LLVMCONF --cppflags` $CPPFLAGS"
AC_CACHE_CHECK([for new LLVM IRBuilder class (LLVM >= 3.3)], [pure_cv_new_builder33],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
       #include <llvm/IR/IRBuilder.h>
      ], [
        llvm::IRBuilder<> b(llvm::getGlobalContext());
      ])
    ], [pure_cv_new_builder33=yes], [pure_cv_new_builder33=no])
  ])
if test "$pure_cv_new_builder33" = yes; then
pure_cv_new_builder26=yes
else
AC_CACHE_CHECK([for new LLVM IRBuilder class (LLVM >= 3.2)], [pure_cv_new_builder32],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
       #include <llvm/IRBuilder.h>
      ], [
        llvm::IRBuilder<> b(llvm::getGlobalContext());
      ])
    ], [pure_cv_new_builder32=yes], [pure_cv_new_builder32=no])
  ])
if test "$pure_cv_new_builder32" = yes; then
pure_cv_new_builder26=yes
else
AC_CACHE_CHECK([for new LLVM IRBuilder class (LLVM >= 2.6)], [pure_cv_new_builder26],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
       #include <llvm/LLVMContext.h>
       #include <llvm/Support/IRBuilder.h>
      ], [
        llvm::IRBuilder<> b(llvm::getGlobalContext());
      ])
    ], [pure_cv_new_builder26=yes], [pure_cv_new_builder26=no])
  ])
fi
fi
if test "$pure_cv_new_builder26" = yes; then
  AC_DEFINE(NEW_BUILDER, 1, [Define when building with new-style LLVM IRBuilder template class.])
  AC_DEFINE(LLVM26, 1, [Define when building with new-style LLVM API (LLVM 2.6 or later).])
AC_CACHE_CHECK([for new llvm::raw_fd_ostream interface (LLVM >= 2.7)], [pure_cv_new_ostream],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
	#include <string>
        #include <llvm/Support/raw_ostream.h>
      ], [
  	std::string error;
        llvm::raw_fd_ostream os("xxxjunk", error, llvm::raw_fd_ostream::F_Excl);
      ])
    ], [pure_cv_new_ostream=yes], [pure_cv_new_ostream=no])
  ])
if test "$pure_cv_new_ostream" = yes; then
  AC_DEFINE(NEW_OSTREAM, 1, [Define when building with new llvm::raw_fd_ostream interface (LLVM >= 2.7).])
fi
AC_CACHE_CHECK([for new llvm::raw_fd_ostream interface (LLVM >= 3.4)], [pure_cv_new_ostream34],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
	#include <string>
        #include <llvm/Support/raw_ostream.h>
      ], [
  	std::string error;
        llvm::raw_fd_ostream os("xxxjunk", error, llvm::sys::fs::F_Excl);
      ])
    ], [pure_cv_new_ostream34=yes], [pure_cv_new_ostream34=no])
  ])
if test "$pure_cv_new_ostream34" = yes; then
  AC_DEFINE(NEW_OSTREAM34, 1, [Define when building with new llvm::raw_fd_ostream interface (LLVM >= 3.4).])
fi
else
AC_CACHE_CHECK([for new LLVM IRBuilder class (LLVM >= 2.4)], [pure_cv_new_builder],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
       #include <llvm/Support/IRBuilder.h>
      ], [
        llvm::IRBuilder<> b;
      ])
    ], [pure_cv_new_builder=yes], [pure_cv_new_builder=no])
  ])
if test "$pure_cv_new_builder" = yes; then
  AC_DEFINE(NEW_BUILDER, 1, [Define when building with new-style LLVM IRBuilder template class.])
fi
fi
AC_CHECK_HEADERS([llvm/Support/DynamicLibrary.h], [], [], [[#include <llvm/ADT/StringRef.h>]])
AC_CHECK_HEADERS([llvm/Support/raw_ostream.h llvm/Support/raw_os_ostream.h llvm/ModuleProvider.h llvm/TypeSymbolTable.h llvm/DataLayout.h llvm/IR/DataLayout.h llvm/DerivedTypes.h llvm/IRBuilder.h llvm/IR/IRBuilder.h llvm/IR/Verifier.h llvm/Support/TargetSelect.h])
if test "$pure_cv_new_builder26" = yes; then
  AC_CHECK_DECLS([llvm::PerformTailCallOpt, llvm::GuaranteedTailCallOpt], [], [], [[#include <llvm/ADT/StringRef.h>
#include <llvm/Target/TargetOptions.h>]])
else
  AC_CHECK_DECLS([llvm::PerformTailCallOpt, llvm::GuaranteedTailCallOpt], [], [], [[#include <llvm/Target/TargetOptions.h>]])
fi
AC_CHECK_DECLS([llvm::parseBitcodeFile], [], [], [[#include <llvm/Bitcode/ReaderWriter.h>]])
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
  Build libpure:        $sharedlib
  fastcc/TCO support:   $fastcc

Now run 'make' to build everything, and 'make install' to install this
software on your system. To remove the installed software at a later
time use the 'make uninstall' command.
])
