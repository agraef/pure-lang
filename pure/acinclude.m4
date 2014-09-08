AC_DEFUN([PURE_CHECK_TIME_H_DECLARES_DAYLIGHT], [
  AC_LANG_PUSH([C++])
  AC_CACHE_CHECK(
    [whether time.h declares the daylight variable],
    pure_cv_daylight_in_time_h,
    [AC_LINK_IFELSE([
      AC_LANG_PROGRAM([
        #include <time.h>
      ], [
        int dst = daylight;
        int tz = timezone;
      ])
    ],
    pure_cv_daylight_in_time_h=yes,
    pure_cv_daylight_in_time_h=no
    )]
  )
  AC_LANG_POP([C++])
  if test "x$pure_cv_daylight_in_time_h" = xyes; then
    AC_DEFINE(
      HAVE_DAYLIGHT_IN_TIME_H,
      1,
      [Define if time.h defines the daylight and timezone variables.])
  fi
])

AC_DEFUN([PURE_CHECK_TM_INCLUDES_TM_GMTOFF], [
  AC_CACHE_CHECK(
    [whether struct tm has tm_gmtoff member],
    pure_cv_tm_includes_tm_gmtoff,
    [AC_LINK_IFELSE([
      AC_LANG_PROGRAM([
        #include <time.h>
      ], [
        time_t t = time(NULL);
        struct tm* lt = localtime(&t);
        int off = lt->tm_gmtoff;
      ])
    ],
    pure_cv_tm_includes_tm_gmtoff=yes,
    pure_cv_tm_includes_tm_gmtoff=no
    )]
  )
  if test "x$pure_cv_tm_includes_tm_gmtoff" = xyes; then
    AC_DEFINE(
      HAVE_TM_GMTOFF_IN_TM,
      1,
      [Define if struct tm has tm_gmtoff member.])
  fi
])

AC_DEFUN([PURE_LIBGMP_PREFIX],
[
  pure_cv_lib_gmp_incdir=
  pure_cv_lib_gmp_ldpath=
  AC_ARG_WITH([libgmp-prefix],
[  --with-libgmp-prefix=DIR  search for libgmp in DIR/include and DIR/lib], [
    for dir in `echo "$withval" | tr : ' '`; do
      if test -d $dir/include; then pure_cv_lib_gmp_incdir="$dir/include"; fi
      if test -d $dir/lib; then pure_cv_lib_gmp_ldpath="$dir/lib"; fi
    done
   ])

  if test -n "$pure_cv_lib_gmp_incdir"; then
    CPPFLAGS="$CPPFLAGS -I$pure_cv_lib_gmp_incdir"
  fi
  if test -n "$pure_cv_lib_gmp_ldpath"; then
    LIBS="$LIBS -L$pure_cv_lib_gmp_ldpath -lgmp"
  fi
])

AC_DEFUN([PURE_LIBMPFR_PREFIX],
[
  pure_cv_lib_mpfr_incdir=
  pure_cv_lib_mpfr_ldpath=
  AC_ARG_WITH([libmpfr-prefix],
[  --with-libmpfr-prefix=DIR  search for libmpfr in DIR/include and DIR/lib], [
    for dir in `echo "$withval" | tr : ' '`; do
      if test -d $dir/include; then pure_cv_lib_mpfr_incdir="$dir/include"; fi
      if test -d $dir/lib; then pure_cv_lib_mpfr_ldpath="$dir/lib"; fi
    done
   ])

  if test -n "$pure_cv_lib_mpfr_incdir"; then
    CPPFLAGS="$CPPFLAGS -I$pure_cv_lib_mpfr_incdir"
  fi
  if test -n "$pure_cv_lib_mpfr_ldpath"; then
    LIBS="$LIBS -L$pure_cv_lib_mpfr_ldpath -lmpfr"
  fi
])

dnl iconv check from Bruno Haible.

AC_DEFUN([AM_ICONV],
[
  dnl Some systems have iconv in libc, some have it in libiconv (OSF/1 and
  dnl those with the standalone portable GNU libiconv installed).

  am_cv_lib_iconv_ldpath=
  AC_ARG_WITH([libiconv-prefix],
[  --with-libiconv-prefix=DIR  search for libiconv in DIR/include and DIR/lib], [
    for dir in `echo "$withval" | tr : ' '`; do
      if test -d $dir/include; then CPPFLAGS="$CPPFLAGS -I$dir/include"; fi
      if test -d $dir/lib; then am_cv_lib_iconv_ldpath="-L$dir/lib"; fi
    done
   ])

  AC_CACHE_CHECK(for iconv, am_cv_func_iconv, [
    am_cv_func_iconv="no, consider installing GNU libiconv"
    am_cv_lib_iconv=no
    AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
      [iconv_t cd = iconv_open("","");
       iconv(cd,NULL,NULL,NULL,NULL);
       iconv_close(cd);],
      am_cv_func_iconv=yes)
    if test "$am_cv_func_iconv" != yes; then
      am_save_LIBS="$LIBS"
      LIBS="$LIBS $am_cv_lib_iconv_ldpath -liconv"
      AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
        [iconv_t cd = iconv_open("","");
         iconv(cd,NULL,NULL,NULL,NULL);
         iconv_close(cd);],
        am_cv_lib_iconv=yes
        am_cv_func_iconv=yes)
      LIBS="$am_save_LIBS"
    fi
  ])
  if test "$am_cv_func_iconv" = yes; then
    AC_DEFINE(HAVE_ICONV, 1, [Define if you have the iconv() function.])
    AC_MSG_CHECKING([for iconv declaration])
    AC_CACHE_VAL(am_cv_proto_iconv, [
      AC_TRY_COMPILE([
#include <stdlib.h>
#include <iconv.h>
extern
#ifdef __cplusplus
"C"
#endif
#if defined(__STDC__) || defined(__cplusplus)
size_t iconv (iconv_t cd, char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);
#else
size_t iconv();
#endif
], [], am_cv_proto_iconv_arg1="", am_cv_proto_iconv_arg1="const")
      am_cv_proto_iconv="extern size_t iconv (iconv_t cd, $am_cv_proto_iconv_arg1 char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);"])
    am_cv_proto_iconv=`echo "[$]am_cv_proto_iconv" | tr -s ' ' | sed -e 's/( /(/'`
    AC_MSG_RESULT([$]{ac_t:-
         }[$]am_cv_proto_iconv)
    AC_DEFINE_UNQUOTED(ICONV_CONST, $am_cv_proto_iconv_arg1,
      [Define as const if the declaration of iconv() needs const.])
  fi
  LIBICONV=
  if test "$am_cv_lib_iconv" = yes; then
    LIBICONV="$am_cv_lib_iconv_ldpath -liconv"
  fi
  AC_SUBST(LIBICONV)
])

dnl nl_langinfo/CODESET check from Bruno Haible.

AC_DEFUN([AM_LANGINFO_CODESET],
[
  AC_CACHE_CHECK([for nl_langinfo and CODESET], am_cv_langinfo_codeset,
    [AC_TRY_LINK([#include <langinfo.h>],
      [char* cs = nl_langinfo(CODESET);],
      am_cv_langinfo_codeset=yes,
      am_cv_langinfo_codeset=no)
    ])
  if test $am_cv_langinfo_codeset = yes; then
    AC_DEFINE(HAVE_LANGINFO_CODESET, 1,
      [Define if you have <langinfo.h> and nl_langinfo(CODESET).])
  fi
])

dnl Check type of signal routines (posix, 4.2bsd, 4.1bsd or v7).

AC_DEFUN([AC_SIGNAL_CHECK],
[AC_REQUIRE([AC_TYPE_SIGNAL])
AC_MSG_CHECKING(for type of signal functions)
AC_CACHE_VAL(q_cv_signal_vintage,
[
  AC_TRY_LINK([#include <signal.h>],[
    sigset_t ss;
    struct sigaction sa;
    sigemptyset(&ss); sigsuspend(&ss);
    sigaction(SIGINT, &sa, (struct sigaction *) 0);
    sigprocmask(SIG_BLOCK, &ss, (sigset_t *) 0);
  ], q_cv_signal_vintage=posix,
  [
    AC_TRY_LINK([#include <signal.h>], [
	int mask = sigmask(SIGINT);
	sigsetmask(mask); sigblock(mask); sigpause(mask);
    ], q_cv_signal_vintage=4.2bsd,
    [
      AC_TRY_LINK([
	#include <signal.h>
	RETSIGTYPE foo() { }], [
		int mask = sigmask(SIGINT);
		sigset(SIGINT, foo); sigrelse(SIGINT);
		sighold(SIGINT); sigpause(SIGINT);
        ], q_cv_signal_vintage=svr3, q_cv_signal_vintage=v7
    )]
  )]
)
])
AC_MSG_RESULT($q_cv_signal_vintage)
if test "$q_cv_signal_vintage" = posix; then
AC_DEFINE(HAVE_POSIX_SIGNALS, 1, [Define if POSIX signal functions are available.])
elif test "$q_cv_signal_vintage" = "4.2bsd"; then
AC_DEFINE(HAVE_BSD_SIGNALS, 1, [Define if BSD signal functions are available.])
elif test "$q_cv_signal_vintage" = svr3; then
AC_DEFINE(HAVE_USG_SIGHOLD, 1, [Define if SVR3 signal functions are available.])
fi
])

dnl Check whether signal handlers must be reinstalled when invoked.

AC_DEFUN([AC_REINSTALL_SIGHANDLERS],
[AC_REQUIRE([AC_TYPE_SIGNAL])
AC_REQUIRE([AC_SIGNAL_CHECK])
AC_MSG_CHECKING([if signal handlers must be reinstalled when invoked])
AC_CACHE_VAL(q_cv_must_reinstall_sighandlers,
[AC_TRY_RUN([
#include <signal.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
typedef RETSIGTYPE sigfunc();
int nsigint;
#ifdef HAVE_POSIX_SIGNALS
sigfunc *
set_signal_handler(sig, handler)
     int sig;
     sigfunc *handler;
{
  struct sigaction act, oact;
  act.sa_handler = handler;
  act.sa_flags = 0;
  sigemptyset (&act.sa_mask);
  sigemptyset (&oact.sa_mask);
  sigaction (sig, &act, &oact);
  return (oact.sa_handler);
}
#else
#define set_signal_handler(s, h) signal(s, h)
#endif
RETSIGTYPE
sigint(s)
    int s;
{
  nsigint++;
}
main()
{
  nsigint = 0;
  set_signal_handler(SIGINT, sigint);
  kill((int)getpid(), SIGINT);
  kill((int)getpid(), SIGINT);
  exit(nsigint != 2);
}
], q_cv_must_reinstall_sighandlers=no, q_cv_must_reinstall_sighandlers=yes,
if test "$q_cv_signal_vintage" = svr3; then
  q_cv_must_reinstall_sighandlers=yes
else
  q_cv_must_reinstall_sighandlers=no
fi)])
if test "$cross_compiling" = yes; then
  AC_MSG_RESULT([$q_cv_must_reinstall_sighandlers assumed for cross compilation])
else
  AC_MSG_RESULT($q_cv_must_reinstall_sighandlers)
fi
if test "$q_cv_must_reinstall_sighandlers" = yes; then
  AC_DEFINE(MUST_REINSTALL_SIGHANDLERS, 1, [Define if signal handlers must be reinstalled by handler.])
fi
])

dnl readline check by Ville Laurikari <vl@iki.fi>
dnl Copyright (c) 2008 Ville Laurikari <vl@iki.fi>
dnl Copying and distribution of this file, with or without modification, are
dnl permitted in any medium without royalty provided the copyright notice
dnl and this notice are preserved.

AC_DEFUN([AC_CHECK_READLINE], [
rllib="no"
if test ! -z "$vl_readline_libs"; then
  ORIG_LIBS="$LIBS"
  AC_CACHE_CHECK([for a readline compatible library],
                 vl_cv_lib_readline, [
    for readline_lib in $vl_readline_libs; do
      for termcap_lib in "" termcap curses ncurses; do
        if test -z "$termcap_lib"; then
          TRY_LIB="-l$readline_lib"
        else
          TRY_LIB="-l$readline_lib -l$termcap_lib"
        fi
        LIBS="$ORIG_LIBS $TRY_LIB"
        AC_TRY_LINK_FUNC(readline, vl_cv_lib_readline="$TRY_LIB")
        if test -n "$vl_cv_lib_readline"; then
          break
        fi
      done
      if test -n "$vl_cv_lib_readline"; then
        break
      fi
    done
    if test -z "$vl_cv_lib_readline"; then
      vl_cv_lib_readline="no"
    fi
  ])
  rllib="$vl_cv_lib_readline"

  if test "$vl_cv_lib_readline" != "no"; then
    RL_LIBS="$vl_cv_lib_readline"
    AC_SUBST(RL_LIBS)
    AC_DEFINE(HAVE_LIBREADLINE, 1,
              [Define if you have a readline compatible library.])
    AC_CHECK_HEADERS(readline/readline.h edit/readline/readline.h editline/readline.h)
    AC_CACHE_CHECK([whether readline supports history],
                   vl_cv_lib_readline_history, [
      vl_cv_lib_readline_history="no"
      AC_TRY_LINK_FUNC(add_history, vl_cv_lib_readline_history="yes")
    ])
    if test "$vl_cv_lib_readline_history" = "yes"; then
      AC_DEFINE(HAVE_READLINE_HISTORY, 1,
                [Define if your readline library supports history.])
      AC_CHECK_HEADERS(readline/history.h edit/readline/history.h)
    fi
  fi
  LIBS="$ORIG_LIBS"
fi
])

# ============================================================================
#  http://www.gnu.org/software/autoconf-archive/ax_cxx_compile_stdcxx_11.html
# ============================================================================
#
# SYNOPSIS
#
#   AX_CXX_COMPILE_STDCXX_11([ext|noext],[mandatory|optional])
#
# DESCRIPTION
#
#   Check for baseline language coverage in the compiler for the C++11
#   standard; if necessary, add switches to CXXFLAGS to enable support.
#
#   The first argument, if specified, indicates whether you insist on an
#   extended mode (e.g. -std=gnu++11) or a strict conformance mode (e.g.
#   -std=c++11).  If neither is specified, you get whatever works, with
#   preference for an extended mode.
#
#   The second argument, if specified 'mandatory' or if left unspecified,
#   indicates that baseline C++11 support is required and that the macro
#   should error out if no mode with that support is found.  If specified
#   'optional', then configuration proceeds regardless, after defining
#   HAVE_CXX11 if and only if a supporting mode is found.
#
# LICENSE
#
#   Copyright (c) 2008 Benjamin Kosnik <bkoz@redhat.com>
#   Copyright (c) 2012 Zack Weinberg <zackw@panix.com>
#   Copyright (c) 2013 Roy Stogner <roystgnr@ices.utexas.edu>
#   Copyright (c) 2014 Alexey Sokolov <sokolov@google.com>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

m4_define([_AX_CXX_COMPILE_STDCXX_11_testbody], [[
  template <typename T>
    struct check
    {
      static_assert(sizeof(int) <= sizeof(T), "not big enough");
    };

    struct Base {
    virtual void f() {}
    };
    struct Child : public Base {
    virtual void f() override {}
    };

    typedef check<check<bool>> right_angle_brackets;

    int a;
    decltype(a) b;

    typedef check<int> check_type;
    check_type c;
    check_type&& cr = static_cast<check_type&&>(c);

    auto d = a;
    auto l = [](){};
]])

AC_DEFUN([AX_CXX_COMPILE_STDCXX_11], [dnl
  m4_if([$1], [], [],
        [$1], [ext], [],
        [$1], [noext], [],
        [m4_fatal([invalid argument `$1' to AX_CXX_COMPILE_STDCXX_11])])dnl
  m4_if([$2], [], [ax_cxx_compile_cxx11_required=true],
        [$2], [mandatory], [ax_cxx_compile_cxx11_required=true],
        [$2], [optional], [ax_cxx_compile_cxx11_required=false],
        [m4_fatal([invalid second argument `$2' to AX_CXX_COMPILE_STDCXX_11])])
  AC_LANG_PUSH([C++])dnl
  ac_success=no
  AC_CACHE_CHECK(whether $CXX supports C++11 features by default,
  ax_cv_cxx_compile_cxx11,
  [AC_COMPILE_IFELSE([AC_LANG_SOURCE([_AX_CXX_COMPILE_STDCXX_11_testbody])],
    [ax_cv_cxx_compile_cxx11=yes],
    [ax_cv_cxx_compile_cxx11=no])])
  if test x$ax_cv_cxx_compile_cxx11 = xyes; then
    ac_success=yes
  fi

  m4_if([$1], [noext], [], [dnl
  if test x$ac_success = xno; then
    for switch in -std=gnu++11 -std=gnu++0x; do
      cachevar=AS_TR_SH([ax_cv_cxx_compile_cxx11_$switch])
      AC_CACHE_CHECK(whether $CXX supports C++11 features with $switch,
                     $cachevar,
        [ac_save_CXXFLAGS="$CXXFLAGS"
         CXXFLAGS="$CXXFLAGS $switch"
         AC_COMPILE_IFELSE([AC_LANG_SOURCE([_AX_CXX_COMPILE_STDCXX_11_testbody])],
          [eval $cachevar=yes],
          [eval $cachevar=no])
         CXXFLAGS="$ac_save_CXXFLAGS"])
      if eval test x\$$cachevar = xyes; then
        CXXFLAGS="$CXXFLAGS $switch"
        ac_success=yes
        break
      fi
    done
  fi])

  m4_if([$1], [ext], [], [dnl
  if test x$ac_success = xno; then
    for switch in -std=c++11 -std=c++0x; do
      cachevar=AS_TR_SH([ax_cv_cxx_compile_cxx11_$switch])
      AC_CACHE_CHECK(whether $CXX supports C++11 features with $switch,
                     $cachevar,
        [ac_save_CXXFLAGS="$CXXFLAGS"
         CXXFLAGS="$CXXFLAGS $switch"
         AC_COMPILE_IFELSE([AC_LANG_SOURCE([_AX_CXX_COMPILE_STDCXX_11_testbody])],
          [eval $cachevar=yes],
          [eval $cachevar=no])
         CXXFLAGS="$ac_save_CXXFLAGS"])
      if eval test x\$$cachevar = xyes; then
        CXXFLAGS="$CXXFLAGS $switch"
        ac_success=yes
        break
      fi
    done
  fi])
  AC_LANG_POP([C++])
  if test x$ax_cxx_compile_cxx11_required = xtrue; then
    if test x$ac_success = xno; then
      AC_MSG_ERROR([*** A compiler with support for C++11 language features is required.])
    fi
  else
    if test x$ac_success = xno; then
      HAVE_CXX11=0
      AC_MSG_NOTICE([No compiler with C++11 support was found])
    else
      HAVE_CXX11=1
      AC_DEFINE(HAVE_CXX11,1,
                [define if the compiler supports basic C++11 syntax])
    fi

    AC_SUBST(HAVE_CXX11)
  fi
])
