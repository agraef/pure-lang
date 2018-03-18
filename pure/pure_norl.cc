
/* This is a version of the Pure main program without readline support, which
   is provided to be used freely under a BSD-style license, see below, as a
   starting point for your own interpreter frontends. */

/* Copyright (c) 2009-2018 by Albert Graef <aggraef@gmail.com>.

   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

   3. Neither the name of the author nor the names of its contributors may be
      used to endorse or promote products derived from this software without
      specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE. */

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <locale.h>
#include <signal.h>
#include "interpreter.hh"
#include "runtime.h"
#include "util.hh"
#include <llvm/Target/TargetOptions.h>

#include "config.h"

using namespace std;

#ifndef HOST
#define HOST "unknown"
#endif
#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION "0.0"
#endif
#ifndef PURELIB
#define PURELIB "/usr/local/lib/pure-" PACKAGE_VERSION
#endif

#define COPYRIGHT "Copyright (c) 2008-2018 by Albert Graef"
#define USAGE \
"Usage:            pure [options ...] [-x] script [args ...]\n\
                  pure [options ...] [-b|-c|-i] [script ...] [-- args ...]\n\
-b                Batch mode (execute given scripts and exit).\n\
-c                Batch compilation (compile scripts to native binary).\n\
--check           Syntax check only, do not actually execute scripts.\n\
--ctags, --etags  Create a tags file in ctags (vi) or etags (emacs) format.\n\
--disable=optname Disable source option (conditional compilation).\n\
--eager-jit       Enable eager JIT compilation (LLVM 2.7 or later).\n\
--enable=optname  Enable source option (conditional compilation).\n\
--escape=char     Interactive commands are prefixed with the specified char.\n\
-fPIC             Create position-independent code (batch compilation).\n\
-g                Enable symbolic debugging.\n\
--help, -h        Print this message and exit.\n\
-i                Interactive mode (read commands from stdin after scripts).\n\
-I directory      Add directory to search for included source files.\n\
-L directory      Add directory to search for dynamic libraries.\n\
-l libname        Library to be linked in batch compilation.\n\
-mopt=val         Add llc machine options in batch compilation.\n\
--main=name       Name of main entry point in batch compilation.\n\
--noediting       Disable command-line editing.\n\
--noprelude, -n   Do not load the prelude.\n\
--norc            Do not run the interactive startup files.\n\
-o filename       Output filename for batch compilation.\n\
--plain           Minimal sign-on message (no fancy logo).\n\
-q                Quiet startup (suppresses sign-on message).\n\
-T filename       Tags file to be written by --ctags or --etags.\n\
--texmacs         Run Pure inside TeXmacs.\n\
-u                Do not strip unused functions in batch compilation.\n\
-v[level]         Set verbosity level (default: 1).\n\
--version         Print version information and exit.\n\
-w                Enable compiler warnings.\n\
-x                Execute script with given command line arguments.\n\
--                Stop option processing and pass remaining arguments.\n\
Type 'help' in the interpreter for more help.\n"
#define LICENSE \
"(Type 'help' for help, 'help copying' for license information.)\n"

static void sig_handler(int sig)
{
#ifdef MUST_REINSTALL_SIGHANDLERS
  signal(sig, sig_handler);
#endif
  interpreter::brkflag = sig;
}

static void my_exit_handler()
{
  /* Add any finalizations to be executed at exit time here. */
}

#ifdef _WIN32

/* Windows doesn't have kill, so we need to set up a special kind of "console"
   event handler for Ctrl+C. That at least enables PurePad to signal us. */

#include <windows.h>

static HANDLE hSigInt, hSigTerm, hSignalHandler;

static DWORD WINAPI SignalHandler(LPVOID dummy)
{
  HANDLE hEvents[2] = { hSigInt, hSigTerm };
  while (1) {
    DWORD ev = WaitForMultipleObjects(2, hEvents, FALSE, INFINITE);
    switch (ev) {
    case WAIT_OBJECT_0:
      raise(SIGINT);
      break;
    case WAIT_OBJECT_0+1:
      raise(SIGTERM);
      break;
    default:
      ExitProcess(0);
    }
  }
}

int InstallSignalHandler()
{
  TCHAR szSigInt[MAX_PATH], szSigTerm[MAX_PATH];
  DWORD dwSignalHandler;
  sprintf(szSigInt, "PURE_SIGINT-%u", (unsigned)GetCurrentProcessId());
  sprintf(szSigTerm, "PURE_SIGTERM-%u", (unsigned)GetCurrentProcessId());
  hSigInt = OpenEvent(EVENT_ALL_ACCESS, FALSE, szSigInt);
  hSigTerm = OpenEvent(EVENT_ALL_ACCESS, FALSE, szSigTerm);
  if (hSigInt != NULL && hSigTerm != NULL) {
    hSignalHandler = CreateThread(NULL, 0, SignalHandler, NULL,
				  0, &dwSignalHandler);
    return hSignalHandler != NULL;
  } else
    return hSigInt == hSigTerm;
}
#endif

static inline bool chkfile(const string& s)
{
  struct stat st;
  return !stat(s.c_str(), &st) && !S_ISDIR(st.st_mode);
}

static void add_path(list<string>& dirs, const string& path)
{
  size_t pos = 0;
  while (pos != string::npos) {
#ifdef _WIN32
    size_t end = path.find(';', pos);
#else
    size_t end = path.find(':', pos);
#endif
    string s;
    if (end == string::npos) {
      s = path.substr(pos);
      pos = end;
    } else {
      s = path.substr(pos, end-pos);
      pos = end+1;
    }
    if (!s.empty()) {
      if (s[s.size()-1] != '/') s.append("/");
      dirs.push_back(s);
    }
  }
}

static string unixize(const string& s)
{
  string t = s;
#ifdef _WIN32
  for (size_t i = 0, n = t.size(); i<n; i++)
    if (t[i] == '\\')
      t[i] = '/';
#endif
  return t;
}

#define BUFSIZE 1024

extern void pure_finalize(void);

int
main(int argc, char *argv[])
{
  char base;
  interpreter interp(argc, argv);
  const string prog = *argv;
  int count = 0;
  bool batch = false, quiet = false, force_interactive = false,
    want_prelude = true, have_prelude = false,
    want_rcfile = true, want_editing = true;
  string rcfile;
#ifdef __MINGW32__
  string outname = "a.exe";
#else
  string outname = "a.out";
#endif
  list<string> libnames;
  string llcopts;
  // This is used in advisory stack checks.
  interpreter::baseptr = &base;
  // We always ignore SIGPIPE by default.
#ifdef SIGPIPE
  signal(SIGPIPE, SIG_IGN);
#endif
  // set up an exit function which saves the history if needed
  atexit(my_exit_handler);
  // set the system locale
  setlocale(LC_ALL, "");
  // get some settings from the environment
  const char *env;
  if ((env = getenv("HOME"))) {
    interp.histfile = string(env)+"/.pure_history";
    rcfile = string(env)+"/.purerc";
  }
  if ((env = getenv("PURE_PS")))
    interp.ps = string(env);
  if ((env = getenv("PURE_STACK"))) {
    char *end;
    size_t n = strtoul(env, &end, 0);
    if (!*end) interpreter::stackmax = n*1024;
  }
  if ((env = getenv("PURE_NOSYMBOLIC"))) interp.symbolic = false;
  if ((env = getenv("PURE_NOCHECKS"))) interp.checks = false;
  if ((env = getenv("PURE_NOCONST"))) interp.consts = false;
  if ((env = getenv("PURE_NOFOLD"))) interp.folding = false;
  if ((env = getenv("PURE_NOTC"))) interp.use_fastcc = false;
  if ((env = getenv("PURE_EAGER_JIT"))) interp.eager_jit = true;
  if ((env = getenv("PURE_ESCAPE"))) {
    string s = string(env);
    string prefixes = ESCAPECHARS;
    if (!s.empty()) {
      if (prefixes.find(s[0]) != string::npos)
	interp.escape_mode = s[0];
      else
	interp.warning(prog + ": warning: invalid escape prefix '" +
		       s.substr(0, 1) + "'");
    }
  }
  if ((env = getenv("PURELIB"))) {
    string s = unixize(env);
    if (!s.empty() && s[s.size()-1] != '/') s.append("/");
    interp.libdir = s;
  } else
    interp.libdir = string(PURELIB)+"/";
  string prelude = interp.libdir+string("prelude.pure");
  // scan the command line options
  list<string> myargs;
  for (char **args = ++argv; *args; ++args) {
    if (**args == '-') {
      char *arg = *args;
      if (strcmp(arg, "-h") == 0 || strcmp(arg, "--help") == 0) {
	cout << "Pure " << PACKAGE_VERSION << " (" << HOST << ") "
	     << COPYRIGHT << '\n' << USAGE;
	return 0;
      } else if (strcmp(arg, "--version") == 0) {
	cout << "Pure " << PACKAGE_VERSION << " (" << HOST << ") "
	     << COPYRIGHT << '\n';
	cout << "Compiled for LLVM " << LLVM_VERSION << " (http://llvm.org)\n";
#ifdef GITREVISION
	if (strcmp(GITREVISION, "unknown"))
	  cout << "Revision " << GITREVISION << '\n';
#endif
	return 0;
      } else if (strcmp(arg, "-b") == 0) {
	batch = true; force_interactive = false;
      } else if (strcmp(arg, "-c") == 0)
	batch = interp.compiling = true;
      else if (strcmp(arg, "-fPIC") == 0 || strcmp(arg, "-fpic") == 0)
	interp.pic = true;
      else if (strcmp(arg, "-g") == 0)
	interp.debugging = true;
      else if (strcmp(arg, "-i") == 0)
	batch = force_interactive = true;
      else if (strcmp(arg, "--ctags") == 0) {
	batch = true; interp.tags = 1;
      } else if (strcmp(arg, "--etags") == 0) {
	batch = true; interp.tags = 2;
      } else if (strcmp(arg, "--check") == 0) {
	// this works basically like tags mode, but only does a syntax check
	// without generating any other output
	batch = true; interp.tags = -1;
      } else if (strcmp(arg, "--eager-jit") == 0)
	interp.eager_jit = true;
      else if (strcmp(arg, "-n") == 0 || strcmp(arg, "--noprelude") == 0)
	want_prelude = false;
      else if (strcmp(arg, "--norc") == 0)
	want_rcfile = false;
      else if (strcmp(arg, "--noediting") == 0)
	want_editing = false;
      else if (strcmp(arg, "--nosymbolic") == 0)
	interp.symbolic = false;
      else if (strcmp(arg, "--symbolic") == 0)
	interp.symbolic = true;
      else if (strcmp(arg, "--nochecks") == 0)
	interp.checks = false;
      else if (strcmp(arg, "--checks") == 0)
	interp.checks = true;
      else if (strcmp(arg, "--noconst") == 0)
	interp.consts = false;
      else if (strcmp(arg, "--const") == 0)
	interp.consts = true;
      else if (strcmp(arg, "--nofold") == 0)
	interp.folding = false;
      else if (strcmp(arg, "--fold") == 0)
	interp.folding = true;
      else if (strcmp(arg, "--notc") == 0)
	interp.use_fastcc = false;
      else if (strcmp(arg, "--tc") == 0)
	interp.use_fastcc = true;
      else if (strcmp(arg, "-q") == 0)
	quiet = true;
      else if (strcmp(arg, "-s") == 0)
	interp.strip = true;
      else if (strcmp(arg, "-u") == 0)
	interp.strip = false;
      else if (strcmp(arg, "-w") == 0)
	interp.compat = true;
      else if (strcmp(arg, "-w2") == 0)
	interp.compat2 = true;
      else if (strcmp(*args, "--escape") == 0 ||
	       strncmp(*args, "--escape=", 9) == 0) {
	string s = string(*args).substr(8);
	if (s.empty()) {
	  if (!*++args) {
	    interp.error(prog + ": --escape lacks option argument");
	    return 1;
	  }
	  s = *args;
	} else
	  s.erase(0, 1);
	string prefixes = ESCAPECHARS;
	if (s.empty())
	  interp.escape_mode = 0;
	else if (prefixes.find(s[0]) != string::npos)
	  interp.escape_mode = s[0];
	else
	  interp.warning(prog + ": warning: invalid escape prefix '" +
			 s.substr(0, 1) + "'");
      } else if (strcmp(*args, "--enable") == 0 ||
	       strncmp(*args, "--enable=", 9) == 0) {
	string s = string(*args).substr(8);
	if (s.empty()) {
	  if (!*++args) {
	    interp.error(prog + ": --enable lacks option argument");
	    return 1;
	  }
	  s = *args;
	} else
	  s.erase(0, 1);
	interp.enable(s, true);
      } else if (strcmp(*args, "--disable") == 0 ||
		 strncmp(*args, "--disable=", 10) == 0) {
	string s = string(*args).substr(9);
	if (s.empty()) {
	  if (!*++args) {
	    interp.error(prog + ": --disable lacks option argument");
	    return 1;
	  }
	  s = *args;
	} else
	  s.erase(0, 1);
	interp.enable(s, false);
      } else if (strcmp(*args, "--main") == 0 ||
		 strncmp(*args, "--main=", 7) == 0) {
	string s = string(*args).substr(6);
	if (s.empty()) {
	  if (!*++args) {
	    interp.error(prog + ": --main lacks name argument");
	    return 1;
	  }
	  s = *args;
	} else
	  s.erase(0, 1);
	interp.mainname = s;
      } else if (strncmp(*args, "-T", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) {
	  if (!*++args) {
	    interp.error(prog + ": -T lacks filename argument");
	    return 1;
	  }
	  s = *args;
	}
	interp.tagsfile = unixize(s);
      } else if (strncmp(*args, "-o", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) {
	  if (!*++args) {
	    interp.error(prog + ": -o lacks filename argument");
	    return 1;
	  }
	  s = *args;
	}
	outname = unixize(s);
      } else if (strncmp(*args, "-l", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) {
	  if (!*++args) {
	    interp.error(prog + ": -l lacks libname argument");
	    return 1;
	  }
	  s = *args;
	}
	libnames.push_back(unixize(s));
      } else if (strncmp(*args, "-m", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) {
	  interp.error(prog + ": -m lacks option argument");
	  return 1;
	}
	if (llcopts.empty())
	  llcopts = *args;
	else {
	  llcopts += " ";
	  llcopts += *args;
	}
      } else if (strncmp(*args, "-I", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) {
	  if (!*++args) {
	    interp.error(prog + ": -I lacks directory argument");
	    return 1;
	  }
	  s = *args;
	}
	s = unixize(s);
	if (!s.empty()) {
	  if (s[s.size()-1] != '/') s.append("/");
	  interp.includedirs.push_back(s);
	}
      } else if (strncmp(*args, "-L", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) {
	  if (!*++args) {
	    interp.error(prog + ": -L lacks directory argument");
	    return 1;
	  }
	  s = *args;
	}
	s = unixize(s);
	if (!s.empty()) {
	  if (s[s.size()-1] != '/') s.append("/");
	  interp.librarydirs.push_back(s);
	}
      } else if (strncmp(*args, "-v", 2) == 0) {
	string s = string(*args).substr(2);
	if (s.empty()) continue;
	char *end;
	if (strtoul(s.c_str(), &end, 0)) {}
	if (*end) {
	  interp.error(prog + ": invalid option " + *args);
	  return 1;
	}
      } else if (strcmp(arg, "-x") == 0) {
	while (*++args) myargs.push_back(*args);
	break;
      } else if (strcmp(arg, "--") == 0) {
	while (*++args) myargs.push_back(*args);
	break;
      } else {
	interp.error(prog + ": invalid option " + *args);
	return 1;
      }
    } else if (!batch) {
      while (*args) myargs.push_back(*args++);
      break;
    }
  }
#if USE_FASTCC && !LLVM31
  // This global option is needed to get tail call optimization (you'll also
  // need to have USE_FASTCC in interpreter.hh enabled).
  if (interp.use_fastcc) llvm::GuaranteedTailCallOpt = true;
#endif
  interp.init_jit_mode();
  if ((env = getenv("PURE_INCLUDE")))
    add_path(interp.includedirs, unixize(env));
  if ((env = getenv("PURE_LIBRARY")))
    add_path(interp.librarydirs, unixize(env));
  if (force_interactive) interp.compiling = false;
  interp.init_sys_vars(PACKAGE_VERSION, HOST, myargs);
  if (want_prelude) {
    // load the prelude if we can find it
    if (chkfile(prelude)) {
      have_prelude = true;
      try { interp.run(prelude, false); } catch (err &e) {
	interp.error(prog + ": " + e.what());
	return 1;
      }
      interp.compile();
    }
  }
  // load scripts specified on the command line
  int32_t last_modno = interp.modno;
  interp.interactive_mode = force_interactive;
  for (; *argv; ++argv)
    if (string(*argv).substr(0,2) == "-v") {
      uint8_t level = 1;
      string s = string(*argv).substr(2);
      if (!s.empty()) level = (uint8_t)strtoul(s.c_str(), 0, 0);
      interp.verbose = level;
    } else if (*argv == string("-x")) {
      if (*++argv) {
	count++; interp.modname = *argv;
	last_modno = interp.modctr;
	try { interp.run(*argv, false); } catch (err &e) {
	  interp.error(prog + ": " + e.what());
	  return 1;
	}
      } else {
	interp.error(prog + ": missing script name");
	return 1;
      }
      break;
    } else if (*argv == string("--"))
      break;
    else if (string(*argv).substr(0,2) == "-T" ||
	     string(*argv).substr(0,2) == "-o" ||
	     string(*argv).substr(0,2) == "-l" ||
	     string(*argv).substr(0,2) == "-I" ||
	     string(*argv).substr(0,2) == "-L") {
      string s = string(*argv).substr(2);
      if (s.empty()) ++argv;
    } else if (string(*argv).substr(0,8) == "--escape") {
      string s = string(*argv).substr(8);
      if (s.empty()) ++argv;
    } else if (string(*argv).substr(0,8) == "--enable") {
      string s = string(*argv).substr(8);
      if (s.empty()) ++argv;
    } else if (string(*argv).substr(0,9) == "--disable") {
      string s = string(*argv).substr(9);
      if (s.empty()) ++argv;
    } else if (string(*argv).substr(0,6) == "--main") {
      string s = string(*argv).substr(6);
      if (s.empty()) ++argv;
    } else if (**argv == '-')
      ;
    else if (**argv) {
      if (count++ == 0) interp.modname = *argv;
      last_modno = interp.modctr;
      try { interp.run(*argv, false); } catch (err &e) {
	interp.error(prog + ": " + e.what());
	return 1;
      }
      if (!batch) break;
    }
  if ((count > 0 || interp.compiling || interp.tags) && !force_interactive) {
    int status = 0;
    if (interp.tags)
      interp.print_tags();
    else {
      if (interp.compiling || interp.verbose&verbosity::dump)
	interp.compile();
      if (interp.compiling)
	status = interp.compiler(outname, libnames, llcopts);
      //printf("status = %d\n", status);
    }
    /* interp.compiler() apparently leaves the code module in a dangling
       state, so make sure that we take the quick way out. There's really no
       need to clean up the interpreter instance if we're exiting anyway. */
    pure_finalize();
    exit((status>=0)?status:1);
  }
  interp.symtab.init_builtins();
  /* Only when running interactively, set up handlers for all standard POSIX
     termination signals (except SIGKILL which is unmaskable). These are
     mapped to Pure exceptions of the form 'signal SIG', so that they can be
     caught with 'catch' or safely return us to the interpreter's interactive
     command line. */
#ifdef SIGHUP
  signal(SIGHUP, sig_handler);
#endif
#ifdef SIGINT
  signal(SIGINT, sig_handler);
#endif
#ifdef SIGALRM
  signal(SIGALRM, sig_handler);
#endif
#ifdef SIGTERM
  signal(SIGTERM, sig_handler);
#endif
#ifdef SIGUSR1
  signal(SIGUSR1, sig_handler);
#endif
#ifdef SIGUSR2
  signal(SIGUSR2, sig_handler);
#endif
#ifdef _WIN32
  InstallSignalHandler();
#endif
  // enter the interactive command loop
  interp.interactive = true;
  interp.interactive_mode = isatty(fileno(stdin)) || force_interactive;
  if (interp.interactive_mode) {
    // We're connected to a terminal (or pretend that we are), print the
    // sign-on message.
    if (!quiet) {
      cout << "Pure " << PACKAGE_VERSION << " (" << HOST << ") "
	   << COPYRIGHT << '\n' << LICENSE;
      if (have_prelude)
	cout << "Loaded prelude from " << prelude << ".\n";
      else if (want_prelude)
	cout << "\nCouldn't find the prelude. Please check your PURELIB environment variable.\n";
      cout << endl;
    }
    interp.compile();
    interp.ttymode = true;
  }
  interp.temp = 1;
  if (last_modno < 0) force_interactive = false;
  // create a new module for the interactive scope
  interp.modno = interp.modctr++;
  delete interp.symtab.current_namespace;
  delete interp.symtab.search_namespaces;
  interp.symtab.current_namespace = new string;
  interp.symtab.search_namespaces = new map< string, set<int32_t> >;
  // source the initialization files, if any
  if (want_rcfile) {
    // Avoid reading .purerc twice, if we happen to be invoked from the user's
    // homedir.
    bool want_both = true;
    char cwd[BUFSIZE];
    if (getcwd(cwd, BUFSIZE) && (env = getenv("HOME"))) {
      char home[BUFSIZE];
      if (chdir(env)) perror("chdir");
      if (getcwd(home, BUFSIZE) && strcmp(home, cwd) == 0)
	want_both = false;
      if (chdir(cwd)) perror("chdir");
    }
    if (!rcfile.empty() && chkfile(rcfile))
      interp.run(rcfile, false, true);
    if (want_both && chkfile(".purerc"))
      interp.run(".purerc", false, true);
    if (chkfile(".pure"))
      interp.run(".pure", false, true);
  }
  interp.run("", false, true);
  if (interp.ttymode) cout << endl;
  /* Take the quick way out. There's really no need to clean up the
     interpreter instance if we're exiting anyway. */
  pure_finalize();
  exit(0);
}
