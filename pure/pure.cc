
/* The Pure main program. This is currently rather simplistic. See the README
   file and the man page for details. */

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <locale.h>
#include <signal.h>
#include <readline/readline.h>
#include <readline/history.h>
#include "interpreter.hh"
#include "runtime.h"
#include "util.hh"
#include <llvm/Target/TargetOptions.h>

#include "config.h"

#ifdef HAVE_GSL
#include <gsl/gsl_errno.h>
#endif

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

#define COPYRIGHT "Copyright (c) 2009 by Albert Graef"
#define USAGE \
"Usage:           pure [options ...] [script ...] [-- args ...]\n\
                 pure [options ...] -x script [args ...]\n\
-c               Batch compilation.\n\
-g               Enable symbolic debugging.\n\
--help, -h       Print this message and exit.\n\
-i               Force interactive mode (read commands from stdin).\n\
-I directory     Add directory to search for included source files.\n\
-L directory     Add directory to search for dynamic libraries.\n\
-l libname       Library to be linked in batch compilation.\n\
--noediting      Do not use readline for command-line editing.\n\
--noprelude, -n  Do not load the prelude.\n\
--norc           Do not run the interactive startup files.\n\
-o filename      Output filename for batch compilation.\n\
-q               Quiet startup (suppresses sign-on message).\n\
-v[level]        Set debugging level (default: 1).\n\
--version        Print version information and exit.\n\
-x               Execute script with given command line arguments.\n\
--               Stop option processing.\n\
Type 'help' in the interpreter for more help.\n"
#define LICENSE "This program is free software distributed under the GNU Public License\n(GPL V3 or later). Type 'help copying' for details.\n"

static const char *commands[] = {
  "break", "cd", "clear", "const", "def", "del", "dump", "extern", "help",
  "infix", "infixl", "infixr", "let", "ls", "namespace", "nonfix", "outfix",
  "override", "postfix", "prefix", "private", "public", "pwd", "quit", "run",
  "save", "show", "stats", "underride", "using", 0
};

/* Generator functions for command completion. */

typedef map<string, symbol> symbol_map;

static char *
command_generator(const char *text, int state)
{
  static bool absname;
  static int list_index, len;
  static int32_t f, n;
  const char *name;
  assert(interpreter::g_interp);
  interpreter& interp = *interpreter::g_interp;

  /* New match. */
  if (!state) {
    list_index = 0;
    /* Must do this here, so that symbols are entered into the globalvars
       table. */
    interp.compile();
    f = 1; n = interp.symtab.nsyms();
    len = strlen(text);
    /* See whether we're looking for an absolutely qualified symbol. */
    absname = strncmp(text, "::", 2) == 0;
  }

  /* Return the next name which partially matches from the
     command list. */
  while ((name = commands[list_index])) {
    list_index++;
    if (strncmp(name, text, len) == 0)
      return strdup(name);
  }

  /* Return the next name which partially matches from the
     symbol list. */
  while (f <= n) {
    /* Skip non-toplevel symbols. */
    const symbol& sym = interp.symtab.sym(f);
    if (!interp.symtab.visible(f) ||
	(sym.prec == PREC_MAX && sym.fix != nonfix && sym.fix != outfix &&
	 interp.globenv.find(f) == interp.globenv.end() &&
	 interp.macenv.find(f) == interp.macenv.end() &&
	 interp.globalvars.find(f) == interp.globalvars.end() &&
	 interp.externals.find(f) == interp.externals.end())) {
      f++;
      continue;
    }
    const string& s = sym.s;
    f++;
    if (absname) {
      /* Absolute symbol, normalize the name. */
      if (strncmp(s.c_str(), text+2, len-2) == 0)
	return strdup(("::"+s).c_str());
    } else {
      if (strncmp(s.c_str(), text, len) == 0)
	return strdup(s.c_str());
      else {
	/* Look for a symbol in the current namespace and the search
	   namespaces. */
	size_t p = s.rfind(text);
	if (p != string::npos &&
	    (p == 0 || (p > 1 && s.compare(p-2, 2, "::") == 0))) {
	  string prefix = s.substr(0, p?p-2:p),
	    name = s.substr(p, string::npos);
	  bool found = prefix==*interp.symtab.current_namespace;
	  for (set<string>::iterator
		 it = interp.symtab.search_namespaces->begin(),
		 end = interp.symtab.search_namespaces->end();
	       !found && it != end; it++)
	    found = prefix==*it;
	  if (found)
	    return strdup(name.c_str());
	}
      }
    }
  }

  /* If no names matched, then return NULL. */
  return 0;
}

static char *
symbol_generator(const char *text, int state)
{
  static bool absname;
  static int len;
  static int32_t f, n;
  assert(interpreter::g_interp);
  interpreter& interp = *interpreter::g_interp;

  /* New match. */
  if (!state) {
    /* Must do this here, so that symbols are entered into the globalvars
       table. */
    interp.compile();
    f = 1; n = interp.symtab.nsyms();
    len = strlen(text);
    /* See whether we're looking for an absolutely qualified symbol. */
    absname = strncmp(text, "::", 2) == 0;
  }

  /* Return the next name which partially matches from the
     symbol list. */
  while (f <= n) {
    /* Skip non-toplevel symbols. */
    const symbol& sym = interp.symtab.sym(f);
    if (!interp.symtab.visible(f) ||
	(sym.prec == PREC_MAX && sym.fix != nonfix && sym.fix != outfix &&
	 interp.globenv.find(f) == interp.globenv.end() &&
	 interp.macenv.find(f) == interp.macenv.end() &&
	 interp.globalvars.find(f) == interp.globalvars.end() &&
	 interp.externals.find(f) == interp.externals.end())) {
      f++;
      continue;
    }
    const string& s = sym.s;
    f++;
    if (absname) {
      /* Absolute symbol, normalize the name. */
      if (strncmp(s.c_str(), text+2, len-2) == 0)
	return strdup(("::"+s).c_str());
    } else {
      if (strncmp(s.c_str(), text, len) == 0)
	return strdup(s.c_str());
      else {
	/* Look for a symbol in the current namespace and the search
	   namespaces. */
	size_t p = s.rfind(text);
	if (p != string::npos &&
	    (p == 0 || (p > 1 && s.compare(p-2, 2, "::") == 0))) {
	  string prefix = s.substr(0, p?p-2:p),
	    name = s.substr(p, string::npos);
	  bool found = prefix==*interp.symtab.current_namespace;
	  for (set<string>::iterator
		 it = interp.symtab.search_namespaces->begin(),
		 end = interp.symtab.search_namespaces->end();
	       !found && it != end; it++)
	    found = prefix==*it;
	  if (found)
	    return strdup(name.c_str());
	}
      }
    }
  }

  /* If no names matched, then return NULL. */
  return 0;
}

static char **
pure_completion(const char *text, int start, int end)
{
  char **matches;

  matches = (char **)NULL;

  /* If this word is at the start of the line, then it is a command to
     complete. Otherwise it is a global function or variable symbol, or the
     name of a file in the current directory. */
  if (start == 0)
    matches = rl_completion_matches(text, command_generator);
  else
    matches = rl_completion_matches(text, symbol_generator);

  return matches;
}

static void sig_handler(int sig)
{
  interpreter::brkflag = sig;
#ifdef MUST_REINSTALL_SIGHANDLERS
  signal(sig, sig_handler);
#endif
}

static const char *histfile = 0;

static void exit_handler()
{
  if (histfile) write_history(histfile);
}

#ifdef _WIN32

/* Braindead Windows doesn't have kill, so we need to set up a special kind of
   "console" event handler for Ctrl+C. That at least enables PurePad to signal
   us. */

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
  sprintf(szSigInt, "PURE_SIGINT-%u", GetCurrentProcessId());
  sprintf(szSigTerm, "PURE_SIGTERM-%u", GetCurrentProcessId());
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

int
main(int argc, char *argv[])
{
  char base;
  interpreter interp;
  int count = 0;
  bool quiet = false, force_interactive = false,
    want_prelude = true, have_prelude = false,
    want_rcfile = true, want_editing = true;
  string rcfile;
#ifdef __MINGW32__
  string outname = "a.exe";
#else
  string outname = "a.out";
#endif
  list<string> libnames;
  // This is used in advisory stack checks.
  interpreter::baseptr = &base;
  // We always ignore SIGPIPE by default.
#ifdef SIGPIPE
  signal(SIGPIPE, SIG_IGN);
#endif
  // set up an exit function which saves the history if needed
  atexit(exit_handler);
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
  if ((env = getenv("PURELIB"))) {
    string s = unixize(env);
    if (!s.empty() && s[s.size()-1] != '/') s.append("/");
    interp.libdir = s;
  } else
    interp.libdir = string(PURELIB)+"/";
  string prelude = interp.libdir+string("prelude.pure");
#if USE_FASTCC
  // This global option is needed to get tail call optimization (you'll also
  // need to have USE_FASTCC in interpreter.hh enabled).
  llvm::PerformTailCallOpt = true;
#endif
#if defined(HAVE_GSL) && DEBUG<2
  // Turn off GSL's own error handler which aborts the program.
  gsl_set_error_handler_off();
#endif
  // scan the command line options
  const string prog = *argv;
  list<string> myargs;
  for (char **args = ++argv; *args; ++args)
    if (*args == string("-h") || *args == string("--help")) {
      cout << "Pure " << PACKAGE_VERSION << " (" << HOST << ") "
	   << COPYRIGHT << endl << USAGE;
      return 0;
    } else if (*args == string("--version")) {
      cout << "Pure " << PACKAGE_VERSION << " (" << HOST << ") "
	   << COPYRIGHT << endl;
      return 0;
    } else if (*args == string("-c"))
      interp.compiling = true;
    else if (*args == string("-g"))
      interp.debugging = true;
    else if (*args == string("-i"))
      force_interactive = true;
    else if (*args == string("-n") || *args == string("--noprelude"))
      want_prelude = false;
    else if (*args == string("--norc"))
      want_rcfile = false;
    else if (*args == string("--noediting"))
      want_editing = false;
    else if (*args == string("-q"))
      quiet = true;
    else if (string(*args).substr(0,2) == "-o") {
      string s = string(*args).substr(2);
      if (s.empty()) {
	if (!*++args) {
	  interp.error(prog + ": -o lacks filename argument");
	  return 1;
	}
	s = *args;
      }
      outname = unixize(s);
    } else if (string(*args).substr(0,2) == "-l") {
      string s = string(*args).substr(2);
      if (s.empty()) {
	if (!*++args) {
	  interp.error(prog + ": -l lacks libname argument");
	  return 1;
	}
	s = *args;
      }
      libnames.push_back(unixize(s));
    } else if (string(*args).substr(0,2) == "-I") {
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
    } else if (string(*args).substr(0,2) == "-L") {
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
    } else if (string(*args).substr(0,2) == "-v") {
      string s = string(*args).substr(2);
      if (s.empty()) continue;
      char *end;
      strtoul(s.c_str(), &end, 0);
      if (*end) {
	interp.error(prog + ": invalid option " + *args);
	return 1;
      }
    } else if (*args == string("-x")) {
      while (*++args) myargs.push_back(*args);
      break;
    } else if (*args == string("--")) {
      while (*++args) myargs.push_back(*args);
      break;
    } else if (**args == '-') {
      interp.error(prog + ": invalid option " + *args);
      return 1;
    }
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
    else if (string(*argv).substr(0,2) == "-o" ||
	     string(*argv).substr(0,2) == "-l" ||
	     string(*argv).substr(0,2) == "-I" ||
	     string(*argv).substr(0,2) == "-L") {
      string s = string(*argv).substr(2);
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
    }
  if ((count > 0 || interp.compiling) && !force_interactive) {
    int status = 0;
    if (interp.compiling || interp.verbose&verbosity::dump)
      interp.compile();
    if (interp.compiling) status = interp.compiler(outname, libnames);
    //printf("status = %d\n", status);
    return (status>=0)?status:1;
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
  if (isatty(fileno(stdin)) || force_interactive) {
    // We're connected to a terminal (or pretend that we are), print the
    // sign-on message.
    if (!quiet) {
      cout << "Pure " << PACKAGE_VERSION << " (" << HOST << ") "
	   << COPYRIGHT << endl << LICENSE;
      if (have_prelude)
	cout << "Loaded prelude from " << prelude << ".\n";
      else if (want_prelude)
	cout << "\nCouldn't find the prelude. Please check your PURELIB environment variable.\n";
      cout << endl;
    }
    interp.compile();
    interp.ttymode = true;
  }
  if (want_editing && isatty(fileno(stdin))) {
    // initialize readline
    extern bool using_readline;
    using_readline = true;
    rl_readline_name = "Pure";
    rl_attempted_completion_function = pure_completion;
    using_history();
    read_history(interp.histfile.c_str());
    stifle_history(600);
    histfile = strdup(interp.histfile.c_str());
  }
  interp.temp = 1;
  if (last_modno < 0) force_interactive = false;
  if (force_interactive) interp.modno = last_modno;
  // source the initialization files, if any
  bool sticky = force_interactive;
  if (want_rcfile) {
    // Avoid reading .purerc twice, if we happen to be invoked from the user's
    // homedir.
    bool want_both = true;
    char cwd[BUFSIZE];
    if (getcwd(cwd, BUFSIZE) && (env = getenv("HOME"))) {
      char home[BUFSIZE];
      chdir(env);
      if (getcwd(home, BUFSIZE) && strcmp(home, cwd) == 0)
	want_both = false;
      chdir(cwd);
    }
    if (!rcfile.empty() && chkfile(rcfile)) {
      interp.run(rcfile, false, sticky);
      sticky = true;
    }
    if (want_both && chkfile(".purerc")) {
      interp.run(".purerc", false, sticky);
      sticky = true;
    }
    if (chkfile(".pure")) {
      interp.run(".pure", false, sticky);
      sticky = true;
    }
  }
  interp.run("", false, sticky);
  if (interp.ttymode) cout << endl;
  return 0;
}
