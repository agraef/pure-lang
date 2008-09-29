%{                                            /* -*- C++ -*- */
#include <cstdlib>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <fnmatch.h>
#include <time.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <string>
#include <sstream>
#include <fstream>
#include "interpreter.hh"
#include "parser.hh"
#include "util.hh"

#include "config.h"

/* Work around an incompatibility in flex (at least versions 2.5.31 through
   2.5.33): it generates code that does not conform to C89.  See Debian bug
   333231 <http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=333231>.  */
# undef yywrap
# define yywrap() 1

/* By default yylex returns int, we use token_type.  Unfortunately yyterminate
   by default returns 0, which is not of token_type.  */
#define yyterminate() return yy::parser::token_type(0)

using namespace std;

static void my_readline(const char *prompt, char *buf, int &result, int max_size);

#define YY_INPUT(buf,result,max_size)					\
  if (interpreter::g_interp->source_s) {				\
    size_t l = strlen(interpreter::g_interp->source_s);			\
    if (l > (size_t)max_size) l = (size_t)max_size;			\
    memcpy(buf, interpreter::g_interp->source_s, l);			\
    interpreter::g_interp->source_s += result = l;			\
  } else if ( interpreter::g_interactive &&				\
	      interpreter::g_interp->ttymode )				\
    my_readline(interpreter::g_interp->ps.c_str(),			\
		buf, result, max_size);					\
  else {								\
    errno=0;								\
    while ((result = fread(buf, 1, max_size, yyin))==0 && ferror(yyin)) \
    {									\
      if( errno != EINTR) {						\
	YY_FATAL_ERROR("input in flex scanner failed");			\
	break;								\
      }									\
      errno=0;								\
      clearerr(yyin);							\
    }									\
  }

typedef yy::parser::token token;

static yy::parser::token_type optoken[10][5] = {
  {token::NA0, token::LT0, token::RT0, token::PR0, token::PO0},
  {token::NA1, token::LT1, token::RT1, token::PR1, token::PO1},
  {token::NA2, token::LT2, token::RT2, token::PR2, token::PO2},
  {token::NA3, token::LT3, token::RT3, token::PR3, token::PO3},
  {token::NA4, token::LT4, token::RT4, token::PR4, token::PO4},
  {token::NA5, token::LT5, token::RT5, token::PR5, token::PO5},
  {token::NA6, token::LT6, token::RT6, token::PR6, token::PO6},
  {token::NA7, token::LT7, token::RT7, token::PR7, token::PO7},
  {token::NA8, token::LT8, token::RT8, token::PR8, token::PO8},
  {token::NA9, token::LT9, token::RT9, token::PR9, token::PO9},
};

struct argl {
  bool ok;
  size_t c;
  list<string> l;
  argl(const char *s, const char *m)
  {
    ok = false; c = 0;
    while (isspace(*s)) ++s;
    if (*s) do {
      const char *r = s, delim = (*r=='"'||*r=='\'')?*r:0;
      if (delim) {
	r = ++s;
	while (*r && *r != delim) ++r;
	if (*r != delim) {
	  cerr << m << ": syntax error, missing string delimiter\n";
	  return;
	}
      } else {
	while (*r && !isspace(*r)) ++r;
      }
      string t = string(s).substr(0, r-s);
      s += t.size();
      if (delim) {
	char *msg;
	++s;
	char *tmp = parsestr(t.c_str(), msg);
	char *tmp2 = fromutf8(tmp);
	t = tmp2;
	free(tmp); free(tmp2);
	if (msg) {
	  cerr << m << ": " << msg << endl;
	  return;
	}
      }
      l.push_back(t); c++;
      while (isspace(*s)) ++s;
    } while (*s);
    ok = true;
  }
};

typedef map<int32_t,ExternInfo> extmap;

struct env_sym {
  const symbol* sym;
  env::const_iterator it, jt;
  extmap::const_iterator xt;
  env_sym(const symbol& _sym, env::const_iterator _it,
	  env::const_iterator _jt,
	  extmap::const_iterator _xt)
    : sym(&_sym), it(_it), jt(_jt), xt(_xt) { }
};

static bool env_compare(env_sym s, env_sym t)
{
  return s.sym->s < t.sym->s;
}

/* This is a watered-down version of the command completion routine from
   pure.cc, used to implement the completion_matches command used by
   pure-mode.el. This isn't perfect, since it will also complete command names
   when not at the beginning of the line, but since the location inside the
   line isn't passed to completion_matches, it's the best that we can do right
   now. */

static const char *commands[] = {
  "cd", "clear", "const", "def", "dump", "extern", "help", "infix", "infixl",
  "infixr", "let", "ls", "nullary", "override", "postfix", "prefix",
  "private", "pwd", "quit", "run", "save", "show", "stats", "underride",
  "using", 0
};

typedef map<string, symbol> symbol_map;

static char *
command_generator(const char *text, int state)
{
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
    if (sym.modno >= 0 && sym.modno != interp.modno ||
	interp.globenv.find(f) == interp.globenv.end() &&
	interp.macenv.find(f) == interp.macenv.end() &&
	interp.globalvars.find(f) == interp.globalvars.end() &&
	interp.externals.find(f) == interp.externals.end()) {
      f++;
      continue;
    }
    const string& s = sym.s;
    f++;
    if (strncmp(s.c_str(), text, len) == 0)
      return strdup(s.c_str());
  }

  /* If no names matched, then return NULL. */
  return 0;
}

static char **
pure_completion(const char *text, int start, int end)
{
  return rl_completion_matches(text, command_generator);
}

static void list_completions(const char *s)
{
  char **matches = pure_completion(s, 0, strlen(s));
  if (matches) {
    if (matches[0])
      if (!matches[1]) {
	printf("%s\n", matches[0]);
	free(matches[0]);
      } else {
	int i;
	free(matches[0]);
	for (i = 1; matches[i]; i++) {
	  printf("%s\n", matches[i]);
	  free(matches[i]);
	}
      }
    free(matches);
  }
}

static void check(const yy::location& l, const char* s)
{
  static set<string> done;
  const char *name;
  size_t i = 0;
  while ((name = commands[i++]))
    /* We warn about each identifier at most once. FIXME: We should also check
       whether the interpreter is running in (global) interactive mode, but at
       present this information isn't available before we actually enter the
       interactive loop, when all source files from the command line have
       already been processed. */
    if (strcmp(name, s) == 0 && done.find(s) == done.end()) {
      assert(interpreter::g_interp);
      interpreter& interp = *interpreter::g_interp;
      interp.warning(l, "warning: identifier '"+string(s)+
		     "' is also an interpreter command");
      done.insert(s);
      return;
    }
}
%}

%option noyywrap nounput debug

id    [a-zA-Z_][a-zA-Z_0-9]*
int   [0-9]+|0[0-7]+|0[xX][0-9a-fA-F]+
exp   ([Ee][+-]?[0-9]+)
float [0-9]+{exp}|[0-9]+\.{exp}|[0-9]*\.[0-9]+{exp}?
str   ([^\"\\\n]|\\(.|\n))*
blank [ \t\f\v\r]

inttag  ::{blank}*int
binttag ::{blank}*bigint
dbltag  ::{blank}*double
strtag  ::{blank}*string
ptrtag  ::{blank}*pointer
mattag  ::{blank}*matrix

%x comment xdecl xdecl_comment xusing xusing_comment

%{
# define YY_USER_ACTION  yylloc->columns(yyleng);
%}

%%

%{
  yylloc->step();
%}

{blank}+   yylloc->step();
[\n]+      yylloc->lines(yyleng); yylloc->step();

^"#!".*    |
"//".*     yylloc->step();

"/*"       BEGIN(comment);
     
<comment>[^*\n]*        yylloc->step();
<comment>"*"+[^*/\n]*   yylloc->step();
<comment>[\n]+          yylloc->lines(yyleng); yylloc->step();
<comment>"*"+"/"        yylloc->step(); BEGIN(INITIAL);

<xdecl>{id}	check(*yylloc, yytext); yylval->sval = new string(yytext); return token::ID;
<xdecl>[()*,=]	return yy::parser::token_type(yytext[0]);
<xdecl>"//".*	yylloc->step();
<xdecl>"/*"	BEGIN(xdecl_comment);
<xdecl>;	BEGIN(INITIAL); return yy::parser::token_type(yytext[0]);
<xdecl>{blank}+	yylloc->step();
<xdecl>[\n]+	yylloc->lines(yyleng); yylloc->step();
<xdecl>.	{
  string msg = "invalid character '"+string(yytext)+"'";
  interp.error(*yylloc, msg);
  BEGIN(INITIAL); return token::ERRTOK;
}
     
<xdecl_comment>[^*\n]*        yylloc->step();
<xdecl_comment>"*"+[^*/\n]*   yylloc->step();
<xdecl_comment>[\n]+          yylloc->lines(yyleng); yylloc->step();
<xdecl_comment>"*"+"/"        yylloc->step(); BEGIN(xdecl);

<xusing>{id}	check(*yylloc, yytext); yylval->sval = new string(yytext); return token::ID;
<xusing>,	return yy::parser::token_type(yytext[0]);
<xusing>"//".*	yylloc->step();
<xusing>"/*"	BEGIN(xusing_comment);
<xusing>;	BEGIN(INITIAL); return yy::parser::token_type(yytext[0]);
<xusing>{blank}+	yylloc->step();
<xusing>[\n]+	yylloc->lines(yyleng); yylloc->step();
<xusing>\"{str}\"   {
  char *msg;
  yytext[yyleng-1] = 0;
  yylval->csval = parsestr(yytext+1, msg);
  yytext[yyleng-1] = '"';
  if (msg) interp.error(*yylloc, msg);
  return token::STR;
}
<xusing>\"{str}      {
  interp.error(*yylloc, "unterminated string constant");
  BEGIN(INITIAL); return token::ERRTOK;
}
<xusing>.	{
  string msg = "invalid character '"+string(yytext)+"'";
  interp.error(*yylloc, msg);
  BEGIN(INITIAL); return token::ERRTOK;
}
     
<xusing_comment>[^*\n]*        yylloc->step();
<xusing_comment>"*"+[^*/\n]*   yylloc->step();
<xusing_comment>[\n]+          yylloc->lines(yyleng); yylloc->step();
<xusing_comment>"*"+"/"        yylloc->step(); BEGIN(xusing);

^!{blank}*.* {
  // shell escape is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext;
  ++s;
  while (isspace(*s)) ++s;
  yylloc->step();
  system(s);
}
^help.* {
  // help command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext+4;
  if (*s && !isspace(*s)) REJECT;
  while (isspace(*s)) ++s;
  yylloc->step();
  string cmd = string("man ") + ((*s)?s:("pure-" PACKAGE_VERSION));
  system(cmd.c_str());
}
^ls.* {
  // ls command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext;
  if (s[2] && !isspace(s[2])) REJECT;
  yylloc->step();
  system(s);
}
^pwd.* {
  // pwd command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext;
  if (s[3] && !isspace(s[3])) REJECT;
  yylloc->step();
  system(s);
}
^cd.* {
  static const char *home = getenv("HOME");
  // cd command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext+2;
  if (*s && !isspace(*s)) REJECT;
  yylloc->step();
  argl args(s, "cd");
  if (!args.ok)
    ;
  else if (args.c == 0) {
    if (!home) home = "/";
    if (chdir(home)) perror("cd");
  } else if (args.c > 1)
    cerr << "cd: extra parameter\n";
  else if (chdir(args.l.begin()->c_str()))
    perror("cd");
}
^show.* {
  // show command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  uint8_t s_verbose = interpreter::g_verbose;
  uint8_t tflag = 0; int pflag = -1;
  bool aflag = false, dflag = false, eflag = false;
  bool cflag = false, fflag = false, mflag = false, vflag = false;
  bool gflag = false, lflag = false, sflag = false;
  const char *s = yytext+4;
  if (*s && !isspace(*s)) REJECT;
  yylloc->step();
  argl args(s, "show");
  list<string>::iterator arg;
  if (!args.ok) goto out;
  // process option arguments
  for (arg = args.l.begin(); arg != args.l.end(); arg++) {
    const char *s = arg->c_str();
    if (s[0] != '-' || !s[1] || !strchr("acdefghlmpstv", s[1])) break;
    while (*++s) {
      switch (*s) {
      case 'a': aflag = true; break;
      case 'c': cflag = true; break;
      case 'd': dflag = true; break;
      case 'e': eflag = true; break;
      case 'f': fflag = true; break;
      case 'g': gflag = true; break;
      case 'l': lflag = true; break;
      case 'm': mflag = true; break;
      case 'p':
	if (isdigit(s[1])) {
	  pflag = strtoul(s+1, 0, 10)>0;
	  while (isdigit(s[1])) ++s;
	} else
	  pflag = 1;
	break;
      case 's': sflag = true; break;
      case 'v': vflag = true; break;
      case 't':
	if (isdigit(s[1])) {
	  tflag = strtoul(s+1, 0, 10);
	  while (isdigit(s[1])) ++s;
	} else
	  tflag = interp.temp;
	break;
      case 'h':
	cout <<
"show command help: show [options ...] [symbol ...]\n\
Options may be combined, e.g., show -tvl is the same as show -t -v -l.\n\
-a  Disassembles pattern matching automata. Useful for debugging purposes.\n\
-c  Print information about defined constants.\n\
-d  Disassembles LLVM IR, showing the generated LLVM assembler code of a\n\
    function. Useful for debugging purposes.\n\
-e  Annotate printed definitions with lexical environment information\n\
    (de Bruijn indices, subterm paths). Useful for debugging purposes.\n\
-f  Print information about defined functions.\n\
-g  Indicates that the following symbols are actually shell glob patterns\n\
    and that all matching symbols should be listed.\n\
-h  Print this list.\n\
-l  Long format, prints definitions along with the summary symbol\n\
    information. This implies -s.\n\
-m  Print information about defined macros.\n\
-p[flag] List only private symbols in the current module if flag is\n\
    nonzero (the default), otherwise list only public symbols of all\n\
    modules. List both private and public symbols if -p is omitted.\n\
-s  Summary format, print just summary information about listed symbols.\n\
-t[level] List only symbols and definitions at the given temporary level\n\
    (the current level by default) or above. Level 1 denotes all temporary\n\
    definitions, level 0 *all* definitions (the default if -t is omitted).\n\
-v  Print information about defined variables.\n";
	goto out;
      default:
	cerr << "show: invalid option character '" << *s << "'\n";
	goto out;
      }
    }
  }
  args.l.erase(args.l.begin(), arg);
  if (eflag) interpreter::g_verbose |= verbosity::envs;
  if (aflag) interpreter::g_verbose |= verbosity::code;
  if (dflag) interpreter::g_verbose |= verbosity::dump;
  if (!cflag && !fflag && !mflag && !vflag)
    cflag = fflag = mflag = vflag = true;
  if (lflag) sflag = true;
  {
    size_t maxsize = 0, nfuns = 0, nmacs = 0, nvars = 0, ncsts = 0,
      nrules = 0, mrules = 0;
    list<env_sym> l; set<int32_t> syms;
    for (env::const_iterator it = interp.globenv.begin();
	 it != interp.globenv.end(); ++it) {
      int32_t f = it->first;
      const env_info& e = it->second;
      const symbol& sym = interp.symtab.sym(f);
      if (sym.modno >= 0 && sym.modno != interp.modno ||
	  pflag >= 0 && (pflag > 0) != (sym.modno >= 0) ||
	  !((e.t == env_info::fun)?fflag:
	    (e.t == env_info::cvar)?cflag:
	    (e.t == env_info::fvar)?vflag:0))
	continue;
      bool matches = e.temp >= tflag;
      if (!matches && !sflag && args.l.empty() &&
	  e.t == env_info::fun && fflag) {
	// if not in summary mode, also list temporary rules for a
	// non-temporary symbol
	rulel::const_iterator r;
	for (r = e.rules->begin(); r != e.rules->end(); r++)
	  if (r->temp >= tflag) {
	    matches = true;
	    break;
	  }
      }
      if (!matches) continue;
      if (!args.l.empty()) {
	// see whether we actually want the defined symbol to be listed
	matches = false;
	for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
	  if (gflag ? (!fnmatch(arg->c_str(), sym.s.c_str(), 0)) :
	      (*arg == sym.s)) {
	    matches = true;
	    break;
	  }
	}
      }
      if (!matches) continue;
      syms.insert(f);
      l.push_back(env_sym(sym, it, interp.macenv.find(f),
			  interp.externals.find(f)));
      if (sym.s.size() > maxsize) maxsize = sym.s.size();
    }
    if (fflag && tflag == 0) {
      // also process the declared externals which don't have any rules yet
      for (extmap::const_iterator it = interp.externals.begin();
	   it != interp.externals.end(); ++it) {
	int32_t f = it->first;
	if (syms.find(f) == syms.end()) {
	  const symbol& sym = interp.symtab.sym(f);
	  if (sym.modno >= 0 && sym.modno != interp.modno ||
	      pflag >= 0 && (pflag > 0) != (sym.modno >= 0))
	    continue;
	  bool matches = true;
	  if (!args.l.empty()) {
	    matches = false;
	    for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
	      if (gflag ? (!fnmatch(arg->c_str(), sym.s.c_str(), 0)) :
		  (*arg == sym.s)) {
		matches = true;
		break;
	      }
	    }
	  }
	  if (!matches) continue;
	  l.push_back(env_sym(sym, interp.globenv.end(),
			      interp.macenv.find(f), it));
	  if (sym.s.size() > maxsize) maxsize = sym.s.size();
	}
      }
    }
    if (mflag) {
      // also list any symbols defined as macros, unless they've already been
      // considered
      for (env::const_iterator it = interp.macenv.begin();
	   it != interp.macenv.end(); ++it) {
	int32_t f = it->first;
	if (syms.find(f) == syms.end()) {
	  const env_info& e = it->second;
	  const symbol& sym = interp.symtab.sym(f);
	  if (sym.modno >= 0 && sym.modno != interp.modno ||
	      pflag >= 0 && (pflag > 0) != (sym.modno >= 0))
	    continue;
	  bool matches = e.temp >= tflag;
	  if (!matches && !sflag && args.l.empty()) {
	    // if not in summary mode, also list temporary rules for a
	    // non-temporary symbol
	    rulel::const_iterator r;
	    for (r = e.rules->begin(); r != e.rules->end(); r++)
	      if (r->temp >= tflag) {
		matches = true;
		break;
	      }
	  }
	  if (!matches) continue;
	  if (!args.l.empty()) {
	    // see whether we actually want the defined symbol to be listed
	    matches = false;
	    for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
	      if (gflag ? (!fnmatch(arg->c_str(), sym.s.c_str(), 0)) :
		  (*arg == sym.s)) {
		matches = true;
		break;
	      }
	    }
	  }
	  if (!matches) continue;
	  syms.insert(f);
	  l.push_back(env_sym(sym, interp.globenv.end(), it,
			      interp.externals.end()));
	  if (sym.s.size() > maxsize) maxsize = sym.s.size();
	}
      }
    }
    l.sort(env_compare);
    if (!l.empty() && (aflag||dflag)) interp.compile();
    // we first dump the entire listing into a string and then output that
    // string through more
    ostringstream sout;
    for (list<env_sym>::const_iterator it = l.begin();
	 it != l.end(); ++it) {
      const symbol& sym = *it->sym;
      int32_t ftag = sym.f;
      map<int32_t,Env>::iterator fenv = interp.globalfuns.find(ftag);
      const env::const_iterator jt = it->it, kt = it->jt;
      const extmap::const_iterator xt = it->xt;
      if (jt == interp.globenv.end() && kt == interp.macenv.end()) {
	assert(xt != interp.externals.end());
	const ExternInfo& info = xt->second;
	sout << info << ";";
	if ((!sflag||lflag) && dflag) {
	  if (!sflag) sout << endl;
	  info.f->print(sout);
	} else
	  sout << endl;
	++nfuns;
      } else if (jt != interp.globenv.end() &&
		 jt->second.t == env_info::fvar) {
	nvars++;
	if (sflag) {
	  sout << sym.s << string(maxsize-sym.s.size(), ' ')
	       << "  var";
	  if (lflag) sout << "  " << sym.s << " = "
			  << *(pure_expr**)jt->second.val << ";";
	  sout << endl;
	} else
	  sout << "let " << sym.s << " = " << *(pure_expr**)jt->second.val
	       << ";\n";
      } else if (jt != interp.globenv.end() &&
		 jt->second.t == env_info::cvar) {
	ncsts++;
	if (sflag) {
	  sout << sym.s << string(maxsize-sym.s.size(), ' ')
	       << "  cst";
	  if (lflag) sout << "  " << sym.s << " = "
			  << *jt->second.cval << ";";
	  sout << endl;
	} else
	  sout << "const " << sym.s << " = " << *jt->second.cval
	       << ";\n";
      } else {
	if (sym.fix == nullary)
	  sout << "nullary " << sym.s << ";\n";
	else if (sym.prec < 10) {
	  switch (sym.fix) {
	  case infix:
	    sout << "infix"; break;
	  case infixl:
	    sout << "infixl"; break;
	  case infixr:
	    sout << "infixr"; break;
	  case prefix:
	    sout << "prefix"; break;
	  case postfix:
	    sout << "postfix"; break;
	  case nullary:
	    assert(0 && "this can't happen"); break;
	  }
	  sout << " " << (int)sym.prec << " " << sym.s << ";\n";
	}
	if (fflag && xt != interp.externals.end()) {
	  const ExternInfo& info = xt->second;
	  sout << info << ";";
	  if ((!sflag||lflag) && dflag) {
	    if (!sflag) sout << endl;
	    info.f->print(sout);
	  } else
	    sout << endl;
	}
	if (mflag && kt != interp.macenv.end()) {
	  uint32_t argc = kt->second.argc;
	  const rulel& rules = *kt->second.rules;
	  const matcher *m = kt->second.m;
	  if (sflag) {
	    ++nmacs; mrules += rules.size();
	    sout << sym.s << string(maxsize-sym.s.size(), ' ') << "  mac";
	    if (lflag) {
	      sout << "  " << rules << ";";
	      if (aflag && m) sout << endl << *m;
	    } else {
	      sout << " " << argc << " args, " << rules.size() << " rules";
	    }
	    sout << endl;
	  } else {
	    size_t n = 0;
	    for (rulel::const_iterator it = rules.begin();
		 it != rules.end(); ++it) {
	      if (it->temp >= tflag) {
		sout << "def " << *it << ";\n";
		++n;
	      }
	    }
	    if (n > 0) {
	      if (aflag && m) sout << *m << endl;
	      mrules += n;
	      ++nmacs;
	    }
	  }
	}
	if (fflag && jt != interp.globenv.end()) {
	  uint32_t argc = jt->second.argc;
	  const rulel& rules = *jt->second.rules;
	  const matcher *m = jt->second.m;
	  if (sflag) {
	    ++nfuns; nrules += rules.size();
	    sout << sym.s << string(maxsize-sym.s.size(), ' ') << "  fun";
	    if (lflag) {
	      sout << "  " << rules << ";";
	      if (aflag && m) sout << endl << *m;
	      if (dflag && fenv != interp.globalfuns.end() && fenv->second.f)
		fenv->second.print(sout);
	    } else {
	      sout << " " << argc << " args, " << rules.size() << " rules";
	    }
	    sout << endl;
	  } else {
	    size_t n = 0;
	    for (rulel::const_iterator it = rules.begin();
		 it != rules.end(); ++it) {
	      if (it->temp >= tflag) {
		sout << *it << ";\n";
		++n;
	      }
	    }
	    if (n > 0) {
	      if (aflag && m) sout << *m << endl;
	      if (dflag && fenv != interp.globalfuns.end() && fenv->second.f)
		fenv->second.print(sout);
	      nrules += n;
	      ++nfuns;
	    }
	  }
	}
      }
    }
    if (sflag) {
      ostringstream summary;
      if (cflag)
	summary << ncsts << " constants, ";
      if (vflag)
	summary << nvars << " variables, ";
      if (mflag)
	summary << nmacs << " macros (" << mrules << " rules), ";
      if (fflag)
	summary << nfuns << " functions (" << nrules << " rules), ";
      string s = summary.str();
      if (!s.empty())
	sout << s.substr(0, s.size()-2) << endl;
    }
    FILE *fp;
    const char *more = getenv("PURE_MORE");
    // FIXME: We should check that 'more' actually exists here.
    if (more && *more && isatty(fileno(stdin)) && (fp = popen(more, "w"))) {
      fputs(sout.str().c_str(), fp);
      pclose(fp);
    } else
      cout << sout.str();
  }
 out:
  interpreter::g_verbose = s_verbose;
}
^dump.* {
  // dump command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  uint8_t tflag = 1; int pflag = -1;
  bool cflag = false, fflag = false, mflag = false, vflag = false;
  bool gflag = false, Fflag = false;
  string fname = ".pure";
  const char *s = yytext+4;
  if (*s && !isspace(*s)) REJECT;
  yylloc->step();
  argl args(s, "dump");
  list<string>::iterator arg;
  if (!args.ok) goto out2;
  // process option arguments
  for (arg = args.l.begin(); arg != args.l.end(); arg++) {
    if (Fflag) { fname = *arg; Fflag = false; continue; }
    const char *s = arg->c_str();
    if (s[0] != '-' || !s[1] || !strchr("Fcfghmptv", s[1])) break;
    while (*++s) {
      switch (*s) {
      case 'F': Fflag = true; break;
      case 'c': cflag = true; break;
      case 'f': fflag = true; break;
      case 'g': gflag = true; break;
      case 'm': mflag = true; break;
      case 'p':
	if (isdigit(s[1])) {
	  pflag = strtoul(s+1, 0, 10)>0;
	  while (isdigit(s[1])) ++s;
	} else
	  pflag = 1;
	break;
      case 'v': vflag = true; break;
      case 't':
	if (isdigit(s[1])) {
	  tflag = strtoul(s+1, 0, 10);
	  while (isdigit(s[1])) ++s;
	} else
	  tflag = interp.temp;
	break;
      case 'h':
	cout <<
"dump command help: dump [-F filename] [options ...] [symbol ...]\n\
Options may be combined, e.g., dump -fg f* is the same as dump -f -g f*.\n\
-F  Write the dump to the given file (default is .pure).\n\
-c  Dump defined constants.\n\
-f  Dump defined functions.\n\
-g  Indicates that the following symbols are actually shell glob patterns\n\
    and that all matching symbols should be dumped.\n\
-h  Print this list.\n\
-m  Dump defined macros.\n\
-p[flag] Dump only private symbols in the current module if flag is\n\
    nonzero (the default), otherwise dump only public symbols of all\n\
    modules. Dump both private and public symbols if -p is omitted.\n\
-t[level] Dump only symbols and definitions at the given temporary level\n\
    (the current level by default) or above. Level 1 denotes all temporary\n\
    definitions (the default if -t is omitted), level 0 *all* definitions.\n\
-v  Dump defined variables.\n";
	goto out2;
      default:
	cerr << "show: invalid option character '" << *s << "'\n";
	goto out2;
      }
    }
  }
  args.l.erase(args.l.begin(), arg);
  if (!cflag && !fflag && !mflag && !vflag)
    cflag = fflag = mflag = vflag = true;
  {
    list<env_sym> l; set<int32_t> syms;
    for (env::const_iterator it = interp.globenv.begin();
	 it != interp.globenv.end(); ++it) {
      int32_t f = it->first;
      const env_info& e = it->second;
      const symbol& sym = interp.symtab.sym(f);
      if (sym.modno >= 0 && sym.modno != interp.modno ||
	  pflag >= 0 && (pflag > 0) != (sym.modno >= 0) ||
	  !((e.t == env_info::fun)?fflag:
	    (e.t == env_info::cvar)?cflag:
	    (e.t == env_info::fvar)?vflag:0))
	continue;
      bool matches = e.temp >= tflag;
      if (!matches && args.l.empty() &&
	  e.t == env_info::fun && fflag) {
	// dump temporary rules for a non-temporary symbol
	rulel::const_iterator r;
	for (r = e.rules->begin(); r != e.rules->end(); r++)
	  if (r->temp >= tflag) {
	    matches = true;
	    break;
	  }
      }
      if (!matches) continue;
      if (!args.l.empty()) {
	// see whether we actually want the defined symbol to be dumped
	matches = false;
	for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
	  if (gflag ? (!fnmatch(arg->c_str(), sym.s.c_str(), 0)) :
	      (*arg == sym.s)) {
	    matches = true;
	    break;
	  }
	}
      }
      if (!matches) continue;
      syms.insert(f);
      l.push_back(env_sym(sym, it, interp.macenv.find(f),
			  interp.externals.find(f)));
    }
    if (fflag && tflag == 0) {
      // also process the declared externals which don't have any rules yet
      for (extmap::const_iterator it = interp.externals.begin();
	   it != interp.externals.end(); ++it) {
	int32_t f = it->first;
	if (syms.find(f) == syms.end()) {
	  const symbol& sym = interp.symtab.sym(f);
	  if (sym.modno >= 0 && sym.modno != interp.modno ||
	      pflag >= 0 && (pflag > 0) != (sym.modno >= 0))
	    continue;
	  bool matches = true;
	  if (!args.l.empty()) {
	    matches = false;
	    for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
	      if (gflag ? (!fnmatch(arg->c_str(), sym.s.c_str(), 0)) :
		  (*arg == sym.s)) {
		matches = true;
		break;
	      }
	    }
	  }
	  if (!matches) continue;
	  l.push_back(env_sym(sym, interp.globenv.end(),
			      interp.macenv.find(f), it));
	}
      }
    }
    if (mflag) {
      // also dump any symbols defined as macros, unless they've already been
      // considered
      for (env::const_iterator it = interp.macenv.begin();
	   it != interp.macenv.end(); ++it) {
	int32_t f = it->first;
	if (syms.find(f) == syms.end()) {
	  const env_info& e = it->second;
	  const symbol& sym = interp.symtab.sym(f);
	  if (sym.modno >= 0 && sym.modno != interp.modno ||
	      pflag >= 0 && (pflag > 0) != (sym.modno >= 0))
	    continue;
	  bool matches = e.temp >= tflag;
	  if (!matches && args.l.empty()) {
	    // also dump temporary rules for a non-temporary symbol
	    rulel::const_iterator r;
	    for (r = e.rules->begin(); r != e.rules->end(); r++)
	      if (r->temp >= tflag) {
		matches = true;
		break;
	      }
	  }
	  if (!matches) continue;
	  if (!args.l.empty()) {
	    // see whether we actually want the defined symbol to be dumped
	    matches = false;
	    for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
	      if (gflag ? (!fnmatch(arg->c_str(), sym.s.c_str(), 0)) :
		  (*arg == sym.s)) {
		matches = true;
		break;
	      }
	    }
	  }
	  if (!matches) continue;
	  syms.insert(f);
	  l.push_back(env_sym(sym, interp.globenv.end(), it,
			      interp.externals.end()));
	}
      }
    }
    l.sort(env_compare);
    if (l.empty()) {
      unlink(fname.c_str());
      goto out2;
    }
    ofstream fout;
    fout.open(fname.c_str());
    time_t t; time(&t);
    fout << "// dump written " << ctime(&t);
    for (list<env_sym>::const_iterator it = l.begin();
	 it != l.end(); ++it) {
      const symbol& sym = *it->sym;
      int32_t ftag = sym.f;
      map<int32_t,Env>::iterator fenv = interp.globalfuns.find(ftag);
      const env::const_iterator jt = it->it, kt = it->jt;
      const extmap::const_iterator xt = it->xt;
      if (jt == interp.globenv.end() && kt == interp.macenv.end()) {
	assert(xt != interp.externals.end());
	const ExternInfo& info = xt->second;
	fout << info << ";\n";
      } else if (jt != interp.globenv.end() &&
		 jt->second.t == env_info::fvar) {
	fout << "let " << sym.s << " = " << *(pure_expr**)jt->second.val
	     << ";\n";
      } else if (jt != interp.globenv.end() &&
		 jt->second.t == env_info::cvar) {
	fout << "const " << sym.s << " = " << *jt->second.cval
	     << ";\n";
      } else {
	if (sym.fix == nullary)
	  fout << "nullary " << sym.s << ";\n";
	else if (sym.prec < 10) {
	  switch (sym.fix) {
	  case infix:
	    fout << "infix"; break;
	  case infixl:
	    fout << "infixl"; break;
	  case infixr:
	    fout << "infixr"; break;
	  case prefix:
	    fout << "prefix"; break;
	  case postfix:
	    fout << "postfix"; break;
	  case nullary:
	    assert(0 && "this can't happen"); break;
	  }
	  fout << " " << (int)sym.prec << " " << sym.s << ";\n";
	}
	if (fflag && xt != interp.externals.end()) {
	  const ExternInfo& info = xt->second;
	  fout << info << ";\n";
	}
	if (mflag && kt != interp.macenv.end()) {
	  const rulel& rules = *kt->second.rules;
	  for (rulel::const_iterator it = rules.begin();
	       it != rules.end(); ++it) {
	    if (it->temp >= tflag) {
	      fout << "def " << *it << ";\n";
	    }
	  }
	}
	if (fflag && jt != interp.globenv.end()) {
	  const rulel& rules = *jt->second.rules;
	  for (rulel::const_iterator it = rules.begin();
	       it != rules.end(); ++it) {
	    if (it->temp >= tflag) {
	      fout << *it << ";\n";
	    }
	  }
	}
      }
    }
  }
 out2:
  ;
}
^save.* {
  // save command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext+4;
  if (*s && !isspace(*s)) REJECT;
  yylloc->step();
  argl args(s, "save");
  if (!args.ok)
    ;
  else if (args.c > 0)
    cerr << "save: extra parameter\n";
  else if (interp.temp == 0xff)
    cerr << "save: already at maximum level\n";
  else {
    cout << "save: now at temporary definitions level #"
	 << (unsigned)++interp.temp << endl;
    if (interp.override) cout << "save: override mode is on\n";
  }
}
^clear.* {
  // clear command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext+5;
  if (*s && !isspace(*s)) REJECT;
  yylloc->step();
  argl args(s, "clear");
  if (!args.ok)
    ;
  else if (args.c > 0) {
    list<string>::iterator s;
    for (s = args.l.begin(); s != args.l.end(); s++) {
      const symbol *sym = interp.symtab.lookup(*s, interp.modno);
      if (sym && sym->f > 0)
	interp.clear(sym->f);
      else
	cerr << "clear: symbol '" << *s << "' not defined\n";
    }
  } else {
    uint8_t old = interp.temp;
    char ans;
    cout << "This will clear all temporary definitions at level #"
	 << (unsigned)interp.temp << ". Continue (y/n)? ";
    cin >> noskipws >> ans;
    if (cin.good() && ans == 'y') interp.clear();
    while (cin.good() && ans != '\n') cin >> noskipws >> ans;
    cin.clear();
    if (old > 1)
      cout << "clear: now at temporary definitions level #"
	   << (unsigned)interp.temp << endl;
    if (interp.override) cout << "clear: override mode is on\n";
  }
}
^run.* {
  // run command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext+3;
  if (*s && !isspace(*s)) REJECT;
  yylloc->step();
  argl args(s, "run");
  if (!args.ok)
    ;
  else if (args.c == 0 || args.c == 1 && args.l.begin()->empty())
    cerr << "run: no script name specified\n";
  else if (args.c > 1)
    cerr << "run: extra parameter\n";
  else
    interp.run(*args.l.begin(), false, true);
}
^override.* {
  // override command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext+8;
  if (*s && !isspace(*s)) REJECT;
  yylloc->step();
  argl args(s, "override");
  if (!args.ok)
    ;
  else if (args.c > 0)
    cerr << "override: extra parameter\n";
  else
    interp.override = true;
}
^underride.* {
  // underride command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext+9;
  if (*s && !isspace(*s)) REJECT;
  yylloc->step();
  argl args(s, "underride");
  if (!args.ok)
    ;
  else if (args.c > 0)
    cerr << "underride: extra parameter\n";
  else
    interp.override = false;
}
^stats.* {
  // stats command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext+5;
  if (*s && !isspace(*s)) REJECT;
  yylloc->step();
  argl args(s, "stats");
  if (!args.ok)
    ;
  else if (args.c == 0)
    interp.stats = true;
  else if (args.c == 1)
    if (args.l.front() == "on")
      interp.stats = true;
    else if (args.l.front() == "off")
      interp.stats = false;
    else
      cerr << "stats: invalid parameter '" << args.l.front()
	   << "' (must be 'on' or 'off')\n";
  else
    cerr << "stats: extra parameter\n";
}
^quit.* {
  // quit command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext+4;
  if (*s && !isspace(*s)) REJECT;
  yylloc->step();
  argl args(s, "quit");
  if (!args.ok)
    ;
  else if (args.c > 0)
    cerr << "quit: extra parameter\n";
  else
    exit(0);
}
^completion_matches.* {
  // completion_matches command is only permitted in interactive mode
  if (!interp.interactive) REJECT;
  const char *s = yytext+18;
  if (*s && !isspace(*s)) REJECT;
  while (isspace(*s)) ++s;
  yylloc->step();
  list_completions(s);
}

{int}      {
  mpz_t *z = (mpz_t*)malloc(sizeof(mpz_t));
  mpz_init(*z);
  mpz_set_str(*z, yytext, 0);
  if (mpz_fits_sint_p(*z)) {
    int n = mpz_get_si(*z);
    mpz_clear(*z); free(z);
    yylval->ival = n;
    return token::INT;
  } else {
    yylval->zval = z;
    return token::CBIGINT;
  }
}
{int}L     {
  mpz_t *z = (mpz_t*)malloc(sizeof(mpz_t));
  mpz_init(*z);
  yytext[yyleng-1] = 0;
  mpz_set_str(*z, yytext, 0);
  yytext[yyleng-1] = 'L';
  yylval->zval = z;
  return token::BIGINT;
}
{float}    yylval->dval = my_strtod(yytext, NULL); return(token::DBL);
\"{str}\"   {
  char *msg;
  yytext[yyleng-1] = 0;
  yylval->csval = parsestr(yytext+1, msg);
  yytext[yyleng-1] = '"';
  if (msg) interp.error(*yylloc, msg);
  return token::STR;
}
\"{str}      {
  char *msg;
  interp.error(*yylloc, "unterminated string constant");
  yylval->csval = parsestr(yytext+1, msg);
  return token::STR;
}
{inttag}/[^a-zA-Z_0-9]   yylval->ival = EXPR::INT; return token::TAG;
{binttag}/[^a-zA-Z_0-9]  yylval->ival = EXPR::BIGINT; return token::TAG;
{dbltag}/[^a-zA-Z_0-9]   yylval->ival = EXPR::DBL; return token::TAG;
{strtag}/[^a-zA-Z_0-9]   yylval->ival = EXPR::STR; return token::TAG;
{ptrtag}/[^a-zA-Z_0-9]   yylval->ival = EXPR::PTR; return token::TAG;
{mattag}/[^a-zA-Z_0-9]   yylval->ival = EXPR::MATRIX; return token::TAG;
extern     BEGIN(xdecl); return token::EXTERN;
infix      yylval->fix = infix; return token::FIX;
infixl     yylval->fix = infixl; return token::FIX;
infixr     yylval->fix = infixr; return token::FIX;
prefix     yylval->fix = prefix; return token::FIX;
postfix    yylval->fix = postfix; return token::FIX;
nullary    return token::NULLARY;
private    return token::PRIVATE;
const      return token::CONST;
def        return token::DEF;
let        return token::LET;
case	   return token::CASE;
of	   return token::OF;
end	   return token::END;
if	   return token::IF;
then	   return token::THEN;
else	   return token::ELSE;
otherwise  return token::OTHERWISE;
when	   return token::WHEN;
with	   return token::WITH;
using      BEGIN(xusing); return token::USING;
{id}       {
  check(*yylloc, yytext);
  if (interp.declare_op) {
    yylval->sval = new string(yytext);
    return token::ID;
  }
  symbol* sym = interp.symtab.lookup(yytext, interp.modno);
  if (sym && sym->prec >= 0 && sym->prec < 10) {
    yylval->xval = new expr(sym->x);
    return optoken[sym->prec][sym->fix];
  } else {
    yylval->sval = new string(yytext);
    return token::ID;
  }
}
[@=|;()\[\]{}\\] return yy::parser::token_type(yytext[0]);
"->"       return token::MAPSTO;
"#<"{id}(" "{int})?">" return token::BADTOK;
([[:punct:]]|[\200-\377])+  {
  if (yytext[0] == '/' && yytext[1] == '*') REJECT; // comment starter
  while (yyleng > 1 && yytext[yyleng-1] == ';') yyless(yyleng-1);
  if (interp.declare_op) {
    yylval->sval = new string(yytext);
    return token::ID;
  }
  symbol* sym = interp.symtab.lookup(yytext, interp.modno);
  while (!sym && yyleng > 1) {
    if (yyleng == 2 && yytext[0] == '-' && yytext[1] == '>')
      return token::MAPSTO;
    yyless(yyleng-1);
    sym = interp.symtab.lookup(yytext, interp.modno);
  }
  if (sym) {
    if (sym->prec < 10) {
      yylval->xval = new expr(sym->x);
      return optoken[sym->prec][sym->fix];
    } else {
      yylval->sval = new string(yytext);
      return token::ID;
    }
  } else
    REJECT;
}
.          {
  string msg = "invalid character '"+string(yytext)+"'";
  interp.error(*yylloc, msg);
  return token::ERRTOK;
}

%%

bool
interpreter::lex_begin(const string& fname)
{
  yy_flex_debug = (verbose&verbosity::lexer) != 0 && !source_s;
  FILE *fp;
  if (source_s)
    fp = 0;
  else if (source.empty())
    fp = stdin;
  else if (!(fp = fopen(fname.c_str(), "r")))
    //error("cannot open '" + source + "'");
    perror(source.c_str());
  if (source_s || fp) {
    yyin = fp;
    yypush_buffer_state(yy_create_buffer(yyin, YY_BUF_SIZE));
    BEGIN(INITIAL);
    return true;
  } else
    return false;
}

void
interpreter::lex_end()
{
  if (!source_s && !source.empty() && yyin)
    fclose(yyin);
  yypop_buffer_state();
}

static char *my_buf = NULL, *my_bufptr = NULL;
static int len = 0;

bool using_readline = false;

void my_readline(const char *prompt, char *buf, int &result, int max_size)
{
  if (!my_buf) {
    if (using_readline) {
      // read a new line using readline()
      my_bufptr = my_buf = readline(prompt);
      if (!my_buf) {
	// EOF, bail out
	result = 0;
	return;
      }
      add_history(my_buf);
    } else {
      // read a new line from stdin
      char s[10000];
      fputs(prompt, stdout); fflush(stdout);
      if (!fgets(s, 10000, stdin)) {
	// EOF, bail out
	result = 0;
	return;
      }
      // get rid of the trailing newline
      size_t l = strlen(s);
      if (l>0 && s[l-1] == '\n')
	s[l-1] = 0;
      my_bufptr = my_buf = strdup(s);
      if (!my_buf) {
	// memory allocation error, bail out
	result = 0;
	return;
      }
    }
    len = strlen(my_buf);
  }
  // how many chars we got
  int l = len-(my_bufptr-my_buf);
  // how many chars to copy (+1 for the trailing newline)
  int k = l+1;
  if (k > max_size) k = max_size;
  // copy chars to the buffer
  strncpy(buf, my_bufptr, k);
  if (k > l) {
    // finish off with trailing newline, get rid of the buffer
    buf[l] = '\n';
    free(my_buf); my_buf = my_bufptr = NULL; len = 0;
  }
  result = k;
}

void Env::print(ostream& os) const
{
  if (!f) return; // not used, probably a shadowed rule
  if (h && h != f) h->print(os);
  f->print(os);
  set<Env*> e;
  for (size_t i = 0, n = fmap.m.size(); i < n; i++) {
    for (EnvMap::const_iterator it = fmap.m[i]->begin(),
	   end = fmap.m[i]->end(); it != end; it++)
      if (e.find(it->second) == e.end()) {
	it->second->print(os);
	e.insert(it->second);
      }
  }
}
