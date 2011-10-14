%{                                            /* -*- C++ -*- */

/* Copyright (c) 2008-2011 by Albert Graef <Dr.Graef@t-online.de>.

   This file is part of the Pure runtime.

   The Pure runtime is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at your
   option) any later version.

   Pure is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
   more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. */

#include <cstdlib>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <fnmatch.h>
#include <time.h>
#include <string>
#include <sstream>
#include <fstream>
#include "interpreter.hh"
#include "lexerdefs.hh"
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

static bool checkcmd(interpreter &interp, const char *s);
static bool checkusercmd(interpreter &interp, const char *s);
static void docmd(interpreter &interp, yy::parser::location_type* yylloc, const char *cmd, const char *cmdline, bool esc);
static string pstring(const char *s);
static string format_namespace(const string& name);
static bool find_namespace(interpreter& interp, const string& name);
static int32_t checktag(const char *s);
static bool checkint(const char *s, string& msg);
static string xsym(const string& ns, const string& s);

/* Uncomment this to enable checking for interactive command names. This is
   rather annoying and hence disabled by default. */
//#define CHECK_NAMES 1

#if CHECK_NAMES
static void check(const yy::location& l, const char* s, bool decl);
#else
#define check(l, s, decl) 
#endif

/* Hook for interactive command input. */
char *(*command_input)(const char *prompt);
void (*exit_handler)();
static void lex_input(const char *prompt, char *buf,
		      size_t &result, size_t max_size);

#define YY_INPUT(buf,result,max_size)					\
  if (interpreter::g_interp->source_s) {				\
    size_t l = strlen(interpreter::g_interp->source_s);			\
    if (l > (size_t)max_size) l = (size_t)max_size;			\
    memcpy(buf, interpreter::g_interp->source_s, l);			\
    interpreter::g_interp->source_s += result = l;			\
  } else if ( interpreter::g_interactive &&				\
	      interpreter::g_interp->ttymode ) {			\
    size_t l;								\
    lex_input(interpreter::g_interp->ps.c_str(),			\
	      buf, l, (size_t)max_size);				\
    result = l;								\
 } else {								\
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

static yy::parser::token_type optoken[5] =
  {token::NA, token::LT, token::RT, token::PR, token::PO};

#define optok(_f, _fix) ((_fix<=infixr && _f==interp.symtab.minus_sym().f)?token::PR:optoken[_fix])
%}

%option noyywrap debug

 /* Special extended Unicode symbols. Contributed by John Cowan. */

punct  (\xC2[\xA1-\xBF]|\xC3[\xD7\xF7]|\xE2[\x84-\xAF][\x80-\xBF]|\xE2\x83[\x90-\xBF])

 /* We take any other valid utf-8, excluding the above, plus the ASCII
    alphabetic symbols and the underscore, as a "letter":
      [a-zA-Z_]                                 # ASCII
    | [\xC4-\xDF][\x80-\xBF]                    # non-overlong 2-byte
    | \xC2[^\x01-\x7F\xA1-\xFF]
    | \xC3[^\x01-\x7F\xD7\xF7\xC0-\xFF]
    | \xE0[\xA0-\xBF][\x80-\xBF]                # excluding overlongs
    | [\xE1\xE3-\xEC\xEE\xEF][\x80-\xBF]{2}     # straight 3-byte
    | \xE2[^\x01-\x7F\x83-\xAF\xC0-\xFF][\x80-\xBF]
    | \xE2\x83[\x80-\x8F]
    | \xED[\x80-\x9F][\x80-\xBF]                # excluding surrogates
    | \xF0[\x90-\xBF][\x80-\xBF]{2}             # planes 1-3
    | [\xF1-\xF3][\x80-\xBF]{3}                 # planes 4-15
    | \xF4[\x80-\x8F][\x80-\xBF]{2}             # plane 16             */

letter ([a-zA-Z_]|[\xC4-\xDF][\x80-\xBF]|\xC2[^\x01-\x7F\xA1-\xFF]|\xC3[^\x01-\x7F\xD7\xF7\xC0-\xFF]|\xE0[\xA0-\xBF][\x80-\xBF]|[\xE1\xE3-\xEC\xEE\xEF][\x80-\xBF]{2}|\xE2[^\x01-\x7F\x83-\xAF\xC0-\xFF][\x80-\xBF]|\xE2\x83[\x80-\x8F]|\xED[\x80-\x9F][\x80-\xBF]|\xF0[\x90-\xBF][\x80-\xBF]{2}|[\xF1-\xF3][\x80-\xBF]{3}|\xF4[\x80-\x8F][\x80-\xBF]{2})

id     ({letter}({letter}|[0-9])*)
qual   ({id}?::({id}::)*)
int    [0-9][0-9A-Za-z]*
exp    ([Ee][+-]?{int})
float  {int}{exp}|{int}\.{exp}|({int})?\.{int}{exp}?
str    ([^\"\\\n]|\\(.|\n))*
cmd    (!|"^"?{id})
blank  [ \t\f\v\r]

%x escape comment xcode xcode_comment xdecl xdecl_comment xusing xusing_comment xsyms xsyms_comment xtag rescan xsyms_rescan

%{
# define YY_USER_ACTION  yylloc->columns(yyleng);
%}

%%

%{
  yylloc->step();
%}

{blank}+   yylloc->step();
[\n]+      yylloc->lines(yyleng); yylloc->step();

^"#!"[ \t]*"--eager"[ \t]+[^ \t\n]+([ \t]+"//".*)? {
  /* --eager pragma. */
  char *s = strchr(yytext, '-')+strlen("--eager"), *t = s;
  while (isspace(*t)) t++;
  s = t;
  while (*t && !isspace(*t)) t++;
  string sym = string(s, t-s);
  int32_t f = pure_sym(sym.c_str());
  if (f > 0)
    interp.eager.insert(f);
  else
    interp.warning(*yylloc, "warning: bad symbol '"+sym+
		   "' in --eager pragma");
}
^"#!"[ \t]*"--required"[ \t]+[^ \t\n]+([ \t]+"//".*)? {
  /* --required pragma. */
  char *s = strchr(yytext, '-')+strlen("--required"), *t = s;
  while (isspace(*t)) t++;
  s = t;
  while (*t && !isspace(*t)) t++;
  string sym = string(s, t-s);
  int32_t f = pure_sym(sym.c_str());
  if (f > 0)
    interp.required.push_back(f);
  else
    interp.warning(*yylloc, "warning: bad symbol '"+sym+
		   "' in --required pragma");
}
^"#!"[ \t]*"--defined"[ \t]+[^ \t\n]+([ \t]+"//".*)? {
  /* --defined pragma. */
  char *s = strchr(yytext, '-')+strlen("--defined"), *t = s;
  while (isspace(*t)) t++;
  s = t;
  while (*t && !isspace(*t)) t++;
  string sym = string(s, t-s);
  int32_t f = pure_sym(sym.c_str());
  env::const_iterator it = interp.globenv.find(f);
  if (f > 0 && (it == interp.globenv.end() || it->second.t == env_info::fun)) {
    if (interp.defined.find(f) == interp.defined.end()) {
      interp.defined.insert(f);
      if (it != interp.globenv.end())
	interp.mark_dirty(f);
      else
	// this forces the cbox to be updated in case of an external
	interp.clearsym(f);
    }
  } else
    interp.warning(*yylloc, "warning: bad symbol '"+sym+
		   "' in --defined pragma");
}
^"#!"[ \t]*"--nodefined"[ \t]+[^ \t\n]+([ \t]+"//".*)? {
  /* --nodefined pragma. */
  char *s = strchr(yytext, '-')+strlen("--nodefined"), *t = s;
  while (isspace(*t)) t++;
  s = t;
  while (*t && !isspace(*t)) t++;
  string sym = string(s, t-s);
  int32_t f = pure_sym(sym.c_str());
  env::const_iterator it = interp.globenv.find(f);
  if (f > 0 && (it == interp.globenv.end() || it->second.t == env_info::fun)) {
    if (interp.defined.find(f) != interp.defined.end()) {
      interp.defined.erase(f);
      if (it != interp.globenv.end())
	interp.mark_dirty(f);
      else
	// this forces the cbox to be updated in case of an external
	interp.clearsym(f);
    }
  } else
    interp.warning(*yylloc, "warning: bad symbol '"+sym+
		   "' in --nodefined pragma");
}
^"#!"[ \t]*"--"[A-Za-z0-9-]+[ \t]*("//".*)? {
  /* Other pragmas (code generation). */
  char *s = strchr(yytext, '-')+2, *t = s;
  while (isalnum(*t) || *t == '-') t++;
  string opt0 = string(s, t-s);
  bool flag = opt0.substr(0,2) != "no";
  string opt = flag?opt0:opt0.substr(2);
  if (opt == "checks") {
    if (interp.checks != flag) {
      interp.compile(); interp.checks = flag;
    }
  } else if (opt == "tc") {
    if (interp.use_fastcc != flag) {
      interp.compile(); interp.use_fastcc = flag;
    }
  } else if (opt == "const") {
    interp.consts = flag;
  } else if (opt == "fold") {
    interp.folding = flag;
  } else if (opt == "warn") {
    interp.compat = flag;
  } else if (opt == "bigint") {
    interp.bigints = flag;
  } else if (opt == "warn2") {
    interp.compat2 = flag;
  } else {
    interp.warning(*yylloc, "warning: unrecognized pragma '--"+opt0+"'");
  }
  yylloc->step();
}
^"#!"[ \t]*"--".* {
  char *s = strchr(yytext, '-');
  interp.warning(*yylloc, "warning: unrecognized pragma '"+string(s)+"'");
  yylloc->step();
}

^"#!".*    |
"//".*     yylloc->step();

"/*"       { parse_comment: BEGIN(comment); }

<escape>""            { BEGIN(INITIAL); return token::ESCAPE; }

<comment>[^*\n]*        yylloc->step();
<comment>"*"+[^*/\n]*   yylloc->step();
<comment>[\n]+          yylloc->lines(yyleng); yylloc->step();
<comment>"*"+"/"        yylloc->step(); BEGIN(INITIAL);
<comment><<EOF>>	interp.error(*yylloc, "open comment at end of file"); BEGIN(INITIAL);

"%<"       { interp.begin_code(); BEGIN(xcode); }

<xcode>"//".*	interp.add_code(yytext);
<xcode>"/*"	interp.add_code(yytext); BEGIN(xcode_comment);
<xcode>\"{str}\"	{
  interp.add_code(yytext);
  int count = 0;
  for (int i = 1; i < (int)yyleng-1; i++)
    if (yytext[i] == '\n') count++;
  yylloc->lines(count);
}
<xcode>[\n]+	interp.add_code(yytext); yylloc->lines(yyleng);
<xcode>.	interp.add_code(yytext);
<xcode>"%>"	interp.end_code(); BEGIN(INITIAL); return token::CODE;
<xcode><<EOF>>	interp.error(*yylloc, "open code section at end of file"); interp.end_code(); BEGIN(INITIAL); return token::CODE;

<xcode_comment>[^*\n]*		interp.add_code(yytext); yylloc->step();
<xcode_comment>"*"+[^*/\n]*	interp.add_code(yytext); yylloc->step();
<xcode_comment>[\n]+		interp.add_code(yytext); yylloc->lines(yyleng); yylloc->step();
<xcode_comment>"*"+"/"		interp.add_code(yytext); yylloc->step(); BEGIN(xcode);
<xcode_comment><<EOF>>		interp.error(*yylloc, "open comment at end of file"); BEGIN(xcode);

<xdecl>extern     return token::EXTERN;
<xdecl>infix      yylval->fix = infix; return token::FIX;
<xdecl>infixl     yylval->fix = infixl; return token::FIX;
<xdecl>infixr     yylval->fix = infixr; return token::FIX;
<xdecl>prefix     yylval->fix = prefix; return token::FIX;
<xdecl>postfix    yylval->fix = postfix; return token::FIX;
<xdecl>outfix     return token::OUTFIX;
<xdecl>nonfix     return token::NONFIX;
<xdecl>private    return token::PRIVATE;
<xdecl>public     return token::PUBLIC;
<xdecl>const      return token::CONST;
<xdecl>def        return token::DEF;
<xdecl>let        return token::LET;
<xdecl>type       return token::TYPE;
<xdecl>case	  return token::CASE;
<xdecl>of	  return token::OF;
<xdecl>end	  return token::END;
<xdecl>if	  return token::IF;
<xdecl>then	  return token::THEN;
<xdecl>else	  return token::ELSE;
<xdecl>otherwise  return token::OTHERWISE;
<xdecl>when	  return token::WHEN;
<xdecl>with	  return token::WITH;
<xdecl>using      return token::USING;
<xdecl>namespace  return token::NAMESPACE;

<xdecl>^{cmd} {
  if ((interp.interactive || interp.output) && checkcmd(interp, yytext))
    goto parse_cmd;
  else if (yytext[0] == '!' || yytext[0] == '^')
    goto xdecl_bad_char;
  else
    goto xdecl_parse_id;
}

<xdecl>{id}	  { xdecl_parse_id: check(*yylloc, yytext, true); yylval->sval = new string(yytext); return token::ID; }
<xdecl>[()*,=]	  return yy::parser::token_type(yytext[0]);
<xdecl>"..."	  return token::ELLIPSIS;
<xdecl>"//".*	  yylloc->step();
<xdecl>"/*"	  BEGIN(xdecl_comment);
<xdecl>;	  BEGIN(INITIAL); return yy::parser::token_type(yytext[0]);
<xdecl>{blank}+	  yylloc->step();
<xdecl>[\n]+	  yylloc->lines(yyleng); yylloc->step();
<xdecl>(.|{punct}|{letter})	{
 xdecl_bad_char:
  string msg = "invalid character '"+string(yytext)+"'";
  interp.error(*yylloc, msg);
}
     
<xdecl_comment>[^*\n]*        yylloc->step();
<xdecl_comment>"*"+[^*/\n]*   yylloc->step();
<xdecl_comment>[\n]+          yylloc->lines(yyleng); yylloc->step();
<xdecl_comment>"*"+"/"        yylloc->step(); BEGIN(xdecl);
<xdecl_comment><<EOF>>        interp.error(*yylloc, "open comment at end of file"); BEGIN(xdecl);

<xusing>extern     BEGIN(INITIAL); return token::EXTERN;
<xusing>infix      BEGIN(INITIAL); yylval->fix = infix; return token::FIX;
<xusing>infixl     BEGIN(INITIAL); yylval->fix = infixl; return token::FIX;
<xusing>infixr     BEGIN(INITIAL); yylval->fix = infixr; return token::FIX;
<xusing>prefix     BEGIN(INITIAL); yylval->fix = prefix; return token::FIX;
<xusing>postfix    BEGIN(INITIAL); yylval->fix = postfix; return token::FIX;
<xusing>outfix     BEGIN(INITIAL); return token::OUTFIX;
<xusing>nonfix     BEGIN(INITIAL); return token::NONFIX;
<xusing>const      BEGIN(INITIAL); return token::CONST;
<xusing>def        BEGIN(INITIAL); return token::DEF;
<xusing>let        BEGIN(INITIAL); return token::LET;
<xusing>type       BEGIN(INITIAL); return token::TYPE;
<xusing>case	   BEGIN(INITIAL); return token::CASE;
<xusing>of	   BEGIN(INITIAL); return token::OF;
<xusing>end	   BEGIN(INITIAL); return token::END;
<xusing>if	   BEGIN(INITIAL); return token::IF;
<xusing>then	   BEGIN(INITIAL); return token::THEN;
<xusing>else	   BEGIN(INITIAL); return token::ELSE;
<xusing>otherwise  BEGIN(INITIAL); return token::OTHERWISE;
<xusing>when	   BEGIN(INITIAL); return token::WHEN;
<xusing>with	   BEGIN(INITIAL); return token::WITH;
<xusing>using      return token::USING;
<xusing>namespace  return token::NAMESPACE;
<xusing>private    return token::PRIVATE;
<xusing>public     return token::PUBLIC;

<xusing>^{cmd} {
  if ((interp.interactive || interp.output) && checkcmd(interp, yytext))
    goto parse_cmd;
  else if (yytext[0] == '!' || yytext[0] == '^')
    goto xusing_bad_char;
  else
    goto xusing_parse_id;
}

<xusing>{qual}?{id}  { xusing_parse_id: yylval->sval = new string(yytext); return token::ID; }
<xusing>"("        BEGIN(xsyms); return yy::parser::token_type(yytext[0]);
<xusing>,	   return yy::parser::token_type(yytext[0]);
<xusing>"//".*	   yylloc->step();
<xusing>"/*"	   BEGIN(xusing_comment);
<xusing>;	   BEGIN(INITIAL); return yy::parser::token_type(yytext[0]);
<xusing>{blank}+   yylloc->step();
<xusing>[\n]+	   yylloc->lines(yyleng); yylloc->step();
<xusing>\"{str}\"  {
  char *msg;
  yytext[yyleng-1] = 0;
  yylval->csval = parsestr(yytext+1, msg);
  yytext[yyleng-1] = '"';
  if (msg) interp.error(*yylloc, msg);
  return token::STR;
}
<xusing>\"{str}    {
  char *msg;
  interp.error(*yylloc, "unterminated string constant");
  yylval->csval = parsestr(yytext+1, msg);
  return token::STR;
}
<xusing>(.|{punct}|{letter})	{
 xusing_bad_char:
  string msg = "invalid character '"+string(yytext)+"'";
  interp.error(*yylloc, msg);
}

<xsyms>^{cmd} {
  if ((interp.interactive || interp.output) && checkcmd(interp, yytext))
    goto parse_cmd;
  else if (yytext[0] == '!' || yytext[0] == '^')
    goto xsyms_parse_op;
  else
    goto xsyms_parse_id;
}

<xsyms>"//".*	   yylloc->step();
<xsyms>"/*"	   { xsyms_parse_comment: BEGIN(xsyms_comment); }
<xsyms,xsyms_rescan>")"	   BEGIN(xusing); return yy::parser::token_type(yytext[0]);
<xsyms>{blank}+    yylloc->step();
<xsyms>[\n]+	   yylloc->lines(yyleng); yylloc->step();
<xsyms>{id}        {
 xsyms_parse_id:
  symbol* sym = interp.symtab.lookup(xsym(interp.xsym_prefix, yytext));
  bool res = interp.symtab.count > 0;
  if (sym) {
    yylval->ival = sym->f;
    return token::XID;
  }
  string msg = res?("symbol '"+string(yytext)+"' is private here"):
    ("undeclared symbol '")+string(yytext)+"'";
  interp.error(*yylloc, msg);
}
<xsyms>([[:punct:]]|{punct})+  {
 xsyms_parse_op:
  if (yytext[0] == '/' && yytext[1] == '*') {
    yyless(2);
    goto xsyms_parse_comment; // comment starter
  }
  while (yyleng > 1 && yytext[yyleng-1] == ';') yyless(yyleng-1);
  symbol* sym = interp.symtab.lookup(xsym(interp.xsym_prefix, yytext));
  bool res = interp.symtab.count > 0;
  while (!sym && !res && yyleng > 1) {
    if (yyleng == 2 && yytext[0] == '-' && yytext[1] == '>')
      return token::MAPSTO;
    yyless(yyleng-1);
    sym = interp.symtab.lookup(xsym(interp.xsym_prefix, yytext));
    res = interp.symtab.count > 0;
  }
  if (sym) {
    yylval->ival = sym->f;
    return token::XID;
  } else if (res) {
    string msg = "symbol '"+string(yytext)+"' is private here";
    interp.error(*yylloc, msg);
  } else {
    assert(yyleng == 1);
    /* If we come here, we failed to recognize the input as a special symbol
       and have to rescan everything in a special mode which excludes this
       rule. This hack is necessary in order to avoid the use of REJECT. */
    yyless(0);
    BEGIN(xsyms_rescan);
  }
}
<xsyms_rescan>(.|{punct}|{letter}) {
  string msg = "invalid character '"+pstring(yytext)+"'";
  interp.error(*yylloc, msg);
  BEGIN(xsyms);
}

<xusing_comment>[^*\n]*        yylloc->step();
<xusing_comment>"*"+[^*/\n]*   yylloc->step();
<xusing_comment>[\n]+          yylloc->lines(yyleng); yylloc->step();
<xusing_comment>"*"+"/"        yylloc->step(); BEGIN(xusing);
<xusing_comment><<EOF>>        interp.error(*yylloc, "open comment at end of file"); BEGIN(xusing);

<xsyms_comment>[^*\n]*        yylloc->step();
<xsyms_comment>"*"+[^*/\n]*   yylloc->step();
<xsyms_comment>[\n]+          yylloc->lines(yyleng); yylloc->step();
<xsyms_comment>"*"+"/"        yylloc->step(); BEGIN(xsyms);
<xsyms_comment><<EOF>>        interp.error(*yylloc, "open comment at end of file"); BEGIN(xsyms);

[0-9]+{exp} goto float_const; // this case must be treated separately
{int}L     {
  string msg;
  if (checkint(yytext, msg)) {
    mpz_t *z = (mpz_t*)malloc(sizeof(mpz_t));
    mpz_init(*z);
    yytext[yyleng-1] = 0;
    mpz_set_str(*z, yytext, 0);
    yytext[yyleng-1] = 'L';
    yylval->zval = z;
  } else {
    mpz_t *z = (mpz_t*)malloc(sizeof(mpz_t));
    mpz_init_set_si(*z, 0);
    yylval->zval = z;
    interp.error(*yylloc, msg);
  }
  return token::BIGINT;
}
{int}      {
  string msg;
  if (checkint(yytext, msg)) {
    mpz_t *z = (mpz_t*)malloc(sizeof(mpz_t));
    mpz_init(*z);
    mpz_set_str(*z, yytext, 0);
    if (!interp.bigints && mpz_fits_sint_p(*z)) {
      int n = mpz_get_si(*z);
      mpz_clear(*z); free(z);
      yylval->ival = n;
      return token::INT;
    } else {
      yylval->zval = z;
      return interp.bigints?token::BIGINT:token::CBIGINT;
    }
  } else {
    yylval->ival = 0;
    interp.error(*yylloc, msg);
    return token::INT;
  }
}
{float}    {
 float_const:
  char *p = NULL;
  yylval->dval = my_strtod(yytext, &p);
  if (p && *p) {
    string msg = "invalid digit '"+string(1, *p)+"' in floating point constant";
    interp.error(*yylloc, msg);
  }
  return token::DBL;
}
<rescan>\"{str}\" |
\"{str}\"   {
  char *msg;
  yytext[yyleng-1] = 0;
  int count = 0;
  for (int i = 1; i < (int)yyleng-1; i++)
    if (yytext[i] == '\n') count++;
  yylloc->lines(count);
  yylval->csval = parsestr(yytext+1, msg);
  yytext[yyleng-1] = '"';
  if (msg) interp.error(*yylloc, msg);
  BEGIN(INITIAL);
  return token::STR;
}
<rescan>\"{str} |
\"{str}      {
  char *msg;
  int count = 0;
  for (int i = 1; i < (int)yyleng; i++)
    if (yytext[i] == '\n') count++;
  yylloc->lines(count);
  interp.error(*yylloc, "unterminated string constant");
  yylval->csval = parsestr(yytext+1, msg);
  BEGIN(INITIAL);
  return token::STR;
}
extern     BEGIN(xdecl); return token::EXTERN;
infix      yylval->fix = infix; interp.declare_op = false; return token::FIX;
infixl     yylval->fix = infixl; interp.declare_op = false; return token::FIX;
infixr     yylval->fix = infixr; interp.declare_op = false; return token::FIX;
prefix     yylval->fix = prefix; interp.declare_op = false; return token::FIX;
postfix    yylval->fix = postfix; interp.declare_op = false; return token::FIX;
outfix     return token::OUTFIX;
nonfix     return token::NONFIX;
private    return token::PRIVATE;
public     return token::PUBLIC;
const      return token::CONST;
def        return token::DEF;
let        return token::LET;
type       return token::TYPE;
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
namespace  BEGIN(xusing); return token::NAMESPACE;

^{cmd} {
 parse_cmd:
  /* These are treated as commands in interactive mode, and as ordinary
     (operator or identifier) symbols otherwise. */
  if ((interp.interactive || interp.output) && checkcmd(interp, yytext)) {
    /* Read the rest of the command line. */
    bool esc = yytext[0] == '^';
    string cmd = yytext+esc, cmdline = yytext+esc;
    register int c;
    int count = 0;
    while ((c = yyinput()) != EOF && c != 0 && c != '\n') {
      cmdline.append(1, c);
      count++;
    }
    if (c == '\n')
      yylloc->lines(1);
    else
      yylloc->columns(count);
    docmd(interp, yylloc, cmd.c_str(), cmdline.c_str(), esc);
  } else if (yytext[0] == '!' || yytext[0] == '^')
    goto parse_op;
  else {
    check(*yylloc, yytext, false);
    goto parse_id;
  }
}

<xtag>::{id} BEGIN(INITIAL); goto parse_tag;
{qual}{id} {
  string qualid = yytext;
  size_t k = symsplit(qualid);
  string qual = qualid.substr(0, k), id = qualid.substr(k+2);
  bool might_be_tag = qual.find("::") == string::npos;
  int32_t tag = might_be_tag && checktag(id.c_str());
  if (!find_namespace(interp, qualid)) {
    // not a valid namespace prefix
    if (tag && find_namespace(interp, qual)) {
      // we can still parse this as an identifier with a type tag
      yyless(k); qualid = yytext; BEGIN(xtag);
      //check(*yylloc, yytext, false);
    } else {
      string msg = "unknown namespace '"+qual+"'";
      if (might_be_tag && !tag) msg += ", or invalid type tag '"+id+"'";
      interp.error(*yylloc, msg);
    }
  }
  if (interp.declare_op) {
    yylval->sval = new string(yytext);
    return token::ID;
  }
  symbol* sym = interp.symtab.lookup(yytext);
  if (sym &&
      ((sym->prec >= 0 && sym->prec < PREC_MAX) || sym->fix == outfix)) {
    if (strstr(yytext, "::")) {
      // Return a new qualified instance here.
      yylval->xval = new expr(sym->f);
      yylval->xval->flags() |= EXPR::QUAL;
    } else
      yylval->xval = new expr(sym->x);
    if (sym->fix == outfix)
      return sym->g?token::LO:token::RO;
    else
      return optok(sym->f, sym->fix);
  } else {
    if (!interp.nerrs && !sym && interp.symtab.count != 1 &&
	(k = symsplit(qualid)) != string::npos) {
      qual = qualid.substr(0, k);
      if (qual.compare(0, 2, "::") == 0) qual.erase(0, 2);
      if (qual != *interp.symtab.current_namespace) {
	string msg = "undeclared symbol '"+string(yytext)+"'";
	interp.error(*yylloc, msg);
      }
    }
    yylval->sval = new string(yytext);
    return token::ID;
  }
}
{id}       {
 parse_id:
  if (interp.declare_op) {
    yylval->sval = new string(yytext);
    int c = yyinput(); unput(c);
    if (ispunct(c) && c != ';') {
      string id = *yylval->sval, sym = string(1, c);
      string msg = "warning: dubious trailing punctuation '"+sym+
	"' at symbol '"+id+"'";
      interp.warning(*yylloc, msg);
    }
    return token::ID;
  }
  symbol* sym = interp.symtab.lookup(yytext);
  if (sym && ((sym->prec >= 0 && sym->prec < PREC_MAX) || sym->fix == outfix)) {
    yylval->xval = new expr(sym->x);
    if (sym->fix == outfix)
      return sym->g?token::LO:token::RO;
    else
      return optok(sym->f, sym->fix);
  } else {
    yylval->sval = new string(yytext);
    return token::ID;
  }
}
::{blank}+({qual})?{id} {
 parse_tag:
  char *s = yytext+2;
  while (isspace(*s)) s++;
  yylval->ival = checktag(s);
  if (yylval->ival)
    return token::TAG;
  else {
    string msg = "invalid type tag '"+string(s)+"'";
    interp.error(*yylloc, msg);
  }
}
<rescan>[@=|;()\[\]{}\\] |
[@=|;()\[\]{}\\] BEGIN(INITIAL); return yy::parser::token_type(yytext[0]);
"->"       return token::MAPSTO;
"#<"{id}(" "{int})?">" return token::BADTOK;
{qual}([[:punct:]]|{punct})+  {
  string qualid = yytext;
  size_t k = symsplit(qualid);
  string qual = qualid.substr(0, k), id = qualid.substr(k+2);
  if (!find_namespace(interp, qualid)) {
    // not a valid namespace prefix
    string msg = "unknown namespace '"+qual+"'";
    interp.error(*yylloc, msg);
  }
  k+=2;
  if (yytext[k] == '/' && yytext[k+1] == '*') {
    /* This is actually a comment starter. Back out and complain about a bad
       qualified symbol */
    yyless(k);
    string msg = "invalid qualified symbol '"+pstring(yytext)+"'";
    interp.error(*yylloc, msg);
    break;
  }
  while ((int)yyleng > (int)k+1 && yytext[yyleng-1] == ';') yyless(yyleng-1);
  if (interp.declare_op) {
    yylval->sval = new string(yytext);
    return token::ID;
  }
  symbol* sym = interp.symtab.lookup(yytext);
  while (!sym && (int)yyleng > (int)k+1) {
    if (yyleng == 2 && yytext[0] == '-' && yytext[1] == '>')
      return token::MAPSTO;
    yyless(yyleng-1);
    sym = interp.symtab.lookup(yytext);
  }
  if (sym) {
    if (sym->prec < PREC_MAX || sym->fix == outfix) {
      if (strstr(yytext, "::")) {
	// Return a new qualified instance here.
	yylval->xval = new expr(sym->f);
	yylval->xval->flags() |= EXPR::QUAL;
      } else
	yylval->xval = new expr(sym->x);
      if (sym->fix == outfix)
	return sym->g?token::LO:token::RO;
      else
	return optok(sym->f, sym->fix);
    } else {
      yylval->sval = new string(yytext);
      return token::ID;
    }
  }
  /* Not a valid symbol. */
  string msg = "invalid qualified symbol '"+pstring(yytext)+"'";
  interp.error(*yylloc, msg);
}
([[:punct:]]|{punct})+  {
 parse_op:
  if (yytext[0] == '/' && yytext[1] == '*') {
    yyless(2);
    goto parse_comment; // comment starter
  }
  while (yyleng > 1 && yytext[yyleng-1] == ';') yyless(yyleng-1);
  if (interp.declare_op) {
    yylval->sval = new string(yytext);
    return token::ID;
  }
  symbol* sym = interp.symtab.lookup(yytext);
  while (!sym && yyleng > 1) {
    if (yyleng == 2 && yytext[0] == '-' && yytext[1] == '>')
      return token::MAPSTO;
    yyless(yyleng-1);
    sym = interp.symtab.lookup(yytext);
  }
  if (sym) {
    if (sym->prec < PREC_MAX || sym->fix == outfix) {
      yylval->xval = new expr(sym->x);
      if (sym->fix == outfix)
	return sym->g?token::LO:token::RO;
      else
	return optok(sym->f, sym->fix);
    } else {
      yylval->sval = new string(yytext);
      return token::ID;
    }
  }
  assert(yyleng == 1);
  /* If we come here, we failed to recognize the input as a special symbol and
     have to rescan everything in a special mode which excludes this
     rule. This hack is necessary in order to avoid the use of REJECT. */
  yyless(0);
  BEGIN(rescan);
}
<rescan>.|{punct}|{letter} |
.|{punct}|{letter} {
  string msg = "invalid character '"+pstring(yytext)+"'";
  interp.error(*yylloc, msg);
  BEGIN(INITIAL);
}

%%

static char *my_buf = NULL, *my_bufptr = NULL;
static int len = 0;

static void lex_input(const char *prompt, char *buf,
		      size_t &result, size_t max_size)
{
  if (!my_buf) {
    interpreter::g_interp->debug_init();
    if (command_input) {
      char *s = command_input(prompt);
      if (!s) {
	// EOF, bail out
	result = 0;
	return;
      }
      my_bufptr = my_buf = toutf8(s);
      free(s);
      if (!my_buf) {
	// memory allocation error, bail out
	result = 0;
	return;
      }
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
      my_bufptr = my_buf = toutf8(s);
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
  if (k > (int)max_size) k = max_size;
  // copy chars to the buffer
  strncpy(buf, my_bufptr, k);
  if (k > l) {
    // finish off with trailing newline, get rid of the buffer
    buf[l] = '\n';
    free(my_buf); my_buf = my_bufptr = NULL; len = 0;
  }
  result = k;
}

bool
interpreter::lex_begin(const string& fname, bool esc)
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
    BEGIN(esc?escape:INITIAL);
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

#ifdef HAVE_LLVM_SUPPORT_RAW_OSTREAM_H
#include <llvm/Support/raw_ostream.h>
#ifdef HAVE_LLVM_SUPPORT_RAW_OS_OSTREAM_H
#include <llvm/Support/raw_os_ostream.h>
#endif
#endif

void Env::print(ostream& os) const
{
  if (!f) return; // not used, probably a shadowed rule
  {
#if HAVE_LLVM_SUPPORT_RAW_OSTREAM_H
    llvm::raw_os_ostream raw_os(os);
#else
    ostream& raw_os = os;
#endif
    if (h && h != f) h->print(raw_os);
    f->print(raw_os);
  }
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

static string pstring(const char *s)
{
  if ((s[0] && s[1]) || isprint(s[0]))
    return string(s);
  else {
    char buf[10];
    sprintf(buf, "\\%u", (unsigned)(unsigned char)s[0]);
    return string(buf);
  }
}

static bool is_id(const string& name)
{
  if (name.empty() || !(isalpha(name[0]) || name[0] == '_')) return false;
  for (size_t i = 1, n = name.length(); i < n; i++)
    if (name[i] == ':') {
      if (++i >= n || name[i] != ':')
	return false;
    } else if (!isalnum(name[i]) && name[0] != '_')
      return false;
  return true;
}

static string format_namespace(const string& name)
{
  if (is_id(name))
    return name;
  else
    return "\""+name+"\"";
}

static bool find_namespace(interpreter& interp, const string& name)
{
  // determine the namespace prefix
  size_t k = symsplit(name);
  if (k == 0 || k == string::npos) return true; // default namespace
  string qual = name.substr(0, k);
  if (qual.compare(0, 2, "::") == 0) {
    // absolute namespace qualifier
    qual.erase(0, 2);
    return interp.namespaces.find(qual) != interp.namespaces.end();
  }
  // search the default namespace
  if (interp.namespaces.find(qual) != interp.namespaces.end())
    return true;
  // search the current namespace
  string& ns = *interp.symtab.current_namespace;
  if (!ns.empty() &&
      interp.namespaces.find(ns+"::"+qual) != interp.namespaces.end())
    return true;
  // search the search namespaces
  for (map< string, set<int32_t> >::iterator
	 it = interp.symtab.search_namespaces->begin(),
	 end = interp.symtab.search_namespaces->end(); it != end; it++) {
    const string& ns = it->first;
    if (!ns.empty() &&
	interp.namespaces.find(ns+"::"+qual) != interp.namespaces.end())
      return true;
  }
  return false;
}

/* This is a watered-down version of the command completion routine from
   pure.cc, used to implement the completion_matches command used by
   pure-mode.el. This isn't perfect, since it lacks filename completion
   support and will also complete command names when not at the beginning of
   the line. But since the location inside the line isn't passed to
   completion_matches, it's the best that we can do right now. Also note that
   this code doesn't need readline in any way, so it will work even if the
   interpreter is built without readline or libedit support. */

static const char *commands[] = {
  "break", "bt", "cd", "clear", "const", "def", "del", "dump", "extern", "help",
  "infix", "infixl", "infixr", "let", "ls", "mem", "namespace", "nonfix",
  "outfix", "override", "postfix", "prefix", "private", "public", "pwd",
  "quit", "run", "save", "show", "stats", "trace", "type", "underride",
  "using", 0
};

static bool inline checksym(int32_t f, const set<int32_t>& syms)
{
  return syms.empty() || syms.find(f) != syms.end();
}

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
	/* Look for a symbol in the current namespace, the __cmd__ namespace
	   and the search namespaces. */
	size_t p = s.rfind(text);
	if (p != string::npos &&
	    (p == 0 || (p > 1 && s.compare(p-2, 2, "::") == 0))) {
	  string prefix = s.substr(0, p?p-2:p),
	    name = s.substr(p, string::npos);
	  bool found = prefix==*interp.symtab.current_namespace ||
	    (prefix=="__cmd__" && checkusercmd(interp, name.c_str()));
	  for (map< string, set<int32_t> >::iterator
		 it = interp.symtab.search_namespaces->begin(),
		 end = interp.symtab.search_namespaces->end();
	       !found && it != end; it++)
	    found = prefix==it->first && checksym(f, it->second);
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
pure_completion(const char *text)
{
  int count = 0, alloc = 1024;
  char *match, **matches;
  match = command_generator(text, 0);
  if (!match) return NULL;
  matches = (char**)malloc(alloc*sizeof(char*));
  if (!matches) return NULL;
  matches[count++] = strdup(text);
  while (match) {
    if (count+1 >= alloc) {
      alloc += 256;
      char **matches1 = (char**)realloc(matches, alloc*sizeof(char*));
      if (!matches1) {
	free(matches);
	return NULL;
      }
      matches = matches1;
    }
    matches[count++] = match;
    match = command_generator(text, 1);
  }
  matches[count++] = NULL;
  return matches;
}

static void list_completions(ostream& os, const char *s)
{
  char **matches = pure_completion(s);
  if (matches) {
    if (matches[0]) {
      if (!matches[1]) {
	os << matches[0] << '\n';
	free(matches[0]);
      } else {
	int i;
	free(matches[0]);
	for (i = 1; matches[i]; i++) {
	  os << matches[i] << '\n';
	  free(matches[i]);
	}
      }
    }
    free(matches);
  }
}

#if CHECK_NAMES
static void check(const yy::location& l, const char* s, bool decl)
{
  if (decl) {
    assert(interpreter::g_interp);
    interpreter& interp = *interpreter::g_interp;
    if (!interp.symtab.current_namespace->empty()) return;
  }
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
#endif

static int32_t checktag(const char *s)
{
  int32_t ret = 0;
  bool qual = strstr(s, "::") != 0;
  const char *id = 0;
  interpreter& interp = *interpreter::g_interp;
  symbol* sym = interp.symtab.lookup(s);
  if (sym && sym->prec == PREC_MAX && sym->fix != outfix) {
    ret = sym->f;
    id = sym->s.c_str();
  }
  if (qual) return ret; // qualified symbol, must be declared already
  // We have an unqualified symbol. Either it is one of the built-in type
  // tags, or some other symbol (if necessary, we create the symbol on the fly
  // in the global namespace).
  if (strcmp(s, "int") == 0)
    return EXPR::INT;
  else if (strcmp(s, "bigint") == 0)
    return EXPR::BIGINT;
  else if (strcmp(s, "double") == 0)
    return EXPR::DBL;
  else if (strcmp(s, "string") == 0)
    return EXPR::STR;
  else if (strcmp(s, "pointer") == 0)
    return EXPR::PTR;
  else if (strcmp(s, "matrix") == 0)
    return EXPR::MATRIX;
  else {
    sym = interp.symtab.sym(s);
    if (sym)
      return sym->f;
    else
      return 0;
  }
}

static inline char upcase(char c)
{
  if (c < 'a')
    return c;
  else
    return c-32;
}

static bool checkint(const char *s, string& msg)
{
  char maxdigit = '9';
  if (*s == '0') {
    ++s;
    switch (upcase(*s)) {
    case 'X':
      maxdigit = 'F';
      ++s;
      break;
    case 'B':
      maxdigit = '1';
      ++s;
      break;
    default:
      maxdigit = '7';
      break;
    }
  }
  while (*s && upcase(*s) <= maxdigit) ++s;
  if (*s == 'L' && s[1] == 0) return true; // bigint constant
  if (*s) {
    msg = "invalid digit '"+string(1, *s)+"' in ";
    switch (maxdigit) {
    case '1': msg += "binary"; break;
    case '7': msg += "octal"; break;
    case 'F': msg += "hexadecimal"; break;
    default: msg += "decimal"; break;
    }
    msg += " integer constant";
    return false;
  } else
    return true;
}

static string xsym(const string& ns, const string& s)
{
  if (ns.empty())
    return "::"+s;
  else
    return "::"+ns+"::"+s;
}

/* Interactive command processing. */

static const char *builtin_commands[] = {
  "!", "break", "bt", "cd", "clear", "completion_matches", "del", "dump",
  "help", "help_index", "help_matches", "ls", "mem", "override", "pwd",
  "quit", "run", "save", "show", "stats", "trace", "underride"
};

static int mycmp(const void *a, const void *b)
{
  return strcmp(*(char* const*)a, *(char* const*)b);
}

static bool checkusercmd(interpreter &interp, const char *s)
{
  symbol* sym = interp.symtab.lookup(string("::__cmd__::")+s);
  return sym && !sym->priv &&
    ((interp.globenv.find(sym->f) != interp.globenv.end()
      && interp.globenv[sym->f].t == env_info::fun) ||
     interp.externals.find(sym->f) != interp.externals.end());
}

static bool checkcmd(interpreter &interp, const char *s)
{
  size_t nel = sizeof(builtin_commands)/sizeof(builtin_commands[0]);
  if (s[0]=='^') s++;
  return bsearch(&s, builtin_commands, nel, sizeof(builtin_commands[0]), mycmp)
    || checkusercmd(interp, s);
}

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
	  cerr << m << ": " << msg << '\n';
	  return;
	}
      }
      l.push_back(t); c++;
      while (isspace(*s)) ++s;
    } while (*s);
    ok = true;
  }
};

static inline bool sym_match(bool gflag, const string& arg, const string& sym)
{
  bool qual = sym.find("::")!=string::npos;
  bool match = gflag?!fnmatch(arg.c_str(), sym.c_str(), 0):(arg == sym);
  if (match)
    return true;
  else if (qual)
    return false;
  string sym1 = "::"+sym;
  if (gflag)
    return !fnmatch(arg.c_str(), sym1.c_str(), 0);
  else
    return arg == sym1;
}

typedef map<int32_t,ExternInfo> extmap;

struct env_sym {
  const symbol* sym;
  env::const_iterator it, jt, kt;
  extmap::const_iterator xt;
  env_sym(const symbol& _sym, env::const_iterator _it,
	  env::const_iterator _jt, env::const_iterator _kt,
	  extmap::const_iterator _xt)
    : sym(&_sym), it(_it), jt(_jt), kt(_kt), xt(_xt) { }
};

static bool env_compare(env_sym s, env_sym t)
{
  return s.sym->s < t.sym->s;
}

static int argcmp(const char *s, const char *t)
{
  while (isspace(*s)) ++s;
  size_t n = strlen(t);
  int res = strncmp(s, t, n);
  if (res) return res;
  s += n;
  // Ignore trailing whitespace.
  while (isspace(*s)) ++s;
  if (*s)
    return 1;
  else
    return 0;
}

static const bool yes_or_no(const string& msg)
{
  char ans;
  cout << msg << " ";
  cin >> noskipws >> ans;
  bool res = cin.good() && ans == 'y';
  while (cin.good() && ans != '\n') cin >> noskipws >> ans;
  if (!cin.good()) cout << endl;
  cin.clear();
  return res;
}

#ifdef _WIN32
#define PATHSEP ";"
#else
#define PATHSEP ":"
#endif

static string decl_str(interpreter &interp, const symbol& sym,
		       bool defined = false)
{
  ostringstream sout;
  if (sym.priv &&
      (sym.prec < PREC_MAX || sym.fix == nonfix || sym.fix == outfix))
    sout << "private ";
  if (sym.fix == nonfix)
    sout << "nonfix " << sym.s << ";\n";
  else if (sym.fix == outfix && sym.g)
    sout << "outfix " << sym.s << " "
	 << interp.symtab.sym(sym.g).s << ";\n";
  else if (sym.prec < PREC_MAX) {
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
    case nonfix:
    case outfix:
      assert(0 && "this can't happen"); break;
    }
    sout << " " << (int)sym.prec << " " << sym.s << ";\n";
  }
  if (defined)
    sout << "#! --defined " << sym.s << "\n";
  return sout.str();
}

class Index {
  FILE *fp;
  void scan();
public:
  map<string,string> m;
  Index() : fp(0) {}
  Index(const string& fname) : fp(fopen(fname.c_str(), "r"))
  { if (fp) { scan(); fclose(fp); } }
  bool lookup(const string& key, string& target)
  {
    map<string,string>::const_iterator it = m.find(key);
    if (it != m.end()) {
      target = it->second;
      return true;
    }
    size_t p = key.rfind('(');
    if (p != string::npos && p>0)
      return false;
    // Try to find a function, macro etc. of this name.
    list<string> fixities, symtypes;
    fixities.push_back("");
    fixities.push_back("prefix");
    fixities.push_back("postfix");
    fixities.push_back("infix");
    fixities.push_back("outfix");
    symtypes.push_back("function");
    symtypes.push_back("macro");
    symtypes.push_back("extern");
    symtypes.push_back("constructor");
    symtypes.push_back("variable");
    symtypes.push_back("constant");
    symtypes.push_back("module");
    for (list<string>::const_iterator s = symtypes.begin();
	 s != symtypes.end(); s++) {
      for (list<string>::const_iterator f = fixities.begin();
	   f != fixities.end(); f++) {
	string mykey = key+" ("+(f->empty()?*s:(*f)+" "+(*s))+")";
	it = m.find(mykey);
	if (it != m.end()) {
	  target = it->second;
	  return true;
	}
	if (*s == "variable" || *s == "constant" || *s == "module")
	  // These don't have any fixities.
	  break;
      }
      // Search for commands.
      string mykey = key+" (command)";
      it = m.find(mykey);
      if (it != m.end()) {
	target = it->second;
	return true;
      }
    }
    return false;
  }
};

void Index::scan()
{
  // We don't want to depend on libxml2, so we do a rather simple-minded
  // textual scan here. This should be good enough for the kind of html
  // found in the index file generated by sphinx.
  register int c;
  int level = 0;
  string last_key, last_subkey;
  while ((c = fgetc(fp)) != EOF) {
    if (c == '<') {
      // scan for the end of this tag
      string tag;
      while ((c = fgetc(fp)) != EOF && c != '>') {
	tag += c;
	if (c == '"') {
	  // scan attribute value
	  while ((c = fgetc(fp)) != EOF && c != '"') {
	    tag += c;
	    if (c == '\\' && (c = fgetc(fp)) != EOF)
	      tag += c;
	  }
	  if (c != EOF) tag += c;
	}
      }
      if (tag == "dl")
	// start of subentry list
	level++;
      else if (tag == "/dl")
	// end of subentry list
	level--;
      else if (level==1 && tag == "dt") {
	if ((c = fgetc(fp)) != EOF && c != '<') {
	  // this looks like an index entry which isn't a search key itself,
	  // but to be prepended to the following subentries
	  string key; if (!isspace(c)) key += c;
	  while ((c = fgetc(fp)) != EOF && c != '<') {
	    if (c == '&') {
	      // scan an entity name
	      string entity;
	      while ((c = fgetc(fp)) != EOF && c != ';') entity += c;
	      if (entity == "amp")
		key += '&';
	      else if (entity == "lt")
		key += '<';
	      else if (entity == "gt")
		key += '>';
	      else
		key += "&"+entity+";";
	    } else if (!key.empty() || !isspace(c))
	      key += c;
	  }
	  // trim trailing whitespace
	  size_t p = key.find_last_not_of(" \t\n\r\f\v");
	  if (p == string::npos)
	    key.clear();
	  else
	    key.erase(p+1);
	  last_key = key;
	}
	if (c != EOF) ungetc(c, fp);
      } else if (level>0 &&
	       tag.compare(0, 8, "a href=\"") == 0 && tag[8] != '#') {
	// we found an index entry, get the target
	string target, key;
	target = tag.substr(8);
	assert(!target.empty() && target[target.size()-1]=='"');
	target.erase(target.size()-1);
	// scan the key
	while ((c = fgetc(fp)) != EOF) {
	  if (c == '<') {
	    // scan a (sub)tag
	    string subtag;
	    while ((c = fgetc(fp)) != EOF && c != '>') subtag += c;
	    // ignore anything but </a>
	    if (subtag == "/a") break;
	  } else if (c == '&') {
	    // scan an entity name
	    string entity;
	    while ((c = fgetc(fp)) != EOF && c != ';') entity += c;
	    if (entity == "amp")
	      key += '&';
	    else if (entity == "lt")
	      key += '<';
	    else if (entity == "gt")
	      key += '>';
	    else if (!entity.empty() && entity[0] == '#') {
	      int c = atoi(entity.substr(1).c_str());
	      if (c > 0)
		key += c;
	      else
		key += "&"+entity+";";
	    } else
	      key += "&"+entity+";";
	  } else if (!key.empty() || !isspace(c))
	    key += c;
	}
	// trim trailing whitespace
	size_t p = key.find_last_not_of(" \t\n\r\f\v");
	if (p == string::npos)
	  key.clear();
	else
	  key.erase(p+1);
	if (key.empty()) continue;
	if (key[0] == '-') {
	  size_t p = key.find_first_not_of('-');
	  if (p != string::npos && p<=2 && isalpha(key[p])) {
	    // this looks like a command line option, strip arguments and
	    // create an index entry for it if not already present
	    string mykey = key;
	    p = key.find_first_of(" [", p);
	    if (p != string::npos) mykey = key.substr(0, p);
	    if (m.find(mykey) == m.end()) m[mykey] = target;
	    if (level>1 && !last_key.empty()) {
	      // this is a subentry, try to extract the program name from it
	      // and add another index entry under the program name
	      p = last_key.find("command line option");
	      if (p != string::npos && p>0 && last_key[p-1] == ' ' &&
		  last_key.substr(p) == "command line option") {
		mykey = last_key.substr(0, p)+mykey;
		if (m.find(mykey) == m.end()) m[mykey] = target;
	      }
	    }
	  }
	}
	if (key[0] == '[' && key[key.size()-1] == ']' &&
	    key.find_first_not_of("0123456789", 1) >= key.size()-1) {
	  // secondary search term, prepend the previous key
	  string last = (level>1)?last_subkey:last_key;
	  key.insert(0, last);
	} else if (level>1) {
	  // this is a subentry, prepend the last key we found
	  if (!last_key.empty()) key.insert(0, last_key+" ");
	  last_subkey = key;
	} else {
	  size_t p = key.rfind('(');
	  if (p != string::npos && p>0) {
	    p = key.find_last_not_of(' ', p-1);
	    if (p != string::npos)
	      last_key = key.substr(0, p+1);
	    else
	      last_key = key;
	  } else
	    last_key = key;
	}
	m[key] = target;
      }
    }
  }
}

static void docmd(interpreter &interp, yy::parser::location_type* yylloc, const char *cmd, const char *cmdline, bool esc)
{
  static Index *idx = NULL;
  if (interp.restricted) {
    cerr << cmd << ": operation not implemented\n";
  } else if (strcmp(cmd, "!") == 0) {
    const char *s = cmdline+1;
    while (isspace(*s)) ++s;
    if (system(s) == -1) perror("system");
  } else if (!esc && checkusercmd(interp, cmd)) {
    // user-defined commands override builtins
    symbol* sym = interp.symtab.lookup(string("::__cmd__::")+cmd);
    assert(sym);
    // get arguments
    string args = cmdline+strlen(cmd);
    // trim leading and trailing whitespace
    size_t pos = args.find_first_not_of(" \t");
    if (pos != string::npos) args.erase(0, pos);
    pos = args.find_last_not_of(" \t");
    if (pos != string::npos) args.erase(pos+1);
    pure_expr *e, *x =
      pure_appx(pure_symbol(sym->f), pure_cstring_dup(args.c_str()), &e);
    char *s;
    if (x && pure_is_cstring_dup(x, &s)) {
      if (*s) {
	cout << s;
	if (s[strlen(s)-1] != '\n') cout << endl;
      }
      free(s);
    } else if (e && pure_is_cstring_dup(e, &s)) {
      if (*s) {
	cerr << cmd << ": " << s;
	if (s[strlen(s)-1] != '\n') cerr << endl;
      }
      free(s);
    }
    if (x) pure_freenew(x);
    if (e) pure_freenew(e);
  } else if (strcmp(cmd, "bt") == 0)  {
    const char *s = cmdline+2;
    argl args(s, "bt");
    if (!args.ok)
      ;
    else if (!interp.debugging)
      cerr << "bt: debugging not enabled (try run -g)\n";
    else if (args.c > 0)
      cerr << "bt: extra parameter\n";
    else if (interp.output)
      interp.backtrace(*interp.output);
    else {
      FILE *fp;
      const char *more = getenv("PURE_MORE");
      // FIXME: We should check that 'more' actually exists here.
      if (more && *more && isatty(fileno(stdin)) && (fp = popen(more, "w"))) {
	ostringstream sout;
	interp.backtrace(sout);
	fputs(sout.str().c_str(), fp);
	pclose(fp);
      } else
	interp.backtrace(cout);
    }
  } else if (strcmp(cmd, "break") == 0)  {
    const char *s = cmdline+5;
    argl args(s, "break");
    if (!args.ok)
      ;
    else if (!interp.debugging)
      cerr << "break: debugging not enabled (try run -g)\n";
    else if (args.c == 0) {
      ostringstream sout;
      list<string> syms;
      for (set<int32_t>::iterator it = interp.breakpoints.begin();
	   it != interp.breakpoints.end(); ++it)
	syms.push_back(interp.symtab.sym(*it).s);
      syms.sort();
      for (list<string>::iterator it = syms.begin(); it != syms.end(); ++it)
	sout << *it << '\n';
      if (interp.output)
	(*interp.output) << sout.str();
      else
	cout << sout.str();
    } else {
      for (list<string>::iterator it = args.l.begin();
	   it != args.l.end(); ++it) {
	const char *s = it->c_str();
	int32_t f = pure_getsym(s);
	if (f > 0) {
	  env::const_iterator jt = interp.globenv.find(f);
	  if ((jt != interp.globenv.end() && jt->second.t == env_info::fun) ||
	      interp.externals.find(f) != interp.externals.end())
	    if (interp.breakpoints.find(f) == interp.breakpoints.end())
	      interp.breakpoints.insert(f);
	    else
	      cerr << "break: breakpoint '" << s << "' already set\n";
	  else
	    f = 0;
	}
	if (f == 0)
	  cerr << "break: unknown function symbol '" << s << "'\n";
      }
    }
  } else if (strcmp(cmd, "trace") == 0)  {
    const char *s = cmdline+5;
    argl args(s, "trace");
    bool mflag = false;
    if (!args.ok) goto trace_out;
    // process option arguments
    if (args.c >= 1) {
      mflag = args.l.front() == "-m";
      if (mflag) {
	  args.c--;
	  args.l.pop_front();
      }
    }
    if (!mflag && !interp.debugging) {
      cerr << "trace: debugging not enabled (try run -g)\n";
      goto trace_out;
    }
    if (args.c == 0) {
      ostringstream sout;
      list<string> syms;
      if (mflag) {
	for (set<int32_t>::iterator it = interp.mac_tracepoints.begin();
	     it != interp.mac_tracepoints.end(); ++it)
	  syms.push_back(interp.symtab.sym(*it).s);
      } else {
	for (set<int32_t>::iterator it = interp.tracepoints.begin();
	     it != interp.tracepoints.end(); ++it)
	  syms.push_back(interp.symtab.sym(*it).s);
      }
      syms.sort();
      for (list<string>::iterator it = syms.begin(); it != syms.end(); ++it)
	sout << *it << '\n';
      if (interp.output)
	(*interp.output) << sout.str();
      else
	cout << sout.str();
    } else if (mflag) {
      for (list<string>::iterator it = args.l.begin();
	   it != args.l.end(); ++it) {
	const char *s = it->c_str();
	int32_t f = pure_getsym(s);
	if (f > 0) {
	  env::const_iterator jt = interp.macenv.find(f);
	  if ((jt != interp.macenv.end() && jt->second.t == env_info::fun) ||
	      f == interp.symtab.eval_sym().f ||
	      f == interp.symtab.ifelse_sym().f ||
	      f == interp.symtab.lambda_sym().f ||
	      f == interp.symtab.case_sym().f ||
	      f == interp.symtab.when_sym().f ||
	      f == interp.symtab.with_sym().f)
	    if (interp.mac_tracepoints.find(f) == interp.mac_tracepoints.end())
	      interp.mac_tracepoints.insert(f);
	    else
	      cerr << "trace: macro tracepoint '" << s << "' already set\n";
	  else
	    f = 0;
	}
	if (f == 0)
	  cerr << "trace: unknown macro symbol '" << s << "'\n";
      }
    } else {
      for (list<string>::iterator it = args.l.begin();
	   it != args.l.end(); ++it) {
	const char *s = it->c_str();
	int32_t f = pure_getsym(s);
	if (f > 0) {
	  env::const_iterator jt = interp.globenv.find(f);
	  if ((jt != interp.globenv.end() && jt->second.t == env_info::fun) ||
	      interp.externals.find(f) != interp.externals.end())
	    if (interp.tracepoints.find(f) == interp.tracepoints.end())
	      interp.tracepoints.insert(f);
	    else
	      cerr << "trace: tracepoint '" << s << "' already set\n";
	  else
	    f = 0;
	}
	if (f == 0)
	  cerr << "trace: unknown function symbol '" << s << "'\n";
      }
    }
  trace_out: ;
  } else if (strcmp(cmd, "del") == 0)  {
    const char *s = cmdline+3;
    argl args(s, "del");
    if (args.ok) {
      bool bflag = false, tflag = false, mflag = false;
      const char *ty = "break";
      char msg[100];
      if (args.c >= 1) {
	bflag = args.l.front() == "-b";
	tflag = args.l.front() == "-t";
	mflag = args.l.front() == "-m";
	if (bflag || tflag || mflag) {
	  args.c--;
	  args.l.pop_front();
	  if (tflag) ty = "trace";
	  if (mflag) ty = "macro trace";
	} else
	  bflag = tflag = true;
      } else
	bflag = tflag = true;
      if (args.c == 0) {
	if ((bflag && !interp.breakpoints.empty()) ||
	    (tflag && !interp.tracepoints.empty()) ||
	    (mflag && !interp.mac_tracepoints.empty())) {
	  sprintf(msg, "This will clear all %spoints. Continue (y/n)?", ty);
	  if (yes_or_no(msg)) {
	    if (bflag) interp.breakpoints.clear();
	    if (tflag) interp.tracepoints.clear();
	    if (mflag) interp.mac_tracepoints.clear();
	  }
	} else {
	  sprintf(msg, "del: no %spoints\n", ty);
	  cerr << msg;
	}
      } else {
	for (list<string>::iterator it = args.l.begin();
	     it != args.l.end(); ++it) {
	  const char *s = it->c_str();
	  int32_t f = pure_getsym(s);
	  if (f > 0) {
	    if (mflag) {
	      if (interp.mac_tracepoints.find(f) !=
		  interp.mac_tracepoints.end())
		interp.mac_tracepoints.erase(f);
	      else {
		sprintf(msg, "del: unknown %spoint '", ty);
		cerr << msg << s << "'\n";
	      }
	    } else {
	      if (bflag &&
		  interp.breakpoints.find(f) != interp.breakpoints.end()) {
		interp.breakpoints.erase(f);
		if (tflag) interp.tracepoints.erase(f);
	      } else if (tflag &&
			 interp.tracepoints.find(f) != interp.tracepoints.end())
		interp.tracepoints.erase(f);
	      else {
		sprintf(msg, "del: unknown %spoint '", ty);
		cerr << msg << s << "'\n";
	      }
	    }
	  } else
	    cerr << "del: unknown " << (mflag?"macro":"function")
		 << " symbol '" << s << "'\n";
	}
      }
    }
  } else if (strcmp(cmd, "help") == 0)  {
    static FILE *fp = NULL;
    const char *s = cmdline+4, *p, *q;
    while (isspace(*s)) ++s;
    string docname = s, default_doc = interp.libdir+"docs/index.html";
    const char *browser = getenv("PURE_HELP");
    // Check that the documentation is installed.
    if (!fp) {
      fp = fopen(default_doc.c_str(), "r");
      if (!fp) {
	static bool init = false;
	cerr << "help: couldn't find online documentation\n";
	if (!init) {
	  cerr << "\n\
Most likely this just means that you haven't installed the documentation\n\
files in the pure-docs package yet. Please check the Pure wiki or the INSTALL\n\
file for instructions on how to do this.\n";
	  init = true;
	}
	goto errout;
      }
      fclose(fp);
    }
    if (!*s)
      // default is to load the top page
      docname = "file:"+default_doc;
    else if ((p = strchr(s, ':')) && p>s && p[1] != ':' &&
	     (!(q = strchr(s, '#')) || q>p)) {
      // proper URL, take as is
      ;
    } else if ((p = strchr(s, '#')) && p>s) {
      // target in library documentation, add path if necessary
      if (!(q = strchr(s, '/')) || q>=p) {
	if (!(q = strchr(s, '.')) || q>=p)
	  // no filename extension, assume .html
	  docname.insert(p-s, ".html");
	docname.insert(0, interp.libdir+"docs/");
      }
      docname.insert(0, "file:");
    } else if (strcmp(s, "copying") == 0) {
      // look up the license information
      docname.insert(0, "file:"+interp.libdir+"docs/pure.html#");
    } else {
      // perform an index lookup
      string target;
      if (!idx) idx = new Index(interp.libdir+"docs/genindex.html");
      if (idx->lookup(s, target))
	docname = "file:"+interp.libdir+"docs/"+target;
      else {
	// not an index entry, assume it's a target in the Pure manual
	docname.insert(0, "file:"+interp.libdir+"docs/pure.html#");
      }
    }
    assert(!docname.empty());
    // remove a lone trailing '#'
    if (docname[docname.length()-1] == '#')
      docname.erase(docname.length()-1);
    // invoke the browser
    if (!browser && (browser = getenv("BROWSER"))) {
      char *browsercmd = strdup(browser);
      if (browsercmd) {
	char *part;
	part = strtok(browsercmd, PATHSEP);
	do {
	  char buf[4096];
	  if (strstr(part, "%s"))
	    snprintf(buf, sizeof(buf), part, docname.c_str());
	  else
	    snprintf(buf, sizeof(buf), "%s %s", part, docname.c_str());
	  if (system(buf) == 0) break;
	} while ((part = strtok(NULL, PATHSEP)));
	free(browsercmd);
      } else
	cerr << "help: memory allocation error\n";
    } else {
      if (!browser) browser = "w3m"; // default
      string helpcmd = string(browser) + " \"" + docname + "\"";
      if (system(helpcmd.c_str()) == -1) perror("system");
    }
  errout: ;
  } else if (strcmp(cmd, "ls") == 0)  {
    if (system(cmdline) == -1) perror("system");
  } else if (strcmp(cmd, "pwd") == 0)  {
    if (system(cmdline) == -1) perror("system");
  } else if (strcmp(cmd, "cd") == 0)  {
    static const char *home = getenv("HOME");
    const char *s = cmdline+2;
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
  } else if (strcmp(cmd, "show") == 0 &&
	     argcmp(cmdline+4, "namespace") == 0) {
    ostringstream sout;
    if (!interp.symtab.current_namespace->empty())
      sout << "namespace "
	   << format_namespace(*interp.symtab.current_namespace) << ";\n";
    if (!interp.symtab.search_namespaces->empty()) {
      sout << "using namespace ";
      int count = 0;
      for (map< string, set<int32_t> >::iterator
	     it = interp.symtab.search_namespaces->begin(),
	     end = interp.symtab.search_namespaces->end();
	   it != end; it++) {
	if (count++ > 0) sout << ", ";
	sout << format_namespace(it->first);
	if (!it->second.empty()) {
	  set<int32_t> right_parens;
	  for (set<int32_t>::iterator jt = it->second.begin();
	       jt != it->second.end(); ++jt) {
	    const symbol& sym = interp.symtab.sym(*jt);
	    if (sym.fix == outfix && sym.g > 0)
	      right_parens.insert(sym.g);
	  }
	  sout << " (";
	  for (set<int32_t>::iterator jt = it->second.begin();
	       jt != it->second.end(); ++jt) {
	    if (right_parens.find(*jt) != right_parens.end())
	      continue;
	    const symbol& sym = interp.symtab.sym(*jt);
	    size_t p = symsplit(sym.s);
	    if (p == string::npos)
	      sout << " " << sym.s;
	    else
	      sout << " " << sym.s.substr(p+2);
	    if (sym.fix == outfix && sym.g > 0) {
	      const symbol& sym2 = interp.symtab.sym(sym.g);
	      size_t p = symsplit(sym2.s);
	      if (p == string::npos)
		sout << " " << sym2.s;
	      else
		sout << " " << sym2.s.substr(p+2);
	    }
	  }
	  sout << " )";
	}
      }
      sout << ";\n";
    }
    if (interp.output)
      (*interp.output) << sout.str();
    else
      cout << sout.str();
  } else if (strcmp(cmd, "show") == 0 &&
	     argcmp(cmdline+4, "namespaces") == 0) {
    ostringstream sout;
    if (!interp.namespaces.empty()) {
      for (set<string>::iterator
	     it = interp.namespaces.begin(),
	     end = interp.namespaces.end();
	   it != end; it++) {
	sout << "namespace " << format_namespace(*it) << ";\n";
      }
    }
    if (interp.output)
      (*interp.output) << sout.str();
    else
      cout << sout.str();
  } else if (strcmp(cmd, "show") == 0)  {
    uint8_t s_verbose = interpreter::g_verbose;
    bool tflag = false; uint32_t tlevel = 0; int pflag = -1;
    bool aflag = false, dflag = false, eflag = false;
    bool cflag = false, fflag = false, mflag = false, vflag = false;
    bool yflag = false, gflag = false, lflag = false, sflag = false;
    const char *s = cmdline+4;
    argl args(s, "show");
    list<string>::iterator arg;
    if (!args.ok) goto out;
    // process option arguments
    for (arg = args.l.begin(); arg != args.l.end(); arg++) {
      const char *s = arg->c_str();
      if (s[0] != '-' || !s[1] || !strchr("acdefghlmpstvy", s[1])) break;
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
	case 'y': yflag = true; break;
	case 't':
	  if (isdigit(s[1])) {
	    tlevel = strtoul(s+1, 0, 10);
	    while (isdigit(s[1])) ++s;
	  } else
	    tlevel = interp.temp;
	  tflag = true;
	  break;
	case 'h':
	  cout <<
"show command help: show [options ...] [symbol ...]\n\
Options may be combined, e.g., show -fg f* is the same as show -f -g f*.\n\
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
-p[flag] List only private symbols if flag is nonzero (the default),\n\
    otherwise list only public symbols. List both private and public\n\
    symbols if -p is omitted.\n\
-s  Summary format, print just summary information about listed symbols.\n\
-t[level] List only symbols and definitions at the given temporary level\n\
    (the current level by default) or above. Level 1 denotes all temporary\n\
    definitions, level 0 *all* definitions. If this option is omitted,\n\
    it defaults to -t0 if any symbols are specified, -t1 otherwise.\n\
-v  Print information about defined variables.\n\
-y  Print information about defined types.\n";
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
    if (!cflag && !fflag && !mflag && !vflag && !yflag)
      cflag = fflag = mflag = vflag = yflag = true;
    if (lflag) sflag = true;
    if (!tflag && args.l.empty()) tlevel = 1;
    {
      size_t maxsize = 0, nfuns = 0, nmacs = 0, ntypes = 0,
	nvars = 0, ncsts = 0, nrules = 0, mrules = 0, trules = 0;
      list<env_sym> l;
      for (int32_t f = 1; f <= interp.symtab.nsyms(); f++) {
	const symbol& sym = interp.symtab.sym(f);
	bool matches = false;
	// skip private/public symbols depending on pflag
	if (pflag >= 0 && (pflag > 0) != sym.priv) continue;
	// look up symbols in the global environment as well as the macro,
	// type and external tables
	env::const_iterator it = interp.globenv.find(f),
	  jt = interp.macenv.find(f),
	  kt = interp.typeenv.find(f);
	extmap::const_iterator xt = interp.externals.find(f);
	// check for symbols in the global environment
	if (it != interp.globenv.end()) {
	  const env_info& e = it->second;
	  if ((e.t == env_info::fun)?fflag:
	      (e.t == env_info::cvar)?cflag:
	      (e.t == env_info::fvar)?vflag:0) {
	    matches = e.temp >= tlevel;
	    if (!matches && !sflag &&
		e.t == env_info::fun && fflag) {
	      // if not in summary mode, also list temporary rules for a
	      // non-temporary symbol
	      rulel::const_iterator r;
	      for (r = e.rules->begin(); r != e.rules->end(); r++)
		if (r->temp >= tlevel) {
		  matches = true;
		  break;
		}
	    }
	    if (!matches) it = interp.globenv.end();
	  }
	}
	if (!matches && fflag && tlevel == 0) {
	  // also list declared externals and operator symbols which don't
	  // have any rules yet
	  matches = xt != interp.externals.end() ||
	    sym.fix == nonfix || (sym.fix == outfix && sym.g) ||
	    sym.prec < PREC_MAX;
	}
	if (mflag && jt != interp.macenv.end()) {
	  // also list symbols defined as macros
	  const env_info& e = jt->second;
	  bool _matches = e.temp >= tlevel;
	  if (!_matches && !sflag) {
	    // if not in summary mode, also list temporary rules for a
	    // non-temporary symbol
	    rulel::const_iterator r;
	    for (r = e.rules->begin(); r != e.rules->end(); r++)
	      if (r->temp >= tlevel) {
		_matches = true;
		break;
	      }
	  }
	  if (!_matches) jt = interp.macenv.end();
	  matches = matches || _matches;
	}
	if (yflag && kt != interp.typeenv.end() &&
	    kt->second.t != env_info::none) {
	  // also list symbols defined as types
	  const env_info& e = kt->second;
	  bool _matches = e.temp >= tlevel;
	  if (!_matches && !sflag) {
	    // if not in summary mode, also list temporary rules for a
	    // non-temporary symbol
	    rulel::const_iterator r;
	    for (r = e.rules->begin(); r != e.rules->end(); r++)
	      if (r->temp >= tlevel) {
		_matches = true;
		break;
	      }
	  }
	  if (!_matches) kt = interp.typeenv.end();
	  matches = matches || _matches;
	}
	if (!matches) continue;
	if (!args.l.empty()) {
	  // see whether we actually want the defined symbol to be listed
	  matches = false;
	  for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
	    if (sym_match(gflag, *arg, sym.s)) {
	      matches = true;
	      break;
	    }
	  }
	}
	if (!matches) continue;
	l.push_back(env_sym(sym, it, jt, kt, xt));
	if (sym.s.size() > maxsize) maxsize = sym.s.size();
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
	map<int32_t,Env>::iterator tenv = interp.globaltypes.find(ftag);
	const env::const_iterator _it = it->it, _jt = it->jt, _kt = it->kt;
	const extmap::const_iterator xt = it->xt;
	if (yflag && _kt != interp.typeenv.end() &&
	    _kt->second.t != env_info::none) {
	  const rulel& rules = *_kt->second.rules;
	  const matcher *m = _kt->second.m;
	  if (sflag) {
	    ++ntypes; trules += rules.size();
	    sout << sym.s << string(maxsize-sym.s.size(), ' ') << "  typ";
	    if (lflag) {
	      sout << "  " << rules << ";";
	      if (aflag && m) sout << '\n' << *m;
	      if (dflag && tenv != interp.globaltypes.end() && tenv->second.f)
		tenv->second.print(sout);
	    } else {
	      sout << " " << rules.size() << " rules";
	    }
	    sout << '\n';
	  } else {
	    size_t n = 0;
	    for (rulel::const_iterator it = rules.begin();
		 it != rules.end(); ++it) {
	      if (it->temp >= tlevel) {
		int32_t i;
		if (it->lhs.is_app() && it->rhs.is_int(i) && i==1) {
		  sout << "type ";
		  printx(sout, it->lhs, true);
		  sout << ";\n";
		} else
		  sout << "type " << *it << ";\n";
		++n;
	      }
	    }
	    if (n > 0) {
	      if (aflag && m) sout << *m << '\n';
	      if (dflag && tenv != interp.globaltypes.end() && tenv->second.f)
		tenv->second.print(sout);
	      trules += n;
	      ++ntypes;
	    }
	  }
	}
	if (_it == interp.globenv.end() && _jt == interp.macenv.end() &&
	    xt != interp.externals.end()) {
	  const ExternInfo& info = xt->second;
	  bool defined = interp.defined.find(sym.f) != interp.defined.end();
	  sout << decl_str(interp, sym, defined);
	  if (sym.priv) sout << "private ";
	  sout << info << ";";
	  if ((!sflag||lflag) && dflag) {
	    if (!sflag) sout << '\n';
	    {
#if HAVE_LLVM_SUPPORT_RAW_OSTREAM_H
	      llvm::raw_os_ostream raw_sout(sout);
#else
	      ostream& raw_sout = sout;
#endif
	      info.f->print(raw_sout);
	    }
	  } else
	    sout << '\n';
	  ++nfuns;
	} else if (_it != interp.globenv.end() &&
		   _it->second.t == env_info::fvar) {
	  sout << decl_str(interp, sym);
	  nvars++;
	  if (sflag) {
	    sout << sym.s << string(maxsize-sym.s.size(), ' ')
		 << "  var";
	    if (lflag) sout << "  " << sym.s << " = "
			    << *(pure_expr**)_it->second.val << ";";
	    sout << '\n';
	  } else
	    sout << "let " << sym.s << " = " << *(pure_expr**)_it->second.val
		 << ";\n";
	} else if (_it != interp.globenv.end() &&
		   _it->second.t == env_info::cvar) {
	  sout << decl_str(interp, sym);
	  ncsts++;
	  if (sflag) {
	    sout << sym.s << string(maxsize-sym.s.size(), ' ')
		 << "  cst";
	    if (lflag) sout << "  " << sym.s << " = "
			    << *_it->second.cval << ";";
	    sout << '\n';
	  } else
	    sout << "const " << sym.s << " = " << *_it->second.cval
		 << ";\n";
	} else {
	  bool defined = interp.defined.find(sym.f) != interp.defined.end();
	  sout << decl_str(interp, sym, defined);
	  if (fflag && xt != interp.externals.end()) {
	    const ExternInfo& info = xt->second;
	    if (sym.priv) sout << "private ";
	    sout << info << ";";
	    if ((!sflag||lflag) && dflag) {
	      if (!sflag) sout << '\n';
	      {
#if HAVE_LLVM_SUPPORT_RAW_OSTREAM_H
		llvm::raw_os_ostream raw_sout(sout);
#else
		ostream& raw_sout = sout;
#endif
		info.f->print(raw_sout);
	      }
	    } else
	      sout << '\n';
	  }
	  if (mflag && _jt != interp.macenv.end()) {
	    uint32_t argc = _jt->second.argc;
	    const rulel& rules = *_jt->second.rules;
	    const matcher *m = _jt->second.m;
	    if (sflag) {
	      ++nmacs; mrules += rules.size();
	      sout << sym.s << string(maxsize-sym.s.size(), ' ') << "  mac";
	      if (lflag) {
		sout << "  " << rules << ";";
		if (aflag && m) sout << '\n' << *m;
	      } else {
		sout << " " << argc << " args, " << rules.size() << " rules";
	      }
	      sout << '\n';
	    } else {
	      size_t n = 0;
	      for (rulel::const_iterator it = rules.begin();
		   it != rules.end(); ++it) {
		if (it->temp >= tlevel) {
		  sout << "def " << *it << ";\n";
		  ++n;
		}
	      }
	      if (n > 0) {
		if (aflag && m) sout << *m << '\n';
		mrules += n;
		++nmacs;
	      }
	    }
	  }
	  if (fflag && _it != interp.globenv.end()) {
	    uint32_t argc = _it->second.argc;
	    const rulel& rules = *_it->second.rules;
	    const matcher *m = _it->second.m;
	    if (sflag) {
	      ++nfuns; nrules += rules.size();
	      sout << sym.s << string(maxsize-sym.s.size(), ' ') << "  fun";
	      if (lflag) {
		sout << "  " << rules << ";";
		if (aflag && m) sout << '\n' << *m;
		if (dflag && fenv != interp.globalfuns.end() && fenv->second.f)
		  fenv->second.print(sout);
	      } else {
		sout << " " << argc << " args, " << rules.size() << " rules";
	      }
	      sout << '\n';
	    } else {
	      size_t n = 0;
	      for (rulel::const_iterator it = rules.begin();
		   it != rules.end(); ++it) {
		if (it->temp >= tlevel) {
		  sout << *it << ";\n";
		  ++n;
		}
	      }
	      if (n > 0) {
		if (aflag && m) sout << *m << '\n';
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
	if (yflag)
	  summary << ntypes << " types (" << trules << " rules), ";
	string s = summary.str();
	if (!s.empty())
	  sout << s.substr(0, s.size()-2) << '\n';
      }
      if (interp.output)
	(*interp.output) << sout.str();
      else {
	FILE *fp;
	const char *more = getenv("PURE_MORE");
	// FIXME: We should check that 'more' actually exists here.
	if (more && *more && isatty(fileno(stdin)) && (fp = popen(more, "w"))) {
	  fputs(sout.str().c_str(), fp);
	  pclose(fp);
	} else
	  cout << sout.str();
      }
    }
  out:
    interpreter::g_verbose = s_verbose;
  } else if (strcmp(cmd, "dump") == 0)  {
    bool tflag = false; uint32_t tlevel = 0; int pflag = -1;
    bool cflag = false, fflag = false, mflag = false, vflag = false;
    bool yflag = false, gflag = false, nflag = false;
    string fname = ".pure";
    const char *s = cmdline+4;
    argl args(s, "dump");
    list<string>::iterator arg;
    if (!args.ok) goto out2;
    // process option arguments
    for (arg = args.l.begin(); arg != args.l.end(); arg++) {
      if (nflag) { fname = *arg; nflag = false; continue; }
      const char *s = arg->c_str();
      if (s[0] != '-' || !s[1] || !strchr("cfghmnptvy", s[1])) break;
      while (*++s) {
	switch (*s) {
	case 'c': cflag = true; break;
	case 'f': fflag = true; break;
	case 'g': gflag = true; break;
	case 'm': mflag = true; break;
	case 'n': nflag = true; break;
	case 'p':
	  if (isdigit(s[1])) {
	    pflag = strtoul(s+1, 0, 10)>0;
	    while (isdigit(s[1])) ++s;
	  } else
	    pflag = 1;
	  break;
	case 'v': vflag = true; break;
	case 'y': yflag = true; break;
	case 't':
	  if (isdigit(s[1])) {
	    tlevel = strtoul(s+1, 0, 10);
	    while (isdigit(s[1])) ++s;
	  } else
	    tlevel = interp.temp;
	  tflag = true;
	  break;
	case 'h':
	  cout <<
"dump command help: dump [-n filename] [options ...] [symbol ...]\n\
Options may be combined, e.g., dump -fg f* is the same as dump -f -g f*.\n\
-c  Dump defined constants.\n\
-f  Dump defined functions.\n\
-g  Indicates that the following symbols are actually shell glob patterns\n\
    and that all matching symbols should be dumped.\n\
-h  Print this list.\n\
-m  Dump defined macros.\n\
-n  Write the dump to the given file (default is .pure).\n\
-p[flag] Dump only private symbols if flag is nonzero (the default),\n\
    otherwise dump only public symbols. Dump both private and public\n\
    symbols if -p is omitted.\n\
-t[level] Dump only symbols and definitions at the given temporary level\n\
    (the current level by default) or above. Level 1 denotes all temporary\n\
    definitions, level 0 *all* definitions. If this option is omitted,\n\
    it defaults to -t0 if any symbols are specified, -t1 otherwise.\n\
-v  Dump defined variables.\n\
-y  Dump defined types.\n";
	  goto out2;
	default:
	  cerr << "dump: invalid option character '" << *s << "'\n";
	  goto out2;
	}
      }
    }
    args.l.erase(args.l.begin(), arg);
    if (!cflag && !fflag && !mflag && !vflag && !yflag)
      cflag = fflag = mflag = vflag = yflag = true;
    if (!tflag && args.l.empty()) tlevel = 1;
    {
      list<env_sym> l;
      for (int32_t f = 1; f <= interp.symtab.nsyms(); f++) {
	const symbol& sym = interp.symtab.sym(f);
	bool matches = false;
	// skip private/public symbols depending on pflag
	if (pflag >= 0 && (pflag > 0) != sym.priv) continue;
	// look up symbols in the global environment as well as the macro,
	// type and external tables
	env::const_iterator it = interp.globenv.find(f),
	  jt = interp.macenv.find(f),
	  kt = interp.typeenv.find(f);
	extmap::const_iterator xt = interp.externals.find(f);
	// check for symbols in the global environment
	if (it != interp.globenv.end()) {
	  const env_info& e = it->second;
	  if ((e.t == env_info::fun)?fflag:
	      (e.t == env_info::cvar)?cflag:
	      (e.t == env_info::fvar)?vflag:0) {
	    matches = e.temp >= tlevel;
	    if (!matches &&
		e.t == env_info::fun && fflag) {
	      // also list temporary rules for a non-temporary symbol
	      rulel::const_iterator r;
	      for (r = e.rules->begin(); r != e.rules->end(); r++)
		if (r->temp >= tlevel) {
		  matches = true;
		  break;
		}
	    }
	    if (!matches) it = interp.globenv.end();
	  }
	}
	if (!matches && fflag && tlevel == 0) {
	  // also list declared externals and operator symbols which don't
	  // have any rules yet
	  matches = xt != interp.externals.end() ||
	    sym.fix == nonfix || (sym.fix == outfix && sym.g) ||
	    sym.prec < PREC_MAX;
	}
	if (mflag && jt != interp.macenv.end()) {
	  // also list symbols defined as macros
	  const env_info& e = jt->second;
	  bool _matches = e.temp >= tlevel;
	  if (!_matches) {
	    // also list temporary rules for a non-temporary symbol
	    rulel::const_iterator r;
	    for (r = e.rules->begin(); r != e.rules->end(); r++)
	      if (r->temp >= tlevel) {
		_matches = true;
		break;
	      }
	  }
	  if (!_matches) jt = interp.macenv.end();
	  matches = matches || _matches;
	}
	if (yflag && kt != interp.typeenv.end() &&
	    kt->second.t != env_info::none) {
	  // also list symbols defined as types
	  const env_info& e = kt->second;
	  bool _matches = e.temp >= tlevel;
	  if (!_matches) {
	    // also list temporary rules for a non-temporary symbol
	    rulel::const_iterator r;
	    for (r = e.rules->begin(); r != e.rules->end(); r++)
	      if (r->temp >= tlevel) {
		_matches = true;
		break;
	      }
	  }
	  if (!_matches) kt = interp.typeenv.end();
	  matches = matches || _matches;
	}
	if (!matches) continue;
	if (!args.l.empty()) {
	  // see whether we actually want the defined symbol to be listed
	  matches = false;
	  for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
	    if (sym_match(gflag, *arg, sym.s)) {
	      matches = true;
	      break;
	    }
	  }
	}
	if (!matches) continue;
	l.push_back(env_sym(sym, it, jt, kt, xt));
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
	const env::const_iterator _it = it->it, _jt = it->jt, _kt = it->kt;
	const extmap::const_iterator _xt = it->xt;
	if (yflag && _kt != interp.typeenv.end() &&
	    _kt->second.t != env_info::none) {
	  const rulel& rules = *_kt->second.rules;
	  for (rulel::const_iterator it = rules.begin();
	       it != rules.end(); ++it) {
	    if (it->temp >= tlevel) {
	      int32_t i;
	      if (it->lhs.is_app() && it->rhs.is_int(i) && i==1) {
		fout << "type ";
		printx(fout, it->lhs, true);
		fout << ";\n";
	      } else
		fout << "type " << *it << ";\n";
	    }
	  }
	}
	if (_it == interp.globenv.end() && _jt == interp.macenv.end() &&
	    _xt != interp.externals.end()) {
	  bool defined = interp.defined.find(sym.f) != interp.defined.end();
	  fout << decl_str(interp, sym, defined);
	  const ExternInfo& info = _xt->second;
	  if (sym.priv) fout << "private ";
	  fout << info << ";\n";
	} else if (_it != interp.globenv.end() &&
		   _it->second.t == env_info::fvar) {
	  fout << decl_str(interp, sym);
	  fout << "let " << sym.s << " = " << *(pure_expr**)_it->second.val
	       << ";\n";
	} else if (_it != interp.globenv.end() &&
		   _it->second.t == env_info::cvar) {
	  fout << decl_str(interp, sym);
	  fout << "const " << sym.s << " = " << *_it->second.cval
	       << ";\n";
	} else {
	  bool defined = interp.defined.find(sym.f) != interp.defined.end();
	  fout << decl_str(interp, sym, defined);
	  if (fflag && _xt != interp.externals.end()) {
	    const ExternInfo& info = _xt->second;
	    if (sym.priv) fout << "private ";
	    fout << info << ";\n";
	  }
	  if (mflag && _jt != interp.macenv.end()) {
	    const rulel& rules = *_jt->second.rules;
	    for (rulel::const_iterator it = rules.begin();
		 it != rules.end(); ++it) {
	      if (it->temp >= tlevel) {
		fout << "def " << *it << ";\n";
	      }
	    }
	  }
	  if (fflag && _it != interp.globenv.end()) {
	    const rulel& rules = *_it->second.rules;
	    for (rulel::const_iterator it = rules.begin();
		 it != rules.end(); ++it) {
	      if (it->temp >= tlevel) {
		fout << *it << ";\n";
	      }
	    }
	  }
	}
      }
    }
  out2:
    ;
  } else if (strcmp(cmd, "clear") == 0)  {
    bool tflag = false; uint32_t tlevel = 0; int pflag = -1;
    bool cflag = false, fflag = false, mflag = false, vflag = false;
    bool yflag = false, gflag = false;
    const char *s = cmdline+5;
    argl args(s, "clear");
    list<string>::iterator arg;
    if (!args.ok) goto out3;
    if (args.c == 1 && args.l.front() == "ans") {
      if (interp.lastres) {
	if (interp.result == interp.lastres) {
	  pure_free(interp.result);
	  interp.result = 0;
	}
	pure_free(interp.lastres);
      }
      interp.lastres = 0;
      goto out3;
    }
    // process option arguments
    for (arg = args.l.begin(); arg != args.l.end(); arg++) {
      const char *s = arg->c_str();
      if (s[0] != '-' || !s[1] || !strchr("cfghmptvy", s[1])) break;
      while (*++s) {
	switch (*s) {
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
	case 'y': yflag = true; break;
	case 't':
	  if (isdigit(s[1])) {
	    tlevel = strtoul(s+1, 0, 10);
	    while (isdigit(s[1])) ++s;
	  } else
	    tlevel = interp.temp;
	  tflag = true;
	  break;
	case 'h':
	  cout <<
"clear command help: clear [options ...] [symbol ...]\n\
Options may be combined, e.g., clear -fg f* is the same as clear -f -g f*.\n\
-c  Clear defined constants.\n\
-f  Clear defined functions.\n\
-g  Indicates that the following symbols are actually shell glob patterns\n\
    and that all matching symbols should be cleared.\n\
-h  Print this list.\n\
-m  Clear defined macros.\n\
-p[flag] Clear only private symbols if flag is nonzero (the default),\n\
    otherwise clear only public symbols. Clear both private and public\n\
    symbols if -p is omitted.\n\
-t[level] Clear only symbols and definitions at the given temporary level\n\
    (the current level by default) or above. Level 1 denotes all temporary\n\
    definitions, level 0 *all* definitions. If this option is omitted,\n\
    it defaults to -t0 if any symbols are specified, -t1 otherwise.\n\
    NOTE: Just 'clear' without any arguments implies -t instead.\n\
-v  Clear defined variables.\n\
-y  Clear defined types.\n";
	  goto out3;
	default:
	  cerr << "clear: invalid option character '" << *s << "'\n";
	  goto out3;
	}
      }
    }
    args.l.erase(args.l.begin(), arg);
    if (!cflag && !fflag && !mflag && !vflag && !yflag)
      cflag = fflag = mflag = vflag = yflag = true;
    if (!tflag && args.l.empty()) tlevel = 1;
    if (args.l.empty() && cflag && fflag && mflag && vflag && pflag == -1) {
      uint32_t old = interp.temp;
      char ans;
      // back out to the previous level by default
      if (!tflag) tlevel = old;
      // we never scrap any permanent definitions here
      if (tlevel == 0) tlevel = 1;
      bool chk = true;
      if (!interp.output) {
	cout << "This will clear all temporary definitions at level #"
	     << tlevel << (tlevel<interp.temp?" and above":"")
	     << ".\nContinue (y/n)? ";
	cin >> noskipws >> ans;
	bool chk = ans == 'y';
	if (cin.good() && chk) {
	  for (size_t i = tlevel; i <= old; i++)
	    interp.clear();
	}
	while (cin.good() && ans != '\n') cin >> noskipws >> ans;
	cin.clear();
      } else {
	for (size_t i = tlevel; i <= old; i++)
	  interp.clear();
      }
      ostream& os = interp.output?*interp.output:cout;
      if (chk && old > 1)
	os << "clear: now at temporary definitions level #"
	   << interp.temp << '\n';
      if (chk && interp.override)
	os << "clear: override mode is on\n";
      goto out3;
    }
    {
      list<env_sym> l;
      for (int32_t f = 1; f <= interp.symtab.nsyms(); f++) {
	const symbol& sym = interp.symtab.sym(f);
	bool matches = false;
	// skip private/public symbols depending on pflag
	if (pflag >= 0 && (pflag > 0) != sym.priv) continue;
	// look up symbols in the global environment as well as the macro,
	// type and external tables
	env::const_iterator it = interp.globenv.find(f),
	  jt = interp.macenv.find(f),
	  kt = interp.typeenv.find(f);
	extmap::const_iterator xt = interp.externals.find(f);
	// check for symbols in the global environment
	if (it != interp.globenv.end()) {
	  const env_info& e = it->second;
	  if ((e.t == env_info::fun)?fflag:
	      (e.t == env_info::cvar)?cflag:
	      (e.t == env_info::fvar)?vflag:0) {
	    matches = e.temp >= tlevel;
	    if (!matches &&
		e.t == env_info::fun && fflag) {
	      // also list temporary rules for a non-temporary symbol
	      rulel::const_iterator r;
	      for (r = e.rules->begin(); r != e.rules->end(); r++)
		if (r->temp >= tlevel) {
		  matches = true;
		  break;
		}
	    }
	  }
	}
	if (!matches && fflag && tlevel == 0 && xt != interp.externals.end()) {
	  // also list declared externals which don't have any rules yet
	  matches = true;
	}
	if (!matches && mflag && jt != interp.macenv.end()) {
	  // also list symbols defined as macros
	  const env_info& e = jt->second;
	  matches = e.temp >= tlevel;
	  if (!matches) {
	    // also list temporary rules for a non-temporary symbol
	    rulel::const_iterator r;
	    for (r = e.rules->begin(); r != e.rules->end(); r++)
	      if (r->temp >= tlevel) {
		matches = true;
		break;
	      }
	  }
	}
	if (!matches && yflag && kt != interp.typeenv.end() &&
	    kt->second.t != env_info::none) {
	  // also list symbols defined as types
	  const env_info& e = kt->second;
	  matches = e.temp >= tlevel;
	  if (!matches) {
	    // also list temporary rules for a non-temporary symbol
	    rulel::const_iterator r;
	    for (r = e.rules->begin(); r != e.rules->end(); r++)
	      if (r->temp >= tlevel) {
		matches = true;
		break;
	      }
	  }
	}
	if (!matches) continue;
	if (!args.l.empty()) {
	  // see whether we actually want the defined symbol to be listed
	  matches = false;
	  for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
	    if (sym_match(gflag, *arg, sym.s)) {
	      matches = true;
	      break;
	    }
	  }
	}
	if (!matches) continue;
	l.push_back(env_sym(sym, it, jt, kt, xt));
      }
      if (l.empty()) goto out3;
      for (list<env_sym>::const_iterator it = l.begin();
	   it != l.end(); ++it) {
	const symbol& sym = *it->sym;
	int32_t ftag = sym.f;
	map<int32_t,Env>::iterator fenv = interp.globalfuns.find(ftag);
	const env::const_iterator _it = it->it, _jt = it->jt, _kt = it->kt;
	if (_it != interp.globenv.end() &&
	    (_it->second.t == env_info::fvar||
	     _it->second.t == env_info::cvar)) {
	  interp.clear(sym.f);
	} else {
	  if (mflag && _jt != interp.macenv.end()) {
	    const env_info& e = _jt->second;
	    if (e.temp >= tlevel)
	      interp.clear_mac(sym.f);
	    else
	      interp.clear_mac_rules(sym.f, tlevel);
	  }
	  if (fflag && _it != interp.globenv.end()) {
	    const env_info& e = _it->second;
	    if (e.temp >= tlevel)
	      interp.clear(sym.f);
	    else
	      interp.clear_rules(sym.f, tlevel);
	  }
	}
	if (yflag && _kt != interp.typeenv.end()) {
	  const env_info& e = _kt->second;
	  if (e.temp >= tlevel)
	    interp.clear_type(sym.f);
	  else
	    interp.clear_type_rules(sym.f, tlevel);
	}
      }
    }
  out3:
    ;
  } else if (strcmp(cmd, "save") == 0)  {
    const char *s = cmdline+4;
    argl args(s, "save");
    if (!args.ok)
      ;
    else if (args.c > 0)
      cerr << "save: extra parameter\n";
    else if (interp.temp == 0xffffffffU)
      cerr << "save: already at maximum level\n";
    else {
      ostream& os = interp.output?*interp.output:cout;
      os << "save: now at temporary definitions level #"
	 << ++interp.temp << '\n';
      if (interp.override) os << "save: override mode is on\n";
    }
  } else if (strcmp(cmd, "run") == 0)  {
    const char *s = cmdline+3;
    argl args(s, "run");
    bool debug = false, quiet = true;
    if (!args.ok)
      ;
    else if (args.c == 0 || (debug = args.c == 1 && args.l.front() == "-g")) {
      // Rerun the interpreter.
      int argc = interp.argc;
      char **argv = interp.argv;
      if (argc > 0 && argv) {
	size_t j = 0;
	// Create a temporary copy since we may have to edit the command line.
	argv = (char**)malloc((argc+3)*sizeof(char*));
	if (!argv) goto run_err;
	argv[j++] = interp.argv[0];
	for (size_t i = 1; interp.argv[i]; i++)
	  if (strcmp(interp.argv[i], "-g") == 0) {
	    if (debug) {
	      argv[j++] = interp.argv[i];
	      debug = false;
	    }
	  } else if (strcmp(interp.argv[i], "-q") == 0) {
	    argv[j++] = interp.argv[i];
	    quiet = false;
	  } else if (strncmp(interp.argv[i], "-T", 2) == 0 ||
		     strncmp(interp.argv[i], "-o", 2) == 0 ||
		     strncmp(interp.argv[i], "-l", 2) == 0 ||
		     strncmp(interp.argv[i], "-I", 2) == 0 ||
		     strncmp(interp.argv[i], "-L", 2) == 0) {
	    if (interp.argv[i][2] == 0 && interp.argv[i+1])
	      argv[j++] = interp.argv[i++];
	    argv[j++] = interp.argv[i];
	  } else {
	    argv[j++] = interp.argv[i];
	  }
	if (debug) {
	  // Add -g to the command line options, to enable debugging.
	  static char opt[] = "-g";
	  argv[j++] = opt;
	}
	if (quiet) {
	  // Add -q to the command line options, to suppress the sign-on
	  // message.
	  static char opt[] = "-q";
	  argv[j++] = opt;
	}
	argv[j++] = 0;
	if (exit_handler) exit_handler();
	execvp(argv[0], argv);
      run_err:
	if (argv != interp.argv && argv) free(argv);
      }
      cerr << "run: exec failed\n";
    } else if (args.c == 1 && args.l.begin()->empty())
      cerr << "run: no script name specified\n";
    else if (args.c > 1)
      cerr << "run: extra parameter\n";
    else {
      try { interp.run(*args.l.begin(), false, true); } catch (err &e) {
	interp.error(*yylloc, e.what());
	interp.nerrs = 0;
      }
    }
  } else if (strcmp(cmd, "override") == 0)  {
    const char *s = cmdline+8;
    argl args(s, "override");
    if (!args.ok)
      ;
    else if (args.c > 0)
      cerr << "override: extra parameter\n";
    else
      interp.override = true;
  } else if (strcmp(cmd, "underride") == 0)  {
    const char *s = cmdline+9;
    argl args(s, "underride");
    if (!args.ok)
      ;
    else if (args.c > 0)
      cerr << "underride: extra parameter\n";
    else
      interp.override = false;
  } else if (strcmp(cmd, "mem") == 0)  {
    const char *s = cmdline+3;
    argl args(s, "mem");
    if (!args.ok)
      ;
    else if (args.c > 0)
      cerr << "mem: extra parameter\n";
    else {
      ostream& os = interp.output?*interp.output:cout;
      size_t used, free;
      interp.mem_usage(used, free);
      os << used << " cells (" << free << " free)\n";
    }
  } else if (strcmp(cmd, "stats") == 0) {
    const char *s = cmdline+5;
    argl args(s, "stats");
    bool mflag = false, on = true;
    list<string>::iterator arg;
    if (!args.ok) goto stats_out;
    // process option arguments
    for (arg = args.l.begin(); arg != args.l.end(); arg++) {
      const char *s = arg->c_str();
      if (s[0] != '-' || !s[1]) break;
      while (*++s) {
	switch (*s) {
	case 'm': mflag = true; break;
	case 'h':
	  cout <<
"stats command help: stats [-m] [on|off]\n\
Switches evaluation statistics on (default) or off. The -m option specifies\n\
that memory usage is to be printed along with the cpu time.\n";
	  goto stats_out;
	default:
	  cerr << "show: invalid option character '" << *s << "'\n";
	  goto stats_out;
	}
      }
    }
    if (arg != args.l.end()) {
      if (*arg == "on")
	on = true;
      else if (*arg == "off")
	on = false;
      else {
	cerr << "stats: invalid parameter '" << *arg
	     << "' (must be 'on' or 'off')\n";
	goto stats_out;
      }
      ++arg;
    }
    if (arg != args.l.end()) {
      cerr << "stats: extra parameter\n";
      goto stats_out;
    }
    if (on) {
      interp.stats = true;
      if (mflag) interp.stats_mem = true;
    } else {
      if (mflag)
	interp.stats_mem = false;
      else
	interp.stats = interp.stats_mem = false;
    }
  stats_out: ;
  } else if (strcmp(cmd, "quit") == 0)  {
    const char *s = cmdline+4;
    argl args(s, "quit");
    if (!args.ok)
      ;
    else if (args.c > 0)
      cerr << "quit: extra parameter\n";
    else
      exit(0);
  } else if (strcmp(cmd, "completion_matches") == 0) {
    const char *s = cmdline+18;
    while (isspace(*s)) ++s;
    list_completions(interp.output?*interp.output:cout, s);
  } else if (strcmp(cmd, "help_matches") == 0) {
    const char *s = cmdline+12;
    while (isspace(*s)) ++s;
    size_t l = strlen(s);
    std::ostream& out = interp.output?*interp.output:cout;
    if (!idx) idx = new Index(interp.libdir+"docs/genindex.html");
    for (map<string,string>::const_iterator it = idx->m.begin(),
	   end = idx->m.end(); it != end; ++it) {
      const string& key = it->first;
      if (strncmp(s, key.c_str(), l) == 0)
	out << key << endl;
    }
  } else if (strcmp(cmd, "help_index") == 0) {
    const char *s = cmdline+10;
    while (isspace(*s)) ++s;
    string target;
    if (!idx) idx = new Index(interp.libdir+"docs/genindex.html");
    if (idx->lookup(s, target))
      (interp.output?*interp.output:cout)
	<< interp.libdir+"docs/"+target << endl;
  }
}
