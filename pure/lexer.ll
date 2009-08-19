%{                                            /* -*- C++ -*- */
#include <cstdlib>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <fnmatch.h>
#include <time.h>
#include <readline/readline.h>
#include <readline/history.h>
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

static void my_readline(const char *prompt, char *buf, int &result, int max_size);
static void docmd(interpreter &interp, yy::parser::location_type* yylloc, const char *cmd, const char *cmdline);
static string pstring(const char *s);
static string format_namespace(const string& name);
static bool find_namespace(interpreter& interp, const string& name);
static int32_t checktag(const char *s);

/* Uncomment this to enable checking for interactive command names. This is
   rather annoying and hence disabled by default. */
//#define CHECK_NAMES 1

#if CHECK_NAMES
static void check(const yy::location& l, const char* s, bool decl);
#else
#define check(l, s, decl) 
#endif

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

static yy::parser::token_type optoken[5] =
  {token::NA, token::LT, token::RT, token::PR, token::PO};

#define optok(_f, _fix) ((_fix<=infixr && _f==interp.symtab.minus_sym().f)?token::PR:optoken[_fix])
%}

%option noyywrap nounput debug

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
int    [0-9]+|0[0-7]+|0[xX][0-9a-fA-F]+
exp    ([Ee][+-]?[0-9]+)
float  [0-9]+{exp}|[0-9]+\.{exp}|[0-9]*\.[0-9]+{exp}?
str    ([^\"\\\n]|\\(.|\n))*
cmd    (!|help|ls|pwd|break|del|cd|show|dump|clear|save|run|override|underride|stats|quit|completion_matches)
blank  [ \t\f\v\r]

%x comment xdecl xdecl_comment xusing xusing_comment xtag rescan

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

"/*"       { parse_comment: BEGIN(comment); }

<comment>[^*\n]*        yylloc->step();
<comment>"*"+[^*/\n]*   yylloc->step();
<comment>[\n]+          yylloc->lines(yyleng); yylloc->step();
<comment>"*"+"/"        yylloc->step(); BEGIN(INITIAL);
<comment><<EOF>>	interp.error(*yylloc, "open comment at end of file"); BEGIN(INITIAL);

<xdecl>extern     return token::EXTERN;
<xdecl>infix      yylval->fix = infix; return token::FIX;
<xdecl>infixl     yylval->fix = infixl; return token::FIX;
<xdecl>infixr     yylval->fix = infixr; return token::FIX;
<xdecl>prefix     yylval->fix = prefix; return token::FIX;
<xdecl>postfix    yylval->fix = postfix; return token::FIX;
<xdecl>outfix     return token::OUTFIX;
<xdecl>nonfix     return token::NONFIX;
<xdecl>nullary    interp.warning(*yylloc, "warning: 'nullary' keyword is deprecated"); return token::NONFIX;
<xdecl>private    return token::PRIVATE;
<xdecl>public     return token::PUBLIC;
<xdecl>const      return token::CONST;
<xdecl>def        return token::DEF;
<xdecl>let        return token::LET;
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
<xdecl>{id}	  check(*yylloc, yytext, true); yylval->sval = new string(yytext); return token::ID;
<xdecl>[()*,=]	  return yy::parser::token_type(yytext[0]);
<xdecl>"//".*	  yylloc->step();
<xdecl>"/*"	  BEGIN(xdecl_comment);
<xdecl>;	  BEGIN(INITIAL); return yy::parser::token_type(yytext[0]);
<xdecl>{blank}+	  yylloc->step();
<xdecl>[\n]+	  yylloc->lines(yyleng); yylloc->step();
<xdecl>(.|{punct}|{letter})	{
  string msg = "invalid character '"+string(yytext)+"'";
  interp.error(*yylloc, msg);
}
     
<xdecl_comment>[^*\n]*        yylloc->step();
<xdecl_comment>"*"+[^*/\n]*   yylloc->step();
<xdecl_comment>[\n]+          yylloc->lines(yyleng); yylloc->step();
<xdecl_comment>"*"+"/"        yylloc->step(); BEGIN(xdecl);
<xdecl_comment><<EOF>>        interp.error(*yylloc, "open comment at end of file"); BEGIN(xdecl);

<xusing>extern     return token::EXTERN;
<xusing>infix      yylval->fix = infix; return token::FIX;
<xusing>infixl     yylval->fix = infixl; return token::FIX;
<xusing>infixr     yylval->fix = infixr; return token::FIX;
<xusing>prefix     yylval->fix = prefix; return token::FIX;
<xusing>postfix    yylval->fix = postfix; return token::FIX;
<xusing>outfix     return token::OUTFIX;
<xusing>nonfix     return token::NONFIX;
<xusing>nullary    interp.warning(*yylloc, "warning: 'nullary' keyword is deprecated"); return token::NONFIX;
<xusing>private    return token::PRIVATE;
<xusing>public     return token::PUBLIC;
<xusing>const      return token::CONST;
<xusing>def        return token::DEF;
<xusing>let        return token::LET;
<xusing>case	   return token::CASE;
<xusing>of	   return token::OF;
<xusing>end	   return token::END;
<xusing>if	   return token::IF;
<xusing>then	   return token::THEN;
<xusing>else	   return token::ELSE;
<xusing>otherwise  return token::OTHERWISE;
<xusing>when	   return token::WHEN;
<xusing>with	   return token::WITH;
<xusing>using      return token::USING;
<xusing>namespace  return token::NAMESPACE;
<xusing>{qual}?{id}  yylval->sval = new string(yytext); return token::ID;
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
  string msg = "invalid character '"+string(yytext)+"'";
  interp.error(*yylloc, msg);
}
     
<xusing_comment>[^*\n]*        yylloc->step();
<xusing_comment>"*"+[^*/\n]*   yylloc->step();
<xusing_comment>[\n]+          yylloc->lines(yyleng); yylloc->step();
<xusing_comment>"*"+"/"        yylloc->step(); BEGIN(xusing);
<xusing_comment><<EOF>>        interp.error(*yylloc, "open comment at end of file"); BEGIN(xusing);

^{cmd} {
  /* These are treated as commands in interactive mode, and as ordinary
     (operator or identifier) symbols otherwise. */
  if (interp.interactive || interp.output) {
    /* Read the rest of the command line. */
    string cmd = yytext, cmdline = yytext;
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
    docmd(interp, yylloc, cmd.c_str(), cmdline.c_str());
  } else if (yytext[0] == '!')
    goto parse_op;
  else {
    check(*yylloc, yytext, false);
    goto parse_id;
  }
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
<rescan>\"{str}\" |
\"{str}\"   {
  char *msg;
  yytext[yyleng-1] = 0;
  int count = 0;
  for (int i = 1; i < yyleng-1; i++)
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
  for (int i = 1; i < yyleng; i++)
    if (yytext[i] == '\n') count++;
  yylloc->lines(count);
  interp.error(*yylloc, "unterminated string constant");
  yylval->csval = parsestr(yytext+1, msg);
  BEGIN(INITIAL);
  return token::STR;
}
extern     BEGIN(xdecl); return token::EXTERN;
infix      yylval->fix = infix; return token::FIX;
infixl     yylval->fix = infixl; return token::FIX;
infixr     yylval->fix = infixr; return token::FIX;
prefix     yylval->fix = prefix; return token::FIX;
postfix    yylval->fix = postfix; return token::FIX;
outfix     return token::OUTFIX;
nonfix     return token::NONFIX;
nullary    interp.warning(*yylloc, "warning: 'nullary' keyword is deprecated"); return token::NONFIX;
private    return token::PRIVATE;
public     return token::PUBLIC;
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
namespace  BEGIN(xusing); return token::NAMESPACE;
<xtag>::{id} BEGIN(INITIAL); goto parse_tag;
{qual}{id} {
  string qualid = yytext;
  size_t k = qualid.rfind("::");
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
	(k = qualid.rfind("::")) != string::npos) {
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
  size_t k = qualid.rfind("::");
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
  while (yyleng > (int)k+1 && yytext[yyleng-1] == ';') yyless(yyleng-1);
  if (interp.declare_op) {
    yylval->sval = new string(yytext);
    return token::ID;
  }
  symbol* sym = interp.symtab.lookup(yytext);
  while (!sym && yyleng > (int)k+1) {
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
    interpreter::g_interp->debug_init();
    if (using_readline) {
      // read a new line using readline()
      char *s = readline(prompt);
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
  size_t k = name.rfind("::");
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
  for (set<string>::iterator it = interp.symtab.search_namespaces->begin(),
	 end = interp.symtab.search_namespaces->end(); it != end; it++) {
    const string& ns = *it;
    if (!ns.empty() &&
	interp.namespaces.find(ns+"::"+qual) != interp.namespaces.end())
      return true;
  }
  return false;
}

/* This is a watered-down version of the command completion routine from
   pure.cc, used to implement the completion_matches command used by
   pure-mode.el. This isn't perfect, since it will also complete command names
   when not at the beginning of the line, but since the location inside the
   line isn't passed to completion_matches, it's the best that we can do right
   now. */

static const char *commands[] = {
  "break", "cd", "clear", "const", "def", "del", "dump", "extern", "help",
  "infix", "infixl", "infixr", "let", "ls", "namespace", "nonfix", "outfix",
  "override", "postfix", "prefix", "private", "public", "pwd", "quit", "run",
  "save", "show", "stats", "underride", "using", 0
};

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

static char **
pure_completion(const char *text, int start, int end)
{
  return rl_completion_matches(text, command_generator);
}

static void list_completions(ostream& os, const char *s)
{
  char **matches = pure_completion(s, 0, strlen(s));
  if (matches) {
    if (matches[0]) {
      if (!matches[1]) {
	os << matches[0] << endl;
	free(matches[0]);
      } else {
	int i;
	free(matches[0]);
	for (i = 1; matches[i]; i++) {
	  os << matches[i] << endl;
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

/* Interactive command processing. */

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

static void docmd(interpreter &interp, yy::parser::location_type* yylloc, const char *cmd, const char *cmdline)
{
  if (interp.restricted) {
    cerr << cmd << ": operation not implemented\n";
  } else if (strcmp(cmd, "!") == 0) {
    const char *s = cmdline+1;
    while (isspace(*s)) ++s;
    system(s);
  } else if (strcmp(cmd, "break") == 0)  {
    const char *s = cmdline+5;
    argl args(s, "break");
    if (!args.ok)
      ;
    else if (!interp.debugging)
      cerr << "break: debugging not enabled (add -g when invoking the interpreter)\n";
    else if (args.c == 0) {
      ostringstream sout;
      list<string> syms;
      for (set<int32_t>::iterator it = interp.breakpoints.begin();
	   it != interp.breakpoints.end(); ++it)
	syms.push_back(interp.symtab.sym(*it).s);
      syms.sort();
      for (list<string>::iterator it = syms.begin(); it != syms.end(); ++it)
	sout << *it << endl;
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
  } else if (strcmp(cmd, "del") == 0)  {
    const char *s = cmdline+3;
    argl args(s, "del");
    if (!args.ok)
      ;
    else if (args.c == 0) {
      if (!interp.breakpoints.empty()) {
	if (yes_or_no("This will clear all breakpoints. Continue (y/n)?"))
	  interp.breakpoints.clear();
      } else
	cerr << "del: no breakpoints\n";
    } else {
      for (list<string>::iterator it = args.l.begin();
	   it != args.l.end(); ++it) {
	const char *s = it->c_str();
	int32_t f = pure_getsym(s);
	if (f > 0) {
	  env::const_iterator jt = interp.globenv.find(f);
	  if (interp.breakpoints.find(f) != interp.breakpoints.end())
	    interp.breakpoints.erase(f);
	  else
	    cerr << "del: unknown breakpoint '" << s << "'\n";
	} else
	  cerr << "del: unknown function symbol '" << s << "'\n";
      }
    }
  } else if (strcmp(cmd, "help") == 0)  {
    const char *s = cmdline+4, *p, *q;
    while (isspace(*s)) ++s;
    string docname = s;
    if (!*s)
      // default is to load the Pure manual
      docname = "file:"+interp.libdir+"pure.html";
    else if ((p = strchr(s, ':')) && p>s && (!(q = strchr(s, '#')) || q>p))
      // proper URL, take as is
      ;
    else if ((p = strchr(s, '#'))) {
      // alternative library documentation, add path if necessary
      if (p == s)
	// no filename, use the default library documentation
	docname.insert(0, interp.libdir+"purelib.html");
      else if (!(q = strchr(s, '/')) || q>=p) {
	if (!(q = strchr(s, '.')) || q>=p)
	  // no filename extension, assume .html
	  docname.insert(p-s, ".html");
	docname.insert(0, interp.libdir);
      }
      docname.insert(0, "file:");
    } else if (strcmp(s, "copying") == 0)
      // look up the license information
      docname.insert(0, "file:"+interp.libdir+"pure.html#");
    else
      // look up the default library documentation
      docname.insert(0, "file:"+interp.libdir+"purelib.html#");
    // remove a lone trailing '#'
    assert(!docname.empty());
    if (docname[docname.length()-1] == '#')
      docname.erase(docname.length()-1);
    // invoke the browser
    const char *browser = getenv("PURE_HELP");
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
      string helpcmd = string(browser) + " " + docname;
      system(helpcmd.c_str());
    }
  } else if (strcmp(cmd, "ls") == 0)  {
    system(cmdline);
  } else if (strcmp(cmd, "pwd") == 0)  {
    system(cmdline);
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
      for (set<string>::iterator
	     it = interp.symtab.search_namespaces->begin(),
	     end = interp.symtab.search_namespaces->end();
	   it != end; it++) {
	if (count++ > 0) sout << ", ";
	sout << format_namespace(*it);
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
    bool gflag = false, lflag = false, sflag = false;
    const char *s = cmdline+4;
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
    if (!tflag && args.l.empty()) tlevel = 1;
    {
      size_t maxsize = 0, nfuns = 0, nmacs = 0, nvars = 0, ncsts = 0,
	nrules = 0, mrules = 0;
      list<env_sym> l; set<int32_t> syms;
      for (env::const_iterator it = interp.globenv.begin();
	   it != interp.globenv.end(); ++it) {
	int32_t f = it->first;
	const env_info& e = it->second;
	const symbol& sym = interp.symtab.sym(f);
	if ((pflag >= 0 && (pflag > 0) != sym.priv) ||
	    !((e.t == env_info::fun)?fflag:
	      (e.t == env_info::cvar)?cflag:
	      (e.t == env_info::fvar)?vflag:0))
	  continue;
	bool matches = e.temp >= tlevel;
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
	syms.insert(f);
	l.push_back(env_sym(sym, it, interp.macenv.find(f),
			    interp.externals.find(f)));
	if (sym.s.size() > maxsize) maxsize = sym.s.size();
      }
      if (fflag && tlevel == 0) {
	// also process the declared externals which don't have any rules yet
	for (extmap::const_iterator it = interp.externals.begin();
	     it != interp.externals.end(); ++it) {
	  int32_t f = it->first;
	  if (syms.find(f) == syms.end()) {
	    const symbol& sym = interp.symtab.sym(f);
	    if (pflag >= 0 && (pflag > 0) != sym.priv)
	      continue;
	    bool matches = true;
	    if (!args.l.empty()) {
	      matches = false;
	      for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
		if (sym_match(gflag, *arg, sym.s)) {
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
	    if (pflag >= 0 && (pflag > 0) != sym.priv)
	      continue;
	    bool matches = e.temp >= tlevel;
	    if (!matches && !sflag) {
	      // if not in summary mode, also list temporary rules for a
	      // non-temporary symbol
	      rulel::const_iterator r;
	      for (r = e.rules->begin(); r != e.rules->end(); r++)
		if (r->temp >= tlevel) {
		  matches = true;
		  break;
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
	  if (sym.fix == nonfix)
	    sout << "nonfix " << sym.s << ";\n";
	  else if (sym.fix == outfix && sym.g)
	    sout << "outfix " << sym.s << " "
		 << interp.symtab.sym(sym.g).s << ";\n";
	  sout << info << ";";
	  if ((!sflag||lflag) && dflag) {
	    if (!sflag) sout << endl;
	    info.f->print(sout);
	  } else
	    sout << endl;
	  ++nfuns;
	} else if (jt != interp.globenv.end() &&
		   jt->second.t == env_info::fvar) {
	  if (sym.fix == nonfix)
	    sout << "nonfix " << sym.s << ";\n";
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
	  if (sym.fix == nonfix)
	    sout << "nonfix " << sym.s << ";\n";
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
		if (it->temp >= tlevel) {
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
		if (it->temp >= tlevel) {
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
    bool gflag = false, nflag = false;
    string fname = ".pure";
    const char *s = cmdline+4;
    argl args(s, "dump");
    list<string>::iterator arg;
    if (!args.ok) goto out2;
    // process option arguments
    for (arg = args.l.begin(); arg != args.l.end(); arg++) {
      if (nflag) { fname = *arg; nflag = false; continue; }
      const char *s = arg->c_str();
      if (s[0] != '-' || !s[1] || !strchr("cfghmnptv", s[1])) break;
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
-v  Dump defined variables.\n";
	  goto out2;
	default:
	  cerr << "dump: invalid option character '" << *s << "'\n";
	  goto out2;
	}
      }
    }
    args.l.erase(args.l.begin(), arg);
    if (!cflag && !fflag && !mflag && !vflag)
      cflag = fflag = mflag = vflag = true;
    if (!tflag && args.l.empty()) tlevel = 1;
    {
      list<env_sym> l; set<int32_t> syms;
      for (env::const_iterator it = interp.globenv.begin();
	   it != interp.globenv.end(); ++it) {
	int32_t f = it->first;
	const env_info& e = it->second;
	const symbol& sym = interp.symtab.sym(f);
	if ((pflag >= 0 && (pflag > 0) != sym.priv) ||
	    !((e.t == env_info::fun)?fflag:
	      (e.t == env_info::cvar)?cflag:
	      (e.t == env_info::fvar)?vflag:0))
	  continue;
	bool matches = e.temp >= tlevel;
	if (!matches &&
	    e.t == env_info::fun && fflag) {
	  // dump temporary rules for a non-temporary symbol
	  rulel::const_iterator r;
	  for (r = e.rules->begin(); r != e.rules->end(); r++)
	    if (r->temp >= tlevel) {
	      matches = true;
	      break;
	    }
	}
	if (!matches) continue;
	if (!args.l.empty()) {
	  // see whether we actually want the defined symbol to be dumped
	  matches = false;
	  for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
	    if (sym_match(gflag, *arg, sym.s)) {
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
      if (fflag && tlevel == 0) {
	// also process the declared externals which don't have any rules yet
	for (extmap::const_iterator it = interp.externals.begin();
	     it != interp.externals.end(); ++it) {
	  int32_t f = it->first;
	  if (syms.find(f) == syms.end()) {
	    const symbol& sym = interp.symtab.sym(f);
	    if (pflag >= 0 && (pflag > 0) != sym.priv)
	      continue;
	    bool matches = true;
	    if (!args.l.empty()) {
	      matches = false;
	      for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
		if (sym_match(gflag, *arg, sym.s)) {
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
	    if (pflag >= 0 && (pflag > 0) != sym.priv)
	      continue;
	    bool matches = e.temp >= tlevel;
	    if (!matches) {
	      // also dump temporary rules for a non-temporary symbol
	      rulel::const_iterator r;
	      for (r = e.rules->begin(); r != e.rules->end(); r++)
		if (r->temp >= tlevel) {
		  matches = true;
		  break;
		}
	    }
	    if (!matches) continue;
	    if (!args.l.empty()) {
	      // see whether we actually want the defined symbol to be dumped
	      matches = false;
	      for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
		if (sym_match(gflag, *arg, sym.s)) {
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
	  if (sym.fix == nonfix)
	    fout << "nonfix " << sym.s << ";\n";
	  else if (sym.fix == outfix && sym.g)
	    fout << "outfix " << sym.s << " "
		 << interp.symtab.sym(sym.g).s << ";\n";
	  assert(xt != interp.externals.end());
	  const ExternInfo& info = xt->second;
	  fout << info << ";\n";
	} else if (jt != interp.globenv.end() &&
		   jt->second.t == env_info::fvar) {
	  if (sym.fix == nonfix)
	    fout << "nonfix " << sym.s << ";\n";
	  fout << "let " << sym.s << " = " << *(pure_expr**)jt->second.val
	       << ";\n";
	} else if (jt != interp.globenv.end() &&
		   jt->second.t == env_info::cvar) {
	  if (sym.fix == nonfix)
	    fout << "nonfix " << sym.s << ";\n";
	  fout << "const " << sym.s << " = " << *jt->second.cval
	       << ";\n";
	} else {
	  if (sym.fix == nonfix)
	    fout << "nonfix " << sym.s << ";\n";
	  else if (sym.fix == outfix && sym.g)
	    fout << "outfix " << sym.s << " "
		 << interp.symtab.sym(sym.g).s << ";\n";
	  else if (sym.prec < PREC_MAX) {
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
	    case nonfix:
	    case outfix:
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
	      if (it->temp >= tlevel) {
		fout << "def " << *it << ";\n";
	      }
	    }
	  }
	  if (fflag && jt != interp.globenv.end()) {
	    const rulel& rules = *jt->second.rules;
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
    bool gflag = false;
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
      if (s[0] != '-' || !s[1] || !strchr("cfghmptv", s[1])) break;
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
-v  Clear defined variables.\n";
	  goto out3;
	default:
	  cerr << "clear: invalid option character '" << *s << "'\n";
	  goto out3;
	}
      }
    }
    args.l.erase(args.l.begin(), arg);
    if (!cflag && !fflag && !mflag && !vflag)
      cflag = fflag = mflag = vflag = true;
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
	   << interp.temp << endl;
      if (chk && interp.override)
	os << "clear: override mode is on\n";
      goto out3;
    }
    {
      list<env_sym> l; set<int32_t> syms;
      for (env::const_iterator it = interp.globenv.begin();
	   it != interp.globenv.end(); ++it) {
	int32_t f = it->first;
	const env_info& e = it->second;
	const symbol& sym = interp.symtab.sym(f);
	if ((pflag >= 0 && (pflag > 0) != sym.priv) ||
	    !((e.t == env_info::fun)?fflag:
	      (e.t == env_info::cvar)?cflag:
	      (e.t == env_info::fvar)?vflag:0))
	  continue;
	bool matches = e.temp >= tlevel;
	if (!matches &&
	    e.t == env_info::fun && fflag) {
	  // clear temporary rules for a non-temporary symbol
	  rulel::const_iterator r;
	  for (r = e.rules->begin(); r != e.rules->end(); r++)
	    if (r->temp >= tlevel) {
	      matches = true;
	      break;
	    }
	}
	if (!matches) continue;
	if (!args.l.empty()) {
	  // see whether we actually want the defined symbol to be cleared
	  matches = false;
	  for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
	    if (sym_match(gflag, *arg, sym.s)) {
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
      if (mflag) {
	// also clear any symbols defined as macros, unless they've already been
	// considered
	for (env::const_iterator it = interp.macenv.begin();
	     it != interp.macenv.end(); ++it) {
	  int32_t f = it->first;
	  if (syms.find(f) == syms.end()) {
	    const env_info& e = it->second;
	    const symbol& sym = interp.symtab.sym(f);
	    if (pflag >= 0 && (pflag > 0) != sym.priv)
	      continue;
	    bool matches = e.temp >= tlevel;
	    if (!matches) {
	      // also clear temporary rules for a non-temporary symbol
	      rulel::const_iterator r;
	      for (r = e.rules->begin(); r != e.rules->end(); r++)
		if (r->temp >= tlevel) {
		  matches = true;
		  break;
		}
	    }
	    if (!matches) continue;
	    if (!args.l.empty()) {
	      // see whether we actually want the defined symbol to be cleared
	      matches = false;
	      for (arg = args.l.begin(); arg != args.l.end(); ++arg) {
		if (sym_match(gflag, *arg, sym.s)) {
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
      if (l.empty()) goto out3;
      for (list<env_sym>::const_iterator it = l.begin();
	   it != l.end(); ++it) {
	const symbol& sym = *it->sym;
	int32_t ftag = sym.f;
	map<int32_t,Env>::iterator fenv = interp.globalfuns.find(ftag);
	const env::const_iterator jt = it->it, kt = it->jt;
	if (jt != interp.globenv.end() &&
	    (jt->second.t == env_info::fvar||jt->second.t == env_info::cvar)) {
	  interp.clear(sym.f);
	} else {
	  if (mflag && kt != interp.macenv.end()) {
	    const env_info& e = kt->second;
	    if (e.temp >= tlevel)
	      interp.clear_mac(sym.f);
	    else
	      interp.clear_mac_rules(sym.f, tlevel);
	  }
	  if (fflag && jt != interp.globenv.end()) {
	    const env_info& e = jt->second;
	    if (e.temp >= tlevel)
	      interp.clear(sym.f);
	    else
	      interp.clear_rules(sym.f, tlevel);
	  }
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
	 << ++interp.temp << endl;
      if (interp.override) os << "save: override mode is on\n";
    }
  } else if (strcmp(cmd, "run") == 0)  {
    const char *s = cmdline+3;
    argl args(s, "run");
    if (!args.ok)
      ;
    else if (args.c == 0 || (args.c == 1 && args.l.begin()->empty()))
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
  } else if (strcmp(cmd, "stats") == 0)  {
    const char *s = cmdline+5;
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
  }
}
