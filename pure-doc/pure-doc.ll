%{                                            /* -*- C++ -*- */
#include <cstdlib>
#include <errno.h>
#include <stdio.h>
#include <string>
#include <iostream>

/* Work around an incompatibility in flex (at least versions 2.5.31 through
   2.5.33): it generates code that does not conform to C89.  See Debian bug
   333231 <http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=333231>.  */
# undef yywrap
# define yywrap() 1

using namespace std;

static char *prog, **files;
static unsigned col = 0, start;
static const unsigned tabsize = 8;
static string buf;
static unsigned tabs(unsigned& col, const char *text, unsigned len);
static void print(unsigned col, string& text);
%}

%option noyywrap nounput

str    ([^\"\\\n]|\\(.|\n))*
space  [ \t\f\v\r\n]

%x comment lcomment

%%

{space}+		|
^"#!".*			tabs(col, yytext, yyleng);

"//".*			{
  start = 0; buf = yytext+2; tabs(col, yytext, yyleng);
  BEGIN(lcomment);
}
"/*"			col += yyleng; start = col; buf = ""; BEGIN(comment);

<lcomment>{space}+	tabs(col, yytext, yyleng);
<lcomment>"//".*	{
  buf += string("\n")+(yytext+2); tabs(col, yytext, yyleng);
}
<lcomment>.		print(start, buf); yyless(0); BEGIN(INITIAL);

<comment>[^*]+		tabs(col, yytext, yyleng); buf += yytext;
<comment>"*"+[^*/]*	tabs(col, yytext, yyleng); buf += yytext;
<comment>"*"+"/"        col += yyleng; print(start, buf); BEGIN(INITIAL);

\"{str}\"		|
\"{str}			tabs(col, yytext, yyleng);
.			col++;

<*><<EOF>>		{
  if (YY_START != INITIAL)
    print(start, buf); BEGIN(INITIAL);
  if (*++files) {
    yyin = fopen(*files, "r");
    if (!yyin) {
      perror(*files);
      exit(1);
    }
    col = 0;
  } else
    yyterminate();
}

%%

static unsigned tabs(unsigned& col, const char *text, unsigned len)
{
  for (unsigned i = 0; i < len; i++)
    if (strchr("\n\f\v\r", text[i]))
      col = 0;
    else if (text[i] == '\t')
      col = (col/tabsize+1)*tabsize;
    else // FIXME: no proper handling of utf-8
      col++;
}

static void print(unsigned col, string& text)
{
  size_t p = text.find_first_not_of(" \t");
  if (p != string::npos) {
    tabs(col, text.c_str(), p);
  }
  text.erase(0, p);

  // We assume that any comment starting with ':', '..' or '__' at the
  // beginning of the first non-empty line is rst source we want.

  p = text.find_first_not_of("\n\f\v\r");
  if (p != string::npos) {
    string start = text.substr(p, 2);
    if (start[0] != ':' &&
	((start != ".." && start != "__") ||
	 (text.length() > 2 && !strchr("\n\f\v\r", text[2]))))
      return;
    if (p > 0) col = 0;
  } else
    return;

  // tokenize text into lines
  char *s = new char [text.size()+1], *t, *last_t;
  strcpy(s, text.c_str());
  t = strtok(s, "\n");
  if (t) {
    last_t = t+strlen(t);
    cout << t << endl;
    t = strtok(NULL, "\n");
    while (t) {
      size_t n = t-(last_t+1);
      // handle empty lines (strtok merges adjacent delims into one)
      for (size_t i = 0; i < n; i++)
	cout << endl;
      // trim whitespace in subsequent lines
      string text = t;
      size_t p = text.find_first_not_of(" \t");
      if (p != string::npos) {
	unsigned col1 = 0;
	tabs(col1, text.c_str(), p);
	text.erase(0, p);
	if (col1 > col)
	  text = string(col1-col, ' ') + text;
	cout << text << endl;
      } else
	cout << endl;
      last_t = t+strlen(t);
      t = strtok(NULL, "\n");
    }
    cout << endl;
  }
  delete[] s;
}

extern "C"
int main(int argc, char *argv[])
{
  prog = *argv;
  files = argv;
  if (*++files) {
    yyin = fopen(*files, "r");
    if (!yyin) {
      perror(*files);
      exit(1);
    }
  } else
    --files;
  yylex();
  exit(0);
}
