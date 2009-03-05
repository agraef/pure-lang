%{                                            /* -*- C++ -*- */
#include <cstdlib>
#include <assert.h>
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
static bool literate = false;
static unsigned col = 0, start;
static const unsigned tabsize = 8;
static string buf, comment_text, literate_text;
static unsigned tabs(unsigned& col, const char *text, unsigned len);
static void print(unsigned col, string& text);

static inline void echo(const char *s)
{
  if (literate) {
    size_t l = strlen(s);
    if (literate_text.length()+l > literate_text.capacity())
      literate_text.reserve(literate_text.capacity()+1024);
    literate_text += s;
  }
}
%}

%option noyywrap nounput

str    ([^\"\\\n]|\\(.|\n))*
space  [ \t\f\v\r\n]

%x comment lcomment

%%

{space}+		|
^"#!".*			echo(yytext); tabs(col, yytext, yyleng);

"//".*			{
  start = 0; buf = yytext+2;
  comment_text = yytext;
  tabs(col, yytext, yyleng);
  BEGIN(lcomment);
}
"/*"			{
  col += yyleng; start = col; buf.clear();
  comment_text = yytext;
  BEGIN(comment);
}

<lcomment>{space}/"//"	comment_text += yytext; tabs(col, yytext, yyleng);
<lcomment>"//".*	{
  buf += string("\n")+(yytext+2);
  comment_text += yytext;
  tabs(col, yytext, yyleng);
}
<lcomment>{space}+	|
<lcomment>.		print(start, buf); yyless(0); BEGIN(INITIAL);

<comment>[^*]+		{
  tabs(col, yytext, yyleng);
  buf += yytext; comment_text += yytext;
}
<comment>"*"+[^*/]*	{
  tabs(col, yytext, yyleng);
  buf += yytext; comment_text += yytext;
}
<comment>"*"+"/"        {
  col += yyleng;
  comment_text += yytext;
  print(start, buf);
  BEGIN(INITIAL);
}

\"{str}\"		|
\"{str}			echo(yytext); tabs(col, yytext, yyleng);
.			echo(yytext); col++;

<*><<EOF>>		{
  if (YY_START != INITIAL)
    print(start, buf); BEGIN(INITIAL);
  literate = false;
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

static unsigned trim(string& text, unsigned col)
{
  size_t p = text.find_first_not_of(" \t");
  if (p != string::npos) {
    unsigned col1 = 0;
    tabs(col1, text.c_str(), p);
    text.erase(0, p);
    if (col1 > col) {
      unsigned indent = col1-col;
      text = string(indent, ' ') + text;
      return indent;
    } else
      return 0;
  } else
    return 0;
}

static void print(unsigned col, string& text)
{
  static unsigned last_offs = 0, last_indent = 0;

  // trim whitespace from the front
  size_t p = text.find_first_not_of(" \t");
  if (p != string::npos) {
    tabs(col, text.c_str(), p);
  }
  text.erase(0, p);

  // trim whitespace from the back
  p = text.find_last_not_of(" \t");
  if (p != string::npos) text.erase(p+1);

  // Look for literate program designations (>>>, <<< just by itself).

  if (text == ">>>") {
    literate = true;
    last_offs = col-2;
    return;
  } else if (text == "<<<") {
    literate = false;
    // Break the literate code into lines and indent appropriately.
    string indent(last_indent, ' ');
    cout << (indent+"::\n\n");
    indent += "  ";
    char *s = new char [literate_text.size()+1], *t, *last_t;
    strcpy(s, literate_text.c_str());
    t = strtok(s, "\n");
    if (t) {
      last_t = t+strlen(t);
      string text = t;
      trim(text, last_offs);
      cout << indent << text << endl;
      t = strtok(NULL, "\n");
      while (t) {
	size_t n = t-(last_t+1);
	// handle empty lines (strtok merges adjacent delims into one)
	for (size_t i = 0; i < n; i++)
	  cout << endl;
	text = t;
	trim(text, last_offs);
	cout << indent << text << endl;
	last_t = t+strlen(t);
	t = strtok(NULL, "\n");
      }
      cout << endl;
    }
    delete[] s;
    literate_text.clear();
    return;
  } else if (literate) {
    echo(comment_text.c_str());
    return;
  }

  // We assume that any comment starting with ':', '..' or '__' at the
  // beginning of the first non-empty line is rst source we want.

  p = text.find_first_not_of("\n\f\v\r\t ");
  if (p != string::npos) {
    string start = text.substr(p, 2);
    if (start[0] != ':' &&
	((start != ".." && start != "__") ||
	 (text.length() > 2 && !strchr("\n\f\v\r\t ", text[2]))))
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
	last_indent = trim(text, col);
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
