%{                                            /* -*- C++ -*- */
#include <cstdlib>
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <string>
#include <list>
#include <map>
#include <iostream>

/* Work around an incompatibility in flex (at least versions 2.5.31 through
   2.5.33): it generates code that does not conform to C89.  See Debian bug
   333231 <http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=333231>.  */
# undef yywrap
# define yywrap() 1

/* Uncomment this to get latex labels for explicit hyperlink targets
   (experimental). This is needed to get proper page numbers in the
   TeX-formatted index. Unfortunately, this also messes up the formatting of
   bulleted and description lists with embedded targets, so it's disabled by
   default until we find a better solution. */

// #define LATEX_TARGETS 1

using namespace std;

static char *prog, **files;
static bool literate = false;
static bool sphinx = false;
static unsigned col = 0, start;
static unsigned tabsize = 8;
static string buf, comment_text, literate_text;
static void tabs(unsigned& col, const char *text, unsigned len);
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
  start = col; buf = yytext+2;
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

static void tabs(unsigned& col, const char *text, unsigned len)
{
  for (unsigned i = 0; i < len; i++)
    if (strchr("\n\f\v\r", text[i]))
      col = 0;
    else if (text[i] == '\t')
      col = (col/tabsize+1)*tabsize;
    else // FIXME: no proper handling of utf-8
      col++;
}

static string expand_tabs(const string& text)
{
  string buf;
  unsigned col = 0;
  for (unsigned i = 0; i < text.size(); i++)
    if (strchr("\n\f\v\r", text[i])) {
      col = 0;
      buf += text[i];
    } else if (text[i] == '\t') {
      unsigned col0 = col;
      col = (col/tabsize+1)*tabsize;
      buf += string(col-col0, ' ');
    } else { // FIXME: no proper handling of utf-8
      col++;
      buf += text[i];
    }
  return buf;
}

static unsigned trim(string& text, unsigned col)
{
  text = expand_tabs(text);
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

/* Handle hyperlink targets. To supplement docutils' own hyperlink processing,
   we also create raw html targets for these. This works around the docutils
   name mangling (which is undesirable if we're looking, e.g., for function
   names), and resolves quirks with w3m which doesn't pick up all 'id'
   attributes. It also allows us to output an index of all explicit targets in
   a document. This is requested with the 'makeindex::' directive.  (This
   feature is rather simplistic right now and can't compete with a carefully
   handmade index, but as docutils doesn't provide an index facility of its
   own, it is certainly better than having no index at all.) */

static string cache;

static void flush_cache()
{
  cout << cache;
  cache.clear();
}

static list<string> targets;
static map<string,int> labels;
static int act_label = 0;

static bool compare(string first, string second)
{
  if (first.empty() || second.empty() ||
      isalnum(first[0]) == isalnum(second[0]))
    return first<second;
  else
    return isalnum(first[0]) < isalnum(second[0]);
}

static bool targetp(const string& text)
{
  size_t p0 = text.find_first_not_of(" \t"), p = p0;
  if (p != string::npos && text.substr(p, 2) == ".." &&
      p+2 < text.size() && (text[p+2] == ' ' || text[p+2] == '\t') &&
      (p = text.find_first_not_of(" \t", p+2)) != string::npos) {
    if (text[p] == '_') {
      string target = text.substr(p+1);
      // trim trailing whitespace
      p = target.find_last_not_of(" \t");
      if (p != string::npos && target[p] == ':') {
	target.erase(p);
	// take care of escapes
	while ((p = target.find("\\:")) != string::npos)
	  target.erase(p, 1);
	while ((p = target.find("\\_")) != string::npos)
	  target.erase(p, 1);
	if (target.empty()) goto notarget;
	size_t n = target.size();
	if (target[0]=='`' && n>1 && target[n-1]=='`')
	  target = target.substr(1, n-2);
	if (target.empty()) goto notarget;
	if (sphinx) {
	  /* Sphinx-compatible output. Generate an index entry. */
	  cache += text; cache += "\n";
	  string indent = text.substr(0, p0);
	  cout << indent << ".. index:: " << target << endl;
	} else if (labels.find(target) == labels.end()) {
	  /* We found a new hyperlink target. Store it away in the cache, to
	     be emitted later, and create a raw html target for it. */
	  targets.push_back(target);
	  labels[target] = act_label++;
	  cache += text; cache += "\n";
	  string indent = text.substr(0, p0);
	  cout << indent << ".. raw:: html" << endl << endl
	       << indent << "   <a name=\"" << target << "\">"
	       << endl << endl;
#if LATEX_TARGETS
	  cout << indent << ".. raw:: latex" << endl << endl
	       << indent << "   \\label{idx:" << labels[target] << "}"
	       << endl << endl;
#endif
	}
	return true;
      } else
	goto notarget;
    } else if (text.substr(p, 11) == "makeindex::" &&
	       text.find_first_not_of(" \t", p+11) == string::npos) {
      /* Emit the index. */
      flush_cache();
      targets.sort(compare);
      char last = 0;
      for (list<string>::iterator it = targets.begin(), end = targets.end();
	   it != end; it++) {
	char ind = (*it)[0];
	if (last && isalpha(ind) && ind != last) cout << endl;
	last = ind;
#if LATEX_TARGETS
	cout << "| `" << *it
	     << "`_\\ :raw-latex-index:`\\ \\ \\pageref{idx:"
	     << labels[*it] << "}`" << endl;
#else
	cout << "| `" << *it << "`_" << endl;
#endif
      }
      cout << endl;
      targets.clear();
      labels.clear();
      return true;
    } else
      goto notarget;
  } else {
  notarget:
    flush_cache();
    return false;
  }
}

static void print(unsigned col, string& text)
{
  static unsigned last_offs = 0, last_indent = 0;

  // trim whitespace from the front
  unsigned col0 = col;
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
    last_offs = col0;
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
    string text = t;
    last_t = t+text.size();
    if (!targetp(text))
      cout << text << endl;
    t = strtok(NULL, "\n");
    while (t) {
      size_t n = t-(last_t+1);
      // handle empty lines (strtok merges adjacent delims into one)
      if (n > 0) flush_cache();
      for (size_t i = 0; i < n; i++)
	cout << endl;
      // trim whitespace in subsequent lines
      string text = t;
      size_t p = text.find_first_not_of(" \t");
      if (p != string::npos) {
	last_indent = trim(text, col);
	if (!targetp(text))
	  cout << text << endl;
      } else {
	flush_cache();
	cout << endl;
      }
      last_t = t+strlen(t);
      t = strtok(NULL, "\n");
    }
    flush_cache();
    cout << endl;
  }
  delete[] s;
}

extern "C"
int main(int argc, char *argv[])
{
  prog = *argv++;
  if (*argv && **argv == '-') {
    char *s = *argv, *end = s;
    if (s[1] == 'h' && !s[2]) {
      cerr << "Usage: " << prog << " [-h|-s|-tn] [source-files ...]" << endl
	   << "-h   print this message" << endl
	   << "-s   generate Sphinx-compatible output" << endl
	   << "-tn  tabs are n spaces wide" << endl;
      exit(0);
    } else if (s[1] == 's' && !s[2]) {
      sphinx = true; end = s+2;
    } else if (s[1] == 't' && s[2])
      tabsize = strtoul(s+2, &end, 10);
    if (*end) {
      cerr << prog << ": invalid option " << s
	   << ", try '" << prog << " -h' for help" << endl;
      exit(1);
    }
    argv++;
  }
  files = argv;
  if (*files) {
    yyin = fopen(*files, "r");
    if (!yyin) {
      perror(*files);
      exit(1);
    }
  } else
    --files;
#if LATEX_TARGETS
  cout << ".. role:: raw-latex-index(raw)\n   :format: latex\n\n";
#endif
  yylex();
  exit(0);
}
