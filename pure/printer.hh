
#ifndef PRINTER_HH
#define PRINTER_HH

#include <iostream>
#include "expr.hh"
#include "matcher.hh"
#include "runtime.h"

/* Pretty-printing of expressions and related structures. */

ostream& operator << (ostream& os, const expr& x);
ostream& operator << (ostream& os, const exprl& xl);
ostream& operator << (ostream& os, const rule& r);
ostream& operator << (ostream& os, const rulel& rl);
ostream& operator << (ostream& os, const env& e);

/* Pretty-printing of pattern matching automata. Useful for debugging
   purposes. */

ostream& operator << (ostream& os, const trans& tr);
ostream& operator << (ostream& os, const state& st);
ostream& operator << (ostream& os, const matcher& m);

/* Pretty-printing of the runtime expression data structure. */

ostream& operator << (ostream& os, const pure_expr *x);

#endif // ! PRINTER_HH
