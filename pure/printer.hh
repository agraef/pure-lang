
/* Copyright (c) 2008-2010 by Albert Graef <Dr.Graef@t-online.de>.

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

#ifndef PRINTER_HH
#define PRINTER_HH

#include <iostream>
#include "expr.hh"
#include "matcher.hh"
#include "runtime.h"

/* Pretty-printing of expressions and related structures. */

ostream& printx(ostream& os, const expr& x, bool pat, bool aspat = true);

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
