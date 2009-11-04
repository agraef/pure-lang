
/* Copyright (c) 2008, 2009 by Albert Graef <Dr.Graef@t-online.de>.

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

#ifndef LEXERDEFS_HH
#define LEXERDEFS_HH

#include "parserdefs.hh"
#include "parser.hh"

// Announce to Flex the prototype we want for lexing function, ...
#define YY_DECL \
  yy::parser::token_type						\
  yylex (yy::parser::semantic_type* yylval,				\
	 yy::parser::location_type* yylloc, interpreter& interp)
// ... and declare it for the parser's sake.
YY_DECL;

#endif // ! LEXERDEFS_HH
