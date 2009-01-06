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
