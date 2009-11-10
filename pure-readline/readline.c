
/* Copyright (c) 2009 Albert Graef

   Copying and distribution of this file, with or without modification,
   are permitted in any medium without royalty provided the copyright
   notice and this notice are preserved.  This file is offered as-is,
   without any warranty. */

#include <stdio.h>

/* You might have to edit this if readline/editline lives elsewhere on your
   system. */
#ifdef USE_LIBEDIT
#include <editline/readline.h>
#else
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include <pure/runtime.h>

extern pure_expr *wrap_readline(const char *prompt)
{
  return pure_cstring(readline(prompt));
}
