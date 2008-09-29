
/* Poor man's Pure interpreter. 2008-06-24 AG */

/* This is an example for calling Pure from a standalone C/C++ application
   which is *not* hosted by the command line version of the Pure interpreter,
   but uses the public runtime API of Pure 0.5 or later to create its own
   interpreter instance. The program implements a little command loop which
   reads Pure code from standard input, evaluates it and prints the results.

   Compile this with 'gcc -o poor poor.c -lpure', and run the resulting
   executable as './poor [args ...]'. You can use the same command line
   arguments as with the real Pure interpreter, including any Pure scripts to
   be loaded at startup. Input is line-oriented, so you can't continue
   definitions across lines, but in return you don't have to terminate each
   line with a ';' either, the eval() function already takes care of that. To
   terminate the program just type the end-of-file character at the beginning
   of a line.

   Please note that the interface to interpreter instances created with the
   runtime API is rather minimalistic right now. The interpreter always runs
   in non-interactive mode (thus none of the interactive commands will work)
   and eval() only returns the result of the last computed expression (this is
   what gets printed in the read-eval-print loop). */

#include <stdio.h>
#include <pure/runtime.h>

int main(int argc, char *argv[])
{
  pure_interp *interp = pure_create_interp(argc, argv);
  char buf[10000];
  if (!interp) return 1;
  fputs("? ", stdout); fflush(stdout);
  while (fgets(buf, sizeof(buf), stdin)) {
    pure_expr *x = eval(buf);
    if (x) {
      char *s = str(x);
      printf("%s\n", s);
      pure_freenew(x); free(s);
    } else if (lasterr())
      fputs(lasterr(), stderr);
    fputs("? ", stdout); fflush(stdout);
  }
  puts("[quit]");
  return 0;
}
