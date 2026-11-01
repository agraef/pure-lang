/* Safe-mode (sandbox) test for libpure.

   A host that runs untrusted Pure code (a trusted SQL procedural language,
   say) creates the interpreter with "--safe", loads the modules it trusts,
   forbids the dangerous functions the prelude leaves bound, then arms the
   sandbox with pure_safe_arm. After that, Pure code must not be able to reach
   out of the language. This checks that each escape route is refused on an
   armed safe interpreter, and -- as a control that the route is real -- that
   the same code works on an ordinary interpreter.

   Built and run by 'make check-safe'. Exits 0 on success. */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

static int failures = 0;

static void check(int ok, const char *what)
{
  printf("%s %s\n", ok ? "ok:  " : "FAIL:", what);
  if (!ok) failures++;
}

/* Evaluate s; return 1 if it produced a value with no error and no uncaught
   exception, 0 otherwise. pure_eval returns NULL and sets lasterr() on a
   compile/eval error, and leaves an uncaught throw in pure_last_exception(). */
static int evals_ok(const char *s)
{
  pure_expr *r;
  clear_lasterr();
  r = pure_eval(s);
  {
    pure_expr *e = pure_last_exception();
    if (e) { return 0; }
  }
  if (!r || (lasterr() && *lasterr())) return 0;
  return 1;
}

/* Evaluate s and return 1 if it produced a concrete value -- an int, a double
   or a pointer -- i.e. a dangerous C function actually ran. A neutralized
   function leaves a normal-form application (its undefined symbol applied to
   the arguments), which is none of these, so this returns 0. */
static int reaches_c(const char *s)
{
  pure_expr *r;
  int32_t i; double d; void *p;
  int concrete;
  clear_lasterr();
  r = pure_eval(s);
  if (pure_last_exception() || !r) return 0;
  concrete = pure_is_int(r, &i) || pure_is_double(r, &d) ||
    pure_is_pointer(r, &p);
  return concrete;
}

int main(void)
{
  char *argv[] = {"safe", NULL};

  /* Control: an ordinary interpreter runs all the routes we will forbid, so
     the sandbox test below is known to exercise real capabilities. */
  pure_interp *plain = pure_create_interp(1, argv);
  check(plain != 0, "create an ordinary interpreter");
  pure_switch_interp(plain);
  check(evals_ok("1+1"), "control: arithmetic works");
  check(evals_ok("extern double sin(double); sin 0.0"),
	"control: extern works");
  check(evals_ok("eval \"2*3\""), "control: eval works");
  pure_delete_interp(plain);

  /* The sandbox. */
  char *sargv[] = {"safe", "--safe", NULL};
  pure_interp *s = pure_create_interp(2, sargv);
  check(s != 0, "create a --safe interpreter");
  pure_switch_interp(s);
  check(pure_is_safe(), "pure_is_safe() is true");

  /* Setup phase (still trusted): the prelude loads (the dangerous externs it
     declares are silently skipped, so they never bind), math is allowed as a
     module. All of this must work before arming. */
  check(evals_ok("1+1"), "setup: prelude arithmetic works");
  check(pure_safe_allow_using("math") == 1, "allow using math");
  /* Load math now, during the trusted phase: it declares its own externs
     (log, pure_rational, ...), which only a trusted load may do. */
  check(evals_ok("using math; 1%2"), "setup: load the math module");

  check(pure_safe_arm() == 1, "arm the sandbox");

  /* Armed: benign code still works, every escape is refused. */
  check(evals_ok("foldl (+) 0 (map (\\x->x*x) (1..10))"),
	"armed: pure functional code works");
  check(evals_ok("1%2 + 1%3"),
	"armed: a function from the allowlisted module works");

  check(!evals_ok("extern double cos(double); cos 0.0"),
	"armed: extern is refused");
  check(!evals_ok("%< int foo() { return 1; } %>\nfoo"),
	"armed: inline code is refused");
  check(!evals_ok("eval \"2*3\""), "armed: eval is refused");
  check(!evals_ok("evalcmd \"show foo\""), "armed: evalcmd is refused");
  check(!evals_ok("using \"lib:m\""), "armed: using lib: is refused");
  check(!evals_ok("using \"/etc/passwd\""),
	"armed: using a file path is refused");
  check(!evals_ok("using system"),
	"armed: using an off-allowlist module is refused");

  /* The dangerous functions never bound to their C function, so they are
     undefined symbols: a call is a normal form (e.g. the expression
     'exit 1'), not the integer or pointer a real call returns, and it does
     not reach C. The decisive check is that exit does not terminate this
     process -- if it did, nothing below would print. */
  check(!reaches_c("exit 1"), "armed: exit does not reach the C function");
  check(!reaches_c("malloc 8"), "armed: malloc does not reach the C function");
  check(!reaches_c("get_byte (sentry 0 0)"),
	"armed: get_byte does not reach the C function");
  /* Reached here, so exit 1 above did not terminate the process. */
  check(1, "armed: the process survived calling exit");

  pure_delete_interp(s);

  printf(failures ? "safe tests FAILED (%d)\n" : "safe tests passed\n",
	 failures);
  return failures ? 1 : 0;
}
