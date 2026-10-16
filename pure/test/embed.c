/* Embedding contract test for libpure.

   Runs as a host application with no Pure code on the C stack, the way a
   database extension or a plugin calls into Pure, and checks the promises
   the "Embedding" section of runtime.h makes:

   - uncaught exceptions are returned to the host instead of aborting it;
   - the interrupt hook cancels running Pure code;
   - the stack limit turns runaway recursion into stack_fault;
   - repeated evaluation runs in bounded memory;
   - two interpreters can be used alternately on one thread, and one
     keeps working after the other is deleted;
   - interpreters created, used and deleted on separate threads work.

   Built and run by 'make check-embed'. Exits 0 on success. */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(__GLIBC__)
#include <malloc.h>
#endif
#include "runtime.h"

static int failures = 0;

static void check(int ok, const char *what)
{
  printf("%s %s\n", ok ? "ok:  " : "FAIL:", what);
  if (!ok) failures++;
}

/* Is x the application (f n) for symbol f and integer n? */
static int is_app_int(pure_expr *x, const char *f, int32_t n)
{
  pure_expr *u, *v;
  int32_t k;
  return x && pure_is_app(x, &u, &v) && pure_is_symbol(u, 0) &&
    strcmp(pure_sym_pname(u->tag), f) == 0 && pure_is_int(v, &k) && k == n;
}

static int is_sym(pure_expr *x, const char *s)
{
  return x && pure_is_symbol(x, 0) && strcmp(pure_sym_pname(x->tag), s) == 0;
}

/* Heap bytes in use, or -1 where this cannot be measured. RSS is not a
   usable measure here: it depends on how the allocator grows and returns
   its arenas, which differs between machines. */
static long heap_in_use(void)
{
#if defined(__GLIBC__) && (__GLIBC__ > 2 || __GLIBC_MINOR__ >= 33)
  return (long)mallinfo2().uordblks;
#else
  return -1;
#endif
}

static volatile int cancel_requested = 0;

static int interrupt_hook(void)
{
  return cancel_requested ? 2 : 0;
}

static void test_exceptions(pure_expr *h)
{
  pure_expr *r, *e = 0;
  int32_t k;

  r = pure_app(h, pure_int(7));
  check(r == 0, "pure_app returns NULL for an uncaught exception");
  check(is_app_int(pure_last_exception(), "oops", 7),
	"pure_last_exception() is the thrown value");
  pure_clear_exception();
  check(pure_last_exception() == 0, "pure_clear_exception clears it");

  r = pure_appl(h, 1, pure_int(8));
  check(r == 0 && is_app_int(pure_last_exception(), "oops", 8),
	"pure_appl reports the exception the same way");

  r = pure_appx(h, pure_int(9), &e);
  check(r == 0 && is_app_int(e, "oops", 9), "pure_appx hands it back");

  r = pure_symbol(pure_sym("thrower"));
  check(r == 0 && is_app_int(pure_last_exception(), "oops", 0),
	"pure_symbol of a throwing definition returns NULL");

  r = pure_eval("throw (oops 10)");
  check(r == 0 && *lasterr(), "pure_eval returns NULL and sets lasterr()");

  r = pure_app(pure_symbol(pure_sym("succ")), pure_int(41));
  check(r && pure_is_int(r, &k) && k == 42, "normal calls still work");
}

static void test_interrupt(void)
{
  pure_expr *r;
  pure_set_interrupt_hook(interrupt_hook);
  cancel_requested = 1;
  r = pure_app(pure_symbol(pure_sym("loop")), pure_int(0));
  cancel_requested = 0;
  pure_set_interrupt_hook(0);
  check(r == 0 && is_app_int(pure_last_exception(), "signal", 2),
	"the interrupt hook cancels an infinite loop");
}

static void test_stack(void)
{
  char base;
  pure_expr *r;
  int32_t k;
  pure_set_stack_limit(&base, 256 * 1024);
  r = pure_app(pure_symbol(pure_sym("deep")), pure_int(100000000));
  check(r == 0 && is_sym(pure_last_exception(), "stack_fault"),
	"runaway recursion is reported as stack_fault");
  r = pure_app(pure_symbol(pure_sym("deep")), pure_int(100));
  check(r && pure_is_int(r, &k) && k == 100,
	"evaluation works after a stack fault");
}

static void test_memory(pure_expr *f)
{
  enum { WARMUP = 100, N = 400 };
  long before, after;
  int i;
  pure_expr *x;
  for (i = 0; i < WARMUP; i++) {
    x = pure_eval("double_all (1..100)");
    if (x) pure_freenew(x);
    x = pure_app(f, pure_int_seq(1, 100, 1));
    if (x) pure_freenew(x);
  }
  before = heap_in_use();
  for (i = 0; i < N; i++) {
    x = pure_eval("double_all (1..100)");
    if (x) pure_freenew(x);
    x = pure_app(f, pure_int_seq(1, 100, 1));
    if (x) pure_freenew(x);
  }
  after = heap_in_use();
  /* Without freeing the compiled code and LLVM analyses of each
     evaluation the heap grew by about 50 KB per pure_eval (20 MB here);
     with it, by about 0.1 KB. */
  if (before >= 0 && after >= 0) {
    printf("      heap growth over %d evaluations: %ld KB\n", N,
	   (after - before) / 1024);
    check(after - before < 2 * 1024 * 1024,
	  "repeated evaluation keeps the heap bounded (< 2 MB growth)");
  }
}

/* Two interpreters on one thread (one per plugin instance, as pure-lv2
   does): evaluate alternately, delete one, keep using the other. */
static void test_two_interpreters(void)
{
  char *argv[] = {"embed", NULL};
  pure_interp *a = pure_create_interp(0, argv);
  pure_interp *b = pure_create_interp(0, argv);
  pure_expr *x;
  int32_t k;
  int i, ok = a && b;
  for (i = 0; i < 20 && ok; i++) {
    pure_switch_interp(i % 2 ? b : a);
    x = pure_eval("foldl (+) 0 (map (\\x->x+1) (0..99))");
    ok = x && pure_is_int(x, &k) && k == 5050;
    if (x) pure_freenew(x);
  }
  check(ok, "two interpreters evaluate alternately on one thread");
  if (b) pure_delete_interp(b);
  if (a) {
    pure_switch_interp(a);
    x = pure_eval("map (\\x->x*2) (1..3)");
    check(x != 0, "an interpreter keeps working after another is deleted");
    if (x) pure_freenew(x);
    pure_delete_interp(a);
  }
}

static void *thread_main(void *arg)
{
  char *argv[] = {"embed", NULL};
  pure_interp *ip = pure_create_interp(0, argv);
  int i, *ok = arg;
  *ok = ip != NULL;
  if (!ip) return NULL;
  pure_switch_interp(ip);
  for (i = 0; i < 5 && *ok; i++) {
    pure_expr *x = pure_eval("foldl (+) 0 (1..100)");
    int32_t k;
    *ok = x && pure_is_int(x, &k) && k == 5050;
    if (x) pure_freenew(x);
  }
  pure_delete_interp(ip);
  return NULL;
}

static void test_threads(void)
{
  enum { N = 4 };
  pthread_t t[N];
  int ok[N], i, all = 1;
  for (i = 0; i < N; i++) pthread_create(&t[i], NULL, thread_main, &ok[i]);
  for (i = 0; i < N; i++) { pthread_join(t[i], NULL); all = all && ok[i]; }
  check(all, "interpreters created and used on separate threads");
}

int main(void)
{
  char *argv[] = {"embed", NULL};
  pure_interp *ip = pure_create_interp(0, argv);
  pure_expr *h, *f;
  check(ip != NULL, "pure_create_interp");
  if (!ip) return 1;
  pure_switch_interp(ip);
  pure_eval("h x = throw (oops x); thrower = throw (oops 0); "
	    "loop n = loop (n+1); "
	    "deep n = if n == 0 then 0 else 1 + deep (n-1); "
	    "double_all xs = map (\\x->2*x) xs;");
  h = pure_new(pure_symbol(pure_sym("h")));
  f = pure_new(pure_symbol(pure_sym("double_all")));
  test_exceptions(h);
  test_interrupt();
  test_stack();
  test_memory(f);
  pure_free(h);
  pure_free(f);
  pure_delete_interp(ip);
  test_two_interpreters();
  test_threads();
  printf("%s\n", failures ? "EMBED TESTS FAILED" : "embed tests passed");
  return failures != 0;
}
