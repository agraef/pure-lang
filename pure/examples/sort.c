
/* Sort a Pure list using the C qsort() function. 2008-06-25 AG */

/* Another example using the runtime API. It implements an external function
   'sort' which can be loaded inside the Pure interpreter. The function, to be
   invoked as 'sort p xs', calls the qsort() routine from the C library to
   sort a Pure list xs using a given Pure predicate p, which compares two
   elements x and y and returns a truth value indicating whether x is less
   than y. The example illustrates how we can program a C function to be
   called from Pure which in turn calls other Pure functions, and takes
   generic pure_expr* values as arguments and returns them as results. */

/* To compile (Linux): 'gcc -shared -o sort.so sort.c -lpure' (add -fPIC on 64
   bit systems). This will create a dynamic library ready to be loaded by the
   Pure interpreter. (On OSX and Windows, replace .so with .dylib or .dll,
   respectively. On OSX, you also have to replace -shared with -dynamiclib.
   On Windows you might wish to add the '-Wl,--enable-auto-import' linker
   option.)

   Now start the interpreter and enter the following to "dlopen" sort.so and
   declare the sort function:

   > using "lib:sort";
   > extern expr* sort(expr* p, expr *xs);

   The sort function is now ready to be called as 'sort p xs', e.g.:

   > sort (<) (1..10);
   [1,2,3,4,5,6,7,8,9,10]
   > sort (>) (1..10);
   [10,9,8,7,6,5,4,3,2,1]

   Have some fun with random lists, comparing our sort function with the one
   from hello.pure. (The rand function is also declared in hello.pure; it is
   just the rand() function from the C library.)

   > run hello.pure
   Hello, world!
   > let xs = [rand; i = 1..100000];
   > stats
   > #sort (<) xs;
   100000
   1.05s
   > #qsort (<) xs;
   100000
   14.05s

   The above results are for my Athlon 2500+. YMMV, but most likely you'll get
   similar results indicating that the C implementation is much faster. That's
   because the quicksort algorithm in hello.pure juggles around with lists,
   whereas the C quicksort routine uses vectors and sorts the elements
   in-place. */

#include <stdlib.h>
#include <pure/runtime.h>

/* Set up a C callback which in turn invokes a Pure predicate to perform the
   comparison of list elements. */

static pure_expr* cmp_p;
static int cmp(const void *xp, const void *yp)
{
  pure_expr *x = *(pure_expr**)xp, *y = *(pure_expr**)yp;
  /* We use pure_appl to invoke the Pure predicate stored in cmp_p on the list
     elements x and y passed by the qsort() routine. */
  pure_expr *p = pure_appl(cmp_p, 2, x, y);
  int res = pure_is_int(p, &res) && res; /* x<y? */
  pure_freenew(p); /* collect temporary */
  if (res)
    res = -1;
  else {
    /* Invoke cmp_p another time to perform the reverse comparison. */
    p = pure_appl(cmp_p, 2, y, x);
    res = pure_is_int(p, &res) && res; /* y<x? */
    pure_freenew(p); /* collect temporary */
    /* Note that if both tests failed then the elements are either equal or
       incomparable, in which case res==0. */
  }
  return res;
}

pure_expr *sort(pure_expr *p, pure_expr *xs)
{
  size_t size;
  pure_expr **elems;
  /* Deconstruct the list argument which is passed as a pure_expr* value.
     This yields a vector of pure_expr* elements which can be passed to the
     qsort() routine. */
  if (pure_is_listv(xs, &size, &elems)) {
    pure_expr *ys;
    /* Invoke qsort() to sort the elems vector. */
    cmp_p = p;
    qsort(elems, size, sizeof(pure_expr*), cmp);
    /* Construct a new list value from the sorted vector, to be returned as
       the function result. */
    ys = pure_listv(size, elems);
    /* The elems vector returned by pure_is_listv is malloc'ed, free it now so
       that we don't leak memory. */
    free(elems);
    return ys;
  } else
    /* The xs argument wasn't a proper list value, return a NULL pointer to
       indicate failure. This will make the 'sort p xs' call a normal form. */
    return 0;
}
