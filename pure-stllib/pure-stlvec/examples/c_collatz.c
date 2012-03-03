/* c_collatz.c -- compute collatz numbers with callbacks to Pure for memoizing.
   Run gcc as follows to build c_collatz.so.

   gcc -shared c_collatz.c -lpure -o c_collatz.so

 */

#include "pure/runtime.h"

typedef long long int bg_int;

// memget n::int a -- returns memoized value for n or 0
// memput n::int value::int -- memoizes value for n

int lenchain(pure_expr *memget, pure_expr *memput, pure_expr *px_max, int n)
{
  int cache_max;
  pure_is_int(px_max, &cache_max);

  int memoized_value;
  if (n<cache_max) {
    pure_expr* px_mv = pure_appl(memget,1,pure_int(n));
    pure_is_int(px_mv, &memoized_value);
  } else {
    memoized_value = 0;
  }

  if (!memoized_value) {
    int steps = 0;
    bg_int cn = n;
    do {
      cn = (cn % 2) ? 3*cn + 1 : cn / 2;
      steps++;
    }
    while (cn > cache_max);
    steps += lenchain(memget, memput, px_max, cn);
    if (cn <cache_max)
      pure_appl(memput, 2, pure_int(n), pure_int(steps));
    memoized_value = steps;
  }

  return memoized_value;
}
