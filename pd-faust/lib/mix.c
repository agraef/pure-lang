
/* Mix down a collection of voices to a single signal. We implement this in C
   for maximum efficiency. */

#include <stdlib.h>
#include <pure/runtime.h>

typedef struct _gsl_matrix
{
  size_t size1;
  size_t size2;
  size_t tda;
  double *data;
} gsl_matrix;

typedef struct _gsl_matrix_symbolic
{
  size_t size1;
  size_t size2;
  size_t tda;
  pure_expr **data;
} gsl_matrix_symbolic;

void mix(pure_expr *in, double *out)
{
  gsl_matrix_symbolic *m;
  gsl_matrix **ms;
  if (pure_is_symbolic_matrix(in, (void**)&m)) {
    size_t i, j, k, l, n = m->size1*m->size2;
    if (n == 0 || m->size2 != m->tda) return;
    ms = alloca(n*sizeof(gsl_matrix*));
    for (i = 0; i < n; i++) {
      if (!pure_is_double_matrix(m->data[i], (void**)&ms[i])) return;
      l = ms[i]->size1*ms[i]->size2;
      if (i == 0)
        k = l;
      else if (k != l)
        return;
      if (ms[i]->size2 != ms[i]->tda) return;
    }
    for (j = 0; j < k; j++) out[j] = 0.0;
    for (i = 0; i < n; i++) {
      for (j = 0; j < k; j++)
        out[j] += ms[i]->data[j];
    }
  }
}
