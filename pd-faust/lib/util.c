
/* Various helper functions implemented in C for maximum efficiency. */

#include <stdlib.h>
#include <string.h>
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

/* Mix down a collection of voices to a single signal. */

void pdfaust_mix(pure_expr *in, double *out)
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

/* Fill the right half of a signal matrix with an input signal, in preparation
   for a crossfade. */

/* NOTE: The input matrix in (passed as a Pure expression) has dimensions k,l
   and the output matrix out (passed as a double*, assumed to be contiguous)
   has the dimensions k,2*l. We then simply copy the ith row of in into the
   ith row of out, starting in the lth column of out, for i=0..k-1. The two
   matrices must not overlap. */

void pdfaust_fill(pure_expr *in, double *out)
{
  gsl_matrix *m;
  if (pure_is_double_matrix(in, (void**)&m)) {
    size_t i, k = m->size1, l = m->size2;
    double *p = out+l;
    if (k*l == 0) return;
    for (i = 0; i < k; i++, p += 2*l)
      memcpy(p, m->data+i*m->tda, l*sizeof(double));
  }
}

/* Helper function to perform a multichannel crossfade. We do all this
   in-place, to avoid costly allocations inside the dsp loop. */

void pdfaust_crossfade(pure_expr *f, pure_expr *d, pure_expr *x, pure_expr *y)
{
  gsl_matrix_symbolic *m;
  gsl_matrix *m1, *m2;
  if (pure_is_symbolic_matrix(d, (void**)&m) &&
      pure_is_double_matrix(x, (void**)&m1) &&
      pure_is_double_matrix(y, (void**)&m2)) {
    size_t i, k = m->size2;
    size_t k1 = m1->size1, l1 = m1->size2;
    size_t k2 = m2->size1, l2 = m2->size2;
    double *data1 = m1->data;
    double *data2 = m2->data;
    /* Create static dummy matrix objects to hold the actual data rows passed
       in the x and y expressions. */
    static pure_expr *a, *b;
    static gsl_matrix *ma, *mb;
    if (!a) {
      a = pure_new(matrix_from_double_array_nodup(2, 0, NULL));
      if (!a || !pure_is_double_matrix(a, (void**)&ma)) return;
    }
    if (!b) {
      b = pure_new(matrix_from_double_array_nodup(1, 0, NULL));
      if (!b || !pure_is_double_matrix(b, (void**)&mb)) return;
    }
    /* Set the block size. */
    ma->size2 = l2; ma->tda = l2;
    mb->size2 = l2; mb->tda = l2;
    for (i = 0; i < k; i++) {
      /* Fill our dummy matrices with the current channel and invoke the xfade
	 dsp for that channel. */
      ma->data = data1+i*m1->tda;
      mb->data = data2+i*m2->tda;
      pure_freenew(pure_appl(f, 3, m->data[i], a, b));
    }
  }
}
