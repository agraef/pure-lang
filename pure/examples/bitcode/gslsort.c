
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_sort_double.h>

/* This sorts a GSL matrix (which must be a row or column vector) in-place. */

void gslsort(gsl_matrix *x)
{
  if (x->size1 <= 1 || x->size2 <= 1)
    gsl_sort(x->data, 1, x->size1*x->size2);
}
