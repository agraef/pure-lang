
#include <math.h>
#include <stdbool.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_version.h>
#include <gsl/gsl_complex_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_statistics_int.h>
#include <gsl/gsl_poly.h>
#include <gsl/gsl_fit.h>
#include <gsl/gsl_multifit.h>

#include <pure/runtime.h>

#include <stdio.h>

/* Copyright (c) 2008 by Albert Graef <Dr.Graef@t-online.de>.
   Copyright (c) 2008 by Robert E. Rucker <erucker@bmc.edu>.

   This file is part of the Pure programming language and system.

   Pure is free software: you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   Pure is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along
   with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* GSL number predicates. */

bool gsl_complexp(pure_expr *x)
{
  gsl_complex z;
  return pure_is_complex(x, z.dat);
}

/* Complex functions. These need to be wrapped, as there's no direct way to
   pass gsl_complex values between Pure and C. */

pure_expr *wrap_gsl_complex_sqrt(pure_expr *x)
{
  gsl_complex z;
  if (pure_is_complex(x, z.dat)) {
    gsl_complex ret = gsl_complex_sqrt(z);
    return pure_complex(ret.dat);
  } else
    return 0;
}

/* Wrappers for complex matrix-scalar operations. */

void wrap_gsl_matrix_complex_set_all(gsl_matrix_complex *m, pure_expr *x)
{
  /* For convenience, we also allow double values here. */
  double d;
  gsl_complex z;
  if (pure_is_double(x, &d)) {
    z.dat[0] = d; z.dat[1] = 0.0;
    gsl_matrix_complex_set_all(m, z);
  } else if (pure_is_complex(x, z.dat))
    gsl_matrix_complex_set_all(m, z);
}

int wrap_gsl_matrix_complex_scale(gsl_matrix_complex *a, pure_expr *x)
{
  double d;
  gsl_complex z;
  if (pure_is_double(x, &d)) {
    z.dat[0] = d; z.dat[1] = 0.0;
    return gsl_matrix_complex_scale(a, z);
  } else if (pure_is_complex(x, z.dat))
    return gsl_matrix_complex_scale(a, z);
  else
    return 0;
}

int wrap_gsl_matrix_complex_add_constant(gsl_matrix_complex *a, pure_expr *x)
{
  double d;
  gsl_complex z;
  if (pure_is_double(x, &d)) {
    z.dat[0] = d; z.dat[1] = 0.0;
    return gsl_matrix_complex_add_constant(a, z);
  } else if (pure_is_complex(x, z.dat))
    return gsl_matrix_complex_add_constant(a, z);
  else
    return 0;
}

/* Additional element-wise matrix operations missing in GSL. */

int gsl_matrix_pow_elements(gsl_matrix* a, const gsl_matrix* b)
{
  size_t i, j;
  const size_t n = a->size1, m = a->size2;
  if (n != b->size1 || m != b->size2)
    return GSL_EBADLEN;
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++)
      a->data[i*a->tda+j] = pow(a->data[i*a->tda+j], b->data[i*b->tda+j]);
  return GSL_SUCCESS;
}

int gsl_matrix_complex_pow_elements(gsl_matrix_complex* a,
				    const gsl_matrix_complex* b)
{
  size_t i, j;
  const size_t n = a->size1, m = a->size2;
  if (n != b->size1 || m != b->size2)
    return GSL_EBADLEN;
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++) {
      gsl_complex *x = (gsl_complex*)(a->data+2*(i*a->tda+j));
      gsl_complex *y = (gsl_complex*)(b->data+2*(i*b->tda+j));
      *x = gsl_complex_pow(*x, *y);
    }
  return GSL_SUCCESS;
}

int gsl_matrix_int_div(gsl_matrix_int* a, const gsl_matrix_int* b)
{
  size_t i, j;
  const size_t n = a->size1, m = a->size2;
  if (n != b->size1 || m != b->size2)
    return GSL_EBADLEN;
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++)
      if (b->data[i*b->tda+j] == 0)
	return GSL_EZERODIV;
      else
	a->data[i*a->tda+j] /= b->data[i*b->tda+j];
  return GSL_SUCCESS;
}

int gsl_matrix_int_mod(gsl_matrix_int* a, const gsl_matrix_int* b)
{
  size_t i, j;
  const size_t n = a->size1, m = a->size2;
  if (n != b->size1 || m != b->size2)
    return GSL_EBADLEN;
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++)
      if (b->data[i*b->tda+j] == 0)
	return GSL_EZERODIV;
      else
	a->data[i*a->tda+j] %= b->data[i*b->tda+j];
  return GSL_SUCCESS;
}

int gsl_matrix_int_shl(gsl_matrix_int* a, const gsl_matrix_int* b)
{
  size_t i, j;
  const size_t n = a->size1, m = a->size2;
  if (n != b->size1 || m != b->size2)
    return GSL_EBADLEN;
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++)
      a->data[i*a->tda+j] <<= b->data[i*b->tda+j];
  return GSL_SUCCESS;
}

int gsl_matrix_int_shr(gsl_matrix_int* a, const gsl_matrix_int* b)
{
  size_t i, j;
  const size_t n = a->size1, m = a->size2;
  if (n != b->size1 || m != b->size2)
    return GSL_EBADLEN;
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++)
      a->data[i*a->tda+j] >>= b->data[i*b->tda+j];
  return GSL_SUCCESS;
}

int gsl_matrix_int_and(gsl_matrix_int* a, const gsl_matrix_int* b)
{
  size_t i, j;
  const size_t n = a->size1, m = a->size2;
  if (n != b->size1 || m != b->size2)
    return GSL_EBADLEN;
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++)
      a->data[i*a->tda+j] &= b->data[i*b->tda+j];
  return GSL_SUCCESS;
}

int gsl_matrix_int_or(gsl_matrix_int* a, const gsl_matrix_int* b)
{
  size_t i, j;
  const size_t n = a->size1, m = a->size2;
  if (n != b->size1 || m != b->size2)
    return GSL_EBADLEN;
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++)
      a->data[i*a->tda+j] |= b->data[i*b->tda+j];
  return GSL_SUCCESS;
}

int gsl_matrix_int_not(gsl_matrix_int* a)
{
  size_t i, j;
  const size_t n = a->size1, m = a->size2;
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++)
      a->data[i*a->tda+j] = ~a->data[i*a->tda+j];
  return GSL_SUCCESS;
}

/* Matrix multiplication. */

int gsl_matrix_multiply(gsl_matrix* A, gsl_matrix* B, gsl_matrix* C)
{
  return gsl_blas_dgemm(CblasNoTrans, CblasNoTrans,
			1.0, A, B, 0.0, C);
}

int gsl_matrix_complex_multiply(gsl_matrix_complex* A, gsl_matrix_complex* B,
				gsl_matrix_complex* C)
{
  gsl_complex a = {1.0, 0.0}, b = {0.0, 0.0};
  return gsl_blas_zgemm(CblasNoTrans, CblasNoTrans,
			a, A, B, b, C);
}

/* Singular value decomposition. XXXTODO: Provide complex matrix versions of
   these, which are missing in GSL. */

int wrap_gsl_linalg_SV_decomp(gsl_matrix* A, gsl_matrix* V, gsl_matrix* S,
			      gsl_matrix* work)
{
  gsl_vector_view _S = gsl_matrix_diagonal(S);
  gsl_vector_view _work = gsl_matrix_row(work, 0);
  return gsl_linalg_SV_decomp(A, V, &_S.vector, &_work.vector);
}

int wrap_gsl_linalg_SV_decomp_mod(gsl_matrix* A, gsl_matrix* X,
				  gsl_matrix* V, gsl_matrix* S,
				  gsl_matrix* work)
{
  gsl_vector_view _S = gsl_matrix_diagonal(S);
  gsl_vector_view _work = gsl_matrix_row(work, 0);
  return gsl_linalg_SV_decomp_mod(A, X, V, &_S.vector, &_work.vector);
}

int wrap_gsl_linalg_SV_decomp_jacobi(gsl_matrix* A, gsl_matrix* V,
				     gsl_matrix* S)
{
  gsl_vector_view _S = gsl_matrix_diagonal(S);
  return gsl_linalg_SV_decomp_jacobi(A, V, &_S.vector);
}

int wrap_gsl_linalg_SV_solve(gsl_matrix* U, gsl_matrix* V, gsl_matrix* S,
			     const gsl_matrix* b, gsl_matrix* x)
{
  gsl_vector_view _S = gsl_matrix_diagonal(S);
  gsl_vector_const_view _b = gsl_matrix_const_column(b, 0);
  gsl_vector_view _x = gsl_matrix_column(x, 0);
  return gsl_linalg_SV_solve(U, V, &_S.vector, &_b.vector, &_x.vector);
}

pure_expr* wrap_gsl_stats_int_minmax(int* data, size_t n)
{
  int x, y;
  gsl_stats_int_minmax(&x, &y, data, 1, n);
  return pure_listl(2, pure_int(x), pure_int(y));
}

pure_expr* wrap_gsl_stats_minmax(double* data, size_t n)
{
  double x, y;
  gsl_stats_minmax(&x, &y, data, 1, n);
  return pure_listl(2, pure_double(x), pure_double(y));
}

pure_expr* wrap_gsl_stats_int_minmax_index(int* data, size_t n)
{
  size_t x, y;
  gsl_stats_int_minmax_index(&x, &y, data, 1, n);
  return pure_listl(2, pure_int(x), pure_int(y));
}

pure_expr* wrap_gsl_stats_minmax_index(double* data, size_t n)
{
  size_t x, y;
  gsl_stats_minmax_index(&x, &y, data, 1, n);
  return pure_listl(2, pure_int(x), pure_int(y));
}

pure_expr* wrap_gsl_poly_complex_eval(double* c, int len, double a, double b)
{
  gsl_complex z;
  z.dat[0] = a;
  z.dat[1] = b;
  z = gsl_poly_complex_eval(c, len, z);
  return pure_complex(z.dat);
}

pure_expr* wrap_gsl_complex_poly_complex_eval
  (gsl_matrix_complex *c, int len, double a, double b)
{
  int i = 0;
  gsl_complex z, *p, cc[2*len];
  double *d = c->data;
  z.dat[0] = a;
  z.dat[1] = b;
  p = cc;
  while (i++ < len) {
    p->dat[0] = *d++;
    p++->dat[1] = *d++;
  }
  z = gsl_complex_poly_complex_eval(cc, len, z);
  return pure_complex(z.dat);
}

pure_expr* wrap_gsl_poly_solve_quadratic(double a, double b, double c)
{
  double x0, x1;

  switch (gsl_poly_solve_quadratic(a, b, c, &x0, &x1)) {
    case 0: return pure_listl(0);
    case 1: return pure_listl(1, pure_double(x0));
    case 2: return pure_listl(2, pure_double(x0), pure_double(x1));
  }
}

pure_expr* wrap_gsl_poly_complex_solve_quadratic(double a, double b, double c)
{
  gsl_complex z0, z1;

  if (gsl_poly_complex_solve_quadratic(a, b, c, &z0, &z1) == 1)
    return pure_listl(1, pure_complex(z0.dat));
  else
    return pure_listl(2, pure_complex(z0.dat), pure_complex(z1.dat));
}

pure_expr* wrap_gsl_poly_solve_cubic(double a, double b, double c)
{
  double x0, x1, x2;

  if (gsl_poly_solve_cubic(a, b, c, &x0, &x1, &x2) == 1)
    return pure_listl(1, pure_double(x0));
  else
    return pure_listl(3, pure_double(x0), pure_double(x1), pure_double(x2));
}

pure_expr* wrap_gsl_poly_complex_solve_cubic(double a, double b, double c)
{
  gsl_complex z0, z1, z2;

  gsl_poly_complex_solve_cubic(a, b, c, &z0, &z1, &z2);
  return pure_listl(3, pure_complex(z0.dat), pure_complex(z1.dat), 
		    pure_complex(z2.dat));
}

pure_expr* wrap_gsl_poly_complex_solve(double* a, size_t n)
{
  double tz[2*(n-1)];
  pure_expr *z[n-1];
  double t[2];
  int i, r;
  
  gsl_poly_complex_workspace* w = gsl_poly_complex_workspace_alloc(n);
  r = gsl_poly_complex_solve(a, n, w, tz);
  gsl_poly_complex_workspace_free(w);
  if (r == GSL_SUCCESS) {
    for (i = 0; i < n-1; ++i) {
      t[0] = tz[2*i];
      t[1] = tz[2*i+1];
      z[i] = pure_complex(t);
    }
    return pure_listv(n-1, z);
  } else
    return pure_listl(0);
}

pure_expr* wrap_gsl_fit_linear(double* x, double* y, size_t n)
{
  double c0, c1, cov00, cov01, cov11, sumsq;
  
  gsl_fit_linear(x, 1, y, 1, n, &c0, &c1, &cov00, &cov01, &cov11, &sumsq);
  return pure_listl(6, pure_double(c0), pure_double(c1), pure_double(cov00), 
		    pure_double(cov01), pure_double(cov11),
		    pure_double(sumsq));
}

pure_expr* wrap_gsl_fit_wlinear(double* x, double* w, double* y, size_t n)
{
  double c0, c1, cov00, cov01, cov11, chisq;
  
  gsl_fit_wlinear(x, 1, w, 1, y, 1, n, &c0, &c1, &cov00, &cov01, &cov11, 
		  &chisq);
  return pure_listl(6, pure_double(c0), pure_double(c1), pure_double(cov00),
		    pure_double(cov01), pure_double(cov11), 
		    pure_double(chisq));
}

pure_expr* wrap_gsl_fit_linear_est(double x, double c0, double c1,
  double cov00, double cov01, double cov11)
{
  double y, y_err;
  
  gsl_fit_linear_est(x, c0, c1, cov00, cov01, cov11, &y, &y_err);
  return pure_listl(2, pure_double(y), pure_double(y_err));
}

pure_expr* wrap_gsl_fit_mul(double* x, double* y, size_t n)
{
  double c1, cov11, sumsq;
  
  gsl_fit_mul(x, 1, y, 1, n, &c1, &cov11, &sumsq);
  return pure_listl(3, pure_double(c1), pure_double(cov11), pure_double(sumsq));
}

pure_expr* wrap_gsl_fit_wmul(double* x, double* w, double* y, size_t n)
{
  double c1, cov11, sumsq;
  
  gsl_fit_wmul(x, 1, w, 1, y, 1, n, &c1, &cov11, &sumsq);
  return pure_listl(3, pure_double(c1), pure_double(cov11), pure_double(sumsq));
}

pure_expr* wrap_gsl_fit_mul_est(double x, double c1, double cov11)
{
  double y, y_err;
  
  gsl_fit_mul_est(x, c1, cov11, &y, &y_err);
  return pure_listl(2, pure_double(y), pure_double(y_err));
}

pure_expr* wrap_gsl_multifit_linear(gsl_matrix* X, gsl_matrix* y)
{
  int i;
  double chisq;
  pure_expr *cx[X->size1];
  double *p;

  gsl_vector* c = gsl_vector_alloc(X->size1);
  gsl_vector* yt = gsl_vector_alloc(X->size1);
  gsl_matrix_get_row(yt, y, 0);
  gsl_matrix* cov = gsl_matrix_alloc(X->size1, X->size2);
  gsl_multifit_linear_workspace* w;
  w = gsl_multifit_linear_alloc(X->size1, X->size2);
  gsl_multifit_linear(X, yt, c, cov, &chisq, w);
  gsl_multifit_linear_free(w);
  gsl_vector_free(yt);
  p = c->data;
  for (i = 0; i < X->size1; ++i) {
    cx[i] = pure_double(*p);
    ++p;
  }
  return pure_listl(3, pure_matrix_columnsv(X->size1, cx),
		    pure_double_matrix(cov), pure_double(chisq));
}
