
#include <gsl/gsl_errno.h>
#include <gsl/gsl_version.h>
#include <gsl/gsl_complex_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>

#include <pure/runtime.h>

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

/* Complex functions. These need to be wrapped, as there's no direct way to
   pass gsl_complex values between Pure and C. */

pure_expr *wrap_complex_sqrt(pure_expr *x)
{
  gsl_complex z;
  if (pure_is_complex(x, z.dat)) {
    gsl_complex ret = gsl_complex_sqrt(z);
    return pure_complex(ret.dat);
  } else
    return 0;
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
