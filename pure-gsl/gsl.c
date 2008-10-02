
#include <gsl/gsl_errno.h>
#include <gsl/gsl_version.h>
#include <gsl/gsl_complex_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>

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
