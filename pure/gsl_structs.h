#ifndef GSL_STRUCTS_H
#define GSL_STRUCTS_H

/* GSL-compatible matrix structs used internally to represent various types of
   matrix expressions. These are provided here for interoperability with GSL
   (http://www.gnu.org/software/gsl). */

/* NOTE: The struct definitions below are supposed to be drop-in replacements
   for the corresponding declarations in the GSL headers. To prevent name
   clashes between the GSL and Pure structs, if you need to include both the
   GSL headers and this header in your application, make sure that you include
   the gsl_matrix.h header first. */

#ifndef __GSL_MATRIX_H__

typedef struct _gsl_block
{
  size_t size;
  double *data;
} gsl_block;

typedef struct _gsl_block_complex
{
  size_t size;
  double *data;
} gsl_block_complex;

typedef struct _gsl_block_int
{
  size_t size;
  int *data;
} gsl_block_int;

typedef struct _gsl_matrix
{
  size_t size1;
  size_t size2;
  size_t tda;
  double *data;
  gsl_block *block;
  int owner;
} gsl_matrix;

typedef struct _gsl_matrix_view
{
  gsl_matrix matrix;
} gsl_matrix_view;

typedef struct _gsl_matrix_complex
{
  size_t size1;
  size_t size2;
  size_t tda;
  double *data;
  gsl_block_complex *block;
  int owner;
} gsl_matrix_complex;

typedef struct _gsl_matrix_complex_view
{
  gsl_matrix_complex matrix;
} gsl_matrix_complex_view;

typedef struct _gsl_matrix_int
{
  size_t size1;
  size_t size2;
  size_t tda;
  int *data;
  gsl_block_int *block;
  int owner;
} gsl_matrix_int;

typedef struct _gsl_matrix_int_view
{
  gsl_matrix_int matrix;
} gsl_matrix_int_view;

#endif

typedef struct _gsl_block_symbolic
{
  size_t size;
  pure_expr **data;
} gsl_block_symbolic;

typedef struct _gsl_matrix_symbolic
{
  size_t size1;
  size_t size2;
  size_t tda;
  pure_expr **data;
  gsl_block_symbolic *block;
  int owner;
  void *q; // used internally, do not touch
} gsl_matrix_symbolic;

typedef struct _gsl_matrix_symbolic_view
{
  gsl_matrix_symbolic matrix;
} gsl_matrix_symbolic_view;

#endif // ! GSL_STRUCTS_H
