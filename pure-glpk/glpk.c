
/* glpk.c - Pure bindings for the GNU Linear Programming Kit (GLPK) */

/* Copyright (c) 2009, 2010, 2011, 2012, 2013 by Jiri Spitz
   <jiri.spitz@bluetone.cz>.

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <glpk.h>
#include <gmp.h>
#include <pure/runtime.h>

#ifdef __MINGW32__
#include <malloc.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if GLP_MAJOR_VERSION < 4 || (GLP_MAJOR_VERSION == 4 && GLP_MINOR_VERSION < 42)
#error GLPK version 4.42 or higher required
#endif


static char errormsg[128];

#define pure_void pure_tuplel(0)

#define GLPK_MAGIC  9041 /* the 1123th prime */
#define TRAN_MAGIC  9043 /* the 1124th prime */
#define TREE_MAGIC  9049 /* the 1125th prime */
#define GRAPH_MAGIC 9059 /* the 1126th prime */
#define ARC_MAGIC   9067 /* the 1127th prime */

typedef struct {
  short magic; /* this provides for some type safety */
  glp_prob *lp; /* the GLPK problem object */
} glp_obj;

typedef struct {
  short magic; /* this provides for some type safety */
  glp_tran *tran; /* the MathProg translate workspace */
} tran_obj;

typedef struct {
  short magic; /* this provides for some type safety */
  glp_tree *tree; /* branch and cut search tree */
} tree_obj;

typedef struct {
  short magic; /* this provides for some type safety */
  glp_graph *graph; /* graph */
} graph_obj;

typedef struct {
  short magic; /* this provides for some type safety */
  glp_arc *arc; /* arc */
} arc_obj;

static inline pure_expr* pure_err_internal(const char *msg)
{
  return pure_app(pure_app(pure_symbol(pure_sym("glp::error")),
                           pure_cstring_dup("[Pure GLPK] error")),
                  pure_cstring_dup(msg));
}

static inline bool is_glp_pointer(pure_expr *x, glp_obj **glpobj)
{
  // Check whether a pointer to a GLPK problem object was passed
  return pure_is_pointer(x, (void**)glpobj) && *glpobj &&
    (*glpobj)->magic == GLPK_MAGIC && (*glpobj)->lp;
}

static inline bool is_tran_pointer(pure_expr *x, tran_obj **tranobj)
{
  // Check whether a pointer to a MathProg translator object was passed
  return pure_is_pointer(x, (void**)tranobj) && *tranobj &&
    (*tranobj)->magic == TRAN_MAGIC && (*tranobj)->tran;
}

static inline bool is_tree_pointer(pure_expr *x, tree_obj **treeobj)
{
  // Check whether a pointer to a branch and cut search tree was passed
  return pure_is_pointer(x, (void**)treeobj) && *treeobj &&
    (*treeobj)->magic == TREE_MAGIC && (*treeobj)->tree;
}

static inline bool is_graph_pointer(pure_expr *x, graph_obj **graphobj)
{
  // Check whether a pointer to a graph was passed
  return pure_is_pointer(x, (void**)graphobj) && *graphobj &&
    (*graphobj)->magic == GRAPH_MAGIC && (*graphobj)->graph;
}

static inline bool is_arc_pointer(pure_expr *x, arc_obj **arcobj)
{
  // Check whether a pointer to an arc was passed
  return pure_is_pointer(x, (void**)arcobj) && *arcobj &&
    (*arcobj)->magic == ARC_MAGIC && (*arcobj)->arc;
}

static inline int pure_is_intordouble(pure_expr *x, double *val)
{
  // Get a number, coerce possible long or bigint to double
  int32_t tmp;
  mpz_t z;
  if (pure_is_double(x, val)) {
    return 1;
  }
  if (pure_is_int(x, &tmp)) {
    *val = (double)tmp;
    return 1;
  }
  if (pure_is_mpz(x, &z)) {
    *val = mpz_get_d(z);
    mpz_clear(z);
    return 1;
  }
  return 0;
}

static int pure_is_intlist(pure_expr **elems, const size_t nelem,
                            const int maxi, const char *caller,
                            int *indices)
{
  /* Convert a list of indices into an array with indices. Checks, whether
     indices are in the interval <1, maxi>. The array is supplied and it is
     responsibility of the caller to free it. The array uses 1-based indices
     and the zeroth elements is unused */
  size_t n;
  int i, indval;
  double val;
  indices[0] = 0;
  for (i = 0; i < nelem; i++) {
    if (!pure_is_int(elems[i], &indval)) {
      return 0;
    }
    if (indval < 1 || indval > maxi) {
      sprintf(errormsg, "%s index out of bounds", caller);
      return -1;
    }
    indices[i + 1] = indval;
  }
  return 1;
}

static int pure_is_pairlist(pure_expr **elems, const size_t nelem,
                            const int maxi, const char *caller,
                            int *indices, double *values)
{
  /* Convert a list of pairs (index, value) into 2 arrays - one with indices
     and the other with values. Checks, whether indices are in the interval
     <1, maxi>. The arrays are supplied and it is responsibility of the caller
     to free them. The arrays use 1-based indices and the zeroth elements are
     unused */
  size_t n;
  pure_expr **tpl;
  int i, indval;
  double val;
  indices[0] = 0;
  values[0] = 0.0;
  for (i = 0; i < nelem; i++) {
    if (!pure_is_tuplev(elems[i], &n, &tpl)) {
      return 0;
    }
    if (n != 2 || !pure_is_int(tpl[0], &indval) ||
               !pure_is_intordouble(tpl[1], &val)) {
      free(tpl);
      return 0;
    }
    if (indval < 1 || indval > maxi) {
      free(tpl);
      sprintf(errormsg, "%s index out of bounds", caller);
      return -1;
    }
    indices[i + 1] = indval;
    values[i + 1] = val;
    free(tpl);
  }
  return 1;
}

static int pure_is_intpairlist(pure_expr **elems, const size_t nelem,
                            int *indices1, int *indices2)
{
  /* Convert a list of pairs (index1, index2) into 2 arrays with indices.
     The arrays are supplied and it is responsibility of the caller
     to free them. The arrays use 1-based indices and the zeroth elements are
     unused */
  size_t n;
  pure_expr **tpl;
  int i, ind1, ind2;
  indices1[0] = 0;
  indices2[0] = 0;
  for (i = 0; i < nelem; i++) {
    if (!pure_is_tuplev(elems[i], &n, &tpl)) {
      return 0;
    }
    if (n != 2 || !pure_is_int(tpl[0], &ind1) ||
               !pure_is_int(tpl[1], &ind2)) {
      free(tpl);
      return 0;
    }
    indices1[i + 1] = ind1;
    indices2[i + 1] = ind2;
    free(tpl);
  }
  return 1;
}

static int pure_is_tripletlist(pure_expr **elems, const size_t nelem,
                               const int maxi, const int maxj,
                               int *indi, int *indj, double *values)
{
  /* Convert a list of triplets (indi, indj, value) into 3 arrays - two with
     indices and the third with values. Checks, whether indices are in the
     intervals <1, maxi> respective <1, maxj>. The arrays are malloced and
     it is responsibility of the caller to free them. The arrays use 1-based
     indices and the zeroth elements are unused */
  size_t n;
  pure_expr **tpl;
  int i, indvali, indvalj;
  double val;
  indi[0] = 0;
  indj[0] = 0;
  values[0] = 0.0;
  for (i = 0; i < nelem; i++) {
    if (!pure_is_tuplev(elems[i], &n, &tpl)) {
      return 0;
    }
    if (n != 3 || !pure_is_int(tpl[0], &indvali) ||
               !pure_is_int(tpl[1], &indvalj) ||
               !pure_is_intordouble(tpl[2], &val)) {
      free(tpl);
      return 0;
    }
    if (indvali < 1 || indvali > maxi) {
      free(tpl);
      strcpy(errormsg, "row index out of bounds");
      return -1;
    }
    if (indvalj < 1 || indvalj > maxj) {
      free(tpl);
      strcpy(errormsg, "column index out of bounds");
      return -1;
    }
    indi[i + 1] = indvali;
    indj[i + 1] = indvalj;
    values[i + 1] = val;
    free(tpl);
  }
  return 1;
}

static pure_expr *pure_pairlist(const int cnt, int *indices,
                         double *values)
{
  /* Convert two arrays - one with indices and the other with values
     into a list of pairs (index, value). */
     int i;
     pure_expr *res, **elems;
     if (!(elems = (pure_expr **)malloc(cnt * sizeof(pure_expr*)))) {
       return pure_err_internal("insufficient memory");
     }
     for (i = 0; i < cnt; i++) {
       elems[i] = pure_tuplel(2, pure_int(indices[i + 1]),
                              pure_double(values[i + 1]));
     }
     res = pure_listv((size_t) cnt, elems);
     free(elems);
     return res;
}

static pure_expr *pure_tripletlist(const int cnt, int *indices1,
                            int *indices2, double *values)
{
  /* Convert three arrays - two with indices and the third with values
     into a list of triplets (index1, index2 , value). */
     size_t n;
     int i;
     pure_expr *res, **elems;
     if (!(elems = (pure_expr **)malloc(cnt * sizeof(pure_expr*)))) {
       return pure_err_internal("insufficient memory");
     }
     for (i = 0; i < n; i++) {
       elems[i] = pure_tuplel(3, pure_int(indices1[i + 1]),
                              pure_int(indices2[i + 1]),
                              pure_double(values[i + 1]));
     }
     res = pure_listv((size_t) cnt, elems);
     free(elems);
     return res;
}

pure_expr *glpk_create_prob()
{
  // Create the GLPK problem object
  glp_obj *glpobj;
  glp_prob *lp;
  if (!(glpobj = (glp_obj*) malloc(sizeof(glp_obj)))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(lp = glp_create_prob())) {
    free(glpobj);
    return pure_err_internal("insufficient memory");
  }
  glpobj->magic = GLPK_MAGIC;
  glpobj->lp = lp;
  return pure_sentry(pure_symbol(pure_sym("glp::delete_prob")),
                       pure_pointer(glpobj));
}

pure_expr *glpk_set_prob_name(pure_expr *ptr, const char *name)
{
  // Set the problem name
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (strlen(name) > 255) {
    return pure_err_internal("string too long");
  }
  glp_set_prob_name(glpobj->lp, name);
  return pure_void;
}

pure_expr *glpk_set_obj_name(pure_expr *ptr, const char *name)
{
  // Set objective name
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (strlen(name) > 255) {
    return pure_err_internal("string too long");
  }
  glp_set_obj_name(glpobj->lp, name);
  return pure_void;
}

pure_expr *glpk_set_obj_dir(pure_expr *ptr, int direction)
{
  // Set minimization (GLP_MIN) or maximization (GLP_MAX)
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_set_obj_dir(glpobj->lp, direction);
  return pure_void;
}

pure_expr *glpk_add_rows(pure_expr *ptr, int cnt)
{
  // Add new rows to the problem, return ordinal number of the first row added
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (cnt < 1) {
    return pure_err_internal("non-positive number of rows to add");
  }
  return pure_int(glp_add_rows(glpobj->lp, cnt));
}

pure_expr *glpk_add_cols(pure_expr *ptr, int cnt)
{
  // Add new cols to the problem, return ordinal number of the first col added
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (cnt < 1) {
    return pure_err_internal("non-positive number of columns to add");
  }
  return pure_int(glp_add_cols(glpobj->lp, cnt));
}

pure_expr *glpk_set_row_name(pure_expr *ptr, int rowind, const char *name)
{
  // Set the row name
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  if (strlen(name) > 255) {
    return pure_err_internal("row name too long");
  }
  glp_set_row_name(glpobj->lp, rowind, name);
  return pure_void;
}

pure_expr *glpk_set_col_name(pure_expr *ptr, int colind, const char *name)
{
  // Set the column name
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  if (strlen(name) > 255) {
    return pure_err_internal("column name too long");
  }
  glp_set_col_name(glpobj->lp, colind, name);
  return pure_void;
}

pure_expr *glpk_set_row_bnds(pure_expr *ptr, int rowind, int rowtype,
                             double lb, double ub)
{
  // Set (change) row bounds
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  glp_set_row_bnds(glpobj->lp, rowind, rowtype, lb, ub);
  return pure_void;
}

pure_expr *glpk_set_col_bnds(pure_expr *ptr, int colind, int coltype,
                             double lb, double ub)
{
  // Sset (change) column bounds
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  glp_set_col_bnds(glpobj->lp, colind, coltype, lb, ub);
  return pure_void;
}

pure_expr *glpk_set_obj_coef(pure_expr *ptr, int colind, double coef)
{
  // Set (change) objective coefficient or constant term
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 0 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  glp_set_obj_coef(glpobj->lp, colind, coef);
  return pure_void;
}

pure_expr *glpk_set_mat_row(pure_expr *ptr, int rowind, pure_expr *row)
{
  // Load or replace matrix row
  int numcols, *indices;
  double *values;
  glp_obj *glpobj;
  pure_expr **elems;
  size_t nelem;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 || rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  if (!pure_is_listv(row, &nelem, &elems) || nelem == 0) {
    return 0;
  }
  numcols = glp_get_num_cols(glpobj->lp);
  if (!(indices = malloc((nelem + 1) * sizeof(int)))) {
    free(elems);
    return pure_err_internal("insufficient memory");
  }
  if (!(values = malloc((nelem + 1) * sizeof(double)))) {
    free(elems);
    free(indices);
    return pure_err_internal("insufficient memory");
  }
  switch (pure_is_pairlist(elems, nelem, numcols, "column", indices, values)) {
  case -1:
    free(indices);
    free(values);
    free(elems);
    return pure_err_internal(errormsg);
  case 0:
    free(indices);
    free(values);
    free(elems);
    return 0;
  case 1:
    glp_set_mat_row(glpobj->lp, rowind, nelem, indices, values);
    free(values);
    free(indices);
    free(elems);
    return pure_void;    
  }
  return pure_err_internal("internal error - please report");
}

pure_expr *glpk_set_mat_col(pure_expr *ptr, int colind, pure_expr *col)
{
  // Load or replace matrix column
  int numrows, *indices;
  double *values;
  glp_obj *glpobj;
  pure_expr **elems;
  size_t nelem;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 || colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  if (!pure_is_listv(col, &nelem, &elems) || nelem == 0) {
    return 0;
  }
  numrows = glp_get_num_rows(glpobj->lp);
  if (!(indices = malloc((nelem + 1) * sizeof(int)))) {
    free(elems);
    return pure_err_internal("insufficient memory");
  }
  if (!(values = malloc((nelem + 1) * sizeof(double)))) {
    free(elems);
    free(indices);
    return pure_err_internal("insufficient memory");
  }
  switch (pure_is_pairlist(elems, nelem, numrows, "row", indices, values)) {
  case -1:
    free(indices);
    free(values);
    free(elems);
    return pure_err_internal(errormsg);
  case 0:
    free(indices);
    free(values);
    free(elems);
    return 0;
  case 1:
    glp_set_mat_col(glpobj->lp, colind, nelem, indices, values);
    free(values);
    free(indices);
    free(elems);
    return pure_void;
  }
  return pure_err_internal("internal error - please report");
}

pure_expr *glpk_load_matrix(pure_expr *ptr, pure_expr *matrix)
{
  // Load or replace the whole problem matrix
  int numrows, numcols, *irow, *icol;
  double *values;
  glp_obj *glpobj;
  pure_expr **elems;
  size_t nelem;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!pure_is_listv(matrix, &nelem, &elems) || nelem == 0) {
    return 0;
  }
  if (!(irow = malloc((nelem + 1) * sizeof(int)))) {
    free(elems);
    return pure_err_internal("insufficient memory");
  };
  if (!(icol = malloc((nelem + 1) * sizeof(int)))) {
    free(elems);
    free(irow);
    return pure_err_internal("insufficient memory");
  };
  if (!(values = malloc((nelem + 1) * sizeof(double)))) {
    free(elems);
    free(irow);
    free(icol);
    return pure_err_internal("insufficient memory");
  };
  numrows = glp_get_num_rows(glpobj->lp);
  numcols = glp_get_num_cols(glpobj->lp);
  switch (pure_is_tripletlist(elems, nelem, numrows, numcols,
          irow, icol, values)) {
  case -1:
    free(irow);
    free(icol);
    free(values);
    free(elems);
    return pure_err_internal(errormsg);
  case 0:
    free(irow);
    free(icol);
    free(values);
    free(elems);
    return 0;
  case 1:
    glp_load_matrix(glpobj->lp, nelem, irow, icol, values);
    free(irow);
    free(icol);
    free(values);
    free(elems);
    return pure_void;
  }
  return pure_err_internal("internal error - please report");
}

pure_expr *glpk_check_dup(int numrows, int numcols, pure_expr *indices)
{
  // Check for duplicate elements in sparse matrix
  int *irow, *icol, result;
  pure_expr **elems;
  size_t nelem;
  if (!pure_is_listv(indices, &nelem, &elems)) {
    return 0;
  }
  if (nelem == 0) {
    free(elems);
    return pure_int(0);
  }
  if (!(irow = malloc((nelem + 1) * sizeof(int)))) {
    free(elems);
    return pure_err_internal("insufficient memory");
  };
  if (!(icol = malloc((nelem + 1) * sizeof(int)))) {
    free(elems);
    free(irow);
    return pure_err_internal("insufficient memory");
  };
  switch (pure_is_intpairlist(elems, nelem, irow, icol)) {
  case 0:
    free(irow);
    free(icol);
    free(elems);
    return 0;
  case 1:
    result = glp_check_dup(numcols, numrows, nelem, irow, icol);
    free(irow);
    free(icol);
    free(elems);
    return pure_int(result);
  }
  return pure_err_internal("internal error - please report");
}

pure_expr *glpk_sort_matrix(pure_expr *ptr)
{
  // Sort elements of the constraint matrix
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_sort_matrix(glpobj->lp);
  return pure_void;
}

pure_expr *glpk_del_rows(pure_expr *ptr, pure_expr *rows)
{
  // Delete rows from the matrix
  int numrows, *indices;
  glp_obj *glpobj;
  pure_expr **elems;
  size_t nelem;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!pure_is_listv(rows, &nelem, &elems) || nelem == 0) {
    return 0;
  }
  numrows = glp_get_num_rows(glpobj->lp);
  if (!(indices = malloc((nelem + 1) * sizeof(int)))) {
    free(elems);
    return pure_err_internal("insufficient memory");
  }
  switch (pure_is_intlist(elems, nelem, numrows, "row", indices)) {
  case -1:
    free(indices);
    free(elems);
    return pure_err_internal(errormsg);
  case 0:
    free(indices);
    free(elems);
    return 0;
  case 1:
    glp_del_rows(glpobj->lp, nelem, indices);
    free(indices);
    free(elems);
    return pure_void;
  }
  return pure_err_internal("internal error - please report");
}

pure_expr *glpk_del_cols(pure_expr *ptr, pure_expr *rows)
{
  // Delete columns from the matrix
  int numcols, *indices;
  glp_obj *glpobj;
  pure_expr **elems;
  size_t nelem;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!pure_is_listv(rows, &nelem, &elems) || nelem == 0) {
    return 0;
  }
  numcols = glp_get_num_cols(glpobj->lp);
  if (!(indices = malloc((nelem + 1) * sizeof(int)))) {
    free(elems);
    return pure_err_internal("insufficient memory");
  }
  switch (pure_is_intlist(elems, nelem, numcols, "column", indices)) {
  case -1:
    free(indices);
    free(elems);
    return pure_err_internal(errormsg);
  case 0:
    free(indices);
    free(elems);
    return 0;
  case 1:
    glp_del_cols(glpobj->lp, nelem, indices);
    free(indices);
    free(elems);
    return pure_void;
  }
  return pure_err_internal("internal error - please report");
}

pure_expr *glpk_copy_prob(pure_expr *dest, pure_expr *src, const int names)
{
  // Copy the whole content of an GLPK problem object to another one
  glp_obj *glpdest, *glpsrc;
  if (!is_glp_pointer(dest, &glpdest) || !is_glp_pointer(src, &glpsrc)) {
    return 0;
  }
  glp_copy_prob(glpdest->lp, glpsrc->lp, names);
  return pure_void;
}

pure_expr *glpk_erase_prob(pure_expr *ptr)
{
  // Erase all data from the GLPK problem object
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_erase_prob(glpobj->lp);
  return pure_void;
}

pure_expr *glpk_delete_prob(pure_expr *ptr)
{
  // Delete the GLPK problem object
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_delete_prob(glpobj->lp);
  free(glpobj);
  ptr->data.p = NULL;
  return pure_void;
}

pure_expr *glpk_get_prob_name(pure_expr *ptr)
{
  // Get the problem name
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_cstring_dup((char *) glp_get_prob_name(glpobj->lp));
}

pure_expr *glpk_get_obj_name(pure_expr *ptr)
{
  // Get the objective name
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_cstring_dup((char *) glp_get_obj_name(glpobj->lp));
}

pure_expr *glpk_get_obj_dir(pure_expr *ptr)
{
  // Get minimization (GLP_MIN) or maximization (GLP_MAX)
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_get_obj_dir(glpobj->lp));
}

pure_expr *glpk_get_num_rows(pure_expr *ptr)
{
  // Get number of rows
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_get_num_rows(glpobj->lp));  
}

pure_expr *glpk_get_num_cols(pure_expr *ptr)
{
  // Get number of columns
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_get_num_cols(glpobj->lp));
}

pure_expr *glpk_get_row_name(pure_expr *ptr, int rowind)
{
  // Get name of a row
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  return pure_cstring_dup(glp_get_row_name(glpobj->lp, rowind));
}

pure_expr *glpk_get_col_name(pure_expr *ptr, int colind)
{
  // Get name of a column
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_cstring_dup(glp_get_col_name(glpobj->lp, colind));
}

pure_expr *glpk_get_row_type(pure_expr *ptr, int rowind)
{
  // Get row type
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  return pure_int(glp_get_row_type(glpobj->lp, rowind));
}

pure_expr *glpk_get_row_lb(pure_expr *ptr, int rowind)
{
  // Get row lower bound
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  return pure_double(glp_get_row_lb(glpobj->lp, rowind));
}

pure_expr *glpk_get_row_ub(pure_expr *ptr, int rowind)
{
  // Get row upper bound
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  return pure_double(glp_get_row_ub(glpobj->lp, rowind));
}

pure_expr *glpk_get_col_type(pure_expr *ptr, int colind)
{
  // Get column type
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_int(glp_get_col_type(glpobj->lp, colind));
}

pure_expr *glpk_get_col_lb(pure_expr *ptr, int colind)
{
  // Get column lower bound
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_double(glp_get_col_lb(glpobj->lp, colind));
}

pure_expr *glpk_get_col_ub(pure_expr *ptr, int colind)
{
  // Get column upper bound
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_double(glp_get_col_ub(glpobj->lp, colind));
}

pure_expr *glpk_get_obj_coef(pure_expr *ptr, int colind)
{
  // Get objective coefficient
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_double(glp_get_obj_coef(glpobj->lp, colind));
}

pure_expr *glpk_get_num_nz(pure_expr *ptr)
{
  // Get number of nonzero coefficients
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_get_num_nz(glpobj->lp));
}

pure_expr *glpk_get_mat_row(pure_expr *ptr, int rowind)
{
  // Retrive a row from the problem matrix
  glp_obj *glpobj;
  int *indices;
  size_t cnt;
  double *values;
  pure_expr *res;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  cnt = glp_get_mat_row(glpobj->lp, rowind, NULL, NULL);
  if (!(indices = malloc((cnt + 1) * sizeof(int)))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(values = malloc((cnt + 1) * sizeof(double)))) {
    free(indices);
    return pure_err_internal("insufficient memory");
  }
  cnt = glp_get_mat_row(glpobj->lp, rowind, indices, values);
  res = pure_pairlist(cnt, indices, values);
  free(indices);
  free(values);
  return res;
}

pure_expr *glpk_get_mat_col(pure_expr *ptr, int colind)
{
  // Retrive a column from the problem matrix
  glp_obj *glpobj;
  int *indices;
  size_t cnt;
  double *values;
  pure_expr *res;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  cnt = glp_get_mat_col(glpobj->lp, colind, NULL, NULL);
  if (!(indices = malloc((cnt + 1) * sizeof(int)))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(values = malloc((cnt + 1) * sizeof(double)))) {
    free(indices);
    return pure_err_internal("insufficient memory");
  }
  cnt = glp_get_mat_col(glpobj->lp, colind, indices, values);
  res = pure_pairlist(cnt, indices, values);
  free(indices);
  free(values);
  return res;
}

pure_expr *glpk_create_index(pure_expr *ptr)
{
  // Create index for searching rows and columns by their names
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_create_index(glpobj->lp);
  return pure_void;
}

pure_expr *glpk_find_row(pure_expr *ptr, const char *name)
{
  // Find a row number by name
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_create_index(glpobj->lp);
  return pure_int(glp_find_row(glpobj->lp, name));
}

pure_expr *glpk_find_col(pure_expr *ptr, const char *name)
{
  // Find a column number by name
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_create_index(glpobj->lp);
  return pure_int(glp_find_col(glpobj->lp, name));
}

pure_expr *glpk_delete_index(pure_expr *ptr)
{
  // Delete index for searching rows and columns by their names
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_delete_index(glpobj->lp);
  return pure_void;
}

pure_expr *glpk_set_rii(pure_expr *ptr, pure_expr *tpl)
{
  // Set the row scale factor
  glp_obj *glpobj;
  int rowind;
  double scale;
  size_t nelem;
  pure_expr **elems;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!pure_is_tuplev(tpl, &nelem, &elems)) {
    return 0;
  }
  if (nelem != 2 || !pure_is_int(elems[0], &rowind) ||
      !pure_is_intordouble(elems[1], &scale)) {
    free(elems);
    return 0;
  }
  free(elems);
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  glp_set_rii(glpobj->lp, rowind, scale);
  return pure_void;
}

pure_expr *glpk_set_sjj(pure_expr *ptr, pure_expr *tpl)
{
  // Set the column scale factor
  glp_obj *glpobj;
  int colind;
  double scale;
  size_t nelem;
  pure_expr **elems;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!pure_is_tuplev(tpl, &nelem, &elems)) {
    return 0;
  }
  if (nelem != 2 || !pure_is_int(elems[0], &colind) ||
      !pure_is_intordouble(elems[1], &scale)) {
    free(elems);
    return 0;
  }
  free(elems);
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  glp_set_sjj(glpobj->lp, colind, scale);
  return pure_void;
}

pure_expr *glpk_get_rii(pure_expr *ptr, int rowind)
{
  // Retrieve the row scale factor
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  return pure_double(glp_get_rii(glpobj->lp, rowind));
}

pure_expr *glpk_get_sjj(pure_expr *ptr, int colind)
{
  // Retrieve the column scale factor
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_double(glp_get_sjj(glpobj->lp, colind));
}

pure_expr *glpk_scale_prob(pure_expr *ptr, int flags)
{
  // Scale the problem data according to supplied flags
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_scale_prob(glpobj->lp, flags);
  return pure_void;
}

pure_expr *glpk_unscale_prob(pure_expr *ptr)
{
  // Unscale the problem data
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_unscale_prob(glpobj->lp);
  return pure_void;
}

pure_expr *glpk_set_row_stat(pure_expr *ptr, int rowind, int status)
{
  // Set the row status
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  glp_set_row_stat(glpobj->lp, rowind, status);
  return pure_void;
}

pure_expr *glpk_set_col_stat(pure_expr *ptr, int colind, int status)
{
  // Set the column status
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  glp_set_col_stat(glpobj->lp, colind, status);
  return pure_void;
}

pure_expr *glpk_std_basis(pure_expr *ptr)
{
  // Construct standard problem basis
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_std_basis(glpobj->lp);
  return pure_void;
}

pure_expr *glpk_adv_basis(pure_expr *ptr)
{
  // Construct advanced problem basis
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_adv_basis(glpobj->lp, 0);
  return pure_void;
}

pure_expr *glpk_cpx_basis(pure_expr *ptr)
{
  // Construct Bixby's problem basis
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  glp_cpx_basis(glpobj->lp);
  return pure_void;
}

static pure_expr *get_spx_parm(pure_expr *parms, glp_smcp *parm, int *cnterr)
{
  // Read the option list for glp_simplex
  pure_expr **list, **tpl, *snd, *res;
  int32_t fst, intparm;
  double doubleparm;
  int i;
  size_t it, cnt;
  *cnterr = 0;
  if (!pure_is_listv(parms, &cnt, &list)) {
    *cnterr = 1;
    return 0;
  }
  for (i = 0; i < cnt; i++) {
    if (!pure_is_tuplev(list[i], &it, &tpl)) {
      list[*cnterr++] = list[i];
      goto err;
    }
    if (it != 2) {
      free(tpl);
      list[*cnterr++] = list[i];
      goto err;
    }
    if (!pure_is_symbol(tpl[0], &fst)) {
      free(tpl);
      list[*cnterr++] = list[i];
      goto err;
    }
    snd = tpl[1];
    free(tpl);
    if (fst == pure_getsym("glp::msg_lev")) {
      if (pure_is_int(snd, &intparm)) parm->msg_lev = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::meth")) {
      if (pure_is_int(snd, &intparm)) parm->meth = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::pricing")) {
      if (pure_is_int(snd, &intparm)) parm->pricing = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::r_test")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->r_test = doubleparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::tol_bnd")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->tol_bnd = doubleparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::tol_dj")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->tol_dj = doubleparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::tol_piv")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->tol_piv = doubleparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::obj_ll")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->obj_ll = doubleparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::obj_ul")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->obj_ul = doubleparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::it_lim")) {
      if (pure_is_int(snd, &intparm)) parm->it_lim = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::tm_lim")) {
      if (pure_is_int(snd, &intparm)) parm->tm_lim = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::out_frq")) {
      if (pure_is_int(snd, &intparm)) parm->out_frq = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::out_dly")) {
      if (pure_is_int(snd, &intparm)) parm->out_dly = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::presolve")) {
      if (pure_is_int(snd, &intparm)) parm->presolve = intparm;
      else list[*cnterr++] = list[i];
    }
    else {
      list[*cnterr++] = list[i];
    }
  err: ;
  }
  if (*cnterr > 0) {
    res = pure_listv((size_t)*cnterr, list);
  }
  else {
    res = 0;
  }
  free(list);
  return res;
}

pure_expr *glpk_simplex(pure_expr *ptr, pure_expr *params)
{
  // Solve the LP problem using simplex method
  glp_smcp *parm;
  pure_expr *ret, *res;
  int cnterr;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!(parm = (glp_smcp *)malloc(sizeof(glp_smcp)))) {
    return pure_err_internal("insufficient memory");
  }
  glp_init_smcp(parm);
  ret = get_spx_parm(params, parm, &cnterr);
  if (cnterr == 0) {
    res = pure_int(glp_simplex(glpobj->lp, parm));
  }
  else {
    res = ret;
  }
  free(parm);
  return res;
}

pure_expr *glpk_exact(pure_expr *ptr, pure_expr * params)
{
  // Solve the LP problem using simplex method in exact arithmetics
  glp_smcp *parm;
  pure_expr *ret, *res;
  int cnterr;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!(parm = (glp_smcp *)malloc(sizeof(glp_smcp)))) {
    return pure_err_internal("insufficient memory");
  }
  glp_init_smcp(parm);
  ret = get_spx_parm(params, parm, &cnterr);
  if (cnterr == 0) {
    res = pure_int(glp_exact(glpobj->lp, parm));
  }
  else {
    res = ret;
  }
  free(parm);
  return res;
}

pure_expr *glpk_get_status(pure_expr *ptr)
{
  // Retrieve generic status of basic solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_get_status(glpobj->lp));
}

pure_expr *glpk_get_prim_stat(pure_expr *ptr)
{
  // Retrieve generic status of primal solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_get_prim_stat(glpobj->lp));
}

pure_expr *glpk_get_dual_stat(pure_expr *ptr)
{
  // Retrieve generic status of dual solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_get_dual_stat(glpobj->lp));
}

pure_expr *glpk_get_obj_val(pure_expr *ptr)
{
  // Retrieve value of the objective function
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_double(glp_get_obj_val(glpobj->lp));
}

pure_expr *glpk_get_row_stat(pure_expr *ptr, int rowind)
{
  // Retrieve generic status of a row variable
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  return pure_int(glp_get_row_stat(glpobj->lp, rowind));
}

pure_expr *glpk_get_row_prim(pure_expr *ptr, int rowind)
{
  // Retrieve row primal value
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  return pure_double(glp_get_row_prim(glpobj->lp, rowind));
}

pure_expr *glpk_get_row_dual(pure_expr *ptr, int rowind)
{
  // Retrieve row dual value
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  return pure_double(glp_get_row_dual(glpobj->lp, rowind));
}

pure_expr *glpk_get_col_stat(pure_expr *ptr, int colind)
{
  // Retrieve generic status of a column variable
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_int(glp_get_col_stat(glpobj->lp, colind));
}

pure_expr *glpk_get_col_prim(pure_expr *ptr, int colind)
{
  // Retrieve column primal value
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_double(glp_get_col_prim(glpobj->lp, colind));
}

pure_expr *glpk_get_col_dual(pure_expr *ptr, int colind)
{
  // Retrieve column dual value
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_double(glp_get_col_dual(glpobj->lp, colind));
}

pure_expr *glpk_get_unbnd_ray(pure_expr *ptr)
{
  // Determine variable causing unboundedness
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_get_unbnd_ray(glpobj->lp));
}

static pure_expr *get_ipt_parm(pure_expr *parms, glp_iptcp *parm, int *cnterr)
{
  // Read the option list for glp_interior
  pure_expr **list, **tpl, *snd, *res;
  int32_t fst, intparm;
  double doubleparm;
  int i;
  size_t it, cnt;
  *cnterr = 0;
  if (!pure_is_listv(parms, &cnt, &list)) {
    *cnterr = 1;
    return 0;
  }
  for (i = 0; i < cnt; i++) {
    if (!pure_is_tuplev(list[i], &it, &tpl)) {
      list[*cnterr++] = list[i];
      goto err;
    }
    if (it != 2) {
      free(tpl);
      list[*cnterr++] = list[i];
      goto err;
    }
    if (!pure_is_symbol(tpl[0], &fst)) {
      free(tpl);
      list[*cnterr++] = list[i];
      goto err;
    }
    snd = tpl[1];
    free(tpl);
    if (fst == pure_getsym("glp::msg_lev")) {
      if (pure_is_int(snd, &intparm)) parm->msg_lev = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::ord_alg")) {
      if (pure_is_int(snd, &intparm)) {parm->ord_alg = intparm;
      printf("%d\n", intparm);}
      else list[*cnterr++] = list[i];
    }
    else {
      list[*cnterr++] = list[i];
    }
  err: ;
  }
  if (*cnterr > 0) {
    res = pure_listv((size_t)*cnterr, list);
  }
  else {
    res = 0;
  }
  free(list);
  return res;
}

pure_expr *glpk_interior(pure_expr *ptr, pure_expr *params)
{
  // Solve the LP problem using interior-point method
  glp_iptcp *parm;
  pure_expr *ret, *res;
  int cnterr;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!(parm = (glp_iptcp *)malloc(sizeof(glp_iptcp)))) {
    return pure_err_internal("insufficient memory");
  }
  glp_init_iptcp(parm);
  ret = get_ipt_parm(params, parm, &cnterr);
  if (cnterr == 0) {
    res = pure_int(glp_interior(glpobj->lp, parm));
  }
  else {
    res = ret;
  }
  free(parm);
  return res;
}

pure_expr *glpk_ipt_status(pure_expr *ptr)
{
  // Retrieve status of interior-point solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_ipt_status(glpobj->lp));
}

pure_expr *glpk_ipt_obj_val(pure_expr *ptr)
{
  // Retrieve the objective function value of interior-point solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_double(glp_ipt_obj_val(glpobj->lp));
}

pure_expr *glpk_ipt_row_prim(pure_expr *ptr, int rowind)
{
  // Retrieve row primal value of interior-point solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  return pure_double(glp_ipt_row_prim(glpobj->lp, rowind));
}

pure_expr *glpk_ipt_row_dual(pure_expr *ptr, int rowind)
{
  // Retrieve row dual value of interior-point solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  return pure_double(glp_ipt_row_dual(glpobj->lp, rowind));
}

pure_expr *glpk_ipt_col_prim(pure_expr *ptr, int colind)
{
  // Retrieve column primal value of interior-point solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_double(glp_ipt_col_prim(glpobj->lp, colind));
}

pure_expr *glpk_ipt_col_dual(pure_expr *ptr, int colind)
{
  // Retrieve column dual value of interior-point solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_double(glp_ipt_col_dual(glpobj->lp, colind));
}

pure_expr *glpk_set_col_kind(pure_expr *ptr, int colind, int kind)
{
  // Set column kind
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  glp_set_col_kind(glpobj->lp, colind, kind);
  return pure_void;
}

pure_expr *glpk_get_col_kind(pure_expr *ptr, int colind)
{
  // Retrieve column kind
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_int(glp_get_col_kind(glpobj->lp, colind));
}

pure_expr *glpk_get_num_int(pure_expr *ptr)
{
  // Retrieve number of integer columns
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_get_num_int(glpobj->lp));
}

pure_expr *glpk_get_num_bin(pure_expr *ptr)
{
  // Retrieve number of binary columns
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_get_num_bin(glpobj->lp));
}

static void mip_callback(glp_tree *tree, void *info)
{
  // Callback routine for the branch-and-cut algorithm
  pure_expr *res;
  tree_obj *treeobj;
  if (!(treeobj = (tree_obj *)malloc(sizeof(tree_obj)))) {
    return;
  }
  treeobj->magic = TREE_MAGIC;
  treeobj->tree = tree;
  res = pure_app(pure_app(pure_symbol(pure_sym("glp::mip_cb")),
                                      pure_pointer(treeobj)),
                                      pure_pointer(info));
  pure_freenew(res);
  free(treeobj);
  return;
}

static pure_expr *get_mip_parm(pure_expr *parms, glp_iocp *parm, int *cnterr)
{
  // Read the option list for glp_simplex
  pure_expr **list, **tpl, *snd, *res;
  int32_t fst, intparm;
  double doubleparm;
  void *ptrparm;
  int i;
  size_t it, cnt;
  *cnterr = 0;
  if (!pure_is_listv(parms, &cnt, &list)) {
    *cnterr = 1;
    return 0;
  }
  for (i = 0; i < cnt; i++) {
    if (!pure_is_tuplev(list[i], &it, &tpl)) {
      list[*cnterr++] = list[i];
      goto err;
    }
    if (it != 2) {
      free(tpl);
      list[*cnterr++] = list[i];
      goto err;
    }
    if (!pure_is_symbol(tpl[0], &fst)) {
      free(tpl);
      list[*cnterr++] = list[i];
      goto err;
    }
    snd = tpl[1];
    free(tpl);
    if (fst == pure_getsym("glp::msg_lev")) {
      if (pure_is_int(snd, &intparm)) parm->msg_lev = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::br_tech")) {
      if (pure_is_int(snd, &intparm)) parm->br_tech = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::bt_tech")) {
      if (pure_is_int(snd, &intparm)) parm->bt_tech = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::tol_int")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->tol_int = doubleparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::tol_obj")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->tol_obj = doubleparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::tm_lim")) {
      if (pure_is_int(snd, &intparm)) parm->tm_lim = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::out_frq")) {
      if (pure_is_int(snd, &intparm)) parm->out_frq = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::out_dly")) {
      if (pure_is_int(snd, &intparm)) parm->out_dly = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::cb_func")) {
      if (pure_is_int(snd, &intparm)) parm->cb_func =
	                                intparm ? mip_callback : NULL;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::cb_info")) {
      if (pure_is_pointer(snd, &ptrparm)) parm->cb_info = (void *)ptrparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::cb_size")) {
      if (pure_is_int(snd, &intparm)) parm->cb_size = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::pp_tech")) {
      if (pure_is_int(snd, &intparm)) parm->pp_tech = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::fp_heur")) {
      if (pure_is_int(snd, &intparm)) parm->fp_heur = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::mip_gap")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->mip_gap = doubleparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::mir_cuts")) {
      if (pure_is_int(snd, &intparm)) parm->mir_cuts = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::gmi_cuts")) {
      if (pure_is_int(snd, &intparm)) parm->gmi_cuts = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::cov_cuts")) {
      if (pure_is_int(snd, &intparm)) parm->cov_cuts = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::clq_cuts")) {
      if (pure_is_int(snd, &intparm)) parm->clq_cuts = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::presolve")) {
      if (pure_is_int(snd, &intparm)) parm->presolve = intparm;
      else list[*cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::binarize")) {
      if (pure_is_int(snd, &intparm)) parm->binarize = intparm;
      else list[*cnterr++] = list[i];
    }
    else {
      list[*cnterr++] = list[i];
    }
  err: ;
  }
  if (*cnterr > 0) {
    res = pure_listv((size_t)*cnterr, list);
  }
  else {
    res = 0;
  }
  free(list);
  return res;
}

pure_expr *glpk_intopt(pure_expr *ptr, pure_expr *params)
{
  // Solve the MIP problem using branch-and-cut method
  glp_iocp *parm;
  pure_expr *ret, *res;
  int cnterr;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!(parm = (glp_iocp *)malloc(sizeof(glp_smcp)))) {
    return pure_err_internal("insufficient memory");
  }
  glp_init_iocp(parm);
  ret = get_mip_parm(params, parm, &cnterr);
  if (cnterr == 0) {
    res = pure_int(glp_intopt(glpobj->lp, parm));
  }
  else {
    res = ret;
  }
  free(parm);
  return res;
}

pure_expr *glpk_mip_status(pure_expr *ptr)
{
  // Retrieve status of mip solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_mip_status(glpobj->lp));
}

pure_expr *glpk_mip_obj_val(pure_expr *ptr)
{
  // Retrieve the objective function value of mip solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_double(glp_mip_obj_val(glpobj->lp));
}

pure_expr *glpk_mip_row_val(pure_expr *ptr, int rowind)
{
  // Retrieve row value of mip solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  return pure_double(glp_mip_row_val(glpobj->lp, rowind));
}

pure_expr *glpk_mip_col_val(pure_expr *ptr, int colind)
{
  // Retrieve column value of mip solution
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_double(glp_mip_col_val(glpobj->lp, colind));
}

static inline pure_expr *kkt_quality(int quality)
{
  // Convert char codes to Pure strings
  char c[2];
  c[0] = quality;
  c[1] = 0;
  return pure_cstring_dup(c);
}

#if GLP_MAJOR_VERSION == 4 && GLP_MINOR_VERSION <= 48
pure_expr *glpx_check_kkt(pure_expr *ptr, int scaled)
{
  // Check Karush-Kuhn-Tucker conditions
  LPXKKT *kkt;
  pure_expr *res;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (glp_get_status(glpobj->lp) != GLP_OPT) {
    return pure_err_internal("no optimal solution");
  }
  kkt = (LPXKKT *)malloc(sizeof(LPXKKT));
  if (!kkt) {
    return pure_err_internal("insufficient memory");
  }
  lpx_check_kkt(glpobj->lp, scaled, kkt);
  res = pure_listl(4, pure_tuplel(5, pure_double(kkt->pe_ae_max),
                                     pure_int(kkt->pe_ae_row),
			             pure_double(kkt->pe_re_max),
				     pure_int(kkt->pe_re_row),
				     kkt_quality(kkt->pe_quality)),
		      pure_tuplel(5, pure_double(kkt->pb_ae_max),
                                     pure_int(kkt->pb_ae_ind),
			             pure_double(kkt->pb_re_max),
				     pure_int(kkt->pb_re_ind),
				     kkt_quality(kkt->pb_quality)),
                      pure_tuplel(5, pure_double(kkt->de_ae_max),
                                     pure_int(kkt->de_ae_col),
				     pure_double(kkt->de_re_max),
				     pure_int(kkt->de_re_col),
				     kkt_quality(kkt->de_quality)),
		      pure_tuplel(5, pure_double(kkt->db_ae_max),
                                     pure_int(kkt->db_ae_ind),
				     pure_double(kkt->db_re_max),
				     pure_int(kkt->db_re_ind),
				     kkt_quality(kkt->db_quality)));
  free(kkt);
  if (res)
    return res;
  else {
    return pure_err_internal("insufficient memory");
  }
}
#else
pure_expr *glpx_check_kkt(pure_expr *ptr, int scaled)
{
  return pure_err_internal("function lpx::check_kkt is not available for GLPK version > 4.48");
}
#endif

#if GLP_MAJOR_VERSION > 4 || GLP_MINOR_VERSION > 48
pure_expr *glpk_check_kkt(pure_expr *ptr, int sol, int cond)
{
  double ae_max, re_max;
  int ae_ind, re_ind;
  pure_expr *res;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (sol == GLP_MIP && (cond == GLP_KKT_DE || cond == GLP_KKT_DB)) {
    return pure_err_internal("dual KKT check is not available for MIP");
  }
  if (cond == GLP_KKT_CS) {
    return pure_err_internal("KKT complementary slackness check is not yet implemented");
  }
  glp_check_kkt(glpobj->lp, sol, cond, &ae_max, &ae_ind, &re_max, &re_ind);
  res = pure_tuplel(4, pure_double(ae_max),
		       pure_int(ae_ind),
		       pure_double(re_max),
		       pure_int(re_ind));
  if (res)
    return res;
  else
    return pure_err_internal("insufficient memory");
}
#else
pure_expr *glpk_check_kkt(pure_expr *ptr, int sol, int cond)
{
  return pure_err_internal("function glp::check_kkt is not available for GLPK version < 4.49");
}
#endif

pure_expr *glpk_read_mps(pure_expr *ptr,  int fmt, const char *fname)
{
  // Read LP problem data from a MPS file
  int status;
  char *oldlocale;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_read_mps(glpobj->lp, fmt, NULL, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_write_mps(pure_expr *ptr, int fmt, const char *fname)
{
  // Write LP problem data into a MPS file
  int status;
  char *oldlocale;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_write_mps(glpobj->lp, fmt, NULL, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_read_lp(pure_expr *ptr, const char *fname)
{
  // Read LP problem data from a CPLEX file
  int status;
  char *oldlocale;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_read_lp(glpobj->lp, NULL, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_write_lp(pure_expr *ptr, const char *fname)
{
  // Write LP problem data into a CPLEX file
  int status;
  char *oldlocale;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_write_lp(glpobj->lp, NULL, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_read_prob(pure_expr *ptr, const char *fname)
{
  // Read LP problem data in GLPK format
  int status;
  char *oldlocale;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_read_prob(glpobj->lp, 0, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_write_prob(pure_expr *ptr, const char *fname)
{
  // Write LP problem data in GLPK format
  int status;
  char *oldlocale;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_write_prob(glpobj->lp, 0, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_mpl_alloc_wksp()
{
  // Create the MathProg translator object
  tran_obj *tranobj;
  glp_tran *tran;
  if (!(tranobj = (tran_obj*) malloc(sizeof(tran_obj)))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(tran = glp_mpl_alloc_wksp())) {
    free(tranobj);
    return pure_err_internal("insufficient memory");
  }
  tranobj->magic = TRAN_MAGIC;
  tranobj->tran = tran;
  return pure_sentry(pure_symbol(pure_sym("glp::mpl_free_wksp")),
                       pure_pointer(tranobj));
}

pure_expr *glpk_mpl_read_model(pure_expr *ptr, const char *fname, int skip)
{
  // Read and translate model section
  int status;
  char *oldlocale;
  tran_obj *tranobj;
  if (!is_tran_pointer(ptr, &tranobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_mpl_read_model(tranobj->tran, fname, skip);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_mpl_read_data(pure_expr *ptr, const char *fname)
{
  // Read and translate data section
  int status;
  char *oldlocale;
  tran_obj *tranobj;
  if (!is_tran_pointer(ptr, &tranobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_mpl_read_data(tranobj->tran, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_mpl_generate(pure_expr *ptr, const char *fname)
{
  // Generate the model
  int status;
  char *oldlocale;
  tran_obj *tranobj;
  if (!is_tran_pointer(ptr, &tranobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_mpl_generate(tranobj->tran, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_mpl_build_prob(pure_expr *ptr_tran, pure_expr *ptr_prob)
{
  // Build problem instance from the model
  int status;
  tran_obj *tranobj;
  glp_obj *glpobj;
  if (!is_tran_pointer(ptr_tran, &tranobj)) {
    return 0;
  }
  if (!is_glp_pointer(ptr_prob, &glpobj)) {
    return 0;
  }
  glp_mpl_build_prob(tranobj->tran, glpobj->lp);
  return pure_void;
}

pure_expr *glpk_mpl_postsolve(pure_expr *ptr_tran,
                              pure_expr *ptr_prob, int sol)
{
  // Postsolve the model
  int status;
  tran_obj *tranobj;
  glp_obj *glpobj;
  if (!is_tran_pointer(ptr_tran, &tranobj)) {
    return 0;
  }
  if (!is_glp_pointer(ptr_prob, &glpobj)) {
    return 0;
  }
  status = glp_mpl_postsolve(tranobj->tran, glpobj->lp, sol);
  return pure_int(status);
}

pure_expr *glpk_mpl_free_wksp(pure_expr *ptr)
{
  // Delete the MathProg translator object
  tran_obj *tranobj;
  if (!is_tran_pointer(ptr, &tranobj)) {
    return 0;
  }
  glp_mpl_free_wksp(tranobj->tran);
  free(tranobj);
  ptr->data.p = NULL;
  return pure_void;
}

static pure_expr *getfile(int fce(glp_prob *ptra, const char *fnamea),
                          pure_expr *ptr, const char *fname)
{
  // Read data from a file using a parametric function
  int status;
  char *oldlocale;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = fce(glpobj->lp, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

static pure_expr *putfile(int fce(glp_prob *ptra, const char *fnamea),
                          pure_expr *ptr, const char *fname)
{
  // Write data into a file using a parametric function
  int status;
  char *oldlocale;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = fce(glpobj->lp, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_print_sol(pure_expr *ptr, const char *fname)
{
  // Write basic solution in printable format
  return putfile(glp_print_sol, ptr, fname);
}

pure_expr *glpk_read_sol(pure_expr *ptr, const char *fname)
{
  // Read basic solution from a text file
  return getfile(glp_read_sol, ptr, fname);
}

pure_expr *glpk_write_sol(pure_expr *ptr, const char *fname)
{
  // Write basic solution into a text file
  return putfile(glp_write_sol, ptr, fname);
}

pure_expr *glpk_print_ranges(pure_expr *ptr, pure_expr *lst, const char *fname)
{
  // Print sensitivity analysis report
  int status, numcols, numrows;
  char *oldlocale;
  int *indices;
  pure_expr **elems;
  size_t nelem;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!pure_is_listv(lst, &nelem, &elems)) {
    return 0;
  }
  numrows = glp_get_num_rows(glpobj->lp);
  numcols = glp_get_num_cols(glpobj->lp);
  if (!(indices = malloc((nelem + 1) * sizeof(int)))) {
    free(elems);
    return pure_err_internal("insufficient memory");
  }
  switch (pure_is_intlist(elems, nelem, numcols + numrows,
          "row or column", indices)) {
  case -1:
    free(indices);
    free(elems);
    return pure_err_internal(errormsg);
  case 0:
    free(indices);
    free(elems);
    return 0;
  case 1:
    oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
    if (!oldlocale) {
      free(indices);
      free(elems);
      return pure_err_internal("insufficient memory");
    }
    setlocale(LC_NUMERIC, "C");
    if (nelem) 
      status = glp_print_ranges(glpobj->lp, nelem, indices, 0, fname);
    else 
      status = glp_print_ranges(glpobj->lp, 0, NULL, 0, fname);
    setlocale(LC_NUMERIC, oldlocale);
    free(oldlocale);
    free(indices);
    free(elems);
    return pure_int(status);
  }
  return pure_err_internal("internal error - please report");
}

pure_expr *glpk_print_ipt(pure_expr *ptr, const char *fname)
{
  // Write interior-point solution in printable format
  return putfile(glp_print_ipt, ptr, fname);
}

pure_expr *glpk_read_ipt(pure_expr *ptr, const char *fname)
{
  // Read interior-point solution from a text file
  return getfile(glp_read_ipt, ptr, fname);
}

pure_expr *glpk_write_ipt(pure_expr *ptr, const char *fname)
{
  // Write interior-point solution into a text file
  return putfile(glp_write_ipt, ptr, fname);
}

pure_expr *glpk_print_mip(pure_expr *ptr, const char *fname)
{
  // Write MIP solution in printable format
  return putfile(glp_print_mip, ptr, fname);
}

pure_expr *glpk_read_mip(pure_expr *ptr, const char *fname)
{
  // Read MIP solution from a text file
  return getfile(glp_read_mip, ptr, fname);
}

pure_expr *glpk_write_mip(pure_expr *ptr, const char *fname)
{
  // Write MIP solution into a text file
  return putfile(glp_write_mip, ptr, fname);
}

pure_expr *glpk_bf_exists(pure_expr *ptr)
{
  // Check whether basis factorisation exists
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_bf_exists(glpobj->lp));
}

pure_expr *glpk_factorize(pure_expr *ptr)
{
  // Compute the basis factorization
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_factorize(glpobj->lp));
}

pure_expr *glpk_bf_updated(pure_expr *ptr)
{
  // Check whether basis factorization has been updated
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_bf_updated(glpobj->lp));
}

pure_expr *glpk_get_bfcp(pure_expr *ptr)
{
  // Get basis factorization parameters
#define BFCP_NUM 11
  int32_t intparm;
  double doubleparm;
  int i;
  glp_bfcp *parm;
  glp_obj *glpobj;
  pure_expr *list[BFCP_NUM];
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!(parm = (glp_bfcp *)malloc(sizeof(glp_bfcp)))) {
    return pure_err_internal("insufficient memory");
  }
  glp_get_bfcp(glpobj->lp, parm);
  i = 0;
  list[i++] = pure_tuplel(2, pure_symbol(pure_sym("glp::fact_type")),
                          pure_int((int32_t)parm->type));
  list[i++] = pure_tuplel(2, pure_symbol(pure_sym("glp::lu_size")),
                          pure_int((int32_t)parm->lu_size));
  list[i++] = pure_tuplel(2, pure_symbol(pure_sym("glp::piv_tol")),
                          pure_double(parm->piv_tol));
  list[i++] = pure_tuplel(2, pure_symbol(pure_sym("glp::piv_lim")),
                          pure_int((int32_t)parm->piv_lim));
  list[i++] = pure_tuplel(2, pure_symbol(pure_sym("glp::suhl")),
                          pure_int((int32_t)parm->suhl));
  list[i++] = pure_tuplel(2, pure_symbol(pure_sym("glp::eps_tol")),
                          pure_double(parm->eps_tol));
  list[i++] = pure_tuplel(2, pure_symbol(pure_sym("glp::max_gro")),
                          pure_double(parm->max_gro));
  list[i++] = pure_tuplel(2, pure_symbol(pure_sym("glp::nfs_max")),
                          pure_int((int32_t)parm->nfs_max));
  list[i++] = pure_tuplel(2, pure_symbol(pure_sym("glp::upd_tol")),
                          pure_double(parm->upd_tol));
  list[i++] = pure_tuplel(2, pure_symbol(pure_sym("glp::nrs_max")),
                          pure_int((int32_t)parm->nrs_max));
  list[i++] = pure_tuplel(2, pure_symbol(pure_sym("glp::rs_size")),
                          pure_int((int32_t)parm->rs_size));
  free(parm);
  return pure_listv(BFCP_NUM, list);
}

pure_expr *glpk_set_bfcp(pure_expr *ptr, pure_expr *parms)
{
  // Change basis factorization parameters
  pure_expr **list, **tpl, *snd, *res;
  int32_t fst, intparm;
  double doubleparm;
  int i;
  size_t it, cnt, cnterr;
  glp_bfcp *parm;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!(parm = (glp_bfcp *)malloc(sizeof(glp_bfcp)))) {
    return pure_err_internal("insufficient memory");
  }
  glp_get_bfcp(glpobj->lp, parm);
  if (!pure_is_listv(parms, &cnt, &list)) {
    cnterr = 1;
    free(parm);
    return 0;
  }
  for (i = 0; i < cnt; i++) {
    if (!pure_is_tuplev(list[i], &it, &tpl)) {
      list[cnterr++] = list[i];
      goto err;
    }
    if (it != 2) {
      free(tpl);
      list[cnterr++] = list[i];
      goto err;
    }
    if (!pure_is_symbol(tpl[0], &fst)) {
      free(tpl);
      list[cnterr++] = list[i];
      goto err;
    }
    snd = tpl[1];
    free(tpl);
    if (fst == pure_getsym("glp::fact_type")) {
      if (pure_is_int(snd, &intparm)) parm->type = intparm;
      else list[cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::lu_size")) {
      if (pure_is_int(snd, &intparm)) parm->lu_size = intparm;
      else list[cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::piv_tol")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->piv_tol = doubleparm;
      else list[cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::piv_lim")) {
      if (pure_is_int(snd, &intparm)) parm->piv_lim = intparm;
      else list[cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::suhl")) {
      if (pure_is_int(snd, &intparm)) parm->suhl = intparm;
      else list[cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::eps_tol")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->eps_tol = doubleparm;
      else list[cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::max_gro")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->max_gro = doubleparm;
      else list[cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::nfs_max")) {
      if (pure_is_int(snd, &intparm)) parm->nfs_max = intparm;
      else list[cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::upd_tol")) {
      if (pure_is_intordouble(snd, &doubleparm)) parm->upd_tol = doubleparm;
      else list[cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::nrs_max")) {
      if (pure_is_int(snd, &intparm)) parm->nrs_max = intparm;
      else list[cnterr++] = list[i];
    }
    else if (fst == pure_getsym("glp::rs_size")) {
      if (pure_is_int(snd, &intparm)) parm->rs_size = intparm;
      else list[cnterr++] = list[i];
    }
    else {
      list[cnterr++] = list[i];
    }
  err: ;
  }
  if (cnterr > 0) {
    res = pure_listv(cnterr, list);
  }
  else {
    if (cnt) {
      glp_set_bfcp(glpobj->lp, parm);
    }
    else {
      glp_set_bfcp(glpobj->lp, NULL); // reset all options to their defaults
    }
    res = pure_void;
  }
  free(list);
  free(parm);
  return res;
}

pure_expr *glpk_get_bhead(pure_expr *ptr, int k)
{
  // Retrieve the basis header information
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (k < 1 ||
      k > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("index out of bounds");
  }
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization must exist");
  }
  return pure_int(glp_get_bhead(glpobj->lp, k));
}

pure_expr *glpk_get_row_bind(pure_expr *ptr, int rowind)
{
  // Retrieve row index in the basis header
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (rowind < 1 ||
      rowind > glp_get_num_rows(glpobj->lp)) {
    return pure_err_internal("row index out of bounds");
  }
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization must exist");
  }
  return pure_int(glp_get_row_bind(glpobj->lp, rowind));
}

pure_expr *glpk_get_col_bind(pure_expr *ptr, int colind)
{
  // Retrieve column index in the basis header
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (colind < 1 ||
      colind > glp_get_num_cols(glpobj->lp)) {
    return pure_err_internal("column index out of bounds");
  }
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization must exist");
  }
  return pure_int(glp_get_col_bind(glpobj->lp, colind));
}

pure_expr *glpk_ftran(pure_expr *ptr, pure_expr *x)
{
  // Perform forward transformation
  size_t rowcnt;
  int i;
  pure_expr **list, *res;
  double *array, val;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization must exist");
  }
  if (!pure_is_listv(x, &rowcnt, &list)) {
    return 0;
  }
  if (rowcnt != glp_get_num_rows(glpobj->lp)) {
    free(list);
    return pure_err_internal("invalid number of list members");
  }
  if (!(array = (double *) malloc((rowcnt + 1) * sizeof(double)))) {
    free(list);
    return pure_err_internal("insufficient memory");
  }
  for (i = 0; i < rowcnt; i++) {
    if (!pure_is_intordouble(list[i], &val)) {
      res = pure_err_internal("non-numeric list member");
      goto err;
    }
    array[i + 1] = val;
  }
  glp_ftran(glpobj->lp, array);
  for (i = 0; i < rowcnt; i++) {
    list[i] = pure_double(array[i + 1]);
  }
  res = pure_listv(rowcnt, list);
err:
  free(list);
  free(array);
  return res;
}

pure_expr *glpk_btran(pure_expr *ptr, pure_expr *x)
{
  // Perform backward transformation
  size_t rowcnt;
  int i;
  pure_expr **list, *res;
  double *array, val;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization must exist");
  }
  if (!pure_is_listv(x, &rowcnt, &list)) {
    return 0;
  }
  if (rowcnt != glp_get_num_rows(glpobj->lp)) {
    free(list);
    return pure_err_internal("invalid number of list members");
  }
  if (!(array = (double *) malloc((rowcnt + 1) * sizeof(double)))) {
    free(list);
    return pure_err_internal("insufficient memory");
  }
  for (i = 0; i < rowcnt; i++) {
    if (!pure_is_intordouble(list[i], &val)) {
      res = pure_err_internal("non-numeric list member");
      goto err;
    }
    array[i + 1] = val;
  }
  glp_btran(glpobj->lp, array);
  for (i = 0; i < rowcnt; i++) {
    list[i] = pure_double(array[i + 1]);
  }
  res = pure_listv(rowcnt, list);
err:
  free(list);
  free(array);
  return res;
}

pure_expr *glpk_warm_up(pure_expr *ptr)
{
  // Warm up LP basis
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  return pure_int(glp_warm_up(glpobj->lp));
}

pure_expr *glpk_eval_tab_row(pure_expr *ptr, int k)
{
  // Compute row of the tableau
  int i, *arrayind, colcnt, rowcnt, ind;
  size_t cnt;
  pure_expr **list, *res;
  double *arrayval, val;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  rowcnt = glp_get_num_rows(glpobj->lp);
  colcnt = glp_get_num_cols(glpobj->lp);
  if (k < 1 || k > rowcnt + colcnt) {
    return pure_err_internal("variable index out of bounds");
  }
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization must exist");
  }
  if ((k <= rowcnt && glp_get_row_stat(glpobj->lp, k) != GLP_BS) ||
      (k > rowcnt && glp_get_col_stat(glpobj->lp, k - rowcnt) != GLP_BS)) {
    return pure_err_internal("variable must be basic");
  }
  if (!(arrayval = (double *) malloc((colcnt + 1) * sizeof(double)))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(arrayind = (int *) malloc((colcnt + 1) * sizeof(int)))) {
    free(arrayval);
    return pure_err_internal("insufficient memory");
  }
  cnt = (size_t)glp_eval_tab_row(glpobj->lp, k, arrayind, arrayval);
  if (!(list = (pure_expr **) malloc(cnt * sizeof(pure_expr *)))) {
    free(arrayval);
    free(arrayind);
    return pure_err_internal("insufficient memory");
  }
  for (i = 0; i < cnt; i++) {
    list[i] = pure_tuplel(2, pure_int(arrayind[i + 1]),
                             pure_double(arrayval[i + 1]));
  }
  res = pure_listv(cnt, list);
  free(list);
  free(arrayval);
  free(arrayind);
  return res;
}

pure_expr *glpk_eval_tab_col(pure_expr *ptr, int k)
{
  // Compute column of the tableau
  int i, *arrayind, colcnt, rowcnt, ind;
  size_t cnt;
  pure_expr **list, *res;
  double *arrayval, val;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  rowcnt = glp_get_num_rows(glpobj->lp);
  colcnt = glp_get_num_cols(glpobj->lp);
  if (k < 1 || k > rowcnt + colcnt) {
    return pure_err_internal("index out of bounds");
  }
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization must exist");
  }
  if ((k <= rowcnt && glp_get_row_stat(glpobj->lp, k) == GLP_BS) ||
      (k > rowcnt && glp_get_col_stat(glpobj->lp, k - rowcnt) == GLP_BS)) {
    return pure_err_internal("variable must be non-basic");
  }
  if (!(arrayval = (double *) malloc((rowcnt + 1) * sizeof(double)))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(arrayind = (int *) malloc((rowcnt + 1) * sizeof(int)))) {
    free(arrayval);
    return pure_err_internal("insufficient memory");
  }
  cnt = (size_t)glp_eval_tab_col(glpobj->lp, k, arrayind, arrayval);
  if (!(list = (pure_expr **) malloc(cnt * sizeof(pure_expr *)))) {
    free(arrayval);
    free(arrayind);
    return pure_err_internal("insufficient memory");
  }
  for (i = 0; i < cnt; i++) {
    list[i] = pure_tuplel(2, pure_int(arrayind[i + 1]),
                             pure_double(arrayval[i + 1]));
  }
  res = pure_listv(cnt, list);
  free(list);
  free(arrayval);
  free(arrayind);
  return res;
}

pure_expr *glpk_transform_row(pure_expr *ptr, pure_expr *inrow)
{
  // Transform explicitly specified row
  int i, *arrayind, colcnt, ind;
  size_t cnt;
  pure_expr **list, **tlist, *res;
  double *arrayval, val;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  colcnt = glp_get_num_cols(glpobj->lp);
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization must exist");
  }
  if (!pure_is_listv(inrow, &cnt, &list) || cnt == 0) {
    return 0;
  }
  if (!(arrayval = (double *) malloc((colcnt + 1) * sizeof(double)))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(arrayind = (int *) malloc((colcnt + 1) * sizeof(int)))) {
    free(arrayval);
    return pure_err_internal("insufficient memory");
  }
  switch (pure_is_pairlist(list, cnt, colcnt, "column", arrayind, arrayval)) {
  case -1:
    free(arrayind);
    free(arrayval);
    free(list);
    return pure_err_internal(errormsg);
  case 0:
    free(arrayind);
    free(arrayval);
    free(list);
    return 0;
  case 1:
    cnt = glp_transform_row(glpobj->lp, cnt, arrayind, arrayval);
    if (!(tlist = (pure_expr **) realloc(list, cnt * sizeof(pure_expr *)))) {
      free(arrayval);
      free(arrayind);
      free(list);
      return pure_err_internal("insufficient memory");
    }
    list = tlist;
    for (i = 0; i < cnt; i++) {
      list[i] = pure_tuplel(2, pure_int(arrayind[i + 1]),
                               pure_double(arrayval[i + 1]));
    }
    res = pure_listv(cnt, list);
    free(arrayind);
    free(arrayval);
    free(list);
    return res;
  }
  return pure_err_internal("internal error - please report");
}

pure_expr *glpk_transform_col(pure_expr *ptr, pure_expr *incol)
{
  // Transform explicitly specified column
  int i, *arrayind, rowcnt, ind;
  size_t cnt;
  pure_expr **list, **tlist, *res;
  double *arrayval, val;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  rowcnt = glp_get_num_rows(glpobj->lp);
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization must exist");
  }
  if (!pure_is_listv(incol, &cnt, &list) || cnt == 0) {
    return 0;
  }
  if (!(arrayval = (double *) malloc((rowcnt + 1) * sizeof(double)))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(arrayind = (int *) malloc((rowcnt + 1) * sizeof(int)))) {
    free(arrayval);
    return pure_err_internal("insufficient memory");
  }
  switch (pure_is_pairlist(list, cnt, rowcnt, "row", arrayind, arrayval)) {
  case -1:
    free(arrayind);
    free(arrayval);
    free(list);
    return pure_err_internal(errormsg);
  case 0:
    free(arrayind);
    free(arrayval);
    free(list);
    return 0;
  case 1:
    cnt = glp_transform_col(glpobj->lp, cnt, arrayind, arrayval);
    if (!(tlist = (pure_expr **) realloc(list, cnt * sizeof(pure_expr *)))) {
      free(arrayval);
      free(arrayind);
      free(list);
      return pure_err_internal("insufficient memory");
    }
    list = tlist;
    for (i = 0; i < cnt; i++) {
      list[i] = pure_tuplel(2, pure_int(arrayind[i + 1]),
                               pure_double(arrayval[i + 1]));
    }
    res = pure_listv(cnt, list);
    free(arrayind);
    free(arrayval);
    free(list);
    return res;
  }
  return pure_err_internal("internal error - please report");
}

pure_expr *glpk_prim_rtest(pure_expr *ptr, pure_expr *incol,
                                int how, double tol)
{
  // Perform primal ratio test
  int i, *arrayind, colcnt, rowcnt, ind;
  size_t cnt, n;
  pure_expr **list, **tpl, *res;
  double *arrayval, val;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  colcnt = glp_get_num_cols(glpobj->lp);
  rowcnt = glp_get_num_rows(glpobj->lp);
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization must exist");
  }
  if (glp_get_prim_stat(glpobj->lp) != GLP_FEAS) {
    return pure_err_internal("primal basic solution must be feasible");
  }
  if (how != -1 && how != 1) {
    return pure_err_internal("\"how\" must be either -1 or 1");
  }
  if (!pure_is_listv(incol, &cnt, &list) || cnt == 0) {
    return 0;
  }
  if (!(arrayval = (double *) malloc((rowcnt + 1) * sizeof(double)))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(arrayind = (int *) malloc((rowcnt + 1) * sizeof(int)))) {
    free(arrayval);
    return pure_err_internal("insufficient memory");
  }
  arrayind[0] = 0;
  arrayval[0] = 0.0;
  for (i = 0; i < cnt; i++) {
    if (!pure_is_tuplev(list[i], &n, &tpl)) {
      res = 0;
      goto finish;
    }
    if (n != 2 || !pure_is_int(tpl[0], &ind) ||
               !pure_is_intordouble(tpl[1], &val)) {
      free(tpl);
      res = 0;
      goto finish;
    }
    if (ind < 1 || ind > colcnt + rowcnt) {
      free(tpl);
      res = pure_err_internal("variable index out of bounds");
      goto finish;
    }
    if ((ind <= rowcnt && glp_get_row_stat(glpobj->lp, ind) != GLP_BS) ||
        (ind > rowcnt && 
	 glp_get_col_stat(glpobj->lp, ind - rowcnt) != GLP_BS)) {
      return pure_err_internal("all variables must be basic");
    }
    arrayind[i + 1] = ind;
    arrayval[i + 1] = val;
    free(tpl);
  }
  res = pure_int(glp_prim_rtest(glpobj->lp, cnt, arrayind,
                                     arrayval, how, tol));
finish:
  free(arrayind);
  free(arrayval);
  free(list);
  return res;
}

pure_expr *glpk_dual_rtest(pure_expr *ptr, pure_expr *incol,
                                int how, double tol)
{
  // Perform dual ratio test
  int i, *arrayind, colcnt, rowcnt, ind;
  size_t cnt, n;
  pure_expr **list, **tpl, *res;
  double *arrayval, val;
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  colcnt = glp_get_num_cols(glpobj->lp);
  rowcnt = glp_get_num_rows(glpobj->lp);
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization must exist");
  }
  if (glp_get_dual_stat(glpobj->lp) != GLP_FEAS) {
    return pure_err_internal("dual basic solution must be feasible");
  }
  if (how != -1 && how != 1) {
    return pure_err_internal("\"how\" must be either -1 or 1");
  }
  if (!pure_is_listv(incol, &cnt, &list) || cnt == 0) {
    return 0;
  }
  if (!(arrayval = (double *) malloc((rowcnt + 1) * sizeof(double)))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(arrayind = (int *) malloc((rowcnt + 1) * sizeof(int)))) {
    free(arrayval);
    return pure_err_internal("insufficient memory");
  }
  arrayind[0] = 0;
  arrayval[0] = 0.0;
  for (i = 0; i < cnt; i++) {
    if (!pure_is_tuplev(list[i], &n, &tpl)) {
      res = 0;
      goto finish;
    }
    if (n != 2 || !pure_is_int(tpl[0], &ind) ||
               !pure_is_intordouble(tpl[1], &val)) {
      free(tpl);
      res = 0;
      goto finish;
    }
    if (ind < 1 || ind > colcnt + rowcnt) {
      free(tpl);
      res = pure_err_internal("variable index out of bounds");
      goto finish;
    }
    if ((ind <= rowcnt && glp_get_row_stat(glpobj->lp, ind) == GLP_BS) ||
        (ind > rowcnt && 
	 glp_get_col_stat(glpobj->lp, ind - rowcnt) == GLP_BS)) {
      return pure_err_internal("all variables must be non-basic");
    }
    arrayind[i + 1] = ind;
    arrayval[i + 1] = val;
    free(tpl);
  }
  res = pure_int(glp_dual_rtest(glpobj->lp, cnt, arrayind,
                                     arrayval, how, tol));
finish:
  free(arrayind);
  free(arrayval);
  free(list);
  return res;
}

pure_expr *glpk_analyze_bound(pure_expr *ptr, int k)
{
  // Analyze active bound of non-basic variable
  glp_obj *glpobj;
  int numrows, numcols;
  double limit1, limit2;
  int var1, var2;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  numrows = glp_get_num_rows(glpobj->lp);
  numcols = glp_get_num_cols(glpobj->lp);
  if (k < 1 || k > numrows + numcols) {
    return pure_err_internal("index out bounds");
  }
  if (glp_get_status(glpobj->lp) != GLP_OPT) {
    return pure_err_internal("not at optimal solution");
  }
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization does not exist");
  }
  if ((k <= numrows && glp_get_row_stat(glpobj->lp, k) == GLP_BS) ||
      (k > numrows && glp_get_col_stat(glpobj->lp, k - numrows) == GLP_BS)) {
    return pure_err_internal("variable must be non-basic");
  }
  glp_analyze_bound(glpobj->lp, k, &limit1, &var1, &limit2, &var2);
  return pure_tuplel(4, pure_double(limit1), pure_int(var1),
		        pure_double(limit2), pure_int(var2));
}

pure_expr *glpk_analyze_coef(pure_expr *ptr, int k)
{
  // Analyze objective coefficient at basic variable
  glp_obj *glpobj;
  int numrows, numcols;
  double coef1, coef2, value1, value2;
  int var1, var2;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  numrows = glp_get_num_rows(glpobj->lp);
  numcols = glp_get_num_cols(glpobj->lp);
  if (k < 1 || k > numrows + numcols) {
    return pure_err_internal("index out bounds");
  }
  if (glp_get_status(glpobj->lp) != GLP_OPT) {
    return pure_err_internal("not at optimal solution");
  }
  if (!glp_bf_exists(glpobj->lp)) {
    return pure_err_internal("basis factorization does not exist");
  }
  if ((k <= numrows && glp_get_row_stat(glpobj->lp, k) != GLP_BS) ||
      (k > numrows && glp_get_col_stat(glpobj->lp, k - numrows) != GLP_BS)) {
    return pure_err_internal("variable must be basic");
  }
  glp_analyze_coef(glpobj->lp, k, &coef1, &var1, &value1,
		                  &coef2, &var2, &value2);
  return pure_tuplel(6, pure_double(coef1), pure_int(var1),
		        pure_double(value1),pure_double(coef2),
		        pure_int(var2), pure_double(value2));
}

pure_expr *glpk_ios_reason(pure_expr *ptr)
{
  // Determine reason for calling the callback routine
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  return pure_int(glp_ios_reason(treeobj->tree));
}

pure_expr *glpk_delete_wrapper(pure_expr *ptr)
{
  // Delete the wrapper of the glp_prob object
  glp_obj *glpobj;
  if (!is_glp_pointer(ptr, &glpobj)) {
    return 0;
  }
  free(glpobj);
  ptr->data.p = NULL;
  return pure_void;
}

pure_expr *glpk_ios_get_prob(pure_expr *ptr)
{
  // Access the problem object
  glp_prob *lp;
  glp_obj *glpobj;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  if (!(glpobj = (glp_obj *)malloc(sizeof(glp_obj)))) {
    return pure_err_internal("insufficient memory");
  }
  glpobj->magic = GLPK_MAGIC;
  glpobj->lp = glp_ios_get_prob(treeobj->tree);
  return pure_sentry(pure_symbol(pure_sym("glp::delete_wrapper")),
                       pure_pointer(glpobj));

}

pure_expr *glpk_ios_row_attr(pure_expr *ptr, int irow)
{
  // Determine additional row attributes
  glp_attr attr;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  if (irow < 1 || irow > glp_ios_pool_size(treeobj->tree)) {
    return pure_err_internal("row index out of bounds");
  }
  glp_ios_row_attr(treeobj->tree, irow, &attr);
  return pure_tuplel(3, pure_int(attr.level),
                        pure_int(attr.origin),
                        pure_int(attr.klass));
}

pure_expr *glpk_ios_mip_gap(pure_expr *ptr)
{
  // Compute relative MIP gap
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  return pure_double(glp_ios_mip_gap(treeobj->tree));
}

pure_expr *glpk_ios_node_data(pure_expr *ptr, int p)
{
  // Access application-speciﬁc data
  int n_cnt;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  glp_ios_tree_size(treeobj->tree, NULL, &n_cnt, NULL);
  if (p < 1 || p > n_cnt) {
  return pure_err_internal("subproblem reference number out of bounds");
  }
  return pure_pointer(glp_ios_node_data(treeobj->tree, p));
}

pure_expr *glpk_ios_select_node(pure_expr *ptr, int p)
{
  // Select subproblem to continue the search
  int n_cnt;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  glp_ios_tree_size(treeobj->tree, NULL, &n_cnt, NULL);
  if (p < 1 || p > n_cnt) {
  return pure_err_internal("subproblem reference number out of bounds");
  }
  glp_ios_select_node(treeobj->tree, p);
  return pure_void;
}

pure_expr *glpk_ios_heur_sol(pure_expr *ptr, pure_expr *x)
{
  // Provide solution found by heuristic
  size_t colcnt;
  int i;
  pure_expr **list, *res;
  double *array, val;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  if (!pure_is_listv(x, &colcnt, &list)) {
    return 0;
  }
  if (colcnt != glp_get_num_cols(glp_ios_get_prob(treeobj->tree))) {
    free(list);
    return pure_err_internal("invalid number of list members");
  }
  if (!(array = (double *) malloc((colcnt + 1) * sizeof(double)))) {
    free(list);
    return pure_err_internal("insufficient memory");
  }
  for (i = 0; i < colcnt; i++) {
    if (!pure_is_intordouble(list[i], &val)) {
      res = pure_err_internal("non-numeric list member");
      goto err;
    }
    array[i + 1] = val;
  }
  res = pure_int(glp_ios_heur_sol(treeobj->tree, array));
err:
  free(list);
  free(array);
  return res;
}

pure_expr *glpk_ios_can_branch(pure_expr *ptr, int j)
{
  // Check whether can branch upon speciﬁed variable
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  if (j < 1 || j > glp_get_num_cols(glp_ios_get_prob(treeobj->tree))) {
    return pure_err_internal("column index out of bounds");
  }
  return pure_int(glp_ios_can_branch(treeobj->tree, j));
}

pure_expr *glpk_ios_branch_upon(pure_expr *ptr, int j, int sel)
{
  // Choose variable to branch upon
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  if (j < 1 || j > glp_get_num_cols(glp_ios_get_prob(treeobj->tree))) {
    return pure_err_internal("column index out of bounds");
  }
  if (!glp_ios_can_branch(treeobj->tree, j)) {
    return pure_err_internal("cannot branch on selected variable");
  }
  glp_ios_branch_upon(treeobj->tree, j, sel);
  return pure_void;
}

pure_expr *glpk_ios_terminate(pure_expr *ptr)
{
  // Terminate the solution process
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  glp_ios_terminate(treeobj->tree);
  return pure_void;
}

pure_expr *glpk_ios_tree_size(pure_expr *ptr)
{
  // Determine the search tree size
  int a_cnt, n_cnt, t_cnt;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  glp_ios_tree_size(treeobj->tree, &a_cnt, &n_cnt, &t_cnt);
  return pure_tuplel(3, pure_int(a_cnt),
                        pure_int(n_cnt),
                        pure_int(t_cnt));
}

pure_expr *glpk_ios_curr_node(pure_expr *ptr)
{
  // Determine current active subproblem
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  return pure_int(glp_ios_curr_node(treeobj->tree));
}

pure_expr *glpk_ios_next_node(pure_expr *ptr, int p)
{
// Determine next active subproblem
  int n_cnt;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  glp_ios_tree_size(treeobj->tree, NULL, &n_cnt, NULL);
  if (p < 1 || p > n_cnt) {
  return pure_err_internal("subproblem reference number out of bounds");
  }
  return pure_int(glp_ios_next_node(treeobj->tree, p));
}

pure_expr *glpk_ios_prev_node(pure_expr *ptr, int p)
{
  // Determine previous active subproblem
  int n_cnt;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  glp_ios_tree_size(treeobj->tree, NULL, &n_cnt, NULL);
  if (p < 1 || p > n_cnt) {
  return pure_err_internal("subproblem reference number out of bounds");
  }
  return pure_int(glp_ios_prev_node(treeobj->tree, p));
}

pure_expr *glpk_ios_up_node(pure_expr *ptr, int p)
{
  // Determine parent active subproblem
  int n_cnt;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  glp_ios_tree_size(treeobj->tree, NULL, &n_cnt, NULL);
  if (p < 1 || p > n_cnt) {
  return pure_err_internal("subproblem reference number out of bounds");
  }
  return pure_int(glp_ios_up_node(treeobj->tree, p));
}

pure_expr *glpk_ios_node_level(pure_expr *ptr, int p)
{
  // Determine subproblem level
  int n_cnt;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  glp_ios_tree_size(treeobj->tree, NULL, &n_cnt, NULL);
  if (p < 1 || p > n_cnt) {
  return pure_err_internal("subproblem reference number out of bounds");
  }
  return pure_int(glp_ios_node_level(treeobj->tree, p));
}

pure_expr *glpk_ios_node_bound(pure_expr *ptr, int p)
{
  // Determine subproblem local bound
  int n_cnt;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  glp_ios_tree_size(treeobj->tree, NULL, &n_cnt, NULL);
  if (p < 1 || p > n_cnt) {
  return pure_err_internal("subproblem reference number out of bounds");
  }
  return pure_double(glp_ios_node_bound(treeobj->tree, p));
}

pure_expr *glpk_ios_best_node(pure_expr *ptr)
{
  // Find active subproblem with the best local bound
  int n_cnt;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  return pure_int(glp_ios_best_node(treeobj->tree));
}

pure_expr *glpk_ios_pool_size(pure_expr *ptr)
{
  // Determine current size of the cut pool
  int n_cnt;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  return pure_int(glp_ios_pool_size(treeobj->tree));
}

pure_expr *glpk_ios_add_row(pure_expr *ptr, const char *name, int klass,
                            int flags, pure_expr *row, int type, double rhs)
{
  // Add constraint to the cut pool
  int numcols, *indices;
  double *values;
  pure_expr **elems, *res;
  size_t nelem;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  if (strlen(name) > 255) {
    return pure_err_internal("string too long");
  }
  if (!pure_is_listv(row, &nelem, &elems) || nelem == 0) {
    return 0;
  }
  numcols = glp_get_num_cols(glp_ios_get_prob(treeobj->tree));
  if (!(indices = malloc((nelem + 1) * sizeof(int)))) {
    free(elems);
    return pure_err_internal("insufficient memory");
  }
  if (!(values = malloc((nelem + 1) * sizeof(double)))) {
    free(elems);
    free(indices);
    return pure_err_internal("insufficient memory");
  }
  switch (pure_is_pairlist(elems, nelem, numcols, "column", indices, values)) {
  case -1:
    free(indices);
    free(values);
    free(elems);
    return pure_err_internal(errormsg);
  case 0:
    free(indices);
    free(values);
    free(elems);
    return 0;
  case 1:
    res = pure_int(glp_ios_add_row(treeobj->tree, name, klass, flags, nelem,
                                   indices, values, type, rhs));
    free(values);
    free(indices);
    free(elems);
    return res;
  }
  return pure_err_internal("internal error - please report");
}

pure_expr *glpk_ios_del_row(pure_expr *ptr, int irow)
{
  // Remove constraint from the cut pool
  glp_attr attr;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  if (irow < 1 || irow > glp_ios_pool_size(treeobj->tree)) {
    return pure_err_internal("row index out of bounds");
  }
  glp_ios_del_row(treeobj->tree, irow);
  return pure_void;
}

pure_expr *glpk_ios_clear_pool(pure_expr *ptr)
{
  // Remove all constraints from the cut pool
  glp_attr attr;
  tree_obj *treeobj;
  if (!is_tree_pointer(ptr, &treeobj)) {
    return 0;
  }
  glp_ios_clear_pool(treeobj->tree);
  return pure_void;
}

pure_expr *glpk_create_graph(int v_size, int a_size)
{
  // Create the GLPK graph object
  graph_obj *graphobj;
  glp_graph *graph;
  if (v_size < 0 || v_size > 256 || a_size < 0 || a_size > 256) {
    return pure_err_internal("invalid data block dimensions");
  }
  if (!(graphobj = (graph_obj*) malloc(sizeof(graph_obj)))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(graph = glp_create_graph(v_size, a_size))) {
    free(graphobj);
    return pure_err_internal("insufficient memory");
  }
  graphobj->magic = GRAPH_MAGIC;
  graphobj->graph = graph;
  return pure_sentry(pure_symbol(pure_sym("glp::delete_graph")),
                       pure_pointer(graphobj));
}

pure_expr *glpk_set_graph_name(pure_expr *ptr, const char *name)
{
  // Set the graph name
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  if (strlen(name) > 255) {
    return pure_err_internal("string too long");
  }
  glp_set_graph_name(graphobj->graph, name);
  return pure_void;
}

pure_expr *glpk_add_vertices(pure_expr *ptr, int nadd)
{
  // Add vertices to a graph
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  if (nadd < 1) {
    return pure_err_internal("invalid number of vertices");
  }
  return pure_int(glp_add_vertices(graphobj->graph, nadd));
}

pure_expr *glpk_delete_arc_wrapper(pure_expr *ptr)
{
  // Delete the wrapper of the glp_arc object
  arc_obj *arcobj;
  if (!is_arc_pointer(ptr, &arcobj)) {
    return 0;
  }
  free(arcobj);
  ptr->data.p = NULL;
  return pure_void;
}

pure_expr *glpk_add_arc(pure_expr *ptr, int i, int j)
{
  // Add arc to a graph
  glp_arc *arc;
  arc_obj *arcobj;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  if (i < 1 || i > graphobj->graph->nv || j < 1 || j > graphobj->graph->nv) {
    return pure_err_internal("vertex index out of bounds");
  }
  if (!(arc = glp_add_arc(graphobj->graph, i, j))) {
    return pure_err_internal("insufficient memory");
  }
  if (!(arcobj = (arc_obj*) malloc(sizeof(arc_obj)))) {
    return pure_err_internal("insufficient memory");
  }
  arcobj->magic = ARC_MAGIC;
  arcobj->arc = arc;
  return pure_sentry(pure_symbol(pure_sym("glp::delete_arc_wrapper")),
                       pure_pointer(arcobj));
}

pure_expr *glpk_erase_graph(pure_expr *ptr, int v_size, int a_size)
{
  // Erase content of the GLPK graph object
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  if (v_size < 0 || v_size > 256 || a_size < 0 || a_size > 256) {
    return pure_err_internal("invalid data block dimensions");
  }
  glp_erase_graph(graphobj->graph, v_size, a_size);
  return pure_void;
}

pure_expr *glpk_delete_graph(pure_expr *ptr)
{
  // Delete the GLPK graph object
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  glp_delete_graph(graphobj->graph);
  free(graphobj);
  ptr->data.p = NULL;
  return pure_void;
}

pure_expr *glpk_read_graph(pure_expr *ptr, const char *fname)
{
  // Read graph in a plain text format
  int status;
  char *oldlocale;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_read_graph(graphobj->graph, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_write_graph(pure_expr *ptr, const char *fname)
{
  // Write graph in a plain text format
  int status;
  char *oldlocale;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_write_graph(graphobj->graph, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_weak_comp(pure_expr *ptr, int v_num)
{
  // Find all weakly connected components of a graph
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  if (v_num < 1 || v_num > graphobj->graph->nv) {
    return pure_err_internal("invalid vertex number");
  }
  return pure_int(glp_weak_comp(graphobj->graph, v_num));
}

pure_expr *glpk_strong_comp(pure_expr *ptr, int v_num)
{
  // Find all strongly connected components of a graph
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  if (v_num < 1 || v_num > graphobj->graph->nv) {
    return pure_err_internal("invalid vertex number");
  }
  return pure_int(glp_strong_comp(graphobj->graph, v_num));
}

pure_expr *glpk_read_mincost(pure_expr *ptr, int v_rhs, int a_low, int a_cap,
                             int a_cost, const char *fname)
{
  // Read minimum cost flow problem data in DIMACS format
  int status;
  char *oldlocale;
  int maxoff, dbl;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  dbl = sizeof(double);
  maxoff = graphobj->graph->a_size - dbl;
  if (a_low > maxoff || a_cap > maxoff || a_cost > maxoff ||
      abs(a_low - a_cap) < dbl || abs(a_low - a_cost) < dbl ||
      abs(a_cap - a_cost) < dbl ||
      v_rhs > (graphobj->graph->v_size - dbl)) {
    return pure_err_internal("mismatch in storage offsets");
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_read_mincost(graphobj->graph, v_rhs, a_low, a_cap,
                            a_cost, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_write_mincost(pure_expr *ptr, int v_rhs, int a_low, int a_cap,
                             int a_cost, const char *fname)
{
  // Write minimum cost flow problem data in DIMACS format
  int status;
  char *oldlocale;
  int maxoff, dbl;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  dbl = sizeof(double);
  maxoff = graphobj->graph->a_size - dbl;
  if (a_low > maxoff || a_cap > maxoff || a_cost > maxoff ||
      abs(a_low - a_cap) < dbl || abs(a_low - a_cost) < dbl ||
      abs(a_cap - a_cost) < dbl ||
      v_rhs > (graphobj->graph->v_size - dbl)) {
    return pure_err_internal("mismatch in storage offsets");
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_write_mincost(graphobj->graph, v_rhs, a_low, a_cap,
                             a_cost, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_int(status);
}

pure_expr *glpk_mincost_lp(pure_expr *ptrlp, pure_expr *ptrg, int names,
                           int v_rhs, int a_low, int a_cap, int a_cost)
{
  // Convert minimum cost flow problem to LP
  int maxoff, dbl;
  glp_obj *glpobj;
  graph_obj *graphobj;
  if (!is_glp_pointer(ptrlp, &glpobj)) {
    return 0;
  }
  if (!is_graph_pointer(ptrg, &graphobj)) {
    return 0;
  }
  dbl = sizeof(double);
  maxoff = graphobj->graph->a_size - dbl;
  if (a_low > maxoff || a_cap > maxoff || a_cost > maxoff ||
      abs(a_low - a_cap) < dbl || abs(a_low - a_cost) < dbl ||
      abs(a_cap - a_cost) < dbl ||
      v_rhs > (graphobj->graph->v_size - dbl)) {
    return pure_err_internal("mismatch in storage offsets");
  }
  glp_mincost_lp(glpobj->lp, graphobj->graph, names, v_rhs, a_low,
                 a_cap, a_cost);
  return pure_void;
}

pure_expr *glpk_mincost_okalg(pure_expr *ptr, int v_rhs, int a_low, int a_cap,
                              int a_cost, int a_x, int v_pi)
{
  // Solve minimum cost flow problem with out-of-kilter algorithm
  int maxoff, dbl;
  double sol;
  int stat;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  dbl = sizeof(double);
  maxoff = graphobj->graph->a_size - dbl;
  if (a_low > maxoff || a_cap > maxoff || a_cost > maxoff || a_x > maxoff ||
      abs(a_low - a_cap) < dbl || abs(a_low - a_cost) < dbl ||
      abs(a_cap - a_cost) < dbl || abs(a_low - a_x) < dbl ||
      abs(a_cap - a_x) < dbl || abs(a_cost - a_x) < dbl ||
      v_rhs > (graphobj->graph->v_size - dbl) ||
      v_pi > (graphobj->graph->v_size - dbl) ||
      abs(v_rhs - v_pi) < dbl) {
    return pure_err_internal("mismatch in storage offsets");
  }
  stat = glp_mincost_okalg(graphobj->graph, v_rhs, a_low,
                           a_cap, a_cost, &sol, a_x, v_pi);
  return pure_tuplel(2, pure_int(stat), pure_double(sol));
}

pure_expr *glpk_netgen(pure_expr *ptr, int v_rhs, int a_cap, int a_cost,
                       pure_expr *parm)
{
  // Klingman's network problem generator
  int maxoff, dbl;
  size_t cnt, i;
  int params[16], ipar;
  pure_expr **tuple, *res;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  dbl = sizeof(double);
  maxoff = graphobj->graph->a_size - dbl;
  if (a_cap > maxoff || a_cost > maxoff || abs(a_cap - a_cost) < dbl ||
      v_rhs > (graphobj->graph->v_size - dbl)) {
    return pure_err_internal("mismatch in storage offsets");
  }
  if (!pure_is_tuplev(parm, &cnt, &tuple)) {
    return 0;
  }
  if (cnt != 15) {
    free(tuple);
    return pure_err_internal("there must be exactly 15 parametres");
  }
  for (i = 0; i < 15; i++) {
    if (!pure_is_int(tuple[i], &ipar)) {
      free(tuple);
      return pure_err_internal("non-integer parameter");
    }
    params[i + 1] = ipar;
  }
  res = pure_int(glp_netgen(graphobj->graph, v_rhs, a_cap, a_cost, params));
  free(tuple);
  return res;
}

pure_expr *glpk_gridgen(pure_expr *ptr, int v_rhs, int a_cap, int a_cost,
                       pure_expr *parm)
{
  // Grid-like network problem generator
  int maxoff, dbl;
  size_t cnt, i;
  int params[15], ipar;
  pure_expr **tuple, *res;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  dbl = sizeof(double);
  maxoff = graphobj->graph->a_size - dbl;
  if (a_cap > maxoff || a_cost > maxoff || abs(a_cap - a_cost) < dbl ||
      v_rhs > (graphobj->graph->v_size - dbl)) {
    return pure_err_internal("mismatch in storage offsets");
  }
  if (!pure_is_tuplev(parm, &cnt, &tuple)) {
    return 0;
  }
  if (cnt != 14) {
    free(tuple);
    return pure_err_internal("there must be exactly 14 parametres");
  }
  for (i = 0; i < 14; i++) {
    if (!pure_is_int(tuple[i], &ipar)) {
      free(tuple);
      return pure_err_internal("non-integer parameter");
    }
    params[i + 1] = ipar;
  }
  res = pure_int(glp_gridgen(graphobj->graph, v_rhs, a_cap, a_cost, params));
  free(tuple);
  return res;
}

pure_expr *glpk_read_maxflow(pure_expr *ptr, int a_cap, const char *fname)
{
  // Read maximum cost flow problem data in DIMACS format
  int status;
  char *oldlocale;
  int maxoff, dbl, s, t;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  dbl = sizeof(double);
  maxoff = graphobj->graph->a_size - dbl;
  if (a_cap > maxoff) {
    return pure_err_internal("mismatch in storage offsets");
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_read_maxflow(graphobj->graph, &s, &t, a_cap, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_tuplel(3, pure_int(status), pure_int(s), pure_int(t));
}

pure_expr *glpk_write_maxflow(pure_expr *ptr, int s, int t, int a_cap,
                              const char *fname)
{
  // Write maximum cost flow problem data in DIMACS format
  int status;
  char *oldlocale;
  int maxoff, dbl;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  dbl = sizeof(double);
  maxoff = graphobj->graph->a_size - dbl;
  if (s < 1 || t < 1 || s > graphobj->graph->v_size ||
      t > graphobj->graph->v_size) {
    return pure_err_internal("invalid node numbers");
  }
  if (a_cap > maxoff) {
    return pure_err_internal("mismatch in storage offsets");
  }
  oldlocale = strdup(setlocale(LC_NUMERIC, NULL));
  if (!oldlocale) {
    return pure_err_internal("insufficient memory");
  }
  setlocale(LC_NUMERIC, "C");
  status = glp_write_maxflow(graphobj->graph, s, t, a_cap, fname);
  setlocale(LC_NUMERIC, oldlocale);
  free(oldlocale);
  return pure_tuplel(3, pure_int(status), pure_int(s), pure_int(t));
}

pure_expr *glpk_maxflow_lp(pure_expr *ptrlp, pure_expr *ptrg, int names,
                           int s, int t, int a_cap)
{
  // Convert maximum flow problem to LP
  int maxoff, dbl;
  glp_obj *glpobj;
  graph_obj *graphobj;
  if (!is_glp_pointer(ptrlp, &glpobj)) {
    return 0;
  }
  if (!is_graph_pointer(ptrg, &graphobj)) {
    return 0;
  }
  dbl = sizeof(double);
  maxoff = graphobj->graph->a_size - dbl;
  if (s < 1 || t < 1 || s > graphobj->graph->v_size ||
      t > graphobj->graph->v_size) {
    return pure_err_internal("invalid node numbers");
  }
  if (a_cap > maxoff) {
    return pure_err_internal("mismatch in storage offsets");
  }
  glp_maxflow_lp(glpobj->lp, graphobj->graph, names, s, t, a_cap);
  return pure_void;
}

pure_expr *glpk_maxflow_ffalg(pure_expr *ptr, int s, int t, int a_cap,
                              int a_x, int v_cut)
{
  // Solve maximum flow problem with Ford-Fulkerson algorithm
  int maxoff, dbl;
  double sol;
  int stat;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  dbl = sizeof(double);
  maxoff = graphobj->graph->a_size - dbl;
  if (s < 1 || t < 1 || s > graphobj->graph->v_size ||
      t > graphobj->graph->v_size) {
    return pure_err_internal("invalid node numbers");
  }
  if (a_cap > maxoff || a_x > maxoff ||
      abs(a_cap - a_x) < dbl ||
      v_cut > (graphobj->graph->v_size - dbl)) {
    return pure_err_internal("mismatch in storage offsets");
  }
  stat = glp_maxflow_ffalg(graphobj->graph, s, t, a_cap, &sol, a_x, v_cut);
  return pure_tuplel(2, pure_int(stat), pure_double(sol));
}

pure_expr *glpk_rmfgen(pure_expr *ptr, int a_cap, pure_expr *parm)
{
  // Goldfarb's maximum flow problem generator
  int maxoff, dbl;
  size_t cnt, i;
  int params[6], ipar, s, t, stat;
  pure_expr **tuple, *res;
  graph_obj *graphobj;
  if (!is_graph_pointer(ptr, &graphobj)) {
    return 0;
  }
  dbl = sizeof(double);
  maxoff = graphobj->graph->a_size - dbl;
  if (a_cap > maxoff) {
    return pure_err_internal("mismatch in storage offsets");
  }
  if (!pure_is_tuplev(parm, &cnt, &tuple)) {
    return 0;
  }
  if (cnt != 5) {
    free(tuple);
    return pure_err_internal("there must be exactly 5 parametres");
  }
  for (i = 0; i < 5; i++) {
    if (!pure_is_int(tuple[i], &ipar)) {
      free(tuple);
      return pure_err_internal("non-integer parameter");
    }
    params[i + 1] = ipar;
  }
  stat = glp_rmfgen(graphobj->graph, &s, &t, a_cap, params);
  res = pure_tuplel(3, pure_int(stat), pure_int(s), pure_int(t));
  free(tuple);
  return res;
}

pure_expr *glpk_version()
{
  // Determine library version
  return pure_cstring_dup(glp_version());
}

pure_expr *glpk_term_out(int sw)
{
  // Enable/disable terminal output
  return pure_int(glp_term_out(sw));
}

static int term_callback(void *info, const char *s)
{
  // Terminal output callback routine
  int ret;
  pure_expr *res;
  res = pure_app(pure_app(pure_symbol(pure_sym("glp::term_cb")),
                                      pure_pointer(info)),
                                      pure_cstring_dup(s));
  pure_is_int(res, &ret);
  return ret;
}

pure_expr *glpk_term_hook(int sw, void *info)
{
  // Enable/disable the terminal hook routine
  if (sw) glp_term_hook(term_callback, info);
  else glp_term_hook(NULL, NULL);
  return pure_void;
}

pure_expr *glpk_mem_usage()
{
  // Get memory usage information

#if GLP_MAJOR_VERSION == 4 && GLP_MINOR_VERSION < 48
  glp_long total, tpeak;
  mpz_t totlo, tothi, tplo, tphi;
  int count, cpeak;
  pure_expr *res;
  glp_mem_usage(&count, &cpeak, &total, &tpeak);
  mpz_init(totlo);
  mpz_init(tothi);
  mpz_init(tplo);
  mpz_init(tphi);
  mpz_set_ui(totlo, (unsigned long int)total.lo);
  mpz_set_ui(tothi, (unsigned long int)total.hi);
  mpz_set_ui(tplo, (unsigned long int)tpeak.lo);
  mpz_set_ui(tphi, (unsigned long int)tpeak.hi);
  res = pure_tuplel(6, pure_int(count), pure_int(cpeak),
                       pure_mpz(tothi), pure_mpz(totlo),
                       pure_mpz(tphi), pure_mpz(tplo));
  mpz_clear(totlo);
  mpz_clear(tothi);
  mpz_clear(tplo);
  mpz_clear(tphi);
  return res;
#else    
  size_t total, tpeak;
  mpz_t tot, tp;
  int count, cpeak;
  pure_expr *res;
  glp_mem_usage(&count, &cpeak, &total, &tpeak);
  mpz_init(tot);
  mpz_init(tp);
  mpz_set_ui(tot, total);
  mpz_set_ui(tp, tpeak);
  res = pure_tuplel(4, pure_int(count), pure_int(cpeak),
                       pure_mpz(tot), pure_mpz(tp));
  mpz_clear(tot);
  mpz_clear(tp);
  return res;
#endif
}

pure_expr *glpk_mem_limit(int limit)
{
  // Set memory usage limit
  glp_mem_limit(limit);
  return pure_void;
}

pure_expr *glpk_free_env()
{
  // Free GLPK library environment
  glp_free_env();
  return pure_void;
}

#ifdef __cplusplus
}
#endif

