#include "pure-gnumeric.h"
#include "pure-loader.h"
#include <glib/gi18n-lib.h>
#include <assert.h>

#include <gmp.h>

/* GSL-compatible matrix structs, cf. gsl_structs.h in the interpreter
   source. */

typedef struct _gsl_block
{
  size_t size;
  double *data;
} gsl_block;

typedef struct _gsl_matrix
{
  size_t size1;
  size_t size2;
  size_t tda;
  double *data;
  gsl_block *block;
  int owner;
} gsl_matrix;

typedef struct _gsl_block_int
{
  size_t size;
  int *data;
} gsl_block_int;

typedef struct _gsl_matrix_int
{
  size_t size1;
  size_t size2;
  size_t tda;
  int *data;
  gsl_block_int *block;
  int owner;
} gsl_matrix_int;

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

static gsl_matrix* 
gsl_matrix_alloc(const size_t n1, const size_t n2)
{
  gsl_block* block;
  gsl_matrix* m;
  if (n1 == 0 || n2 == 0)
    return 0;
  m = (gsl_matrix*)malloc(sizeof(gsl_matrix));
  if (m == 0)
    return 0;
  block = (gsl_block*)malloc(sizeof(gsl_block));
  if (block == 0) {
    free(m);
    return 0;
  }
  block->size = n1*n2;
  block->data = (double*)malloc(block->size*sizeof(double));
  if (block->data == 0) {
    free(m);
    free(block);
    return 0;
  }
  m->data = block->data;
  m->size1 = n1;
  m->size2 = n2;
  m->tda = n2; 
  m->block = block;
  m->owner = 1;
  return m;
}

static gsl_matrix*
gsl_matrix_calloc(const size_t n1, const size_t n2)
{
  gsl_matrix* m = gsl_matrix_alloc(n1, n2);
  if (m == 0) return 0;
  memset(m->data, 0, m->block->size*sizeof(double));
  return m;
}

static gsl_matrix_symbolic* 
gsl_matrix_symbolic_alloc(const size_t n1, const size_t n2)
{
  gsl_block_symbolic* block;
  gsl_matrix_symbolic* m;
  if (n1 == 0 || n2 == 0)
    return 0;
  m = (gsl_matrix_symbolic*)malloc(sizeof(gsl_matrix_symbolic));
  if (m == 0)
    return 0;
  block = (gsl_block_symbolic*)malloc(sizeof(gsl_block_symbolic));
  if (block == 0) {
    free(m);
    return 0;
  }
  block->size = n1*n2;
  block->data = (pure_expr**)malloc(block->size*sizeof(pure_expr*)) ;
  if (block->data == 0) {
    free(m);
    free(block);
    return 0;
  }
  m->data = block->data;
  m->size1 = n1;
  m->size2 = n2;
  m->tda = n2; 
  m->block = block;
  m->owner = 1;
  m->q = 0;
  return m;
}

static gsl_matrix_symbolic*
gsl_matrix_symbolic_calloc(const size_t n1, const size_t n2)
{
  gsl_matrix_symbolic* m = gsl_matrix_symbolic_alloc(n1, n2);
  if (m == 0) return 0;
  memset(m->data, 0, m->block->size*sizeof(pure_expr*));
  return m;
}

static void gsl_matrix_symbolic_free(gsl_matrix_symbolic *m)
{
  if (m->owner) free(m->block);
  free(m);
}

static inline gsl_matrix_symbolic*
create_symbolic_matrix(size_t nrows, size_t ncols)
{
  if (nrows == 0 || ncols == 0 ) {
    size_t nrows1 = (nrows>0)?nrows:1;
    size_t ncols1 = (ncols>0)?ncols:1;
    gsl_matrix_symbolic *m = gsl_matrix_symbolic_calloc(nrows1, ncols1);
    if (!m) return 0;
    m->size1 = nrows; m->size2 = ncols;
    return m;
  } else
    return gsl_matrix_symbolic_alloc(nrows, ncols);
}

static inline gsl_matrix*
create_double_matrix(size_t nrows, size_t ncols)
{
  if (nrows == 0 || ncols == 0 ) {
    size_t nrows1 = (nrows>0)?nrows:1;
    size_t ncols1 = (ncols>0)?ncols:1;
    gsl_matrix *m = gsl_matrix_calloc(nrows1, ncols1);
    if (!m) return 0;
    m->size1 = nrows; m->size2 = ncols;
    return m;
  } else
    return gsl_matrix_alloc(nrows, ncols);
}

/* NOTE: This assumes that Gnumeric represents all strings as UTF-8 internally
   (as does Pure), so that no encoding conversions are necessary. */

static pure_expr *rangeref2tuple(const GnmEvalPos *pos, const GnmRangeRef *rr)
{
  Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
  if (rr->a.col == rr->b.col && rr->a.row == rr->b.row)
    return pure_tuplel(3, pure_pointer(sheet),
		       pure_int(rr->a.col), pure_int(rr->a.row));
  else
    return pure_tuplel(5, pure_pointer(sheet),
		       pure_int(rr->a.col), pure_int(rr->a.row),
		       pure_int(rr->b.col), pure_int(rr->b.row));
}

static GnmValue *tuple2rangeref(const GnmEvalPos *pos, pure_expr *x)
{
  pure_expr **xs = NULL;
  size_t n;
  void *p;
  int x1, y1, x2, y2;
  if (pure_is_tuplev(x, &n, &xs) && n >= 3 && pure_is_pointer(xs[0], &p) &&
      pure_is_int(xs[1], &x1) && pure_is_int(xs[2], &y1) && x1>=0 && y1>=0) {
    Sheet *sheet = eval_sheet((Sheet*)p, pos->sheet);
    GnmRangeRef rr;
    if (n == 3) {
      x2 = x1; y2 = y1;
    } else if (!(n == 5 && pure_is_int(xs[3], &x2) &&
		 pure_is_int(xs[4], &y2) && x2>=0 && y2>=0)) {
      free(xs);
      return NULL;
    }
    free(xs);
    // handle inversion
    if (x1 > x2) {
      int x = x1; x1 = x2; x2 = x;
    }
    if (y1 > y2) {
      int y = y1; y1 = y2; y2 = y;
    }
    gnm_cellref_init(&rr.a, sheet, x1, y1, FALSE);
    gnm_cellref_init(&rr.b, sheet, x2, y2, FALSE);
    return value_new_cellrange_unsafe(&rr.a, &rr.b);
  } else {
    if (xs) free(xs);
    return NULL;
  }
}

static char *rangeref2str(const GnmEvalPos *pos, const GnmRangeRef *rr)
{
  GnmParsePos pp;
  GnmConventionsOut out;
  char *buf, *bufp; const char *p;
  out.accum = g_string_new(NULL);
  out.pp    = parse_pos_init_evalpos(&pp, pos);
  out.convs = gnm_conventions_default;
  rangeref_as_string(&out, rr);
  /* Do some cosmetic surgery. Range references are always absolute here, so
     we can remove the redundant '$' signs. */
  p = out.accum->str;
  bufp = buf = alloca(strlen(p)+1);
  for (; *p; p++)
    if (*p != '$')
      *(bufp++) = *p;
  *bufp = '\0';
  g_string_free(out.accum, TRUE);
  return strdup(buf);
}

static GnmValue *str2rangeref(const GnmEvalPos *pos, const char *s)
{
  GnmParsePos pp;
  GnmValue *res = NULL;
  GnmConventions const *convs = gnm_conventions_default;
  GnmExprTop const *texpr = gnm_expr_parse_str
    (s, parse_pos_init_evalpos(&pp, pos),
     GNM_EXPR_PARSE_FORCE_ABSOLUTE_REFERENCES, convs, NULL);
  if (texpr != NULL) {
    res = gnm_expr_top_get_range(texpr);
    gnm_expr_top_unref(texpr);
  }
  return res;
}

pure_expr *
value2pure(const GnmEvalPos *pos, const GnmValue *v, const char *spec)
{
  switch (v->v_any.type) {
  case VALUE_ERROR:
    return pure_app(pure_symbol(pure_sym("gnm_error")),
		    pure_string_dup(v->v_err.mesg->str));
  case VALUE_EMPTY:
    return pure_tuplel(0);
  case VALUE_BOOLEAN:
    return pure_int(v->v_bool.val);
  case VALUE_FLOAT:
    return pure_double(value_get_as_float(v));
  case VALUE_STRING:
    return pure_string_dup(v->v_str.val->str);
  case VALUE_ARRAY: {
    size_t nrows = (size_t)v->v_array.y, ncols = (size_t)v->v_array.x, i, j;
    bool dblmat = true;
    for (i = 0; dblmat && i < nrows; i++)
      for (j = 0; dblmat && j < ncols; j++) {
	gint x = (gint)j, y = (gint)i;
	const GnmValue *val = v->v_array.vals[x][y];
	if (val->v_any.type != VALUE_FLOAT)
	  dblmat = false;
      }
    if (dblmat) {
      // Double matrix.
      gsl_matrix *mat = create_double_matrix(nrows, ncols);
      if (!mat)
	return NULL;
      else {
	double *data = mat->data;
	size_t tda = mat->tda;
	for (i = 0; i < nrows; i++)
	  for (j = 0; j < ncols; j++) {
	    gint x = (gint)j, y = (gint)i;
	    const GnmValue *val = v->v_array.vals[x][y];
	    data[i*tda+j] = value_get_as_float(val);
	  }
	return pure_double_matrix(mat);
      }
    } else {
      // Different value types, create a symbolic matrix.
      gsl_matrix_symbolic *mat = create_symbolic_matrix(nrows, ncols);
      if (!mat)
	return NULL;
      else {
	pure_expr **data = mat->data;
	size_t tda = mat->tda;
	for (i = 0; i < nrows; i++)
	  for (j = 0; j < ncols; j++) {
	    gint x = (gint)j, y = (gint)i;
	    pure_expr *val = value2pure(pos, v->v_array.vals[x][y], NULL);
	    if (!val) {
	      size_t i1, j1;
	      for (i1 = 0; i1 < i; i1++)
		for (j1 = 0; j1 < ncols; j1++)
		  pure_freenew(data[i1*tda+j1]);
	      for (j1 = 0; j1 < j; j1++)
		pure_freenew(data[i*tda+j1]);
	      gsl_matrix_symbolic_free(mat);
	      return NULL;
	    }
	    data[i*tda+j] = val;
	  }
	return pure_symbolic_matrix(mat);
      }
    }
  }
  case VALUE_CELLRANGE: {
    GnmRangeRef const *rr = &v->v_range.cell;
    if (spec && *spec == 'r') {
      // Pass range reference as string.
      char *s = rangeref2str(pos, rr);
      return s?pure_string(s):NULL;
    } else {
      Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
      int x, y;
      int x1 = gnm_cellref_get_col(&rr->a, pos),
	y1 = gnm_cellref_get_row(&rr->a, pos),
	x2 = gnm_cellref_get_col(&rr->b, pos),
	y2 = gnm_cellref_get_row(&rr->b, pos);
      size_t nrows = y2-y1+1, ncols = x2-x1+1, i, j;
      bool dblmat = true;
      if (!sheet) return NULL;
      for (x = x1; x <= x2; x++)
	for (y = y1; y <= y2; y++) {
	  const GnmCell *cell = sheet_cell_get(sheet, x, y);
	  if (cell && cell->value->v_any.type != VALUE_FLOAT)
	    dblmat = false;
	}
      if (dblmat) {
	// Double matrix.
	gsl_matrix *mat = create_double_matrix(nrows, ncols);
	if (!mat)
	  return NULL;
	else {
	  double *data = mat->data;
	  size_t tda = mat->tda;
	  for (i = 0, y = y1; i < nrows; i++, y++)
	    for (j = 0, x = x1; j < ncols; j++, x++) {
	      const GnmCell *cell = sheet_cell_get(sheet, x, y);
	      data[i*tda+j] = cell?value_get_as_float(cell->value):0.0;
	    }
	  return pure_double_matrix(mat);
	}
      } else {
	// Different value types, create a symbolic matrix.
	gsl_matrix_symbolic *mat = create_symbolic_matrix(nrows, ncols);
	if (!mat)
	  return NULL;
	else {
	  pure_expr **data = mat->data;
	  size_t tda = mat->tda;
	  for (i = 0, y = y1; i < nrows; i++, y++)
	    for (j = 0, x = x1; j < ncols; j++, x++) {
	      const GnmCell *cell = sheet_cell_get(sheet, x, y);
	      pure_expr *val = cell?value2pure(pos, cell->value, NULL):
		pure_tuplel(0);
	      if (!val) {
		size_t i1, j1;
		for (i1 = 0; i1 < i; i1++)
		  for (j1 = 0; j1 < ncols; j1++)
		    pure_freenew(data[i1*tda+j1]);
		for (j1 = 0; j1 < j; j1++)
		  pure_freenew(data[i*tda+j1]);
		gsl_matrix_symbolic_free(mat);
		return NULL;
	      }
	      data[i*tda+j] = val;
	    }
	  return pure_symbolic_matrix(mat);
	}
      }
    }
  }
  default:
    return NULL;
  }
}

/* Standard Gnumeric errors. We invoke the appropriate error value routines
   for these, as they may have to be translated to the host locale. */

static struct {
  char const *C_name;
  GnmValue *(*err_fun)(GnmEvalPos const *pos);
} standard_errors[] = {
  { N_("#NULL!"), value_new_error_NULL },
  { N_("#DIV/0!"), value_new_error_DIV0 },
  { N_("#VALUE!"), value_new_error_VALUE },
  { N_("#REF!"), value_new_error_REF },
  { N_("#NAME?"), value_new_error_NAME },
  { N_("#NUM!"), value_new_error_NUM },
  { N_("#N/A"), value_new_error_NA },
  { NULL, NULL }
};

GnmValue *
pure2value(const GnmEvalPos *pos, pure_expr *x, const char *spec)
{
  int32_t iv, f;
  double dv;
  const char *s;
  size_t sz;
  pure_expr **xv;
  void *p;
  pure_expr *fun, *arg;
  GnmValue *v;
  if (!x)
    v = NULL;
  else if (pure_is_app(x, &fun, &arg) &&
	   pure_is_symbol(fun, &f) &&
	   strcmp(pure_sym_pname(f), "gnm_error") == 0 &&
	   pure_is_string(arg, &s)) {
    int i;
    for (i = 0;
	 standard_errors[i].C_name && strcmp(standard_errors[i].C_name, s);
	 i++) ;
    if (standard_errors[i].C_name)
      v = standard_errors[i].err_fun(pos);
    else
      v = value_new_error(pos, s);
  } else if (pure_is_int(x, &iv))
    v = value_new_int(iv);
  else if (pure_is_mpz(x, NULL))
    v = value_new_float((gnm_float)mpz_get_d(x->data.z));
  else if (pure_is_double(x, &dv))
    v = value_new_float((gnm_float)dv);
  else if (pure_is_string(x, &s)) {
    if (spec && *spec == 'r') {
      GnmValue *res = str2rangeref(pos, s);
      return (res != NULL) ? res : value_new_error_REF(pos);
    } else
      v = value_new_string(s);
  } else if (pure_is_double_matrix(x, &p)) {
    gsl_matrix *mat = (gsl_matrix*)p;
    double *data = mat->data;
    size_t i, j, nrows = mat->size1, ncols = mat->size2, tda = mat->tda;
    if (nrows==0 || ncols==0)
      v = value_new_empty();
    else {
      v = value_new_array_empty(ncols, nrows);
      for (i = 0; i < nrows; i++)
	for (j = 0; j < ncols; j++) {
	  gint x = (gint)j, y = (gint)i;
	  GnmValue *val = value_new_float((gnm_float)data[i*tda+j]);
	  v->v_array.vals[x][y] = val;
	}
    }
  } else if (pure_is_int_matrix(x, &p)) {
    gsl_matrix_int *mat = (gsl_matrix_int*)p;
    int *data = mat->data;
    size_t i, j, nrows = mat->size1, ncols = mat->size2, tda = mat->tda;
    if (nrows==0 || ncols==0)
      v = value_new_empty();
    else {
      v = value_new_array_empty(ncols, nrows);
      for (i = 0; i < nrows; i++)
	for (j = 0; j < ncols; j++) {
	  gint x = (gint)j, y = (gint)i;
	  GnmValue *val = value_new_int(data[i*tda+j]);
	  v->v_array.vals[x][y] = val;
	}
    }
  } else if (pure_is_symbolic_matrix(x, &p)) {
    gsl_matrix_symbolic *mat = (gsl_matrix_symbolic*)p;
    pure_expr **data = mat->data;
    size_t i, j, nrows = mat->size1, ncols = mat->size2, tda = mat->tda;
    if (nrows==0 || ncols==0)
      v = value_new_empty();
    else {
      v = value_new_array_empty(ncols, nrows);
      for (i = 0; i < nrows; i++)
	for (j = 0; j < ncols; j++) {
	  gint x = (gint)j, y = (gint)i;
	  GnmValue *val = pure2value(pos, data[i*tda+j], NULL);
	  if (!val) val = value_new_error_VALUE(pos);
	  v->v_array.vals[x][y] = val;
	}
    }
  } else if (pure_is_listv(x, &sz, &xv)) {
    if (sz==0)
      v = value_new_empty();
    else {
      size_t i;
      v = value_new_array_empty(sz, 1);
      for (i = 0; i < sz; i++) {
	GnmValue *val = pure2value(pos, xv[i], NULL);
	if (!val) val = value_new_error_VALUE(pos);
	v->v_array.vals[i][0] = val;
      }
      free(xv);
    }
  } else if (pure_is_tuplev(x, &sz, NULL) && (sz==0 || sz>1)) {
    if (sz==0)
      v = value_new_empty();
    else {
      size_t i;
      (void)pure_is_tuplev(x, &sz, &xv);
      v = value_new_array_empty(sz, 1);
      for (i = 0; i < sz; i++) {
	GnmValue *val = pure2value(pos, xv[i], NULL);
	if (!val) val = value_new_error_VALUE(pos);
	v->v_array.vals[i][0] = val;
      }
      free(xv);
    }
  } else {
    /* Convert anything else to a string in Pure syntax. */
    char *s = str(x);
    if (s) {
      v = value_new_string(s);
      free(s);
    } else
      v = NULL;
  }
  return v;
}

typedef struct {
  const GnmFuncEvalInfo *ei;
  pure_expr **args;
  int nargs, min, max;
} MyEvalInfo;

// These should be TLD when the Pure interpreter becomes multithreaded.
static MyEvalInfo *eval_info;
static unsigned ds_id, gl_id;

#ifndef _WIN32
static pure_expr *eval_expr(void)
{
  if (eval_info) {
    GnmFunc const *func = eval_info->ei->func_call->func;
    pure_expr *x = pure_quoted_symbol(pure_sym(func->name));
    if (eval_info->min != eval_info->max)
      x = pure_app
	(pure_appv(x, eval_info->min, eval_info->args),
	 pure_listv(eval_info->nargs-eval_info->min,
		    eval_info->args+eval_info->min));
    else
      x = pure_appv(x, eval_info->nargs, eval_info->args);
    return x;
  } else
    return NULL;
}
#endif

static inline const char *skip(const char *spec)
{
  if (!spec) return NULL;
  while (*spec == '|') ++spec;
  return spec;
}

#define next(spec) (spec?(*spec?skip(++spec):spec):NULL)

GnmValue *
call_pure_function(GnmFuncEvalInfo *ei, gint n_args,
		   GnmValue const * const *argv)
{
  GnmFunc const *func = ei->func_call->func;
  const char *spec = (func->fn_type==GNM_FUNC_TYPE_ARGS)
    ?skip(func->fn.args.arg_spec):NULL;
  int i, j, min, max;
  pure_expr *x, *e, **args;
  GnmValue *ret;

  if (n_args < 0) {
    function_def_count_args(func, &min, &max);
    n_args = max;
  } else {
    min = 0; max = n_args;
  }

  if (n_args < 0 || min<0 || max<0 || max<min)
    // This shouldn't happen.
    return NULL;

  args = g_new(pure_expr*, n_args);

  for (i = 0; i < n_args && argv[i] != NULL; i++, next(spec)) {
    args[i] = value2pure(ei->pos, argv[i], spec);
    if (!args[i]) {
      for (j = 0; j < i; j++) pure_free(args[j]);
      g_free(args);
      ret = value_new_error_VALUE(ei->pos);
      return ret;
    } else
      pure_new(args[i]);
  }

  if (i < min) {
    // Not enough arguments. This shouldn't happen, so we just bail out here.
    for (j = 0; j < i; j++) pure_free(args[j]);
    g_free(args);
    return NULL;
  }

  { MyEvalInfo new_info = { ei, args, i, min, max };
    // Save the old context in case we're invoked recursively.
    MyEvalInfo *save_info = eval_info;
    eval_info = &new_info; if (!save_info) ds_id = gl_id = 0;
    if ((x = pure_symbolx(pure_sym(func->name), &e))) {
      if (min != max) {
	// Variadic function, pass extra arguments as a list.
	x = pure_appxv(x, min, args, &e);
	if (x)
	  x = pure_appx(x, pure_listv(i-min, args+min), &e);
      } else
	x = pure_appxv(x, i, args, &e);
    }
    if (x) pure_ref(x); else if (e) pure_freenew(e);
    for (j = 0; j < i; j++) pure_free(args[j]);
    g_free(args);
    if (x) pure_unref(x);
    // Restore the old context.
    eval_info = save_info; if (!save_info) ds_id = gl_id = 0;
  }

  if (x) {
    ret = pure2value(ei->pos, x, NULL);
    pure_freenew(x);
    if (!ret) ret = value_new_error_VALUE(ei->pos);
  } else
    ret = value_new_error_NULL(ei->pos);
  return ret;
}

/* Note that unlike Gnumeric we don't try to coerce anything here, we only
   check the arguments for compatibility, so the caller has to make sure that
   argument types really match up. In particular, if a Gnumeric function wants
   a boolean or a number then you really have to provide one. */

static bool compat(GnmValue *v, const char *spec)
{
  // any value
  if (!spec || !*spec || *spec == '?')
    return true;
  // errors or scalars
  if (*spec == 'E')
    return v->v_any.type != VALUE_ARRAY && v->v_any.type != VALUE_CELLRANGE;
  // we don't allow any errors past this point
  if (v->v_any.type == VALUE_ERROR)
    return false;
  // ordinary scalars
  if (*spec == 'S')
    return v->v_any.type != VALUE_ARRAY && v->v_any.type != VALUE_CELLRANGE;
  // we don't allow any empty values past this point
  /* XXXFIXME: We should actually allow empty values for optional arguments,
     but we don't keep track of this right now. */
  if (v->v_any.type == VALUE_EMPTY)
    return false;
  switch (v->v_any.type) {
  case VALUE_BOOLEAN:
  case VALUE_FLOAT:
    // Gnumeric doesn't actually distinguish these.
    return *spec == 'b' || *spec == 'f';
  case VALUE_STRING:
    return *spec == 's';
  case VALUE_ARRAY:
    return *spec == 'A';
  case VALUE_CELLRANGE:
    return *spec == 'r';
  default:
    return false;
  }
}

pure_expr *
pure_gnmcall(const char *name, pure_expr *args)
{
  GnmFunc *func = gnm_func_lookup(name, NULL);
  const char *spec = (func->fn_type==GNM_FUNC_TYPE_ARGS)
    ?skip(func->fn.args.arg_spec):NULL;
  GnmValue **val, *ret_val;
  gint n_args, i, j;
  size_t n;
  pure_expr **xv, *ret;
  if (!func || !eval_info || !eval_info->ei) return NULL;
  if (!pure_is_listv(args, &n, &xv)) return NULL;
  n_args = (gint)n;
  val = g_new(GnmValue*, n_args);
  for (i = 0; i < n_args; i++, next(spec)) {
    val[i] = pure2value(eval_info->ei->pos, xv[i], spec);
    if (!val[i] || !compat(val[i], spec)) {
      for (j = 0; j < i; j++)
	value_release(val[j]);
      if (val[i]) value_release(val[i]);
      if (xv) free(xv);
      g_free(val);
      return NULL;
    }
  }
  if (xv) free(xv);
  ret_val = function_def_call_with_values(eval_info->ei->pos, func, n_args,
					  (GnmValue const * const *)val);
  ret = value2pure(eval_info->ei->pos, ret_val, NULL);
  value_release(ret_val);
  for (i = 0; i < n; i++)
    value_release(val[i]);
  g_free(val);
  return ret;
}

/* Support for asynchronous data sources (from sample_datasource.c). */

static inline bool is_nil(pure_expr *x)
{
  size_t n;
  return pure_is_listv(x, &n, NULL) && n==0;
}

static inline bool is_cons(pure_expr *x, pure_expr **y, pure_expr **z)
{
  pure_expr *f;
  size_t n;
  if (pure_is_appv(x, &f, &n, NULL) && n==2 &&
      strcmp(pure_sym_pname(f->tag), ":") == 0) {
    if (y) *y = x->data.x[0]->data.x[1];
    if (z) *z = x->data.x[1];
    return true;
  } else
    return false;
}

bool pure_write_blob(FILE *fp, const DepKey *key, pure_expr *x)
{
  pure_expr *b;
  // We should maybe encode these in binary.
  char buf[100];
  sprintf(buf, "%p-%p-%u", key->node, key->dep, key->id);
  if (x)
    b = pure_app(pure_symbol(pure_sym("blob")),
		 pure_listl(2, pure_cstring_dup(buf), x));
  else
    b = pure_app(pure_symbol(pure_sym("blob")),
		 pure_cstring_dup(buf));
  void *p;
  bool ret = false;
  if (b) pure_new(b);
  if (b && pure_is_pointer(b, &p)) {
    pure_expr *size = pure_app(pure_symbol(pure_sym("blob_size")), b);
    if (size) {
      size_t n = 0;
      int32_t iv;
      if (pure_is_int(size, &iv))
	n = (unsigned)iv;
      else if (pure_is_mpz(size, NULL))
	n = mpz_get_ui(size->data.z);
      if (n > 0) {
	ret = fwrite(&n, sizeof(size_t), 1, fp) == 1 &&
	  fwrite(p, 1, n, fp) == n;
#if 0
	fprintf(stderr, "%p-%p-%u: wrote %lu bytes, ret = %d\n",
		key->node, key->dep, key->id, (unsigned long)n, (int)ret);
#endif
      }
      pure_freenew(size);
    }
  }
  if (b) pure_free(b);
  fflush(fp);
  return ret;
}

bool pure_read_blob(FILE *fp, DepKey *key, pure_expr **x)
{
  size_t n, m;
  void *buf;
  pure_expr *y, **xv = NULL;
  const char *s;
  if (fread(&n, sizeof(size_t), 1, fp) != 1)
    return false;
  buf = malloc(n);
  if (!buf)
    return false;
  m = 0;
  while (m < n && !feof(fp)) {
    size_t k = fread(buf+m, 1, n, fp);
    m += k;
    if (k == 0) g_usleep(100);
  }
  if (m < n) {
    fprintf(stderr, "** pure_read_blob: read failed\n");
    free(buf);
    return false;
  }
  y = pure_app(pure_symbol(pure_sym("val")), pure_pointer(buf));
  free(buf);
  if (!y) {
    fprintf(stderr, "** pure_read_blob: invalid blob\n");
    return false;
  }
  if (pure_is_string(y, &s) ||
      (pure_is_listv(y, &n, &xv) && n == 2 && pure_is_string(xv[0], &s))) {
    if (sscanf(s, "%p-%p-%u", &key->node, &key->dep, &key->id) < 3) {
      pure_freenew(y);
      if (xv) free(xv);
      fprintf(stderr, "** pure_read_blob: bad blob format\n");
      return false;
    }
    if (xv) {
      *x = xv[1];
      pure_ref(*x);
      free(xv);
    } else
      *x = NULL;
    pure_freenew(y);
    if (*x) pure_unref(*x);
    return true;
  } else {
    pure_freenew(y);
    if (xv) free(xv);
    fprintf(stderr, "** pure_read_blob: bad blob format\n");
    return false;
  }
}

#ifndef _WIN32
static void out(FILE *fp, DepKey *key, pure_expr *x)
{
  if (!pure_write_blob(fp, key, x))
    // Write error, bail out.
    exit(1);
}
#endif

pure_expr *pure_datasource(pure_expr *x)
{
#ifndef _WIN32
  int pid;
  pure_expr *ret;
  if (!x || !eval_info || !eval_info->ei || !pure_async_filename)
    return NULL;
  else {
    DepKey key = { eval_info->ei->func_call, eval_info->ei->pos->dep, ds_id };
    if (!pure_async_func_init(eval_info->ei, eval_expr(), ds_id++, &ret)) {
      if (!ret)
	ret = pure_app(pure_symbol(pure_sym("gnm_error")),
		       pure_string_dup("#N/A"));
      return ret;
    } else if ((pid = fork()) == 0) {
      /* child */
      FILE *pure_async_file = fopen(pure_async_filename, "ab");
      /* evaluate expression, and write results to the pipe */
      pure_expr *force = pure_new(pure_symbol(pure_sym("::force")));
      pure_expr *e, *u = pure_appx(force, pure_new(x), &e), *y, *z;
#if 0
      fprintf(stderr, "[%d] child: %p-%p-%u\n", getpid(),
	      key.node, key.dep, key.id);
#endif
      while (u && is_cons(u, &y, &z)) {
	y = pure_appx(force, y, &e);
	out(pure_async_file, &key, y);
	z = pure_appx(force, z, &e);
	if (!z) exit(0); // exception
	pure_new(z);
	pure_free(u);
	u = z;
      }
      if (u && !is_nil(u))
	out(pure_async_file, &key, u);
      exit(0);
    } else if (pid > 0) {
      /* parent */
#if 0
      fprintf(stderr, "[%d] started child [%d]: %p-%p-%u\n", getpid(), pid,
	      key.node, key.dep, key.id);
#endif
      pure_async_func_process(eval_info->ei, key.id, pid);
      if (!ret)
	ret = pure_app(pure_symbol(pure_sym("gnm_error")),
		       pure_string_dup("#N/A"));
      return ret;
    } else {
      perror("fork");
      return NULL;
    }
  }
#else
  return NULL;
#endif
}

#ifndef _WIN32
static pure_expr *try_trigger(int timeout, unsigned id,
			      pure_expr *cond, pure_expr *value,
			      pure_expr *data)
{
  int res;
  pure_expr *e, *ret = NULL;
  cond = pure_appx(cond, data, &e);
  if (cond) {
    if (pure_is_int(cond, &res)) {
      pure_freenew(cond);
      if (res) {
	/* The condition fired. */
	value = pure_appx(value, data, &e);
	if (value) {
	  pure_async_set_value(eval_info->ei, id, value);
	  ret = value;
	} else {
	  if (e) pure_freenew(e);
	  ret = pure_app(pure_symbol(pure_sym("gnm_error")),
			 pure_string_dup("#NULL"));
	  pure_async_set_value(eval_info->ei, id, ret);
	}
	if (timeout == 0) {
	  if (ret) pure_ref(ret);
	  pure_async_func_stop(eval_info->ei, id);
	  if (ret) pure_unref(ret);
	}
      }
    } else
      pure_freenew(cond);
  } else if (e)
    pure_freenew(e);
  return ret;
}
#endif

static pure_expr *NA_expr(void)
{
  return pure_app(pure_symbol(pure_sym("gnm_error")), pure_string_dup("#N/A"));
}

#define trigger_ret(ret) (ret?ret:NA_expr())

pure_expr *pure_trigger(int timeout, pure_expr *cond, pure_expr *value,
			pure_expr *data)
{
#ifndef _WIN32
  int pid;
  unsigned myid = ds_id;
  pure_expr *ret= try_trigger(timeout, myid, cond, value, data);
  if (ret) return ret;
  if (!cond || !value || !eval_info || !eval_info->ei || !pure_async_filename)
    return NULL;
  else {
    DepKey key = { eval_info->ei->func_call, eval_info->ei->pos->dep, ds_id };
    if (!pure_async_func_init(eval_info->ei, eval_expr(), ds_id++, &ret))
      return trigger_ret(ret);
    else if ((pid = fork()) == 0) {
      /* child */
      FILE *pure_async_file = fopen(pure_async_filename, "ab");
      /* output a steady stream of messages to the pipe */
#if 0
      fprintf(stderr, "[%d] child: %p-%p-%u\n", getpid(),
	      key.node, key.dep, key.id);
#endif
      if (timeout > 0)
	while (timeout-- > 0) {
	  sleep(1);
	  out(pure_async_file, &key, NULL);
	}
      else
	while (1) {
	  sleep(1);
	  out(pure_async_file, &key, NULL);
	}
      exit(0);
    } else if (pid > 0) {
      /* parent */
#if 0
      fprintf(stderr, "[%d] started child [%d]: %p-%p-%u\n", getpid(), pid,
	      key.node, key.dep, key.id);
#endif
      pure_async_func_process(eval_info->ei, key.id, pid);
      return trigger_ret(ret);
    } else {
      perror("fork");
      return NULL;
    }
  }
#else
  return NULL;
#endif
}

/* Retrieve and manipulate cell ranges. */

pure_expr *pure_this_cell(void)
{
  if (eval_info && eval_info->ei) {
    GnmRangeRef rr;
    char *s;
    gnm_cellref_init(&rr.a, NULL, 0, 0, TRUE);
    gnm_cellref_init(&rr.b, NULL, 0, 0, TRUE);
    s = rangeref2str(eval_info->ei->pos, &rr);
    return s?pure_string(s):NULL;
  } else
    return NULL;
}

pure_expr *pure_parse_range(const char *s)
{
  if (eval_info && eval_info->ei) {
    GnmValue *v = str2rangeref(eval_info->ei->pos, s);
    assert(!v || v->v_any.type == VALUE_CELLRANGE);
    if (v) {
      GnmRangeRef const *rr = &v->v_range.cell;
      pure_expr *x = rangeref2tuple(eval_info->ei->pos, rr);
      value_release(v);
      return x;
    } else
      return NULL;
  } else
    return NULL;
}

pure_expr *pure_make_range(pure_expr *x)
{
  if (eval_info && eval_info->ei) {
    GnmValue *v = tuple2rangeref(eval_info->ei->pos, x);
    assert(!v || v->v_any.type == VALUE_CELLRANGE);
    if (v) {
      GnmRangeRef const *rr = &v->v_range.cell;
      char *s = rangeref2str(eval_info->ei->pos, rr);
      value_release(v);
      return s?pure_string(s):NULL;
    } else
      return NULL;
  } else
    return NULL;
}

pure_expr *pure_get_cell(const char *s)
{
  const GnmEvalPos *pos =
    (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
  GnmValue *v = pos?str2rangeref(pos, s):NULL;
  if (v) {
    pure_expr *ret = NULL;
    GnmRangeRef const *rr = &v->v_range.cell;
    Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
    int x1 = gnm_cellref_get_col(&rr->a, pos),
      y1 = gnm_cellref_get_row(&rr->a, pos),
      x2 = gnm_cellref_get_col(&rr->b, pos),
      y2 = gnm_cellref_get_row(&rr->b, pos);
    size_t nrows = y2-y1+1, ncols = x2-x1+1;
    if (sheet && nrows == 1 && ncols == 1) {
      const GnmCell *cell = sheet_cell_get(sheet, x1, y1);
      ret = cell?value2pure(pos, cell->value, NULL):pure_tuplel(0);
    }
    value_release(v);
    return ret;
  } else
    return NULL;
}

pure_expr *pure_get_cell_text(const char *s)
{
  const GnmEvalPos *pos =
    (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
  GnmValue *v = pos?str2rangeref(pos, s):NULL;
  if (v) {
    pure_expr *ret = NULL;
    GnmRangeRef const *rr = &v->v_range.cell;
    Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
    int x1 = gnm_cellref_get_col(&rr->a, pos),
      y1 = gnm_cellref_get_row(&rr->a, pos),
      x2 = gnm_cellref_get_col(&rr->b, pos),
      y2 = gnm_cellref_get_row(&rr->b, pos);
    size_t nrows = y2-y1+1, ncols = x2-x1+1;
    if (sheet && nrows == 1 && ncols == 1) {
      const GnmCell *cell = sheet_cell_get(sheet, x1, y1);
      ret = cell?pure_string(gnm_cell_get_entered_text(cell)):
	pure_string_dup("");
    }
    value_release(v);
    return ret;
  } else
    return NULL;
}

pure_expr *pure_get_cell_format(const char *s)
{
  const GnmEvalPos *pos =
    (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
  GnmValue *v = pos?str2rangeref(pos, s):NULL;
  if (v) {
    pure_expr *ret = NULL;
    GnmRangeRef const *rr = &v->v_range.cell;
    Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
    int x1 = gnm_cellref_get_col(&rr->a, pos),
      y1 = gnm_cellref_get_row(&rr->a, pos),
      x2 = gnm_cellref_get_col(&rr->b, pos),
      y2 = gnm_cellref_get_row(&rr->b, pos);
    size_t nrows = y2-y1+1, ncols = x2-x1+1;
    if (sheet && nrows == 1 && ncols == 1) {
      const GnmCell *cell = sheet_cell_get(sheet, x1, y1);
      const GOFormat *fmt = cell?gnm_cell_get_format(cell):NULL;
      const char *fmtstr = fmt?go_format_as_XL(fmt):"";
      ret = pure_string_dup(fmtstr);
    }
    value_release(v);
    return ret;
  } else
    return NULL;
}

pure_expr *pure_set_cell(const char *s, pure_expr *x)
{
  const GnmEvalPos *pos =
    (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
  GnmValue *v = pos?str2rangeref(pos, s):NULL;
  if (v) {
    pure_expr *ret = NULL;
    GnmRangeRef const *rr = &v->v_range.cell;
    Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
    int x1 = gnm_cellref_get_col(&rr->a, pos),
      y1 = gnm_cellref_get_row(&rr->a, pos),
      x2 = gnm_cellref_get_col(&rr->b, pos),
      y2 = gnm_cellref_get_row(&rr->b, pos);
    size_t nrows = y2-y1+1, ncols = x2-x1+1;
    if (sheet && nrows == 1 && ncols == 1) {
      GnmCell *cell = sheet_cell_fetch(sheet, x1, y1);
      if (cell) {
	GnmValue *v = pure2value(pos, x, NULL);
	if (!v) v = value_new_error_VALUE(pos);
	sheet_cell_set_value(cell, v);
	ret = pure_tuplel(0);
     }
    }
    value_release(v);
    return ret;
  } else
    return NULL;
}

pure_expr *pure_set_cell_text(const char *s, pure_expr *x)
{
  const char *text;
  if (pure_is_string(x, &text)) {
    const GnmEvalPos *pos =
      (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
    GnmValue *v = pos?str2rangeref(pos, s):NULL;
    if (v) {
      pure_expr *ret = NULL;
      GnmRangeRef const *rr = &v->v_range.cell;
      Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
      int x1 = gnm_cellref_get_col(&rr->a, pos),
	y1 = gnm_cellref_get_row(&rr->a, pos),
	x2 = gnm_cellref_get_col(&rr->b, pos),
	y2 = gnm_cellref_get_row(&rr->b, pos);
      size_t nrows = y2-y1+1, ncols = x2-x1+1;
      if (sheet && nrows == 1 && ncols == 1) {
	GnmCell *cell = sheet_cell_fetch(sheet, x1, y1);
	if (cell) {
	  sheet_cell_set_text(cell, text, NULL);
	  ret = pure_tuplel(0);
	}
      }
      value_release(v);
      return ret;
    } else
      return NULL;
  } else
    return NULL;
}

pure_expr *pure_set_cell_format(const char *s, pure_expr *x)
{
  const char *fmtstr;
  if (pure_is_string(x, &fmtstr)) {
    const GnmEvalPos *pos =
      (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
    GnmValue *v = pos?str2rangeref(pos, s):NULL;
    if (v) {
      pure_expr *ret = NULL;
      GnmRangeRef const *rr = &v->v_range.cell;
      Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
      int x1 = gnm_cellref_get_col(&rr->a, pos),
	y1 = gnm_cellref_get_row(&rr->a, pos),
	x2 = gnm_cellref_get_col(&rr->b, pos),
	y2 = gnm_cellref_get_row(&rr->b, pos);
      size_t nrows = y2-y1+1, ncols = x2-x1+1;
      if (sheet && nrows == 1 && ncols == 1) {
	GnmCell *cell = sheet_cell_fetch(sheet, x1, y1);
	if (cell) {
	  gnm_cell_set_format(cell, fmtstr);
	  gnm_cell_unrender(cell);
	  ret = pure_tuplel(0);
	}
      }
      value_release(v);
      return ret;
    } else
      return NULL;
  } else
    return NULL;
}

pure_expr *pure_get_range(const char *s)
{
  const GnmEvalPos *pos = (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
  GnmValue *v = pos?str2rangeref(pos, s):NULL;
  if (v) {
    pure_expr *ret = NULL;
    GnmRangeRef const *rr = &v->v_range.cell;
    Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
    int x, y, x1 = gnm_cellref_get_col(&rr->a, pos),
      y1 = gnm_cellref_get_row(&rr->a, pos),
      x2 = gnm_cellref_get_col(&rr->b, pos),
      y2 = gnm_cellref_get_row(&rr->b, pos);
    size_t nrows = y2-y1+1, ncols = x2-x1+1, i, j;
    if (sheet) {
      bool dblmat = true;
      for (x = x1; x <= x2; x++)
	for (y = y1; y <= y2; y++) {
	  const GnmCell *cell = sheet_cell_get(sheet, x, y);
	  if (cell && cell->value->v_any.type != VALUE_FLOAT)
	    dblmat = false;
	}
      if (dblmat) {
	// Double matrix.
	gsl_matrix *mat = create_double_matrix(nrows, ncols);
	if (mat) {
	  double *data = mat->data;
	  size_t tda = mat->tda;
	  for (i = 0, y = y1; i < nrows; i++, y++)
	    for (j = 0, x = x1; j < ncols; j++, x++) {
	      const GnmCell *cell = sheet_cell_get(sheet, x, y);
	      data[i*tda+j] = cell?value_get_as_float(cell->value):0.0;
	    }
	  ret = pure_double_matrix(mat);
	}
      } else {
	// Different value types, create a symbolic matrix.
	gsl_matrix_symbolic *mat = create_symbolic_matrix(nrows, ncols);
	if (mat) {
	  pure_expr **data = mat->data;
	  size_t tda = mat->tda;
	  for (i = 0, y = y1; i < nrows; i++, y++)
	    for (j = 0, x = x1; j < ncols; j++, x++) {
	      const GnmCell *cell = sheet_cell_get(sheet, x, y);
	      pure_expr *val = cell?value2pure(pos, cell->value, NULL):
		pure_tuplel(0);
	      if (!val) {
		size_t i1, j1;
		for (i1 = 0; i1 < i; i1++)
		  for (j1 = 0; j1 < ncols; j1++)
		    pure_freenew(data[i1*tda+j1]);
		for (j1 = 0; j1 < j; j1++)
		  pure_freenew(data[i*tda+j1]);
		gsl_matrix_symbolic_free(mat);
		goto out;
	      }
	      data[i*tda+j] = val;
	    }
	  ret = pure_symbolic_matrix(mat);
	}
      }
    }
 out:
    value_release(v);
    return ret;
  } else
    return NULL;
}

pure_expr *pure_get_range_text(const char *s)
{
  const GnmEvalPos *pos = (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
  GnmValue *v = pos?str2rangeref(pos, s):NULL;
  if (v) {
    pure_expr *ret = NULL;
    GnmRangeRef const *rr = &v->v_range.cell;
    Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
    int x, y, x1 = gnm_cellref_get_col(&rr->a, pos),
      y1 = gnm_cellref_get_row(&rr->a, pos),
      x2 = gnm_cellref_get_col(&rr->b, pos),
      y2 = gnm_cellref_get_row(&rr->b, pos);
    size_t nrows = y2-y1+1, ncols = x2-x1+1, i, j;
    if (sheet) {
      gsl_matrix_symbolic *mat = create_symbolic_matrix(nrows, ncols);
      if (mat) {
	pure_expr **data = mat->data;
	size_t tda = mat->tda;
	for (i = 0, y = y1; i < nrows; i++, y++)
	  for (j = 0, x = x1; j < ncols; j++, x++) {
	    const GnmCell *cell = sheet_cell_get(sheet, x, y);
	    pure_expr *val = cell?pure_string(gnm_cell_get_entered_text(cell)):
	      pure_string_dup("");
	    if (!val) {
	      size_t i1, j1;
	      for (i1 = 0; i1 < i; i1++)
		for (j1 = 0; j1 < ncols; j1++)
		  pure_freenew(data[i1*tda+j1]);
	      for (j1 = 0; j1 < j; j1++)
		pure_freenew(data[i*tda+j1]);
	      gsl_matrix_symbolic_free(mat);
	      goto out;
	    }
	    data[i*tda+j] = val;
	  }
	ret = pure_symbolic_matrix(mat);
      }
    }
 out:
    value_release(v);
    return ret;
  } else
    return NULL;
}

pure_expr *pure_get_range_format(const char *s)
{
  const GnmEvalPos *pos = (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
  GnmValue *v = pos?str2rangeref(pos, s):NULL;
  if (v) {
    pure_expr *ret = NULL;
    GnmRangeRef const *rr = &v->v_range.cell;
    Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
    int x, y, x1 = gnm_cellref_get_col(&rr->a, pos),
      y1 = gnm_cellref_get_row(&rr->a, pos),
      x2 = gnm_cellref_get_col(&rr->b, pos),
      y2 = gnm_cellref_get_row(&rr->b, pos);
    size_t nrows = y2-y1+1, ncols = x2-x1+1, i, j;
    if (sheet) {
      gsl_matrix_symbolic *mat = create_symbolic_matrix(nrows, ncols);
      if (mat) {
	pure_expr **data = mat->data;
	size_t tda = mat->tda;
	for (i = 0, y = y1; i < nrows; i++, y++)
	  for (j = 0, x = x1; j < ncols; j++, x++) {
	    const GnmCell *cell = sheet_cell_get(sheet, x, y);
	    const GOFormat *fmt = cell?gnm_cell_get_format(cell):NULL;
	    const char *fmtstr = fmt?go_format_as_XL(fmt):"";
	    pure_expr *val = pure_string_dup(fmtstr);
	    if (!val) {
	      size_t i1, j1;
	      for (i1 = 0; i1 < i; i1++)
		for (j1 = 0; j1 < ncols; j1++)
		  pure_freenew(data[i1*tda+j1]);
	      for (j1 = 0; j1 < j; j1++)
		pure_freenew(data[i*tda+j1]);
	      gsl_matrix_symbolic_free(mat);
	      goto out;
	    }
	    data[i*tda+j] = val;
	  }
	ret = pure_symbolic_matrix(mat);
      }
    }
 out:
    value_release(v);
    return ret;
  } else
    return NULL;
}

pure_expr *pure_set_range(const char *s, pure_expr *xs)
{
  const GnmEvalPos *pos = (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
  GnmValue *v = pos?str2rangeref(pos, s):NULL;
  if (v) {
    pure_expr *ret = NULL, **xv;
    size_t sz;
    void *p;
    GnmRangeRef const *rr = &v->v_range.cell;
    Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
    int x, y, x1 = gnm_cellref_get_col(&rr->a, pos),
      y1 = gnm_cellref_get_row(&rr->a, pos),
      x2 = gnm_cellref_get_col(&rr->b, pos),
      y2 = gnm_cellref_get_row(&rr->b, pos);
    if (sheet) {
      if (pure_is_double_matrix(xs, &p)) {
	gsl_matrix *mat = (gsl_matrix*)p;
	double *data = mat->data;
	size_t i, j, nrows = mat->size1, ncols = mat->size2, tda = mat->tda;
	for (j = 0, x = x1; j < ncols && x <= x2; j++, x++)
	  for (i = 0, y = y1; i < nrows && y <= y2; i++, y++) {
	    GnmCell *cell = sheet_cell_fetch(sheet, x, y);
	    if (cell) {
	      GnmValue *v = value_new_float((gnm_float)data[i*tda+j]);
	      sheet_cell_set_value(cell, v);
	    }
	  }
	ret = pure_tuplel(0);
      } else if (pure_is_int_matrix(xs, &p)) {
	gsl_matrix_int *mat = (gsl_matrix_int*)p;
	int *data = mat->data;
	size_t i, j, nrows = mat->size1, ncols = mat->size2, tda = mat->tda;
	for (j = 0, x = x1; j < ncols && x <= x2; j++, x++)
	  for (i = 0, y = y1; i < nrows && y <= y2; i++, y++) {
	    GnmCell *cell = sheet_cell_fetch(sheet, x, y);
	    if (cell) {
	      GnmValue *v = value_new_int(data[i*tda+j]);
	      sheet_cell_set_value(cell, v);
	    }
	  }
	ret = pure_tuplel(0);
      } else if (pure_is_symbolic_matrix(xs, &p)) {
	gsl_matrix_symbolic *mat = (gsl_matrix_symbolic*)p;
	pure_expr **data = mat->data;
	size_t i, j, nrows = mat->size1, ncols = mat->size2, tda = mat->tda;
	for (j = 0, x = x1; j < ncols && x <= x2; j++, x++)
	  for (i = 0, y = y1; i < nrows && y <= y2; i++, y++) {
	    GnmCell *cell = sheet_cell_fetch(sheet, x, y);
	    if (cell) {
	      GnmValue *v = pure2value(pos, data[i*tda+j], NULL);
	      if (!v) v = value_new_error_VALUE(pos);
	      sheet_cell_set_value(cell, v);
	    }
	  }
	ret = pure_tuplel(0);
      } else if (pure_is_listv(xs, &sz, &xv) || pure_is_tuplev(xs, &sz, &xv)) {
	size_t j;
	for (j = 0, x = x1; j < sz && x <= x2; j++, x++) {
	  GnmCell *cell = sheet_cell_fetch(sheet, x, y1);
	  if (cell) {
	    GnmValue *v = pure2value(pos, xv[j], NULL);
	    if (!v) v = value_new_error_VALUE(pos);
	    sheet_cell_set_value(cell, v);
	  }
	}
	if (xv) free(xv);
	ret = pure_tuplel(0);
      }
    }
    value_release(v);
    return ret;
  } else
    return NULL;
}

pure_expr *pure_set_range_text(const char *s, pure_expr *xs)
{
  const GnmEvalPos *pos = (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
  GnmValue *v = pos?str2rangeref(pos, s):NULL;
  if (v) {
    pure_expr *ret = NULL, **xv;
    size_t sz;
    void *p;
    GnmRangeRef const *rr = &v->v_range.cell;
    Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
    int x, y, x1 = gnm_cellref_get_col(&rr->a, pos),
      y1 = gnm_cellref_get_row(&rr->a, pos),
      x2 = gnm_cellref_get_col(&rr->b, pos),
      y2 = gnm_cellref_get_row(&rr->b, pos);
    const char *text;
    if (sheet) {
      if (pure_is_symbolic_matrix(xs, &p)) {
	gsl_matrix_symbolic *mat = (gsl_matrix_symbolic*)p;
	pure_expr **data = mat->data;
	size_t i, j, nrows = mat->size1, ncols = mat->size2, tda = mat->tda;
	for (j = 0, x = x1; j < ncols && x <= x2; j++, x++)
	  for (i = 0, y = y1; i < nrows && y <= y2; i++, y++) {
	    GnmCell *cell = sheet_cell_fetch(sheet, x, y);
	    if (cell && pure_is_string(data[i*tda+j], &text)) {
	      sheet_cell_set_text(cell, text, NULL);
	    }
	  }
	ret = pure_tuplel(0);
      } else if (pure_is_listv(xs, &sz, &xv) || pure_is_tuplev(xs, &sz, &xv)) {
	size_t j;
	for (j = 0, x = x1; j < sz && x <= x2; j++, x++) {
	  GnmCell *cell = sheet_cell_fetch(sheet, x, y1);
	  if (cell && pure_is_string(xv[j], &text)) {
	    sheet_cell_set_text(cell, text, NULL);
	  }
	}
	if (xv) free(xv);
	ret = pure_tuplel(0);
      }
    }
    value_release(v);
    return ret;
  } else
    return NULL;
}

pure_expr *pure_set_range_format(const char *s, pure_expr *xs)
{
  const GnmEvalPos *pos = (eval_info && eval_info->ei)?eval_info->ei->pos:NULL;
  GnmValue *v = pos?str2rangeref(pos, s):NULL;
  if (v) {
    pure_expr *ret = NULL, **xv;
    size_t sz;
    void *p;
    GnmRangeRef const *rr = &v->v_range.cell;
    Sheet *sheet = eval_sheet(rr->a.sheet, pos->sheet);
    int x, y, x1 = gnm_cellref_get_col(&rr->a, pos),
      y1 = gnm_cellref_get_row(&rr->a, pos),
      x2 = gnm_cellref_get_col(&rr->b, pos),
      y2 = gnm_cellref_get_row(&rr->b, pos);
    const char *fmtstr;
    if (sheet) {
      if (pure_is_symbolic_matrix(xs, &p)) {
	gsl_matrix_symbolic *mat = (gsl_matrix_symbolic*)p;
	pure_expr **data = mat->data;
	size_t i, j, nrows = mat->size1, ncols = mat->size2, tda = mat->tda;
	for (j = 0, x = x1; j < ncols && x <= x2; j++, x++)
	  for (i = 0, y = y1; i < nrows && y <= y2; i++, y++) {
	    GnmCell *cell = sheet_cell_fetch(sheet, x, y);
	    if (cell && pure_is_string(data[i*tda+j], &fmtstr)) {
	      gnm_cell_set_format(cell, fmtstr);
	      gnm_cell_unrender(cell);
	    }
	  }
	ret = pure_tuplel(0);
      } else if (pure_is_listv(xs, &sz, &xv) || pure_is_tuplev(xs, &sz, &xv)) {
	size_t j;
	for (j = 0, x = x1; j < sz && x <= x2; j++, x++) {
	  GnmCell *cell = sheet_cell_fetch(sheet, x, y1);
	  if (cell && pure_is_string(xv[j], &fmtstr)) {
	    gnm_cell_set_format(cell, fmtstr);
	    gnm_cell_unrender(cell);
	  }
	}
	if (xv) free(xv);
	ret = pure_tuplel(0);
      }
    }
    value_release(v);
    return ret;
  } else
    return NULL;
}

#include <gtk/gtk.h>
#include <sheet-object.h>
#include <sheet-object-impl.h>
#include <sheet-object-widget.h>
#include <sheet-object-image.h>
#include <sheet-object-graph.h>
#include <gnm-so-filled.h>
#include <gnm-so-line.h>
#include <gnm-so-polygon.h>

#define SHEET_WIDGET_FRAME_TYPE		(sheet_widget_frame_get_type ())
#define SHEET_WIDGET_SCROLLBAR_TYPE	(sheet_widget_scrollbar_get_type ())
#define SHEET_WIDGET_SPINBUTTON_TYPE	(sheet_widget_spinbutton_get_type ())
#define SHEET_WIDGET_SLIDER_TYPE	(sheet_widget_slider_get_type ())
#define SHEET_WIDGET_BUTTON_TYPE	(sheet_widget_button_get_type ())
#define SHEET_WIDGET_CHECKBOX_TYPE	(sheet_widget_checkbox_get_type ())
#define SHEET_WIDGET_RADIO_BUTTON_TYPE	(sheet_widget_radio_button_get_type ())
#define SHEET_WIDGET_LIST_TYPE		(sheet_widget_list_get_type ())
#define SHEET_WIDGET_COMBO_TYPE		(sheet_widget_combo_get_type ())

/* Gnumeric 1.12.21+? */
#ifndef SHEET_OBJECT
#define SHEET_OBJECT(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), GNM_SO_TYPE, SheetObject))
#endif
#ifndef SHEET_OBJECT_IMAGE_TYPE
#define SHEET_OBJECT_IMAGE_TYPE (sheet_object_image_get_type ())
#define SHEET_OBJECT_GRAPH_TYPE (sheet_object_graph_get_type ())
#endif

static GocItem *get_goc_item (SheetObjectView *view)
{
  GocGroup *group = GOC_GROUP(view);
  if (group == NULL || group->children == NULL ||
      !GOC_IS_ITEM(group->children->data))
    return NULL;
  return GOC_ITEM(group->children->data);
}

static char *texpr2str(const GnmEvalPos *pos, const GnmExprTop *e)
{
  GnmParsePos pp;
  GnmConventions const *convs = gnm_conventions_default;
  char *s = gnm_expr_top_as_string(e, parse_pos_init_evalpos(&pp, pos), convs);
  char *buf, *bufp; const char *p = s;
  /* Do some cosmetic surgery. We only deal with absolute range references
     anyway, so we can remove the redundant '$' signs. */
  bufp = buf = alloca(strlen(p)+1);
  for (; *p; p++)
    if (*p != '$')
      *(bufp++) = *p;
  *bufp = '\0';
  g_free(s);
  return strdup(buf);
}

#if GNM_VERSION_EPOCH <= 1 && GNM_VERSION_MAJOR <= 9

/* Older Gnumeric versions lack these. */

static GnmExprTop const *
sheet_widget_list_base_get_result_link(SheetObject *so)
{
  const GnmDependent *dep = sheet_widget_list_base_get_result_dep(so);
  GnmExprTop const *texpr = dep?dep->texpr:NULL;
  if (texpr) gnm_expr_top_ref(texpr);
  return texpr;
}

static GnmExprTop const *
sheet_widget_list_base_get_content_link(SheetObject *so)
{
  const GnmDependent *dep = sheet_widget_list_base_get_content_dep(so);
  GnmExprTop const *texpr = dep?dep->texpr:NULL;
  if (texpr) gnm_expr_top_ref(texpr);
  return texpr;
}

#endif

static GtkFrame *get_frame_widget(GtkWidget *w)
{
  /* In newer Gnumeric versions, the GtkFrame widget of a frame sheet object
     is actually a child inside a GtkEventBox. */
  if (GTK_IS_FRAME(w))
    return GTK_FRAME(w);
  else
    return GTK_FRAME(gtk_bin_get_child(GTK_BIN(w)));
}

pure_expr *pure_sheet_objects(void)
{
  if (eval_info && eval_info->ei) {
    const GnmEvalPos *pos = eval_info->ei->pos;
    Sheet *sheet = pos->sheet;
    Workbook *wb = sheet->workbook;
    GSList *sheets = workbook_sheets(wb), *sptr, *soptr;
    size_t i, n;
    pure_expr **xs = NULL, *ret;
    for (n = 0, sptr = sheets; sptr != NULL; sptr = sptr->next)
      for (soptr = SHEET(sptr->data)->sheet_objects; soptr != NULL;
	   n++, soptr = soptr->next) ;
    if (n > 0) {
      xs = g_new(pure_expr*, n);
      assert(xs);
    }
    for (n = 0, sptr = sheets; sptr != NULL; sptr = sptr->next) {
      size_t m = n;
      const char *sheet_name = SHEET(sptr->data)->name_unquoted;
      pure_expr *sheet_name_str = pure_string_dup(sheet_name);
      for (soptr = SHEET(sptr->data)->sheet_objects; soptr != NULL;
	   soptr = soptr->next) {
	const char *descr = NULL;
	pure_expr *info = NULL, **widgets = NULL;
	GObject *obj = G_OBJECT (soptr->data);
	SheetObject *so = SHEET_OBJECT(obj);
	GType t = G_OBJECT_TYPE(obj);
	GList *w = so->realized_list;
	size_t n_widgets = g_list_length(so->realized_list);
	GtkWidget *widget = NULL;
	if (w) widgets = g_new(pure_expr*, n_widgets);
	for (n_widgets = 0; w; w = w->next) {
	  SheetObjectView *view = w->data;
	  GocItem *item = view?get_goc_item(view):NULL;
	  if (!item)
	    break;
	  else if (GOC_IS_WIDGET(item)) {
	    /* Note that in general, any number of widgets can be associated
	       with a sheet object, we collect these in a list. */
	    widget = GOC_WIDGET(item)->widget;
	    widgets[n_widgets++] = pure_pointer(widget);
	  } else if (GOC_IS_RECTANGLE(item)) {
	    descr = "rectangle";
	    break;
	  } else if (GOC_IS_CIRCLE(item)) {
	    descr = "circle";
	    break;
	  } else if (GOC_IS_ELLIPSE(item)) {
	    descr = "ellipse";
	    break;
	  } else if (GOC_IS_LINE(item)) {
	    descr = "line";
	    break;
	  } else if (GOC_IS_POLYGON(item)) {
	    descr = "polygon";
	    break;
	  } else if (GOC_IS_STYLED_ITEM(item)) {
	    descr = "styled";
	    break;
	  } else {
	    descr = "unkown";
	    break;
	  }
	}
	if (n_widgets == 0 && widgets) {
	  g_free(widgets); widgets = NULL;
	}
	/* Gnumeric specific widget types. */
	if (t == SHEET_WIDGET_FRAME_TYPE) {
	  /* Frame objects don't expose any properties right now, so we take
	     the label from the widget if it's available. */
	  const gchar *str =
	    widget?gtk_frame_get_label(get_frame_widget(widget)):NULL;
	  info = pure_tuplel(5, sheet_name_str, pure_string_dup("frame"),
			     pure_string_dup(str),
			     NA_expr(),
			     pure_listv(n_widgets, widgets));
	} else if (t == SHEET_WIDGET_SCROLLBAR_TYPE) {
	  const GnmExprTop *e = sheet_widget_adjustment_get_link(so);
	  char *link = e?texpr2str(pos, e):strdup("");
	  if (e) gnm_expr_top_unref(e);
	  info = pure_tuplel(5, sheet_name_str, pure_string_dup("scrollbar"),
			     NA_expr(),
			     pure_string(link),
			     pure_listv(n_widgets, widgets));
	} else if (t == SHEET_WIDGET_SPINBUTTON_TYPE) {
	  const GnmExprTop *e = sheet_widget_adjustment_get_link(so);
	  char *link = e?texpr2str(pos, e):strdup("");
	  if (e) gnm_expr_top_unref(e);
	  info = pure_tuplel(5, sheet_name_str, pure_string_dup("spinbutton"),
			     NA_expr(),
			     pure_string(link),
			     pure_listv(n_widgets, widgets));
	} else if (t == SHEET_WIDGET_SLIDER_TYPE) {
	  const GnmExprTop *e = sheet_widget_adjustment_get_link(so);
	  char *link = e?texpr2str(pos, e):strdup("");
	  if (e) gnm_expr_top_unref(e);
	  info = pure_tuplel(5, sheet_name_str, pure_string_dup("slider"),
			     NA_expr(),
			     pure_string(link),
			     pure_listv(n_widgets, widgets));
	} else if (t == SHEET_WIDGET_BUTTON_TYPE) {
	  gchar *str = NULL;
	  const GnmExprTop *e = sheet_widget_button_get_link(so);
	  char *link = e?texpr2str(pos, e):strdup("");
	  if (e) gnm_expr_top_unref(e);
	  g_object_get(obj, "text", &str, NULL);
	  info = pure_tuplel(5, sheet_name_str, pure_string_dup("button"),
			     pure_string(str),
			     pure_string(link),
			     pure_listv(n_widgets, widgets));
	} else if (t == SHEET_WIDGET_CHECKBOX_TYPE) {
	  gchar *str = NULL;
	  const GnmExprTop *e = sheet_widget_checkbox_get_link(so);
	  char *link = e?texpr2str(pos, e):strdup("");
	  if (e) gnm_expr_top_unref(e);
	  g_object_get(obj, "text", &str, NULL);
	  info = pure_tuplel(5, sheet_name_str, pure_string_dup("checkbox"),
			     pure_string(str),
			     pure_string(link),
			     pure_listv(n_widgets, widgets));
	} else if (t == SHEET_WIDGET_RADIO_BUTTON_TYPE) {
	  gchar *str = NULL;
	  const GnmExprTop *e = sheet_widget_radio_button_get_link(so);
	  char *link = e?texpr2str(pos, e):strdup("");
	  if (e) gnm_expr_top_unref(e);
	  g_object_get(obj, "text", &str, NULL);
	  info = pure_tuplel(5, sheet_name_str, pure_string_dup("radiobutton"),
			     pure_string(str),
			     pure_string(link),
			     pure_listv(n_widgets, widgets));
	} else if (t == SHEET_WIDGET_LIST_TYPE) {
	  const GnmExprTop *e = sheet_widget_list_base_get_result_link(so);
	  const GnmExprTop *c = sheet_widget_list_base_get_content_link(so);
	  char *link = e?texpr2str(pos, e):strdup("");
	  char *content_link = c?texpr2str(pos, c):strdup("");
	  if (e) gnm_expr_top_unref(e);
	  if (c) gnm_expr_top_unref(c);
	  info = pure_tuplel(5, sheet_name_str, pure_string_dup("list"),
			     pure_string(content_link),
			     pure_string(link),
			     pure_listv(n_widgets, widgets));
	} else if (t == SHEET_WIDGET_COMBO_TYPE) {
	  const GnmExprTop *e = sheet_widget_list_base_get_result_link(so);
	  const GnmExprTop *c = sheet_widget_list_base_get_content_link(so);
	  char *link = e?texpr2str(pos, e):strdup("");
	  char *content_link = c?texpr2str(pos, c):strdup("");
	  if (e) gnm_expr_top_unref(e);
	  if (c) gnm_expr_top_unref(c);
	  info = pure_tuplel(5, sheet_name_str, pure_string_dup("combo"),
			     pure_string(content_link),
			     pure_string(link),
			     pure_listv(n_widgets, widgets));
	} else {
	  /* Other canvas object types that we know about. */
	  if (t == SHEET_OBJECT_IMAGE_TYPE) {
	    gchar *str = NULL;
	    gpointer ptr = NULL;
	    g_object_get(obj, "image-type", &str, "image-data", &ptr, NULL);
	    info = pure_tuplel(5, sheet_name_str,
			       pure_string(str?g_strdup_printf("image/%s", str):
					   strdup("image")),
			       pure_pointer(ptr),
			       NA_expr(),
			       NA_expr());
	  } else if (t == SHEET_OBJECT_GRAPH_TYPE) {
	    info = pure_tuplel(5, sheet_name_str, pure_string_dup("graph"),
			       NA_expr(),
			       NA_expr(),
			       NA_expr());
	  } else if (descr) {
	    if (t == GNM_SO_FILLED_TYPE) {
	      gchar *str = NULL;
	      g_object_get(obj, "text", &str, NULL);
	      info = pure_tuplel(5, sheet_name_str, pure_string_dup(descr),
				 pure_string(str),
				 NA_expr(),
				 NA_expr());
	    } else if (t == GNM_SO_LINE_TYPE || t == GNM_SO_POLYGON_TYPE) {
	      info = pure_tuplel(5, sheet_name_str, pure_string_dup(descr),
				 NA_expr(),
				 NA_expr(),
				 NA_expr());
	    }
	  }
	  /* FIXME: Figure out what else might be useful to support. */
	}
	if (info) xs[n++] = info;
	if (widgets) g_free(widgets);
      }
      if (sheet_name_str->refc == 0) pure_freenew(sheet_name_str);
      /* Apparently the objects for each sheet are listed with the last added
	 object first, so we reverse the list here. */
      for (i = m; i < (n-m)/2; i++) {
	pure_expr *x = xs[i];
	xs[i] = xs[n+m-i-1];
	xs[n+m-i-1] = x;
      }
    }
    g_slist_free(sheets);
    ret = pure_listv(n, xs); g_free(xs);
    return ret;
  } else
    return NULL;
}

/* OpenGL support. This only works when GtkGLExt is available. */

static gboolean check_name(SheetObject *so, const char *name)
{
  gboolean ok = FALSE;
  gchar *id = NULL;
  /* NOTE: This requires gnumeric 1.9.14 to work. */
  g_object_get(so, "name", &id, NULL);
  if (id) {
    ok = strcmp(id, name) == 0;
    g_free(id);
  }
  return ok;
}

static GtkWidget *get_frame_sheet(const Sheet *sheet, const char *name)
{
  GSList *ptr;
  for (ptr = sheet->sheet_objects; ptr != NULL ; ptr = ptr->next) {
    GObject *obj = G_OBJECT(ptr->data);
    SheetObject *so = SHEET_OBJECT(obj);
    GType t = G_OBJECT_TYPE(obj);
    GList *w = so->realized_list;
    if (t == SHEET_WIDGET_FRAME_TYPE && w && w->data) {
      SheetObjectView *view = w->data;
      GocItem *item = get_goc_item(view);
      if (item && GOC_IS_WIDGET(item)) {
	GtkWidget *widget = GOC_WIDGET(item)->widget;
	const gchar *label = gtk_frame_get_label(get_frame_widget(widget));
	if (check_name(so, name) ||
	    (label && strcmp(label, name) == 0))
	  return widget;
      }
    }
  }
  return NULL;
}

static GtkWidget *get_frame(const GnmEvalPos *pos, const char *name)
{
  Sheet *sheet = pos->sheet;
  Workbook *wb = sheet->workbook;
  GSList *sheets, *ptr;
  /* We first look for the frame in the current sheet, then in all sheets of
     its workbook. */
  GtkWidget *w = get_frame_sheet(sheet, name);
  if (w) return w;
  if ((sheets = workbook_sheets(wb))) {
    for (ptr = sheets; ptr != NULL ; ptr = ptr->next) {
      Sheet *s = SHEET(ptr->data);
      if (s != sheet) {
	w = get_frame_sheet(s, name);
	if (w) break;
      }
    }
    g_slist_free(sheets);
  }
  return w;
}

#ifdef USE_GL

static gint ptrcmp(gconstpointer a, gconstpointer b)
{
  if (a < b)
    return -1;
  else if (a > b)
    return 1;
  else
    return 0;
}

static GSList *search_frame_so(SheetObject *so, const char *name,
			       GSList *l)
{
  GType t = G_OBJECT_TYPE(so);
  if (t == SHEET_WIDGET_FRAME_TYPE) {
    gboolean ok = check_name(so, name);
    GList *w = so->realized_list;
    for (; w; w = w->next) {
      SheetObjectView *view = w->data;
      GocItem *item = get_goc_item(view);
      if (item && GOC_IS_WIDGET(item)) {
	GtkWidget *widget = GOC_WIDGET(item)->widget;
	const gchar *label = gtk_frame_get_label(get_frame_widget(widget));
	if (ok || (label && strcmp(label, name) == 0))
	  l = g_slist_insert_sorted(l, widget, ptrcmp);
      }
    }
  }
  return l;
}

static GSList *search_frame_sheet(const Sheet *sheet, const char *name,
				  GSList *l, SheetObject **sop)
{
  GSList *ptr;
  for (ptr = sheet->sheet_objects; ptr != NULL ; ptr = ptr->next) {
    GObject *obj = G_OBJECT(ptr->data);
    SheetObject *so = SHEET_OBJECT(obj);
    l = search_frame_so(so, name, l);
    if (l) {
      *sop = so;
      break;
    }
  }
  return l;
}

static GSList *get_frames(const GnmEvalPos *pos, const char *name,
			  SheetObject **sop)
{
  Sheet *sheet = pos->sheet;
  Workbook *wb = sheet->workbook;
  GSList *sheets, *ptr;
  /* We first look for the frame in the current object, then in the current
     sheet, then in all sheets of the workbook. */
  GSList *l = *sop?search_frame_so(*sop, name, NULL):NULL;
  if (l) return l;
  *sop = NULL;
  l = search_frame_sheet(sheet, name, NULL, sop);
  if (l) return l;
  if ((sheets = workbook_sheets(wb))) {
    for (ptr = sheets; ptr != NULL ; ptr = ptr->next) {
      Sheet *s = SHEET(ptr->data);
      if (s != sheet) {
	l = search_frame_sheet(sheet, name, NULL, sop);
	if (l) break;
      }
    }
    g_slist_free(sheets);
  }
  return l;
}
#endif

bool pure_check_window(const char *name)
{
  if (eval_info && eval_info->ei) {
    const GnmFuncEvalInfo *ei = eval_info->ei;
    const GnmEvalPos *pos = ei->pos;
    GtkWidget *w = get_frame(pos, name);
    return w != NULL;
  } else
    return false;
}

#ifdef USE_GL

#include <gtk/gtk.h>
#include <gtk/gtkgl.h>
#include <gdk/gdkgl.h>
#include <GL/gl.h>

static gboolean gl_destroy_cb(GtkWidget *drawing_area, GLWindow *glw)
{
  if (!glw->being_destroyed) {
    GSList *windows = g_slist_copy(glw->windows), *l;
    for (l = windows; l; l = l->next) {
      GtkWidget *w = GTK_WIDGET(l->data);
      GtkWidget *drawing_area =
	gtk_bin_get_child(GTK_BIN(get_frame_widget(w)));
      if (!drawing_area) {
	glw->windows = g_slist_remove(glw->windows, w);
#if 0
	fprintf(stderr, "removing view %p\n", w);
#endif
      }
    }
    g_slist_free(windows);
    if (!glw->windows)
      pure_remove_gl_window(&glw->key);
  }
  return TRUE;
}

static void do_callback(pure_expr *cb, GtkWidget *drawing_area,
			pure_expr *user_data, pure_expr *cb_data)
{
  pure_expr *ret;
  if (!cb || !drawing_area || !user_data || !cb_data) return;
  ret = pure_appl(cb, 2, user_data, cb_data);
  if (ret) pure_freenew(ret);
}

/* These have been superseded in GtkGLExt 3.0, map them to the new calls. */

#define gdk_gl_drawable_gl_begin(drawable, glcontext) gdk_gl_context_make_current(glcontext, drawable, drawable)
#define gdk_gl_drawable_gl_end(gldrawable) gdk_gl_context_release_current()

static gboolean gl_setup_cb(GtkWidget *drawing_area, GdkEvent *event,
			    GLWindow *glw)
{
  GdkGLContext *glContext = gtk_widget_get_gl_context(drawing_area);
  GdkGLDrawable *glDrawable = gtk_widget_get_gl_drawable(drawing_area);
  if (!gdk_gl_drawable_gl_begin(glDrawable, glContext))
    g_assert_not_reached();
  do_callback(glw->setup_cb, drawing_area, glw->user_data,
	      pure_pointer(drawing_area));
  gdk_gl_drawable_gl_end(glDrawable);
  return TRUE;
}

static gboolean gl_config_cb(GtkWidget *drawing_area, GdkEvent *event,
			     GLWindow *glw)
{
  GdkGLContext *glContext = gtk_widget_get_gl_context(drawing_area);
  GdkGLDrawable *glDrawable = gtk_widget_get_gl_drawable(drawing_area);
  GtkAllocation *allocation = g_new0(GtkAllocation, 1);
  if (!gdk_gl_drawable_gl_begin(glDrawable, glContext))
    g_assert_not_reached();
  gtk_widget_get_allocation(drawing_area, allocation);
  do_callback(glw->config_cb, drawing_area, glw->user_data,
	      pure_tuplel(2, pure_int(allocation->width),
			  pure_int(allocation->height)));
  g_free(allocation);
  gdk_gl_drawable_gl_end(glDrawable);
  return TRUE;
}

static gboolean gl_display_cb(GtkWidget *drawing_area, cairo_t *cr,
			      GLWindow *glw)
{
  GdkGLContext *glContext = gtk_widget_get_gl_context(drawing_area);
  GdkGLDrawable *glDrawable = gtk_widget_get_gl_drawable(drawing_area);
  if (!gdk_gl_drawable_gl_begin(glDrawable, glContext))
    g_assert_not_reached();
  do_callback(glw->display_cb, drawing_area, glw->user_data, pure_tuplel(0));
  if (gdk_gl_drawable_is_double_buffered(glDrawable))
    gdk_gl_drawable_swap_buffers(glDrawable);
  else
    glFlush();
  gdk_gl_drawable_gl_end(glDrawable);
  return TRUE;
}

static gboolean gl_timer_cb(GLWindow *glw)
{
  if (!glw->being_destroyed) {
    GSList *l = glw->windows;
    for (; l; l = l->next) {
      GtkWidget *w = GTK_WIDGET(l->data);
      GtkWidget *drawing_area =
	gtk_bin_get_child(GTK_BIN(get_frame_widget(w)));
      if (drawing_area) {
	GtkAllocation *allocation = g_new0(GtkAllocation, 1);
	gtk_widget_get_allocation(drawing_area, allocation);
	GdkRectangle r = { 0, 0, allocation->width,
			   allocation->height };
	g_free(allocation);
	GdkGLContext *glContext = gtk_widget_get_gl_context(drawing_area);
	GdkGLDrawable *glDrawable = gtk_widget_get_gl_drawable(drawing_area);
	if (!gdk_gl_drawable_gl_begin(glDrawable, glContext))
	  g_assert_not_reached();
	do_callback(glw->timer_cb, drawing_area, glw->user_data,
		    pure_tuplel(0));
	gdk_window_invalidate_rect(gtk_widget_get_window(drawing_area),
				   &r, FALSE);
	gdk_gl_drawable_gl_end(glDrawable);
      }
    }
  }
  return TRUE;
}

static gboolean init_gl_window(GLWindow *glw, GtkWidget *w)
{
  /* Check to see whether we already added a drawable to the frame widget. */
  if (gtk_bin_get_child(GTK_BIN(get_frame_widget(w))))
    return false;
  else {
    GtkWidget *drawing_area = gtk_drawing_area_new();
    GdkGLConfig *config;
    if (!drawing_area) return false;
    config = gdk_gl_config_new_by_mode(GDK_GL_MODE_RGBA | GDK_GL_MODE_DEPTH |
				       GDK_GL_MODE_DOUBLE);
    if (!config) g_assert_not_reached();
    if (!gtk_widget_set_gl_capability(drawing_area, config, NULL, TRUE,
				      GDK_GL_RGBA_TYPE)) {
      gtk_widget_destroy(drawing_area);
      return false;
    }
    /* Set up the callbacks. */
    g_signal_connect(G_OBJECT(drawing_area), "configure-event",
		     G_CALLBACK(gl_config_cb), glw);
    g_signal_connect(G_OBJECT(drawing_area), "draw",
		     G_CALLBACK(gl_display_cb), glw);
    g_signal_connect(G_OBJECT(drawing_area), "destroy",
		     G_CALLBACK(gl_destroy_cb), glw);
    gtk_container_add(GTK_CONTAINER(get_frame_widget(w)), drawing_area);
    gtk_widget_show(drawing_area);
    return true;
  }
}

static void init_gl_timer(GLWindow *glw, int timeout)
{
  glw->timer_id = 0;
  glw->timeout = timeout;
  if (timeout > 0)
    glw->timer_id = g_timeout_add(timeout, (GSourceFunc)gl_timer_cb, glw);
}

static gboolean g_slist_equal(const GSList *l1, const GSList *l2)
{
  if (l1 == l2) return TRUE;
  for (; l1 && l2 && l1->data==l2->data; l1 = l1->next, l2 = l2->next) ;
  return l1==l2;
}

pure_expr *pure_gl_window(const char *name, int timeout,
			  pure_expr *setup_cb,
			  pure_expr *config_cb,
			  pure_expr *display_cb,
			  pure_expr *timer_cb,
			  pure_expr *user_data)
{
  if (eval_info && eval_info->ei) {
    const GnmFuncEvalInfo *ei = eval_info->ei;
    const GnmEvalPos *pos = ei->pos;
    DepKey key = { ei->func_call, ei->pos->dep, gl_id++ };
    GLWindow *glw;
    static gboolean once = TRUE, init = FALSE;
    g_return_val_if_fail (name && setup_cb && config_cb && display_cb &&
			  timer_cb && user_data, NULL);
    if (once) {
      /* This needs to be done exactly once to initialize the GtkGL
	 extension. */
      int argc = 1;
      char *argv[] = { "", NULL };
      init = gtk_init_check(&argc, (char***)&argv);
      once = FALSE;
    }
    if (!init) return NULL; // GtkGL failed to initialize.
    /* See whether we already have a window for this instance. */
    if ((glw = pure_get_gl_window(&key))) {
      GSList *windows = get_frames(pos, name, &glw->so), *l;
      if (strcmp(name, glw->name)) {
	free(glw->name); glw->name = strdup(name);
      }
      pure_free(glw->setup_cb);
      pure_free(glw->config_cb);
      pure_free(glw->display_cb);
      pure_free(glw->timer_cb);
      pure_free(glw->user_data);
      glw->setup_cb = glw->config_cb = glw->display_cb = glw->timer_cb =
	glw->user_data = NULL;
      if (!g_slist_equal(windows, glw->windows)) {
	GSList *ws = g_slist_copy(glw->windows);
	/* Remove old views. */
	glw->being_destroyed = TRUE;
	for (l = ws; l; l = l->next) {
	  GtkWidget *w = GTK_WIDGET(l->data);
	  if (!g_slist_find(windows, w)) {
	    GtkWidget *drawing_area =
	      gtk_bin_get_child(GTK_BIN(get_frame_widget(w)));
	    /* Destroy the GL window. */
#if 0
	    fprintf(stderr, "removing old view %p\n", w);
#endif
	    gtk_widget_destroy(drawing_area);
	    glw->windows = g_slist_remove(glw->windows, w);
	  }
	}
	glw->being_destroyed = FALSE;
	g_slist_free(ws);
	ws = NULL;
	/* Add new views. */
	for (l = windows; l; l = l->next) {
	  GtkWidget *w = GTK_WIDGET(l->data);
	  if (init_gl_window(glw, w)) {
	    glw->windows = g_slist_insert_sorted(glw->windows, w, ptrcmp);
	    ws = g_slist_prepend(ws, w);
#if 0
	    fprintf(stderr, "adding new view %p\n", w);
#endif
	  }
	}
	g_slist_free(windows);
	windows = g_slist_reverse(ws);
      } else {
	g_slist_free(windows);
	windows = NULL;
      }
      if (glw->windows) {
	/* Update the callbacks. */
	glw->setup_cb = pure_new(setup_cb);
	glw->config_cb = pure_new(config_cb);
	glw->display_cb = pure_new(display_cb);
	glw->timer_cb = pure_new(timer_cb);
	glw->user_data = pure_new(user_data);
	if (timeout != glw->timeout) {
	  /* Reinstall the timer callback. */
	  if (glw->timeout > 0) g_source_remove(glw->timer_id);
	  init_gl_timer(glw, timeout);
	}
	/* Run the setup callback on the new views. */
	for (l = windows; l; l = l->next) {
	  GtkWidget *w = GTK_WIDGET(l->data);
	  GtkWidget *drawing_area =
	    gtk_bin_get_child(GTK_BIN(get_frame_widget(w)));
	  gl_setup_cb(drawing_area, NULL, glw);
	}
	g_slist_free(windows);
	return pure_pointer(glw->windows->data);
      } else {
	/* Not valid. Bail out with failure. */
	g_slist_free(windows);
	pure_remove_gl_window(&key);
	return NULL;
      }
    } else {
      SheetObject *so = NULL;
      GSList *windows = get_frames(pos, name, &so), *l;
      if (windows) {
	glw = g_new(GLWindow, 1);
	if (!glw) g_assert_not_reached();
	glw->being_destroyed = FALSE;
	glw->windows = NULL;
	glw->so = so;
	glw->setup_cb = glw->config_cb = glw->display_cb = glw->timer_cb =
	  glw->user_data = NULL;
	for (l = windows; l; l = l->next) {
	  GtkWidget *w = GTK_WIDGET(l->data);
	  if (init_gl_window(glw, w)) {
	    glw->windows = g_slist_insert_sorted(glw->windows, w, ptrcmp);
#if 0
	    fprintf(stderr, "adding view %p\n", w);
#endif
	  }
	}
	g_slist_free(windows);
	if (!glw->windows) {
	  g_free(glw);
	  return NULL;
	}
	/* Set the callbacks. */
	glw->setup_cb = pure_new(setup_cb);
	glw->config_cb = pure_new(config_cb);
	glw->display_cb = pure_new(display_cb);
	glw->timer_cb = pure_new(timer_cb);
	glw->user_data = pure_new(user_data);
	glw->name = strdup(name);
	pure_add_gl_window(&key, glw);
	/* Install the timer callback. */
	init_gl_timer(glw, timeout);
	/* Run the setup callback. */
	for (l = glw->windows; l; l = l->next) {
	  GtkWidget *w = GTK_WIDGET(l->data);
	  GtkWidget *drawing_area =
	    gtk_bin_get_child(GTK_BIN(get_frame_widget(w)));
	  gl_setup_cb(drawing_area, NULL, glw);
	}
	return pure_pointer(glw->windows->data);
      } else
	return NULL;
    }
  } else
    return NULL;
}

/* The GTK3 version of GtkGLExt doesn't include these convenience functions,
   so we simply include the source from GtkGLExt 1.0 here. */

#define __GDKGL_H_INSIDE__
#define GDK_GL_COMPILATION
#include "gdkglshapes.c"

#else

pure_expr *pure_gl_window(const char *name, int timeout,
			  pure_expr *setup_cb,
			  pure_expr *config_cb,
			  pure_expr *display_cb,
			  pure_expr *timer_cb,
			  pure_expr *user_data)
{
  return NULL;
}

#endif
