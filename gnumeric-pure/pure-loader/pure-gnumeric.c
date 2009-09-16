#include "pure-gnumeric.h"
#include "pure-loader.h"
#include <glib/gi18n-lib.h>

// NOTE: This stuff requires that Pure was built with GSL matrix support.

#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gmp.h>

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

pure_expr *
value2pure(const GnmEvalPos *pos, const GnmValue *v, const char *spec)
{
  switch (v->type) {
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
	if (val->type != VALUE_FLOAT)
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
      GnmParsePos pp;
      GnmConventionsOut out;
      char *buf, *bufp; const char *p;
      out.accum = g_string_new(NULL);
      out.pp    = parse_pos_init_evalpos(&pp, pos);
      out.convs = gnm_conventions_default;
      rangeref_as_string(&out, rr);
      /* Do some cosmetic surgery. Range references are always taken as
	 absolute here, so we remove the redundant '$' signs. */
      p = out.accum->str;
      bufp = buf = alloca(strlen(p)+1);
      for (; *p; p++)
	if (*p != '$')
	  *(bufp++) = *p;
      *bufp = '\0';
      g_string_free(out.accum, TRUE);
      return pure_string_dup(buf);
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
	  if (cell && cell->value->type != VALUE_FLOAT)
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
      return (res != NULL) ? res : value_new_error_REF(pos);
    } else
      v = value_new_string(s);
  } else if(pure_is_double_matrix(x, &p)) {
    gsl_matrix *mat = (gsl_matrix*)p;
    double *data = mat->data;
    size_t i, j, nrows = mat->size1, ncols = mat->size2, tda = mat->tda;
    v = value_new_array_empty(ncols, nrows);
    for (i = 0; i < nrows; i++)
      for (j = 0; j < ncols; j++) {
	gint x = (gint)j, y = (gint)i;
	GnmValue *val = value_new_float((gnm_float)data[i*tda+j]);
	v->v_array.vals[x][y] = val;
      }
  } else if(pure_is_symbolic_matrix(x, &p)) {
    gsl_matrix_symbolic *mat = (gsl_matrix_symbolic*)p;
    pure_expr **data = mat->data;
    size_t i, j, nrows = mat->size1, ncols = mat->size2, tda = mat->tda;
    v = value_new_array_empty(ncols, nrows);
    for (i = 0; i < nrows; i++)
      for (j = 0; j < ncols; j++) {
	gint x = (gint)j, y = (gint)i;
	GnmValue *val = pure2value(pos, data[i*tda+j], NULL);
	if (!val) val = value_new_error_VALUE(pos);
	v->v_array.vals[x][y] = val;
      }
  } else if (pure_is_listv(x, &sz, &xv)) {
    size_t i;
    v = value_new_array_empty(sz, 1);
    for (i = 0; i < sz; i++) {
      GnmValue *val = pure2value(pos, xv[i], NULL);
      if (!val) val = value_new_error_VALUE(pos);
      v->v_array.vals[i][0] = val;
    }
    free(xv);
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

// These should be TLD when the Pure interpreter becomes multithreaded.
static const GnmFuncEvalInfo *eval_info;
static pure_expr *eval_expr;
static unsigned id;

#define next(spec) (spec?(*spec?++spec:spec):NULL)

GnmValue *
call_pure_function(GnmFuncEvalInfo *ei, gint n_args,
		   GnmValue const * const *argv)
{
  GnmFunc const *func = ei->func_call->func;
  const char *spec = (func->fn_type==GNM_FUNC_TYPE_ARGS)
    ?func->fn.args.arg_spec:NULL;
  int i, j, min, max;
  pure_expr *x, *y, *e, *fun, **args, *save_expr;
  const GnmFuncEvalInfo *save_info;
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
      for (j = 0; j < i; j++) pure_freenew(args[j]);
      g_free(args);
      ret = value_new_error_VALUE(ei->pos);
      return ret;
    }
  }

  if (i < min) {
    // Not enough arguments. This shouldn't happen, so we just bail out here.
    for (j = 0; j < i; j++) pure_freenew(args[j]);
    g_free(args);
    return NULL;
  }

  fun = pure_quoted_symbol(pure_sym(func->name));
  if (min != max)
    // Variadic function, pass extra arguments as a list.
    x = pure_app(pure_appv(fun, min, args), pure_listv(i-min, args+min));
  else
    x = pure_appv(fun, i, args);
  g_free(args);
  // Save the old context in case we're invoked recursively.
  save_info = eval_info; save_expr = eval_expr;
  eval_info = ei; eval_expr = x; if (!save_info) id = 0;
  y = pure_evalx(x, &e);
  // Restore the old context.
  eval_info = save_info; eval_expr = save_expr; if (!save_info) id = 0;

  if (y) {
    ret = pure2value(ei->pos, y, NULL);
    pure_freenew(y);
    if (!ret) ret = value_new_error_VALUE(ei->pos);
  } else {
    if (e) pure_freenew(e);
    ret = value_new_error_NULL(ei->pos);
  }
  return ret;
}

pure_expr *
pure_gnmcall(const char *name, pure_expr *args)
{
  GnmFunc *func = gnm_func_lookup(name, NULL);
  const char *spec = (func->fn_type==GNM_FUNC_TYPE_ARGS)
    ?func->fn.args.arg_spec:NULL;
  GnmValue **val, *ret_val;
  gint n_args, i, j;
  size_t n;
  pure_expr **xv, *ret;
  if (!func || !eval_info) return NULL;
  if (!pure_is_listv(args, &n, &xv)) return NULL;
  n_args = (gint)n;
  val = g_new(GnmValue*, n_args);
  for (i = 0; i < n_args; i++, next(spec)) {
    val[i] = pure2value(eval_info->pos, xv[i], spec);
    if (!val[i]) {
      for (j = 0; j < i; j++)
	value_release(val[j]);
      if (xv) free(xv);
      g_free(val);
      return NULL;
    }
  }
  if (xv) free(xv);
  ret_val = function_def_call_with_values(eval_info->pos, func, n_args,
					  (GnmValue const * const *)val);
  ret = value2pure(eval_info->pos, ret_val, NULL);
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

bool pure_write_blob(FILE *fp, const keyval_t *key, pure_expr *x)
{
  pure_expr *b;
  // We should maybe encode these in binary.
  char buf[100];
  sprintf(buf, "%p-%p-%u", key->p, key->q, key->id);
  b = pure_app(pure_symbol(pure_sym("blob")),
	       pure_tuplel(2, pure_cstring_dup(buf), x));
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
		key->p, key->q, key->id, (unsigned long)n, (int)ret);
#endif
      }
      pure_freenew(size);
    }
  }
  if (b) pure_free(b);
  fflush(fp);
  return ret;
}

bool pure_read_blob(FILE *fp, keyval_t *key, pure_expr **x)
{
  size_t n;
  void *buf;
  pure_expr *y, **xv = NULL;
  const char *s;
  if (fread(&n, sizeof(size_t), 1, fp) != 1)
    return false;
  buf = malloc(n);
  if (!buf)
    return false;
  if (fread(buf, 1, n, fp) != n) {
    free(buf);
    return false;
  }
  y = pure_app(pure_symbol(pure_sym("val")), pure_pointer(buf));
  free(buf);
  if (!y) return false;
  if (pure_is_tuplev(y, &n, &xv) && n == 2 && pure_is_string(xv[0], &s)) {
    if (sscanf(s, "%p-%p-%u", &key->p, &key->q, &key->id) < 3) {
      pure_freenew(y);
      if (xv) free(xv);
      return false;
    }
    *x = xv[1];
    pure_ref(*x);
    pure_freenew(y);
    free(xv);
    pure_unref(*x);
    return true;
  } else {
    pure_freenew(y);
    if (xv) free(xv);
    return false;
  }
}

static void out(FILE *fp, keyval_t *key, pure_expr *x)
{
  if (!pure_write_blob(fp, key, x))
    // Write error, bail out.
    exit(1);
}

pure_expr *pure_datasource(pure_expr *x)
{
  int pid;
  pure_expr *ret;
  keyval_t key = { eval_info->func_call, eval_info->pos->dep, id };
  if (!x || !eval_info || !pure_async_filename) return NULL;
  if (!pure_async_func_init(eval_info, eval_expr, id++, &ret))
    return ret;
  else if ((pid = fork()) == 0) {
    /* child */
    FILE *pure_async_file = fopen(pure_async_filename, "ab");
    /* evaluate expression, and write results to the pipe */
    pure_expr *u = pure_new(pure_force(x)), *y, *z;
#if 0
    fprintf(stderr, "[%d] child: %p-%p-%u\n", getpid(), key.p, key.q, key.id);
#endif
    while (is_cons(u, &y, &z)) {
      out(pure_async_file, &key, pure_force(y));
      pure_new(pure_force(z));
      pure_free(u);
      u = z;
    }
    if (!is_nil(u))
      out(pure_async_file, &key, u);
    exit(0);
  } else if (pid > 0) {
    /* parent */
#if 0
    fprintf(stderr, "[%d] started child [%d]: %p-%p-%u\n", getpid(), pid,
	    key.p, key.q, key.id);
#endif
    pure_async_func_process(eval_info, key.id, pid);
    return ret;
  } else {
    perror("fork");
    return NULL;
  }
}
