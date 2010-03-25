/* sql3util.c - Utilities to interface with SQLite3. */

#include <stdlib.h>
#include <pure/runtime.h>
#include <stdio.h>
#include <string.h>
#include <sqlite3.h>

//==>(db_ptr,ec)
pure_expr* sql3util_open(char* path, int flags) {
  sqlite3* dbp;
  int ec = sqlite3_open_v2(path , &dbp, flags, NULL);
  pure_expr *p_dbp = pure_pointer(dbp);
  pure_expr *p_errCode = pure_int(ec);
  pure_expr *p_ret = pure_tuplel(2, p_dbp, p_errCode);
  return p_ret;
}

//==>(stmt_ptr,ec)
pure_expr* sql3util_prepare(sqlite3 *db, char *sql)
{
  sqlite3_stmt* stmtp;
  const char* zTail;
  size_t maxlen = strlen(sql) + 1;
  int ec = sqlite3_prepare_v2(db, sql, maxlen, &stmtp, &zTail);

  pure_expr *p_stmtp = pure_pointer(stmtp);
  pure_expr *p_errCode = pure_int(ec);
  pure_expr *p_ret = pure_tuplel(2, p_stmtp, p_errCode);
  return p_ret;
}

/* algorithm lifted from sqlite3 date.c module */

double sql3util_ymd2julian(int Y, int M, int D){
  int A, B, X1, X2;

  if( M<=2 ){
    Y--;
    M += 12;
  }
  A = Y/100;
  B = 2 - A + (A/4);
  X1 = 36525*(Y+4716)/100;
  X2 = 306001*(M+1)/10000;
  return X1 + X2 + D + B - 1524.5;
}

/* algorithm lifted from sqlite3 date.c module */

//=>(Y,M,D)
pure_expr* sql3util_julian2ymd(double jd){
  int Z, A, B, C, D, E, M, X1;

  Z = (int)(jd+0.5);
  A = (int)((Z - 1867216.25)/36524.25);
  A = Z + 1 + A - (A/4);
  B = A + 1524;
  C = (int)((B - 122.1)/365.25);
  D = (36525*C)/100;
  E = (int)((B-D)/30.6001);
  X1 = (int)(30.6001*E);
  M = E<14 ? E-1 : E-13;

  pure_expr *pD = pure_int(B - D - X1);
  pure_expr *pM = pure_int(M);
  pure_expr *pY = pure_int(M>2 ? C - 4716 : C - 4715);
  pure_expr *pret = pure_tuplel(3,pY,pM,pD);
  return pret;
}



/* Marshalling between Pure and SQLite3 data types (sqlite3_value*).
   2010-03-19 AG. */

int sql3util_bind_text(sqlite3_stmt *sp, int col, char *text)
{
  return sqlite3_bind_text(sp, col, text, -1, SQLITE_TRANSIENT);
}

int sql3util_bind_blob(sqlite3_stmt *sp, int col, pure_expr *x){
  int32_t iv; size_t n; void *pv; pure_expr **xs; int res = 0;
  pure_is_tuplev(x, &n, &xs);
  if (xs && pure_is_int(xs[0], &iv) && pure_is_pointer(xs[1], &pv))
    res = sqlite3_bind_blob(sp, col, pv, iv, SQLITE_TRANSIENT);
  else
    pure_throw(pure_app(pure_quoted_symbol(pure_sym("sql3::bad_sql_value")),
			x));
  if (xs) free(xs);
  return res;
}

/* Returns as a pair n::int,p::pointer where n is the number of bytes
   and p is a cooked pointer holding a copy of the blob's data which
   frees itself when garbage-collected. */
pure_expr *sql3util_column_blob(sqlite3_stmt *sp, int col){
  int n = sqlite3_column_bytes(sp, col);
  const void *m = sqlite3_column_blob(sp, col);
  void *p = m?malloc(n):NULL;
  if (p && n>0) memcpy(p, m, n);
  return pure_tuplel(2, pure_int(n),
		     pure_sentry(pure_symbol(pure_sym("free")),
				 pure_pointer(p)));
}


/* SQL NULL representation. */

static int32_t sqlnull_sym = 0; /* FIXME: TLD */

static inline pure_expr *pure_sqlnull(void)
{
  if (sqlnull_sym == 0) sqlnull_sym = pure_sym("sql3::SQLNULL");
  return pure_symbol(sqlnull_sym);
}

static inline bool pure_is_sqlnull(pure_expr *x)
{
  int32_t sym;
  if (sqlnull_sym == 0) sqlnull_sym = pure_sym("sql3::SQLNULL");
  return pure_is_symbol(x, &sym) && sym == sqlnull_sym;
}

/* Convert an SQLite value to a corresponding Pure expression. This provides
   interfaces for the sqlite3_column_value family and the sqlite3_value
   function which need these conversions. */

pure_expr *sql3util_column_value(sqlite3_stmt* stm, int col)
{
  switch (sqlite3_column_type(stm, col)) {
  case SQLITE_INTEGER:
    /* This will always return a machine int, if you want bigints, you'll have
       to use an explicit conversion. */
    return pure_int(sqlite3_column_int(stm, col));
  case SQLITE_FLOAT:
    return pure_double(sqlite3_column_double(stm, col));
  case SQLITE_TEXT: {
    const char *s = (const char*)sqlite3_column_text(stm, col);
    return pure_string_dup(s);
  }
  case SQLITE_BLOB: {
    /* This gets returned as a pair n::int,p::pointer where n is the number of
       bytes and p is a cooked pointer holding a copy of the blob's data which
       frees itself when garbage-collected. */
    int n = sqlite3_column_bytes(stm, col);
    const void *m = sqlite3_column_blob(stm, col);
    void *p = m?malloc(n):NULL;
    if (p && n>0) memcpy(p, m, n);
    return pure_tuplel(2, pure_int(n),
		       pure_sentry(pure_symbol(pure_sym("free")),
				   pure_pointer(p)));
  }
  case SQLITE_NULL:
    return pure_sqlnull();
  default: {
    /* unknown type, convert to string */
    const char *s = (const char*)sqlite3_column_text(stm, col);
    return pure_string_dup(s);
  }
  }
}

pure_expr *sql3util_value(sqlite3_value *val)
{
  switch (sqlite3_value_type(val)) {
  case SQLITE_INTEGER:
    /* This will always return a machine int, if you want bigints, you'll have
       to use an explicit conversion. */
    return pure_int(sqlite3_value_int(val));
  case SQLITE_FLOAT:
    return pure_double(sqlite3_value_double(val));
  case SQLITE_TEXT: {
    const char *s = (const char*)sqlite3_value_text(val);
    return pure_string_dup(s);
  }
  case SQLITE_BLOB: {
    /* This gets returned as a pair n::int,p::pointer where n is the number of
       bytes and p is a cooked pointer holding a copy of the blob's data which
       frees itself when garbage-collected. */
    int n = sqlite3_value_bytes(val);
    const void *m = sqlite3_value_blob(val);
    void *p = m?malloc(n):NULL;
    if (p && n>0) memcpy(p, m, n);
    return pure_tuplel(2, pure_int(n),
		       pure_sentry(pure_symbol(pure_sym("free")),
				   pure_pointer(p)));
  }
  case SQLITE_NULL:
    return pure_sqlnull();
  default: {
    /* unknown type, convert to string */
    const char *s = (const char*)sqlite3_value_text(val);
    return pure_string_dup(s);
  }
  }
}

/* Convert a Pure expression to a corresponding SQLite value. This provides
   interfaces for the sqlite3_bind_value and sqlite3_result_value families
   which need these conversions. */

int sql3util_bind_value(sqlite3_stmt* stm, int col, pure_expr *x)
{
  int32_t iv; double dv; const char *sv; void *pv; mpz_t zv;
  size_t n;
  if (pure_is_int(x, &iv))
    return sqlite3_bind_int(stm, col, iv);
  else if (pure_is_mpz(x, &zv)) {
    mpz_clear(zv);
    return sqlite3_bind_int64(stm, col, pure_get_int64(x));
  } else if (pure_is_double(x, &dv))
    return sqlite3_bind_double(stm, col, dv);
  else if (pure_is_string(x, &sv))
    return sqlite3_bind_text(stm, col, sv, -1, SQLITE_TRANSIENT);
  else if (pure_is_tuplev(x, &n, NULL) && n==2) {
    pure_expr **xs;
    int res = 0;
    pure_is_tuplev(x, &n, &xs);
    if (xs && pure_is_int(xs[0], &iv) && pure_is_pointer(xs[1], &pv))
      res = sqlite3_bind_blob(stm, col, pv, iv, SQLITE_TRANSIENT);
    else
      pure_throw(pure_app(pure_quoted_symbol(pure_sym("sql3::bad_sql_value")),
			  x));
    if (xs) free(xs);
    return res;
  } else if (pure_is_sqlnull(x))
    return sqlite3_bind_null(stm, col);
  else {
    pure_throw
      (pure_app(pure_quoted_symbol(pure_sym("sql3::bad_sql_value")), x));
    return 0;
  }
}

void sql3util_result_value(sqlite3_context* ctx, pure_expr *x)
{
  int32_t iv; double dv; const char *sv; void *pv; mpz_t zv;
  size_t n;
  if (pure_is_int(x, &iv))
    sqlite3_result_int(ctx, iv);
  else if (pure_is_mpz(x, &zv)) {
    mpz_clear(zv);
    sqlite3_result_int64(ctx, pure_get_int64(x));
  } else if (pure_is_double(x, &dv))
    sqlite3_result_double(ctx, dv);
  else if (pure_is_string(x, &sv))
    sqlite3_result_text(ctx, sv, -1, SQLITE_TRANSIENT);
  else if (pure_is_tuplev(x, &n, NULL) && n==2) {
    pure_expr **xs;
    pure_is_tuplev(x, &n, &xs);
    if (xs && pure_is_int(xs[0], &iv) && pure_is_pointer(xs[1], &pv))
      sqlite3_result_blob(ctx, pv, iv, SQLITE_TRANSIENT);
    else
      sqlite3_result_error(ctx, "bad result type", -1);
    if (xs) free(xs);
  } else if (pure_is_sqlnull(x))
    sqlite3_result_null(ctx);
  else
    sqlite3_result_error(ctx, "bad result type", -1);
}

/* Helper functions for managing callbacks. 2010-03-20 AG. */

typedef struct {
  int nargs;
  pure_expr *a, *x, *y;
  bool except;
} cbdata;

/* C wrapper around a Pure callback. This is used for scalar functions. */

static void function_cb(sqlite3_context *ctx, int nargs, sqlite3_value **args)
{
  cbdata *cb = (cbdata*)sqlite3_user_data(ctx);
  if (cb && cb->x) {
    int i;
    pure_expr *x = cb->x, *e = NULL;
    if (cb->nargs > 0) {
      pure_expr **xv = malloc(cb->nargs*sizeof(pure_expr*));
      if (!xv) {
	sqlite3_result_error(ctx, "[pure] memory overflow", -1);
	return;
      }
      for (i = 0; i < nargs; i++)
	xv[i] = sql3util_value(args[i]);
      x = pure_appxv(x, cb->nargs, xv, &e);
      free(xv);
    } else if (cb->nargs == 0)
      /* Zero-argument function, pass a dummy () argument. */
      x = pure_appx(x, pure_tuplel(0), &e);
    /* Variadic function, pass a single list with all the arguments. */
    else if (nargs <= 0)
      x = pure_appx(x, pure_listl(0), &e);
    else {
      pure_expr **xv = malloc(nargs*sizeof(pure_expr*));
      if (!xv) {
	sqlite3_result_error(ctx, "[pure] memory overflow", -1);
	return;
      }
      for (i = 0; i < nargs; i++)
	xv[i] = sql3util_value(args[i]);
      x = pure_appx(x, pure_listv(nargs, xv), &e);
      free(xv);
    }
    if (x) {
      sql3util_result_value(ctx, x);
      pure_freenew(x);
    } else {
      /* Pure exception. We handle this here so that we can report an error to
	 the SQLite engine. */
      if (e) pure_freenew(e);
      sqlite3_result_error(ctx, "[pure] exception", -1);
    }
  } else
    sqlite3_result_error(ctx, "[pure] invalid callback function", -1);
}

/* Step function in an aggregate. */

static pure_expr **local_state(sqlite3_context *ctx, pure_expr *a)
{
  pure_expr **p = (pure_expr**)
    sqlite3_aggregate_context(ctx, sizeof(pure_expr*));
  if (!*p)
    // need to initialize this instance
    *p = pure_new(a);
  return p;
}

static void step_cb(sqlite3_context *ctx, int nargs, sqlite3_value **args)
{
  cbdata *cb = (cbdata*)sqlite3_user_data(ctx);
  if (cb && cb->x && cb->y && cb->a) {
    int i;
    pure_expr *x = cb->x, **a = NULL, *e = NULL;
    if (cb->except)
      /* Exception in previous invocation of the step function, bail out. */
      return;
    /* Get our local state. */
    a = local_state(ctx, cb->a);
    if (cb->nargs >= 0) {
      pure_expr **xv = malloc((cb->nargs+1)*sizeof(pure_expr*));
      if (!xv) {
	sqlite3_result_error(ctx, "[pure] memory overflow", -1);
	return;
      }
      xv[0] = *a;
      for (i = 0; i < nargs; i++)
	xv[i+1] = sql3util_value(args[i]);
      x = pure_appxv(x, cb->nargs+1, xv, &e);
      free(xv);
    } else {
      /* Variadic function, pass a single list with all the arguments. */
      pure_expr **xv = malloc(nargs*sizeof(pure_expr*));
      if (nargs > 0 && !xv) {
	sqlite3_result_error(ctx, "[pure] memory overflow", -1);
	return;
      }
      for (i = 0; i < nargs; i++)
	xv[i] = sql3util_value(args[i]);
      x = pure_appxl(x, &e, 2, *a, pure_listv(nargs, xv));
      if (xv) free(xv);
    }
    if (x) {
      /* Update the accumulated value. */
      pure_expr *b = *a;
      *a = pure_new(x);
      pure_free(b);
    } else {
      /* Record an exception in a step function. */
      if (e) pure_freenew(e);
      pure_free(*a);
      cb->except = true;
    }
  } else
    sqlite3_result_error(ctx, "[pure] invalid callback function", -1);
}

/* Final function in an aggregate. */

static void final_cb(sqlite3_context *ctx)
{
  cbdata *cb = (cbdata*)sqlite3_user_data(ctx);
  if (cb && cb->y && cb->a) {
    pure_expr *x = cb->y, **a = NULL, *e = NULL;
    if (cb->except) {
      /* Exception in previous invocation of the step function, bail out with
	 an error. */
      sqlite3_result_error(ctx, "[pure] exception", -1);
      return;
    }
    /* Get our local state. */
    a = local_state(ctx, cb->a);
    x = pure_appx(x, *a, &e);
    if (x) {
      sql3util_result_value(ctx, x);
      pure_freenew(x);
    } else {
      if (e) pure_freenew(e);
      sqlite3_result_error(ctx, "[pure] exception", -1);
    }
    pure_free(*a);
  } else
    sqlite3_result_error(ctx, "[pure] invalid callback function", -1);
}

/* Passing a NULL pointer for the function argument x removes any existing
   definition. NOTE: This leaks a tiny amount of memory for each callback
   definition, since at present we don't keep track of the cbdata blocks
   anywhere. This won't usually be a problem, unless your application opens an
   arbitrary amount of database connections and register callbacks in each of
   them, so don't do that. */

int sql3util_create_function(sqlite3 *db, const char *name, int nargs,
			     pure_expr *x)
{
  void *p;
  if (pure_is_pointer(x, &p) && p == NULL)
    return sqlite3_create_function(db, name, nargs, SQLITE_UTF8, NULL,
				   NULL, NULL, NULL);
  else {
    cbdata *cb = (cbdata*)malloc(sizeof(cbdata));
    pure_expr **xv;
    size_t n;
    if (!cb) return SQLITE_NOMEM;
    cb->nargs = nargs; cb->except = false;
    if (pure_is_tuplev(x, &n, &xv) && n>=3) {
      /* A pair of functions (step,final) and a start value which implements
	 an aggregate function. */
      cb->x = pure_new(xv[0]); cb->y = pure_new(xv[1]);
      if (n == 3)
	cb->a = pure_new(xv[2]);
      else
	/* start value is a tuple */
	cb->a = pure_new(pure_tuplev(n-2, xv+2));
      free(xv);
      return sqlite3_create_function(db, name, nargs, SQLITE_UTF8, cb,
				     NULL, step_cb, final_cb);
    } else {
      /* A simple scalar function. */
      cb->x = pure_new(x); cb->y = cb->a = NULL;
      return sqlite3_create_function(db, name, nargs, SQLITE_UTF8, cb,
				     function_cb, NULL, NULL);
    }
  }
}
