/* sql3util.c */

#include <stdlib.h>
#include <pure/runtime.h>
#include <stdio.h>
#include <string.h>
#include <sqlite3.h>

/* Utilities to interface with sqlite3. To compile and link (Linux):

   gcc -fPIC -c sql3util.c
   gcc -shared sql3util.o -o sql3util.so -lsqlite3
 
   Or just run 'make'. */

sqlite3* sql3util_open(char* path) {
  sqlite3* db;
  int ec = sqlite3_open(path , &db);
  if(ec!=SQLITE_OK) {
    printf("sql3util error %s, ec = %d\n", path, ec); 
    sqlite3_close(db);
    db = NULL;
  }
  return db;
}

pure_expr* sql3util_prepare(sqlite3 *db, char *sql)
{
  size_t size;
  sqlite3_stmt* stmtp;
  const char* zTail;
  size_t maxlen = strlen(sql) + 1;
  int ec = sqlite3_prepare_v2(db, sql, maxlen, &stmtp, &zTail);

  pure_expr *p_stmtp = pure_pointer(stmtp);
  pure_expr *p_errCode = pure_int(ec);
  pure_expr *p_ret = pure_tuplel(2, p_stmtp, p_errCode);
  return p_ret;
}

int sql3util_bind_text(sqlite3_stmt* sp, int col, char* text)
{
  return sqlite3_bind_text(sp, col, text, -1, SQLITE_TRANSIENT);
}

/* Marshalling between Pure and SQLite3 data types (sqlite3_value*).
   2010-03-19 AG. */

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

/* Convert a sqlite3_value*, as returned by sqlite3_column_value or passed as
   a function parameter, to a corresponding Pure expression. */

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

/* Convert a Pure expression to a corresponding SQLite value. Note that since
   SQLite3 doesn't provide any direct means to construct a new sqlite3_value*
   object, instead we provide interfaces for the sqlite3_bind_value and
   sqlite3_result_value families which need these conversions. */

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
    sqlite3_bind_double(stm, col, dv);
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
    pure_throw(pure_app(pure_quoted_symbol(pure_sym("sql3::bad_sql_value")),
			x));
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
