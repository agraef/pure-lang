
/* Copyright (c) 2008 by Albert Graef <Dr.Graef@t-online.de>.

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

#ifdef _WIN32
#include <windows.h>
#endif

/* system headers */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#ifdef __MINGW32__
#include <malloc.h>
#endif

#include <sql.h>
#include <sqlext.h>

#if (ODBCVER < 0x0300)
#error "Sorry, this module requires ODBC 3.0 or later!"
#endif

#include <gmp.h>
#include <pure/runtime.h>

#define error_handler(msg) \
  pure_app(pure_app(pure_symbol(pure_sym("odbc::error")), \
  pure_cstring_dup("other error")), pure_cstring_dup(msg))

/* Query parameter structure */

typedef struct {
  short type; /* SQL parameter type */
  short ctype; /* C parameter type */
  SQLLEN len; /* length or indicator */
  long buflen; /* real buffer length */
  long prec; /* precision */
  void *ptr; /* buffer pointer */
  union {
    long iv; /* integer parameter */
    double fv; /* floating point parameter */
    char *buf; /* string or byte string parameter */
  } data;
} ODBCParam;

/* ODBC handle structure */

typedef struct {
  SQLHENV henv; /* environment handle */
  SQLHDBC hdbc; /* connection handle */
  SQLHSTMT hstmt; /* statement handle */
  unsigned char exec; /* set while statement is being executed */
  short *coltype; /* column types in current result set */
  short cols; /* number of columns */
  ODBCParam *argv; /* marked parameters */
  int argc; /* number of marked parameters */
} ODBCHandle;

static int init_args(ODBCHandle *db, int argc)
{
  int i;
  if (!(db->argv = malloc(argc*sizeof(ODBCParam))))
    return 0;
  db->argc = argc;
  for (i = 0; i < argc; i++) {
    db->argv[i].type = SQL_UNKNOWN_TYPE;
    db->argv[i].len = SQL_NULL_DATA;
  }
  return 1;
}

static void free_args(ODBCHandle *db)
{
  if (db->argv) {
    int i;
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    for (i = 0; i < db->argc; i++)
      if ((db->argv[i].type == SQL_BIGINT || db->argv[i].type == SQL_CHAR ||
	   db->argv[i].type == SQL_BINARY) &&
	  db->argv[i].data.buf)
	free(db->argv[i].data.buf);
    free(db->argv);
    db->argv = NULL;
    db->argc = 0;
  }
}

static int set_arg(ODBCHandle *db, int i, pure_expr *x)
{
  int32_t iv;
  double fv;
  char *s;
  mpz_t z;
  size_t size;
  pure_expr **elems;
  size_t numelem;
  unsigned char *buf;
  int64_t buflen;
  if (pure_is_int(x, &iv)) {
    db->argv[i].type = SQL_INTEGER;
    db->argv[i].ctype = SQL_C_SLONG;
    db->argv[i].len = sizeof(long);
    db->argv[i].buflen = sizeof(long);
    db->argv[i].prec = 10;
    db->argv[i].data.iv = iv;
    db->argv[i].ptr = &db->argv[i].data.iv;
    return 1;
  } else if (pure_is_mpz(x, &z)) {
    /* convert big integer values to BIGINTs via a string representation,
       so we don't have to fiddle with long long's here */
    db->argv[i].type = SQL_BIGINT;
    db->argv[i].ctype = SQL_C_CHAR;
    db->argv[i].len = SQL_NTS;
    db->argv[i].data.buf = mpz_get_str(NULL, 10, z);
    if (!db->argv[i].data.buf) return 0;
    db->argv[i].buflen = strlen(db->argv[i].data.buf)+1;
    db->argv[i].prec = db->argv[i].buflen-1;
    db->argv[i].ptr = db->argv[i].data.buf;
    mpz_clear(z);
    return 1;
  } else if (pure_is_double(x, &fv)) {
    db->argv[i].type = SQL_DOUBLE;
    db->argv[i].ctype = SQL_C_DOUBLE;
    db->argv[i].len = sizeof(double);
    db->argv[i].buflen = sizeof(double);
    db->argv[i].prec = 15;
    db->argv[i].data.fv = fv;
    db->argv[i].ptr = &db->argv[i].data.fv;
    return 1;
  } else if (pure_is_cstring_dup(x, &s)) {
    if (!s) return 0;
    db->argv[i].type = SQL_CHAR;
    db->argv[i].ctype = SQL_C_CHAR;
    db->argv[i].len = SQL_NTS;
    db->argv[i].buflen = strlen(s)+1;
    /* FIXME: The prec value should actually be buflen-1 here, but the MS
       Access ODBC interface barks at these. Hopefully this doesn't mess
       things up with other ODBC drivers. */
    db->argv[i].prec = db->argv[i].buflen;
    db->argv[i].data.buf = s;
    db->argv[i].ptr = s;
    return 1;
  } else if (pure_is_tuplev(x, &numelem, &elems)) {
      switch (numelem) {
	case 2:
	  if (!pure_is_pointer(elems[1], (void**)&buf)) {
            free(elems);
            return 0;
          }
	  if (pure_is_int(elems[0], &iv))
	    buflen = (int64_t)iv;
          else if (pure_is_mpz(elems[0], &z)) {
            mpz_clear(z);
            buflen = pure_get_long(elems[0]);
	  } else {
            free(elems);
            return 0;
          }
	  free(elems);
	  if (buflen<0) buflen = 0;
	  db->argv[i].type = SQL_BINARY;
	  db->argv[i].ctype = SQL_C_BINARY;
	  db->argv[i].len = (SQLLEN) buflen;
	  db->argv[i].buflen = (SQLLEN) buflen;
	  db->argv[i].prec = (SQLLEN) buflen;
	  if (buflen > 0) {
	    if (!(db->argv[i].data.buf = malloc(buflen)))
	      return 0;
	    memcpy(db->argv[i].data.buf, buf, (size_t) buflen);
	  } else
	    db->argv[i].data.buf = NULL;
	  db->argv[i].ptr = db->argv[i].data.buf;
	  return 1;
	case 0:
          db->argv[i].type = SQL_CHAR;
          db->argv[i].ctype = SQL_C_DEFAULT;
          db->argv[i].len = SQL_NULL_DATA;
          db->argv[i].buflen = 0;
          /* FIXME: The prec value should actually be zero, but again MS Access
             doesn't seem to like zero values here. Hopefully this doesn't mess
             things up with other ODBC drivers. */
          db->argv[i].prec = 1;
          db->argv[i].data.buf = NULL;
          db->argv[i].ptr = NULL;
          return 1;
	default: 
	  free(elems);
	  return 0;
    }
  } else
    return 0;
}

static void sql_close(ODBCHandle *db)
{
  if (db->exec) {
    if (db->coltype) free(db->coltype);
    free_args(db);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    db->coltype = NULL;
    db->cols = 0;
    db->exec = 0;
  }
}

static pure_expr *pure_err(SQLHENV henv, SQLHDBC hdbc, SQLHSTMT hstmt)
{
  SQLCHAR stat[10], msg[300];
  SQLINTEGER err;
  short len;
  /* check for SQL statement errors */
  if (hstmt && SQLGetDiagRec(SQL_HANDLE_STMT, hstmt, 1, stat, &err,
			     msg, sizeof(msg), &len) == SQL_SUCCESS)
    goto exit;
  /* check for connection errors */
  if (hdbc && SQLGetDiagRec(SQL_HANDLE_DBC, hdbc, 1, stat, &err,
			    msg, sizeof(msg), &len) == SQL_SUCCESS)
    goto exit;
  /* check for environment errors */
  if (henv && SQLGetDiagRec(SQL_HANDLE_ENV, henv, 1, stat, &err,
			    msg, sizeof(msg), &len) == SQL_SUCCESS)
    goto exit;
  return 0;
 exit:
  return pure_app(pure_app(pure_symbol(pure_sym("odbc::error")), pure_cstring_dup((const char*)msg)),
	       pure_cstring_dup((const char*)stat));
}

pure_expr *odbc_sources()
{
  SQLHENV henv;
  long ret;
  pure_expr **xv, *res;
  int n;
  SQLCHAR l_dsn[100],l_desc[100];
  short l_len1, l_len2, l_next;
  /* create an environment handle */
  if ((ret = SQLAllocHandle(SQL_HANDLE_ENV, NULL, &henv)) != SQL_SUCCESS &&
      ret != SQL_SUCCESS_WITH_INFO)
    return 0;
  if ((ret = SQLSetEnvAttr(henv, SQL_ATTR_ODBC_VERSION,
			   (SQLPOINTER) SQL_OV_ODBC3,
			   SQL_IS_UINTEGER)) != SQL_SUCCESS &&
      ret != SQL_SUCCESS_WITH_INFO) {
    pure_expr *msg = pure_err(henv, 0, 0);
    SQLFreeHandle(SQL_HANDLE_ENV, henv);
    return msg;
  }
  /* count the number of data sources */
  for (n = 0, l_next = SQL_FETCH_FIRST;
	SQLDataSources(henv, l_next, l_dsn, sizeof(l_dsn), &l_len1,
		       l_desc, sizeof(l_desc), &l_len2) == SQL_SUCCESS;
	l_next = SQL_FETCH_NEXT)
    n++;
  if (!(xv = (pure_expr**)malloc(n*sizeof(pure_expr)))) {
    SQLFreeHandle(SQL_HANDLE_ENV, henv);
    return 0;
  }
  /* retrieve the data source names and descriptions */
  for (n = 0, l_next = SQL_FETCH_FIRST;
    SQLDataSources(henv, l_next, l_dsn, sizeof(l_dsn), &l_len1,
		l_desc, sizeof(l_desc), &l_len2) == SQL_SUCCESS;
    l_next = SQL_FETCH_NEXT)
    xv[n++] = pure_tuplel(2, pure_cstring_dup((const char*)l_dsn),
		 pure_cstring_dup((const char*)l_desc));
  /* free the environment handle */
  SQLFreeHandle(SQL_HANDLE_ENV, henv);
  res = pure_listv(n, xv);
  free(xv);
  return res;
}

pure_expr *odbc_drivers()
{
  SQLHENV henv;
  long ret;
  pure_expr **xv, *res;
  int n;
  SQLCHAR l_drv[100],l_attr[10000];
  short l_len1, l_len2, l_next;
  /* create an environment handle */
  if ((ret = SQLAllocHandle(SQL_HANDLE_ENV, NULL, &henv)) != SQL_SUCCESS &&
    ret != SQL_SUCCESS_WITH_INFO)
    return 0;
  if ((ret = SQLSetEnvAttr(henv, SQL_ATTR_ODBC_VERSION,
			   (SQLPOINTER) SQL_OV_ODBC3,
			   SQL_IS_UINTEGER)) != SQL_SUCCESS &&
      ret != SQL_SUCCESS_WITH_INFO) {
    pure_expr *msg = pure_err(henv, 0, 0);
    SQLFreeHandle(SQL_HANDLE_ENV, henv);
    return msg;
  }
  /* count the number of driver descriptions */
  for (n = 0, l_next = SQL_FETCH_FIRST;
       SQLDrivers(henv, l_next, l_drv, sizeof(l_drv), &l_len1,
		  l_attr, sizeof(l_attr), &l_len2) == SQL_SUCCESS;
       l_next = SQL_FETCH_NEXT)
    n++;
  if (!(xv = (pure_expr **) malloc(n*sizeof(pure_expr)))) {
    SQLFreeHandle(SQL_HANDLE_ENV, henv);
    return error_handler("malloc error");
  }
  /* retrieve the driver and descriptions */
  for (n = 0, l_next = SQL_FETCH_FIRST;
       SQLDrivers(henv, l_next, l_drv, sizeof(l_drv), &l_len1,
		  l_attr, sizeof(l_attr), &l_len2) == SQL_SUCCESS;
       l_next = SQL_FETCH_NEXT) {
    int k;
    SQLCHAR *l_attrp;
    pure_expr **yv;
    /* count the number of attributes */
    for (k = 0, l_attrp = l_attr; *l_attrp;
	 l_attrp = l_attrp+strlen((char*)l_attrp)+1)
      k++;
    if (!(yv = malloc(k*sizeof(pure_expr)))) {
      int i;
      for (i = 0; i < n; i++)
	pure_freenew(xv[i]);
      free(xv);
      SQLFreeHandle(SQL_HANDLE_ENV, henv);
      return error_handler("malloc error");
    }
    /* get the attribute strings */
    for (k = 0, l_attrp = l_attr; *l_attrp;
	 l_attrp = l_attrp+strlen((char*)l_attrp)+1)
      yv[k++] = pure_cstring_dup((const char*)l_attrp);
    xv[n++] = pure_tuplel(2, pure_cstring_dup((const char*)l_drv), pure_listv(k, yv));
    free(yv);
  }
  /* free the environment handle */
  SQLFreeHandle(SQL_HANDLE_ENV, henv);
  res = pure_listv(n, xv);
  free(xv);
  return res;
}

pure_expr *odbc_connect(char* conn)
{
  if (conn) {
    ODBCHandle *db = (ODBCHandle*)malloc(sizeof(ODBCHandle));
    long ret;
    short buflen;
    char buf[1024];
    if (!db) return error_handler("malloc error");
    /* create the environment handle */
    if ((ret = SQLAllocHandle(SQL_HANDLE_ENV, NULL, &db->henv)) !=
	SQL_SUCCESS &&
	ret != SQL_SUCCESS_WITH_INFO) {
      return 0;
    }
    if ((ret = SQLSetEnvAttr(db->henv, SQL_ATTR_ODBC_VERSION,
			     (SQLPOINTER) SQL_OV_ODBC3,
			     SQL_IS_UINTEGER)) != SQL_SUCCESS &&
	ret != SQL_SUCCESS_WITH_INFO) {
      pure_expr *msg = pure_err(db->henv, 0, 0);
      SQLFreeHandle(SQL_HANDLE_ENV, db->henv);
      return msg;
    }
    /* create the connection handle */
    if ((ret = SQLAllocHandle(SQL_HANDLE_DBC, db->henv, &db->hdbc)) !=
	SQL_SUCCESS &&
	ret != SQL_SUCCESS_WITH_INFO) {
      pure_expr *msg = pure_err(db->henv, 0, 0);
      SQLFreeHandle(SQL_HANDLE_ENV, db->henv);
      return msg;
    }
    /* connect */
    if ((ret = SQLDriverConnect(db->hdbc, 0, (SQLCHAR*)conn, SQL_NTS,
				(SQLCHAR*)buf, sizeof(buf), &buflen,
				SQL_DRIVER_NOPROMPT)) != SQL_SUCCESS &&
	ret != SQL_SUCCESS_WITH_INFO) {
      pure_expr *msg = pure_err(db->henv, db->hdbc, 0);
      SQLFreeHandle(SQL_HANDLE_DBC, db->hdbc);
      SQLFreeHandle(SQL_HANDLE_ENV, db->henv);
      return msg;
    }
    /* create the statement handle */
    if ((ret = SQLAllocHandle(SQL_HANDLE_STMT, db->hdbc, &db->hstmt)) !=
	SQL_SUCCESS &&
	ret != SQL_SUCCESS_WITH_INFO) {
      pure_expr *msg = pure_err(db->henv, db->hdbc, 0);
      SQLDisconnect(db->hdbc);
      SQLFreeHandle(SQL_HANDLE_DBC, db->hdbc);
      SQLFreeHandle(SQL_HANDLE_ENV, db->henv);
      return msg;
    }
    /* initialize statement properties */
    db->argv = NULL;
    db->argc = 0;
    db->coltype = NULL;
    db->cols = 0;
    db->exec = 0;
    /* return the result */
    return pure_sentry(pure_symbol(pure_sym("odbc::disconnect")),
		       pure_pointer(db));
  } else
    return 0;
}

pure_expr *odbc_disconnect(pure_expr *dbpointer)
{
  ODBCHandle *db;
  if (pure_is_pointer(dbpointer, (void**)&db) &&
      db->henv) {
    sql_close(db);
    SQLCloseCursor(db->hstmt);
    SQLFreeHandle(SQL_HANDLE_STMT, db->hstmt);
    db->hstmt = 0;
    SQLDisconnect(db->hdbc);
    SQLFreeHandle(SQL_HANDLE_DBC, db->hdbc);
    db->hdbc = 0;
    SQLFreeHandle(SQL_HANDLE_ENV, db->henv);
    db->henv = 0;
    /* FIXME: This leaks memory on the handle itself. */
    return pure_tuplel(0);
  } else
    return 0;
}

pure_expr *odbc_info(pure_expr *dbpointer)
{
  ODBCHandle *db;
  if (pure_is_pointer(dbpointer, (void**)&db) &&
      db->henv) {
    long ret;
    int n = 0;
    pure_expr *xv[8], *res;
    char info[1024];
    short len;
    if ((ret  = SQLGetInfo(db->hdbc, SQL_DATA_SOURCE_NAME,
			   info, sizeof(info), &len)) == SQL_SUCCESS ||
	ret == SQL_SUCCESS_WITH_INFO)
      xv[n++] = pure_cstring_dup(info);
    else
      xv[n++] = pure_string_dup("");
    if ((ret  = SQLGetInfo(db->hdbc, SQL_DATABASE_NAME,
			   info, sizeof(info), &len)) == SQL_SUCCESS ||
	ret == SQL_SUCCESS_WITH_INFO)
      xv[n++] = pure_cstring_dup(info);
    else
      xv[n++] = pure_string_dup("");
    if ((ret  = SQLGetInfo(db->hdbc, SQL_DBMS_NAME,
			   info, sizeof(info), &len)) == SQL_SUCCESS ||
	ret == SQL_SUCCESS_WITH_INFO)
      xv[n++] = pure_cstring_dup(info);
    else
      xv[n++] = pure_string_dup("");
    if ((ret  = SQLGetInfo(db->hdbc, SQL_DBMS_VER,
			   info, sizeof(info), &len)) == SQL_SUCCESS ||
	ret == SQL_SUCCESS_WITH_INFO)
      xv[n++] = pure_cstring_dup(info);
    else
      xv[n++] = pure_string_dup("");
    if ((ret  = SQLGetInfo(db->hdbc, SQL_DRIVER_NAME,
			   info, sizeof(info), &len)) == SQL_SUCCESS ||
	ret == SQL_SUCCESS_WITH_INFO)
      xv[n++] = pure_cstring_dup(info);
    else
      xv[n++] = pure_string_dup("");
    if ((ret  = SQLGetInfo(db->hdbc, SQL_DRIVER_VER,
			   info, sizeof(info), &len)) == SQL_SUCCESS ||
	ret == SQL_SUCCESS_WITH_INFO)
      xv[n++] = pure_cstring_dup(info);
    else
      xv[n++] = pure_string_dup("");
    if ((ret  = SQLGetInfo(db->hdbc, SQL_DRIVER_ODBC_VER,
			   info, sizeof(info), &len)) == SQL_SUCCESS ||
	ret == SQL_SUCCESS_WITH_INFO)
      xv[n++] = pure_cstring_dup(info);
    else
      xv[n++] = pure_string_dup("");
    if ((ret  = SQLGetInfo(db->hdbc, SQL_ODBC_VER,
			   info, sizeof(info), &len)) == SQL_SUCCESS ||
	ret == SQL_SUCCESS_WITH_INFO)
      xv[n++] = pure_cstring_dup(info);
    else
      xv[n++] = pure_string_dup("");
    res = pure_tuplev(n, xv);
    return res;
  } else
    return 0;
}

pure_expr *odbc_getinfo(pure_expr *argv0, unsigned int info_type)
{
  ODBCHandle *db;
  if (pure_is_pointer(argv0, (void**)&db) &&
      db->henv) {
    long ret;
    char info[1024];
    short len;
    unsigned char *buf;
    /* A few queries (which are not supported by this interface right now)
       take pointer arguments, therefore we initialize the beginning of the
       buffer to prevent segfaults. */
    memset(info, 0, 32);
    if ((ret  = SQLGetInfo(db->hdbc, info_type,
			   info, sizeof(info), &len)) == SQL_SUCCESS ||
	ret == SQL_SUCCESS_WITH_INFO) {
      if (!(buf = (unsigned char*) malloc(len + 1)))
	return error_handler("malloc error");
      memcpy(buf, info, len);
      buf[len] = 0;
      return pure_sentry(pure_symbol(pure_sym("free")), pure_pointer(buf));
    } else
      return pure_err(db->henv, db->hdbc, 0);
  } else
    return 0;
}

/* Number of entries in SQLGetTypeInfo() result set. Table entries are
   allocated in chunks of this value. */
#define NMAX 128

/* Maximum length of string values. */
#define SL 256

#define checkstr(s,l) ((l==SQL_NULL_DATA)?pure_tuplel(0):pure_cstring_dup((char*)s))
#define checkint(x,l) ((l==SQL_NULL_DATA)?pure_tuplel(0):pure_int(x))
#define checkuint(x,l) ((l==SQL_NULL_DATA)?pure_tuplel(0):pure_int((long) x))
#define checkbool(x,l) ((l==SQL_NULL_DATA)?pure_tuplel(0):pure_int(x))

pure_expr *odbc_typeinfo(pure_expr *argv0, int id)
{
  ODBCHandle *db;
  if (pure_is_pointer(argv0, (void**)&db) &&
      db->henv) {
    pure_expr *res, **xs = (pure_expr**)malloc(NMAX*sizeof(pure_expr)), **xs1;
    int i, n = 0, m = NMAX;

    UCHAR  name[SL], prefix[SL], suffix[SL], params[SL], local_name[SL];
    SWORD  type, nullable, case_sen, searchable, unsign, money, auto_inc;
    SWORD  min_scale, max_scale;
    UDWORD prec;
    SDWORD len[20], ret;
    SWORD  sql_type, subcode, intv_prec;
    UDWORD prec_radix;

    if (!xs) return error_handler("malloc error");
    sql_close(db);

    ret = SQLBindCol(db->hstmt,  1, SQL_C_CHAR,  name,       SL, &len[1]);
    ret = SQLBindCol(db->hstmt,  2, SQL_C_SHORT, &type,       0, &len[2]);
    ret = SQLBindCol(db->hstmt,  3, SQL_C_LONG,  &prec,       0, &len[3]);
    ret = SQLBindCol(db->hstmt,  4, SQL_C_CHAR,  prefix,     SL, &len[4]);
    ret = SQLBindCol(db->hstmt,  5, SQL_C_CHAR,  suffix,     SL, &len[5]);
    ret = SQLBindCol(db->hstmt,  6, SQL_C_CHAR,  params,     SL, &len[6]);
    ret = SQLBindCol(db->hstmt,  7, SQL_C_SHORT, &nullable,   0, &len[7]);
    ret = SQLBindCol(db->hstmt,  8, SQL_C_SHORT, &case_sen,   0, &len[8]);
    ret = SQLBindCol(db->hstmt,  9, SQL_C_SHORT, &searchable, 0, &len[9]);
    ret = SQLBindCol(db->hstmt, 10, SQL_C_SHORT, &unsign,     0, &len[10]);
    ret = SQLBindCol(db->hstmt, 11, SQL_C_SHORT, &money,      0, &len[11]);
    ret = SQLBindCol(db->hstmt, 12, SQL_C_SHORT, &auto_inc,   0, &len[12]);
    ret = SQLBindCol(db->hstmt, 13, SQL_C_CHAR,  local_name, SL, &len[13]);
    ret = SQLBindCol(db->hstmt, 14, SQL_C_SHORT, &min_scale,  0, &len[14]);
    ret = SQLBindCol(db->hstmt, 15, SQL_C_SHORT, &max_scale,  0, &len[15]);
    ret = SQLBindCol(db->hstmt, 16, SQL_C_SHORT, &sql_type,   0, &len[16]);
    ret = SQLBindCol(db->hstmt, 17, SQL_C_SHORT, &subcode,    0, &len[17]);
    ret = SQLBindCol(db->hstmt, 18, SQL_C_LONG,  &prec_radix, 0, &len[18]);
    ret = SQLBindCol(db->hstmt, 19, SQL_C_SHORT, &intv_prec,  0, &len[19]);

    ret = SQLGetTypeInfo(db->hstmt, id);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) goto err;

    do {
      ret = SQLFetch(db->hstmt);
      switch (ret) {
       case SQL_SUCCESS_WITH_INFO:
       case SQL_SUCCESS:
	 if (n >= m) {
	   if ((xs1 = (pure_expr**)realloc(xs, (m+=NMAX)*sizeof(pure_expr))))
	     xs = xs1;
	   else
	     goto fatal;
	 }
	 xs[n++] = pure_tuplel(19,
			    checkstr(name, len[1]),
			    checkint(type, len[2]),
			    checkuint(prec, len[3]),
			    checkstr(prefix, len[4]),
			    checkstr(suffix, len[5]),
			    checkstr(params, len[6]),
			    checkint(nullable, len[7]),
			    checkbool(case_sen, len[8]),
			    checkint(searchable, len[9]),
			    checkbool(unsign, len[10]),
			    checkbool(money, len[11]),
			    checkbool(auto_inc, len[12]),
			    checkstr(local_name, len[13]),
			    checkint(min_scale, len[14]),
			    checkint(max_scale, len[15]),
			    checkint(sql_type, len[16]),
			    checkint(subcode, len[17]),
			    checkuint(prec_radix, len[18]),
			    checkint(intv_prec, len[19]));
	 break;
       case SQL_NO_DATA_FOUND:
	 break;
      default:
	goto err;
      }
    } while (ret != SQL_NO_DATA_FOUND);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    if (n == 0) {
      free(xs);
      return pure_listl(0);
    } else {
      res = pure_listv(n, xs);
      free(xs);
      return res;
      }
  err:
    for (i = 0; i < n; i++) pure_freenew(xs[i]);
    free(xs);
    res = pure_err(db->henv, db->hdbc, db->hstmt);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    return res;
  fatal:
    for (i = 0; i < n; i++) pure_freenew(xs[i]);
    free(xs);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    return error_handler("realloc error");
  } else
    return 0;
}

pure_expr *odbc_tables(pure_expr *argv0)
{
  ODBCHandle *db;
  if (pure_is_pointer(argv0, (void**)&db) &&
      db->henv) {
    pure_expr *res, **xs = (pure_expr**)malloc(NMAX*sizeof(pure_expr)), **xs1;
    int i, n = 0, m = NMAX;

    UCHAR  name[SL], type[SL];
    SDWORD len[6], ret;

    if (!xs) return error_handler("malloc error");
    sql_close(db);

    ret = SQLBindCol(db->hstmt,  3, SQL_C_CHAR,  name,       SL, &len[3]);
    ret = SQLBindCol(db->hstmt,  4, SQL_C_CHAR,  type,       SL, &len[4]);

    ret = SQLTables(db->hstmt, NULL, 0, NULL, 0, NULL, 0, NULL,0);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) goto err;

    do {
      ret = SQLFetch(db->hstmt);
      switch (ret) {
       case SQL_SUCCESS_WITH_INFO:
       case SQL_SUCCESS:
	 if (n >= m) {
	   if ((xs1 = (pure_expr**)realloc(xs, (m+=NMAX)*sizeof(pure_expr))))
	     xs = xs1;
	   else
	     goto fatal;
	 }
	 xs[n++] = pure_tuplel(2,
			    checkstr(name, len[3]),
			    checkstr(type, len[4]));
	 break;
       case SQL_NO_DATA_FOUND:
	 break;
      default:
	goto err;
      }
    } while (ret != SQL_NO_DATA_FOUND);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    if (n == 0) {
      free(xs);
      return pure_listl(0);
    } else {
      res = pure_listv(n, xs);
      free(xs);
      return res;
      }
  err:
    for (i = 0; i < n; i++) pure_freenew(xs[i]);
    free(xs);
    res = pure_err(db->henv, db->hdbc, db->hstmt);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    return res;
  fatal:
    for (i = 0; i < n; i++) pure_freenew(xs[i]);
    free(xs);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    return error_handler("realloc error");
  } else
    return 0;
}

pure_expr *odbc_columns(pure_expr *argv0, const char *tab)
{
  ODBCHandle *db;
  if (pure_is_pointer(argv0, (void**)&db) &&
      db->henv) {
    pure_expr *res, **xs = (pure_expr**)malloc(NMAX*sizeof(pure_expr)), **xs1;
    int i, n = 0, m = NMAX;

    UCHAR  name[SL], type[SL], nullable[SL], deflt[SL];
    SDWORD len[19], ret;

    if (!xs) return error_handler("malloc error");
    if (!tab) { free(xs); return error_handler("invalid table name string"); }
    sql_close(db);

    ret = SQLBindCol(db->hstmt,  4, SQL_C_CHAR,  name,       SL, &len[4]);
    ret = SQLBindCol(db->hstmt,  6, SQL_C_CHAR,  type,       SL, &len[6]);
    ret = SQLBindCol(db->hstmt, 13, SQL_C_CHAR,  deflt,      SL, &len[13]);
    ret = SQLBindCol(db->hstmt, 18, SQL_C_CHAR,  nullable,   SL, &len[18]);

    ret = SQLColumns(db->hstmt, NULL, 0, NULL, 0, (SQLCHAR*)tab, SQL_NTS,
		     NULL, 0);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) goto err;

    do {
      ret = SQLFetch(db->hstmt);
      switch (ret) {
       case SQL_SUCCESS_WITH_INFO:
       case SQL_SUCCESS:
	 if (n >= m) {
	   if ((xs1 = (pure_expr**)realloc(xs, (m+=NMAX)*sizeof(pure_expr))))
	     xs = xs1;
	   else
	     goto fatal;
	 }
	 xs[n++] = pure_tuplel(4,
			    checkstr(name, len[4]),
			    checkstr(type, len[6]),
			    checkstr(nullable, len[18]),
			    checkstr(deflt, len[13]));
	 break;
       case SQL_NO_DATA_FOUND:
	 break;
      default:
	goto err;
      }
    } while (ret != SQL_NO_DATA_FOUND);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    if (n == 0) {
      free(xs);
      return pure_listl(0);
    } else {
      res = pure_listv(n, xs);
      free(xs);
      return res;
      }
  err:
    for (i = 0; i < n; i++) pure_freenew(xs[i]);
    free(xs);
    res = pure_err(db->henv, db->hdbc, db->hstmt);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    return res;
  fatal:
    for (i = 0; i < n; i++) pure_freenew(xs[i]);
    free(xs);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    return error_handler("realloc error");
  } else
    return 0;
}

pure_expr *odbc_primary_keys(pure_expr *argv0, const char *tab)
{
  ODBCHandle *db;
  if (pure_is_pointer(argv0, (void**)&db) &&
      db->henv) {
    pure_expr *res, **xs = (pure_expr**)malloc(NMAX*sizeof(pure_expr)), **xs1;
    int i, n = 0, m = NMAX;

    UCHAR  name[SL];
    SDWORD len[5], ret;

    if (!xs) return error_handler("malloc error");
    if (!tab) { free(xs); return error_handler("invalid table name string"); }
    sql_close(db);

    ret = SQLBindCol(db->hstmt,  4, SQL_C_CHAR,  name,       SL, &len[4]);

    ret = SQLPrimaryKeys(db->hstmt, NULL, 0, NULL, 0, (SQLCHAR*)tab, SQL_NTS);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) goto err;

    do {
      ret = SQLFetch(db->hstmt);
      switch (ret) {
       case SQL_SUCCESS_WITH_INFO:
       case SQL_SUCCESS:
	 if (n >= m) {
	   if ((xs1 = (pure_expr**)realloc(xs, (m+=NMAX)*sizeof(pure_expr))))
	     xs = xs1;
	   else
	     goto fatal;
	 }
	 xs[n++] = checkstr(name, len[4]);
	 break;
       case SQL_NO_DATA_FOUND:
	 break;
      default:
	goto err;
      }
    } while (ret != SQL_NO_DATA_FOUND);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    if (n == 0) {
      free(xs);
      return pure_listl(0);
    } else {
      res = pure_listv(n, xs);
      free(xs);
      return res;
      }
  err:
    for (i = 0; i < n; i++) pure_freenew(xs[i]);
    free(xs);
    res = pure_err(db->henv, db->hdbc, db->hstmt);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    return res;
  fatal:
    for (i = 0; i < n; i++) pure_freenew(xs[i]);
    free(xs);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    return error_handler("realloc error");
  } else
    return 0;
}

pure_expr *odbc_foreign_keys(pure_expr *argv0, const char *tab)
{
  ODBCHandle *db;
  if (pure_is_pointer(argv0, (void**)&db) &&
      db->henv) {
    pure_expr *res, **xs = (pure_expr**)malloc(NMAX*sizeof(pure_expr)), **xs1;
    int i, n = 0, m = NMAX;

    UCHAR  name[SL], pktabname[SL], pkname[SL];
    SDWORD len[9], ret;

    if (!xs) return error_handler("malloc error");
    if (!tab) { free(xs); return error_handler("invalid table name string"); }
    sql_close(db);

    ret = SQLBindCol(db->hstmt,  3, SQL_C_CHAR,  pktabname,  SL, &len[3]);
    ret = SQLBindCol(db->hstmt,  4, SQL_C_CHAR,  pkname,     SL, &len[4]);
    ret = SQLBindCol(db->hstmt,  8, SQL_C_CHAR,  name,       SL, &len[8]);

    ret = SQLForeignKeys(db->hstmt, NULL, 0, NULL, 0, NULL, 0, NULL, 0,
			 NULL, 0, (SQLCHAR*)tab, SQL_NTS);
    if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO) goto err;

    do {
      ret = SQLFetch(db->hstmt);
      switch (ret) {
       case SQL_SUCCESS_WITH_INFO:
       case SQL_SUCCESS:
	 if (n >= m) {
	   if ((xs1 = (pure_expr**)realloc(xs, (m+=NMAX)*sizeof(pure_expr))))
	     xs = xs1;
	   else
	     goto fatal;
	 }
	 xs[n++] = pure_tuplel(3,
			    checkstr(name, len[8]),
			    checkstr(pktabname, len[3]),
			    checkstr(pkname, len[4]));
	 break;
       case SQL_NO_DATA_FOUND:
	 break;
      default:
	goto err;
      }
    } while (ret != SQL_NO_DATA_FOUND);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    if (n == 0) {
      free(xs);
      return pure_listl(0);
    } else {
      res = pure_listv(n, xs);
      free(xs);
      return res;
      }
  err:
    for (i = 0; i < n; i++) pure_freenew(xs[i]);
    free(xs);
    res = pure_err(db->henv, db->hdbc, db->hstmt);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    return res;
  fatal:
    for (i = 0; i < n; i++) pure_freenew(xs[i]);
    free(xs);
    SQLFreeStmt(db->hstmt, SQL_RESET_PARAMS);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    return error_handler("realloc error");
  } else
    return 0;
}

#define BUFSZ 65536
#define BUFSZ2 5000

pure_expr *odbc_sql_exec(pure_expr *argv0, const char *query, pure_expr *argv2)
{
  ODBCHandle *db;
  pure_expr **xv;
  size_t n;
  if (pure_is_pointer(argv0, (void**)&db) && db->henv &&
      pure_is_listv(argv2, &n, &xv)) {
    long ret;
    pure_expr *res, **xs;
    size_t i;
    short cols, *coltype = NULL;
    char buf[BUFSZ2];
    /* finalize previous query */
    sql_close(db);
    /* prepare statement */
    if (!query) {
      free(xv);
      return error_handler("invalid query string");
    }
    if ((ret = SQLPrepare(db->hstmt, (SQLCHAR*)query, SQL_NTS))
	!= SQL_SUCCESS &&
	ret != SQL_SUCCESS_WITH_INFO) {
      free(xv);
      return pure_err(db->henv, db->hdbc, db->hstmt);
    }
    /* bind parameters */
    if (n > 0) {
      if (!init_args(db, n))
	goto fatal;
      for (i = 0; i < n; i++)
	if (!set_arg(db, i, xv[i])) {
	  int alloc_error =
	    (db->argv[i].type == SQL_BIGINT ||
	     db->argv[i].type == SQL_CHAR ||
	     db->argv[i].type == SQL_BINARY) &&
	    !db->argv[i].data.buf;
	  free_args(db);
	  if (alloc_error)
	    goto fatal;
	  else
	    goto fail;
	}
      free(xv);
    }
    for (i = 0; i < db->argc; i++)
      if ((ret = SQLBindParameter(db->hstmt, i+1, SQL_PARAM_INPUT,
				  db->argv[i].ctype,
				  db->argv[i].type,
				  db->argv[i].prec, 0,
				  db->argv[i].ptr,
				  db->argv[i].buflen,
				  &db->argv[i].len)) != SQL_SUCCESS &&
	  ret != SQL_SUCCESS_WITH_INFO)
	goto err;
    /* execute statement */
    if ((ret = SQLExecute(db->hstmt)) != SQL_SUCCESS &&
	ret != SQL_SUCCESS_WITH_INFO)
      return pure_err(db->henv, db->hdbc, db->hstmt);
    /* determine the number of columns */
    if ((ret = SQLNumResultCols(db->hstmt, &cols)) != SQL_SUCCESS &&
	ret != SQL_SUCCESS_WITH_INFO)
      goto err;
    if (cols == 0) {
      SQLINTEGER rows;
      if ((ret = SQLRowCount(db->hstmt, &rows)) == SQL_SUCCESS ||
	  ret == SQL_SUCCESS_WITH_INFO)
	res = pure_int((long)rows);
      else
	res = pure_int(0);
      db->exec = 1;
      goto exit;
    }
    /* get the column names and types */
    if (!(coltype = malloc(cols*sizeof(short))))
      goto fatal;
    if (!(xs = malloc(cols*sizeof(pure_expr))))
      goto fatal;
    for (i = 0; i < cols; i++) {
      buf[0] = 0;
      if ((ret = SQLDescribeCol(db->hstmt, i+1, (SQLCHAR*)buf, sizeof(buf), NULL,
				&coltype[i], NULL, NULL, NULL))
	  != SQL_SUCCESS &&
	  ret != SQL_SUCCESS_WITH_INFO) {
	int j;
	for (j = 0; j < i; j++) pure_freenew(xs[j]);
	free(xs);
	goto err;
      }
      xs[i] = pure_cstring_dup(buf);
    }
    res = pure_listv(cols, xs);
    free(xs);
    if (res) {
      db->coltype = coltype;
      db->cols = cols;
      coltype = NULL;
      db->exec = 1;
    } else {
      free_args(db);
      SQLFreeStmt(db->hstmt, SQL_CLOSE);
    }
    goto exit;
  fail:
    free_args(db);
    free(xv);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    res = 0;
    goto exit;
  err:
    res = pure_err(db->henv, db->hdbc, db->hstmt);
    free_args(db);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    goto exit;
  fatal:
    free_args(db);
    free(xv);
    SQLFreeStmt(db->hstmt, SQL_CLOSE);
    res = error_handler("alloc error");
  exit:
    if (coltype) free(coltype);
    return res;
  } else
    return 0;
}

pure_expr *odbc_sql_fetch(pure_expr *argv0)
{
  ODBCHandle *db;
  if (pure_is_pointer(argv0, (void**)&db) &&
      db->henv && db->coltype) {
    long ret;
    pure_expr *res, **xs;
    short i, j, cols = db->cols, *coltype = db->coltype;
    long iv, sz = BUFSZ;
    double fv;
    char *buf = malloc(sz);
    SQLLEN len;
    if (!buf) goto fatal;
    /* fetch the next record */
    if ((ret = SQLFetch(db->hstmt)) == SQL_NO_DATA_FOUND) {
      res = 0;
      goto exit;
    } else if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO)
      goto err;
    if (!(xs = malloc(cols*sizeof(pure_expr))))
      goto fatal;
    /* get the columns */
    for (i = 0; i < cols; i++) {
      switch (coltype[i]) {
      case SQL_BIT:
      case SQL_TINYINT:
      case SQL_SMALLINT:
      case SQL_INTEGER:
	if ((ret = SQLGetData(db->hstmt, i+1, SQL_INTEGER, &iv,
			      sizeof(iv), &len) != SQL_SUCCESS) &&
	    ret != SQL_SUCCESS_WITH_INFO)
	  goto err2;
	if (len == SQL_NULL_DATA)
	  xs[i] = pure_tuplel(0);
	else
	  xs[i] = pure_int(iv);
	break;
      case SQL_BIGINT:
	/* hack to get bigint values converted to mpz_t, without having to
	   fiddle around with long long values
	   FIXME: we should really avoid the string conversion here */
	if ((ret = SQLGetData(db->hstmt, i+1, SQL_CHAR, buf,
			      sz, &len) != SQL_SUCCESS) &&
	    ret != SQL_SUCCESS_WITH_INFO)
	  goto err2;
	if (len == SQL_NULL_DATA)
	  xs[i] = pure_tuplel(0);
	else {
	  mpz_t z;
	  mpz_init(z);
	  mpz_set_str(z, buf, 0);
	  xs[i] = pure_mpz(z);
	  mpz_clear(z);
	}
	break;
      case SQL_DOUBLE:
      case SQL_DECIMAL:
      case SQL_NUMERIC:
      case SQL_FLOAT:
      case SQL_REAL:
	if ((ret = SQLGetData(db->hstmt, i+1, SQL_DOUBLE, &fv,
			      sizeof(fv), &len) != SQL_SUCCESS) &&
	    ret != SQL_SUCCESS_WITH_INFO)
	  goto err2;
	if (len == SQL_NULL_DATA)
	  xs[i] = pure_tuplel(0);
	else
	  xs[i] = pure_double(fv);
	break;
      case SQL_BINARY:
      case SQL_VARBINARY:
      case SQL_LONGVARBINARY: {
	char *bufp = buf;
	SQLLEN total = 0, actsz = sz;
	*buf = 0;
	while (1) {
	  if ((ret = SQLGetData(db->hstmt, i+1, SQL_BINARY, bufp,
				actsz, &len)) == SQL_SUCCESS ||
	      ret == SQL_NO_DATA) {
	    if (len == SQL_NULL_DATA)
	      break;
	    if (INT_MAX - len <= total)
	      goto fatal2;
	    else
	      total += len;
	    break;
	  } else if (ret == SQL_SUCCESS_WITH_INFO) {
	    /* we probably need to make room for additional data */
	    char *buf1;
	    if (len == SQL_NULL_DATA)
	      break;
	    if (INT_MAX - BUFSZ <= total)
	      goto fatal2;
	    else
	      total += actsz;
	    if (!(buf1 = realloc(buf, sz+BUFSZ)))
	      goto fatal2;
	    buf = buf1;
	    bufp = buf+total;
	    sz += BUFSZ;
	    actsz = BUFSZ;
	  } else {
	    /* some other error, bail out */
	    goto err2;
	  }
	}
	if (len == SQL_NULL_DATA)
	  xs[i] = pure_tuplel(0);
	else if (total == 0) {
	  xs[i] = pure_tuplel(2, pure_bigintval(pure_long(0)),
			      pure_pointer(NULL));
	} else {
	  char *buf1 = realloc(buf, total);
	  if (buf1) buf = buf1;
	  xs[i] = pure_tuplel(2, pure_bigintval(pure_long((int64_t) total)),
			      pure_sentry(pure_symbol(pure_sym("free")),
					  pure_pointer(buf)));
	  /* make a new buffer */
	  if (!(buf = malloc(BUFSZ)))
	    goto fatal2;
	  else
	    sz = BUFSZ;
	}
	break;
      }
      default: {
	char *bufp = buf;
	long total = 0, actsz = sz;
	*buf = 0;
	while (1) {
	  if ((ret = SQLGetData(db->hstmt, i+1, SQL_CHAR, bufp,
				actsz, &len)) == SQL_SUCCESS ||
	      ret == SQL_NO_DATA) {
	    if (len == SQL_NULL_DATA)
	      break;
	    if (INT_MAX - len <= total)
	      goto fatal2;
	    else
	      total += len;
	    break;
	  } else if (ret == SQL_SUCCESS_WITH_INFO) {
	    /* we probably need to make room for additional data */
	    char *buf1;
	    if (len == SQL_NULL_DATA)
	      break;
	    if (INT_MAX - BUFSZ <= total)
	      goto fatal2;
	    else
	      total += actsz-1;
	    if (!(buf1 = realloc(buf, sz+BUFSZ)))
	      goto fatal2;
	    buf = buf1;
	    bufp = buf+total;
	    sz += BUFSZ;
	    actsz = BUFSZ+1;
	  } else {
	    /* some other error, bail out */
	    goto err2;
	  }
	}
	if (len == SQL_NULL_DATA)
	  xs[i] = pure_tuplel(0);
	else {
	  xs[i] = pure_cstring_dup(buf);
	  if (sz > BUFSZ) {
	    /* shrink a (potentially large) buffer */
	    char *buf1 = realloc(buf, BUFSZ);
	    if (buf1) {
	      buf = buf1;
	      sz = BUFSZ;
	    }
	  }
	}
      }
      }
    }
    res = pure_listv(cols, xs);
    free(xs);
    goto exit;
  err2:
    for (j = 0; j < i; j++) pure_freenew(xs[j]);
    free(xs);
  err:
    res = pure_err(db->henv, db->hdbc, db->hstmt);
    goto exit;
  fatal2:
    for (j = 0; j < i; j++) pure_freenew(xs[j]);
    free(xs);
  fatal:
    res = error_handler("alloc or other error");
  exit:
    if (buf) free(buf);
    return res;
  } else
    return 0;
}

pure_expr *odbc_sql_more(pure_expr *argv0)
{
  ODBCHandle *db;
  if (pure_is_pointer(argv0, (void**)&db) &&
      db->henv && db->exec) {
    long ret;
    pure_expr *res, **xs;
    short i, cols, *coltype = NULL;
    char buf[BUFSZ2];
    /* get the next result set */
    if ((ret = SQLMoreResults(db->hstmt)) == SQL_NO_DATA_FOUND) {
      res = 0;
      goto exit;
    } else if (ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO)
      goto err;
    /* determine the number of columns */
    if ((ret = SQLNumResultCols(db->hstmt, &cols)) != SQL_SUCCESS &&
	ret != SQL_SUCCESS_WITH_INFO)
      goto err;
    if (cols == 0) {
      SQLINTEGER rows;
      if ((ret = SQLRowCount(db->hstmt, &rows)) == SQL_SUCCESS ||
	  ret == SQL_SUCCESS_WITH_INFO)
	res = pure_int((long)rows);
      else
	res = pure_int(0);
      if (db->coltype) free(db->coltype);
      db->coltype = NULL;
      db->cols = 0;
      goto exit;
    }
    /* get the column names and types */
    if (!(coltype = malloc(cols*sizeof(short))))
      goto fatal;
    if (!(xs = malloc(cols*sizeof(pure_expr))))
      goto fatal;
    for (i = 0; i < cols; i++) {
      buf[0] = 0;
      if ((ret = SQLDescribeCol(db->hstmt, i+1, (SQLCHAR*)buf, sizeof(buf), NULL,
				&coltype[i], NULL, NULL, NULL))
	  != SQL_SUCCESS &&
	  ret != SQL_SUCCESS_WITH_INFO) {
	int j;
	for (j = 0; j < i; j++) pure_freenew(xs[j]);
	free(xs);
	goto err;
      }
      xs[i] = pure_cstring_dup(buf);
    }
    res = pure_listv(cols, xs);
    free(xs);
    if (res) {
      free(db->coltype);
      if (db->coltype) db->coltype = coltype;
      db->cols = cols;
      coltype = NULL;
    }
    goto exit;
  err:
    res = pure_err(db->henv, db->hdbc, db->hstmt);
    goto exit;
  fatal:
    res = error_handler("malloc error");
  exit:
    if (coltype) free(coltype);
    return res;
  } else
    return 0;
}

pure_expr *odbc_sql_close(pure_expr *argv0)
{
  ODBCHandle *db;
  if (pure_is_pointer(argv0, (void**)&db) &&
      db->henv && db->exec) {
    sql_close(db);
    return pure_tuplel(0);
  } else
    return 0;
}
