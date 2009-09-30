
#include <pure/runtime.h>
#include <lo/lo.h>
#include <gmp.h>
#include <gsl/gsl_matrix.h>
#include <stdio.h>
#include <stdlib.h>

// #define DEBUG

int Pure_lo_message_add(lo_message msg, const char *types, pure_expr *x)
{
  int count = 0;
  void *p;
  size_t sz;
  pure_expr **xs;
  int i;
  int64_t i64;
  mpz_t z;
  float f;
  char *s;
  lo_blob b;
  uint8_t *m;
  lo_timetag tt;
  double d;
  int ret = 0;

  if (!pure_is_listv(x, &sz, &xs) && !pure_is_tuplev(x, &sz, &xs)) return -2;

  while (types && *types) {
    pure_expr *x = NULL;

    switch (*types++) {

    case LO_INT32:
      if (count < sz && (x = xs[count++]) && pure_is_int(x, &i))
	lo_message_add_int32(msg, i);
      else
	goto err;
      break;

    case LO_FLOAT:
      if (count < sz && (x = xs[count++]) && pure_is_double(x, &d))
	lo_message_add_float(msg, (float)d);
      else
	goto err;
      break;

    case LO_STRING:
      if (count < sz && (x = xs[count++]) && pure_is_cstring_dup(x, &s)) {
	lo_message_add_string(msg, s);
	free(s);
      } else
	goto err;
      break;

    case LO_BLOB:
      if (count < sz && (x = xs[count++]) && pure_is_pointer(x, &p)) {
	b = (lo_blob)p;
	lo_message_add_blob(msg, b);
      } else
	goto err;
      break;

    case LO_INT64:
      if (count < sz && (x = xs[count++]) && pure_is_mpz(x, &z)) {
	lo_message_add_int64(msg, pure_get_int64(x));
	mpz_clear(z);
      } else
	goto err;
      break;

    case LO_TIMETAG:
      if (count < sz && (x = xs[count++])) {
	if (pure_is_pointer(x, &p)) {
	  tt = *(lo_timetag*)p;
	  lo_message_add_timetag(msg, tt);
	} else if (pure_is_int_matrix(x, &p)) {
	  /* We also allow a 1x2 int vector here. */
	  gsl_matrix_int *mat = (gsl_matrix_int*)p;
	  int *data = mat->data;
	  size_t nrows = mat->size1, ncols = mat->size2;
	  if (nrows == 1 && ncols == 2) {
	    tt = *(lo_timetag*)data;
	    lo_message_add_timetag(msg, tt);
	  } else
	    goto err;
	} else
	  goto err;
      } else
	goto err;
      break;

    case LO_DOUBLE:
      if (count < sz && (x = xs[count++]) && pure_is_double(x, &d))
	lo_message_add_double(msg, d);
      else
	goto err;
      break;

    case LO_SYMBOL:
      if (count < sz && (x = xs[count++]) && pure_is_cstring_dup(x, &s)) {
	lo_message_add_symbol(msg, s);
	free(s);
      } else
	goto err;
      break;

    case LO_CHAR:
      if (count < sz && (x = xs[count++]) && pure_is_int(x, &i))
	lo_message_add_char(msg, i);
      else
	goto err;
      break;

    case LO_MIDI:
      if (count < sz && (x = xs[count++])) {
	if (pure_is_pointer(x, &p)) {
	  m = (uint8_t*)p;
	  lo_message_add_midi(msg, m);
	} else if (pure_is_int_matrix(x, &p)) {
	  /* We also allow a 1x4 int vector here. */
	  gsl_matrix_int *mat = (gsl_matrix_int*)p;
	  int *data = mat->data;
	  size_t nrows = mat->size1, ncols = mat->size2;
	  if (nrows == 1 && ncols == 4) {
	    m = (uint8_t*)matrix_to_byte_array(NULL, x);
	    if (m) {
	      lo_message_add_midi(msg, m);
	      free(m);
	    }
	  } else
	    goto err;
	} else
	  goto err;
      } else
	goto err;
      break;

    case LO_TRUE:
      lo_message_add_true(msg);
      break;

    case LO_FALSE:
      lo_message_add_false(msg);
      break;

    case LO_NIL:
      lo_message_add_nil(msg);
      break;

    case LO_INFINITUM:
      lo_message_add_infinitum(msg);
      break;

    default:
      ret = -1; // unknown type
#ifdef DEBUG
      fprintf(stderr, "liblo warning: unknown type '%c'\n", *(types-1));
#endif
      break;
    }
  }
  if (count < sz) {
    ret = -2; // bad format/args
#ifdef DEBUG
    fprintf(stderr, "liblo error: lo_send or lo_message_add called with "
	    "mismatching types and data, exiting.\n");
#endif
  }
  if (xs) free(xs);
  return ret;
 err:
#ifdef DEBUG
  fprintf(stderr, "liblo error: lo_send or lo_message_add called with "
	  "invalid arg %d, probably arg mismatch, exiting.\n", count);
#endif
  if (xs) free(xs);
  return -2;
}

/* XXXFIXME: This needs to be crosschecked against the liblo source. We need
   this to set a proper error code in Pure_lo_send. */

typedef struct _lo_address {
	char            *host;
	int              socket;
	char            *port;
	int              protocol;
	struct addrinfo *ai;
	int              errnum;
	const char      *errstr;
	int              ttl;
} *mylo_address;

int Pure_lo_send(mylo_address t, const char *path, const char *types,
		 pure_expr *x)
{
  int ret;
  lo_message msg = lo_message_new();

  t->errnum = 0;
  t->errstr = NULL;

  ret = Pure_lo_message_add(msg, types, x);

  if (ret) {
    lo_message_free(msg);
    t->errnum = ret;
    if (ret == -1) t->errstr = "unknown type";
    else t->errstr = "bad format/args";
    return ret;
  }

  ret = lo_send_message(t, path, msg);
  lo_message_free(msg);

  return ret;
}

int Pure_lo_send_timestamped(mylo_address t, lo_timetag *ts,
			     const char *path, const char *types, pure_expr *x)
{
  int ret;
  lo_message msg = lo_message_new();
  lo_bundle b = lo_bundle_new(*ts);

  t->errnum = 0;
  t->errstr = NULL;

  ret = Pure_lo_message_add(msg, types, x);

  if (t->errnum) {
    lo_message_free(msg);
    return t->errnum;
  }

  lo_bundle_add_message(b, path, msg);
  ret = lo_send_bundle(t, b);
  lo_message_free(msg);
  lo_bundle_free(b);

  return ret;
}

int Pure_lo_send_from(mylo_address to, lo_server from, lo_timetag *ts,
		      const char *path, const char *types, pure_expr *x)
{
  lo_bundle b = NULL;
  int ret;
  lo_message msg = lo_message_new();
  if (ts->sec!=LO_TT_IMMEDIATE.sec || ts->frac!=LO_TT_IMMEDIATE.frac)
    b = lo_bundle_new(*ts);

  // Clear any previous errors
  to->errnum = 0;
  to->errstr = NULL;

  ret = Pure_lo_message_add(msg, types, x);

  if (to->errnum) {
    if (b) lo_bundle_free(b);
    lo_message_free(msg);
    return to->errnum;
  }

  if (b) {
    lo_bundle_add_message(b, path, msg);
    ret = lo_send_bundle_from(to, from, b);
  } else {
    ret = lo_send_message_from(to, from, path, msg);
  }
    
  // Free-up memory
  lo_message_free(msg);
  if (b) lo_bundle_free(b);

  return ret;
}
