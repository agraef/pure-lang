
#include <pure/runtime.h>
#include <lo/lo.h>
#include <gmp.h>
#include <gsl/gsl_matrix.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

/* XXXFIXME: This needs to be crosschecked against the liblo source. We need
   these to hack some fields which aren't provided anywhere in the liblo API. */

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

typedef struct _lo_message {
	char      *types;
	size_t     typelen;
	size_t     typesize;
	void      *data;
	size_t     datalen;
	size_t     datasize;
	lo_address source;
        lo_arg   **argv;
} *mylo_message;

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

/* Generic server callback function which stores messages in a ringbuffer. */

#include <jack/ringbuffer.h>

static bool init = false;
static jack_ringbuffer_t *rb = NULL;

static size_t packet_size(const char *host, const char *port, int proto,
			 const char *path, const char *types,
			 lo_arg **argv, int argc)
{
  size_t sz = 0, i = 0, n = strlen(host), m = strlen(port);
  sz += sizeof(size_t);
  sz += n;
  sz += sizeof(size_t);
  sz += m;
  sz += sizeof(int);
  n = strlen(path); m = strlen(types);
  sz += sizeof(size_t);
  sz += n;
  sz += sizeof(size_t);
  sz += m;
  while (types && *types) {
    switch (*types++) {
    case LO_CHAR:
      sz += sizeof(char);
      break;
    case LO_INT32:
      sz += sizeof(int);
      break;
    case LO_INT64:
      sz += sizeof(int64_t);
      break;
    case LO_FLOAT:
      sz += sizeof(float);
      break;
    case LO_DOUBLE:
      sz += sizeof(double);
      break;
    case LO_STRING:
    case LO_SYMBOL:
      m = strlen((char*)argv[i]);
      sz += sizeof(size_t);
      sz += m;
      break;
    case LO_BLOB:
      m = lo_blob_datasize(argv[i]);
      sz += sizeof(size_t);
      sz += m;
      break;
    case LO_TIMETAG:
      sz += sizeof(lo_timetag);
      break;
    case LO_MIDI:
      sz += 4*sizeof(uint8_t);
      break;
    default:
      /* no data */
      break;
    }
  }
  return sz;
}

/* NOTE: These are obviously not atomic, so we assume that there's only one
   writer and one reader here. */

static bool write_packet(const char *host, const char *port, int proto,
			 const char *path, const char *types,
			 lo_arg **argv, int argc)
{
  size_t i = 0, n = strlen(host), m = strlen(port);
  if (packet_size(host, port, proto, path, types, argv, argc) >
      jack_ringbuffer_write_space(rb))
    return false;
  jack_ringbuffer_write(rb, (char*)&n, sizeof(size_t));
  jack_ringbuffer_write(rb, host, n);
  jack_ringbuffer_write(rb, (char*)&m, sizeof(size_t));
  jack_ringbuffer_write(rb, port, m);
  jack_ringbuffer_write(rb, (char*)&proto, sizeof(int));
  n = strlen(path); m = strlen(types);
  jack_ringbuffer_write(rb, (char*)&n, sizeof(size_t));
  jack_ringbuffer_write(rb, path, n);
  jack_ringbuffer_write(rb, (char*)&m, sizeof(size_t));
  jack_ringbuffer_write(rb, types, m);
  while (types && *types) {
    switch (*types++) {
    case LO_CHAR:
      jack_ringbuffer_write(rb, (char*)argv[i++], sizeof(char));
      break;
    case LO_INT32:
      jack_ringbuffer_write(rb, (char*)argv[i++], sizeof(int));
      break;
    case LO_INT64:
      jack_ringbuffer_write(rb, (char*)argv[i++], sizeof(int64_t));
      break;
    case LO_FLOAT:
      jack_ringbuffer_write(rb, (char*)argv[i++], sizeof(float));
      break;
    case LO_DOUBLE:
      jack_ringbuffer_write(rb, (char*)argv[i++], sizeof(double));
      break;
    case LO_STRING:
    case LO_SYMBOL:
      m = strlen((char*)argv[i]);
      jack_ringbuffer_write(rb, (char*)&m, sizeof(size_t));
      jack_ringbuffer_write(rb, (char*)argv[i++], m);
      break;
    case LO_BLOB:
      m = lo_blob_datasize(argv[i]);
      jack_ringbuffer_write(rb, (char*)&m, sizeof(size_t));
      jack_ringbuffer_write(rb, (char*)lo_blob_dataptr(argv[i++]), m);
      break;
    case LO_TIMETAG:
      jack_ringbuffer_write(rb, (char*)argv[i++], sizeof(lo_timetag));
      break;
    case LO_MIDI:
      jack_ringbuffer_write(rb, (char*)argv[i++], 4*sizeof(uint8_t));
      break;
    default:
      /* no data */
      break;
    }
  }
  return true;
}

static void read_packet(char **host, char **port, int *proto,
			char **path, char **_types,
			lo_arg ***argv, int *argc)
{
  size_t i = 0, n, m;
  char *buf, *types;
  jack_ringbuffer_read(rb, (char*)&n, sizeof(size_t));
  *host = calloc(n+1, 1); assert(*host);
  jack_ringbuffer_read(rb, *host, n);
  jack_ringbuffer_read(rb, (char*)&m, sizeof(size_t));
  *port = calloc(m+1, 1); assert(*port);
  jack_ringbuffer_read(rb, *port, m);
  jack_ringbuffer_read(rb, (char*)proto, sizeof(int));
  jack_ringbuffer_read(rb, (char*)&n, sizeof(size_t));
  *path = calloc(n+1, 1); assert(*path);
  jack_ringbuffer_read(rb, *path, n);
  jack_ringbuffer_read(rb, (char*)&m, sizeof(size_t));
  *_types = calloc(m+1, 1); assert(*_types);
  jack_ringbuffer_read(rb, *_types, m);
  types = *_types;
  *argc = m; /* upper bound */
  *argv = calloc(m, sizeof(lo_arg*)); assert(*argv);
  while (types && *types) {
    switch (*types++) {
    case LO_CHAR:
      buf = malloc(sizeof(char)); assert(buf);
      (*argv)[i++] = (lo_arg*)buf;
      jack_ringbuffer_read(rb, buf, sizeof(char));
      break;
    case LO_INT32:
      buf = malloc(sizeof(int)); assert(buf);
      (*argv)[i++] = (lo_arg*)buf;
      jack_ringbuffer_read(rb, buf, sizeof(int));
      break;
    case LO_INT64:
      buf = malloc(sizeof(int64_t)); assert(buf);
      (*argv)[i++] = (lo_arg*)buf;
      jack_ringbuffer_read(rb, buf, sizeof(int64_t));
      break;
    case LO_FLOAT:
      buf = malloc(sizeof(float)); assert(buf);
      (*argv)[i++] = (lo_arg*)buf;
      jack_ringbuffer_read(rb, buf, sizeof(float));
      break;
    case LO_DOUBLE:
      buf = malloc(sizeof(double)); assert(buf);
      (*argv)[i++] = (lo_arg*)buf;
      jack_ringbuffer_read(rb, buf, sizeof(double));
      break;
    case LO_STRING:
    case LO_SYMBOL:
      jack_ringbuffer_read(rb, (char*)&m, sizeof(size_t));
      buf = calloc(m+1, 1); assert(buf); (*argv)[i++] = (lo_arg*)buf;
      jack_ringbuffer_read(rb, buf, m);
      break;
    case LO_BLOB:
      jack_ringbuffer_read(rb, (char*)&m, sizeof(size_t));
      buf = calloc(m+1, 1); assert(buf);
      jack_ringbuffer_read(rb, buf, m);
      (*argv)[i++] = lo_blob_new(m, buf); free(buf);
      break;
    case LO_TIMETAG:
      buf = malloc(sizeof(lo_timetag)); assert(buf);
      (*argv)[i++] = (lo_arg*)buf;
      jack_ringbuffer_read(rb, buf, sizeof(lo_timetag));
      break;
    case LO_MIDI:
      buf = malloc(4*sizeof(uint8_t)); assert(buf);
      (*argv)[i++] = (lo_arg*)buf;
      jack_ringbuffer_read(rb, buf, 4*sizeof(uint8_t));
      break;
    default:
      /* no data */
      break;
    }
  }
  *argc = i;
}
			 
int Pure_osc_handler(const char *path, const char *types, lo_arg **argv,
		     int argc, void *msg, void *user_data)
{
  lo_address src = lo_message_get_source(msg);
  const char *host = lo_address_get_hostname(src);
  const char *port = lo_address_get_port(src);
  int proto = lo_address_get_protocol(src);

  if (!init) {
    rb = jack_ringbuffer_create(0x100000);
    init = true;
    if (!rb)
      fprintf(stderr, "generic_handler: error allocating ringbuffer\n");
  }
  if (rb)
    write_packet(host, port, proto, path, types, argv, argc);
  return 0;
}

pure_expr *Pure_osc_recv_noblock(void)
{
  pure_expr *res;
  mylo_address src;
  mylo_message msg;
  char *host, *port, *path, *types, *_types;
  int proto;
  lo_arg **argv;
  int argc, i = 0;
  if (!rb || jack_ringbuffer_read_space(rb) == 0) return NULL;
  read_packet(&host, &port, &proto, &path, &types, &argv, &argc);
  _types = types;
  /* Reconstruct the message. */
  src = lo_address_new(host, port);
  src->protocol = proto;
  msg = lo_message_new();
  assert(src && msg);
  /* We have to to go a long way here. There must be a better way to do this. */
  while (types && *types) {
    switch (*types++) {
    case LO_CHAR:
      lo_message_add_char(msg, argv[i++]->c);
      break;
    case LO_INT32:
      lo_message_add_int32(msg, argv[i++]->i);
      break;
    case LO_INT64:
      lo_message_add_int64(msg, argv[i++]->h);
      break;
    case LO_FLOAT:
      lo_message_add_float(msg, argv[i++]->f);
      break;
    case LO_DOUBLE:
      lo_message_add_double(msg, argv[i++]->d);
      break;
    case LO_STRING:
      lo_message_add_string(msg, (char*)argv[i++]);
      break;
    case LO_SYMBOL:
      lo_message_add_symbol(msg, (char*)argv[i++]);
      break;
    case LO_BLOB:
      lo_message_add_blob(msg, (lo_blob)argv[i++]);
      break;
    case LO_TIMETAG:
      lo_message_add_timetag(msg, argv[i++]->t);
      break;
    case LO_MIDI:
      lo_message_add_midi(msg, argv[i++]->m);
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
      /* ?? */
      break;
    }
  }
  argc = i;
  msg->source = src;
  res = pure_tuplel(2, pure_cstring_dup(path), pure_pointer(msg));
  free(host); free(port); free(path); free(_types);
  for (i = 0; i < argc; i++)
    free(argv[i]);
  free(argv);
  return res;
}

pure_expr *Pure_osc_recv(void)
{
  pure_expr *res;
  while ((res = Pure_osc_recv_noblock()) == NULL)
    usleep(10000);
  return res;
}

/* Generic error callback. */

void Pure_osc_error(int num, const char *msg, const char *path)
{
  if (path)
    fprintf(stderr, "liblo server error %d: %s (%s)\n", num, msg, path);
  else
    fprintf(stderr, "liblo server error %d: %s\n", num, msg);
}
