
/* Copyright (c) 2009 by Albert Graef <Dr.Graef@t-online.de>.

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <pure/runtime.h>
#include <m_pd.h>

#ifndef VERSION
#define VERSION "0.0"
#endif

#ifndef LIBDIR
#define LIBDIR "/usr/local/lib/pd"
#endif

/* Ticks per millisecond of the internal clock. FIXME: Currently this is a
   fixed value (32.*441000./1000., cf. m_sched.c), will have to be rewritten
   if this changes. */

#define TICKS 14112.0

/* These aren't in the official API, but have been around at least since Pd
   0.39, so are hopefully stable. */

typedef struct _namelist
{
    struct _namelist *nl_next;
    char *nl_string;
} t_namelist;

extern t_namelist *sys_searchpath;

/* Return the hosting Pd version as a string in the format "major.minor". */

extern const char *pd_version_s(void)
{
  static char buf[MAXPDSTRING];
  sprintf(buf, "%d.%d", PD_MAJOR_VERSION, PD_MINOR_VERSION);
  return buf;
}

/* Return the Pd library directory. */

extern const char *pd_libdir_s(void)
{
  return LIBDIR;
}

/* Return the Pd search path, as a list of directory names. */

extern pure_expr *pd_path_sl(void)
{
#if 1
  /* You might have to disable this for some Windows builds of pd.dll which
     for some reason lack sys_searchpath. */
  size_t n;
  pure_expr *xs[1024];
  t_namelist *p = sys_searchpath;
  for (n = 0; p && n < 1024; ) {
    if (p->nl_string) {
      pure_expr *x = pure_cstring_dup(p->nl_string);
      if (x) xs[n++] = x;
    }
    p = p->nl_next;
  }
  return pure_listv(n, xs);
#else
  return pure_listv(0, 0);
#endif
}

/* Alternate interface to Pd's post() routine. The Pd routine can't be used
   directly in Pure since it's a printf-style varargs routine. Our version
   here just provides the capability to post a message string. */

extern void pd_post(const char *s)
{
  post("%s", s);
}

/* Provide access to the current logical Pd time in milliseconds. */

extern double pd_time(void)
{
  return clock_getlogicaltime()/TICKS;
}

/* Provide access to Pd arrays (sample buffers). */

#if PD_MAJOR_VERSION == 0 && PD_MINOR_VERSION < 41
/* Work around broken 64 bit support in older Pd versions. NOTE: This just
   makes the stuff below compile, but won't fix the bugs in Pd itself; you'll
   have to upgrade your Pd version anyway. */
#define garray_getfloatwords(x, size, vec) garray_getfloatarray(x, size, (t_float**)vec)
#endif

/* Declare this here so that we don't depend on GSL being installed. */
typedef struct 
{
  size_t size1;
  size_t size2;
  size_t tda;
  double* data;
  void* block;
  int owner;
} gsl_matrix;

extern int pd_getbuffersize(const char *name)
{
  t_symbol *sym = gensym((char*)name);
  t_garray *a = (t_garray*)pd_findbyclass(sym, garray_class);
  if (a) {
    int sz;
    t_word *buf;
    if (garray_getfloatwords(a, &sz, &buf))
      return sz;
    else
      return 0;
  } else
    return 0;
}

extern void pd_setbuffersize(const char *name, uint32_t sz)
{
  t_symbol *sym = gensym((char*)name);
  t_garray *a = (t_garray*)pd_findbyclass(sym, garray_class);
  if (a) garray_resize(a, (t_floatarg)sz);
}

extern pure_expr *pd_getbuffer(const char *name)
{
  t_symbol *sym = gensym((char*)name);
  t_garray *a = (t_garray*)pd_findbyclass(sym, garray_class);
  if (a) {
    int sz;
    t_word *buf;
    if (garray_getfloatwords(a, &sz, &buf) && buf) {
      pure_expr *res = matrix_from_double_array(1, sz, 0);
      if (res) {
	gsl_matrix *m = (gsl_matrix*)res->data.mat.p;
	double *p = m->data;
	int i;
	for (i = 0; i < sz; i++)
	  p[i] = (double)buf[i].w_float;
      }
      return res;
    } else
      return 0;
  } else
    return 0;
}

extern void pd_setbuffer(const char *name, pure_expr *x)
{
  uint32_t i, n;
  size_t m;
  int ix;
  pure_expr **xv = 0;
  if (pure_is_tuplev(x, &m, &xv) && m == 2 && pure_is_int(xv[0], &ix))
    x = xv[1];
  else
    ix = 0;
  if (xv) free(xv);
  if (matrix_type(x) == 1 && (n = matrix_size(x)) > 0) {
    gsl_matrix *m = (gsl_matrix*)x->data.mat.p;
    double *p = m->data;
    t_symbol *sym = gensym((char*)name);
    t_garray *a = (t_garray*)pd_findbyclass(sym, garray_class);
    if (a) {
      int sz;
      t_word *buf;
      if (garray_getfloatwords(a, &sz, &buf) && buf) {
	if (ix < 0) ix = 0;
	if (ix > sz) ix = sz;
	if (n > (uint32_t)(sz-ix)) n = (uint32_t)(sz-ix);
	for (i = 0; i < n; i++)
	  buf[ix+i].w_float = (float)p[i];
	garray_redraw(a);
      }
    }
  }
}

/* We maintain a single Pure interpreter instance for all Pure objects. */

static pure_interp *interp = 0;
static int void_sym = 0, delay_sym = 0;

/* The Pure object class. */

typedef struct _pure {
  t_object x_obj;
#ifdef __MINGW32__
  /* This seems to be necessary as some as yet undetermined Pd routine seems
     to write past the end of x_obj on Windows. */
  int fence;			/* dummy field (not used) */
#endif
  int n_in, n_out;		/* number of extra inlets and outlets */
  struct _px **in;		/* extra inlet proxies, see t_px below */
  t_outlet **out;		/* outlets */
  pure_expr *foo;		/* the object function */
  char *args;			/* creation arguments */
  char *tmp;			/* temporary storage */
  struct _pure *next, *prev;	/* double-linked list of all Pure objects */
  t_clock *clock;		/* wakeup for asynchronous processing */
  pure_expr *msg;		/* pending asynchronous message */
} t_pure;

/* Proxy objects for extra inlets (pilfered from flext by Thomas Grill). */

typedef struct _px {
  t_object obj;
#ifdef __MINGW32__
  int fence;
#endif
  int ix;			/* inlet index */
  t_pure *x;			/* parent */
} t_px;

/* The runtime class, which is used to control the Pure runtime environment.
   Responds to 'bang' messages by reloading all scripts and reinitializing all
   Pure objects accordingly. */

typedef struct _runtime {
  t_object obj;
#ifdef __MINGW32__
  int fence;
#endif
  t_outlet *out1, *out2;
} t_runtime;

/* Head and tail of the list of Pure objects. */

static t_pure *xhead = 0, *xtail = 0;

/* We keep track of the different classes in a linked list for now. */

typedef struct _classes {
  t_symbol *sym;
  t_class *class;
  char *dir;
  struct _classes *next;
} t_classes;

static t_classes *pure_classes = 0;
static t_class *px_class = 0, *runtime_class = 0;

static void add_class(t_symbol *sym, t_class *class, char *dir)
{
  t_classes *new = malloc(sizeof(t_classes));
  if (!new) return;
  new->sym = sym;
  new->class = class;
  new->dir = strdup(dir);
  new->next = pure_classes;
  pure_classes = new;
}

static t_class *lookup(t_symbol *sym)
{
  t_classes *act = pure_classes;
  while (act && act->sym != sym) act = act->next;
  if (act)
    return act->class;
  else
    return 0;
}

/* Helper functions to convert between Pd atoms and Pure expressions. */

static char *get_expr(t_symbol *sym, int argc, t_atom *argv)
{
  t_binbuf *b;
  char *exp_string, *s, *t;
  int exp_strlen, i, l = strcmp(sym->s_name, "pure")?strlen(sym->s_name)+1:0;

  b = binbuf_new();
  binbuf_add(b, argc, argv);
  binbuf_gettext(b, &exp_string, &exp_strlen);
  exp_string = (char *)t_resizebytes(exp_string, exp_strlen, exp_strlen+1);
  exp_string[exp_strlen] = 0;
  if (!(s = malloc(l+exp_strlen+1)))
    return 0;
  if (l > 0) {
    strcpy(s, sym->s_name); s[l-1] = ' ';
  }
  for (t = s+l, i = 0; i < exp_strlen; i++)
    if (exp_string[i] != '\\')
      *(t++) = exp_string[i];
  *t = 0;
  binbuf_free(b);
  freebytes(exp_string, exp_strlen+1);
  if ((t = realloc(s, strlen(s)+1)))
    s = t;
  return s;
}

static inline pure_expr *parse_symbol(t_pure *x, const char *s)
{
  size_t i, n = strlen(s);
  if (!isalpha(s[0])) goto err;
  for (i = 1; i < n; i++)
    if (!isalnum(s[i])) goto err;
  return pure_symbol(pure_sym(s));
 err:
  /* treat as a Pure string */
  return pure_cstring_dup(s);
}

static inline pure_expr *parse_expr(t_pure *x, const char *s)
{
  pure_expr *y = pure_eval(s);
  if (y == 0)
#if CHECK_SYNTAX
    /* complain about bad Pure syntax */
    pd_error(x, "pd-pure: invalid expression '%s'", s);
#else
    /* cast to a Pure string */
    y = pure_cstring_dup(s);
#endif
  return y;
}

static inline bool is_double(pure_expr *x, double *d)
{
  int i;
  if (pure_is_double(x, d))
    return true;
  else if (pure_is_int(x, &i)) {
    *d = (double)i;
    return true;
  } else
    return false;
}

static inline bool is_delay(pure_expr *x, double *t, pure_expr **msg)
{
  int sym;
  pure_expr *y, *z, *u, *v;
  if (pure_is_app(x, &y, &z) && pure_is_app(y, &u, &v) &&
      pure_is_symbol(u, &sym) && sym == delay_sym) {
    int i;
    if (pure_is_double(v, t))
      ;
    else if (pure_is_int(v, &i))
      *t = (double)i;
    else
      return false;
    *msg = z;
    return true;
  } else
    return false;
}

static inline void create_atom(t_atom *a, pure_expr *x)
{
  char *s;
  double d;
  if (is_double(x, &d))
    SETFLOAT(a, d);
  else {
    t_symbol *sym;
    if (!pure_is_cstring_dup(x, &s)) s = str(x);
    sym = gensym(s?s:"");
    SETSYMBOL(a, sym);
    if (s) free(s);
  }
}

/* Process an output message and route it through the given outlet. */

static inline bool check_outlet(t_pure *x, int k)
{
  /* check outlet index */
#if CHECK_OUTLET
  /* complain about bad outlets */
  if (k < 0) {
    pd_error(x, "pd-pure: bad outlet index %d, must be >= 0", k);
    return 0;
  } else if (x->n_out <= 0) {
    pd_error(x, "pd-pure: bad outlet index %d, object has no outlets", k);
    return 0;
  } else if (k >= x->n_out) {
    pd_error(x, "pd-pure: bad outlet index %d, must be < %d", k, x->n_out);
    return 0;
  } else
    return 1;
#else
  return (k >= 0) && (k < x->n_out);
#endif
}

static void send_message(t_pure *x, int k, pure_expr *y)
{
  char *sval = 0;
  double dval;
  int sym;
  pure_expr *f, **args;
  size_t argc;
  t_atom *argv = 0;
  int i;
  /* Translate Pure lists to corresponding Pd list messages. */
  if (pure_is_listv(y, &argc, &args)) {
    if (argc > 0) {
      argv = malloc(argc*sizeof(t_atom));
      if (!argv) goto errexit;
      for (i = 0; i < argc; i++)
	create_atom(&argv[i], args[i]);
    }
    outlet_list(x->out[k], &s_list, argc, argv);
    goto errexit;
  }
  /* get arguments */
  pure_is_appv(y, &f, &argc, &args);
  /* pd message generation */
  if (pure_is_cstring_dup(f, &sval)) {
    if (!check_outlet(x, k)) goto errexit;
    if (argc == 0)
      /* single symbol value */
      outlet_anything(x->out[k], gensym(sval), 0, NULL);
    else {
      t_symbol *t = gensym(sval);
      if (argc > 0) {
	argv = malloc(argc*sizeof(t_atom));
	if (!argv) goto errexit;
      }
      for (i = 0; i < argc; i++)
	create_atom(&argv[i], args[i]);
      outlet_anything(x->out[k], t, argc, argv);
    }
  } else if (is_double(f, &dval)) {
    if (!check_outlet(x, k)) goto errexit;
    if (argc == 0)
      /* single double value */
      outlet_float(x->out[k], dval);
    else {
      /* create a list message with the double value in front */
      argv = malloc((argc+1)*sizeof(t_atom));
      if (!argv) goto errexit;
      create_atom(&argv[0], f);
      for (i = 0; i < argc; i++)
	create_atom(&argv[i+1], args[i]);
      outlet_list(x->out[k], &s_list, argc+1, argv);
    }
  } else if (pure_is_symbol(f, &sym) && sym > 0 && sym != void_sym) {
    /* manufacture a message with the given symbol as selector */
    const char *pname;
    if (!check_outlet(x, k)) goto errexit;
    if ((pname = pure_sym_pname(sym))) {
       /* FIXME: This should be converted to the system encoding. */
      t_symbol *t = gensym((char*)pname);
      if (argc > 0) {
	argv = malloc(argc*sizeof(t_atom));
	if (!argv) goto errexit;
      }
      for (i = 0; i < argc; i++)
	create_atom(&argv[i], args[i]);
      outlet_anything(x->out[k], t, argc, argv);
    }
  }
 errexit:
  if (sval) free(sval);
  if (args) free(args);
  if (argv) free(argv);
}

/* Schedule a message to be delivered to the object after the given time
   interval (see timeout below). Notes: Negative or zero time values mean that
   the event is scheduled to be delivered immediately. There is only one timer
   per object. If another event has already been scheduled, it is cancelled
   and the new one is scheduled instead. Also, if the time value is inf or
   nan, any existing timer event is cancelled. */

static inline void delay_message(t_pure *x, double t, pure_expr *msg)
{
  const double inf = 1.0e307 * 1.0e307;
  if (x->msg) pure_free(x->msg);
  if (t != inf && /* this is false only for nan: */t == t) {
    x->msg = pure_new(msg);
    if (t < 0.0) t = 0.0;
    clock_delay(x->clock, t);
  } else
    clock_unset(x->clock);
}

/* Handle a message to the given inlet. The message is first converted to a
   corresponding Pure expression to which the object function is applied. The
   returned value is processed, converting results back to corresponding Pd
   messages and sending these messages through the appropriate outlets.
   Messages of the form 'delay t' (where t is an int or double time value in
   milliseconds) do not cause any output, but are scheduled to be delivered to
   the object after the given time interval (see timeout below). */

static void receive_message(t_pure *x, t_symbol *s, int k,
			    int argc, t_atom *argv)
{
  size_t i, j, n, m;
  double t;
  pure_expr *f = x->foo, *y, *z, *msg;
  pure_expr **xv = 0, **yv = 0;
  int ix;

  /* check whether we have something to evaluate */
  if (!f) return;

  /* Build the parameter expression from the message. Floats, lists and
     symbols get special treatment, other kinds of objects are passed using
     their string representation. */
  if (argc == 1 && s == &s_float && argv[0].a_type == A_FLOAT)
    y = pure_double(argv[0].a_w.w_float);
  else if (s == &s_list) {
    xv = (argc>0)?malloc(argc*sizeof(pure_expr*)):0;
    for (i = 0; i < argc; i++) {
      if (argv[i].a_type == A_FLOAT)
	z = pure_double(argv[i].a_w.w_float);
      else if (argv[i].a_type == A_SYMBOL)
	z = parse_symbol(x, argv[i].a_w.w_symbol->s_name);
      else {
	char buf[MAXPDSTRING];
	atom_string(argv+i, buf, MAXPDSTRING);
	z = pure_cstring_dup(buf);
      }
      if (z)
	xv[i] = z;
      else {
	for (j = 0; j < i; j++)
	  pure_free(xv[j]);
	free(xv);
	return;
      }
    }
    y = pure_listv(argc, xv);
    if (xv) free(xv); xv = 0;
  } else {
    y = parse_symbol(x, s->s_name);
    for (i = 0; i < argc; i++) {
      if (argv[i].a_type == A_FLOAT)
	z = pure_double(argv[i].a_w.w_float);
      else if (argv[i].a_type == A_SYMBOL)
	z = parse_symbol(x, argv[i].a_w.w_symbol->s_name);
      else {
	char buf[MAXPDSTRING];
	atom_string(argv+i, buf, MAXPDSTRING);
	z = pure_cstring_dup(buf);
      }
      if (z)
	y = pure_app(y, z);
      else {
	pure_freenew(y);
	return;
      }
    }
  }
  /* add the inlet index if needed */
  if (x->n_in > 0)
    y = pure_tuplel(2, pure_int(k), y);
  /* apply the object function */
  y = pure_new(pure_app(f, y));
  /* process the results and route them through the appropriate outlets */
  if (pure_is_listv(y, &n, &xv)) {
    for (i = 0; i < n; i++) {
      if (yv) free(yv); yv = 0;
      if (pure_is_tuplev(xv[i], &m, &yv) && m == 2 && pure_is_int(yv[0], &ix))
	send_message(x, ix, yv[1]);
      else if (is_delay(xv[i], &t, &msg))
	delay_message(x, t, msg);
      else
	send_message(x, 0, xv[i]);
    }
  } else if (pure_is_tuplev(y, &m, &yv) && m == 2 && pure_is_int(yv[0], &ix))
    send_message(x, ix, yv[1]);
  else if (is_delay(y, &t, &msg))
    delay_message(x, t, msg);
  else
    send_message(x, 0, y);
  if (xv) free(xv);
  if (yv) free(yv);
  pure_free(y);
}

/* Timer callback. This works similar to receive_message above, but is called
   when a scheduled clock event arrives. Here, the object function is applied
   to the pending message, and the results are then processed as usual. */

static void timeout(t_pure *x)
{
  size_t i, n, m;
  double t;
  pure_expr *f = x->foo, *y = x->msg, *msg;
  pure_expr **xv = 0, **yv = 0;
  int ix;

  /* check whether we have something to evaluate */
  if (!f || !y) return;
  /* apply the object function */
  y = pure_new(pure_app(f, y));
  pure_free(x->msg); x->msg = 0;
  /* process the results and route them through the appropriate outlets */
  if (pure_is_listv(y, &n, &xv)) {
    for (i = 0; i < n; i++) {
      if (yv) free(yv); yv = 0;
      if (pure_is_tuplev(xv[i], &m, &yv) && m == 2 && pure_is_int(yv[0], &ix))
	send_message(x, ix, yv[1]);
      else if (is_delay(xv[i], &t, &msg))
	delay_message(x, t, msg);
      else
	send_message(x, 0, xv[i]);
    }
  } else if (pure_is_tuplev(y, &m, &yv) && m == 2 && pure_is_int(yv[0], &ix))
    send_message(x, ix, yv[1]);
  else if (is_delay(y, &t, &msg))
    delay_message(x, t, msg);
  else
    send_message(x, 0, y);
  if (xv) free(xv);
  if (yv) free(yv);
  pure_free(y);
}

/* Handle a message to the leftmost inlet. */

static void pure_any(t_pure *x, t_symbol *s, int argc, t_atom *argv)
{
  receive_message(x, s, 0, argc, argv);
}

/* Handle messages to secondary inlets (these are routed through proxies,
   since only the first inlet of a Pd object can actually process arbitrary
   input messages). */

static void px_any(t_px *px, t_symbol *s, int argc, t_atom *argv)
{
  receive_message(px->x, s, px->ix, argc, argv);
}

/* Manage the object list. */

static void xappend(t_pure *x)
{
  x->prev = xtail; x->next = 0;
  if (xtail) xtail->next = x; xtail = x;
  if (!xhead) xhead = x;
}

static void xunlink(t_pure *x)
{
  if (x->prev)
    x->prev->next = x->next;
  else
    xhead = x->next;
  if (x->next)
    x->next->prev = x->prev;
  else
    xtail = x->prev;
}

/* Create a new Pure object. */

static void *pure_init(t_symbol *s, int argc, t_atom *argv)
{
  int i;
  t_pure *x;
  t_class *c = lookup(s);

  if (!c) return 0; /* this shouldn't happen unless we're out of memory */
  x = (t_pure*)pd_new(c);
  xappend(x);

  x->foo = 0;
  x->n_in = 0; x->n_out = 1;
  x->in = 0;
  x->out = 0;
  x->args = get_expr(s, argc, argv);
  x->tmp = 0;
  if (!x->args) {
    pd_error(x, "pd-pure: memory allocation failed");
    return (void *)x;
  }
  x->clock = clock_new(x, (t_method)timeout);
  x->msg = 0;
  /* initialize the object function and determine the number of inlets and
     outlets (these cannot be changed later) */
  if (x->args != 0) {
    int n_in = 1, n_out = 1;
    pure_expr *f = parse_expr(x, x->args);
    x->foo = f;
    if (f) {
      size_t n;
      pure_expr **xv = 0;
      pure_new(f);
      /* handle custom inlet/outlet configurations (n_in,n_out,foo) */
      if (pure_is_tuplev(f, &n, &xv) && n == 3 &&
	  pure_is_int(xv[0], &n_in) && pure_is_int(xv[1], &n_out)) {
	x->foo = pure_new(xv[2]); pure_free(f);
	if (n_in < 1) {
	  pd_error(x, "pd-pure: bad number %d of inlets, must be >= 1", n_in);
	  n_in = 1;
	}
	if (n_out < 0) {
	  pd_error(x, "pd-pure: bad number %d of outlets, must be >= 0", n_out);
	  n_out = 0;
	}
	x->n_in = n_in-1;
	x->n_out = n_out;
      }
      if (xv) free(xv);
    } else {
      const char *err = lasterr();
      pd_error(x, "pd-pure: error in '%s' creation function", s->s_name);
      if (err && *err) pd_error(x, "pd-pure: %s", err);
    }
  }
  if (x->foo != 0) {
    /* allocate memory for inlets and outlets */
    if (x->n_in > 0)
      x->in = malloc(x->n_in*sizeof(t_px*));
    if (x->n_out > 0)
      x->out = malloc(x->n_out*sizeof(t_outlet*));
    if (x->n_in > 0 && x->in == 0 ||
	x->n_out > 0 && x->out == 0)
      pd_error(x, "pd-pure: memory allocation failed");
    if (!x->in) x->n_in = 0;
    if (!x->out) x->n_out = 0;
    /* initialize the proxies for the extra inlets */
    for (i = 0; i < x->n_in; i++) {
      x->in[i] = (t_px*)pd_new(px_class);
      x->in[i]->x = x;
      x->in[i]->ix = i+1;
      inlet_new(&x->x_obj, &x->in[i]->obj.ob_pd, 0, 0);
    }
    /* initialize the outlets */
    for (i = 0; i < x->n_out; i++)
      x->out[i] = outlet_new(&x->x_obj, 0);
  } else {
    x->n_in = x->n_out = 0;
  }
  return (void *)x;
}

/* Finalize a Pure object. */

static void pure_fini(t_pure *x)
{
  int i;
  free(x->args);
  if (x->foo) pure_free(x->foo);
  x->foo = 0;
  for (i = 0; i < x->n_in; i++)
    pd_free((t_pd*)x->in[i]);
  if (x->in) free(x->in);
  if (x->out) free(x->out);
  xunlink(x);
}

/* Reinitialize Pure objects in a new interpreter context. */

static void pure_refini(t_pure *x)
{
  pure_free(x->foo);
  if (x->foo) x->foo = 0;
  if (x->msg) {
    x->tmp = str(x->msg);
    pure_free(x->msg);
    x->msg = 0;
  }
}

static void pure_reinit(t_pure *x)
{
  if (x->args != 0) {
    int n_in = 1, n_out = 1;
    pure_expr *f = parse_expr(x, x->args);
    x->foo = f;
    if (f) {
      size_t n;
      pure_expr **xv = 0;
      pure_new(f);
      if (pure_is_tuplev(f, &n, &xv) && n == 3 &&
	  pure_is_int(xv[0], &n_in) && pure_is_int(xv[1], &n_out)) {
	x->foo = pure_new(xv[2]); pure_free(f);
	/* FIXME: Number of inlets and outlets are initialized at object
	   creation time, can't change them here. */
      }
      if (xv) free(xv);
    }
  }
  if (x->tmp) {
    x->msg = parse_expr(x, x->tmp);
    free(x->tmp); x->tmp = 0;
  }
}

/* Setup for a Pure object class with the given name. */

static void class_setup(char *name, char *dir)
{
  t_symbol *class_s = gensym(name);
  t_class *class =
    class_new(class_s, (t_newmethod)pure_init, (t_method)pure_fini,
	      sizeof(t_pure), CLASS_DEFAULT, A_GIMME, A_NULL);
  class_addanything(class, pure_any);
  class_sethelpsymbol(class, gensym("../../extra/pure/pure-help"));
  add_class(class_s, class, dir);
}

/* Loader setup, pilfered from pd-lua (claudiusmaximus@goto10.org). */

void class_set_extern_dir(t_symbol *s);

static char dirbuf[MAXPDSTRING], cmdbuf[1000];

static int pure_loader(t_canvas *canvas, char *name)
{
  char *ptr;
  int fd = canvas_open(canvas, name, ".pure", dirbuf, &ptr, MAXPDSTRING, 1);
  if (fd >= 0) {
    close(fd);
    class_set_extern_dir(gensym(dirbuf));
    /* Load the Pure script. */
    sprintf(cmdbuf, "using \"%s/%s.pure\";\n", dirbuf, name);
#ifdef VERBOSE
    printf("pd-pure: compiling %s.pure\n", name);
#endif
    pure_evalcmd(cmdbuf);
#ifdef EAGER
    /* Force eager compilation. */
    pure_interp_compile(interp, pure_sym(name));
#endif
    /* Create the object class. */
    class_setup(name, dirbuf);
    class_set_extern_dir(&s_);
    return lookup(gensym(name)) != 0;
  } else
    return 0;
}

/* Reload all loaded Pure scripts in a new interpreter instance. */

static void reload(t_classes *c)
{
  /* Walk the list of classes recursively, in postorder, which is the order in
     which the classes were originally created. */
  if (c) {
    reload(c->next);
    if (strcmp(c->sym->s_name, "pure")) {
      class_set_extern_dir(gensym(c->dir));
      sprintf(cmdbuf, "using \"%s/%s.pure\";\n", c->dir, c->sym->s_name);
#ifdef VERBOSE
      printf("pd-pure: compiling %s.pure\n", c->sym->s_name);
#endif
      pure_evalcmd(cmdbuf);
#ifdef EAGER
      pure_interp_compile(interp, pure_sym(c->sym->s_name));
#endif
      class_set_extern_dir(&s_);
    }
  }
}

/* Create a new interpreter instance and reinitialize all objects. This must
   be invoked in a safe context, where no evaluations are in progress. */

static void pure_restart(void)
{
  t_pure *x;
  for (x = xhead; x; x = x->next)
    pure_refini(x);
  pure_delete_interp(interp);
  interp = pure_create_interp(0, 0);
  pure_switch_interp(interp);
#ifdef VERBOSE
  printf("pd-pure: reloading, please wait...\n");
#endif
  reload(pure_classes);
  for (x = xhead; x; x = x->next)
    pure_reinit(x);
  void_sym = pure_sym("()");
  delay_sym = pure_sym("pd_delay");
}

/* Methods of the runtime class. */

static void *runtime_init(t_symbol *s, int argc, t_atom *argv)
{
  t_runtime *x = (t_runtime*)pd_new(runtime_class);
  x->out1 = outlet_new(&x->obj, 0);
  x->out2 = outlet_new(&x->obj, 0);
  return (void *)x;
}

static void runtime_any(t_runtime *x, t_symbol *s, int argc, t_atom *argv)
{
  if (s == &s_bang && argc == 0) {
    outlet_bang(x->out2);
    pure_restart();
    outlet_bang(x->out1);
  } else if (s->s_thing)
    if (argc > 0 && argv[0].a_type == A_SYMBOL)
      pd_typedmess(s->s_thing, argv[0].a_w.w_symbol, argc-1, argv+1);
    else
      pd_list(s->s_thing, &s_list, argc, argv);
}

/* Loader setup. */

extern void pure_setup(void)
{
  char buf[MAXPDSTRING];
  char *ptr;
  int fd;
  interp = pure_create_interp(0, 0);
  if (interp) {
    pure_expr *x = pure_symbol(pure_sym("version"));
    char *pure_version = 0;
    pure_is_cstring_dup(x, &pure_version);
    post("pd-pure %s (pure-%s) (c) 2009 Albert Graef <Dr.Graef@t-online.de>", VERSION, pure_version);
    post("pd-pure: compiled for pd-%d.%d on %s %s", PD_MAJOR_VERSION, PD_MINOR_VERSION, __DATE__, __TIME__);
    if (pure_version) free (pure_version);
    /* Register the loader for Pure externals. */
    sys_register_loader(pure_loader);
    /* Create the proxy class for extra inlets. */
    px_class = class_new(gensym("pure proxy"), 0, 0,
			 sizeof(t_px), CLASS_PD|CLASS_NOINLET,
			 A_NULL);
    class_addanything(px_class, px_any);
    /* Create the runtime class. */
    runtime_class = class_new(gensym("pure-runtime"),
			      (t_newmethod)runtime_init, 0,
			      sizeof(t_runtime), CLASS_DEFAULT,
			      A_GIMME, A_NULL);
    class_addanything(runtime_class, runtime_any);
    class_sethelpsymbol(runtime_class, gensym("pure-help"));
    /* Create a class for 'pure' objects which allows you to access any
       predefined Pure function without loading a script. */
    class_setup("pure", "");
    /* Look up a few symbols that we need. */
    void_sym = pure_sym("()");
    delay_sym = pure_sym("pd_delay");
  } else
    error("pd-pure: error initializing interpreter; loader not registered");
}
