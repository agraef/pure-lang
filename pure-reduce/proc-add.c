
#include <stdlib.h>
#include "reduce-algebra/csl/cslbase/headers.h"

static size_t bufsz = 0, buflen = 0;
static char *buf = NULL;

static character_reader *proc_input = NULL;
static character_writer *proc_output = NULL;

static int char_output(int c)
{
  if (c > 0) {
    if (buflen+2 > bufsz) {
      char *buf1 = realloc(buf, bufsz+1024);
      if (!buf1) return 1;
      bufsz += 1024;
      buf = buf1;
    }
    buf[buflen++] = c;
  }
  return 0;
}

int PROC_capture_output(int flag)
{
  if (flag) {
    buflen = 0;
    proc_output = char_output;
  } else
    proc_output = NULL;
  return PROC_set_callbacks(proc_input, proc_output);
}

const char *PROC_get_output(void)
{
  if (buf) {
    buf[buflen] = 0;
    return buf;
  } else
    return "";
}

void PROC_clear_output(void)
{
  bufsz = buflen = 0;
  free(buf); buf = NULL;
}

static char *inbuf = NULL, *inbufptr = NULL;

static int char_input(void)
{
  if (inbufptr && *inbufptr)
    return *inbufptr++;
  else if (inbuf) {
    free(inbuf);
    inbufptr = inbuf = NULL;
    proc_input = NULL;
    PROC_set_callbacks(proc_input, proc_output);
  }
  return EOF;
}

int PROC_feed_input(const char *s)
{
  if (inbuf) free(inbuf);
  if (s) {
    inbufptr = inbuf = strdup(s);
    proc_input = char_input;
  } else {
    inbufptr = inbuf = NULL;
    proc_input = NULL;
  }
  return PROC_set_callbacks(proc_input, proc_output);
}

const char *PROC_my_string_data(PROC_handle p)
{
  static size_t bufsz = 0;
  static char *buf = NULL;
  Lisp_Object w = (Lisp_Object)p;
  size_t n = (size_t)length_of_header(vechdr(w)) - CELL;
  if (n+1 > bufsz) {
    char *buf1 = realloc(buf, n+1);
    if (!buf1) return NULL;
    bufsz = n+1;
    buf = buf1;
  }
  strncpy(buf, &celt(w, 0), n);
  buf[n] = 0;
  return buf;
}

int PROC_checksym(const char *s)
{
  void *res;
  PROC_push_symbol(s);
  PROC_make_function_call("quote", 1);
  PROC_make_function_call("getd", 1);
  PROC_lisp_eval();
  res = PROC_get_value();
  return !PROC_null(res);
}

int PROC_make_cons(void)
{
    Lisp_Object nil = C_nil;
    Lisp_Object w;
#ifdef CONSERVATIVE
    volatile Lisp_Object sp;
    C_stackbase = (Lisp_Object *)&sp;
#endif
    if (procstack == nil) return 1; /* Not enough args available */
    w = qcar(procstack);
    nil = C_nil;
    if (exception_pending()) {
      flip_exception();
      return 2;  /* Failed to pop from stack */
    }
    procstack = qcdr(procstack);
    if (procstack == nil) return 1; /* Not enough args available */
    w = cons(qcar(procstack), w);
    nil = C_nil;
    if (exception_pending()) {
      flip_exception();
      return 2;  /* Failed to pop from stack */
    }
    procstack = qcdr(procstack);
    w = cons(w, procstack);
    if (exception_pending()) {
      flip_exception();
      return 5;  /* Failed to push onto stack */
    }
    procstack = w;
    return 0;
}

/* TeXmacs support. The TeXmacs plugin requires Reduce anyway to do its
   pretty-printing, so we might just as well include these little helper
   functions here so that the plugin doesn't need its own C support library.
   Most of the support functions of the plugin are written in Pure, but the
   following functions are implemented in C for performance, since they have
   to be invoked inside the Pure interpreter's pretty-printer. */

#include <ctype.h>
#include <string.h>

/* stuff from proc.h we have to get rid of now so that we can include the Pure
   runtime header */
#undef eval
#undef symbolp
#include <pure/runtime.h>

/* Checks an identifier. */

int texmacs_valid(const char *s)
{
  while (*s) {
    if (!isalnum(*s)) return 0;
    s++;
  }
  return 1;
}

#define MAXLEN 10000

const char *texmacs_post(const char *s)
{
  int eq = 0;
  static char buf[MAXLEN];
  char *t = buf;
  while (isspace(*s)) s++;
  while (*s) {
    if (*s == '=') {
      if (eq || s[1] == '=') {
	if (t-buf+1 >= MAXLEN) break;
	*t++ = *s++;
      } else {
	if (t-buf+12 >= MAXLEN) break;
	strncpy(t, "{\\longequal}", 12);
	t += 12; s++;
      }
      eq = 1;
      continue;
    } else
      eq = 0;
    if (strncmp(s, "\\left\\{", 7) == 0) {
      if (t-buf+7 >= MAXLEN) break;
      strncpy(t, "\\left\\[", 7);
      t += 7; s += 7;
    } else if (strncmp(s, "\\right\\}", 8) == 0) {
      if (t-buf+8 >= MAXLEN) break;
      strncpy(t, "\\right\\]", 8);
      t += 8; s += 8;
    } else {
      if (t-buf+1 >= MAXLEN) break;
      *t++ = *s++;
    }
  }
  *t++ = 0;
  return buf;
}
