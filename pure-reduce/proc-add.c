
#include <stdlib.h>
#include "reduce-algebra/csl/cslbase/headers.h"

static size_t bufsz = 0, buflen = 0;
static char *buf = NULL;

static int capture_output(int c)
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
    return PROC_set_callbacks(NULL, capture_output);
  } else
    return PROC_set_callbacks(NULL, NULL);
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
