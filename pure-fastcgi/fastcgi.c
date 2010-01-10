
#include <pure/runtime.h>
#include <fastcgi/fcgi_stdio.h>

extern void fastcgi_defs(void)
{
  pure_let(pure_sym("fastcgi::stdin"), pure_pointer(stdin));
  pure_let(pure_sym("fastcgi::stdout"), pure_pointer(stdout));
  pure_let(pure_sym("fastcgi::stderr"), pure_pointer(stderr));
}

extern int fastcgi_fprintf(FILE *fp, const char *format)
{
  /* Note that 'format' *must* be passed as the format string here in order to
     expand embedded '%%' literals. Some recent gcc versions will complain
     about this ("format not a string literal and no format arguments"), these
     warnings can be ignored. */
  return fprintf(fp, format);
}

extern int fastcgi_fprintf_int(FILE *fp, const char *format, int32_t x)
{
  return fprintf(fp, format, x);
}

extern int fastcgi_fprintf_double(FILE *fp, const char *format, double x)
{
  return fprintf(fp, format, x);
}

extern int fastcgi_fprintf_string(FILE *fp, const char *format, const char *x)
{
  return fprintf(fp, format, x);
}

extern int fastcgi_fprintf_pointer(FILE *fp, const char *format, const void *x)
{
  return fprintf(fp, format, x);
}
