
/* GMP and MPFR support for fastcgi printf. These need to be compiled
   separately. */

#include <stdio.h>
#include <gmp.h>
#include <mpfr.h>
#include <pure/runtime.h>

#define NO_FCGI_DEFINES
#include <fcgi_stdio.h>

/* Note: When running inside Apache, fastcgi doesn't seem to set up a proper
   FILE* for its standard streams, so these functions will fail. They should
   work ok for other streams opened via FCGI_fopen, though. */

extern int fastcgi_fprintf_mpz(FCGI_FILE *_fp, const char *format, mpz_t x)
{
  FILE *fp = FCGI_ToFILE(_fp);
  if (!fp) return -1;
  return gmp_fprintf(fp, format, x);
}

extern int fastcgi_fprintf_mpfr(FCGI_FILE *_fp, const char *format, mpfr_ptr x)
{
  FILE *fp = FCGI_ToFILE(_fp);
  if (!fp) return -1;
  return mpfr_fprintf(fp, format, x);
}
