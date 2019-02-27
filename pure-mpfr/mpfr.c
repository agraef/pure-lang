
/* Copyright (c) 2011 by Albert Graef <Dr.Graef@t-online.de>.

   This file is part of the Pure standard library.

   The Pure standard library is free software: you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation, either version 3 of the License,
   or (at your option) any later version.

   Pure is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
   more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <gmp.h>
#include <mpfr.h>
#include <pure/runtime.h>

/* Number of decimal digits in the print representation. The default, 0, means
   maximum precision, to be used if you always want to be able to reconstruct
   the original mpfr value from the string representation. */

static int default_num_digits = 0;

int mpfr_get_print_prec(void)
{
  return default_num_digits;
}

void mpfr_set_print_prec(int p)
{
  if (p < 0) p = 0;
  default_num_digits = p;
}

static int num_digits(int b, mpfr_prec_t p)
{
  /* calculate the number of digits required at maximum to represent a number
     with the given precision */
  if (default_num_digits)
    return default_num_digits;
  else
    return 1 + (int)ceil(p*log(2)/log(b));
}

/* Pretty-printing support. */

static const char *mpfr_str(mpfr_ptr p)
{
  if (mpfr_nan_p(p))
    return "nan";
  else if (mpfr_inf_p(p))
    return (mpfr_sgn(p) < 0)?"-inf":"inf";
  else {
    /* Estimated size of the string representation. Give a few extra bytes for
       sign, decimal point, exponent and null terminator. */
    size_t n = num_digits(10, mpfr_get_prec(p)), m = n+30;
    static char *buf = NULL;
    char format[30];
    if (buf) free(buf);
    buf = malloc(m+2);
    sprintf(format, "%%0.%luRg", (unsigned long)n);
    if (buf && mpfr_snprintf(buf, m, format, p) >= 0) {
      /* Quick and dirty hack for locales which use ',' for the decimal
	 point. XXXFIXME: This only does the right thing for a few common
	 languages, we should really adjust to the actual numeric settings of
	 the system locale here. */
      char *t = strchr(buf, ',');
      while (t) { *t = '.'; t = strchr(t+1, ','); }
      if (strchr("0123456789", buf[buf[0]=='-'?1:0]) &&
	  !strchr(buf, '.') && !strchr(buf, 'e') && !strchr(buf, 'E'))
	strcat(buf, ".0");
      return buf;
    } else {
      /* We don't have enough memory or mpfr_snprintf failed for some reason.
	 Make some fallback representation which can be produced in static
	 memory. */
      static char buf[100];
      sprintf(buf, "#<mpfr %p>", p);
      return buf;
    }
  }
}

static int mpfr_prec(mpfr_ptr x)
{
  /* Calculate the (normalized) precedence of an mpfr value, so that the
     pretty-printer can print it in a syntactically correct way. This is the
     precedence of unary minus if the number is negative (including -inf), and
     NPREC_MAX otherwise (atomic value). */
  if (mpfr_sgn(x) < 0)
    /* Unfortunately, we can't query the precedence of unary minus directly,
       but it always has the same precedence level as binary minus and prefix
       fixity. */
    return (pure_sym_nprec(pure_sym("-"))/10)*10+OP_PREFIX;
  else
    return NPREC_MAX;
}

/* Syntactic equality (this requires Pure 0.49). */

static bool mpfr_same(mpfr_ptr x, mpfr_ptr y)
{
  if (x == y)
    return true;
  else
    return mpfr_equal_p(x, y);
}

/* Hashing (this requires Pure 0.50). */

static uint32_t mpfr_hash(mpfr_ptr x)
{
  uint32_t h = 0;
  int i, len = (int)ceil((double)x->_mpfr_prec/(double)GMP_NUMB_BITS);
  if (sizeof(mp_limb_t) == 8) {
    for (i=0; i<len; i++) {
      h ^= (uint32_t)(uint64_t)x->_mpfr_d[i];
      h ^= (uint32_t)(((uint64_t)x->_mpfr_d[i])>>32);
    }
  } else {
    for (i=0; i<len; i++)
      h ^= x->_mpfr_d[i];
  }
  if (sizeof(mp_exp_t) == 8) {
    h ^= (uint32_t)(uint64_t)x->_mpfr_exp;
    h ^= (uint32_t)(((uint64_t)x->_mpfr_exp)>>32);
  } else {
    h ^= x->_mpfr_exp;
  }
  if (x->_mpfr_sign < 0)
    h = -h;
  return h;
}

/* Initialize the mpfr* tag and return its value. This also sets up the
   pretty-printing and defines some manifest constants. */

#ifndef GMP_RNDN
/* compatibility with MPFR <= 2.4 */
#define MPFR_RNDN GMP_RNDN
#define MPFR_RNDZ GMP_RNDZ
#define MPFR_RNDU GMP_RNDU
#define MPFR_RNDD GMP_RNDD
#define RND_MAX GMP_RNDD
#else
#define RND_MAX MPFR_RNDA
#endif

int mpfr_tag(void)
{
  static pure_interp_key_t t_key = 0;
  int *t_ptr, t;
  if (!t_key) t_key = pure_interp_key(free);
  if ((t_ptr = pure_interp_get(t_key)))
    t = *t_ptr;
  else {
    t = pure_pointer_tag("mpfr*");
    pure_pointer_add_equal(t, (pure_equal_fun)mpfr_same);
    pure_pointer_add_hash(t, (pure_hash_fun)mpfr_hash);
    pure_pointer_add_printer(t, (pure_printer_fun)mpfr_str,
			     (pure_printer_prec_fun)mpfr_prec);
    pure_def(pure_sym("MPFR_RNDN"), pure_int(MPFR_RNDN));
    pure_def(pure_sym("MPFR_RNDZ"), pure_int(MPFR_RNDZ));
    pure_def(pure_sym("MPFR_RNDU"), pure_int(MPFR_RNDU));
    pure_def(pure_sym("MPFR_RNDD"), pure_int(MPFR_RNDD));
#ifdef GMP_RNDN
    pure_def(pure_sym("MPFR_RNDA"), pure_int(MPFR_RNDA));
#endif
    t_ptr = malloc(sizeof(int)); assert(t_ptr);
    *t_ptr = t;
    pure_interp_set(t_key, t_ptr);
  }
  return t;
}

/* Create mpfr values with automatic memory management. */

void mpfr_free(mpfr_ptr p)
{
  mpfr_clear(p);
  free(p);
}

static pure_expr *make_mpfr(mpfr_ptr p)
{
  static pure_interp_key_t key = 0;
  int32_t f, *fptr;
  if (!key) key = pure_interp_key(free);
  if (!(fptr = pure_interp_get(key))) {
    fptr = malloc(sizeof(int32_t)); assert(fptr);
    *fptr = pure_sym("::mpfr_free");
    pure_interp_set(key, fptr);
  }
  f = *fptr;
  return pure_sentry(pure_symbol(f), pure_tag(mpfr_tag(), pure_pointer(p)));
}

/* Conversions. */

pure_expr *mpfr_from_double(double x, int prec, int rnd)
{
  mpfr_ptr p = malloc(sizeof(mpfr_t));
  if (!p) return NULL;
  if (prec < MPFR_PREC_MIN) prec = MPFR_PREC_MIN;
  if (rnd < 0 || rnd > RND_MAX)
    rnd = mpfr_get_default_rounding_mode();
  mpfr_init2(p, prec);
  mpfr_set_d(p, x, rnd);
  return make_mpfr(p);
}

pure_expr *mpfr_from_mpfr(mpfr_ptr x, int prec, int rnd)
{
  mpfr_ptr p = malloc(sizeof(mpfr_t));
  if (!p) return NULL;
  if (prec < MPFR_PREC_MIN) prec = MPFR_PREC_MIN;
  if (rnd < 0 || rnd > RND_MAX)
    rnd = mpfr_get_default_rounding_mode();
  mpfr_init2(p, prec);
  mpfr_set(p, x, rnd);
  return make_mpfr(p);
}

pure_expr *mpfr_from_bigint(mpz_t x, int prec, int rnd)
{
  mpfr_ptr p = malloc(sizeof(mpfr_t));
  if (!p) return NULL;
  if (prec < MPFR_PREC_MIN) prec = MPFR_PREC_MIN;
  if (rnd < 0 || rnd > RND_MAX)
    rnd = mpfr_get_default_rounding_mode();
  mpfr_init2(p, prec);
  mpfr_set_z(p, x, rnd);
  return make_mpfr(p);
}

pure_expr *mpfr_from_str(const char *s, int prec, int rnd)
{
  mpfr_ptr p = malloc(sizeof(mpfr_t));
  int res;
  if (!p) return NULL;
  if (prec < MPFR_PREC_MIN) prec = MPFR_PREC_MIN;
  if (rnd < 0 || rnd > RND_MAX)
    rnd = mpfr_get_default_rounding_mode();
  mpfr_init2(p, prec);
  res = mpfr_set_str(p, s, 10, rnd);
  if (res) {
    mpfr_clear(p);
    free(p);
    return NULL;
  } else
    return make_mpfr(p);
}

double mpfr_to_double(mpfr_ptr x)
{
  return mpfr_get_d(x, mpfr_get_default_rounding_mode());
}

int mpfr_to_int(mpfr_ptr x)
{
  // we always round towards zero here, to be consistent with the int built-in
  return mpfr_get_si(x, MPFR_RNDZ);
}

pure_expr *mpfr_to_bigint(mpfr_ptr x)
{
  if (mpfr_number_p(x)) {
    pure_expr *y;
    mpz_t z;
    mpz_init(z);
    // we always round towards zero here, like the bigint built-in
    (void)mpfr_get_z(z, x, MPFR_RNDZ);
    y = pure_mpz(z);
    mpz_clear(z);
    return y;
  } else
    return NULL;
}

pure_expr *floor_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_floor(y, x);
  return make_mpfr(y);
}

pure_expr *ceil_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_ceil(y, x);
  return make_mpfr(y);
}

pure_expr *round_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_round(y, x);
  return make_mpfr(y);
}

pure_expr *trunc_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_trunc(y, x);
  return make_mpfr(y);
}

/* Arithmetic. */

pure_expr *neg_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_neg(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *add_mpfr(mpfr_ptr x, mpfr_ptr y)
{
  mpfr_ptr z = malloc(sizeof(mpfr_t));
  if (!z) return NULL;
  mpfr_init(z);
  mpfr_add(z, x, y, mpfr_get_default_rounding_mode());
  return make_mpfr(z);
}

pure_expr *sub_mpfr(mpfr_ptr x, mpfr_ptr y)
{
  mpfr_ptr z = malloc(sizeof(mpfr_t));
  if (!z) return NULL;
  mpfr_init(z);
  mpfr_sub(z, x, y, mpfr_get_default_rounding_mode());
  return make_mpfr(z);
}

pure_expr *mul_mpfr(mpfr_ptr x, mpfr_ptr y)
{
  mpfr_ptr z = malloc(sizeof(mpfr_t));
  if (!z) return NULL;
  mpfr_init(z);
  mpfr_mul(z, x, y, mpfr_get_default_rounding_mode());
  return make_mpfr(z);
}

pure_expr *div_mpfr(mpfr_ptr x, mpfr_ptr y)
{
  mpfr_ptr z = malloc(sizeof(mpfr_t));
  if (!z) return NULL;
  mpfr_init(z);
  mpfr_div(z, x, y, mpfr_get_default_rounding_mode());
  return make_mpfr(z);
}

pure_expr *pow_mpfr(mpfr_ptr x, mpfr_ptr y)
{
  mpfr_ptr z = malloc(sizeof(mpfr_t));
  if (!z) return NULL;
  mpfr_init(z);
  mpfr_pow(z, x, y, mpfr_get_default_rounding_mode());
  return make_mpfr(z);
}

/* Math functions. */

pure_expr *sqrt_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_sqrt(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *exp_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_exp(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *ln_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_log(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *log_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_log10(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *sin_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_sin(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *cos_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_cos(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *tan_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_tan(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *asin_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_asin(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *acos_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_acos(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *atan_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_atan(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *atan2_mpfr(mpfr_ptr y, mpfr_ptr x)
{
  mpfr_ptr z = malloc(sizeof(mpfr_t));
  if (!z) return NULL;
  mpfr_init(z);
  mpfr_atan2(z, y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(z);
}

pure_expr *sinh_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_sinh(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *cosh_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_cosh(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *tanh_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_tanh(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *asinh_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_asinh(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *acosh_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_acosh(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}

pure_expr *atanh_mpfr(mpfr_ptr x)
{
  mpfr_ptr y = malloc(sizeof(mpfr_t));
  if (!y) return NULL;
  mpfr_init(y);
  mpfr_atanh(y, x, mpfr_get_default_rounding_mode());
  return make_mpfr(y);
}
