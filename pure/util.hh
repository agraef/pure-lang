#ifndef UTIL_HH
#define UTIL_HH

#include <string.h>

/* Quick and dirty checks for IEEE floating point infinities and NaNs. Thanks
   to John Cowan. */

inline int is_nan(double f) {
  return !(f == f);
}

inline int is_inf(double f) {
  return (!is_nan(f) && is_nan(f-f));
}

/* Determine the system's default encoding. */

char *default_encoding();

/* Thread-safe, locale-independent routines to convert between double
   precision floating point numbers and strings. */

double my_strtod(const char  *nptr, char **endptr);
char *my_formatd(char *buffer, const char  *format, double d);

/* utf-8 string helpers. */

/* Convert between utf-8 and the given encoding (the system encoding by
   default). The resulting string is malloc'ed and must be free'd by the
   caller. */

char *toutf8(const char *s, const char *codeset = 0);
char *fromutf8(const char *s, char *codeset = 0);

/* Determine the length of a string (i.e., the number of utf-8 chars it
   contains), or the number of utf-8 chars up to the given position in the
   string, respectively. */

size_t u8strlen(const char *s);
size_t u8strpos(const char *s, const char *t);

/* Find the character code (code point) of the nth char of a string (takes
   linear time!). */

unsigned long u8strchar(const char *s, size_t n);

/* Find the position of the nth char. */

const char *u8strcharpos(const char *s, size_t n);

/* Convert a utf-8 character code to a single-character string. Writes the
   result to and returns the given buffer which must be large enough to hold
   a utf-8 char plus the terminating zero byte (5 bytes). */

char *u8char(char *buf, unsigned long c);

/* Convert from a single-character utf-8 string back to the character code.
   The result is -1 if the given string is not a valid utf-8 character, and
   the code point number otherwise. */

long u8charcode(const char *c);

/* Generic utf-8 string processing. Execute the given function f on the given
   pointer p and each utf-8 character of the given string s in turn. For each
   character code c, f is invoked as f(p, c). */

void u8dostr(const char *s, void (*f)(void*, unsigned long), void *p);

/* Conversion between Pure string syntax and plain string values. Also does
   automatic conversion between system encoding and utf-8. The resulting
   string is malloc'ed and must be free'd by the caller. */

char *parsestr(const char *s, char*& err);
char *printstr(const char *s);

#endif // ! UTIL_HH
