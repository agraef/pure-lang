
/* Copyright (c) 2008-2010 by Albert Graef <Dr.Graef@t-online.de>.

   This file is part of the Pure runtime.

   The Pure runtime is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at your
   option) any later version.

   Pure is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
   more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef UTIL_HH
#define UTIL_HH

#include <stdint.h>
#include <string.h>
#include <string>

/* Exception handling utility class. */

class err {
public:
  err(const std::string& what) : m_what(what) { };
  const std::string& what() const { return m_what; };
private:
  std::string m_what;
};

/* Quick and dirty checks for IEEE floating point infinities and NaNs. Thanks
   to John Cowan. */

inline int is_nan(double f) {
  return !(f == f);
}

inline int is_inf(double f) {
  return (!is_nan(f) && is_nan(f-f));
}

/* POSIX CRC32 checksum algorithm. */

uint32_t cksum(size_t n, const unsigned char *buf);

/* Determine the system's default encoding. */

char *default_encoding();

/* Thread-safe, locale-independent routines to convert between double
   precision floating point numbers and strings. */

double my_strtod(const char  *nptr, char **endptr);
char *my_formatd(char *buffer, const char  *format, double d);

/* Windows doesn't have strptime, so we provide a suitable replacement from
   GNU libc (see strptime.c). */

#ifndef HAVE_STRPTIME
extern "C"
char *strptime(const char *s, const char *format, struct tm *tm);
#endif

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

/* Conversion between Pure string syntax and plain string values. Also does
   automatic conversion between system encoding and utf-8. The resulting
   string is malloc'ed and must be free'd by the caller. */

char *parsestr(const char *s, char*& err);
char *printstr(const char *s);

/* Split an identifier in Pure namespace::name syntax. Returns the position of
   the end of the namespace prefix (excluding the '::'), or string::npos for
   unqualified identifiers. */

size_t symsplit(const std::string& s);

#endif // ! UTIL_HH
