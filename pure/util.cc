
/* Copyright (c) 2008-2012 by Albert Graef <Dr.Graef@t-online.de>.

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

/* NOTE: This module includes a few macros and functions from third parties
   which are under the following copyrights (please see the corresponding
   delimited source sections at the end of this file for pointers to the
   original sources and licensing details pertaining to these sources):

   The glib string-double conversion routines (my_strtod, my_formatd, and the
   accompanying character macros from gstrfuncs.h and gstrfuncs.c) are:
   Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald.

   The POSIX CRC32 checksum algorithm (cksum function from cksum.c) is:
   Copyright (C) 2004, 2005, 2006 Board of Trustees, Leland Stanford
   Jr. University.  All rights reserved. */

#include "util.hh"
#include <cstdlib>
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <locale.h>
#include <iconv.h>
#include <ctype.h>
#include <wctype.h>

#include "config.h"

#ifndef _WIN32
#ifdef HAVE_LANGINFO_CODESET
#include <langinfo.h>
#endif
#else
#include <windows.h>
#endif

// Windows doesn't have this. Fake it.

#ifndef HAVE_MKSTEMP

#include <fcntl.h>
#define _S_IREAD 256
#define _S_IWRITE 128

extern "C"
int mkstemp(char *tmpl)
{
  int ret = -1;
  mktemp(tmpl);
  ret = open(tmpl, O_RDWR|O_BINARY|O_CREAT|O_EXCL|_O_SHORT_LIVED,
	     _S_IREAD|_S_IWRITE);
  return ret;
}

#endif

/* some utf-8 helpers pilfered from the Q source ***************************/

#if !defined(_WIN32) && !defined(HAVE_LANGINFO_CODESET)

/* simplistic emulation of nl_langinfo(CODESET) on POSIX systems which don't
   have it (like older FreeBSD releases), pilfered from
   http://www.cl.cam.ac.uk/~mgk25/ucs */

/*
 * This is a quick-and-dirty emulator of the nl_langinfo(CODESET)
 * function defined in the Single Unix Specification for those systems
 * (FreeBSD, etc.) that don't have one yet. It behaves as if it had
 * been called after setlocale(LC_CTYPE, ""), that is it looks at
 * the locale environment variables.
 *
 * http://www.opengroup.org/onlinepubs/7908799/xsh/langinfo.h.html
 *
 * Please extend it as needed and suggest improvements to the author.
 * This emulator will hopefully become redundant soon as
 * nl_langinfo(CODESET) becomes more widely implemented.
 *
 * Since the proposed Li18nux encoding name registry is still not mature,
 * the output follows the MIME registry where possible:
 *
 *   http://www.iana.org/assignments/character-sets
 *
 * A possible autoconf test for the availability of nl_langinfo(CODESET)
 * can be found in
 *
 *   http://www.cl.cam.ac.uk/~mgk25/unicode.html#activate
 *
 * Markus.Kuhn@cl.cam.ac.uk -- 2002-03-11
 * Permission to use, copy, modify, and distribute this software
 * for any purpose and without fee is hereby granted. The author
 * disclaims all warranties with regard to this software.
 *
 * Latest version:
 *
 *   http://www.cl.cam.ac.uk/~mgk25/ucs/langinfo.c
 */

typedef int nl_item;

#define MYCODESET ((nl_item) 1)

#define C_CODESET "US-ASCII"     /* Return this as the encoding of the
				  * C/POSIX locale. Could as well one day
				  * become "UTF-8". */

#define digit(x) ((x) >= '0' && (x) <= '9')

static char *my_nl_langinfo(nl_item item)
{
  static char buf[16];
  char *l, *p;
  
  if (item != MYCODESET)
    return NULL;
  
  if (((l = getenv("LC_ALL"))   && *l) ||
      ((l = getenv("LC_CTYPE")) && *l) ||
      ((l = getenv("LANG"))     && *l)) {
    /* check standardized locales */
    if (!strcmp(l, "C") || !strcmp(l, "POSIX"))
      return C_CODESET;
    /* check for encoding name fragment */
    if (strstr(l, "UTF") || strstr(l, "utf"))
      return "UTF-8";
    if ((p = strstr(l, "8859-"))) {
      memcpy(buf, "ISO-8859-\0\0", 12);
      p += 5;
      if (digit(*p)) {
	buf[9] = *p++;
	if (digit(*p)) buf[10] = *p++;
	return buf;
      }
    }
    if (strstr(l, "KOI8-R")) return "KOI8-R";
    if (strstr(l, "KOI8-U")) return "KOI8-U";
    if (strstr(l, "620")) return "TIS-620";
    if (strstr(l, "2312")) return "GB2312";
    if (strstr(l, "HKSCS")) return "Big5HKSCS";   /* no MIME charset */
    if (strstr(l, "Big5") || strstr(l, "BIG5")) return "Big5";
    if (strstr(l, "GBK")) return "GBK";           /* no MIME charset */
    if (strstr(l, "18030")) return "GB18030";     /* no MIME charset */
    if (strstr(l, "Shift_JIS") || strstr(l, "SJIS")) return "Shift_JIS";
    /* check for conclusive modifier */
    if (strstr(l, "euro")) return "ISO-8859-15";
    /* check for language (and perhaps country) codes */
    if (strstr(l, "zh_TW")) return "Big5";
    if (strstr(l, "zh_HK")) return "Big5HKSCS";   /* no MIME charset */
    if (strstr(l, "zh")) return "GB2312";
    if (strstr(l, "ja")) return "EUC-JP";
    if (strstr(l, "ko")) return "EUC-KR";
    if (strstr(l, "ru")) return "KOI8-R";
    if (strstr(l, "uk")) return "KOI8-U";
    if (strstr(l, "pl") || strstr(l, "hr") ||
	strstr(l, "hu") || strstr(l, "cs") ||
	strstr(l, "sk") || strstr(l, "sl")) return "ISO-8859-2";
    if (strstr(l, "eo") || strstr(l, "mt")) return "ISO-8859-3";
    if (strstr(l, "el")) return "ISO-8859-7";
    if (strstr(l, "he")) return "ISO-8859-8";
    if (strstr(l, "tr")) return "ISO-8859-9";
    if (strstr(l, "th")) return "TIS-620";      /* or ISO-8859-11 */
    if (strstr(l, "lt")) return "ISO-8859-13";
    if (strstr(l, "cy")) return "ISO-8859-14";
    if (strstr(l, "ro")) return "ISO-8859-2";   /* or ISO-8859-16 */
    if (strstr(l, "am") || strstr(l, "vi")) return "UTF-8";
    /* Send me further rules if you like, but don't forget that we are
     * *only* interested in locale naming conventions on platforms
     * that do not already provide an nl_langinfo(CODESET) implementation. */
    return "ISO-8859-1"; /* should perhaps be "UTF-8" instead */
  }
  return C_CODESET;
}

#endif

#ifdef MY_STRDUP

/* Optimize strdup/free. Due to the way that Pure creates dynamic and
   temporary string values on the fly, calling strdup() on each and every such
   operation is inefficient and may lead to memory fragmentation. Therefore we
   keep a fixed-size cache here so that the last N allocations may be reused
   without allocating additional memory if possible.

   NOTE: This is experimental. The current code is a bit simplistic but seems
   to work reasonably well. We assign the cache slots in a round-robin
   fashion, and reuse previously freed strings using a first-fit policy. Newly
   allocated strings kick out old cache entries if they can't be stored in
   free cache entries, so that long-living strings are eventually removed from
   the cache in an automatic fashion. */

#define NCACHE 10
static char *lastalloc[NCACHE], *lastfree[NCACHE];
static size_t lastlen[NCACHE];
static int lasti = 0;

//#define DEBUG_CACHE

char *my_strdup(const char *s)
{
  if (!s) return 0;
  size_t l = strlen(s);
  for (int i = 0; i < NCACHE; i++)
    if (lastfree[i] && lastlen[i] >= l) {
      char *t = strcpy(lastfree[i], s);
#ifdef DEBUG_CACHE
      printf("my_strdup: reusing slot #%d len=%lu '%s' (%p)\n", i,
	     lastlen[i], t, t);
#endif
      lastfree[i] = 0; lastlen[i] = strlen(t); lastalloc[i] = t;
      return t;
    }
  char *t = strdup(s);
  if (!t) return 0;
#ifdef DEBUG_CACHE
  printf("** my_strdup: initializing slot #%d '%s' (%p)\n", lasti, t, t);
#endif
  // fill the cache in a round-robin fashion
  lastlen[lasti] = strlen(t);
  if (lastfree[lasti]) {
    free(lastfree[lasti]);
    lastfree[lasti] = 0;
  }
  lastalloc[lasti++] = t;
  if (lasti >= NCACHE) lasti = 0;
  return t;
}

void my_strfree(char *s)
{
  if (!s) return;
  for (int i = 0; i < NCACHE; i++)
    if (s == lastalloc[i]) {
#ifdef DEBUG_CACHE
      printf("my_strfree: freeing slot #%d (%p)\n", i, s);
#endif
      if (lastfree[i]) free(lastfree[i]);
      lastfree[i] = s; lastalloc[i] = 0;
      return;
    }
  // not in cache, free right away
#ifdef DEBUG_CACHE
  printf("** my_strfree: freeing '%s' %p\n", s, s);
#endif
  free(s);
}

#endif

/* decoding and encoding of UTF-8 chars */

static inline long
u8decodec(const char *s)
{
  size_t n = 0;
  unsigned p = 0, q = 0;
  unsigned long c = 0;
  if (s[0] == 0)
    return -1;
  else if (s[1] == 0)
    return (unsigned char)s[0];
  for (; n == 0 && *s; s++) {
    unsigned char uc = (unsigned char)*s;
    if (q == 0) {
      if (((signed char)uc) < 0) {
	switch (uc & 0xf0) {
	case 0xc0: case 0xd0:
	  q = 1;
	  c = uc & 0x1f;
	  break;
	case 0xe0:
	  q = 2;
	  c = uc & 0xf;
	  break;
	case 0xf0:
	  if ((uc & 0x8) == 0) {
	    q = 3;
	    c = uc & 0x7;
	  } else
	    c = uc;
	  break;
	default:
	  c = uc;
	  break;
	}
      } else
	c = uc;
      p = 0; if (q == 0) n++;
    } else if ((uc & 0xc0) == 0x80) {
      /* continuation byte */
      c = c << 6 | (uc & 0x3f);
      if (--q == 0)
	n++;
      else
	p++;
    } else {
      /* malformed char */
      return -1;
    }
  }
  if (n == 1 && *s == 0)
    return c;
  else
    return -1;
}

static inline long
u8decode(const char *s, const char **t)
{
  size_t n = 0;
  unsigned p = 0, q = 0;
  unsigned long c = 0;
  if (s[0] == 0)
    return -1;
  else if (s[1] == 0) {
    *t = s+1;
    return (unsigned char)s[0];
  }
  for (; n == 0 && *s; s++) {
    unsigned char uc = (unsigned char)*s;
    if (q == 0) {
      if (((signed char)uc) < 0) {
	switch (uc & 0xf0) {
	case 0xc0: case 0xd0:
	  q = 1;
	  c = uc & 0x1f;
	  break;
	case 0xe0:
	  q = 2;
	  c = uc & 0xf;
	  break;
	case 0xf0:
	  if ((uc & 0x8) == 0) {
	    q = 3;
	    c = uc & 0x7;
	  } else
	    c = uc;
	  break;
	default:
	  c = uc;
	  break;
	}
      } else
	c = uc;
      p = 0; if (q == 0) n++;
    } else if ((uc & 0xc0) == 0x80) {
      /* continuation byte */
      c = c << 6 | (uc & 0x3f);
      if (--q == 0)
	n++;
      else
	p++;
    } else {
      /* malformed char */
      return -1;
    }
  }
  if (n == 1) {
    *t = s;
    return c;
  } else
    return -1;
}

static inline char *
u8encode(char *t, unsigned long c)
{
  unsigned char *uc = (unsigned char*)t;
  if (c < 0x80) {
    uc[1] = 0;
    uc[0] = c;
  } else if (c < 0x800) {
    uc[2] = 0;
    uc[1] = 0x80 | (c&0x3f);
    c = c >> 6;
    uc[0] = 0xc0 | c;
  } else if (c < 0x10000) {
    uc[3] = 0;
    uc[2] = 0x80 | (c&0x3f);
    c = c >> 6;
    uc[1] = 0x80 | (c&0x3f);
    c = c >> 6;
    uc[0] = 0xe0 | c;
  } else {
    uc[4] = 0;
    uc[3] = 0x80 | (c&0x3f);
    c = c >> 6;
    uc[2] = 0x80 | (c&0x3f);
    c = c >> 6;
    uc[1] = 0x80 | (c&0x3f);
    c = c >> 6;
    uc[0] = 0xf0 | c;
  }
  return t;
}

/* UTF-8 string length and position of a substring */

size_t u8strlen(const char *s)
{
  size_t n = 0;
  unsigned p = 0, q = 0;
 start:
  for (; *s; s++) {
    unsigned char uc = (unsigned char)*s;
    if (q == 0) {
      if (((signed char)uc) < 0) {
	switch (uc & 0xf0) {
	case 0xc0: case 0xd0:
	  q = 1;
	  break;
	case 0xe0:
	  q = 2;
	  break;
	case 0xf0:
	  if ((uc & 0x8) == 0)
	    q = 3;
	  break;
	}
      }
      p = 0; n++;
    } else if ((uc & 0xc0) == 0x80) {
      /* continuation byte */
      if (--q == 0)
	p = 0;
      else
	p++;
    } else {
      /* malformed char */
      s -= p+1; p = q = 0;
    }
  }
  if (q > 0) {
    /* unterminated char */
    s -= p; p = q = 0;
    goto start;
  }
  return n;
}

size_t u8strpos(const char *s, const char *t)
{
  size_t n = 0;
  unsigned p = 0, q = 0;
 start:
  for (; s < t && *s; s++) {
    unsigned char uc = (unsigned char)*s;
    if (q == 0) {
      if (((signed char)uc) < 0) {
	switch (uc & 0xf0) {
	case 0xc0: case 0xd0:
	  q = 1;
	  break;
	case 0xe0:
	  q = 2;
	  break;
	case 0xf0:
	  if ((uc & 0x8) == 0)
	    q = 3;
	  break;
	}
      }
      p = 0; n++;
    } else if ((uc & 0xc0) == 0x80) {
      /* continuation byte */
      if (--q == 0)
	p = 0;
      else
	p++;
    } else {
      /* malformed char */
      s -= p+1; p = q = 0;
    }
  }
  if (q > 0) {
    /* unterminated char */
    s -= p; p = q = 0;
    goto start;
  }
  return n;
}

/* find a UTF-8 char at a given position */

static inline const char *
u8strind(const char *s, size_t i)
{
  unsigned p = 0, q = 0;
 start:
  for (; *s && i > 0; s++) {
    unsigned char uc = (unsigned char)*s;
    if (q == 0) {
      if (((signed char)uc) < 0) {
	switch (uc & 0xf0) {
	case 0xc0: case 0xd0:
	  q = 1;
	  break;
	case 0xe0:
	  q = 2;
	  break;
	case 0xf0:
	  if ((uc & 0x8) == 0)
	    q = 3;
	  break;
	}
      }
      p = 0; if (q == 0) i--;
    } else if ((uc & 0xc0) == 0x80) {
      /* continuation byte */
      if (--q == 0) {
	p = 0; i--;
      } else
	p++;
    } else {
      /* malformed char */
      i--; s -= p+1; p = q = 0;
    }
  }
  if (q > 0) {
    /* unterminated char */
    i--; s -= p; p = q = 0;
    goto start;
  }
  return s;
}

/* conversion between UTF-8 and the system encoding */

char *default_encoding()
{
#ifdef HAVE_LANGINFO_CODESET
  /* use nl_langinfo() if it's available */
  return nl_langinfo(CODESET);
#else
#ifdef _WIN32
  /* Always use the OEM codepage on Windows. */
  unsigned cp = GetOEMCP();
  static char buf[20];
  if (cp == 65001)
    // codepage 65001 is UTF-8
    strcpy(buf, "UTF-8");
  else if (cp == 65000)
    // codepage 65000 is UTF-7
    strcpy(buf, "UTF-7");
  else
    sprintf(buf, "cp%d", cp);
  return buf;
#else
  /* use our own emulation of nl_langinfo() */
  return my_nl_langinfo(CODESET);
#endif /* _WIN32 */
#endif /* HAVE_LANGINFO_CODESET */
}

#define myiconv(ic, inbuf, inbytes, outbuf, outbytes) \
  iconv(ic, (ICONV_CONST char**)inbuf, inbytes, outbuf, outbytes)

#define CHUNKSZ 128

char *
toutf8(const char *s, const char *codeset)
{
  iconv_t ic;
  if (!codeset || !*codeset)
    codeset = default_encoding();
  if (codeset && strcmp(codeset, "UTF-8"))
    ic = iconv_open("UTF-8", codeset);
  else
    ic = (iconv_t)-1;

  if (ic == (iconv_t)-1)
    return strdup(s);

  else {
    size_t l = strlen(s);
    char *t = (char*)malloc(l+1), *t1;
    char *inbuf = (char*)s, *outbuf = t; // const char* -> char*. Ugh.
    size_t inbytes = l, outbytes = l;

    while (myiconv(ic, &inbuf, &inbytes, &outbuf, &outbytes) ==
	   (size_t)-1)
      if (errno == E2BIG) {
	/* try to enlarge the output buffer */
	size_t k = outbuf-t;
	if ((t1 = (char*)realloc(t, l+CHUNKSZ+1))) {
	  t = t1;
	  outbuf = t+k;
	  l += CHUNKSZ;
	  outbytes += CHUNKSZ;
	} else {
	  /* memory overflow */
	  free(t);
	  return strdup(s);
	}
      } else {
	/* conversion error */
	free(t);
	return strdup(s);
      }

    /* terminate the output string */
    *outbuf = 0;
    iconv_close(ic);

    if (!(t1 = (char*)realloc(t, strlen(t)+1)))
      /* this shouldn't happen */
      return t;
    else
      return t1;
  }
}

char *
fromutf8(const char *s, char *codeset)
{
  iconv_t ic;
  if (!codeset || !*codeset)
    codeset = default_encoding();
  if (codeset && strcmp(codeset, "UTF-8"))
    ic = iconv_open(codeset, "UTF-8");
  else
    ic = (iconv_t)-1;

  if (ic == (iconv_t)-1)
    return strdup(s);

  else {
    size_t l = strlen(s);
    char *t = (char*)malloc(l+1), *t1;
    char *inbuf = (char*)s, *outbuf = t; // const char* -> char*. Ugh.
    size_t inbytes = l, outbytes = l;

    while (myiconv(ic, &inbuf, &inbytes, &outbuf, &outbytes) ==
	   (size_t)-1)
      if (errno == E2BIG) {
	/* try to enlarge the output buffer */
	size_t k = outbuf-t;
	if ((t1 = (char*)realloc(t, l+CHUNKSZ+1))) {
	  t = t1;
	  outbuf = t+k;
	  l += CHUNKSZ;
	  outbytes += CHUNKSZ;
	} else {
	  /* memory overflow */
	  free(t);
	  return strdup(s);
	}
      } else {
	/* conversion error */
	free(t);
	return strdup(s);
      }

    /* here we might have to deal with a stateful encoding, so make sure that
       we emit the closing shift sequence */

    while (myiconv(ic, NULL, NULL, &outbuf, &outbytes) ==
	   (size_t)-1)
      if (errno == E2BIG) {
	/* try to enlarge the output buffer */
	size_t k = outbuf-t;
	if ((t1 = (char*)realloc(t, l+CHUNKSZ+1))) {
	  t = t1;
	  outbuf = t+k;
	  l += CHUNKSZ;
	  outbytes += CHUNKSZ;
	} else {
	  /* memory overflow */
	  free(t);
	  return strdup(s);
	}
      } else {
	/* conversion error */
	free(t);
	return strdup(s);
      }

    /* terminate the output string */
    *outbuf = 0;
    iconv_close(ic);

    if (!(t1 = (char*)realloc(t, strlen(t)+1)))
      /* this shouldn't happen */
      return t;
    else
      return t1;
  }
}

char *
my_toutf8(const char *s, const char *codeset)
{
  iconv_t ic;
  if (!codeset || !*codeset)
    codeset = default_encoding();
  if (codeset && strcmp(codeset, "UTF-8"))
    ic = iconv_open("UTF-8", codeset);
  else
    ic = (iconv_t)-1;

  if (ic == (iconv_t)-1)
    return my_strdup(s);

  else {
    size_t l = strlen(s);
    char *t = (char*)malloc(l+1), *t1;
    char *inbuf = (char*)s, *outbuf = t; // const char* -> char*. Ugh.
    size_t inbytes = l, outbytes = l;

    while (myiconv(ic, &inbuf, &inbytes, &outbuf, &outbytes) ==
	   (size_t)-1)
      if (errno == E2BIG) {
	/* try to enlarge the output buffer */
	size_t k = outbuf-t;
	if ((t1 = (char*)realloc(t, l+CHUNKSZ+1))) {
	  t = t1;
	  outbuf = t+k;
	  l += CHUNKSZ;
	  outbytes += CHUNKSZ;
	} else {
	  /* memory overflow */
	  free(t);
	  return my_strdup(s);
	}
      } else {
	/* conversion error */
	free(t);
	return my_strdup(s);
      }

    /* terminate the output string */
    *outbuf = 0;
    iconv_close(ic);

    if (!(t1 = my_strdup(t)))
      /* this shouldn't happen */
      return t;
    else {
      free(t);
      return t1;
    }
  }
}

char *
my_fromutf8(const char *s, char *codeset)
{
  iconv_t ic;
  if (!codeset || !*codeset)
    codeset = default_encoding();
  if (codeset && strcmp(codeset, "UTF-8"))
    ic = iconv_open(codeset, "UTF-8");
  else
    ic = (iconv_t)-1;

  if (ic == (iconv_t)-1)
    return my_strdup(s);

  else {
    size_t l = strlen(s);
    char *t = (char*)malloc(l+1), *t1;
    char *inbuf = (char*)s, *outbuf = t; // const char* -> char*. Ugh.
    size_t inbytes = l, outbytes = l;

    while (myiconv(ic, &inbuf, &inbytes, &outbuf, &outbytes) ==
	   (size_t)-1)
      if (errno == E2BIG) {
	/* try to enlarge the output buffer */
	size_t k = outbuf-t;
	if ((t1 = (char*)realloc(t, l+CHUNKSZ+1))) {
	  t = t1;
	  outbuf = t+k;
	  l += CHUNKSZ;
	  outbytes += CHUNKSZ;
	} else {
	  /* memory overflow */
	  free(t);
	  return my_strdup(s);
	}
      } else {
	/* conversion error */
	free(t);
	return my_strdup(s);
      }

    /* here we might have to deal with a stateful encoding, so make sure that
       we emit the closing shift sequence */

    while (myiconv(ic, NULL, NULL, &outbuf, &outbytes) ==
	   (size_t)-1)
      if (errno == E2BIG) {
	/* try to enlarge the output buffer */
	size_t k = outbuf-t;
	if ((t1 = (char*)realloc(t, l+CHUNKSZ+1))) {
	  t = t1;
	  outbuf = t+k;
	  l += CHUNKSZ;
	  outbytes += CHUNKSZ;
	} else {
	  /* memory overflow */
	  free(t);
	  return my_strdup(s);
	}
      } else {
	/* conversion error */
	free(t);
	return my_strdup(s);
      }

    /* terminate the output string */
    *outbuf = 0;
    iconv_close(ic);

    if (!(t1 = my_strdup(t)))
      /* this shouldn't happen */
      return t;
    else {
      free(t);
      return t1;
    }
  }
}

/* conversion between UTF-8 and wchar_t */

#ifdef __STDC_ISO_10646__
#define towchar(c) ((wchar_t)(c))
#else
static inline wchar_t *
ictowcs(wchar_t *t, char *s)
{
  static iconv_t myic[2] = { (iconv_t)-1, (iconv_t)-1 };
  if (myic[1] == (iconv_t)-1)
    myic[1] = iconv_open("WCHAR_T", "UTF-8");
  if (myic[1] == (iconv_t)-1)
    return NULL;
  else {
    size_t l = strlen(s);
    char *inbuf = s; wchar_t *outbuf = t;
    size_t inbytes = l, outbytes = l*sizeof(wchar_t);

    if (myiconv(myic[1], &inbuf, &inbytes, (char**)&outbuf, &outbytes) ==
	(size_t)-1)
      return NULL;
    /* terminate the output string */
    *outbuf = 0;
    return t;
  }
}

static wchar_t towchar(unsigned long c)
{
  char s[5]; /* single utf-8 char can have at most 4 bytes, plus terminal 0 */
  wchar_t t[5]; /* just to be safe; 2 should actually be enough */
  u8encode(s, c);
  if (ictowcs(t, s))
    return t[0];
  else
    /* Your system is so utterly broken that we can't even convert UTF-8 to
       wchar_t. But let's just pretend we have an ISO 10646 compatible
       encoding anyway. */
    return (wchar_t)c;
}
#endif

unsigned long u8strchar(const char *s, size_t n)
{
  s = u8strind(s, n);
  assert(s);
  if (!*s) return 0;
  const char *t; long c = u8decode(s, &t);
  if (c < 0) c = *s;
  return c;
}

const char *u8strcharpos(const char *s, size_t n)
{
  s = u8strind(s, n);
  assert(s);
  return s;
}

char *u8char(char *buf, unsigned long c)
{
  return u8encode(buf, c);
}

long u8charcode(const char *c)
{
  return u8decodec(c);
}

static char *
pchar(char *s, long c, char d)
{
  switch (c) {
  case '\t':
    return (strcpy(s, "\\t"));
  case '\b':
    return (strcpy(s, "\\b"));
  case '\f':
    return (strcpy(s, "\\f"));
  case '\n':
    return (strcpy(s, "\\n"));
  case '\r':
    return (strcpy(s, "\\r"));
  case '\\':
    return (strcpy(s, "\\\\"));
  case '\"':
    return (strcpy(s, "\\\""));
  default:
    if (iswprint(towchar(c))) {
      u8encode(s, c);
      return (s);
    } else {
      if (d != 0 && isxdigit(d))
	sprintf(s, "\\(0x%lx)", c);
      else
	sprintf(s, "\\0x%lx", c);
      return (s);
    }
  }
}

static char *
pstr(char *s1, const char *s2)
{
  char *s;
  const char *t = 0;

  *s1 = '\0';
  for (s = s1; *s2; s2 = t) {
    long c = u8decode(s2, &t);
    if (c < 0) {
      c = (unsigned char)*s2;
      t = s2+1;
    }
    s += strlen(pchar(s, c, *t));
  }
  return (s1);
}

#include "w3centities.c"

static int entcmp(const void *_x, const void *_y)
{
  const char *x = (const char*)_x;
  const Entity *y = (const Entity*)_y;
  return strcmp(x, y->name);
}

static inline long entity(const char *name)
{
  Entity *x = (Entity*)
    bsearch(name, entities, sizeof(entities) / sizeof(Entity),
	    sizeof(Entity), entcmp);
  return x?x->c:-1;
}

static long parse_entity(char *s, char **t)
{
  long c;
  char *p = strchr(s, ';');
  if (!p) return -1;
  *p = 0;
  c = entity(s);
  *p = ';';
  if (c < 0) return c;
  *t = p+1;
  return c;
}

static long int bstrtol(const char *nptr, char **endptr)
{
  if (nptr[0] == '0' && (nptr[1] == 'b' || nptr[1] == 'B'))
    return strtol(nptr+2, endptr, 2);
  else
    return strtol(nptr, endptr, 0);
}

static char *
scanchar(char *t, char **s, char **p)
{
  char            c;

  /* scan character at the head of *s, advance s accordingly: */

  *p = NULL;
  if (!**s)
    return NULL;
  else if ((c = *(*s)++) == '\\') {
    switch (c = *(*s)++) {
    case '\n':
      break;
    case 't':
      *t++ = '\t';
      break;
    case 'b':
      *t++ = '\b';
      break;
    case 'f':
      *t++ = '\f';
      break;
    case 'n':
      *t++ = '\n';
      break;
    case 'r':
      *t++ = '\r';
      break;
    case '"': case '\\':
      *t++ = c;
      break;
    case '&': {
      long c = parse_entity(*s, s);
      if (c >= 0) {
	if (c >= 0x110000)
	  c %=  0x110000;
	u8encode(t, c);
	t += strlen(t);
      } else {
	*p = *s-1;
	*t++ = c;
      }
      break;
    }
    case '(': {
      if ('0' <= **s && **s <= '9') {
	char *r;
	long c = bstrtol(*s, &r);
	if (*r == ')') {
	  *s = r+1;
	  if (c >= 0x110000)
	    c %=  0x110000;
	  u8encode(t, c);
	  t += strlen(t);
	} else {
	  *p = *s-1;
	  *t++ = '(';
	}
      } else {
	*p = *s-1;
	*t++ = c;
      }
      break;
    }
    default:
      if ('0' <= c && c <= '9') {
	long c = bstrtol(--*s, s);
	if (c >= 0x110000)
	  c %=  0x110000;
	u8encode(t, c);
	t += strlen(t);
      } else {
	*p = *s-1;
	*t++ = c;
      }
      break;
    }
  } else
    *t++ = c;
  return t;
}

static char *
scanstr(char *s1, char *s2)
{
  char           *s = s1;

  while (*s2) {
    char *p;
    s = scanchar(s, &s2, &p);
    if (p) {
      p[-1] = 0;
      s1 = NULL;
    }
  }
  *s = 0;
  return s1;
}

char *parsestr(const char *s, char*& err)
{
  static char *buf;
  char *t = toutf8(s);
  size_t l = strlen(t);
  buf = (char*)malloc(l+1);
  err = NULL;
  if (!scanstr(buf, t)) {
    for (size_t i = strlen(t); i < l-1; i += strlen(t+i+1)+1) {
      static char msg[1024];
      char *u = t+i+1, c[5];
      const char *v = u8strind(u, 1);
      c[0] = '\\'; strncpy(c+1, u, v-u); c[v-u+1] = 0;
      char *e = fromutf8(c);
      sprintf(msg, "syntax error, invalid character escape '%s'", e);
      free(e);
      err = msg;
    }
  }
  free(t);
  return buf;
}

char *printstr(const char *s)
{
  static char *buf;
  buf = (char*)malloc(4*strlen(s)+1);;
  char *t = fromutf8(pstr(buf, s));
  free(buf);
  return t;
}

size_t symsplit(const std::string& s)
{
  size_t pos = s.rfind("::");
  if (pos != std::string::npos)  {
    /* As the symbol might actually begin with ':', we might have to back out
       some more. */
    while (pos > 0 && s[pos-1] == ':') --pos;
  }
  return pos;
}

/***************************************************************************/

/* POSIX CRC32 checksum algorithm, original source from
   http://gback.cs.vt.edu:8080/source/xref/tests/cksum.c. */

/* Copyright (C) 2004, 2005, 2006 Board of Trustees, Leland Stanford
   Jr. University.  All rights reserved.

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. */

/* crctab[] and cksum() are from the `cksum' entry in SUSv3. */

#include <stdint.h>

static unsigned long crctab[] = {
  0x00000000,
  0x04c11db7, 0x09823b6e, 0x0d4326d9, 0x130476dc, 0x17c56b6b,
  0x1a864db2, 0x1e475005, 0x2608edb8, 0x22c9f00f, 0x2f8ad6d6,
  0x2b4bcb61, 0x350c9b64, 0x31cd86d3, 0x3c8ea00a, 0x384fbdbd,
  0x4c11db70, 0x48d0c6c7, 0x4593e01e, 0x4152fda9, 0x5f15adac,
  0x5bd4b01b, 0x569796c2, 0x52568b75, 0x6a1936c8, 0x6ed82b7f,
  0x639b0da6, 0x675a1011, 0x791d4014, 0x7ddc5da3, 0x709f7b7a,
  0x745e66cd, 0x9823b6e0, 0x9ce2ab57, 0x91a18d8e, 0x95609039,
  0x8b27c03c, 0x8fe6dd8b, 0x82a5fb52, 0x8664e6e5, 0xbe2b5b58,
  0xbaea46ef, 0xb7a96036, 0xb3687d81, 0xad2f2d84, 0xa9ee3033,
  0xa4ad16ea, 0xa06c0b5d, 0xd4326d90, 0xd0f37027, 0xddb056fe,
  0xd9714b49, 0xc7361b4c, 0xc3f706fb, 0xceb42022, 0xca753d95,
  0xf23a8028, 0xf6fb9d9f, 0xfbb8bb46, 0xff79a6f1, 0xe13ef6f4,
  0xe5ffeb43, 0xe8bccd9a, 0xec7dd02d, 0x34867077, 0x30476dc0,
  0x3d044b19, 0x39c556ae, 0x278206ab, 0x23431b1c, 0x2e003dc5,
  0x2ac12072, 0x128e9dcf, 0x164f8078, 0x1b0ca6a1, 0x1fcdbb16,
  0x018aeb13, 0x054bf6a4, 0x0808d07d, 0x0cc9cdca, 0x7897ab07,
  0x7c56b6b0, 0x71159069, 0x75d48dde, 0x6b93dddb, 0x6f52c06c,
  0x6211e6b5, 0x66d0fb02, 0x5e9f46bf, 0x5a5e5b08, 0x571d7dd1,
  0x53dc6066, 0x4d9b3063, 0x495a2dd4, 0x44190b0d, 0x40d816ba,
  0xaca5c697, 0xa864db20, 0xa527fdf9, 0xa1e6e04e, 0xbfa1b04b,
  0xbb60adfc, 0xb6238b25, 0xb2e29692, 0x8aad2b2f, 0x8e6c3698,
  0x832f1041, 0x87ee0df6, 0x99a95df3, 0x9d684044, 0x902b669d,
  0x94ea7b2a, 0xe0b41de7, 0xe4750050, 0xe9362689, 0xedf73b3e,
  0xf3b06b3b, 0xf771768c, 0xfa325055, 0xfef34de2, 0xc6bcf05f,
  0xc27dede8, 0xcf3ecb31, 0xcbffd686, 0xd5b88683, 0xd1799b34,
  0xdc3abded, 0xd8fba05a, 0x690ce0ee, 0x6dcdfd59, 0x608edb80,
  0x644fc637, 0x7a089632, 0x7ec98b85, 0x738aad5c, 0x774bb0eb,
  0x4f040d56, 0x4bc510e1, 0x46863638, 0x42472b8f, 0x5c007b8a,
  0x58c1663d, 0x558240e4, 0x51435d53, 0x251d3b9e, 0x21dc2629,
  0x2c9f00f0, 0x285e1d47, 0x36194d42, 0x32d850f5, 0x3f9b762c,
  0x3b5a6b9b, 0x0315d626, 0x07d4cb91, 0x0a97ed48, 0x0e56f0ff,
  0x1011a0fa, 0x14d0bd4d, 0x19939b94, 0x1d528623, 0xf12f560e,
  0xf5ee4bb9, 0xf8ad6d60, 0xfc6c70d7, 0xe22b20d2, 0xe6ea3d65,
  0xeba91bbc, 0xef68060b, 0xd727bbb6, 0xd3e6a601, 0xdea580d8,
  0xda649d6f, 0xc423cd6a, 0xc0e2d0dd, 0xcda1f604, 0xc960ebb3,
  0xbd3e8d7e, 0xb9ff90c9, 0xb4bcb610, 0xb07daba7, 0xae3afba2,
  0xaafbe615, 0xa7b8c0cc, 0xa379dd7b, 0x9b3660c6, 0x9ff77d71,
  0x92b45ba8, 0x9675461f, 0x8832161a, 0x8cf30bad, 0x81b02d74,
  0x857130c3, 0x5d8a9099, 0x594b8d2e, 0x5408abf7, 0x50c9b640,
  0x4e8ee645, 0x4a4ffbf2, 0x470cdd2b, 0x43cdc09c, 0x7b827d21,
  0x7f436096, 0x7200464f, 0x76c15bf8, 0x68860bfd, 0x6c47164a,
  0x61043093, 0x65c52d24, 0x119b4be9, 0x155a565e, 0x18197087,
  0x1cd86d30, 0x029f3d35, 0x065e2082, 0x0b1d065b, 0x0fdc1bec,
  0x3793a651, 0x3352bbe6, 0x3e119d3f, 0x3ad08088, 0x2497d08d,
  0x2056cd3a, 0x2d15ebe3, 0x29d4f654, 0xc5a92679, 0xc1683bce,
  0xcc2b1d17, 0xc8ea00a0, 0xd6ad50a5, 0xd26c4d12, 0xdf2f6bcb,
  0xdbee767c, 0xe3a1cbc1, 0xe760d676, 0xea23f0af, 0xeee2ed18,
  0xf0a5bd1d, 0xf464a0aa, 0xf9278673, 0xfde69bc4, 0x89b8fd09,
  0x8d79e0be, 0x803ac667, 0x84fbdbd0, 0x9abc8bd5, 0x9e7d9662,
  0x933eb0bb, 0x97ffad0c, 0xafb010b1, 0xab710d06, 0xa6322bdf,
  0xa2f33668, 0xbcb4666d, 0xb8757bda, 0xb5365d03, 0xb1f740b4
};

/* This is the algorithm used by the Posix `cksum' utility. */

uint32_t cksum(size_t n, const unsigned char *b)
{
  uint32_t s = 0;
  size_t i;
  for (i = n; i > 0; --i)
    {
      unsigned char c = *b++;
      s = (s << 8) ^ crctab[(s >> 24) ^ c];
    }
  while (n != 0)
    {
      unsigned char c = n;
      n >>= 8;
      s = (s << 8) ^ crctab[(s >> 24) ^ c];
    }
  return ~s;
}

/***************************************************************************/

/* Thread-safe, locale-independent glib routines to convert between double
   precision floating point numbers and strings. Original source:
   gstrfuncs.h and gstrfuncs.c in glib-2.22.2/glib/, available from
   http://www.gnome.org. */

/* GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/* Functions like the ones in <ctype.h> that are not affected by locale. */
typedef enum {
  ASCII_ALNUM  = 1 << 0,
  ASCII_ALPHA  = 1 << 1,
  ASCII_CNTRL  = 1 << 2,
  ASCII_DIGIT  = 1 << 3,
  ASCII_GRAPH  = 1 << 4,
  ASCII_LOWER  = 1 << 5,
  ASCII_PRINT  = 1 << 6,
  ASCII_PUNCT  = 1 << 7,
  ASCII_SPACE  = 1 << 8,
  ASCII_UPPER  = 1 << 9,
  ASCII_XDIGIT = 1 << 10
} AsciiType;

static const unsigned short ascii_table[256] = {
  0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004,
  0x004, 0x104, 0x104, 0x004, 0x104, 0x104, 0x004, 0x004,
  0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004,
  0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004,
  0x140, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0,
  0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0,
  0x459, 0x459, 0x459, 0x459, 0x459, 0x459, 0x459, 0x459,
  0x459, 0x459, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0,
  0x0d0, 0x653, 0x653, 0x653, 0x653, 0x653, 0x653, 0x253,
  0x253, 0x253, 0x253, 0x253, 0x253, 0x253, 0x253, 0x253,
  0x253, 0x253, 0x253, 0x253, 0x253, 0x253, 0x253, 0x253,
  0x253, 0x253, 0x253, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0,
  0x0d0, 0x473, 0x473, 0x473, 0x473, 0x473, 0x473, 0x073,
  0x073, 0x073, 0x073, 0x073, 0x073, 0x073, 0x073, 0x073,
  0x073, 0x073, 0x073, 0x073, 0x073, 0x073, 0x073, 0x073,
  0x073, 0x073, 0x073, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x004
  /* the upper 128 are all zeroes */
};

#define ascii_isalnum(c) \
  ((ascii_table[(unsigned char) (c)] & ASCII_ALNUM) != 0)

#define ascii_isalpha(c) \
  ((ascii_table[(unsigned char) (c)] & ASCII_ALPHA) != 0)

#define ascii_iscntrl(c) \
  ((ascii_table[(unsigned char) (c)] & ASCII_CNTRL) != 0)

#define ascii_isdigit(c) \
  ((ascii_table[(unsigned char) (c)] & ASCII_DIGIT) != 0)

#define ascii_isgraph(c) \
  ((ascii_table[(unsigned char) (c)] & ASCII_GRAPH) != 0)

#define ascii_islower(c) \
  ((ascii_table[(unsigned char) (c)] & ASCII_LOWER) != 0)

#define ascii_isprint(c) \
  ((ascii_table[(unsigned char) (c)] & ASCII_PRINT) != 0)

#define ascii_ispunct(c) \
  ((ascii_table[(unsigned char) (c)] & ASCII_PUNCT) != 0)

#define ascii_isspace(c) \
  ((ascii_table[(unsigned char) (c)] & ASCII_SPACE) != 0)

#define ascii_isupper(c) \
  ((ascii_table[(unsigned char) (c)] & ASCII_UPPER) != 0)

#define ascii_isxdigit(c) \
  ((ascii_table[(unsigned char) (c)] & ASCII_XDIGIT) != 0)

/*
 * This function behaves like the standard strtod() function does in the C
 * locale. It does this without actually changing the current locale, since
 * that would not be thread-safe.
 */
double
my_strtod (const char  *nptr,
	   char       **endptr)
{
  char *fail_pos;
  double val;
  struct lconv *locale_data;
  const char *decimal_point;
  int decimal_point_len;
  const char *p, *decimal_point_pos;
  const char *end = NULL; /* Silence gcc */
  int strtod_errno;

  fail_pos = NULL;

  locale_data = localeconv ();
  decimal_point = locale_data->decimal_point;
  decimal_point_len = strlen (decimal_point);

  /* assert (decimal_point_len != 0); */
  
  decimal_point_pos = NULL;
  end = NULL;

  if (decimal_point[0] != '.' || 
      decimal_point[1] != 0)
    {
      p = nptr;
      /* Skip leading space */
      while (ascii_isspace (*p))
	p++;
      
      /* Skip leading optional sign */
      if (*p == '+' || *p == '-')
	p++;
      
      if (p[0] == '0' && 
	  (p[1] == 'x' || p[1] == 'X'))
	{
	  p += 2;
	  /* HEX - find the (optional) decimal point */
	  
	  while (ascii_isxdigit (*p))
	    p++;
	  
	  if (*p == '.')
	    decimal_point_pos = p++;
	      
	  while (ascii_isxdigit (*p))
	    p++;
	  
	  if (*p == 'p' || *p == 'P')
	    p++;
	  if (*p == '+' || *p == '-')
	    p++;
	  while (ascii_isdigit (*p))
	    p++;

	  end = p;
	}
      else if (ascii_isdigit (*p) || *p == '.')
	{
	  while (ascii_isdigit (*p))
	    p++;
	  
	  if (*p == '.')
	    decimal_point_pos = p++;
	  
	  while (ascii_isdigit (*p))
	    p++;
	  
	  if (*p == 'e' || *p == 'E')
	    p++;
	  if (*p == '+' || *p == '-')
	    p++;
	  while (ascii_isdigit (*p))
	    p++;

	  end = p;
	}
      /* For the other cases, we need not convert the decimal point */
    }

  if (decimal_point_pos)
    {
      char *copy, *c;

      /* We need to convert the '.' to the locale specific decimal point */
      copy = (char*)malloc (end - nptr + 1 + decimal_point_len);

      /* assert (copy != NULL); */
      
      c = copy;
      memcpy (c, nptr, decimal_point_pos - nptr);
      c += decimal_point_pos - nptr;
      memcpy (c, decimal_point, decimal_point_len);
      c += decimal_point_len;
      memcpy (c, decimal_point_pos + 1, end - (decimal_point_pos + 1));
      c += end - (decimal_point_pos + 1);
      *c = 0;

      errno = 0;
      val = strtod (copy, &fail_pos);
      strtod_errno = errno;

      if (fail_pos)
	{
	  if (fail_pos - copy > decimal_point_pos - nptr)
	    fail_pos = (char *)nptr + (fail_pos - copy) - (decimal_point_len - 1);
	  else
	    fail_pos = (char *)nptr + (fail_pos - copy);
	}
      
      free (copy);
	  
    }
  else if (end)
    {
      char *copy;
      
      copy = (char*)malloc (end - (char *)nptr + 1);
      /* assert (copy != NULL); */
      memcpy (copy, nptr, end - nptr);
      *(copy + (end - (char *)nptr)) = 0;
      
      errno = 0;
      val = strtod (copy, &fail_pos);
      strtod_errno = errno;

      if (fail_pos)
	{
	  fail_pos = (char *)nptr + (fail_pos - copy);
	}
      
      free (copy);
    }
  else
    {
      errno = 0;
      val = strtod (nptr, &fail_pos);
      strtod_errno = errno;
    }

  if (endptr)
    *endptr = fail_pos;

  errno = strtod_errno;

  return val;
}

/*
 * Converts a double to a string, using the '.' as decimal point. To format
 * the number you pass in a printf()-style format string. Allowed conversion
 * specifiers are 'e', 'E', 'f', 'F', 'g' and 'G'.
 */
char *
my_formatd (char        *buffer,
	    const char  *format,
	    double       d)
{
  struct lconv *locale_data;
  const char *decimal_point;
  int decimal_point_len;
  char *p;
  int rest_len;
  char format_char;

  format_char = format[strlen (format) - 1];
  
  if (format[0] != '%')
    return NULL;

  if (strpbrk (format + 1, "'l%"))
    return NULL;

  if (!(format_char == 'e' || format_char == 'E' ||
	format_char == 'f' || format_char == 'F' ||
	format_char == 'g' || format_char == 'G'))
    return NULL;

  sprintf (buffer, format, d);

  locale_data = localeconv ();
  decimal_point = locale_data->decimal_point;
  decimal_point_len = strlen (decimal_point);

  /* assert (decimal_point_len != 0); */

  if (decimal_point[0] != '.' ||
      decimal_point[1] != 0)
    {
      p = buffer;

      if (*p == '+' || *p == '-')
	p++;

      while (ascii_isdigit ((unsigned char)*p))
	p++;

      if (strncmp (p, decimal_point, decimal_point_len) == 0)
	{
	  *p = '.';
	  p++;
	  if (decimal_point_len > 1) {
	    rest_len = strlen (p + (decimal_point_len-1));
	    memmove (p, p + (decimal_point_len-1),
		     rest_len);
	    p[rest_len] = 0;
	    
	  }
	}
    }
  
  return buffer;
}
