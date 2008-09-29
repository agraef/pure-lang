
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
    uc[1] = 0x80 | c&0x3f;
    c = c >> 6;
    uc[0] = 0xc0 | c;
  } else if (c < 0x10000) {
    uc[3] = 0;
    uc[2] = 0x80 | c&0x3f;
    c = c >> 6;
    uc[1] = 0x80 | c&0x3f;
    c = c >> 6;
    uc[0] = 0xe0 | c;
  } else {
    uc[4] = 0;
    uc[3] = 0x80 | c&0x3f;
    c = c >> 6;
    uc[2] = 0x80 | c&0x3f;
    c = c >> 6;
    uc[1] = 0x80 | c&0x3f;
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
  /* always use the ANSI codepage on Windows (this might cause lossage if your
     system uses a different OEM codepage!?) */
  static char buf[20];
  sprintf(buf, "cp%d", GetACP());
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

/* conversion between UTF-8 and wchar_t */

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

#ifdef __STDC_ISO_10646__
#define towchar(c) ((wchar_t)(c))
#else
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

void u8dostr(const char *s, void (*f)(void*, unsigned long), void *p)
{
  const char *t;
  for (; *s; s = t) {
    long c = u8decode(s, &t);
    if (c < 0) {
      c = (unsigned char)*s;
      t = s+1;
    }
    f(p, c);
  }
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
  const char *t;

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
	long c = strtol(*s, &r, 0);
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
	long c = strtol(--*s, s, 0);
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

/* Thread-safe, locale-independent routines to convert between double
   precision floating point numbers and strings. These were pilfered from
   GNOME glib, and massaged a little to make them compile without the glib
   infrastructure. Source: glib/gstrfuncs.c in the glib tarball, distributed
   under GPL V2, available at http://www.gnome.org. */

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
