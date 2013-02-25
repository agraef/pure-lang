/* Port of CSV module from Q to Pure

   Author: Eddie Rucker
   Date: July 3, 2008
   Modified: November 21, 2008 to handle namespaces
   Rewritten: June 11, 2010
   updated: July 7, 2010 add space_before_quote_flag and trim_space_flag
   updated: July 15, 2010 handle space after quote and code cleanup
   updated: November 7, 2010 sniff for correct line terminators, including
            \r for EXCEL on MAC, and allow the user to define their own.
   updated: July 14, 2012 fix opening nonexisting file crash
*/

/*

Copyright (c) 2008, 2009, 2010, 2011 Robert E. Rucker

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of its contributors may be
   used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <pure/runtime.h>

#define BUFFSIZE 1024
#define RECSIZE   128

#define FILE_END   1

/* error types */

#define ERR_MEM   -1
#define ERR_READ  -2
#define ERR_WRITE -3
#define ERR_PARSE -4

/* quote styles */

#define QUOTE_ALL     0
#define QUOTE_MINIMAL 2
#define QUOTE_MASK    3

/* trim field space flags */

#define TRIM_RIGHT  4
#define TRIM_LEFT   8

/* quote space flags */

#define SPACE_AFTER_QUOTE  16
#define SPACE_BEFORE_QUOTE 32

/* file options */
#define LIST 1
#define HEADER 2

#define error(msg) \
  pure_app(pure_symbol(pure_sym("csv::error")), pure_cstring_dup(msg))

typedef struct {
  char *quote;
  size_t quote_n;
  char *escape;    
  size_t escape_n;
  char *delimiter;
  size_t delimiter_n;
  char *terminator;
  size_t terminator_n;
  int flags;
} dialect_t;

typedef struct {
  size_t growto;
  size_t len;
  pure_expr **x;
} record_t;

typedef struct {
  size_t growto;
  size_t len;
  char *c;
} buffer_t;

typedef struct {
  buffer_t *buffer;
  record_t *record;
  dialect_t *dialect;
  pure_expr *header;  // header keys for optional field indexing by string
  unsigned int opts;  // output a list/vector, has header
  char rw;            // read, write, or append char
  FILE *fp;
  unsigned long line; // line number for errors
} csv_t;

static void dialect_free(dialect_t *d)
{
  free(d->quote);
  free(d->escape);
  free(d->delimiter);
  free(d->terminator);
  free(d);
}

/* Sniff file for line terminator */
static char *sniff_quoted(char *fname, char *rw, char *quote)
{
  int qf = 0;
  if (*rw == 'w') { // check for platform
#ifdef WINDOWS
    return "\r\n";
#else
    return "\n";
#endif
  } else {
    FILE *fp = fopen(fname, "r");
    if (fp == NULL) {
#ifdef WINDOWS
      return "\r\n";
#else
      return "\n";
#endif
    }
    char ch, *t = quote;
    while ((ch = getc(fp)) != EOF) { // start sniffing
      if (*t == 0) { // skip quote
	qf ^= 1;
	t = quote;
      }
      if (ch == *t) {
	++t;
	continue;
      }
      if (*t == ch)
	++t;
      if (!qf && ch == '\n') {
	fclose(fp);
	return "\n";
      } else if (!qf && ch == '\r') {
	if (getc(fp) == '\n') {
	  fclose(fp);
	  return "\r\n";
	} else {
	  fclose(fp);
	  return "\r";
	}
      }
    }
    fclose(fp);
    return "\r\n"; // who knows?
  }
}

#define KEY(w) pure_symbol(pure_sym(w))

static dialect_t *dialect_new(char *fname, char *rw, pure_expr *rec)
{
  dialect_t *d;
  if ((d = (dialect_t *)malloc(sizeof(dialect_t)))) {
    d->quote = d->escape = d->delimiter = d->terminator = NULL;
    
    pure_is_cstring_dup(record_elem_at(rec, KEY("csv::quote")),
			&d->quote);
    pure_is_cstring_dup(record_elem_at(rec, KEY("csv::escape")),
			&d->escape);
    pure_is_cstring_dup(record_elem_at(rec, KEY("csv::delimiter")),
			&d->delimiter);
    pure_is_cstring_dup(record_elem_at(rec, KEY("csv::terminator")),
			&d->terminator);
    pure_is_int(record_elem_at(rec, KEY("csv::flags")), &d->flags);
    d->quote_n = strlen(d->quote);
    d->escape_n = strlen(d->escape);
    d->delimiter_n = strlen(d->delimiter);
    if (!strcmp(d->terminator, "")) {
      free(d->terminator);
      d->terminator = strdup(sniff_quoted(fname, rw, d->quote));
    }
    d->terminator_n = strlen(d->terminator);
    return d;
  }
  return NULL;
}

/*** Record functions ***/

static record_t *record_new(void)
{
  record_t *r;
  if ((r = (record_t *)malloc(sizeof(record_t)))) {
    r->len = 0;
    r->growto = RECSIZE;
    if ((r->x = (pure_expr **)malloc(r->growto*sizeof(pure_expr *))))
      return r;
    free(r);
  }
  return NULL;
}

static void record_clear(record_t *r)
{
  r->len = 0;
}

static void record_free(record_t *r)
{
  free(r->x);
  free(r);
}

/* links a list pointer to x */

static record_t *record_add(record_t *r, pure_expr *x)
{
  if (r->len == r->growto) {
    r->growto <<= 1;
    pure_expr **t;
    if ((t = (pure_expr **)realloc(r->x, r->growto*sizeof(pure_expr *))))
      r->x = t;
    else {
      record_free(r);
      return NULL;
    }
  }
  *(r->x + r->len) = x;
  ++r->len;
  return r;
}

static buffer_t *buffer_new(void)
{
  buffer_t *t;
  if ((t = (buffer_t *)malloc(sizeof(buffer_t)))) {
    t->len = 0;
    t->growto = BUFFSIZE;
    if ((t->c = (char *)malloc(t->growto*sizeof(char))))
      return t;
    free(t);
  }
  return NULL;
}

static void buffer_clear(buffer_t *b)
{
  b->len = 0;
}

static void buffer_free(buffer_t *b)
{
  free(b->c);
  free(b);
}

static buffer_t *buffer_add(buffer_t *b, char *s, int count)
{
  if (b->len + count >= b->growto) {
    while (b->len + count >= b->growto)
      b->growto <<= 1;
    char *t;
    if ((t = (char *)realloc(b->c, b->growto*sizeof(char))))
      b->c = t;
    else {
      buffer_free(b);
      return NULL;
    }
  }
  memcpy(b->c + b->len, s, count);
  b->len += count;
  return b;
}

static buffer_t *buffer_del(buffer_t *b, size_t pos, int count)
{
  b->len -= count;
  memcpy(b->c + pos, b->c + pos + count, b->len);
  return b;
}

/* Reads from file past buffer->len. If buffer address changes, offset
   is the difference between the old and new address. */
static int buffer_fill(csv_t *csv, long *offset)
{
  buffer_t *b = csv->buffer;
  dialect_t *d = csv->dialect;
  int ch = 0, brk = 0;
  size_t n = b->growto - b->len;
  char *s = b->c + b->len;
  char *ofs = b->c;
  while (1) {
    // be sure there is room for a \0
    while (n-d->terminator_n-1 > 0 && (ch = getc(csv->fp)) != EOF) {
      --n;
      *s++ = ch;
      if (n > d->terminator_n &&
	  !strncmp(s-d->terminator_n, d->terminator, d->terminator_n)) {
	brk = 1;
	break;
      }
    }
    *s = '\0';
    if (ferror(csv->fp)) return ERR_READ;
    if (brk)
      break;
    else if (ch == EOF) {
      if (s-1 == b->c)
	return FILE_END;
      break;
    }
    char *t;
    size_t m = s-b->c;
    n += b->growto;
    b->growto <<= 1;
    if ((t = (char *)realloc(b->c, b->growto))) {
      b->c = t;
      s = b->c + m;
    } else
      return ERR_MEM;
  }
  *offset = b->c - ofs;
  b->len = s - b->c;
  return 0;
}

void csv_close(csv_t *csv)
{
  if (csv->buffer) buffer_free(csv->buffer);
  if (csv->record) record_free(csv->record);
  if (csv->header) pure_free(csv->header);
  if (csv->dialect) dialect_free(csv->dialect);
  if (csv->fp) fclose(csv->fp);
  if (csv) free(csv);
}

pure_expr *csv_read(csv_t *csv);

static pure_expr *create_header(csv_t *csv)
{
  pure_expr *t = csv_read(csv);
  pure_expr *f = pure_symbol(pure_getsym("=>"));
  pure_expr *xs[csv->record->len];
  int i;
  for (i = 0; i < csv->record->len; ++i)
    xs[i] = pure_appl(f, 2, csv->record->x[i], pure_int(i));
  return pure_matrix_columnsvq(csv->record->len, xs);
}

csv_t *csv_open(char *fname, 
		char *rw, 
		pure_expr *dialect,
		unsigned int opts)
{
  csv_t *t;
  dialect_t *d;
  if ((d = dialect_new(fname, rw, dialect)) == NULL)
    return NULL;
  if ((t = (csv_t *)malloc(sizeof(csv_t)))) {
    t->line = 1;
    t->buffer = NULL;
    t->record = NULL;
    t->header = NULL;
    if ((t->buffer = buffer_new())) {
      t->rw = *rw; // get first char since rw is one of "r", "w", "a".
      if ((t->fp = fopen(fname, rw)) != NULL) {
	if ((t->record = record_new())) {
	  t->dialect = d;
	  if ((opts & HEADER) && rw[0] == 'r') {
	    unsigned int temp_flags = d->flags;
	    t->dialect->flags &= 0xFFFC; // header must return strings
	    t->opts = 0; // cut options off
	    t->header = (opts & HEADER) ? pure_new(create_header(t)) : NULL;
	    t->dialect->flags = temp_flags; // reset quote style
	  }
	  t->opts = opts;
	  return t;
	}
      } else return NULL;
    }
    dialect_free(d);
    csv_close(t);
  }
  return NULL;
}

pure_expr *csv_getheader(csv_t *csv)
{
  if (csv->header)
    return csv->header;
  else
    return pure_listv(0, NULL);
}

static char *trim_space(char *s, unsigned int side)
{
  if (side & TRIM_LEFT) {
    while (isspace(*s))
      ++s;
  }
  if (side & TRIM_RIGHT) {
    char *t = s + strlen(s) - 1;
    while (isspace(*t))
      --t;
    *++t = '\0';
  }
  return s;
}

static int write_quoted(csv_t *csv, pure_expr **xs, size_t len)
{
  int i;
  char *ts;
  dialect_t *d = csv->dialect;
  buffer_t *b = csv->buffer;
  buffer_clear(b);
  for (i = 0; i < len && b; ++i) {
    if (pure_is_cstring_dup(xs[i], &ts)) {
      ts = trim_space(ts, d->flags);
      buffer_add(b, d->quote, d->quote_n);
      char *p, *tp; // p == current pointer, tp = starting point for copy
      unsigned qflag = 0;
      p = tp = ts;
      while (*p) {
	if (!strncmp(p, d->quote, d->quote_n)) {
	  qflag = 1;
	  buffer_add(b, tp, p-tp+1);
	  buffer_add(b, d->quote, d->quote_n);
	  tp = p += d->quote_n;
	} else if (!strncmp(p, d->delimiter, d->delimiter_n)) {
	  qflag = 1;
	  p += d->delimiter_n;
	} else if (!strncmp(p, d->terminator, d->terminator_n)) {
	  ++csv->line;
	  qflag = 1;
	  p += d->terminator_n;
	} else
	  ++p;
      }
      buffer_add(b, tp, p-tp);
      // kill leading quote
      if (!qflag && (d->flags & QUOTE_MASK) == QUOTE_MINIMAL)
	buffer_del(b, b->len-(p-ts)-1, d->quote_n);
      else
	buffer_add(b, d->quote, d->quote_n);
      free(ts);
    } else {
      ts = str(xs[i]);
      if ((d->flags & QUOTE_MASK) == QUOTE_ALL) {
	buffer_add(b, d->quote, d->quote_n);
	buffer_add(b, ts, strlen(ts));
	buffer_add(b, d->quote, d->quote_n);
      } else
	buffer_add(b, ts, strlen(ts));
      free(ts);
    }
    if (i+1 < len) 
      buffer_add(b, d->delimiter, d->delimiter_n);
  }
  buffer_add(b, d->terminator, d->terminator_n);
  ++csv->line;
  if (!b)
    return ERR_MEM;
  else if (fwrite(b->c, 1, b->len, csv->fp) == b->len)
    return 0;
  else // don't free buffers because of sentry
    return ERR_WRITE;
}

static int write_escaped(csv_t *csv, pure_expr **xs, size_t len)
{
  int i;
  char *ts;
  dialect_t *d = csv->dialect;
  buffer_t *b = csv->buffer;
  buffer_clear(b);
  for (i = 0; i < len && b; ++i) {
    if (pure_is_cstring_dup(xs[i], &ts)) {
      ts = trim_space(ts, d->flags);
      char *p, *tp;
      p = tp = ts; // p == current pointer, tp = starting point for copy
      while (*p) {
	if (!strncmp(p, d->escape, d->escape_n)) {
	  buffer_add(b, tp, p-tp);
	  buffer_add(b, d->escape, d->escape_n);
	  tp = p;
	  p += d->escape_n;
	} else if (!strncmp(p, d->delimiter, d->delimiter_n)) {
	  buffer_add(b, tp, p-tp);
	  buffer_add(b, d->escape, d->escape_n);
	  tp = p;
	  p += d->delimiter_n;
	} else if (!strncmp(p, d->terminator, d->terminator_n)) {
	  buffer_add(b, tp, p-tp);
	  buffer_add(b, d->escape, d->escape_n);
	  ++csv->line;
	  tp = p;
	  p += d->terminator_n;
	} else
	  ++p;
      }
      buffer_add(b, tp, p-tp);
      free(ts);
    } else {
      ts = str(xs[i]);
      buffer_add(b, ts, strlen(ts));
      free(ts);
    }
    if (i+1 < len) 
      buffer_add(b, d->delimiter, d->delimiter_n);
  }
  buffer_add(b, d->terminator, d->terminator_n);
  if (!b)
    return ERR_MEM;
  else if (fwrite(b->c, 1, b->len, csv->fp) == b->len)
    return 0;
  else // don't free buffers because of sentry
    return ERR_WRITE;
}

pure_expr *csv_write(csv_t *csv, pure_expr **xs, size_t len)
{
  char msg[50];

  if (csv->rw == 'r') // we can be sure rw is one of 'r', 'w', 'a'
    return error("cannot write on a file opened for reading");
  int err = csv->dialect->escape_n ?
    write_escaped(csv, xs, len) : write_quoted(csv, xs, len);
  switch (err) {
  case 0:
    return pure_tuplev(0, NULL);
  case ERR_WRITE:
    sprintf(msg, "error writing line %ld", csv->line);
    return error(msg);
  case ERR_MEM: 
    return error("out of memory");
  default:
    return NULL;
  }
}

static int is_space(char *s, dialect_t *d)
{
  // Other unicode white space is probably significant
  return strncmp(s, d->delimiter, d->delimiter_n) && 
    (*s == ' ' || *s == '\v' || *s == '\t');
}

static int is_space_string(char *start, char *end, dialect_t *d)
{
  while (start < end && is_space(start, d))
    ++start;
  return start == end;
}

static pure_expr *char_to_expr(char *s, dialect_t *d)
{
  if (!strncmp(s, d->quote, d->quote_n)) { // skip quote this is a string
    s += d->quote_n;
    s = trim_space(s, d->flags);
    return pure_cstring_dup(s);
  }
  s = trim_space(s, d->flags);
  if (*s && (d->flags & QUOTE_MASK) != QUOTE_ALL) {
    char *p;
    long i = strtol(s, &p, 0);
    if (*p == 0) return pure_int(i);
    double f = strtod(s, &p);
    if (*p == 0) return pure_double(f);
  }
  return pure_cstring_dup(s);
}

static int read_quoted(csv_t *csv)
{
  char *s;
  char *t, *w; // field start, overwrite
  dialect_t *d = csv->dialect;
  long offset;
  int res;
  record_clear(csv->record);
  buffer_clear(csv->buffer);
  if ((res = buffer_fill(csv, &offset)))
    return res;
  s = csv->buffer->c;
  while (*s) {
  outer_loop:
    t = w = s;
    if (!strncmp(s, d->quote, d->quote_n)) { // quoted field
      w = s += d->quote_n; // leave quote as a flag for string conversion
      while (*s)
	if (!strncmp(s, d->quote, d->quote_n)) {
	  s += d->quote_n; // skip escape quote
	  if (is_space(s, d) && (d->flags & SPACE_AFTER_QUOTE)) {
	    ++s;
	    while (is_space(s, d))
	      ++s;
	  }
	  if (!strncmp(s, d->terminator, d->terminator_n)) {
	    s += d->terminator_n;
	    ++csv->line;
	    break;
	  } else if (!strncmp(s, d->delimiter, d->delimiter_n)) {
	    s += d->delimiter_n;
	    break;
	  } else if (!strncmp(s, d->quote, d->quote_n)) {
	    memcpy(w, s, d->quote_n);
	    w += d->quote_n;
	    s += d->quote_n;
	  } else 
	    return ERR_PARSE;
	} else if (!strncmp(s, d->terminator, d->terminator_n)) { // inside ""
	  ++csv->line;
	  memcpy(w, s, d->terminator_n);
	  w += d->terminator_n;
	  s += d->terminator_n;
	  if ((res = buffer_fill(csv, &offset)) != 0) // get more
	    return res;
	  w += offset;
	  s += offset;
	  t += offset;
	} else
	  *w++ = *s++; // copy over escape quote
    } else if (strncmp(s, d->delimiter, d->delimiter_n)) // not delimiter
      while (*s) {
	if (!strncmp(s, d->terminator, d->terminator_n)) {
	  s += d->terminator_n;
	  ++csv->line;
	  break;
	} else if (!strncmp(s, d->delimiter, d->delimiter_n)) {
	  s += d->delimiter_n;
	  break;
	} else if (!strncmp(s, d->quote, d->quote_n)) {
	  if ((d->flags & SPACE_BEFORE_QUOTE) && is_space_string(t, s, d))
	    goto outer_loop;
	  else
	    return ERR_PARSE;
	} else
	  *w++ = *s++;
      }
    else // s is the delimiter
      s += d->delimiter_n;
    *w = '\0'; // terminate field
    if (!record_add(csv->record, char_to_expr(t, d)))
      return ERR_MEM;
  }
  return 0;
}

static int read_escaped(csv_t *csv)
{
  char *s;
  char *t, *w; // field start, overwrite
  dialect_t *d = csv->dialect;
  long offset;
  int res;
  record_clear(csv->record);
  buffer_clear(csv->buffer);
  if ((res = buffer_fill(csv, &offset)) != 0)
    return res;
  s = csv->buffer->c;
  while (*s) {
    t = w = s;
    while (*s) {
      if (!strncmp(s, d->escape, d->escape_n)) {
	s += d->escape_n;
	if (!strncmp(s, d->escape, d->escape_n)) {
	  memcpy(w, s, d->escape_n);
	  w += d->escape_n;
	  s += d->escape_n;
	} else if (!strncmp(s, d->delimiter, d->delimiter_n)) {
	  memcpy(w, s, d->delimiter_n);
	  w += d->delimiter_n;
	  s += d->delimiter_n;
	} else if (!strncmp(s, d->terminator, d->terminator_n)) {
	  ++csv->line;
	  memcpy(w, d->terminator, d->terminator_n);
	  w += d->terminator_n;
	  s += d->terminator_n;
	  if ((res = buffer_fill(csv, &offset)) != 0) // read more data
	    return res;
	  w += offset;
	  s += offset;
	  t += offset;
	}
      } else if (!strncmp(s, d->terminator, d->terminator_n)) {
	s += d->terminator_n;
	++csv->line;
	break;
      } else if (!strncmp(s, d->delimiter, d->delimiter_n)) {
	s += d->delimiter_n;
	break;
      } else
	*w++ = *s++;
    }
    *w = '\0'; // terminate field
    if (!(record_add(csv->record, char_to_expr(t, d))))
      return ERR_MEM;
  }
  return 0;
}

pure_expr *csv_read(csv_t *csv)
{
  if (csv->rw == 'w' || csv->rw == 'a')
    return error("cannot read from a file opened for writing");
  int err = csv->dialect->escape_n ? read_escaped(csv) : read_quoted(csv);
  char msg[50];
  switch (err) {
  case 0:
    return (csv->opts & LIST) ? pure_listv(csv->record->len, csv->record->x) :
    pure_matrix_columnsvq(csv->record->len, csv->record->x);
  case ERR_PARSE:
    sprintf(msg, "parse error at line %ld", csv->line);
    return error(msg);
  case FILE_END:
    return pure_tuplev(0, NULL);
  case ERR_READ:
    sprintf(msg, "read error at line %ld", csv->line);
    return error(msg);
  case ERR_MEM:
    return error("out of memory");
  default:
    return NULL;
  }
}
