/* Port of CSV module from Q to Pure

   Author: Eddie Rucker
   Date: July 3, 2008
   Modified: November 21, 2008 to handle namespaces
   Rewritten: June 11, 2010
*/

/*

Copyright (c) 2008, 2009, 2010, Robert E. Rucker

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
#include <pure/runtime.h>
#define BUFFSIZE 1024
#define RECSIZE 128

#define FILE_END 1
#define ERR_MEM -1
#define ERR_READ -2
#define ERR_WRITE -3
#define ERR_PARSE -4

/* quote_flag style */

#define MINIMAL 1
#define STRING  2
#define ALL     3

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
  int quote_flag;
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
  char rw;  // read, write, or append char
  FILE *fp;
  unsigned long line; // line number for errors
} csv_t;

void dialect_free(dialect_t *d)
{
  free(d->quote);
  free(d->escape);
  free(d->delimiter);
  free(d->terminator);
  free(d);
}

dialect_t *dialect_new(char *quote,
		       char *escape,
		       char *delimiter,
		       char *terminator,
		       int quote_flag)
{
  dialect_t *d;
  if (d = (dialect_t *)malloc(sizeof(dialect_t))) {
    d->quote = d->escape = d->delimiter = d->terminator = NULL;
    if ((d->quote = strdup(quote)) &&
	(d->escape = strdup(escape)) &&
	(d->delimiter = strdup(delimiter)) &&
	(d->terminator = strdup(terminator))) {
      d->quote_n = strlen(quote);
      d->escape_n = strlen(escape);
      d->delimiter_n = strlen(delimiter);
      d->terminator_n = strlen(terminator);
      d->quote_flag = quote_flag;
      return d;
    }
    dialect_free(d);
  }
  return NULL;
}

/*** Record functions ***/

static record_t *record_new(void)
{
  record_t *r;
  if (r = (record_t *)malloc(sizeof(record_t))) {
    r->len = 0;
    r->growto = RECSIZE;
    if (r->x = (pure_expr **)malloc(r->growto*sizeof(pure_expr *)))
      return r;
    free(r);
  }
  return NULL;
}

static void record_clear(record_t *r)
{
  if (r) r->len = 0;
}

static void record_free(record_t *r)
{
  if (r) { // avoid double free errors
    free(r->x);
    free(r);
  }
}

/* links a list pointer to x */

static record_t *record_add(record_t *r, pure_expr *x)
{
  if (r) {
    if (r->len == r->growto) {
      r->growto <<= 1;
      pure_expr **t;
      if (t = (pure_expr **)realloc(r->x, r->growto*sizeof(pure_expr *)))
	r->x = t;
      else {
	record_free(r);
	return NULL;
      }
    }
    *(r->x + r->len) = x;
    ++r->len;
  }
  return r;
}

static buffer_t *buffer_new(void)
{
  buffer_t *t;
  if (t = (buffer_t *)malloc(sizeof(buffer_t))) {
    t->len = 0;
    t->growto = BUFFSIZE;
    if (t->c = (char *)malloc(t->growto*sizeof(char)))
      return t;
    free(t);
  }
  return NULL;
}

static void buffer_clear(buffer_t *b)
{
  if (b) b->len = 0;
}

static void buffer_free(buffer_t *b)
{
  if (b) {
    free(b->c);
    free(b);
  }
}

static buffer_t *buffer_add(buffer_t *b, char *s, int count)
{
  if (b) {
    if (b->len + count >= b->growto) {
      while (b->len + count >= b->growto)
	b->growto <<= 1;
      char *t;
      if (t = (char *)realloc(b->c, b->growto*sizeof(char)))
	b->c = t;
      else {
	buffer_free(b);
	return b;
      }
    }
    memcpy(b->c + b->len, s, count);
    b->len += count;
  }
  return b;
}

static buffer_t *buffer_del(buffer_t *b, size_t pos, int count)
{
  if (b) {
    b->len -= count;
    memcpy(b->c + pos, b->c + pos + count, b->len);
  }
  return b;
}

/* Reads from file past buffer->len. If buffer address changes, offset
   is the difference between the old and new address. */

static int buffer_fill(csv_t *csv, long *offset)
{
  buffer_t *b = csv->buffer;
  int ch, n = b->growto - b->len;
  char *s = b->c + b->len;
  char *ofs = b->c;
  while (1) {
    while (n-- > 0) {
      if ((ch = getc(csv->fp)) == EOF)
	return FILE_END;
      if ((*s++ = ch) == '\n') {
	*offset = b->c - ofs;
	b->len = s - b->c;
	return 0;
      }
    }
    char *t;
    n = b->growto;
    b->growto <<= 1;
    if (t = (char *)realloc(b->c, b->growto)) {
      b->c = t;
      s = b->c + n;
    } else
      return ERR_MEM;
  }
}

void csv_close(csv_t *csv)
{
  if (csv) {
    if (csv->buffer) {
      buffer_free(csv->buffer);
      csv->buffer = NULL;
    }
    if (csv->record) {
      record_free(csv->record);
      csv->record = NULL;
    }
    if (csv->fp) {
      fclose(csv->fp);
      csv->fp = NULL;
    }
  }
}

void csv_free(csv_t *csv)
{
  csv_close(csv);
  free(csv);
}

csv_t *csv_open(char *fname, char *rw, dialect_t *d)
{
  csv_t *t;
  if (t = (csv_t *)malloc(sizeof(csv_t))) {
    t->line = 1;
    t->buffer = NULL;
    t->record = NULL;
    if (t->buffer = buffer_new()) {
      t->dialect = d;
      t->rw = *rw; // get first char since rw is one of "r", "w", "a".
      if (t->fp = fopen(fname, rw))
	if (t->record = record_new())
	  return t;
    }
    csv_free(t);
  }
  return NULL;
}

static int write_quoted(csv_t *csv, pure_expr **xs, size_t len)
{
  int i;
  char *ts;
  dialect_t *d = csv->dialect;
  buffer_t *b = csv->buffer;
  buffer_clear(b);
  for (i = 0; i < len && b; ++i) {
    if (pure_is_string(xs[i], &ts)) {
      buffer_add(b, d->quote, d->quote_n);
      char *p, *tp;
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
	} else if (*p == '\n') {
	  ++csv->line;
	  qflag = 1;
	  ++p;
	} else
	  ++p;
      }
      buffer_add(b, tp, p-tp);
      if (!qflag && d->quote_flag == MINIMAL) // kill leading quote
	buffer_del(b, b->len-(p-ts)-1, d->quote_n);
      else
	buffer_add(b, d->quote, d->quote_n);
    } else {
      ts = str(xs[i]);
      if (d->quote_flag == ALL) {
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
    if (pure_is_string(xs[i], &ts)) {
      char *p, *tp;
      p = tp = ts;
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
  case ERR_MEM: 
    return error("out of memory");
  case ERR_WRITE:
    sprintf(msg, "error writing line %ld", csv->line);
    return error(msg);
  default:
    return pure_tuplev(0, NULL);
  }
}

static pure_expr *char_to_expr(char *s, dialect_t *d)
{
  if (!strncmp(s, d->quote, d->quote_n)) { // skip quote this is a string
    s += d->quote_n;
    return pure_cstring_dup(s);
  }
  if (d->quote_flag != ALL) {
    char *p;
    long i = strtol(s, &p, 0);
    if (*p == 0)
      return pure_int(i);
    double f = strtod(s, &p);
    if (*p == 0)
      return pure_double(f);
  }
  return pure_cstring_dup(s);
}

static int read_quoted(csv_t *csv)
{
  char *s;
  char *t, *w; // field start, overwrite
  dialect_t *d = csv->dialect;
  long offset;
  int res, more = 1;
  record_clear(csv->record);
  buffer_clear(csv->buffer);
  if ((res = buffer_fill(csv, &offset)) != 0)
    return res;
  s = csv->buffer->c; 
  while (more) {
    t = w = s;
    if (!strncmp(s, d->quote, d->quote_n)) { // quoted field
      w = s += d->quote_n; // leave quote as a flag for string conversion
      while (1)
	if (!strncmp(s, d->quote, d->quote_n)) {
	  s += d->quote_n; // skip escape quote
	  if (*s == '\n') {
	    ++csv->line;
	    more = 0;
	    break;
	  } else if (!strncmp(s, d->delimiter, d->delimiter_n)) {
	    s += d->delimiter_n;
	    break;
	  } else if (!strncmp(s, d->quote, d->quote_n)) {
	    memcpy(w, s, d->quote_n);
	    w += d->quote_n;
	    s += d->quote_n;
	  } else {
	    return ERR_PARSE;
	  }
	} else if (*s == '\n') { // inside quotes
	  ++csv->line;
	  *w++ = *s++;
	  if ((res = buffer_fill(csv, &offset)) != 0) // get more data
	    return res;
	  w += offset;
	  s += offset;
	  t += offset;
	} else
	  *w++ = *s++; // copy over escape quote
    } else if (strncmp(s, d->delimiter, d->delimiter_n)) // not delimiter
      while (1)
	if (!strncmp(s, "\r\n", 2)) {
	  s += 2;
	  ++csv->line;
	  more = 0;
	  break;
	} else if (*s == '\n') {
	  ++s;
	  ++csv->line;
	  more = 0;
	  break;
	} else if (!strncmp(s, d->delimiter, d->delimiter_n)) {
	  s += d->delimiter_n;
	  break;
	} else if (!strncmp(s, d->quote, d->quote_n))
	  return ERR_PARSE;
	else
	  *w++ = *s++;
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
  int res, more = 1;
  record_clear(csv->record);
  buffer_clear(csv->buffer);
  if ((res = buffer_fill(csv, &offset)) != 0)
    return res;
  s = csv->buffer->c;
  while (more) {
    t = w = s;
    while (1) {
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
	} else if (!strncmp(s, "\r\n", 2)) {
	  ++csv->line;
	  memcpy(w, "\r\n", 2);
	  w += 2;
	  s += 2;
	  if ((res = buffer_fill(csv, &offset)) != 0) // read some more data
	    return res;
	  w += offset;
	  s += offset;
	  t += offset;
	} else if (*s == '\n') {
	  ++csv->line;
	  *s++ = *t++;
	  if ((res = buffer_fill(csv, &offset)) != 0) // read some more data
	    return res;
	  w += offset;
	  s += offset;
	  t += offset;
	}
      } else if (!strncmp(s, "\r\n", 2)) {
	s += 2;
	++csv->line;
	more = 0;
	break;
      } else if (*s == '\n') {
	++s;
	++csv->line;
	more = 0;
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
  case ERR_MEM:
    return error("out of memory");
  case ERR_READ:
    sprintf(msg, "read error at line %ld", csv->line);
    return error(msg);
  case ERR_PARSE:
    sprintf(msg, "parse error at line %ld", csv->line);
    return error(msg);
  case 0:
    return pure_matrix_columnsvq(csv->record->len, csv->record->x);
  case FILE_END:
    return pure_tuplev(0, NULL);
  }
}
