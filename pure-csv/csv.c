/* Port of CSV module from Q to Pure

   Author: Eddie Rucker
   Date:   July 3, 2008
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pure/runtime.h>

#define STRSIZE 128
#define BUFSIZE 1024

#define QUOTE_ALL 0
#define QUOTE_STRINGS 1
#define QUOTE_EMBEDDED 2

#define error_handler(msg) \
  return pure_app(pure_symbol(pure_sym("csv_error")), pure_cstring_dup(msg))

/* Return a CSV record as a Pure string.
   Input:
     fp:    File pointer to read from
     quote: Pure string representing CSV quotes

   Output:
     pure_string representing a CSV record. The string may have embedded '\n's.
       Does not check for badly formated records.

   Exceptions:
     csv_error msg is invoked f there is a system memory allocation error,
     beyond end of file, or a file error is encountered.
*/
pure_expr *c_csv_fgets(FILE *fp, char *quote) {
  char *bf, *tb, *s;
  int quote_count = 0, n_quote;
  long sz = BUFSIZE, n = 0;
  pure_expr *ret;

  if (!(s = bf = (char *)malloc(sz))) {
    error_handler("malloc error");
  }

  n_quote = strlen(quote);
  while (1) {
    s = bf + n;
    if (n > sz) {
      if (!(tb = realloc(bf, sz <<= 1))) {
	free(bf);
	error_handler("realloc error");
      }
      s = (bf = tb) + n;
    }
    tb = fgets(s, BUFSIZE, fp);
    if (ferror(fp)) {
      free(bf);
      return 0;
    }
    if (tb == NULL) {
      if (n == 0)
	return NULL;
      else
	return pure_cstring(bf);
    }
    n += strlen(s);
    if (*(bf+n-1) != '\n')
      continue;
    while (*s) {
      if (!strncmp(s, quote, n_quote)) {
	++quote_count;
	s += n_quote;
      } else 
	++s;
    }
    if (!(quote_count & 1))
      /* let pure handle freeing bf */
      return pure_cstring(bf);
  }
}

/* Convert a string to a number.
   Input:
     s: string to be converted.
     cvt_flag: 0 -> no conversion, 1 -> attempt conversion

   Output:
     Pure int, Pure double, or Pure string depending on cvt_flag=1 and
     s obeys strtol and strtod specs.
*/
pure_expr *convert_string(char *s, int cvt_flag) {
  long i;
  double d;
  char *p;

  if (cvt_flag) {
    i = strtol(s, &p, 0);
    if (*p == 0)
      return pure_int(i);
    d = strtod(s, &p);
    if (*p == 0)
      return pure_double(d);
  }
  return pure_cstring_dup(s);
}

#define putfld(len) \
  if (n_fld + len >= fld_sz) { \
    if (!(tfld = (char *)realloc(fld, fld_sz <<= 1))) \
      goto done; \
    fld = tfld; \
  } \
  fldp = fld + n_fld; \
  strncpy(fldp, s, len); \
  n_fld += len \


#define putrec(qt) \
  *(fldp + 1) = 0; \
  if (n_rec >= rec_sz - 1) { \
    if (!(trec = (pure_expr **)realloc(rec, (rec_sz+=64)*sizeof(pure_expr)))) \
      goto done; \
    rec = trec; \
  } \
  rec[n_rec++] = convert_string(fld, qt)

#define free_params				\
  free(delimiter);				\
  free(escape);					\
  free(quote);					\
  free(lineterm);				\
  free(elems)

/* Convert a CSV string to a list of fields
   input: 
     dialect: (Conversion flag, field delimeter char, string delimeter char)
     s: CSV formatted string

   Output: record of fields

   Exceptions:
     Invokes 'csv_error MSG' if the string is badly formatted, or memory error

   Notes:
     \r char is treated as white space except inside "" 
*/
pure_expr *c_csvstr_to_list(char *s, pure_expr *dialect) {
  size_t n_elems;
  pure_expr **elems, **rec, **trec;
  int n, st = 0, fld_sz = 256, n_fld, rec_sz = 64, n_ws = 0, n_rec = 0,
    n_delimiter, n_escape, n_quote, n_lineterm, skipspace_f, esc_eq_quote,
    quoting_style;
  char *fld, *tfld, *fldp, errmsg[80], *delimiter, *escape, *quote, *lineterm;

  if (!(pure_is_listv(dialect, &n_elems, &elems)
	&& n_elems == 6
	&& pure_is_cstring_dup(elems[0], &delimiter)
	&& pure_is_cstring_dup(elems[1], &escape)
	&& pure_is_cstring_dup(elems[2], &quote)
	&& pure_is_int(elems[3], &quoting_style)
	&& pure_is_cstring_dup(elems[4], &lineterm)
	&& pure_is_int(elems[5], &skipspace_f)))
    return 0;
  
  if (!(fld = (char *)malloc(fld_sz))) {
    free_params;
    error_handler("malloc error");
  }

  if (!(rec = (pure_expr **)malloc(rec_sz*sizeof(pure_expr)))) {
    free(fld);  
    free_params;
    error_handler("malloc error");
  }
  n_delimiter = strlen(delimiter);
  n_escape = strlen(escape);
  n_quote = strlen(quote);
  esc_eq_quote = !strcmp(escape, quote);
  n_lineterm = strlen(lineterm);
  fldp = fld;
  while (st < 10) {
    switch (st) {
    case 0:
      fldp = fld;
      *fldp = 0;
      n_fld = 0;
      if (!strncmp(s, delimiter, n_delimiter)) {
	putrec(QUOTE_ALL);
	s += n_delimiter;
      } else if (!strncmp(s, quote, n_quote)) {
	s += n_quote;
	st = 1;
      } else if (!*s || *s == EOF || !strncmp(s, lineterm, n_lineterm)) {
	putrec(QUOTE_ALL);
	st = 10;
      } else if (isspace(*s) && skipspace_f) {
	++s;
      } else if (!strncmp(s, escape, n_escape)) {
	sprintf(errmsg, "column %d: unexpected escape.", n_fld+1);
	st = 20;
      } else {
	putfld(1);
	++s;
	st = 4;
      }
      break;
    case 1:
      if (!strncmp(s, quote, n_quote)) {
	s += n_quote;
	st = 2;
      } else if (!*s || *s == EOF) {
	sprintf(errmsg, "column %d: expected {%s}.", 
		n_fld+1, quote);
	st = 20;
      } else if (!strncmp(s, escape, n_escape)) {
	s += n_escape;
	putfld(n_escape);
	++s;
      } else {
	putfld(1);
	++s;
      }
      break;
    case 2:
      if (!strncmp(s, quote, n_quote) && esc_eq_quote) {
	putfld(n_quote);
	s += n_quote;
	st = 1;
      } else if (!strncmp(s, delimiter, n_delimiter)) {
	putrec(QUOTE_ALL);
	s += n_delimiter;
	st = 0;
      } else if (!*s || *s == EOF || !strncmp(s, lineterm, n_lineterm)) {
	putrec(QUOTE_ALL);
	st = 10;
      } else if (isspace(*s)) {
	++s;
	st = 3;
      } else {
	sprintf(errmsg, "column %d: expected {%s}.", 
		n_fld+1, delimiter);
	st = 20;
      }
      break;
    case 3:
      if (!strncmp(s, delimiter, n_delimiter)) {
	putrec(QUOTE_ALL);
	s += n_delimiter;
	st = 0;
      } else if (!*s || *s == '\n' || *s == EOF) {
	putrec(QUOTE_ALL);
	st = 10;
      } else if (isspace(*s)) {
	++s;
      } else {
	sprintf(errmsg, "column %d: expected {%s}.",
		n_fld+1, delimiter);
	st = 20;
      }
      break;
    case 4:
      if (!strncmp(s, quote, n_quote) || !strncmp(s, escape, n_escape)) {
	sprintf(errmsg, "column %d: expected {%s}.", 
		n_fld+1, delimiter);
	st = 20;
      } else if (!strncmp(s, delimiter, n_delimiter)) {
	fldp -= n_ws;
	n_fld -= n_ws;
	putrec(quoting_style);
	s += n_delimiter;
	st = 0;
      } else if (!*s || *s == EOF || !strncmp(s, lineterm, n_lineterm)) {
	putrec(quoting_style);
	st = 10;
      } else if (isspace(*s)) {
	n_ws = n_ws ? n_ws+1 : 1;
	putfld(1);
	++s;
      } else {
	n_ws = 0;
	putfld(1);
	++s;
      }
      break;
    }
  }
 done:
  free(fld);
  free_params;
  if (st == 10)
    return pure_listv(n_rec, realloc(rec, sizeof(pure_expr)*n_rec));
  else {
    for (n = 0; n < n_rec; ++n)
      pure_free(rec[n]);
    free(rec);
    if (st==20)
      error_handler(errmsg);
    error_handler("malloc error");

  }
}

#define resize_str \
  if (len > sz) { \
    if (!(ts = (char *)realloc(s, sz <<= 1))) { \
      free(s); \
      error_handler("realloc error"); \
    } \
    s = ts; \
  } \
  t = s + mrk


#define insert					\
  mrk = len;					\
  len += strlen(tb);				\
  resize_str;					\
  strncpy(t, tb, len - mrk)

/* Convert list to a CSV formated string 
   Input:
     dialect: (Conversion flag, field delimeter char, string delimeter char)
     list: record to be converted
   
   Output: CSV formatted string
   
   Exceptions:
     Invokes csv_error if no more memory is available or if field cannot be
     converted.

   Notes:
     \r char is treated as white space except inside "" 
*/
pure_expr *c_list_to_csvstr(pure_expr *list, pure_expr *dialect) {
  size_t n_elems;
  int i, n, k, sz = 256, mrk, quote_cnt, delim_cnt, lineterm_cnt, len = 0, 
    skipspace_f, n_escape, n_quote, n_delimiter, n_lineterm, quoting_style,
    ival;
  char *s, *ts, *p, *sval, tb[48], errmsg[80], *escape, *quote, *delimiter,
    *lineterm;
  double dval;
  pure_expr **elems, **xs;
  register char *t;
  
  if (!(pure_is_listv(dialect, &n_elems, &elems)
	&& n_elems == 6
	&& pure_is_string_dup(elems[0], &delimiter)
	&& pure_is_string_dup(elems[1], &escape)
	&& pure_is_string_dup(elems[2], &quote)
	&& pure_is_int(elems[3], &quoting_style)
	&& pure_is_string_dup(elems[4], &lineterm)
	&& pure_is_int(elems[5], &skipspace_f)
	&& pure_is_listv(list, &n_elems, &xs))) {
    return 0;
  }
  
  if (!(s = (char *)malloc(sz))) {
    free_params;
    error_handler("malloc error");
  }
  
  n_escape = strlen(escape);
  n_quote = strlen(quote);
  n_delimiter = strlen(delimiter);
  n_lineterm = strlen(lineterm);
  for (i = 0; i < n_elems; ++i) {
    if (pure_is_int(xs[i], &ival)) {
      if (!quoting_style)
        sprintf(tb, "%s%d%s%s", quote, ival, quote, delimiter);
      else
        sprintf(tb, "%d%s", ival, delimiter);
      insert;
    } else if (pure_is_double(xs[i], &dval)) {
      if (!quoting_style)
        sprintf(tb, "%s%.16g%s%s", quote, dval, quote, delimiter);
      else
        sprintf(tb, "%.16g%s", dval, delimiter);
      insert;
    } else if (pure_is_cstring_dup(xs[i], &sval)) {
      quote_cnt = 0;
      delim_cnt = 0;
      lineterm_cnt = 0;
      p = sval;
      if (skipspace_f && quoting_style == QUOTE_EMBEDDED)
        while (isspace(*p)
               && strncmp(p, quote, n_delimiter)
               && strncmp(p, delimiter, n_delimiter)
               && strncmp(p, lineterm, n_lineterm))
          ++p;
      k = p - sval;
      mrk = len;
      while (*p) {
        if (!strncmp(p, quote, n_quote)) {
          ++quote_cnt;
          p += n_quote;
          len += n_escape + n_quote;
        } else if (!strncmp(p, delimiter, n_delimiter)) {
          ++delim_cnt;
          p += n_delimiter;
          len += n_delimiter;
        } else if (!strncmp(p, lineterm, n_lineterm)) {
          ++lineterm_cnt;
          p += n_lineterm;
          len += n_lineterm;
        } else {
          ++len;
          ++p;
        }
      }
      len += n_delimiter;
      p = sval + k;
      if (quoting_style == QUOTE_EMBEDDED
	  && !(quote_cnt+delim_cnt+lineterm_cnt)) {
        resize_str;
        k = len - mrk - 1;
        strncpy(t, p, k);
        t += k;
      } else {
        /* Add space for surrounding quotes */
        len += n_quote << 1;
        resize_str;
        strncpy(t, quote, n_quote);
        t += n_quote;
        while (*p) {
          if (!strncmp(p, quote, n_quote)) {
            strncpy(t, escape, n_escape);
            t += n_escape;
            strncpy(t, quote, n_quote);
            t += n_quote;
            p += n_quote;
          } else
            *t++ = *p++;
        }
        strncpy(t, quote, n_quote);
        t += n_quote;
      }
      strncpy(t, delimiter, n_delimiter);
      t += n_delimiter;
    } else {
      sprintf(errmsg, "field %d: invalid conversion type.", 
	      i+1);
      free_params;
      error_handler(errmsg);
    }
  }
  mrk = (len -= n_delimiter); /* write over last delimiter */
  len += n_lineterm;
  resize_str;
  strcpy(t, lineterm);
  free_params;
  return pure_cstring((char *)realloc(s, len+1));
}
