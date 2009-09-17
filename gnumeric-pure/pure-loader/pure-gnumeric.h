#ifndef _PURE_GNUMERIC_H
#define _PURE_GNUMERIC_H

#include <gnumeric-features.h>

#if GNM_VERSION_EPOCH <= 1 && GNM_VERSION_MAJOR <= 9 && GNM_VERSION_MINOR < 12
#define OLD_API
#endif

/*
 * Interface to Gnumeric internal functions.
 */

#include <gnumeric.h>
#include <cell.h>
#include <expr.h>
#include <expr-impl.h>
#include <parse-util.h>
#include <func.h>
#include <sheet.h>
#include <workbook.h>
#include <value.h>
#ifdef OLD_API
#include <str.h>
#endif
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pure/runtime.h>

pure_expr* value2pure(const GnmEvalPos *pos, const GnmValue *v,
		      const char *spec);
GnmValue* pure2value(const GnmEvalPos *pos, pure_expr *x, const char *spec);
GnmValue* call_pure_function(GnmFuncEvalInfo *ei, gint n_args,
			     GnmValue const * const *argv);
pure_expr* pure_gnmcall(const char *name, pure_expr *args);
pure_expr *pure_datasource(pure_expr *x);

pure_expr *pure_this_cell(void);
pure_expr *pure_get_cell(const char *s);
pure_expr *pure_set_cell(const char *s, pure_expr *x);
pure_expr *pure_set_text(const char *s, pure_expr *x);
pure_expr *pure_get_range(const char *s);
pure_expr *pure_set_range(const char *s, pure_expr *xs);

typedef struct { const void *p, *q; unsigned id; } keyval_t;

bool pure_write_blob(FILE *fp, const keyval_t *key, pure_expr *x);
bool pure_read_blob(FILE *fp, keyval_t *key, pure_expr **x);

#endif /* _PURE_GNUMERIC_H */
