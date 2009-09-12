#ifndef _PURE_GNUMERIC_H
#define _PURE_GNUMERIC_H

/*
 * Interface to Gnumeric internal functions.
 */

#include <gnumeric.h>
#include <cell.h>
#include <expr.h>
#include <expr-impl.h>
#include <func.h>
#include <sheet.h>
#include <value.h>
#include <str.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pure/runtime.h>

pure_expr* value2pure(const GnmEvalPos *pos, const GnmValue *v);
GnmValue* pure2value(const GnmEvalPos *pos, pure_expr *x);
GnmValue* call_pure_function(GnmFuncEvalInfo *ei, gint n_args,
			     GnmValue const * const *argv);
pure_expr* pure_gnmcall(const char *name, pure_expr *args);
pure_expr *pure_datasource(pure_expr *x);

#endif /* _PURE_GNUMERIC_H */
