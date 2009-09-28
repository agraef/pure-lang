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
pure_expr *pure_trigger(int timeout, pure_expr *cond, pure_expr *value);

pure_expr *pure_this_cell(void);
pure_expr *pure_parse_range(const char *s);
pure_expr *pure_make_range(pure_expr *x);
pure_expr *pure_get_cell(const char *s);
pure_expr *pure_set_cell(const char *s, pure_expr *x);
pure_expr *pure_set_cell_text(const char *s, pure_expr *x);
pure_expr *pure_get_range(const char *s);
pure_expr *pure_set_range(const char *s, pure_expr *xs);
pure_expr *pure_set_range_text(const char *s, pure_expr *xs);

bool pure_check_window(const char *name);
pure_expr *pure_gl_window(const char *name, int timeout,
			  pure_expr *setup_cb,
			  pure_expr *config_cb,
			  pure_expr *display_cb,
			  pure_expr *timer_cb,
			  pure_expr *user_data);

typedef struct {
  GnmExprFunction const *node; /* Expression node that calls us. */
  GnmDependent *dep;           /* GnmDependent containing that node. */
  unsigned id;                 /* id of this item. */
} DepKey;

typedef struct {
  DepKey key;
  pure_expr *expr;  /* Pure funcall that initiated this datasource. */
  pure_expr *value; /* Current value of the datasource. */
  int pid; /* inferior process */
} DataSource;

typedef struct {
  DepKey key;
  char *name; /* Name of the frame widget containing the window. */
  SheetObject *so; /* The sheet object of the frame widget. */
  GSList *windows; /* The corresponding views (frame widgets). */
  gboolean being_destroyed;
  guint timeout, timer_id;
  pure_expr *setup_cb, *config_cb, *display_cb, *timer_cb; /* Callbacks. */
  pure_expr *user_data; /* User data supplied to the callbacks. */
} GLWindow;

bool pure_write_blob(FILE *fp, const DepKey *key, pure_expr *x);
bool pure_read_blob(FILE *fp, DepKey *key, pure_expr **x);

#endif /* _PURE_GNUMERIC_H */
