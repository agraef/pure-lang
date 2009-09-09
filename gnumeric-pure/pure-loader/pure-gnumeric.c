#include "pure-gnumeric.h"
#include <gnumeric-config.h>
#include <glib/gi18n-lib.h>

pure_expr *
value2pure(const GnmValue *v)
{
  switch (v->type) {
  case VALUE_BOOLEAN:
    return pure_int(v->v_bool.val);
  case VALUE_FLOAT:
    return pure_double(value_get_as_float(v));
  case VALUE_STRING:
    // XXXFIXME: Do we have to convert from the system encoding here?
    return pure_string_dup(v->v_str.val->str);
  case VALUE_CELLRANGE:
  case VALUE_ARRAY:
  case VALUE_ERROR:
  case VALUE_EMPTY:
    // XXXTODO
  default:
    return NULL;
  }
}

GnmValue *
pure2value(pure_expr *x)
{
  int32_t iv;
  double dv;
  const char *s;
  GnmValue *v;
  if (!x)
    v = NULL;
  else if (pure_is_int(x, &iv))
    v = value_new_int(iv);
  else if (pure_is_double(x, &dv))
    v = value_new_float((gnm_float)dv);
  else if (pure_is_string(x, &s))
    // XXXFIXME: Do we have to convert to the system encoding here?
    v = value_new_string(s);
  // XXXTODO: handle list and matrix return values
  else
    v = NULL;
  return v;
}

GnmValue *
call_pure_function(GnmFuncEvalInfo *ei, gint n_args,
		   GnmValue const * const *argv)
{
  GnmFunc const *func = ei->func_call->func;
  int i, j, min, max;
  pure_expr *x, *y, *z, *e, *fun, **args;
  GnmValue *ret;

  if (n_args < 0) {
    function_def_count_args(func, &min, &max);
    n_args = max;
  } else {
    min = 0; max = n_args;
  }

  if (n_args < 0 || min<0 || max<0 || max<min)
    // This shouldn't happen.
    return NULL;

  args = g_new(pure_expr*, n_args);

  for (i = 0; i < n_args && argv[i] != NULL; i++) {
    args[i] = value2pure(argv[i]);
    if (!args[i]) {
      gchar *msg = g_strdup_printf(_("** Invalid argument (#%d)"), i);
      for (j = 0; j < i; j++) pure_freenew(args[j]);
      g_free(args);
      ret = value_new_error(ei->pos, msg);
      g_free(msg);
      return ret;
    }
  }

  if (i < min) {
    // Not enough arguments. This shouldn't happen, so we just bail out here.
    for (j = 0; j < i; j++) pure_freenew(args[j]);
    g_free(args);
    return NULL;
  }

  fun = pure_quoted_symbol(pure_sym(func->name));
  if (min != max)
    // Variadic function, pass extra arguments as a list.
    x = pure_app(pure_appv(fun, min, args), pure_listv(i-min, args+min));
  else
    x = pure_appv(fun, i, args);
  g_free(args);
  y = pure_evalx(x, &e);

  if (y) {
    ret = pure2value(y);
    if (!ret) {
      char *s = str(y);
      gchar *msg = s?g_strdup_printf(_("** Invalid return value: %s"), s):
	_("Invalid return value");
      if (s) free(s);
      ret = value_new_error(ei->pos, msg);
      g_free(msg);
    }
    pure_freenew(y);
  } else {
    char *s = str(e);
    gchar *msg = s?g_strdup_printf(_("** Pure exception: %s"), s):
      _("Pure exception");
    if (e) pure_free(e); if (s) free(s);
    ret = value_new_error(ei->pos, msg);
    g_free(msg);
  }
  return ret;
}
