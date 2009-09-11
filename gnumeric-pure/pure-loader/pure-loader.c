#include <pure/runtime.h>
#include "pure-gnumeric.h"

#include <gnumeric-config.h>
#include "pure-loader.h"
#include <gnumeric.h>

#include <application.h>
#include <workbook-view.h>
#include <workbook.h>
#include <sheet.h>
#include <value.h>
#include <expr.h>
#include <expr-impl.h>
#include <gnm-plugin.h>

#include <goffice/app/error-info.h>
#include <goffice/app/go-plugin.h>
#include <goffice/app/go-plugin-loader.h>
#include <goffice/app/go-plugin-service.h>
#include <goffice/app/io-context.h>
#include <goffice/app/module-plugin-defs.h>
#include <gsf/gsf-impl-utils.h>

#include <glib/gi18n-lib.h>
#include <stdlib.h>
#include <stdio.h>

#define TYPE_GNM_PURE_PLUGIN_LOADER	(gnm_pure_plugin_loader_get_type ())
#define GNM_PURE_PLUGIN_LOADER(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), TYPE_GNM_PURE_PLUGIN_LOADER, GnmPurePluginLoader))
#define IS_GNM_PURE_PLUGIN_LOADER(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), TYPE_GNM_PURE_PLUGIN_LOADER))

typedef struct {
  GObject base;
  gchar* module_name;
} GnmPurePluginLoader;
typedef GObjectClass GnmPurePluginLoaderClass;

static const char help_template_text[] =
  N_("@FUNCTION=%s\n"
     "@SYNTAX=%s(...)\n"
     "@DESCRIPTION="
     "This Pure function hasn't been documented.\n");

static GnmFuncHelp help_template[] = {
  { GNM_FUNC_HELP_OLD, NULL },
  { GNM_FUNC_HELP_END }
};

static pure_interp *interp;

static GnmValue*
call_pure_function_args(GnmFuncEvalInfo *ei, GnmValue const * const *args)
{
  return call_pure_function(ei, -1, args);
}

static GnmValue*
call_pure_function_nodes(GnmFuncEvalInfo *ei, int argc,
			 GnmExprConstPtr const *argv)
{
  GnmValue *v, **args = g_new(GnmValue*, argc);
  gint i;
  for (i = 0; i < argc; i++)
    args[i] = gnm_expr_eval(argv[i], ei->pos,
			    GNM_EXPR_EVAL_PERMIT_NON_SCALAR);
  v = call_pure_function(ei, argc, (GnmValue const * const *)args);
  g_free(args);
  return v;
}

static gboolean
gplp_func_desc_load(GOPluginService *service,
		    char const *name,
		    GnmFuncDescriptor *res)
{
  pure_expr *descfun = pure_symbol(pure_sym("gnm_info"));
  // XXXFIXME: Do we have to convert from the system encoding here?
  pure_expr *desc = pure_app(descfun, pure_string_dup(name));
  gchar *arg_spec = NULL;
  gchar *arg_names = NULL;
  gchar *help_text = NULL;

  if (desc) {
    size_t size;
    pure_expr **elems = NULL;
    const char *spec = NULL, *names = NULL, *help = NULL;
    // XXXFIXME: Do we have to convert to the system encoding here?
    if (pure_is_tuplev(desc, &size, &elems) && size>0 && size<=3 &&
	(size==1 || (pure_is_string(elems[0], &spec) &&
		     pure_is_string(elems[1], &names))) &&
	(size == 2 ||
	 (size == 1 && pure_is_string(elems[0], &help)) ||
	 (size == 3 && pure_is_string(elems[2], &help)))) {
      if (spec) arg_spec = g_strdup(spec);
      if (names) arg_names = g_strdup(names);
      if (help) help_text = g_strdup(help);
    }
    if (elems) free(elems);
    pure_freenew(desc);
  }

  res->name = g_strdup(name);
  res->arg_spec = arg_spec;
  res->arg_names = arg_names;
  if (!help_text) help_text = g_strdup_printf(help_template_text, name, name);
  help_template[0].text = help_text;
  res->help = g_slice_dup(GnmFuncHelp, help_template);
  res->fn_args = NULL;
  res->fn_nodes = NULL;
  if (arg_spec)
    res->fn_args = &call_pure_function_args;
  else
    res->fn_nodes = &call_pure_function_nodes;
  res->linker = NULL;
  res->unlinker = NULL;
  res->impl_status = GNM_FUNC_IMPL_STATUS_UNIQUE_TO_GNUMERIC;
  res->test_status = GNM_FUNC_TEST_STATUS_UNKNOWN;

  return TRUE;
}

static void
gplp_set_attributes(GOPluginLoader *loader, GHashTable *attrs,
		    ErrorInfo **ret_error)
{
  GnmPurePluginLoader *loader_pure = GNM_PURE_PLUGIN_LOADER(loader);
  gchar *module_name = NULL;
  GO_INIT_RET_ERROR_INFO(ret_error);
  module_name = g_hash_table_lookup(attrs, "module_name");
  if (module_name)
    loader_pure->module_name = g_strdup(module_name);
  else
    *ret_error = error_info_new_str(_("** Pure module name not given."));
}

static GList *modnames;

static void
gplp_load_base(GOPluginLoader *loader, ErrorInfo **ret_error)
{
  GnmPurePluginLoader *loader_pure = GNM_PURE_PLUGIN_LOADER(loader);
  const char *modname = loader_pure->module_name;
  const char *dir = go_plugin_get_dir_name(go_plugin_loader_get_plugin(loader));
  gchar *script, *path;

  script = g_strconcat(modname, ".pure", NULL);
  path = g_build_filename(dir, script, NULL);
  if (g_file_test(path, G_FILE_TEST_EXISTS)) {
    if (!interp) {
      // First invocation, create an interpreter instance.
      interp = pure_create_interp(0, 0);
    }
    if (!interp)
      *ret_error = error_info_new_printf(_("** Error creating Pure interpreter."));
    else {
      gchar *cmdbuf = g_strdup_printf("using \"%s\";\n", path);
      // FIXME: This only prints errors on stderr right now.
      pure_evalcmd(cmdbuf);
      g_free(cmdbuf);
      modnames = g_list_append(modnames, path);
    }
  } else {
    *ret_error = error_info_new_printf(_("** Couldn't find \"%s\"."), script);
  }
  g_free(script);
}

static void pure_reload_script(gpointer data, gpointer unused)
{
  const char *path = (const char*)data;
  gchar *cmdbuf = g_strdup_printf("using \"%s\";\n", path);
  pure_evalcmd(cmdbuf);
  g_free(cmdbuf);
}

void pure_reload(GnmAction const *action, WorkbookControl *wbc)
{
  if (interp && g_list_first(modnames)) {
    pure_delete_interp(interp);
    interp = pure_create_interp(0, 0);
    pure_switch_interp(interp);
    g_list_foreach(modnames, pure_reload_script, NULL);
  }
}

void pure_edit(GnmAction const *action, WorkbookControl *wbc)
{
  GList *current = g_list_last(modnames);
  if (current) {
    const char *path = (const char*)current->data;
    const char *editor = getenv("EDITOR");
    gchar *cmdbuf = g_strdup_printf("%s \"%s\" &", editor?editor:"emacs", path);
    if (system(cmdbuf) == -1) perror("system");
    g_free(cmdbuf);
  }
}

static void
gplp_load_service_function_group(GOPluginLoader *loader,
				 GOPluginService *service,
				 ErrorInfo **ret_error)
{
  PluginServiceFunctionGroupCallbacks *cbs;
  g_return_if_fail (IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP (service));
  GO_INIT_RET_ERROR_INFO (ret_error);
  cbs = plugin_service_get_cbs (service);
  cbs->func_desc_load = &gplp_func_desc_load;
}

static gboolean
gplp_service_load(GOPluginLoader *l, GOPluginService *s, ErrorInfo **err)
{
  if (IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP (s))
    gplp_load_service_function_group (l, s, err);
  else
    return FALSE;
  return TRUE;
}

static gboolean
gplp_service_unload(GOPluginLoader *l, GOPluginService *s, ErrorInfo **err)
{
  if (IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP (s))
    ;
  else
    return FALSE;
  return TRUE;
}

static void
gplp_finalize(GObject *obj)
{
  GnmPurePluginLoader *loader_pure = GNM_PURE_PLUGIN_LOADER(obj);
  g_free(loader_pure->module_name);
  loader_pure->module_name = NULL;
  G_OBJECT_CLASS(g_type_class_peek(G_TYPE_OBJECT))->finalize(obj);
}

static void
go_plugin_loader_init(GOPluginLoaderClass *iface)
{
  iface->set_attributes		= gplp_set_attributes;
  iface->load_base		= gplp_load_base;
  iface->service_load		= gplp_service_load;
  iface->service_unload		= gplp_service_unload;
}

static void
gplp_class_init (GObjectClass *gobject_class)
{
  gobject_class->finalize = gplp_finalize;
}

static void
gplp_init (GnmPurePluginLoader *loader_pure)
{
  g_return_if_fail (IS_GNM_PURE_PLUGIN_LOADER (loader_pure));
  loader_pure->module_name = NULL;
}

GSF_DYNAMIC_CLASS_FULL(GnmPurePluginLoader, gnm_pure_plugin_loader,
		       NULL, NULL, gplp_class_init, NULL,
		       gplp_init, G_TYPE_OBJECT, 0,
		       GSF_INTERFACE_FULL(gnm_pure_plugin_loader_type,
					  go_plugin_loader_init,
					  GO_PLUGIN_LOADER_TYPE))
