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

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <glib/gstdio.h>
#include <glib/gi18n-lib.h>

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

static DependentFlags atl_func_link(GnmFuncEvalInfo *ei);
static void atl_func_unlink(GnmFuncEvalInfo *ei);

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
  res->linker = atl_func_link;
  res->unlinker = atl_func_unlink;
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

/* Support for asynchronous data sources (from sample_datasource.c). */

char *atl_filename = NULL;
static int atl_fd = -1;
static FILE *atl_file = NULL;
static guint atl_source = 0;
static GHashTable *watched_values = NULL;
static GHashTable *watchers = NULL;

typedef struct {
  char *name;
  pure_expr *value;
  GHashTable *deps;
} WatchedValue;

typedef struct {
  GnmExprFunction const *node; /* Expression node that calls us */
  GnmDependent *dep; /* GnmDependent containing that node */
  WatchedValue *value;
  int pid; /* inferior process */
} Watcher;

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnumeric:pure"

static guint
watcher_hash(Watcher const *w)
{
  return (GPOINTER_TO_INT(w->node) << 16) + GPOINTER_TO_INT(w->dep);
}

static gint
watcher_equal(Watcher const *w1, Watcher const *w2)
{
  return w1->node == w2->node && w1->dep == w2->dep;
}

static WatchedValue *
watched_value_fetch(char const *tag)
{
  WatchedValue *val = g_hash_table_lookup(watched_values, tag);
  if (val == NULL) {
    val = g_new(WatchedValue, 1);
    val->name = g_strdup(tag);
    val->value = NULL;
    val->deps = g_hash_table_new(g_direct_hash, g_direct_equal);
    g_hash_table_insert(watched_values, val->name, val);
  }
  return val;
}

static void
cb_watcher_queue_recalc(gpointer key, gpointer value, gpointer closure)
{
  Watcher const *w = key;
  Sheet *sheet = w->dep->sheet;
  dependent_queue_recalc(w->dep);
  if (sheet && workbook_get_recalcmode(sheet->workbook))
    workbook_recalc(sheet->workbook);
}

static gboolean
cb_atl_input(GIOChannel *gioc, GIOCondition cond, gpointer ignored)
{
  char buf[1024]; // XXXFIXME
  while (fgets(buf, sizeof (buf), atl_file) != NULL) {
    char *sym = buf;
    char *value_str = strchr(buf, ':');
    if (value_str) {
      pure_expr *val;
      *value_str++ = '\0';
      val = pure_eval(value_str);
      if (val) {
	WatchedValue *wv = watched_value_fetch(sym);
	if (wv->value) pure_free(wv->value);
	wv->value = pure_new(val);
	g_hash_table_foreach(wv->deps, cb_watcher_queue_recalc, NULL);
#if 0
	fprintf(stderr, "'%s' <= %s", sym, value_str);
#endif
      }
    }
  }
  return TRUE;
}

gboolean
atl_func_init(const GnmFuncEvalInfo *ei, char **name, pure_expr **x)
{
  Watcher key = { ei->func_call, ei->pos->dep, NULL, 0 };
  WatchedValue *val;
  *name = g_strdup_printf("pure-gnumeric-%p-%p", ei->func_call, ei->pos->dep);
  val = watched_value_fetch(*name);
  *x = NULL;
  if (val) {
    gboolean ret = TRUE;
    /* If caller wants to be notified of updates */
    if (key.node != NULL && key.dep != NULL) {
      Watcher *w = g_hash_table_lookup(watchers, &key);
      if (w == NULL) {
	w = g_new(Watcher, 1);
	key.value = val;
	*w = key;
	g_hash_table_insert(watchers, w, w);
	g_hash_table_insert(w->value->deps, w, w);
	ret = FALSE;
      } else if (w->value != val) {
	g_hash_table_remove(w->value->deps, w);
	w->value = val;
	g_hash_table_insert(w->value->deps, w, w);
      }
#if 0
      fprintf(stderr, "%s func_call = %p, dep = %p\n", ret?"old":"new",
	      ei->func_call, ei->pos->dep);
#endif
    }
    if (val->value)
      *x = val->value;
    else
      *x = pure_app(pure_symbol(pure_sym("gnm_error")),
		    pure_string_dup("#N/A"));
    return ret;
  } else
    return TRUE;
}

void
atl_func_process(const GnmFuncEvalInfo *ei, int pid)
{
  Watcher key = { ei->func_call, ei->pos->dep, NULL, 0 };
  if (key.node != NULL && key.dep != NULL) {
    Watcher *w = g_hash_table_lookup(watchers, &key);
    if (w)
      w->pid = pid;
  }
}

static DependentFlags
atl_func_link(GnmFuncEvalInfo *ei)
{
#if 0
  fprintf(stderr, "link atl_func %p\n", ei);
#endif
  return DEPENDENT_ALWAYS_UNLINK;
}

static void
atl_func_unlink(GnmFuncEvalInfo *ei)
{
  Watcher key, *w;
  key.node = ei->func_call;
  key.dep = ei->pos->dep;
  w = g_hash_table_lookup(watchers, &key);
  if (w != NULL) {
    if (w->value != NULL)
      g_hash_table_remove(w->value->deps, w);
#if 0
    fprintf(stderr, "unlink atl_func %p [%d]\n", ei, w->pid);
#endif
    if (w->pid > 0) {
      // get rid of the inferior process
      int status;
      kill(w->pid, SIGTERM);
      if (waitpid(w->pid, &status, 0) < 0) perror("waitpid");
      if (w->value && w->value->value) pure_free(w->value->value);
    }
    g_free(w);
  }
}

static void
watcher_init(void)
{
  GIOChannel *channel = NULL;
  char *filename;
  // Set up a pipe for asynchronous data processing.
#if 0
  fprintf(stderr, ">>>>>>>>>>>>>>>>>>>>>>>>>>>> LOAD ATL\n");
#endif
  g_return_if_fail(atl_fd < 0);
  filename = g_build_filename(g_get_home_dir (), "atl", NULL);
  g_unlink(filename);
  if (mkfifo(filename, S_IRUSR | S_IWUSR) == 0) {
    atl_filename = filename;
    atl_fd = g_open(atl_filename, O_RDWR|O_NONBLOCK, 0);
  } else
    g_free (filename);
  if (atl_fd >= 0) {
    atl_file = fdopen(atl_fd, "rb");
    channel = g_io_channel_unix_new(atl_fd);
    atl_source = g_io_add_watch(channel,
				G_IO_IN | G_IO_ERR | G_IO_HUP | G_IO_NVAL,
				cb_atl_input, NULL);
    g_io_channel_unref(channel);
  }
  watched_values = g_hash_table_new((GHashFunc)g_str_hash,
				    (GEqualFunc)g_str_equal);
  watchers = g_hash_table_new((GHashFunc)watcher_hash,
			      (GEqualFunc)watcher_equal);
}

static void
watcher_fini(void)
{
#if 0
  fprintf(stderr, "UNLOAD ATL >>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
#endif
  if (atl_source) {
    g_source_remove(atl_source);
    atl_source = 0;
  }
  if (atl_filename) {
    g_unlink(atl_filename);
    g_free(atl_filename);
    atl_filename = NULL;
  }
  if (atl_fd >= 0) {
    close(atl_fd);
    atl_fd = -1;
  }
  if (atl_file != NULL) {
    fclose(atl_file);
    atl_file = NULL;
  }
  g_hash_table_destroy(watched_values);
  watched_values = NULL;
  g_hash_table_destroy(watchers);
  watchers = NULL;
}

/****************************************************************************/

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
  watcher_init();
  return TRUE;
}

static gboolean
gplp_service_unload(GOPluginLoader *l, GOPluginService *s, ErrorInfo **err)
{
  if (IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP (s))
    ;
  else
    return FALSE;
  watcher_fini();
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
