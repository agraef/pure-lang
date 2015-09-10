
#include "pure-loader.h"

#include <pure/runtime.h>

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
#ifndef _WIN32
#include <sys/wait.h>
#endif
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

/* Gnumeric 1.12.21+? */
#ifndef IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP
#define IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNM_PLUGIN_SERVICE_FUNCTION_GROUP_TYPE))
#define PluginServiceFunctionGroupCallbacks GnmPluginServiceFunctionGroupCallbacks
#endif

static pure_interp *interp;

typedef struct {
  GObject base;
  gchar* module_name;
} GnmPurePluginLoader;
typedef GObjectClass GnmPurePluginLoaderClass;

static const char help_template_text[] =
  "This Pure function hasn't been documented.";

static GnmFuncHelp help_template[] = {
  { GNM_FUNC_HELP_NAME, NULL },
  { GNM_FUNC_HELP_DESCRIPTION, NULL },
  { GNM_FUNC_HELP_END }
};

static void pure_init_help_consts(void)
{
  pure_def(pure_sym("GNM_FUNC_HELP_NAME"), pure_int(GNM_FUNC_HELP_NAME));
  pure_def(pure_sym("GNM_FUNC_HELP_ARG"), pure_int(GNM_FUNC_HELP_ARG));
  pure_def(pure_sym("GNM_FUNC_HELP_DESCRIPTION"), pure_int(GNM_FUNC_HELP_DESCRIPTION));
  pure_def(pure_sym("GNM_FUNC_HELP_NOTE"), pure_int(GNM_FUNC_HELP_NOTE));
  pure_def(pure_sym("GNM_FUNC_HELP_EXAMPLES"), pure_int(GNM_FUNC_HELP_EXAMPLES));
  pure_def(pure_sym("GNM_FUNC_HELP_SEEALSO"), pure_int(GNM_FUNC_HELP_SEEALSO));
#ifndef OLD_API
  pure_def(pure_sym("GNM_FUNC_HELP_EXTREF"), pure_int(GNM_FUNC_HELP_EXTREF));
  pure_def(pure_sym("GNM_FUNC_HELP_EXCEL"), pure_int(GNM_FUNC_HELP_EXCEL));
  pure_def(pure_sym("GNM_FUNC_HELP_ODF"), pure_int(GNM_FUNC_HELP_ODF));
#endif
  pure_let(pure_sym("gnm_version"), pure_string_dup(GNM_VERSION_FULL));
  pure_let(pure_sym("gnm_pure_version"), pure_string_dup(PLUGIN_VERSION));
}

static GnmFuncHelp *pure_default_gnm_help(const char *name)
{
  GnmFuncHelp *help = g_new(GnmFuncHelp, 3);
  if (help) {
    int i;
    for (i = 0; i < 3; i++)
      help[i] = help_template[i];
    help[0].text = g_strdup_printf("%s:", name);
    help[1].text = g_strdup(help_template_text);
  }
  return help;
}

static inline bool is_pair(pure_expr *x, pure_expr **y, pure_expr **z)
{
  pure_expr *f;
  size_t n;
  if (pure_is_appv(x, &f, &n, NULL) && n==2 &&
      strcmp(pure_sym_pname(f->tag), "=>") == 0) {
    if (y) *y = x->data.x[0]->data.x[1];
    if (z) *z = x->data.x[1];
    return true;
  } else
    return false;
}

static GnmFuncHelp *pure_get_gnm_help(pure_expr *x)
{
  size_t i, j, n;
  pure_expr **xv = NULL, *a, *b;
  GnmFuncHelp *help = NULL;
  if (pure_is_listv(x, &n, &xv)) {
    if (n == 0) return NULL;
    help = g_new(GnmFuncHelp, n+1);
    for (i = 0, j = 0; i < n; i++) {
      int32_t iv;
      const char *s;
      // We quietly drop entries which are not in the right format.
      if (is_pair(xv[i], &a, &b) &&
	  pure_is_int(a, &iv) && iv >= GNM_FUNC_HELP_NAME &&
#ifdef OLD_API
	  iv <= GNM_FUNC_HELP_SEEALSO &&
#else
	  iv <= GNM_FUNC_HELP_ODF &&
#endif
	  pure_is_string(b, &s)) {
	help[j].type = iv;
	help[j].text = g_strdup(s);
	j++;
      }
    }
    help[j].type = GNM_FUNC_HELP_END;
    help[j].text = NULL;
    if (j == 0) {
      g_free(help);
      help = NULL;
    }
  }
  if (xv) free(xv);
  return help;
}

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
  for (i = 0; i < argc; i++) value_release(args[i]);
  g_free(args);
  return v;
}

static GnmDependentFlags func_link(GnmFuncEvalInfo *ei, gboolean qlink);

static gboolean
gplp_func_desc_load(GOPluginService *service,
		    char const *name,
		    GnmFuncDescriptor *res)
{
  pure_expr *descfun = pure_symbol(pure_sym("gnm_info"));
  pure_expr *desc = pure_app(descfun, pure_string_dup(name));
  gchar *arg_spec = NULL;
  GnmFuncHelp *help = NULL;

  g_return_val_if_fail(interp!=NULL,FALSE);

  if (desc) {
    size_t size;
    pure_expr **elems = NULL;
    const char *spec = NULL;
    if (pure_is_tuplev(desc, &size, &elems) && size>0 && size<=2 &&
	(size == 1)
	? (pure_is_string(elems[0], &spec) ||
	   (help = pure_get_gnm_help(elems[0])))
	: (pure_is_string(elems[0], &spec) &&
	   (help = pure_get_gnm_help(elems[1])))) {
      if (spec) {
	/* Gnumeric doesn't like it if we have bad type letters in these
	   specs, so better check them here. XXXFIXME: 'a' and 'B' are listed
	   in the documentation but don't actually seem to be supported!? */
	int i, n = strlen(spec), count = 0;
	char *s = alloca(n+1), *t = s;
	for (i = 0; i < n; i++)
	  if (spec[i] == '|') {
	    if (count++ == 0) *(t++) = '|';
	  } else if (strchr("bfsSErA?", spec[i]))
	    *(t++) = spec[i];
	  else
	    *(t++) = '?';
	*t = '\0';
	arg_spec = g_strdup(s);
      }
    }
    if (elems) free(elems);
    pure_freenew(desc);
  }

  /* It's a good idea to JIT the function here, beforehand, so that it's ready
     to go when the user entered his formula. Also, if the function is only
     invoked in background tasks it may otherwise be compiled over and over
     again which is wasteful. */
  pure_interp_compile(interp, pure_sym(name));

  res->name = g_strdup(name);
  res->arg_spec = arg_spec;
  if (!help) help = pure_default_gnm_help(name);
  res->help = help;
  res->fn_args = NULL;
  res->fn_nodes = NULL;
  if (arg_spec)
    res->fn_args = &call_pure_function_args;
  else
    res->fn_nodes = &call_pure_function_nodes;
  res->linker = func_link;
  res->impl_status = GNM_FUNC_IMPL_STATUS_UNIQUE_TO_GNUMERIC;
  res->test_status = GNM_FUNC_TEST_STATUS_UNKNOWN;

  return TRUE;
}

static void
gplp_set_attributes(GOPluginLoader *loader, GHashTable *attrs,
		    GOErrorInfo **ret_error)
{
  GnmPurePluginLoader *loader_pure = GNM_PURE_PLUGIN_LOADER(loader);
  gchar *module_name = NULL;
  GO_INIT_RET_ERROR_INFO(ret_error);
  module_name = g_hash_table_lookup(attrs, "module_name");
  if (module_name)
    loader_pure->module_name = g_strdup(module_name);
  else
    *ret_error = go_error_info_new_str(_("** Pure module name not given."));
}

static GList *modnames;

static void
gplp_load_base(GOPluginLoader *loader, GOErrorInfo **ret_error)
{
  GnmPurePluginLoader *loader_pure = GNM_PURE_PLUGIN_LOADER(loader);
  const char *modname = loader_pure->module_name;
  const char *dir = go_plugin_get_dir_name(go_plugin_loader_get_plugin(loader));
  const char *homedir = g_getenv("HOME");
  gchar *path;
  if (!homedir) homedir = g_get_home_dir();
  /* We expand ~ here, other non-absolute paths are taken relative to the
     plugin directory. */
  if (strncmp(modname, "~/", 2) == 0)
    path = g_build_filename(homedir, modname+2, NULL);
  else if (g_path_is_absolute(modname))
    path = g_strdup(modname);
  else
    path = g_build_filename(dir, modname, NULL);
  if (g_file_test(path, G_FILE_TEST_EXISTS)) {
    if (!interp) {
      // First invocation, create an interpreter instance.
#ifdef PURE_INCLUDES
      char *argv[] = { "", PURE_INCLUDES, NULL };
      int argc = sizeof(argv)/sizeof(char*)-1;
      interp = pure_create_interp(argc, argv);
#else
      interp = pure_create_interp(0, 0);
#endif
      if (interp) pure_init_help_consts();
    }
    if (!interp)
      *ret_error = go_error_info_new_printf(_("** Error creating Pure interpreter."));
    else {
      gchar *cwd = g_get_current_dir(), *dir = g_path_get_dirname(path);
      gchar *cmdbuf = g_strdup_printf("using \"%s\";\n", path);
      /* Plugin scripts get loaded in the directory of the script, so that
	 they can find other files that they may need. We also record that we
	 have loaded the script so that it can be edited and reloaded later. */
      g_chdir(dir);
      // FIXME: This only prints errors on stderr right now.
      pure_evalcmd(cmdbuf);
      g_chdir(cwd); g_free(cmdbuf); g_free(dir); g_free(cwd);
      modnames = g_list_append(modnames, path);
    }
  } else {
    *ret_error = go_error_info_new_printf(_("** Couldn't find \"%s\"."), path);
  }
}

static void pure_reload_script(gpointer data, gpointer unused)
{
  const char *path = (const char*)data;
  gchar *cwd = g_get_current_dir(), *dir = g_path_get_dirname(path);
  gchar *cmdbuf = g_strdup_printf("using \"%s\";\n", path);
  g_chdir(dir);
  pure_evalcmd(cmdbuf);
  g_chdir(cwd); g_free(cmdbuf); g_free(dir); g_free(cwd);
}

#ifndef _WIN32
static void datasource_reinit(void);
#endif
static void gl_reinit(void);

void pure_stop(GnmAction const *action, WorkbookControl *wbc)
{
#ifndef _WIN32
  if (interp && g_list_first(modnames)) {
    datasource_reinit();
  }
#endif
}

void pure_reload(GnmAction const *action, WorkbookControl *wbc)
{
  if (interp && g_list_first(modnames)) {
#ifdef PURE_INCLUDES
    char *argv[] = { "", PURE_INCLUDES, NULL };
    int argc = sizeof(argv)/sizeof(char*)-1;
#else
    char **argv = NULL;
    int argc = 0;
#endif
#ifndef _WIN32
    datasource_reinit();
#endif
    gl_reinit();
    pure_delete_interp(interp);
    interp = pure_create_interp(argc, argv);
    pure_switch_interp(interp);
    if (interp) pure_init_help_consts();
    g_list_foreach(modnames, pure_reload_script, NULL);
  }
}

static void pure_edit_script(gpointer data, gpointer user_data)
{
  const char *path = (const char*)data;
  gchar **files = (gchar**)user_data;
  gchar *new_files = *files ? g_strdup_printf("%s \"%s\"", *files, path) :
    g_strdup_printf("\"%s\"", path);
  if (*files) g_free(*files);
  *files = new_files;
}

void pure_edit(GnmAction const *action, WorkbookControl *wbc)
{
  GList *current = g_list_last(modnames);
  if (current) {
    const char *editor = getenv("EDITOR");
    gchar *cmdbuf = g_strdup_printf("%s", editor?editor:"emacs");
    g_list_foreach(modnames, pure_edit_script, &cmdbuf);
    cmdbuf = g_realloc(cmdbuf, strlen(cmdbuf)+3);
    if (!cmdbuf) return;
    strcat(cmdbuf, " &");
    if (system(cmdbuf) == -1) perror("system");
    g_free(cmdbuf);
  }
}

/* GnmDependent -> cell item mappings. These are used to keep track of
   asynchronous datasources and OpenGL windows associated with a cell. */

static guint
depkey_hash(DepKey const *k)
{
  return
    (GPOINTER_TO_INT(k->node) << 16) + (GPOINTER_TO_INT(k->dep) << 8) + k->id;
}

static gint
depkey_equal(DepKey const *k1, DepKey const *k2)
{
  return k1->node == k2->node && k1->dep == k2->dep && k1->id == k2->id;
}

#ifndef _WIN32

/* Support for asynchronous data sources (adapted from sample_datasource.c). */

char *pure_async_filename = NULL;
static int pure_async_fd = -1;
static FILE *pure_async_file = NULL;
static guint pure_async_source = 0;
static GHashTable *datasources = NULL;

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnumeric:pure"

static void
update_value(DataSource *ds, pure_expr *x)
{
  Sheet *sheet = ds->key.dep->sheet;
  if (x) {
    if (ds->value) pure_free(ds->value);
    ds->value = pure_new(x);
  }
  dependent_queue_recalc(ds->key.dep);
  if (sheet && workbook_get_recalcmode(sheet->workbook))
    workbook_recalc(sheet->workbook);
#if 0
  {
    char *s = x?str(x):strdup("#N/A");
    fprintf(stderr, "%p-%p-%u <= %s\n", ds->key.node, ds->key.dep, ds->key.id, s);
    free(s);
  }
#endif
}

static gboolean
cb_pure_async_input(GIOChannel *gioc, GIOCondition cond, gpointer ignored)
{
  DepKey key;
  pure_expr *val;
  while (pure_read_blob(pure_async_file, &key, &val)) {
    DataSource *ds = g_hash_table_lookup(datasources, &key);
    if (ds) update_value(ds, val);
  }
  return TRUE;
}

gboolean
pure_async_func_init(const GnmFuncEvalInfo *ei, pure_expr *ex,
		     unsigned id, pure_expr **x)
{
  DataSource *ds = NULL,
    new = { { ei->func_call, ei->pos->dep, id }, NULL, NULL, 0 };
  gboolean ret = FALSE;
  /* If caller wants to be notified of updates */
  if (new.key.node != NULL && new.key.dep != NULL) {
    ds = g_hash_table_lookup(datasources, &new.key);
    if (ds == NULL) {
      ds = g_new(DataSource, 1);
      new.value = NULL; if (ex) new.expr = pure_new(ex);
      *ds = new;
      g_hash_table_insert(datasources, &ds->key, ds);
      ret = TRUE;
    } else if ((ex&&ds->expr)?!same(ex, ds->expr):ex!=ds->expr) {
      /* The initiating expression has changed. Nuke any old process and make
	 sure that we start a new one. */
      if (ds->pid > 0) {
	int status;
	kill(ds->pid, SIGTERM);
	if (waitpid(ds->pid, &status, 0) < 0) perror("waitpid");
	ds->pid = 0;
      }
      if (ds->expr) pure_free(ds->expr);
      if (ex) ds->expr = pure_new(ex);
      if (ds->value) pure_free(ds->value);
      ds->value = NULL;
      ret = TRUE;
    }
#if 0
    if (ret) fprintf(stderr, "new datasource = %p-%p-%u\n",
		     ei->func_call, ei->pos->dep, id);
#endif
#if 0
    if (!ret) fprintf(stderr, "datasource = %p-%p-%u\n",
		      ei->func_call, ei->pos->dep, id);
#endif
  }
  *x = ds->value;
  if (ex) pure_freenew(ex);
  return ret;
}

void
pure_async_func_process(const GnmFuncEvalInfo *ei, unsigned id, int pid)
{
  DepKey key = { ei->func_call, ei->pos->dep, id };
  if (key.node != NULL && key.dep != NULL) {
    DataSource *ds = g_hash_table_lookup(datasources, &key);
    if (ds) ds->pid = pid;
  }
}

void
pure_async_func_stop(const GnmFuncEvalInfo *ei, unsigned id)
{
  DepKey key = { ei->func_call, ei->pos->dep, id };
  DataSource *ds;
  if ((ds = g_hash_table_lookup(datasources, &key)) != NULL) {
#if 0
    fprintf(stderr, "stop datasource = %p-%p-%u [%d]\n",
	    ei->func_call, ei->pos->dep, ds->key.id, ds->pid);
#endif
    if (ds->pid > 0) {
      // get rid of the inferior process
      int status;
      kill(ds->pid, SIGTERM);
      if (waitpid(ds->pid, &status, 0) < 0) perror("waitpid");
    }
    if (ds->value) pure_free(ds->value);
    if (ds->expr) pure_free(ds->expr);
    g_hash_table_remove(datasources, &key);
    g_free(ds);
  }
}

void
pure_async_set_value(const GnmFuncEvalInfo *ei, unsigned id, pure_expr *x)
{
  DepKey key = { ei->func_call, ei->pos->dep, id };
  DataSource *ds;
  if ((ds = g_hash_table_lookup(datasources, &key)) != NULL) {
#if 0
    fprintf(stderr, "change datasource = %p-%p-%u [%d]\n",
	    ei->func_call, ei->pos->dep, ds->key.id, ds->pid);
#endif
    if (x != ds->value) {
      if (ds->value) pure_free(ds->value);
      ds->value = pure_new(x);
    }
  }
}

static void
cb_datasource_fini(gpointer key, gpointer value, gpointer closure)
{
  DataSource *ds = value;
#if 0
  fprintf(stderr, "delete datasource = %p-%p-%u [%d]\n",
	  ds->key.node, ds->key.dep, ds->key.id, ds->pid);
#endif
  if (ds->pid > 0) {
    // get rid of the inferior process
    int status;
    kill(ds->pid, SIGTERM);
    if (waitpid(ds->pid, &status, 0) < 0) perror("waitpid");
  }
  if (ds->value) pure_free(ds->value);
  if (ds->expr) pure_free(ds->expr);
  g_free(ds);
}

static void
datasource_reinit(void)
{
  if (datasources) {
    g_hash_table_foreach(datasources, cb_datasource_fini, NULL);
    g_hash_table_destroy(datasources);
  }
  datasources = g_hash_table_new((GHashFunc)depkey_hash,
				 (GEqualFunc)depkey_equal);
}

static void
datasource_init(void)
{
  GIOChannel *channel = NULL;
  char *filename = NULL, nambuf[L_tmpnam];
  // Set up a pipe for asynchronous data processing.
  if (pure_async_fd >= 0) return; // already initialized
#if 0
  fprintf(stderr, ">>>>>>>>>>>>>>>>>>>>>>>>>>>> LOAD PURE_ASYNC\n");
#endif
  if (tmpnam(nambuf) && (filename = g_strdup(nambuf)) &&
    unlink(filename) <= 0 &&
    mkfifo(filename, S_IRUSR | S_IWUSR) == 0) {
    pure_async_filename = filename;
    pure_async_fd = g_open(pure_async_filename, O_RDWR|O_NONBLOCK, 0);
  } else if (filename)
    g_free(filename);
  if (pure_async_fd >= 0) {
    pure_async_file = fdopen(pure_async_fd, "rb");
    channel = g_io_channel_unix_new(pure_async_fd);
    pure_async_source =
      g_io_add_watch(channel, G_IO_IN | G_IO_ERR | G_IO_HUP | G_IO_NVAL,
		     cb_pure_async_input, NULL);
    g_io_channel_unref(channel);
  }
  datasources = g_hash_table_new((GHashFunc)depkey_hash,
				 (GEqualFunc)depkey_equal);
}

static void
datasource_fini(void)
{
#if 0
  fprintf(stderr, "UNLOAD PURE_ASYNC >>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
#endif
  if (pure_async_source) {
    g_source_remove(pure_async_source);
    pure_async_source = 0;
  }
  if (pure_async_filename) {
    g_unlink(pure_async_filename);
    g_free(pure_async_filename);
    pure_async_filename = NULL;
  }
  if (pure_async_fd >= 0) {
    close(pure_async_fd);
    pure_async_fd = -1;
  }
  if (pure_async_file != NULL) {
    fclose(pure_async_file);
    pure_async_file = NULL;
  }
  if (datasources) g_hash_table_destroy(datasources);
  datasources = NULL;
}

#endif

/****************************************************************************/

static GHashTable *gl_windows = NULL;

static void gl_init(void)
{
  /* Initialize the hash table. */
  gl_windows = g_hash_table_new((GHashFunc)depkey_hash,
				(GEqualFunc)depkey_equal);
  if (!gl_windows) g_assert_not_reached();
}

static void
cb_gl_fini(gpointer key, gpointer value, gpointer closure)
{
  GLWindow *glw = value;
  if (glw->timeout > 0) g_source_remove(glw->timer_id);
  if (glw->setup_cb) pure_free(glw->setup_cb);
  if (glw->config_cb) pure_free(glw->config_cb);
  if (glw->display_cb) pure_free(glw->display_cb);
  if (glw->timer_cb) pure_free(glw->timer_cb);
  if (glw->user_data) pure_free(glw->user_data);
  if (glw->name) free(glw->name);
  if (glw->windows) g_slist_free(glw->windows);
  g_free(glw);
}

static void gl_fini(void)
{
  /* Destroy the hash table and all related resources. */
  if (gl_windows) {
    g_hash_table_foreach(gl_windows, cb_gl_fini, NULL);
    g_hash_table_destroy(gl_windows);
  }
  gl_windows = NULL;
}

static void
cb_gl_refini(gpointer key, gpointer value, gpointer closure)
{
  GLWindow *glw = value;
  if (glw->timeout > 0) g_source_remove(glw->timer_id);
  if (glw->setup_cb) pure_free(glw->setup_cb);
  if (glw->config_cb) pure_free(glw->config_cb);
  if (glw->display_cb) pure_free(glw->display_cb);
  if (glw->timer_cb) pure_free(glw->timer_cb);
  if (glw->user_data) pure_free(glw->user_data);
  if (glw->name) free(glw->name);
  glw->setup_cb = glw->config_cb = glw->display_cb = glw->timer_cb =
    glw->user_data = NULL;
  glw->timeout = glw->timer_id = 0;
  /* Get rid of the GL windows. */
  if (glw->windows) {
    GSList *l;
    glw->being_destroyed = TRUE;
    for (l = glw->windows; l; l = l->next) {
      GtkWidget *w = GTK_WIDGET(l->data);
      GtkWidget *drawing_area = gtk_bin_get_child(GTK_BIN(w));
#if 0
      fprintf(stderr, "removing view %p\n", w);
#endif
      gtk_widget_destroy(drawing_area);
    }
    glw->being_destroyed = FALSE;
    g_slist_free(glw->windows);
  }
  g_free(glw);
}

static void gl_reinit(void)
{
  if (gl_windows) {
    g_hash_table_foreach(gl_windows, cb_gl_refini, NULL);
    g_hash_table_destroy(gl_windows);
  }
  gl_init();
}

GLWindow *pure_get_gl_window(const DepKey *key)
{
  return g_hash_table_lookup(gl_windows, key);
}

void pure_add_gl_window(const DepKey *key, GLWindow *glw)
{
  glw->key = *key;
  g_hash_table_insert(gl_windows, &glw->key, glw);
}

void pure_remove_gl_window(const DepKey *key)
{
  GLWindow *glw = g_hash_table_lookup(gl_windows, key);
  if (glw) {
    g_hash_table_remove(gl_windows, key);
    cb_gl_fini(&glw->key, glw, NULL);
  }
}

/****************************************************************************/

/* NOTE: Unfortunately, this interface was changed in rev. 90c2eaa3 of
   Gnumeric (2012-07-21 "Dependencies: reduce code duplication."). There used
   to be two separate link and unlink functions which were merged into
   one. Adjusted to the new interface 20130901 ag; this code thus won't
   compile with older Gnumeric versions (<1.11) any more. */

static GnmDependentFlags
func_link(GnmFuncEvalInfo *ei, gboolean qlink)
{
  if (qlink) {
  } else {
    DepKey key = { ei->func_call, ei->pos->dep, 0 };
    GLWindow *glw;
#ifndef _WIN32
    DataSource *ds;
    while ((ds = g_hash_table_lookup(datasources, &key)) != NULL) {
#if 0
      fprintf(stderr, "delete datasource = %p-%p-%u [%d]\n",
        ei->func_call, ei->pos->dep, ds->key.id, ds->pid);
#endif
      if (ds->pid > 0) {
        // get rid of the inferior process
        int status;
	kill(ds->pid, SIGTERM);
	if (waitpid(ds->pid, &status, 0) < 0) perror("waitpid");
      }
      if (ds->value) pure_free(ds->value);
      if (ds->expr) pure_free(ds->expr);
      g_free(ds);
      key.id++;
    }
    key.id = 0;
#endif
    while ((glw = g_hash_table_lookup(gl_windows, &key)) != NULL) {
      GSList *l = glw->windows;
      if (glw->timeout > 0) g_source_remove(glw->timer_id);
      glw->timer_id = 0; glw->timeout = 0;
      glw->being_destroyed = TRUE;
      for (; l; l = l->next) {
        GtkWidget *w = GTK_WIDGET(l->data);
	GtkWidget *drawing_area = gtk_bin_get_child(GTK_BIN(w));
	/* Destroy the GL window. */
#if 0
	fprintf(stderr, "delete GL window = %p-%p-%u [%p]\n",
                ei->func_call, ei->pos->dep, glw->key.id, drawing_area);
#endif
	gtk_widget_destroy(drawing_area);
      }
      pure_remove_gl_window(&key);
      key.id++;
    }
  }
  return DEPENDENT_NO_FLAG;
}

/****************************************************************************/

static void
gplp_load_service_function_group(GOPluginLoader *loader,
				 GOPluginService *service,
				 GOErrorInfo **ret_error)
{
  PluginServiceFunctionGroupCallbacks *cbs;
  g_return_if_fail (IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP (service));
  GO_INIT_RET_ERROR_INFO (ret_error);
  cbs = go_plugin_service_get_cbs(service);
  cbs->func_desc_load = &gplp_func_desc_load;
}

static gboolean
gplp_service_load(GOPluginLoader *l, GOPluginService *s, GOErrorInfo **err)
{
  if (IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP(s))
    gplp_load_service_function_group(l, s, err);
  else
    return FALSE;
#ifndef _WIN32
  datasource_init();
#endif
  gl_init();
  return TRUE;
}

static gboolean
gplp_service_unload(GOPluginLoader *l, GOPluginService *s, GOErrorInfo **err)
{
  if (IS_GNM_PLUGIN_SERVICE_FUNCTION_GROUP(s))
    ;
  else
    return FALSE;
#ifndef _WIN32
  datasource_fini();
#endif
  gl_fini();
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
					  GO_TYPE_PLUGIN_LOADER))
