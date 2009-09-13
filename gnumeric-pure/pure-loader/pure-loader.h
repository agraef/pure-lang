#ifndef GNM_PURE_PLUGIN_LOADER_H
#define GNM_PURE_PLUGIN_LOADER_H

#include <glib-object.h>
#include <pure/runtime.h>

GType gnm_pure_plugin_loader_get_type (void);
void  gnm_pure_plugin_loader_register_type (GTypeModule *module);
void  pure_reload(GnmAction const *action, WorkbookControl *wbc);
void  pure_edit(GnmAction const *action, WorkbookControl *wbc);
extern char *pure_async_filename;
gboolean pure_async_func_init(const GnmFuncEvalInfo *ei, pure_expr *ex,
			      unsigned id, pure_expr **x);
void pure_async_func_process(const GnmFuncEvalInfo *ei, unsigned id, int pid);

#endif /* GNM_PURE_PLUGIN_LOADER_H */
