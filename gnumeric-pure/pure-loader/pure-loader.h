#ifndef GNM_PURE_PLUGIN_LOADER_H
#define GNM_PURE_PLUGIN_LOADER_H

#include <glib-object.h>

GType gnm_pure_plugin_loader_get_type (void);
void  gnm_pure_plugin_loader_register_type (GTypeModule *module);
void  pure_reload(GnmAction const *action, WorkbookControl *wbc);

#endif /* GNM_PURE_PLUGIN_LOADER_H */
