
/* This gives access to the PmDeviceInfo structure, as returned by the
   Pm_GetDeviceInfo routine. */

#include <portmidi.h>
#include "pmdev.h"

pure_expr *pm_device_info(int id)
{
  const PmDeviceInfo *info = Pm_GetDeviceInfo(id);
  if (!info) return 0;
  /* The first field is useless and seems to be bogus anyway, skip it. */
  return pure_tuplel(5,
		     pure_cstring_dup(info->interf),
		     pure_cstring_dup(info->name),
		     pure_int(info->input),
		     pure_int(info->output),
		     pure_int(info->opened));
}
