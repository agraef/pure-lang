
#include <pure/runtime.h>
#include <sndfile.h>

const char *sf_get_version(void);

SF_INFO *sf_make_info(int samplerate, int channels, int format);
SF_INFO *sf_new_info(void);
pure_expr *sf_get_info(SF_INFO *info);

SF_FORMAT_INFO *sf_make_format_info(int format);
pure_expr *sf_get_format_info(SF_FORMAT_INFO *info);
