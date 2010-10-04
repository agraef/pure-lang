
#include <pure/runtime.h>
#include "midifile.h"

pure_expr *mf_new(int format, MidiFileDivisionType_t division, int resolution);
pure_expr *mf_load(char *filename);
bool mf_save(pure_expr *x, char *filename);
bool mf_free(pure_expr *mf);
pure_expr *mf_info(pure_expr *mf);
pure_expr *mf_get_track(pure_expr *mf, int number);
pure_expr *mf_get_tracks(pure_expr *mf);
bool mf_put_track(pure_expr *x, pure_expr *xs);
bool mf_put_tracks(pure_expr *x, pure_expr *xs);
