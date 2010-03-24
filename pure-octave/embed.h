#ifndef OCTAVE_EMBED_H__
#define OCTAVE_EMBED_H__

#include <pure/runtime.h>

#ifdef __cplusplus
extern "C" {
#endif

extern void octave_init(int argc, char *argv[]);
extern void octave_fini(void);
extern int octave_eval(const char *cmd);
extern pure_expr *octave_call(const char *fun, int nargout, pure_expr *args);
extern pure_expr *octave_get(const char *id);
extern pure_expr *octave_set(const char *id, pure_expr *x);

#ifdef __cplusplus
}
#endif

#endif

