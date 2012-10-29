#ifndef OCTAVE_EMBED_H__
#define OCTAVE_EMBED_H__

#include <pure/runtime.h>

#ifdef __cplusplus
extern "C" {
#endif

extern void octave_init(int argc, char *argv[]);
extern void octave_fini(void);
extern int octave_eval(const char *cmd);
extern pure_expr *octave_call(pure_expr *fun, int nargout, pure_expr *args);
extern pure_expr *octave_get(const char *id);
extern pure_expr *octave_set(const char *id, pure_expr *x);
extern pure_expr *octave_func(pure_expr *fun);
extern bool octave_valuep(pure_expr *x);
extern bool octave_converters(bool enable);
extern void octave_free(void *val);

#ifdef __cplusplus
}
#endif

#endif

