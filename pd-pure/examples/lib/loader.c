
/* Generic loader code for batch-compiled pd-pure objects. This is to be
   linked against the batch-compiled Pure module to produce a shared library
   object which can be loaded by Pd. */

#include <pure/runtime.h>
#include <m_pd.h>

/* This is just an example. Adjust these for your object library as needed. */
#define LOADER_SETUP test_setup
static const char *loader_name = "test";
static const char *classes[] = {"add", "counter", NULL};

/* This is the main entry point in the batch-compiled Pure module. */
extern void __pure_main__(int argc, char** argv);
/* This is defined in pd-pure (requires pd-pure 0.15 or later). */
extern int pure_register_class(const char *name, pure_interp *interp);

extern void LOADER_SETUP(void)
{
  pure_interp *interp, *s_interp = pure_current_interp();
  /* Note that we should already have an interpreter instance running here. If
     not then this indicates that pd-pure isn't loaded and we bail out with an
     error. */
  if (s_interp) {
    /* This creates a Pure interpreter for the module and runs the
       initialization code of the included scripts, if any. */
    __pure_main__(0, 0);
    /* Save the interpreter instance we just created. */
    interp = pure_current_interp();
    /* Switch back to the default pd-pure interpreter. */
    pure_switch_interp(s_interp);
    if (interp) {
      /* Register our object classes with pd-pure. */
      bool ok = true;
      const char **c;
      for (c = classes; *c; c++) {
	if (!pure_register_class(*c, interp)) {
	  ok = false;
	  error("%s: failed to register class %s", loader_name, *c);
	}
      }
      if (ok) post("%s: registered with pd-pure", loader_name);
    } else
      error("%s: failed to load module", loader_name);
  } else
    error("%s: pd-pure not loaded", loader_name);
}
