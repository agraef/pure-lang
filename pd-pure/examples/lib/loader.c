
/* Generic loader code for batch-compiled pd-pure objects. This is to be
   linked against the batch-compiled Pure module to produce a shared library
   object which can be loaded by Pd. */

#include <pure/runtime.h>
#include <m_pd.h>

/* This is just an example. Adjust these for your object library as needed.
   If you change the name of the module ('test' in this example), you'll also
   have to adjust the names of the __test_main__ and test_setup entry points
   below and in the Makefile accordingly. */
#define LOADER_MAIN __test_main__
#define LOADER_SETUP test_setup
static const char *loader_name = "test";
static const char *classes[] = {"add", "counter", NULL};

/* This is the main entry point in the batch-compiled Pure module. */
extern void LOADER_MAIN(int argc, char** argv);
/* This is defined in pd-pure (requires pd-pure 0.15 or later). */
extern int pure_register_class(const char *name, pure_interp *interp,
			       const char *help);

extern void LOADER_SETUP(void)
{
  pure_interp *interp, *s_interp = pure_current_interp();
  /* Note that we should already have an interpreter instance running here. If
     not then this indicates that pd-pure isn't loaded and we bail out with an
     error. */
  if (s_interp) {
    /* This creates a Pure interpreter for the module and runs the
       initialization code of the included scripts, if any. */
    LOADER_MAIN(0, 0);
    /* Save the interpreter instance we just created. */
    interp = pure_current_interp();
    /* Switch back to the default pd-pure interpreter. */
    pure_switch_interp(s_interp);
    if (interp) {
      /* Register our object classes with pd-pure. */
      bool ok = true;
      const char **c;
      for (c = classes; *c; c++) {
      /* NOTE: The help argument is always NULL here. You can also specify the
	 absolute path of a help file which should be used instead of the
	 default pd-pure help file for the corresponding object class. */
	if (!pure_register_class(*c, interp, NULL)) {
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
