
/* Generic loader for batch-compiled pd-pure objects. This is to be linked
   against the batch-compiled Pure module to produce a shared library object
   which can be loaded by Pd. */

#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <pure/runtime.h>
#include <m_pd.h>

#ifdef FAUST2
static const char *loader_name = "pd-faust2";
#else
static const char *loader_name = "pd-faust";
#endif
static const char *classes[] = {"fdsp~", "fsynth~", "midiseq", "oscseq", NULL};

#ifndef VERSION
#define VERSION "0.0"
#endif

/* This is defined in the batch-compiled Pure module. */
extern void __pdfaust_main__(int argc, char** argv);
/* This is defined in pd-pure. */
extern const char *pd_libdir(void);
extern int pure_register_class(const char *name, pure_interp *interp,
			       const char *help);

#define HELP "/extra/faust/faust-help.pd"

#ifdef FAUST2
extern void pdfaust2_setup(void)
#else
extern void pdfaust_setup(void)
#endif
{
  pure_interp *interp, *s_interp = pure_current_interp();
  if (s_interp) {
    /* Save the current working directory. */
    char buf[PATH_MAX], *cwd = getcwd(buf, PATH_MAX);
    /* Try to execute this in the installation directory, so that the fdsp~
       and fsynth~ objects find their stuff during initialization. */
    if (chdir(pd_libdir()) || chdir("extra/faust")) cwd && chdir(cwd);
    __pdfaust_main__(0, 0);
    /* Restore the working directory. */
    cwd && chdir(cwd);
    interp = pure_current_interp();
    pure_switch_interp(s_interp);
    if (interp) {
      bool ok = true;
      const char **c;
      int l = strlen(pd_libdir())+strlen(HELP);
      char *help = malloc(l+1);
      strcpy(help, pd_libdir()); strcat(help, HELP);
      post("%s %s (c) 2011-2014 Albert Graef <aggraef@gmail.com>",
	   loader_name, VERSION);
      for (c = classes; *c; c++) {
	if (!pure_register_class(*c, interp, help)) {
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
