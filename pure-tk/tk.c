
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>
#include <unistd.h>

#include <tcl.h>
#include <tk.h>
#include <pure/runtime.h>

/* The Tcl interpreter. */

static Tcl_Interp* interp;

/* Handle X11 protocol errors */

static int XErrorProc(ClientData data, XErrorEvent *errEventPtr)
{
  fprintf(stderr, "X protocol error: ");
  fprintf(stderr, "error=%d request=%d minor=%d\n",
	  errEventPtr->error_code, errEventPtr->request_code,
	  errEventPtr->minor_code);
  return 0;
}

/* Helper functions for error handling. */

static inline void set_result(char **result, const char *s)
{
  *result = malloc(strlen(s)+1);
  if (*result) strcpy(*result, s);
}

/* Construct an error term. tk_error is supposed to be defined by the
   application in order to implement any desired error handling. */

static inline pure_expr *tk_error(char *result)
{
  if (result)
    return pure_app(pure_symbol(pure_sym("tk_error")), pure_string(result));
  else
    return 0;
}

/* Tcl command to invoke Pure callbacks. */

static int tk_pure(ClientData clientData,
		   Tcl_Interp *interp,
		   int argc, char **argv)
{
  pure_expr *x, *e = NULL;
  const char *s;
  int i, fno;

  Tcl_ResetResult(interp);
  if (argc < 2) {
    /* Callback is missing. */
    Tcl_AppendResult(interp, "missing callback", NULL);
    return TCL_ERROR;
  }
  fno = pure_getsym(argv[1]);
  if (fno <= 0) {
    /* Not a valid function name, i.e., the callback hasn't been defined yet.
       This isn't an error, so we just return indicating success. */
    return TCL_OK;
  }
  /* Apply the callback to the given arguments. */
  x = pure_symbolx(fno, &e);
  if (x && argc > 2) {
    pure_expr **xv = (pure_expr**)malloc((argc-2)*sizeof(pure_expr*));
    for (i = 2; i < argc; i++)
      xv[i-2] = pure_string_dup(argv[i]);
    x = pure_appxv(x, argc-2, xv, &e);
    free(xv);
  }
  Tcl_ResetResult(interp);
  if (!x) {
    /* Callback raised an exception. */
    if (e) pure_freenew(e);
    Tcl_AppendResult(interp, "callback error", NULL);
    return TCL_ERROR;
  }
  /* If we got a string result, return it. */
  if (pure_is_string(x, &s) && *s)
    Tcl_AppendResult(interp, s, NULL);
  pure_freenew(x);
  /* Callback was executed successfully. */
  return TCL_OK;
}

/* Starting and stopping the Tcl interpreter. */

static void tk_stop(void);

static bool tk_start(char **result)
{
  static bool first_init = false;
  Tk_Window mainw;
  if (!first_init) {
    first_init = true;
    /* this works around a bug in some Tcl/Tk versions */
    Tcl_FindExecutable(NULL);
    /* finalize Tcl at program exit */
    atexit(Tcl_Finalize);
  }
  *result = NULL;
  if (interp) return true;
  /* start up a new interpreter */
  if (!(interp = Tcl_CreateInterp())) return false;
  if (Tcl_Init(interp) != TCL_OK) {
    if (interp->result && *interp->result)
      set_result(result, interp->result);
    else
      set_result(result, "error initializing Tcl");
    tk_stop();
    return false;
  }
  /* create a command to invoke Pure callbacks from Tcl */
  Tcl_CreateCommand(interp, "pure", (Tcl_CmdProc*)tk_pure,
		    (ClientData)0, NULL);
  /* oddly, there are no `env' variables passed, and this one is needed */
  Tcl_SetVar2(interp, "env", "DISPLAY", getenv("DISPLAY"), TCL_GLOBAL_ONLY);
  if (Tk_Init(interp) != TCL_OK) {
    if (interp->result && *interp->result)
      set_result(result, interp->result);
    else
      set_result(result, "error initializing Tk");
    tk_stop();
    return false;
  }
  /* set up an X error handler */
  mainw = Tk_MainWindow(interp);
  Tk_CreateErrorHandler(Tk_Display(mainw), -1, -1, -1,
			XErrorProc, (ClientData)mainw);
  return true;
}

static void tk_stop(void)
{
  if (interp) {
    Tcl_DeleteInterp(interp);
    interp = NULL;
  }
}

/* Process events in the Tcl interpreter. */

static bool do_event(void)
{
  return Tcl_DoOneEvent(TCL_DONT_WAIT);
}

static void tk_do_events(void)
{
  if (!interp) return;
  while (Tk_MainWindow(interp) && do_event()) ;
  if (!Tk_MainWindow(interp)) tk_stop();
}

static bool tk_running(void)
{
  tk_do_events();
  return interp != NULL;
}

/* Evaluate Tcl commands. */

static bool tk_eval(const char *s, char **result)
{
  int status;
  char *cmd;
  *result = NULL;
  if (!interp) return false;
  cmd = malloc(strlen(s)+1);
  if (!cmd) return false;
  strcpy(cmd, s);
  status = Tcl_Eval(interp, cmd);
  if (interp && interp->result && *interp->result)
    set_result(result, interp->result);
  else if (status == TCL_BREAK)
    set_result(result, "invoked \"break\" outside of a loop");
  else if (status == TCL_CONTINUE)
    set_result(result, "invoked \"continue\" outside of a loop");
  if (status == TCL_BREAK || status == TCL_CONTINUE)
    status = TCL_ERROR;
  tk_do_events();
  free(cmd);
  return status != TCL_ERROR;
}

/* Interface functions. */

pure_expr *tk(const char *s)
{
  char *result = NULL;
  if (tk_start(&result)) {
    bool res;
    /* Make sure that we don't pull the rug under ourselves. */
    Tcl_Interp* _interp = interp;
    Tcl_Preserve(_interp);
    res = tk_eval(s, &result);
    Tcl_Release(_interp);
    if (res)
      return (result&&*result)?pure_string(result):pure_tuplel(0);
    else
      return tk_error(result);
  } else
    return tk_error(result);
}

pure_expr *tk_set(const char *s, pure_expr *x)
{
  const char *t;
  if (pure_is_string(x, &t)) {
    char *result = NULL;
    if (tk_start(&result)) {
      const char *res = Tcl_SetVar(interp, s, t, TCL_GLOBAL_ONLY);
      if (res)
	return x;
      else
	return NULL;
    } else
      return tk_error(result);
  } else
    return NULL;
}

pure_expr *tk_unset(const char *s)
{
  char *result = NULL;
  if (tk_start(&result)) {
    int res = Tcl_UnsetVar(interp, s, TCL_GLOBAL_ONLY);
    if (res == TCL_OK)
      return pure_tuplel(0);
    else
      return 0;
  } else
    return tk_error(result);
}

pure_expr *tk_get(const char *s)
{
  char *result = NULL;
  if (tk_start(&result)) {
    const char *res = Tcl_GetVar(interp, s, TCL_GLOBAL_ONLY);
    if (res)
      return pure_string_dup(res);
    else
      return NULL;
  } else
    return tk_error(result);
}

pure_expr *tk_split(const char *s)
{
  int argc, ret;
  const char **argv;
  ret = Tcl_SplitList(NULL, s, &argc, &argv);
  if (ret == TCL_OK) {
    pure_expr *x;
    if (argc <= 0)
      x = pure_listl(0);
    else {
      pure_expr **xv = (pure_expr**)malloc(argc*sizeof(pure_expr*));
      int i;
      for (i = 0; i < argc; i++)
	xv[i] = pure_string_dup(argv[i]);
      x = pure_listv(argc, xv);
      free(xv);
    }
    Tcl_Free((char *)argv);
    return x;
  } else {
    if (argv) Tcl_Free((char *)argv);
    return NULL;
  }
}

pure_expr *tk_join(pure_expr *x)
{
  size_t i, n;
  pure_expr **xv;
  if (pure_is_listv(x, &n, &xv)) {
    char *s, *ret;
    char **argv = (char**)malloc(n*sizeof(char*));
    pure_expr *x;
    for (i = 0; i < n; i++) {
      x = xv[i];
      if (pure_is_string_dup(x, &s))
	argv[i] = s;
      else {
	size_t j;
	for (j = 0; j < i; j++) free(argv[j]);
	free(argv);
	free(xv);
	return NULL;
      }
    }
    free(xv);
    ret = Tcl_Merge(n, (const char**)argv);
    for (i = 0; i < n; i++) free(argv[i]);
    free(argv);
    x = pure_string_dup(ret);
    Tcl_Free(ret);
    return x;
  } else
    return NULL;
}

void tk_quit(void)
{
  tk_stop();
}

bool tk_ready(void)
{
  return tk_running();
}

pure_expr *tk_main(void)
{
  char *result = NULL;
  if (tk_start(&result)) {
    while (interp && Tk_MainWindow(interp) && Tcl_DoOneEvent(0)) ;
    if (interp && !Tk_MainWindow(interp)) tk_stop();
    return pure_tuplel(0);
  } else
    return tk_error(result);
}
