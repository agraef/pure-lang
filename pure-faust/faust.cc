
/* Copyright (c) 2009, Albert Graef

   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

   3. Neither the name of the author nor the names of its contributors may be
      used to endorse or promote products derived from this software without
      specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE. */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ltdl.h>

#include <pure/runtime.h>

/* The Faust UI and dsp classes. */

class UI
{
  bool	fStopped;
public:
		
  UI() : fStopped(false) {}
  virtual ~UI() {}

  virtual void addButton(const char* label, double* zone) = 0;
  virtual void addToggleButton(const char* label, double* zone) = 0;
  virtual void addCheckButton(const char* label, double* zone) = 0;
  virtual void addVerticalSlider(const char* label, double* zone, float init, float min, float max, float step) = 0;
  virtual void addHorizontalSlider(const char* label, double* zone, float init, float min, float max, float step) = 0;
  virtual void addNumEntry(const char* label, double* zone, float init, float min, float max, float step) = 0;

  virtual void addNumDisplay(const char* label, double* zone, int precision) = 0;
  virtual void addTextDisplay(const char* label, double* zone, char* names[], float min, float max) = 0;
  virtual void addHorizontalBargraph(const char* label, double* zone, float min, float max) = 0;
  virtual void addVerticalBargraph(const char* label, double* zone, float min, float max) = 0;
	
  virtual void openFrameBox(const char* label) = 0;
  virtual void openTabBox(const char* label) = 0;
  virtual void openHorizontalBox(const char* label) = 0;
  virtual void openVerticalBox(const char* label) = 0;
  virtual void closeBox() = 0;
	
  virtual void run() = 0;
	
  void stop()	{ fStopped = true; }
  bool stopped() 	{ return fStopped; }

  virtual void declare(double* zone, const char* key, const char* value) {}
};

/***************************************************************************
   Pure UI interface
 ***************************************************************************/

enum ui_elem_type_t {
  UI_BUTTON, UI_TOGGLE_BUTTON, UI_CHECK_BUTTON,
  UI_V_SLIDER, UI_H_SLIDER, UI_NUM_ENTRY,
  UI_V_BARGRAPH, UI_H_BARGRAPH,
  UI_END_GROUP, UI_V_GROUP, UI_H_GROUP, UI_T_GROUP
};

struct ui_elem_t {
  ui_elem_type_t type;
  const char *label;
  double *zone;
  float init, min, max, step;
};

class PureUI : public UI
{
public:
  int nelems;
  ui_elem_t *elems;
		
  PureUI();
  virtual ~PureUI();

protected:
  void add_elem(ui_elem_type_t type, const char *label = NULL);
  void add_elem(ui_elem_type_t type, const char *label, double *zone);
  void add_elem(ui_elem_type_t type, const char *label, double *zone,
		float init, float min, float max, float step);
  void add_elem(ui_elem_type_t type, const char *label, double *zone,
		float min, float max);

public:
  virtual void addButton(const char* label, double* zone);
  virtual void addToggleButton(const char* label, double* zone);
  virtual void addCheckButton(const char* label, double* zone);
  virtual void addVerticalSlider(const char* label, double* zone, float init, float min, float max, float step);
  virtual void addHorizontalSlider(const char* label, double* zone, float init, float min, float max, float step);
  virtual void addNumEntry(const char* label, double* zone, float init, float min, float max, float step);

  virtual void addNumDisplay(const char* label, double* zone, int precision);
  virtual void addTextDisplay(const char* label, double* zone, char* names[], float min, float max);
  virtual void addHorizontalBargraph(const char* label, double* zone, float min, float max);
  virtual void addVerticalBargraph(const char* label, double* zone, float min, float max);
  
  virtual void openFrameBox(const char* label);
  virtual void openTabBox(const char* label);
  virtual void openHorizontalBox(const char* label);
  virtual void openVerticalBox(const char* label);
  virtual void closeBox();
	
  virtual void run();
};

PureUI::PureUI()
{
  nelems = 0;
  elems = NULL;
}

PureUI::~PureUI()
{
  if (elems) free(elems);
}

inline void PureUI::add_elem(ui_elem_type_t type, const char *label)
{
  ui_elem_t *elems1 = (ui_elem_t*)realloc(elems, (nelems+1)*sizeof(ui_elem_t));
  if (elems1)
    elems = elems1;
  else
    return;
  elems[nelems].type = type;
  elems[nelems].label = label;
  elems[nelems].zone = NULL;
  elems[nelems].init = 0.0;
  elems[nelems].min = 0.0;
  elems[nelems].max = 0.0;
  elems[nelems].step = 0.0;
  nelems++;
}

inline void PureUI::add_elem(ui_elem_type_t type, const char *label, double *zone)
{
  ui_elem_t *elems1 = (ui_elem_t*)realloc(elems, (nelems+1)*sizeof(ui_elem_t));
  if (elems1)
    elems = elems1;
  else
    return;
  elems[nelems].type = type;
  elems[nelems].label = label;
  elems[nelems].zone = zone;
  elems[nelems].init = 0.0;
  elems[nelems].min = 0.0;
  elems[nelems].max = 0.0;
  elems[nelems].step = 0.0;
  nelems++;
}

inline void PureUI::add_elem(ui_elem_type_t type, const char *label, double *zone,
			     float init, float min, float max, float step)
{
  ui_elem_t *elems1 = (ui_elem_t*)realloc(elems, (nelems+1)*sizeof(ui_elem_t));
  if (elems1)
    elems = elems1;
  else
    return;
  elems[nelems].type = type;
  elems[nelems].label = label;
  elems[nelems].zone = zone;
  elems[nelems].init = init;
  elems[nelems].min = min;
  elems[nelems].max = max;
  elems[nelems].step = step;
  nelems++;
}

inline void PureUI::add_elem(ui_elem_type_t type, const char *label, double *zone,
			     float min, float max)
{
  ui_elem_t *elems1 = (ui_elem_t*)realloc(elems, (nelems+1)*sizeof(ui_elem_t));
  if (elems1)
    elems = elems1;
  else
    return;
  elems[nelems].type = type;
  elems[nelems].label = label;
  elems[nelems].zone = zone;
  elems[nelems].init = 0.0;
  elems[nelems].min = min;
  elems[nelems].max = max;
  elems[nelems].step = 0.0;
  nelems++;
}

void PureUI::addButton(const char* label, double* zone)
{ add_elem(UI_BUTTON, label, zone); }
void PureUI::addToggleButton(const char* label, double* zone)
{ add_elem(UI_TOGGLE_BUTTON, label, zone); }
void PureUI::addCheckButton(const char* label, double* zone)
{ add_elem(UI_CHECK_BUTTON, label, zone); }
void PureUI::addVerticalSlider(const char* label, double* zone, float init, float min, float max, float step)
{ add_elem(UI_V_SLIDER, label, zone, init, min, max, step); }
void PureUI::addHorizontalSlider(const char* label, double* zone, float init, float min, float max, float step)
{ add_elem(UI_H_SLIDER, label, zone, init, min, max, step); }
void PureUI::addNumEntry(const char* label, double* zone, float init, float min, float max, float step)
{ add_elem(UI_NUM_ENTRY, label, zone, init, min, max, step); }

// FIXME: addNumDisplay and addTextDisplay not implemented in Faust yet?
void PureUI::addNumDisplay(const char* label, double* zone, int precision) {}
void PureUI::addTextDisplay(const char* label, double* zone, char* names[], float min, float max) {}
void PureUI::addHorizontalBargraph(const char* label, double* zone, float min, float max)
{ add_elem(UI_H_BARGRAPH, label, zone, min, max); }
void PureUI::addVerticalBargraph(const char* label, double* zone, float min, float max)
{ add_elem(UI_V_BARGRAPH, label, zone, min, max); }

void PureUI::openFrameBox(const char* label) {}
void PureUI::openTabBox(const char* label)
{ add_elem(UI_T_GROUP, label); }
void PureUI::openHorizontalBox(const char* label)
{ add_elem(UI_H_GROUP, label); }
void PureUI::openVerticalBox(const char* label)
{ add_elem(UI_V_GROUP, label); }
void PureUI::closeBox()
{ add_elem(UI_END_GROUP); }

void PureUI::run() {}
			
class dsp {
 protected:
	int fSamplingFreq;
 public:
	dsp() {}
	virtual ~dsp() {}
	virtual int getNumInputs() = 0;
	virtual int getNumOutputs() = 0;
	virtual void buildUserInterface(UI* interface) = 0;
	virtual void init(int samplingRate) = 0;
 	virtual void compute(int len, double** inputs, double** outputs) = 0;
};

/* Interface of a Faust module compiled with the pure.cpp architecture file. */

typedef dsp *(*newdspfun)();
typedef void (*deldspfun)(dsp*);

/* The Faust DSP proxy. This structure keeps all the necessary information,
   including the handle of the module from which the DSP gets loaded and the
   DSP object itself. */

struct faust_t {
  int *refc;
  lt_dlhandle h;
  newdspfun newdsp;
  deldspfun deldsp;
  int rate;
  dsp *d;
  PureUI *ui;
  double **inbuf, **outbuf;
  int nsamples;
};

/* Initialization and finalization. */

static void ltdl_init()
{
  static bool init = false;
  if (!init) {
    /* Initialize libltdl. If that fails then bail out. FIXME: this is rather
       unfriendly, so we should probably just set a flag here. */
    if (lt_dlinit()) {
      fprintf(stderr, "error initializing ltdl!\n");
      exit(1);
    }
    atexit((void(*)())lt_dlexit);
    init = true;
  }
}

static void init_bufs(faust_t *fd)
{
  // Make sure that all labels are properly initialized.
  for (int i = 0; i < fd->ui->nelems; i++) {
    if (!fd->ui->elems[i].label)
      fd->ui->elems[i].label = "";
  }
  // Intialize the audio buffers.
  int n = fd->d->getNumInputs();
  if (n > 0) {
    fd->inbuf = (double**)malloc(n*sizeof(double*));
    assert(fd->inbuf);
    for (int i = 0; i < n; i++)
      fd->inbuf[i] = NULL;
  }
  int m = fd->d->getNumOutputs();
  if (m > 0) {
    fd->outbuf = (double**)malloc(m*sizeof(double*));
    assert(fd->outbuf);
    for (int i = 0; i < m; i++)
      fd->outbuf[i] = NULL;
  }
}

/* Interface operations. */

extern "C"
void faust_exit(faust_t *fd)
{
  ltdl_init();
  if (!fd) return;
  if (fd->d) fd->deldsp(fd->d);
  if (fd->ui) delete fd->ui;
  if (fd->inbuf) free(fd->inbuf);
  if (fd->outbuf) free(fd->outbuf);
  if (fd->refc && --(*fd->refc) == 0) {
    if (fd->h) lt_dlclose(fd->h);
    free(fd->refc);
  }
  free(fd);
}

extern "C"
faust_t *faust_init(const char *name, int rate)
{
  faust_t *fd = (faust_t*)malloc(sizeof(faust_t));
  char *path;
  if (!fd || !name) {
    if (fd) free(fd);
    return NULL;
  }
  fd->refc = NULL;
  fd->h = NULL;
  fd->newdsp = NULL;
  fd->deldsp = NULL;
  fd->rate = 0;
  fd->d = NULL;
  fd->ui = NULL;
  fd->inbuf = fd->outbuf = NULL;
  fd->nsamples = 0;
  fd->refc = (int*)malloc(sizeof(int));
  if (!fd->refc) goto error;
  *fd->refc = 1;
  /* Make sure that we search the current directory for modules. */
  ltdl_init();
  path = lt_dlgetsearchpath()?strdup(lt_dlgetsearchpath()):NULL;
  lt_dlsetsearchpath(".");
  fd->h = lt_dlopenext(name);
  if (path) {
    lt_dlsetsearchpath(path);
    free(path);
  }
  if (!fd->h) goto error;
  fd->newdsp = (newdspfun)lt_dlsym(fd->h, "newdsp");
  fd->deldsp = (deldspfun)lt_dlsym(fd->h, "deldsp");
  if (!fd->newdsp || !fd->deldsp)
    goto error;
  fd->d = fd->newdsp();
  if (!fd->d) goto error;
  fd->d->init(rate);
  fd->rate = rate;
  fd->ui = new PureUI();
  if (!fd->ui) goto error;
  fd->d->buildUserInterface(fd->ui);
  init_bufs(fd);
  return fd;
 error:
  faust_exit(fd);
  return NULL;
}

extern "C"
void faust_reinit(faust_t *fd, int rate)
{
  fd->d->init(rate);
}

extern "C"
faust_t *faust_clone(faust_t *fd1)
{
  faust_t *fd = (faust_t*)malloc(sizeof(faust_t));
  if (!fd) return NULL;
  *fd = *fd1;
  fd->d = fd->newdsp();
  if (!fd->d) {
    free(fd);
    return NULL;
  }
  fd->d->init(fd->rate);
  fd->ui = new PureUI();
  if (!fd->ui) {
    fd->deldsp(fd->d);
    free(fd);
    return NULL;
  }
  fd->d->buildUserInterface(fd->ui);
  init_bufs(fd);
  (*fd->refc)++;
  return fd;
}

/* GSL-compatible matrix structs, cf. gsl_structs.h in the interpreter
   source. */

typedef struct _gsl_block
{
  size_t size;
  double *data;
} gsl_block;

typedef struct _gsl_matrix
{
  size_t size1;
  size_t size2;
  size_t tda;
  double *data;
  gsl_block *block;
  int owner;
} gsl_matrix;

typedef struct _gsl_block_symbolic
{
  size_t size;
  pure_expr **data;
} gsl_block_symbolic;

typedef struct _gsl_matrix_symbolic
{
  size_t size1;
  size_t size2;
  size_t tda;
  pure_expr **data;
  gsl_block_symbolic *block;
  int owner;
} gsl_matrix_symbolic;

extern "C"
pure_expr *faust_compute(faust_t *fd, pure_expr *in, pure_expr *out)
{
  int n = fd->d->getNumInputs(), m = fd->d->getNumOutputs();
  double *in_data, *out_data;
  size_t in_nrows, in_ncols, in_tda, out_nrows, out_ncols, out_tda;
  void *p;
  if (pure_is_double_matrix(in, &p)) {
    gsl_matrix *mat = (gsl_matrix*)p;
    in_data = mat->data;
    in_nrows = mat->size1; in_ncols = mat->size2; in_tda = mat->tda;
  } else if (pure_is_symbolic_matrix(in, &p)) {
    gsl_matrix_symbolic *mat = (gsl_matrix_symbolic*)p;
    in_data = NULL;
    in_nrows = mat->size1; in_ncols = mat->size2; in_tda = mat->tda;
    if (in_ncols > 0) return NULL;
  } else
    return NULL;
  if (in_nrows < n) return NULL;
  if (pure_is_double_matrix(out, &p)) {
    gsl_matrix *mat = (gsl_matrix*)p;
    out_data = mat->data;
    out_nrows = mat->size1; out_ncols = mat->size2; out_tda = mat->tda;
  } else if (pure_is_symbolic_matrix(out, &p)) {
    gsl_matrix_symbolic *mat = (gsl_matrix_symbolic*)p;
    out_data = NULL;
    out_nrows = mat->size1; out_ncols = mat->size2; out_tda = mat->tda;
    if (out_ncols > 0) return NULL;
  } else
    return NULL;
  if (out_nrows < m) return NULL;
  if ((in_ncols > 0 || n>0) && (out_ncols > 0 || m>0) &&
      in_ncols != out_ncols) return NULL;
  /* Number of samples to be processed. */
  int count = (in_ncols>0)?in_ncols:out_ncols;
  /* The Faust compute() method expects a vector of double pointers for each
     channel, we compute this on the fly. Note that the out matrix will be
     modified in-place. */
  for (size_t i = 0; i < n; i++)
    fd->inbuf[i] = in_data+i*in_tda;
  for (size_t i = 0; i < m; i++)
    fd->outbuf[i] = out_data+i*out_tda;
  fd->d->compute(count, fd->inbuf, fd->outbuf);
  return out;
}

struct stack_elem_t {
  int type;
  const char *label;
  int n;
  pure_expr **xv;
} *stack = NULL; // TLD

int astacksz = 0, stacksz = 0; // TLD

static void clear()
{
  for (int i = 0; i < stacksz; i++)
    if (stack[i].xv) free(stack[i].xv);
  free(stack); stack = NULL; astacksz = stacksz = 0;
}

static int push(int type, const char *label, int n, pure_expr **xv)
{
  if (stacksz+1 >= astacksz) {
    stack_elem_t *stack1 =
      (stack_elem_t*)realloc(stack, (astacksz+100)*sizeof(stack_elem_t));
    if (!stack1) return 0;
    stack = stack1;
    astacksz += 100;
  }
  stack[stacksz].type = type;
  stack[stacksz].label = label;
  stack[stacksz].n = n;
  stack[stacksz].xv = xv;
  stacksz++;
  return 1;
}

static int pop(int &type, const char *&label, int &n, pure_expr **&xv)
{
  if (stacksz <= 0) return 0;
  stacksz--;
  type = stack[stacksz].type;
  label = stack[stacksz].label;
  n = stack[stacksz].n;
  xv = stack[stacksz].xv;
  return 1;
}

extern "C"
pure_expr *faust_info(faust_t *fd)
{
  PureUI *ui = fd->ui;
  if (ui->nelems <= 0)
    return pure_tuplel(3, pure_int(fd->d->getNumInputs()),
		       pure_int(fd->d->getNumOutputs()), pure_listl(0));
  pure_expr **xv = NULL;
  int n = 0;
  for (int i = 0; i < ui->nelems; i++) {
    pure_expr *x;
    int type;
    const char *label;
    pure_expr **xv1 = (pure_expr**)realloc(xv, (n+1)*sizeof(pure_expr*));
    if (xv1)
      xv = xv1;
    else {
      free(xv);
      clear();
      return NULL;
    }
    switch (ui->elems[i].type) {
    case UI_BUTTON:
      xv[n++] = pure_appl(pure_symbol(pure_sym("button")), 2,
			  pure_pointer(ui->elems[i].zone),
			  pure_cstring_dup(ui->elems[i].label));
      break;
    case UI_TOGGLE_BUTTON:
      /* FIXME: What's the toggle button supposed to be? Taken to mean a
	 checkbox for now. */
    case UI_CHECK_BUTTON:
      xv[n++] = pure_appl(pure_symbol(pure_sym("checkbox")), 2,
			  pure_pointer(ui->elems[i].zone),
			  pure_cstring_dup(ui->elems[i].label));
      break;
    case UI_V_SLIDER:
      xv[n++] = pure_appl(pure_symbol(pure_sym("vslider")), 2,
			  pure_pointer(ui->elems[i].zone),
			  pure_tuplel(5, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].init),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max),
				      pure_double(ui->elems[i].step)));
      break;
    case UI_H_SLIDER:
      xv[n++] = pure_appl(pure_symbol(pure_sym("hslider")), 2,
			  pure_pointer(ui->elems[i].zone),
			  pure_tuplel(5, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].init),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max),
				      pure_double(ui->elems[i].step)));
      break;
    case UI_NUM_ENTRY:
      xv[n++] = pure_appl(pure_symbol(pure_sym("nentry")), 2,
			    pure_pointer(ui->elems[i].zone),
			  pure_tuplel(5, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].init),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max),
				      pure_double(ui->elems[i].step)));
      break;
    case UI_V_BARGRAPH:
      xv[n++] = pure_appl(pure_symbol(pure_sym("vbargraph")), 2,
			  pure_pointer(ui->elems[i].zone),
			  pure_tuplel(3, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max)));
      break;
    case UI_H_BARGRAPH:
      xv[n++] = pure_appl(pure_symbol(pure_sym("hbargraph")), 2,
			  pure_pointer(ui->elems[i].zone),
			  pure_tuplel(3, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max)));
      break;
    case UI_T_GROUP:
    case UI_V_GROUP:
    case UI_H_GROUP:
      push(ui->elems[i].type, ui->elems[i].label, n, xv);
      n = 0; xv = NULL;
      break;
    case UI_END_GROUP:
      x = pure_listv(n, xv); free(xv);
      pop(type, label, n, xv);
      switch (type) {
      case UI_T_GROUP:
	xv[n++] = pure_app(pure_symbol(pure_sym("tgroup")),
			   pure_tuplel(2, pure_cstring_dup(label), x));
	break;
      case UI_V_GROUP:
	xv[n++] = pure_app(pure_symbol(pure_sym("vgroup")),
			   pure_tuplel(2, pure_cstring_dup(label), x));
	break;
      case UI_H_GROUP:
	xv[n++] = pure_app(pure_symbol(pure_sym("hgroup")),
			   pure_tuplel(2, pure_cstring_dup(label), x));
	break;
      default:
	/* can't happen */
	xv[n++] = NULL;
	break;
      }
      break;
    default:
      /* can't happen */
      xv[n++] = NULL;
      break;
    }
  }
  clear();
  assert(n==1);
  pure_expr *x = xv[0];
  free(xv);
  return pure_tuplel(3, pure_int(fd->d->getNumInputs()),
		     pure_int(fd->d->getNumOutputs()), x);
}
