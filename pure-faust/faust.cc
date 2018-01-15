
/* Copyright (c) 2009-2012 by Albert Graef <Dr.Graef@t-online.de>.

   pure-faust is free software: you can redistribute it and/or modify it under
   the terms of the GNU Lesser General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   pure-faust is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. */

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <string.h>
#include <math.h>
#include <ltdl.h>

#include <pure/runtime.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#if PURE_POINTER_TAG
/* Convenience macros to handle tagged pointers (new in Pure 0.45). */
#define __pointer(ty, p) pure_tag(ty, pure_pointer(p))
#define __check_tag(ty, x) pure_check_tag(ty, x)
#define __tag(ty) pure_pointer_tag(#ty)
#else
/* For compatibility with older Pure versions. */
#define __pointer(ty, p) pure_pointer(p)
#define __check_tag(ty, x) 1
#define __tag(ty) 0
#endif

/* The Faust UI and dsp classes. */

#include <list>
#include <map>

using namespace std;

typedef pair<const char*,const char*> strpair;

struct Meta
{
  list<strpair> data;
  void declare (const char* key, const char* value)
  { data.push_back(strpair(key, value)); }
};

class UI
{
  bool	fStopped;
public:

  UI() : fStopped(false) {}
  virtual ~UI() {}

  virtual void addButton(const char* label, double* zone) = 0;
  virtual void addCheckButton(const char* label, double* zone) = 0;
  virtual void addVerticalSlider(const char* label, double* zone, double init, double min, double max, double step) = 0;
  virtual void addHorizontalSlider(const char* label, double* zone, double init, double min, double max, double step) = 0;
  virtual void addNumEntry(const char* label, double* zone, double init, double min, double max, double step) = 0;

  virtual void addHorizontalBargraph(const char* label, double* zone, double min, double max) = 0;
  virtual void addVerticalBargraph(const char* label, double* zone, double min, double max) = 0;

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
  UI_BUTTON, UI_CHECK_BUTTON,
  UI_V_SLIDER, UI_H_SLIDER, UI_NUM_ENTRY,
  UI_V_BARGRAPH, UI_H_BARGRAPH,
  UI_END_GROUP, UI_V_GROUP, UI_H_GROUP, UI_T_GROUP
};

struct ui_elem_t {
  ui_elem_type_t type;
  const char *label;
  double *zone;
  double init, min, max, step;
};

class PureUI : public UI
{
public:
  int nelems;
  ui_elem_t *elems;
  map< int, list<strpair> > metadata;

  PureUI();
  virtual ~PureUI();

protected:
  void add_elem(ui_elem_type_t type, const char *label = NULL);
  void add_elem(ui_elem_type_t type, const char *label, double *zone);
  void add_elem(ui_elem_type_t type, const char *label, double *zone,
		double init, double min, double max, double step);
  void add_elem(ui_elem_type_t type, const char *label, double *zone,
		double min, double max);

public:
  virtual void addButton(const char* label, double* zone);
  virtual void addCheckButton(const char* label, double* zone);
  virtual void addVerticalSlider(const char* label, double* zone, double init, double min, double max, double step);
  virtual void addHorizontalSlider(const char* label, double* zone, double init, double min, double max, double step);
  virtual void addNumEntry(const char* label, double* zone, double init, double min, double max, double step);

  virtual void addHorizontalBargraph(const char* label, double* zone, double min, double max);
  virtual void addVerticalBargraph(const char* label, double* zone, double min, double max);

  virtual void openTabBox(const char* label);
  virtual void openHorizontalBox(const char* label);
  virtual void openVerticalBox(const char* label);
  virtual void closeBox();

  virtual void run();

  virtual void declare(double* zone, const char* key, const char* value);
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

void PureUI::declare(double* zone, const char* key, const char* value)
{
  map< int, list<strpair> >::iterator it = metadata.find(nelems);
  if (it != metadata.end())
    it->second.push_back(strpair(key, value));
  else
    metadata[nelems] = list<strpair>(1, strpair(key, value));
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
			     double init, double min, double max, double step)
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
			     double min, double max)
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
void PureUI::addCheckButton(const char* label, double* zone)
{ add_elem(UI_CHECK_BUTTON, label, zone); }
void PureUI::addVerticalSlider(const char* label, double* zone, double init, double min, double max, double step)
{ add_elem(UI_V_SLIDER, label, zone, init, min, max, step); }
void PureUI::addHorizontalSlider(const char* label, double* zone, double init, double min, double max, double step)
{ add_elem(UI_H_SLIDER, label, zone, init, min, max, step); }
void PureUI::addNumEntry(const char* label, double* zone, double init, double min, double max, double step)
{ add_elem(UI_NUM_ENTRY, label, zone, init, min, max, step); }

void PureUI::addHorizontalBargraph(const char* label, double* zone, double min, double max)
{ add_elem(UI_H_BARGRAPH, label, zone, min, max); }
void PureUI::addVerticalBargraph(const char* label, double* zone, double min, double max)
{ add_elem(UI_V_BARGRAPH, label, zone, min, max); }

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
  dsp *prev, *next;
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
typedef Meta *(*newmetafun)();
typedef void (*delmetafun)(Meta*);

/* A proxy for the loadable module of a Faust dsp. */

struct module_t;
static module_t *load_module(const char *name);
static void unload_module(module_t *mod);
static void clone_module(module_t *mod);

struct module_t {
  newdspfun newdsp;
  deldspfun deldsp;
  newmetafun newmeta;
  delmetafun delmeta;
  time_t mtime;
protected:
  module_t(const char *name);
  ~module_t() { if (h) lt_dlclose(h); }
  void reload();
  lt_dlhandle h;
  int refc;
  friend module_t *load_module(const char *name);
  friend void unload_module(module_t *mod);
  friend void clone_module(module_t *mod);
};

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

module_t::module_t(const char *name)
{
  ltdl_init();
  /* Make sure that we search the current directory for modules. */
  char *path = lt_dlgetsearchpath()?strdup(lt_dlgetsearchpath()):NULL;
  lt_dlsetsearchpath(".");
  h = lt_dlopenext(name);
  if (path) {
    lt_dlsetsearchpath(path);
    free(path);
  }
  if (h) {
    newdsp = (newdspfun)lt_dlsym(h, "newdsp");
    deldsp = (deldspfun)lt_dlsym(h, "deldsp");
    newmeta = (newmetafun)lt_dlsym(h, "newmeta");
    delmeta = (delmetafun)lt_dlsym(h, "delmeta");
    // Record the timestamp of the module, so that we can reload it after
    // changes. This emulates the functionality of Pure's built-in Faust
    // interface.
    const lt_dlinfo *info = lt_dlgetinfo(h);
    struct stat st;
    if (!stat(info->filename, &st))
      mtime = st.st_mtime;
    else
      mtime = 0;
  } else {
    newdsp = NULL;
    deldsp = NULL;
    newmeta = NULL;
    delmeta = NULL;
    mtime = 0;
  }
  refc = 0;
}

void module_t::reload()
{
  if (!h) return;
  const lt_dlinfo *info = lt_dlgetinfo(h);
  if (!info || !info->filename) return;
  struct stat st;
  if (!stat(info->filename, &st)) {
    time_t mtime1 = st.st_mtime;
    // If the module is newer that what's already loaded, try to reload it.
    if (mtime1 > mtime) {
      char *filename = strdup(info->filename);
      lt_dlclose(h);
      h = lt_dlopen(filename);
      free(filename);
      if (h) {
	newdsp = (newdspfun)lt_dlsym(h, "newdsp");
	deldsp = (deldspfun)lt_dlsym(h, "deldsp");
	newmeta = (newmetafun)lt_dlsym(h, "newmeta");
	delmeta = (delmetafun)lt_dlsym(h, "delmeta");
	mtime = mtime1;
      } else {
	// This renders the module invalid.
	newdsp = NULL;
	deldsp = NULL;
	newmeta = NULL;
	delmeta = NULL;
	mtime = 0;
      }
    }
  }
}

// Keep track of the current set of modules that have been loaded.
static map<lt_dlhandle,module_t*> loaded_modules;

static module_t *load_module(const char *name)
{
  module_t *mod = new module_t(name);
  if (!mod->h || !mod->newdsp || !mod->deldsp) {
    delete mod;
    return NULL;
  }
  // See whether this module has been loaded already.
  map<lt_dlhandle,module_t*>::iterator it = loaded_modules.find(mod->h);
  if (it != loaded_modules.end()) {
    // Module is loaded already, return the existing instance instead.
    delete mod;
    mod = it->second;
    // Try to reload the module if it has been modified.
    loaded_modules.erase(mod->h);
    mod->reload();
  }
  if (!mod->h) return NULL;
  loaded_modules[mod->h] = mod;
  mod->refc++;
  return mod;
}

static inline void unload_module(module_t *mod)
{
  if (--mod->refc == 0) {
    if (mod->h) loaded_modules.erase(mod->h);
    delete mod;
  }
}

static inline void clone_module(module_t *mod)
{
  ++mod->refc;
}

/* The Faust DSP proxy. This structure keeps all the necessary information,
   including the handle of the module from which the DSP gets loaded and the
   DSP object itself. */

struct faust_t {
  char *name;
  module_t *mod;
  time_t mtime;
  int rate;
  dsp *d;
  PureUI *ui;
  double **inbuf, **outbuf;
  int nsamples;
};

static inline bool valid(faust_t *fd)
{
  return fd->mtime == fd->mod->mtime;
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
  if (!fd) return;
  // NOTE: If valid(fd) returns false then the Faust module was reloaded and
  // the dsp object was deleted already. In that case we still have to free
  // the other resources, though.
  if (fd->mod && fd->mod->deldsp && fd->d && valid(fd))
    fd->mod->deldsp(fd->d);
  if (fd->ui) delete fd->ui;
  if (fd->inbuf) free(fd->inbuf);
  if (fd->outbuf) free(fd->outbuf);
  if (fd->mod) unload_module(fd->mod);
  if (fd->name) free(fd->name);
  free(fd);
}

static char *dsp_name(const char *name)
{
  // get the basename, strip off any trailing suffix
  const char *p = strrchr(name, '/');
  if (p) p++; else p = name;
  const char *q = strchr(p, '.');
  size_t l = q?q-p:strlen(p);
  char *res = (char*)malloc(l+1);
  if (res) {
    strncpy(res, p, l);
    res[l] = 0;
  }
  return res;
}

extern "C"
faust_t *faust_init(const char *name, int rate)
{
  faust_t *fd;
  if (!name) return NULL;
  fd = (faust_t*)malloc(sizeof(faust_t));
  if (!fd) return NULL;
  fd->name = dsp_name(name);
  fd->rate = 0;
  fd->mod = NULL;
  fd->d = NULL;
  fd->ui = NULL;
  fd->inbuf = fd->outbuf = NULL;
  fd->nsamples = 0;
  fd->mod = load_module(name);
  if (!fd->mod) goto error;
  fd->mtime = fd->mod->mtime;
  fd->d = fd->mod->newdsp();
  if (!fd->d) goto error;
  fd->d->init(rate);
  fd->rate = rate;
  fd->ui = new PureUI();
  if (!fd->ui) goto error;
  fd->d->buildUserInterface(fd->ui);
  // Get rid of bogus "0x00" labels in older Faust versions. Also, for
  // backward compatibility with even older Faust versions, make sure that
  // default toplevel groups and explicit toplevel groups with an empty label
  // are treated alike (these both return "0x00" labels in previous Faust
  // versions, but would be treated inconsistently in earlier versions). Note
  // that all this has been changed once again in the latest Faust versions,
  // which now use the declared plugin name as default if available, or the
  // basename of the dsp file otherwise.
  for (int i = 0; i < fd->ui->nelems; i++) {
    if (!fd->ui->elems[i].label) continue;
    if (!*fd->ui->elems[i].label ||
	strcmp(fd->ui->elems[i].label, "0x00") == 0) {
      if (i == 0)
	// toplevel group with empty label, map to dsp name
	fd->ui->elems[i].label = fd->name;
      else
	// empty label
	fd->ui->elems[i].label = "";
    }
  }
  init_bufs(fd);
  return fd;
 error:
  faust_exit(fd);
  return NULL;
}

extern "C"
void faust_reinit(faust_t *fd, int rate)
{
  if (!valid(fd)) return;
  fd->d->init(rate);
}

extern "C"
faust_t *faust_clone(faust_t *fd1)
{
  if (!valid(fd1)) return NULL;
  faust_t *fd = (faust_t*)malloc(sizeof(faust_t));
  if (!fd) return NULL;
  *fd = *fd1;
  fd->name = strdup(fd->name);
  fd->d = fd->mod->newdsp();
  if (!fd->d) {
    free(fd);
    return NULL;
  }
  fd->d->init(fd->rate);
  fd->ui = new PureUI();
  if (!fd->ui) {
    fd->mod->deldsp(fd->d);
    free(fd);
    return NULL;
  }
  fd->d->buildUserInterface(fd->ui);
  for (int i = 0; i < fd->ui->nelems; i++) {
    if (!fd->ui->elems[i].label) continue;
    if (!*fd->ui->elems[i].label ||
	strcmp(fd->ui->elems[i].label, "0x00") == 0) {
      if (i == 0)
	fd->ui->elems[i].label = fd->name;
      else
	fd->ui->elems[i].label = "";
    }
  }
  init_bufs(fd);
  clone_module(fd->mod);
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
  if (!valid(fd)) return NULL;
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
  /* Number of samples to be processed. */
  int count = 0;
  if (n==0 || m==0)
    count = (in_ncols<out_ncols)?out_ncols:in_ncols;
  else
    count = (in_ncols<out_ncols)?in_ncols:out_ncols;
  if (count == 0) return out; // nothing to do
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
  int i, n;
  pure_expr **xv;
} *stack = NULL; // TLD

int astacksz = 0, stacksz = 0; // TLD

static void clear()
{
  for (int i = 0; i < stacksz; i++)
    if (stack[i].xv) free(stack[i].xv);
  free(stack); stack = NULL; astacksz = stacksz = 0;
}

static int push(int i, int n, pure_expr **xv)
{
  if (stacksz+1 >= astacksz) {
    stack_elem_t *stack1 =
      (stack_elem_t*)realloc(stack, (astacksz+100)*sizeof(stack_elem_t));
    if (!stack1) return 0;
    stack = stack1;
    astacksz += 100;
  }
  stack[stacksz].i = i;
  stack[stacksz].n = n;
  stack[stacksz].xv = xv;
  stacksz++;
  return 1;
}

static int pop(int &i, int &n, pure_expr **&xv)
{
  if (stacksz <= 0) return 0;
  stacksz--;
  i = stack[stacksz].i;
  n = stack[stacksz].n;
  xv = stack[stacksz].xv;
  return 1;
}

static pure_expr *faust_make_meta(list<strpair>& m)
{
  size_t n = m.size();
  pure_expr **xv = (pure_expr**)malloc(n*sizeof(pure_expr*));
  pure_expr *f = pure_symbol(pure_sym("=>"));
  assert(f && xv);
  list<strpair>::iterator it = m.begin(), end = m.end();
  for (size_t i = 0; i < n; i++, it++) {
    assert(it != end);
    xv[i] = pure_appl(f, 2,
		      pure_cstring_dup(it->first),
		      pure_cstring_dup(it->second));
  }
  pure_expr *x = pure_listv(n, xv); free(xv);
  return x;
}

extern "C"
pure_expr *faust_info(faust_t *fd)
{
  if (!valid(fd)) return NULL;
  PureUI *ui = fd->ui;
  if (ui->nelems <= 0)
    return pure_tuplel(3, pure_int(fd->d->getNumInputs()),
		       pure_int(fd->d->getNumOutputs()), pure_listl(0));
  pure_expr **xv = NULL;
  int n = 0;
  for (int i = 0; i < ui->nelems; i++) {
    pure_expr *x;
    pure_expr **xv1 = (pure_expr**)realloc(xv, (n+1)*sizeof(pure_expr*));
    if (xv1)
      xv = xv1;
    else {
      free(xv);
      clear();
      return NULL;
    }
    int ty = __tag(double*);
    switch (ui->elems[i].type) {
    case UI_BUTTON:
      xv[n++] = pure_appl(pure_symbol(pure_sym("button")), 3,
			  __pointer(ty, ui->elems[i].zone),
			  faust_make_meta(ui->metadata[i]),
			  pure_cstring_dup(ui->elems[i].label));
      break;
    case UI_CHECK_BUTTON:
      xv[n++] = pure_appl(pure_symbol(pure_sym("checkbox")), 3,
			  __pointer(ty, ui->elems[i].zone),
			  faust_make_meta(ui->metadata[i]),
			  pure_cstring_dup(ui->elems[i].label));
      break;
    case UI_V_SLIDER:
      xv[n++] = pure_appl(pure_symbol(pure_sym("vslider")), 3,
			  __pointer(ty, ui->elems[i].zone),
			  faust_make_meta(ui->metadata[i]),
			  pure_tuplel(5, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].init),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max),
				      pure_double(ui->elems[i].step)));
      break;
    case UI_H_SLIDER:
      xv[n++] = pure_appl(pure_symbol(pure_sym("hslider")), 3,
			  __pointer(ty, ui->elems[i].zone),
			  faust_make_meta(ui->metadata[i]),
			  pure_tuplel(5, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].init),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max),
				      pure_double(ui->elems[i].step)));
      break;
    case UI_NUM_ENTRY:
      xv[n++] = pure_appl(pure_symbol(pure_sym("nentry")), 3,
			  __pointer(ty, ui->elems[i].zone),
			  faust_make_meta(ui->metadata[i]),
			  pure_tuplel(5, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].init),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max),
				      pure_double(ui->elems[i].step)));
      break;
    case UI_V_BARGRAPH:
      xv[n++] = pure_appl(pure_symbol(pure_sym("vbargraph")), 3,
			  __pointer(ty, ui->elems[i].zone),
			  faust_make_meta(ui->metadata[i]),
			  pure_tuplel(3, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max)));
      break;
    case UI_H_BARGRAPH:
      xv[n++] = pure_appl(pure_symbol(pure_sym("hbargraph")), 3,
			  __pointer(ty, ui->elems[i].zone),
			  faust_make_meta(ui->metadata[i]),
			  pure_tuplel(3, pure_cstring_dup(ui->elems[i].label),
				      pure_double(ui->elems[i].min),
				      pure_double(ui->elems[i].max)));
      break;
    case UI_T_GROUP:
    case UI_V_GROUP:
    case UI_H_GROUP:
      push(i, n, xv);
      n = 0; xv = NULL;
      break;
    case UI_END_GROUP: {
      int i = 0;
      x = pure_listv(n, xv); free(xv);
      pop(i, n, xv);
      switch (ui->elems[i].type) {
      case UI_T_GROUP:
	xv[n++] = pure_appl
	  (pure_symbol(pure_sym("tgroup")), 2,
	   faust_make_meta(ui->metadata[i]),
	   pure_tuplel(2, pure_cstring_dup(ui->elems[i].label), x));
	break;
      case UI_V_GROUP:
	xv[n++] = pure_appl
	  (pure_symbol(pure_sym("vgroup")), 2,
	   faust_make_meta(ui->metadata[i]),
	   pure_tuplel(2, pure_cstring_dup(ui->elems[i].label), x));
	break;
      case UI_H_GROUP:
	xv[n++] = pure_appl
	  (pure_symbol(pure_sym("hgroup")), 2,
	   faust_make_meta(ui->metadata[i]),
	   pure_tuplel(2, pure_cstring_dup(ui->elems[i].label), x));
	break;
      default:
	/* can't happen */
	xv[n++] = NULL;
	break;
      }
      break;
    }
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

extern "C"
pure_expr *faust_meta(faust_t *fd)
{
  newmetafun newmeta = fd->mod->newmeta;
  delmetafun delmeta = fd->mod->delmeta;
  if (!newmeta) return 0;
  Meta *m = newmeta();
  if (!m) return 0;
  pure_expr *x = faust_make_meta(m->data);
  if (delmeta) delmeta(m);
  return x;
}
