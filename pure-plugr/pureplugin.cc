
/* Generic plugin architecture for Pure. 2014-12-11 Copyright (c) 2014 by
   Albert Gräf <aggraef@gmail.com>. Distributed under the LGPL, please see
   the accompanying COPYING and COPYING.LESSER files for details. */

#define _POSIX_C_SOURCE 200809L  /* for strdup */

#include <alloca.h>
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#include <pure/runtime.h>

// You'll have to set these if you want to compile your own plugins.
// (The pure2plugr script automatically takes care of this.)
#ifndef PLUGIN_NAME
#define PLUGIN_NAME "pureplugin"
#endif
#ifndef LOADER_NAME
#define LOADER_MAIN __pureplugin_main__
#endif

// You can also define this if the main function of your plugin is named
// something else than `plugin`. But this should rarely be necessary.
#ifndef PLUGIN_FUN
#define PLUGIN_FUN "plugin"
#endif

#include "pureplugin.hh"

// This is the main entry point in the batch-compiled Pure module.
extern "C" void LOADER_NAME(int argc, char** argv);

static bool check(unsigned i, const char *s, bool chk)
{
  if (!chk)
    fprintf(stderr, "%s: port #%u: bad %s\n", PLUGIN_NAME, i, s);
  return chk;
}

static inline bool pure_is_number(pure_expr *x, double *num)
{
  int i;
  if (pure_is_double(x, num))
    return true;
  else if (pure_is_int(x, &i)) {
    *num = (double)i;
    return true;
  } else
    return false;
}

int PurePlugin::res;
bool PurePlugin::owner = true;
pure_interp *PurePlugin::interp = 0;
uint32_t PurePlugin::n;
char **PurePlugin::sym, **PurePlugin::name, **PurePlugin::units;
uint8_t *PurePlugin::ty;
uint16_t *PurePlugin::flags;
float *PurePlugin::mins, *PurePlugin::maxs, *PurePlugin::steps,
  *PurePlugin::defs;
uint32_t PurePlugin::n_in, PurePlugin::n_out,
  *PurePlugin::in, *PurePlugin::out;
uint32_t PurePlugin::n_evin, PurePlugin::n_evout,
  *PurePlugin::evin, *PurePlugin::evout;
uint32_t PurePlugin::n_ctl, *PurePlugin::ctl;

int PurePlugin::ncontrols()
{
  if (interp) return res;
  // We're running for the first time. Create an interpreter instance and
  // initialize the static data. Note that even here we need to obtain the GIL
  // since some hosts may execute multiple plugin initializations in parallel.
  pure_interp *s_interp = pure_lock_interp(0);
  LOADER_NAME(0, 0);
  interp = pure_current_interp();
  if (!interp) {
    fprintf(stderr, "%s: couldn't load Pure interpreter\n", PLUGIN_NAME);
    res = -1;
    pure_unlock_interp(s_interp);
    return res;
  }
  // Invoke the manifest function to get the port information.
  pure_expr *e, *info = pure_symbolx(pure_sym("manifest"), &e);
  size_t m;
  pure_expr **xv;
  bool ok = info && pure_is_listv(info, &m, &xv);
  if (ok) {
    uint32_t k_in = 0, k_out = 0, k_evin = 0, k_evout = 0, k_ctl = 0;
    n = m;
    sym = (char**)calloc(n, sizeof(char*));
    name = (char**)calloc(n, sizeof(char*));
    ty = (uint8_t*)calloc(n, sizeof(uint8_t));
    flags = (uint16_t*)calloc(n, sizeof(uint16_t));
    mins = (float*)calloc(n, sizeof(float));
    maxs = (float*)calloc(n, sizeof(float));
    steps = (float*)calloc(n, sizeof(float));
    defs = (float*)calloc(n, sizeof(float));
    units = (char**)calloc(n, sizeof(char*));
    in = (uint32_t*)calloc(n, sizeof(uint32_t));
    out = (uint32_t*)calloc(n, sizeof(uint32_t));
    evin = (uint32_t*)calloc(n, sizeof(uint32_t));
    evout = (uint32_t*)calloc(n, sizeof(uint32_t));
    ctl = (uint32_t*)calloc(n, sizeof(uint32_t));
    for (int i = 0; i < n; i++) {
      size_t m, l = 0;
      pure_expr **yv;
      char *s;
      int k;
      double x;
      pure_is_tuplev(xv[i], &m, &yv);
      // In the following we try to provide some reasonable defaults if a port
      // description is invalid or lacks some elements. We also allow the port
      // name to be omitted, assuming it to be identical to the port symbol if
      // not specified. FIXME: Maybe some of these error conditions should
      // rather cause us to bail out and refuse to create the plugin.
      if (m == 0 || m > 9) {
	fprintf(stderr, "%s: port #%u: bad port description\n", PLUGIN_NAME, i);
	m = 0;
      }
      if (m > 0 && check(i, "symbol", pure_is_cstring_dup(yv[0], &s)))
	sym[i] = s;
      else {
	char s[10];
	sprintf(s, "port#%u", i);
	sym[i] = strdup(s);
      }
      if (m > 1 && pure_is_cstring_dup(yv[1], &s)) {
	l = 1;
	name[i] = s;
      } else
	name[i] = strdup(sym[i]);
      l++;
      // We offer the same set of port types and flags for all supported
      // architectures (even though some of these don't make sense for some
      // architectures), so that manifests work across different architectures.
      if (m > l && check(i, "type", pure_is_int(yv[l], &k) && k>0 && k<5))
	ty[i] = k;
      else
	ty[i] = 1;
      l++;
      k = (ty[i] == 4)?4:0;
      // Many hosts don't like ports which are used both for input (bit 1 of
      // flags set) and output (bit 2) ports, so make sure that it is either
      // one or the other.
      if (m > l && check(i, "flags", pure_is_int(yv[l], &k) &&
			 (k&3) != 0 && (k&3) != 3))
	flags[i] = k;
      else
	flags[i] = 1 | (k&4);
      l++;
      if (m > l && check(i, "default value", pure_is_number(yv[l], &x)))
	defs[i] = x;
      else
	defs[i] = NAN;
      l++;
      if (m > l && check(i, "min value", pure_is_number(yv[l], &x)))
	mins[i] = x;
      else
	mins[i] = NAN;
      l++;
      if (m > l && check(i, "max value", pure_is_number(yv[l], &x)))
	maxs[i] = x;
      else
	maxs[i] = NAN;
      l++;
      if (m > l && check(i, "step size", pure_is_number(yv[l], &x)))
	steps[i] = x;
      else
	steps[i] = NAN;
      l++;
      if (m > l && check(i, "unit name", pure_is_cstring_dup(yv[l], &s)))
	units[i] = s;
      else
	units[i] = 0;
      bool is_ctrl = ty[i] == 1;
      bool is_audio = ty[i] == 2 || ty[i] == 3;
      bool is_midi = ty[i] == 4 && (flags[i]&4);
      bool is_input = flags[i]&1;
      bool is_output = flags[i]&2;
      if (is_audio) {
	// audio port
	if (is_input)
	  in[k_in++] = i;
	if (is_output)
	  out[k_out++] = i;
      } else if (is_midi) {
	// MIDI port
	if (is_input)
	  evin[k_evin++] = i;
	if (is_output)
	  evout[k_evout++] = i;
      } else if (is_ctrl) {
	// control port
	ctl[k_ctl++] = i;
      }
      if (yv) free(yv);
    }
    if (xv) free(xv);
    pure_freenew(info);
    if (!(in = (uint32_t*)realloc(in, k_in*sizeof(uint32_t))))
      k_in = 0;
    if (!(out = (uint32_t*)realloc(out, k_out*sizeof(uint32_t))))
      k_out = 0;
    if (!(evin = (uint32_t*)realloc(evin, k_evin*sizeof(uint32_t))))
      k_evin = 0;
    if (!(evout = (uint32_t*)realloc(evout, k_evout*sizeof(uint32_t))))
      k_evout = 0;
    if (!(ctl = (uint32_t*)realloc(ctl, k_ctl*sizeof(uint32_t))))
      k_ctl = 0;
    n_in = k_in; n_out = k_out;
    n_evin = k_evin; n_evout = k_evout;
    n_ctl = k_ctl;
#if 0
    fprintf(stderr, "%s: successfully loaded plugin\n%u ports, %u/%u audio ports, %u/%u midi ports\n",
	    PLUGIN_NAME, n, n_in, n_out,
	    n_evin, n_evout);
#endif
  } else if (info) {
    char *s = str(info);
    fprintf(stderr, "%s: bad manifest '%s'\n", PLUGIN_NAME, s);
    free(s);
    pure_freenew(info);
    goto fail;
  } else {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: bad manifest (unhandled exception '%s')\n",
	      PLUGIN_NAME, s);
      free(s);
      pure_freenew(e);
    } else
      fprintf(stderr, "%s: bad manifest (unknown error)\n", PLUGIN_NAME);
    goto fail;
  }
  pure_unlock_interp(s_interp);
  res = n_ctl;
  return res;
 fail:
  res = -1;
  pure_unlock_interp(s_interp);
  return res;
}

PurePlugin::PurePlugin(double sr)
{
  rate = sr;
  // these will be updated later
  nsamples = 0;
  active = running = false;
  fun = 0;
  locked = 0; s_interp = 0;
  // initialize the plugin
  int m = ncontrols();
  if (m < 0) return;
  // allocate the data buffers
  data = (void**)calloc(n, sizeof(void*));
  assert(n==0 || data);
  if (owner) {
    // need to allocate storage for the port values
    control_data = (float*)calloc(m, sizeof(float));
    assert(m==0 || control_data);
    for (int i = 0, j = 0; i < n; i++) {
      if (ty[i] == 1) {
	data[i] = control_data+(j++);
	*(float*)data[i] = defs[i];
      }
    }
  }
  midibufsz = 32; midibufptr = 0;
  midibuf = (uint8_t*)calloc(midibufsz, 1);
  // initialize the plugin function
  assert(interp);
  lock();
  pure_expr *e;
  fun = pure_symbolx(pure_sym(PLUGIN_FUN), &e);
  if (!fun) {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: unhandled exception '%s'\n", PLUGIN_NAME, s);
      return;
    }
  }
  pure_new(fun);
  // Create a pointer object representing ourselves and pass it as the first
  // argument to the plugin function. This should yield a closure (or partial
  // application) which is then used on all subsequent invocations.
  pure_expr *p = pure_tag(pure_pointer_tag("PurePlugin*"),
			  pure_pointer(this));
  if (!p) {
    pure_free(fun);
    fun = 0;
    unlock();
    return;
  }
  pure_expr *fun2 = pure_appx(fun, p, &e);
  if (!fun2) {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: unhandled exception '%s'\n", PLUGIN_NAME, s);
      free(s);
      pure_freenew(e);
    }
    pure_free(fun);
    fun = 0;
  } else {
    pure_new(fun2);
    pure_free(fun);
    fun = fun2;
  }
  unlock();
}

PurePlugin::~PurePlugin()
{
  if (fun) {
    lock();
    reset_midi();
    pure_free(fun);
    unlock();
  }
  if (data) {
    free(data);
    if (owner && control_data) free(control_data);
  }
  if (midibuf) free(midibuf);
}

void PurePlugin::lock()
{
  assert(locked>=0);
  if (locked++ == 0)
    s_interp  = pure_lock_interp(interp);
}

void PurePlugin::unlock()
{
  assert(locked>0);
  if (--locked == 0)
    pure_unlock_interp(s_interp);
}

void PurePlugin::receive_midi(int port, int offs, int sz, unsigned char *data)
{
  lock();
  pure_expr *x = matrix_from_byte_array(1, sz, data);
  assert(x);
  midiin.push_back(PureMidiData(port, offs, pure_new(x)));
  unlock();
}

void PurePlugin::send_midi(send_midi_cb cb, void *user_data)
{
  lock();
  prepare_midi_buffers();
  for (std::vector<PureMidiData>::iterator it = midiout.begin(),
	 end = midiout.end(); it != end; it++) {
    uint8_t *v;
    int sz = matrix_size(it->ev);
    if (sz != 0 &&
	(v = (uint8_t*)matrix_to_byte_array(make_midi_buffer(sz), it->ev))) {
      cb(it->port, it->offs, sz, v, user_data);
    }
  }
  reset_midi();
  unlock();
}

void PurePlugin::reset_midi()
{
  lock();
  for (std::vector<PureMidiData>::iterator it = midiin.begin(),
	 end = midiin.end(); it != end; it++)
    pure_free(it->ev);
  midiin.clear();
  for (std::vector<PureMidiData>::iterator it = midiout.begin(),
	 end = midiout.end(); it != end; it++)
    pure_free(it->ev);
  midiout.clear();
  unlock();
}

void PurePlugin::prepare_midi_buffers()
{
  // Make sure that we have enough memory to hold all the MIDI events to be
  // output during this cycle.
  int total = 0;
  for (std::vector<PureMidiData>::iterator it = midiout.begin(),
	 end = midiout.end(); it != end; it++) {
    int sz = matrix_size(it->ev);
    total += sz;
  }
  if (total > midibufsz) {
    midibufsz = total;
    midibuf = (uint8_t*)realloc(midibuf, midibufsz);
    assert(midibuf);
  }
  midibufptr = 0;
}

uint8_t *PurePlugin::make_midi_buffer(int sz)
{
  uint8_t *buf = midibuf+midibufptr;
  midibufptr += sz;
  return buf;
}

void PurePlugin::activate(bool state)
{
  if (active == state) return;
  active = state;
  lock();
  pure_expr *e, *ret = pure_appx(fun, pure_int(active), &e);
  if (!ret) {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: unhandled exception '%s'\n", PLUGIN_NAME, s);
      free(s);
      pure_freenew(e);
    }
  } else
    pure_freenew(ret);
  unlock();
}

void PurePlugin::process(float **inputs, float **outputs, int blocksz)
{
  // Set up the audio inputs and outputs if necessary.
  if (inputs) {
    nsamples = blocksz;
    for (int i = 0; i < n_in; i++)
      data[in[i]] = inputs[i];
  }
  if (outputs) {
    nsamples = blocksz;
    for (int i = 0; i < n_out; i++)
      data[out[i]] = outputs[i];
  }
  // Invoke the plugin function. Note that the MIDI input queue should already
  // be set up at this point.
  lock();
  running = true;
  pure_expr *e, *ret = pure_appx(fun, pure_tuplel(0, 0), &e);
  running = false;
  if (!ret) {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: unhandled exception '%s'\n", PLUGIN_NAME, s);
      free(s);
      pure_freenew(e);
    }
  } else
    pure_freenew(ret);
  unlock();
  // Reset the audio inputs and outputs if necessary.
  if (inputs) {
    for (int i = 0; i < n_in; i++)
      data[in[i]] = 0;
  }
  if (outputs) {
    for (int i = 0; i < n_out; i++)
      data[out[i]] = 0;
  }
}

pure_expr *PurePlugin::manifest()
{
  lock();
  pure_expr **xv = (pure_expr**)alloca(n*sizeof(pure_expr*));
  for (unsigned i = 0; i < n; i++) {
    if (ty[i] == 1 || ty[i] == 3) {
      xv[i] = pure_tuplel(9, pure_cstring_dup(sym[i]),
			  pure_cstring_dup(name[i]),
			  pure_int(ty[i]), pure_int(flags[i]),
			  pure_double(defs[i]),
			  pure_double(mins[i]),
			  pure_double(maxs[i]),
			  pure_double(steps[i]),
			  pure_cstring_dup(units[i]?units[i]:""));
    } else {
      xv[i] = pure_tuplel(4, pure_cstring_dup(sym[i]),
			  pure_cstring_dup(name[i]),
			  pure_int(ty[i]), pure_int(flags[i]));
    }
  }
  pure_expr *ret = pure_listv(n, xv);
  unlock();
  return ret;
}

pure_expr *PurePlugin::audio_inputs()
{
  lock();
  pure_expr **xv = (pure_expr**)alloca(n_in*sizeof(pure_expr*));
  for (int i = 0; i < n_in; i++)
    xv[i] = pure_int(in[i]);
  pure_expr *ret = pure_listv(n_in, xv);
  unlock();
  return ret;
}

pure_expr *PurePlugin::audio_outputs()
{
  lock();
  pure_expr **xv = (pure_expr**)alloca(n_out*sizeof(pure_expr*));
  for (int i = 0; i < n_out; i++)
    xv[i] = pure_int(out[i]);
  pure_expr *ret = pure_listv(n_out, xv);
  unlock();
  return ret;
}

pure_expr *PurePlugin::midi_inputs()
{
  lock();
  pure_expr **xv = (pure_expr**)alloca(n_evin*sizeof(pure_expr*));
  for (int i = 0; i < n_evin; i++)
    xv[i] = pure_int(evin[i]);
  pure_expr *ret = pure_listv(n_evin, xv);
  unlock();
  return ret;
}

pure_expr *PurePlugin::midi_outputs()
{
  lock();
  pure_expr **xv = (pure_expr**)alloca(n_evout*sizeof(pure_expr*));
  for (int i = 0; i < n_evout; i++)
    xv[i] = pure_int(evout[i]);
  pure_expr *ret = pure_listv(n_evout, xv);
  unlock();
  return ret;
}

/* Functions to read and write port values. XXXFIXME: There seems to be no
   reliable way to determine the lifetime of the port data. We therefore
   enforce that these routines are only called inside the plugin's process()
   method which should always be safe. However, this seems overly restrictive
   for hosts that do offer persistent buffers. */

/* Get a port value. The port is specified using its index. For a control port
   this yields a double, for audio/CV ports a double vector (containing a
   block of audio samples). For MIDI ports the result is a list of pairs of
   ints and int vectors; the int value denotes a timestamp (frame number
   relative to the current block of audio samples), and the vector contains
   the bytes of a single MIDI message. This operation fails if the port number
   is out of range or the type of port data isn't recognized. */

pure_expr *PurePlugin::get(int k)
{
  if (k<0 || k>=n || !running) return 0;
  // Note that there's no need to call lock() here, as we're running inside
  // the process() callback and thus the GIL is already ours.
  switch (ty[k]) {
  case 1:
    return pure_double(*(float*)data[k]);
  case 2: case 3:
    // FIXME: We should really preallocate some static Pure vectors here, to
    // avoid dynamic allocations as much as possible.
    return matrix_from_float_array(1, nsamples, data[k]);
  case 4:
    // XXXTODO: Provide some way to handle time/tempo and transport
    // information (flags = 8). Some VST and LV2 hosts such as Ardour provide
    // these in various ways, so we'd like to make these available to plugins.
    if ((flags[k] & 4) && (flags[k] & 1)) {
      // FIXME: In the present implementation we have to iterate over the
      // entire midin queue to pick the MIDI events that we want. Since most
      // plugin architectures only support a single MIDI input port anyway,
      // this shouldn't be much of an issue, though.
      pure_expr **xv = (pure_expr**)alloca(midiin.size()*sizeof(pure_expr*));
      int n = 0;
      for (std::vector<PureMidiData>::iterator it = midiin.begin(),
	     end = midiin.end(); it != end; it++) {
	if (it->port == k)
	  xv[n++] = pure_tuplel(2, pure_int(it->offs), it->ev);
      }
      return pure_listv(n, xv);
    } else
      return 0;
  default:
    return 0;
  }
}

/* Set a port value. The value must be in the same format as returned by
   PurePlugin::get(), corresponding to the type of port. This operation fails
   if the port number is out of range or the given port data is invalid.
   Otherwise () is returned. NOTE: In the case of an audio/CV port it is
   checked that the given sample vector doesn't overflow the host audio
   buffers; otherwise the operation will fail. */

pure_expr *PurePlugin::set(int k, pure_expr *x)
{
  if (k<0 || k>=n || !running) return 0;
  switch (ty[k]) {
  case 1: {
    double d;
    if (pure_is_double(x, &d)) {
      *(float*)data[k] = d;
      return pure_tuplel(0, 0);
    } else
      return 0;
  }
  case 2: case 3: {
    void *v;
    if (pure_is_double_matrix(x, &v) && matrix_size(x) <= nsamples &&
	matrix_to_float_array(data[k], x))
      return pure_tuplel(0, 0);
    else
      return 0;
  }
  case 4:
    if ((flags[k] & 4) && (flags[k] & 2)) {
      size_t n;
      pure_expr **xv;
      if (pure_is_listv(x, &n, &xv)) {
	// Make sure to get rid of any existing MIDI data first.
	for (std::vector<PureMidiData>::iterator it = midiout.begin(),
	       end = midiout.end(); it != end; it++)
	  pure_free(it->ev);
	midiout.clear();
	if (n == 0) return pure_tuplel(0, 0);
	for (size_t i = 0; i < n; i++) {
	  int frames = 0;
	  size_t m;
	  pure_expr *x, **yv;
	  bool ok = pure_is_tuplev(xv[i], &m, &yv); // always true
	  // We allow the timestamp to be omitted (assuming 0), so we expect a
	  // tuple of one or two values here.
	  if (m == 1)
	    x = yv[0];
	  else if (m ==2 && pure_is_int(yv[0], &frames))
	    x = yv[1];
	  else
	    ok = false;
	  free(yv);
	  int sz = ok?matrix_size(x):0;
	  void *data;
	  ok = ok && pure_is_int_matrix(x, &data) && sz != 0;
	  if (!ok) {
#if 0 // you might want to enable this for debugging purposes
	    char *s = str(x);
	    fprintf(stderr, "%s: dropped invalid MIDI event '%s'\n",
		    PLUGIN_NAME, s);
	    free(s);
#endif
	    continue;
	  }
	  midiout.push_back(PureMidiData(k, frames, pure_new(x)));
	}
	free(xv);
	return pure_tuplel(0, 0);
      } else
	return 0;
    } else
      return 0;
  default:
    return 0;
  }
}

/* VST-specific part starts here. ********************************************/

// Some boilerplate code pilfered from the mda Linux vst source code.
#include "pluginterfaces/vst2.x/aeffectx.h"
extern "C" {
#define VST_EXPORT   __attribute__ ((visibility ("default")))
extern VST_EXPORT AEffect * VSTPluginMain(audioMasterCallback audioMaster);
// This is for legacy (<2.4) VST hosts which look for the 'main' entry point.
AEffect *main_plugin (audioMasterCallback audioMaster) asm ("main");
#define main main_plugin
VST_EXPORT AEffect * main(audioMasterCallback audioMaster)
{
  return VSTPluginMain(audioMaster);
}
}

#include "audioeffectx.h"

// Helper data structure to implement a MIDI output queue.
struct VSTMidiEvents {
  int size; // current size
  int alloc; // allocated size
  // VstEvents struct. This is allocated dynamically to be of sufficient size
  // to hold alloc event pointers.
  VstEvents *evs;
  // The actual VstMidiEvent and VstMidiSysexEvent structs which contain the
  // data. This is allocated dynamically to be of sufficient size to hold at
  // least alloc events of the corresponding type.
  VstMidiEvent *midi_evs;
  VstMidiSysexEvent *sysex_evs;
  // Reserve sufficient space in the queue to hold at least the given number
  // of events.
  bool reserve(int n);
  // Add the given MIDI event (regular or sysex) to the queue and update the
  // evs struct accordingly.
  bool add(int offs, int sz, unsigned char *data);
  // Clear the queue. Note that this only resets the size value but doesn't
  // free any storage.
  void clear() { size = 0; evs->numEvents = 0; }
  // Constructor.
  VSTMidiEvents() : size(0), alloc(0), evs(0), midi_evs(0), sysex_evs(0)
  {
    // Make sure that we reserve some initial storage.
    reserve(32); evs->numEvents = 0;
  }
};

bool VSTMidiEvents::reserve(int n)
{
  const int min_events = sizeof(evs->events)/sizeof(VstEvent*);
  if (n < min_events) n = min_events;
  if (n <= alloc) return true;
  evs = (VstEvents*)realloc(evs, sizeof(VstEvents)+
			    n*sizeof(VstEvent*)-sizeof(evs->events));
  assert(evs);
  if (alloc == 0) memset(evs, 0, sizeof(VstEvents));
  midi_evs =
    (VstMidiEvent*)realloc(midi_evs, n*sizeof(VstMidiEvent));
  sysex_evs =
    (VstMidiSysexEvent*)realloc(sysex_evs, n*sizeof(VstMidiSysexEvent));
  assert(midi_evs && sysex_evs);
  alloc = n;
  return true;
}

bool VSTMidiEvents::add(int offs, int sz, unsigned char *data)
{
  if (sz <= 0 || !data) return false;
  if (!reserve(size+1)) return false;
  // Make sure that all fields are zeroed out to begin with.
  memset(&midi_evs[size], 0, sizeof(VstMidiEvent));
  memset(&sysex_evs[size], 0, sizeof(VstMidiSysexEvent));
  if (offs < 0) offs = 0;
  if (data[0] == 0xf0) {
    // sysex
    VstMidiSysexEvent *ev = &sysex_evs[size];
    ev->type = kVstSysExType;
    ev->byteSize = sizeof(VstMidiSysexEvent);
    ev->deltaFrames = offs;
    ev->dumpBytes = sz;
    // this buffer is guaranteed to persist until the next cycle
    ev->sysexDump = (char*)data;
    evs->events[size] = (VstEvent*)ev;
  } else {
    // regular MIDI event
    VstMidiEvent *ev = &midi_evs[size];
    ev->type = kVstMidiType;
    ev->flags = kVstMidiEventIsRealtime;
    ev->byteSize = sizeof(VstMidiEvent);
    ev->deltaFrames = offs;
    if (sz>4) sz = 4;
    memcpy(ev->midiData, data, sz);
    evs->events[size] = (VstEvent*)ev;
  }
  evs->numEvents = ++size;
  return true;
}

class VSTPlugR : public AudioEffectX
{
public:
  VSTPlugR(audioMasterCallback audioMaster);
  ~VSTPlugR();

  virtual void processReplacing(float **inputs, float **outputs,
				VstInt32 sampleframes);
  virtual VstInt32 processEvents(VstEvents* events);

  virtual void suspend();
  virtual void resume();
  virtual void setSampleRate(float sampleRate);

  virtual void setProgram(VstInt32 program);
  virtual void setProgramName(const char *name);
  virtual void getProgramName(char *name);
  virtual bool getProgramNameIndexed(VstInt32 category,
				     VstInt32 index, char* text);

  virtual void setParameter(VstInt32 index, float value);
  virtual float getParameter(VstInt32 index);
  virtual bool string2parameter(VstInt32 index, char *text);
  virtual void getParameterLabel(VstInt32 index, char *label);
  virtual void getParameterDisplay(VstInt32 index, char *text);
  virtual void getParameterName(VstInt32 index, char *text);

  virtual bool getInputProperties(VstInt32 index,
				  VstPinProperties *properties);
  virtual bool getOutputProperties(VstInt32 index,
				   VstPinProperties* properties);

  virtual bool getEffectName(char* name);
  virtual bool getVendorString(char* text);
  virtual bool getProductString(char* text);
  virtual VstInt32 getVendorVersion();
  virtual VstInt32 canDo(char* text);

  // We process all MIDI channels on input and output.
  virtual VstInt32 getNumMidiInputChannels()  { return 16; }
  virtual VstInt32 getNumMidiOutputChannels()  { return 16; }

  VSTMidiEvents evs;
  PurePlugin *plugin;

private:
  char progname[kVstMaxProgNameLen+1];
  float *defprog;
  int n_ctlout;
};

// Create a "unique" VST plugin ID using Murmur2 hashes. This can't possibly
// avoid all collisions, but will hopefully be good enough.

// Code pilfered from http://code.google.com/p/smhasher/.
static uint32_t MurmurHash2(const void *key, int len, uint32_t seed)
{
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.

  const uint32_t m = 0x5bd1e995;
  const int r = 24;

  // Initialize the hash to a 'random' value

  uint32_t h = seed ^ len;

  // Mix 4 bytes at a time into the hash

  const unsigned char * data = (const unsigned char *)key;

  while(len >= 4)
  {
    uint32_t k = *(uint32_t*)data;

    k *= m;
    k ^= k >> r;
    k *= m;

    h *= m;
    h ^= k;

    data += 4;
    len -= 4;
  }

  // Handle the last few bytes of the input array

  switch(len)
  {
  case 3: h ^= data[2] << 16;
  case 2: h ^= data[1] << 8;
  case 1: h ^= data[0];
      h *= m;
  };

  // Do a few final mixes of the hash to ensure the last few
  // bytes are well-incorporated.

  h ^= h >> 13;
  h *= m;
  h ^= h >> 15;

  return h;
}

static uint32_t idhash(const char *s)
{
  // Arbitrary seed value (should be the same for all instances).
  const uint32_t seed = 9314;
  // XXXFIXME: The rules for valid-formed VST ids don't seem to be very
  // clear. Can it be just any 32 bit number? But it looks like at least the
  // most significant bit should be 0 here, so we enforce that. (Some VST
  // hosts such as Carla display the id as zero otherwise.)
  return MurmurHash2(s, strlen(s), seed) & 0x7fffffff;
}

// initialization and finalization

// This interface function is used by the code in vstplugmain.cpp to create an
// instance of the plugin. It simply invokes the constructor of our VSTPlugR
// class here.
AudioEffect *createEffectInstance(audioMasterCallback audioMaster)
{
  return new VSTPlugR(audioMaster);
}

VSTPlugR::VSTPlugR(audioMasterCallback audioMaster)
  : AudioEffectX(audioMaster, 1, PurePlugin::ncontrols())
{
  plugin = new PurePlugin(getSampleRate());
  // VST-specific initialization:
  if (audioMaster) {
    setNumInputs(plugin->n_in);
    setNumOutputs(plugin->n_out);		
    canProcessReplacing();
    // We assume that a plugin is an instrument if it has at least one MIDI
    // input and at least one audio output.
    if (plugin->n_evin > 0 && plugin->n_out > 0) isSynth();
    // XXXFIXME: Maybe do something more clever for the unique id.
    setUniqueID((VstInt32)idhash(PLUGIN_NAME));
  }
  // We only provide one "program" (a.k.a. built-in control preset), which
  // corresponds to the initial control values of the plugin.
  curProgram = 0;
  setProgramName("Default");
  // Initialize the program storage with the given defaults for the controls.
  defprog = (float*)calloc(plugin->n_ctl, sizeof(float));
  assert(plugin->n_ctl == 0 || defprog);
  // We also count the number of output controls here. (We need to know if
  // there are any such controls so that we can force a GUI update in the
  // audio callback, cf. VSTPlugR::processReplacing().)
  n_ctlout = 0;
  for (int i = 0; i < plugin->n_ctl; i++) {
    defprog[i] = plugin->defs[plugin->ctl[i]];
    if ((plugin->flags[i] & 2)) n_ctlout++;
  }
}

VSTPlugR::~VSTPlugR()
{
  delete plugin;
  if (defprog) free(defprog);
}

// plugin activation and deactivation

void VSTPlugR::suspend()
{
  plugin->suspend();
}

void VSTPlugR::resume()
{
  plugin->rate = getSampleRate();
  plugin->resume();
}

void VSTPlugR::setSampleRate(float sampleRate)
{
  AudioEffect::setSampleRate(sampleRate);
  plugin->rate = sampleRate;
}

// programs a.k.a. built-in presets (see above)

void VSTPlugR::setProgram(VstInt32 prog)
{
  if (prog < 0 || prog >= 1) return;
  curProgram = prog;
  for (int i = 0; i < plugin->n_ctl; i++)
    // only set the input controls
    if ((plugin->flags[i] & 1))
      *(float*)plugin->data[plugin->ctl[i]] = defprog[i];
  // Some hosts may require this to force a GUI update of the parameters.
  updateDisplay();
}

void VSTPlugR::setProgramName(const char* name)
{
  vst_strncpy(progname, name, kVstMaxProgNameLen);
}

void VSTPlugR::getProgramName(char *name)
{
  vst_strncpy(name, progname, kVstMaxProgNameLen);
}

bool VSTPlugR::getProgramNameIndexed(VstInt32 category, VstInt32 index, 
				     char* text)
{
  if (index < 1) {
    vst_strncpy(text, progname, kVstMaxProgNameLen);
    return true;
  } else
    return false;
}

// control values (setters, getters, meta data)

void VSTPlugR::getParameterName(VstInt32 index, char *label)
{
  strcpy(label, "");
  if (index >= 0 && index < plugin->n_ctl) {
    int j = plugin->ctl[index];
    // Note that the VST spec mandates a maximum size of kVstMaxParamStrLen
    // for the label string, which is a rather small constant. This seems
    // overly restrictive, however, given that virtually all VST hosts provide
    // for much longer names. We allow 32 characters here which is hopefully
    // on the safe side.
    vst_strncpy(label, plugin->name[j], 32);
  }
}

void VSTPlugR::getParameterLabel(VstInt32 index, char *label)
{
  strcpy(label, "");
  if (index >= 0 && index < plugin->n_ctl) {
    int j = plugin->ctl[index];
    if (plugin->units[j])
      vst_strncpy(label, plugin->units[j], 32);
  }
}

// NOTE: VST parameters are always floats with unit range (0..1). So we need
// to convert between Pure control values with a given range and the VST range
// here (if the range isn't fully specified, i.e., if min or max values are
// NAN then we just take the values as is). We use the following quantization
// algorithm for mapping VST to Faust control values.

static double quantize(double x, double d)
{
  if (isnan(d) || d == 0.0) return x;
  // Round off x to the nearest increment of d. Note that this may produce
  // rounding artifacts if d is some power of 10 less than 0, since these
  // can't be represented exactly in binary.
  double i;
  if (x*d < 0.0)
    modf(x/d-0.5, &i);
  else
    modf(x/d+0.5, &i);
  return i*d;
}

void VSTPlugR::getParameterDisplay(VstInt32 index, char *text)
{
  strcpy(text, "");
  if (index >= 0 && index < plugin->n_ctl) {
    int j = plugin->ctl[index];
    sprintf(text, "%0.5g", *(float*)plugin->data[j]);
  }
}

float VSTPlugR::getParameter(VstInt32 index)
{
  if (index >= 0 && index < plugin->n_ctl) {
    int j = plugin->ctl[index];
    float min = plugin->mins[j];
    float max = plugin->maxs[j];
    if (isnan(min) || isnan(max))
      return *(float*)plugin->data[j];
    if (min == max)
      return 0.0f;
    else
      return (*(float*)plugin->data[j]-min)/(max-min);
  } else
    return 0.0f;
}

void VSTPlugR::setParameter(VstInt32 index, float value)
{
  const double eps = 1e-5;
  if (index >= 0 && index < plugin->n_ctl) {
    int j = plugin->ctl[index];
    float min = plugin->mins[j];
    float max = plugin->maxs[j];
    float step = plugin->steps[j];
    float val = (isnan(min) || isnan(max))?value:
      (min == max)?min:min+quantize(value*(max-min), step);
    // prevent some rounding artifacts near zero
    if (fabs(val) < fabs(step) || fabs(val)/fabs(max-min) < eps)
      val = 0.0;
    *(float*)plugin->data[j] = val;
  }
}

bool VSTPlugR::string2parameter(VstInt32 index, char *text)
{
  if (!text) return true;
  if (index >= 0 && index < plugin->n_ctl) {
    int j = plugin->ctl[index];
    float min = plugin->mins[j];
    float max = plugin->maxs[j];
    float step = plugin->steps[j];
    double val = atof(text);
    if (min == max)
      val = min;
    else if (!isnan(min) && !isnan(step))
      val = min+quantize(val-min, step);
    if (min > max) {
      float m = max;
      max = min; min = m;
    }
    if (val < min)
      val = min;
    else if (val > max)
      val = max;
    *(float*)plugin->data[j] = val;
  } else
    return false;
  return true;
}

// audio inputs and outputs

bool VSTPlugR::getInputProperties(VstInt32 index,
				    VstPinProperties* properties)
{
  const int n = plugin->n_in;
  if (index < 0 || index >= n)
    return false;
  int j = plugin->in[index];
  vst_strncpy(properties->label, plugin->name[j], kVstMaxLabelLen);
  vst_strncpy(properties->shortLabel, plugin->sym[j], kVstMaxShortLabelLen);
  properties->flags = kVstPinIsActive;
  // XXXTODO: deal with multi-channel setups (>2) here
  if (n == 2)
    properties->flags |= kVstPinIsStereo;
  return true;
}

bool VSTPlugR::getOutputProperties(VstInt32 index,
				     VstPinProperties* properties)
{
  const int n = plugin->n_out;
  if (index < 0 || index >= n)
    return false;
  int j = plugin->out[index];
  vst_strncpy(properties->label, plugin->name[j], kVstMaxLabelLen);
  vst_strncpy(properties->shortLabel, plugin->sym[j], kVstMaxShortLabelLen);
  properties->flags = kVstPinIsActive;
  // XXXTODO: deal with multi-channel setups (>2) here
  if (n == 2)
    properties->flags |= kVstPinIsStereo;
  return true;
}

// global meta data: only the plugin name is available in the manifest right
// now, so we fill the remaining data with dummy information

// XXXTODO: add optional fields for the other meta data to the plugin
// manifest, so that we can provide some more meaningful values here

bool VSTPlugR::getEffectName(char *name)
{
  vst_strncpy(name, PLUGIN_NAME, kVstMaxEffectNameLen);
  return true;
}

bool VSTPlugR::getVendorString(char *text)
{
  strcpy(text, "");
  return true;
}

bool VSTPlugR::getProductString(char *text)
{
  snprintf(text, kVstMaxEffectNameLen, "%s (UPlugR)", PLUGIN_NAME);
  return true;
}

VstInt32 VSTPlugR::getVendorVersion()
{ 
  return 1000;
}

VstInt32 VSTPlugR::canDo(char *text)
{
  if (strcmp(text, "receiveVstEvents") == 0)
    return (plugin->n_evin>0)?1:-1;
  if (strcmp(text, "receiveVstMidiEvent") == 0)
    return (plugin->n_evin>0)?1:-1;
  // This may convince some VST hosts to send us sysex events (seems to be
  // supported by some Bitwig versions at least).
  if (strcmp(text, "receiveVstSysexEvent") == 0)
    return (plugin->n_evin>0)?1:-1;
  // XXXTODO: receive time/tempo information
  //if (strcmp(text, "receiveVstTimeInfo") == 0) return (plugin->n_evin>0);
  if (strcmp(text, "sendVstEvents") == 0)
    return (plugin->n_evout>0)?1:-1;
  if (strcmp(text, "sendVstMidiEvent") == 0)
    return (plugin->n_evout>0)?1:-1;
  return -1;
}

// audio and MIDI process functions

static void midi_out_cb(int port, int offs, int sz, unsigned char *data,
			void *user_data)
{
  VSTPlugR *w = (VSTPlugR*)user_data;
  // VST only supports a single MIDI output port, so the port number is
  // ignored.
  w->evs.add(offs, sz, data);
}

void VSTPlugR::processReplacing(float **inputs, float **outputs,
				VstInt32 n_samples)
{
  plugin->lock();
  plugin->process(inputs, outputs, n_samples);
  // Make sure that we have a VstEvents struct of sufficient size.
  evs.reserve(plugin->midiout.size());
  // Send any MIDI messages in the output queue.
  plugin->send_midi(midi_out_cb, this);
  plugin->unlock();
  sendVstEventsToHost(evs.evs);
  evs.clear();
  // Some hosts may require this to force a GUI update of the output
  // controls.
  if (n_ctlout > 0) updateDisplay();
}

VstInt32 VSTPlugR::processEvents(VstEvents* events)
{
  // Process incoming MIDI events. Note that VST only supports a single MIDI
  // input port, so all events will be received on the first available MIDI
  // input port of the plugin.
  if (plugin->n_evin <= 0) return 0;
  plugin->lock();
  for (VstInt32 i = 0; i < events->numEvents; i++) {
    if (events->events[i]->type == kVstMidiType) {
      VstMidiEvent* ev = (VstMidiEvent*)events->events[i];
      uint8_t *data = (uint8_t*)&ev->midiData[0];
      VstInt32 frames = ev->deltaFrames;
#if 0
      fprintf(stderr, "ev length = %d, offset = %d, detune = %d, off velocity = %d\n", ev->noteLength, ev->noteOffset, (int)(signed char)ev->detune, (int)ev->noteOffVelocity);
#endif
      // Calculate the actual byte size of the MIDI message.
      int sz = 3; // most voice messages have 2 data bytes, use as default
      uint8_t status = data[0] & 0xf0;
      switch (status) {
      case 0x80: case 0x90: case 0xa0: case 0xb0: case 0xe0:
	// These are all the 3-byte voice messages.
	break;
      case 0xc0: case 0xd0:
	// 2-byte voice messages
	sz = 2;
	break;
      case 0xf0:
	// system realtime messages
	switch (data[0]) {
	case 0xf2: // song position (2 data bytes)
	  break;
	case 0xf1: case 0xf3: // MTC quarter frame, song select (1 data byte)
	  sz = 2;
	  break;
	case 0xf0: case 0xf7:
	  // these belong to sysex, we don't expect to see them here
	  return 0;
	default: // anything else is either system realtime or undefined
	  sz = 1; // (no data bytes)
	  break;
	}
      default:
	// not a valid status byte, ignore this message
	continue;
      }
      plugin->receive_midi(plugin->evin[0], frames, sz, data);
    } else if (events->events[i]->type == kVstSysExType) {
      VstMidiSysexEvent* ev = (VstMidiSysexEvent*)events->events[i];
      int sz = ev->dumpBytes;
      uint8_t *data = (uint8_t*)ev->sysexDump;
      VstInt32 frames = ev->deltaFrames;
      plugin->receive_midi(plugin->evin[0], frames, sz, data);
    } else {
      fprintf(stderr, "%s: unknown event type %d\n",
	      PLUGIN_NAME, events->events[i]->type);
    }
  }
  plugin->unlock();
  return 1;
}
