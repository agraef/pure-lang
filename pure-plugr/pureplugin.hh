
/* Generic plugin architecture for Pure. 2014-12-11 Copyright (c) 2014 by
   Albert Gräf <aggraef@gmail.com>.  Distributed under the LGPL, please see
   the accompanying COPYING and COPYING.LESSER files for details. */

/* This has been derived from the pure-lv2 plugin. As with JUCE, the idea is
   to make the same plugin binary usable with different kinds of plugin hosts,
   by providing entry points for all supported architectures. The core logic
   of the plugin is factored out as a separate generic base class, so that new
   plugin types can be supported with a minimal amount of additional code.
   Right now it only supports VST, but support for other plugin architectures
   like AU, LV2, Pd etc. (and even JUCE) is planned. */

#ifndef __pureplugin_hh__
#define __pureplugin_hh__

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <pure/runtime.h>
#include <vector>

struct PureMidiData {
  int port; // port number
  int offs; // sample offset (relative to current audio block)
  pure_expr *ev; // actual MIDI event (represented as a Pure expression)
  PureMidiData(int k, int d, pure_expr *x) : port(k), offs(d), ev(x) {}
};

// This callback needs to be passed to the send_midi() method below. The
// callback then gets invoked for each MIDI message to be output in the audio
// callback of the plugin architecture.
typedef void (*send_midi_cb)(int port, int offs, int sz, unsigned char *data,
			     void *user_data);

struct PurePlugin {
  /* Some plugin architectures like VST need to know about the number of
     control parameters of a plugin before the plugin is instantiated. The
     following static method returns this number and also initializes all the
     other static plugin data. */
  static int ncontrols();

  // Static data. This is the same for all plugin instances.

  /* Recorded result from previous invocation of ncontrols(). Normally this is
     a nonnegative number indicating the number of (input and output)
     controls. If this is -1 then the plugin failed to load for some reason
     and all attempts to create plugin instances will fail. */
  static int res;

  /* Whether we own the port data. This is set to true by default, so that the
     required storage will be allocated and deallocated when a plugin instance
     is created. On plugin architectures like LV2, where the host provides the
     storage instead, this must be set to false before creating any plugin
     instances. */
  static bool owner;

  // Global interpreter instance.
  static pure_interp *interp;
  
  // Total number of ports.
  static uint32_t n;
  // Port symbols and names (index range: 0..n-1).
  static char **sym, **name;
  // Port types and flags.
  static uint8_t *ty;
  static uint16_t *flags;
  // Ranges, step sizes, default values and unit names of CV and control ports.
  // These values may be NAN to indicate a missing value.
  static float *mins, *maxs, *steps, *defs;
  // Unit names of CV and control ports (0 indicates no unit).
  static char **units;

  // Number and port indices of audio input/output ports (index range of in:
  // 0..n_in-1, out: 0..n_out-1).
  static uint32_t n_in, n_out, *in, *out;

  // Number and port indices of event input/output ports (index range of evin:
  // 0..n_evin-1, evout: 0..n_evout-1).
  static uint32_t n_evin, n_evout, *evin, *evout;

  // Number and port indices of the control ports (index range: 0..n_ctl-1).
  static uint32_t n_ctl, *ctl;

  // Instance data. These belong to a particular plugin instance.

  // current sample rate and block size
  double rate;
  uint32_t nsamples;

  // activation and running status
  bool active, running;

  // plugin function
  pure_expr *fun;

  /* Port data (index range: 0..n-1). The type of the port data depends on the
     architecture and the port type. For control ports, it is always a single
     float value. */
  void **data;

  // Control data storage (only if own data is used).
  float *control_data;

  // MIDI event queues (input/output).
  std::vector<PureMidiData> midiin, midiout;

  // Plugin instantiation and destruction.
  PurePlugin(double rate = 0.0);
  ~PurePlugin();

  // Check whether the plugin was instantiated correctly.
  bool ok() { return res>=0 && fun; }

  // Receive and send MIDI events.
  void receive_midi(int port, int offs, int sz, unsigned char *data);
  void send_midi(send_midi_cb cb, void *user_data);
  // This resets the MIDI buffers for the next cycle. It is invoked
  // automatically at the end of send_midi(), so normally it shouldn't be
  // necessary to call this explicitly.
  void reset_midi();

  /* Suspend and resume. This also calls into the plugin function to inform
     the Pure plugin about the change of state. Note that it is up to the host
     plugin architecture if the process callback is run when a plugin is in
     suspended state. */
  void activate(bool state);
  void suspend() { activate(false); }
  void resume() { activate(true); }

  /* Process callback. This is the core of the Pure plugin. It takes care of
     invoking the plugin function with the appropriate audio and MIDI
     inputs/outputs, and is to be invoked by the plugin architecture's audio
     callback after setting up the MIDI input queue for the current audio
     block. For plugin architectures which pass the audio input/output buffers
     in their audio callback, they can be provided here; if the architecture
     sets these up in some other way (e.g., LV2) then these parameters can be
     omitted. After executing this method the architecture's callback should
     pick up and output any MIDI data generated by the plugin via the
     send_midi() method. */
  void process(float **inputs = NULL, float **outputs = NULL,
	       int blocksz = 0);

  // Helper methods to access the plugin data on the Pure side. NOTE: These
  // methods *must* be virtual so that they are invoked from the right
  // instance.

  // General information about the plugin (manifest data, audio and MIDI
  // input/output port numbers).
  virtual pure_expr *manifest();
  virtual pure_expr *audio_inputs();
  virtual pure_expr *audio_outputs();
  virtual pure_expr *midi_inputs();
  virtual pure_expr *midi_outputs();

  // Get and set port values.
  virtual pure_expr *get(int port);
  virtual pure_expr *set(int port, pure_expr *x);
};

#endif /* __pureplugin_hh__ */
