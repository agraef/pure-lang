
/* Some interface operations for use by Pure PlugR plugins. 2014-12-12
   Copyright (c) 2014 by Albert Gräf <aggraef@gmail.com>. Distributed under
   the LGPL, please see the accompanying COPYING and COPYING.LESSER files for
   details. */

#include "pureplugin.hh"

// These are all just trivial wrappers around the corresponding data and
// methods in the PurePlugin class, cf. pureplugin.cc.

/* Retrieve the port information (i.e., the "manifest") of the plugin. */

extern "C"
pure_expr *plugr_info(PurePlugin *p)
{
  if (!p) return 0;
  return p->manifest();
}

/* Check whether the plugin has been activated. */

extern "C"
int plugr_active(PurePlugin *p)
{
  if (!p) return 0;
  return p->active;
}

/* Return the sample rate at which the plugin runs. */

extern "C"
double plugr_rate(PurePlugin *p)
{
  if (!p) return 0.0;
  return p->rate;
}

/* Return the current block size. */

extern "C"
int plugr_nsamples(PurePlugin *p)
{
  if (!p) return 0;
  return p->nsamples;
}

/* Return the audio inputs and outputs of the plugin, as a list of port
   indices. */

extern "C"
pure_expr *plugr_audio_inputs(PurePlugin *p)
{
  if (!p) return 0;
  return p->audio_inputs();
}

extern "C"
pure_expr *plugr_audio_outputs(PurePlugin *p)
{
  if (!p) return 0;
  return p->audio_outputs();
}

/* Return the MIDI inputs and outputs of the plugin, as a list of port
   indices. */

extern "C"
pure_expr *plugr_midi_inputs(PurePlugin *p)
{
  if (!p) return 0;
  return p->midi_inputs();
}

extern "C"
pure_expr *plugr_midi_outputs(PurePlugin *p)
{
  if (!p) return 0;
  return p->midi_outputs();
}

/* Functions to read and write port values. */

extern "C"
pure_expr *plugr_get(PurePlugin *p, int k)
{
  if (!p) return 0;
  return p->get(k);
}

extern "C"
pure_expr *plugr_set(PurePlugin *p, int k, pure_expr *x)
{
  if (!p) return 0;
  return p->set(k, x);
}
