#ifndef __lv2pure_h__
#define __lv2pure_h__

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <lv2/lv2plug.in/ns/lv2core/lv2.h>
#include <lv2/lv2plug.in/ns/ext/urid/urid.h>
#include <lv2/lv2plug.in/ns/ext/atom/atom.h>
#include <lv2/lv2plug.in/ns/ext/atom/forge.h>
#include <lv2/lv2plug.in/ns/ext/time/time.h>

typedef struct {
  // features
  LV2_URID_Map* map;

  // URIs
  uint32_t atom_chunk, atom_sequence, midi_event,
    atom_blank, atom_float, atom_double, atom_int, atom_long,
    time_pos, time_beat, time_bpm, time_speed, time_bar,
    time_beats_per_bar, time_beat_unit;

  // atom forge
  LV2_Atom_Forge forge;
  LV2_Atom_Forge_Frame frame;

  // sample rate, block size and plugin path and uri
  double rate;
  uint32_t nsamples;
  char *path;
  const char *uri;

  // activation and running status
  bool active, running;

  // Plugin function.
  pure_expr *fun;

  // Total number of ports.
  uint32_t n;
  // Port names and symbols.
  char **sym, **name;
  // Port types and flags (index range: 0..n-1).
  uint8_t *ty;
  uint16_t *flags;
  // Ranges and default values of control ports (0..n-1).
  float *mins, *maxs, *defs;
  // Port data (0..n-1).
  void **data;

  // Number and port indices of audio/CV input/output ports (index range of
  // in: 0..n_in-1, out: 0..n_out-1).
  uint32_t n_in, n_out, *in, *out;

  // Number and port indices of atom/event input/output ports (index range of
  // evin: 0..n_evin-1, evout: 0..n_evout-1).
  uint32_t n_evin, n_evout, *evin, *evout;
} lv2plugin_t;

#endif /* __lv2pure_h__ */
