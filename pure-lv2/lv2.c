
/* Some interface operations for use by Pure LV2 plugins. 2014-02-28 Copyright
   (c) 2014 by Albert Gräf <aggraef@gmail.com>. Distributed under the new BSD
   license, please see the accompanying COPYING file for details. */

#include <alloca.h>
#include <pure/runtime.h>
#include "lv2pure.h"
#include <lv2/lv2plug.in/ns/ext/atom/util.h>

/* Retrieve the port information (i.e., the "manifest") of the plugin. This
   should match the port descriptions returned by the plugin function when
   instantiating the plugin, but will have all invalid and missing parts
   filled in, so that a plugin may check exactly what port information was
   transmitted to the LV2 host. */

pure_expr *lv2pure_info(lv2plugin_t *p)
{
  if (!p) return 0;
  pure_expr **xv = calloc(p->n, sizeof(pure_expr*));
  for (unsigned i = 0; i < p->n; i++) {
    if (p->ty[i] == 1 || p->ty[i] == 3)
      xv[i] = pure_tuplel(7, pure_cstring_dup(p->sym[i]),
			  pure_cstring_dup(p->name[i]),
			  pure_int(p->ty[i]), pure_int(p->flags[i]),
			  pure_double(p->defs[i]),
			  pure_double(p->mins[i]),
			  pure_double(p->maxs[i]));
    else
      xv[i] = pure_tuplel(4, pure_cstring_dup(p->sym[i]),
			  pure_cstring_dup(p->name[i]),
			  pure_int(p->ty[i]), pure_int(p->flags[i]));
  }
  pure_expr *ret = pure_listv(p->n, xv); free(xv);
  return ret;
}

/* Check whether the plugin has been activated. The plugin may use this
   information in any desired way (e.g., an audio plugin may generate silence
   or pass through the input signal when deactivated). */

int lv2pure_active(lv2plugin_t *p)
{
  if (!p) return 0;
  return p->active;
}

/* Return the sample rate at which the plugin runs. */

double lv2pure_rate(lv2plugin_t *p)
{
  if (!p) return 0.0;
  return p->rate;
}

/* Return the current block size. */

int lv2pure_nsamples(lv2plugin_t *p)
{
  if (!p) return 0;
  return p->nsamples;
}

/* Return the audio inputs and outputs of the plugin, as a list of port
   indices. */

pure_expr *lv2pure_audio_inputs(lv2plugin_t *p)
{
  if (!p) return 0;
  size_t n = p->n_in;
  pure_expr **xv = (pure_expr**)calloc(n, sizeof(pure_expr*));
  for (size_t i = 0; i < n; i++)
    xv[i] = pure_int(p->in[i]);
  pure_expr *ret = pure_listv(n, xv); free(xv);
  return ret;
}

pure_expr *lv2pure_audio_outputs(lv2plugin_t *p)
{
  if (!p) return 0;
  size_t n = p->n_out;
  pure_expr **xv = (pure_expr**)calloc(n, sizeof(pure_expr*));
  for (size_t i = 0; i < n; i++)
    xv[i] = pure_int(p->out[i]);
  pure_expr *ret = pure_listv(n, xv); free(xv);
  return ret;
}

/* Return the MIDI inputs and outputs of the plugin, as a list of port
   indices. */

pure_expr *lv2pure_midi_inputs(lv2plugin_t *p)
{
  if (!p) return 0;
  size_t n = p->n_evin;
  pure_expr **xv = (pure_expr**)calloc(n, sizeof(pure_expr*));
  for (size_t i = 0; i < n; i++)
    xv[i] = pure_int(p->evin[i]);
  pure_expr *ret = pure_listv(n, xv); free(xv);
  return ret;
}

pure_expr *lv2pure_midi_outputs(lv2plugin_t *p)
{
  if (!p) return 0;
  size_t n = p->n_evout;
  pure_expr **xv = (pure_expr**)calloc(n, sizeof(pure_expr*));
  for (size_t i = 0; i < n; i++)
    xv[i] = pure_int(p->evout[i]);
  pure_expr *ret = pure_listv(n, xv); free(xv);
  return ret;
}

/* Return the plugin path. After plugin instantiation is complete, this points
   to the bundle directory, so that the plugin may find any auxiliary data
   files there. NOTE: This information is *not* yet available at instantiation
   time, i.e., when the plugin is first called with the `info` parameter to
   obtain the manifest. So, the retrieval of the plugin path *must* be
   deferred until instantiation is complete, i.e., until the plugin is
   actually run for the first time. */

pure_expr *lv2pure_path(lv2plugin_t *p)
{
  if (p && p->path)
    return pure_cstring_dup(p->path);
  else
    return 0;
}

// Helper function to process transport information.

static pure_expr *position(lv2plugin_t *p, const LV2_Atom_Object* obj)
{
  LV2_Atom *beat = NULL, *bpm = NULL, *speed = NULL, *bar = NULL,
    *beats_per_bar = NULL, *beat_unit = NULL;
  pure_expr *xv[5], *mapsto = pure_symbol(pure_sym("=>"));
  uint32_t n = 0;
  lv2_atom_object_get(obj,
		      p->time_beat, &beat,
		      p->time_bpm, &bpm,
		      p->time_beats_per_bar, &beats_per_bar,
		      p->time_beat_unit, &beat_unit,
		      p->time_speed, &speed,
		      p->time_bar, &bar,
		      NULL);
  if (bpm && bpm->type == p->atom_float) {
    // tempo
    const float _bpm = ((LV2_Atom_Float*)bpm)->body;
    xv[n++] = pure_appl(mapsto, 2, pure_symbol(pure_sym("lv2::bpm")),
			pure_double(_bpm));
  }
  if (beats_per_bar && beats_per_bar->type == p->atom_float &&
      beat_unit && beat_unit->type == p->atom_int) {
    // meter
    const float _beats_per_bar = ((LV2_Atom_Float*)beats_per_bar)->body;
    bool is_int = _beats_per_bar == (int)_beats_per_bar;
    pure_expr *x = is_int?pure_int((int)_beats_per_bar):
      pure_double(_beats_per_bar);
    const unsigned _beat_unit = ((LV2_Atom_Int*)beat_unit)->body;
    xv[n++] = pure_appl(mapsto, 2, pure_symbol(pure_sym("lv2::meter")),
			pure_tuplel(2, x, pure_int(_beat_unit)));
  }
  if (speed && speed->type == p->atom_float) {
    // speed change, e.g. 0 (stop) to 1 (play)
    const float _speed = ((LV2_Atom_Float*)speed)->body;
    xv[n++] = pure_appl(mapsto, 2, pure_symbol(pure_sym("lv2::speed")),
			pure_double(_speed));
  }
  if (bar && bar->type == p->atom_long) {
    // bar number
    const long _bar = ((LV2_Atom_Long*)bar)->body;
    xv[n++] = pure_appl(mapsto, 2, pure_symbol(pure_sym("lv2::bar")),
			pure_int(_bar));
  }
  if (beat && beat->type == p->atom_float) {
    // beat position
    const float _beat = ((LV2_Atom_Float*)beat)->body;
    xv[n++] = pure_appl(mapsto, 2, pure_symbol(pure_sym("lv2::beat")),
			pure_double(_beat));
  }
  pure_freenew(mapsto);
  return pure_matrix_columnsv(n, xv);
}

/* Functions to read and write port values. XXXFIXME: There seems to be no
   reliable way to determine the lifetime of the connected port data. We
   therefore enforce that these routines are only called inside the plugin's
   run() method which should always be safe. However, this seems overly
   restrictive for hosts that do offer persistent buffers. */

/* Get a port value. The port is specified using its index. For a control port
   this yields a double, for audio and CV ports a double vector (containing a
   block of audio samples). For MIDI ports the result is a list of pairs of
   ints and int vectors; the int value denotes a timestamp (frame number
   relative to the current block of audio samples), and the vector contains
   the bytes of a single MIDI message. This operation fails if the port number
   is out of range or the type of port data isn't recognized. */

pure_expr *lv2pure_get(lv2plugin_t *p, int k)
{
  if (!p || k<0 || k>=p->n || !p->data[k] || !p->running) return 0;
  // FIXME: We should preallocate some static Pure vectors here, to avoid
  // dynamic allocations as much as possible.
  switch (p->ty[k]) {
  case 1:
    return pure_double(*(float*)p->data[k]);
  case 2: case 3:
    return matrix_from_float_array(1, p->nsamples, p->data[k]);
  case 4:
    if ((p->flags[k] & 12) && (p->flags[k] & 1)) {
      LV2_Atom_Sequence *seq = (LV2_Atom_Sequence*)p->data[k];
      size_t n = 0;
      pure_expr **xv;
      bool have_midi = p->flags[k] & 4, have_time = p->flags[k] & 8;
      LV2_ATOM_SEQUENCE_FOREACH(seq, ev) {
	if (have_midi && ev->body.type == p->midi_event)
	  n++;
	else if (have_time && ev->body.type == p->atom_blank) {
	  const LV2_Atom_Object *obj = (const LV2_Atom_Object*)&ev->body;
	  if (obj->body.otype == p->time_pos) n++;
	}
      }
      if (n == 0) return pure_listv(0, 0);
      xv = alloca(n*sizeof(pure_expr*));
      n = 0;
      LV2_ATOM_SEQUENCE_FOREACH(seq, ev) {
	if (have_midi && ev->body.type == p->midi_event) {
	  uint8_t *data = (uint8_t*)(ev+1);
	  uint32_t size = ev->body.size;
	  int *v = (int*)alloca(size*sizeof(int));
	  for (uint32_t k = 0; k < size; k++)
	    v[k] = data[k];
	  // NOTE: ev->time.frames is actually a 64 byte value, but it seems
	  // that the 32 most significant bits are never used?
	  xv[n++] = pure_tuplel(2, pure_int((uint32_t)ev->time.frames),
				matrix_from_int_array(1, size, v));
	} else if (have_time && ev->body.type == p->atom_blank) {
	  const LV2_Atom_Object *obj = (const LV2_Atom_Object*)&ev->body;
	  if (obj->body.otype == p->time_pos) {
	    // transport information
	    xv[n++] = pure_tuplel(2, pure_int((uint32_t)ev->time.frames),
				  position(p, obj));
	  }
	}
      }
      return pure_listv(n, xv);
    } else
      return 0;
  default:
    return 0;
  }
}

// Helper function to write a MIDI message to a MIDI output port.
static bool forge_midi(lv2plugin_t* self,
		       uint32_t tme,
		       const uint8_t* const buffer,
		       uint32_t size)
{
  LV2_Atom midiatom;
  midiatom.type = self->midi_event;
  midiatom.size = size;
  bool ret = lv2_atom_forge_frame_time(&self->forge, tme) &&
    lv2_atom_forge_raw(&self->forge, &midiatom, sizeof(LV2_Atom)) &&
    lv2_atom_forge_raw(&self->forge, buffer, size);
  if (ret) lv2_atom_forge_pad(&self->forge, sizeof(LV2_Atom) + size);
  return ret;
}

/* Set a port value. The value must be in the same format as returned by
   lv2pure_get(), corresponding to the type of port. This operation fails if
   the port number is out of range or the given port data is invalid.
   Otherwise () is returned. */

pure_expr *lv2pure_set(lv2plugin_t *p, int k, pure_expr *x)
{
  if (!p || k<0 || k>=p->n || !p->data[k] || !p->running) return 0;
  switch (p->ty[k]) {
  case 1: {
    double d;
    if (pure_is_double(x, &d)) {
      *(float*)p->data[k] = d;
      return pure_tuplel(0, 0);
    } else
      return 0;
  }
  case 2: case 3:
    if (matrix_to_float_array(p->data[k], x))
      return pure_tuplel(0, 0);
    else
      return 0;
  case 4:
    if ((p->flags[k] & 4) && (p->flags[k] & 2)) {
      LV2_Atom_Sequence *seq = (LV2_Atom_Sequence*)p->data[k];
      size_t n;
      pure_expr **xv;
      // Prepare the MIDI output.
      const uint32_t capacity = seq->atom.size;
      lv2_atom_forge_set_buffer(&p->forge, (uint8_t*)seq, capacity);
      lv2_atom_forge_sequence_head(&p->forge, &p->frame, 0);
      if (pure_is_listv(x, &n, &xv)) {
	if (n == 0) return pure_tuplel(0, 0);
	bool ret = true;
	for (size_t i = 0; i < n; i++) {
	  uint32_t frames = 0;
	  size_t m;
	  pure_expr *x, **yv;
	  void *data; uint8_t *v;
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
	  ok = ok && pure_is_int_matrix(x, &data);
	  uint32_t k = ok?matrix_size(x):0;
	  ok = ok && k != 0 && (v = matrix_to_byte_array(NULL, x));
	  if (!ok) {
#if 0 // you might want to enable this for debugging purposes
	    char *s = str(x);
	    fprintf(stderr, "%s: dropped invalid MIDI event '%s'\n", p->uri, s);
	    free(s);
#endif
	    continue;
	  }
	  ret = forge_midi(p, frames, v, k);
 	  free(v);
	  if (!ret) {
#if 1
	    fprintf(stderr, "%s: MIDI buffer full (%u bytes)\n",
	      p->uri, seq->atom.size);
#endif
	    break;
	  }
	}
	free(xv);
#if 0
	printf("%s: wrote %u bytes\n", p->uri, seq->atom.size);
#endif
	return ret?pure_tuplel(0, 0):0;
      } else
	return 0;
    } else
      return 0;
  default:
    return 0;
  }
}
