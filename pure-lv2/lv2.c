
/* Some interface operations for use by LV2 Pure plugins. 2014-02-28 Copyright
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

/* Get a port value. The port is specified using its index. For a control port
   this yields a double, for audio and CV ports a double vector (containing a
   block of audio samples), for MIDI ports a list of int vectors, where each
   vector contains the bytes of a single MIDI message. This operation fails if
   the port number is out of range or the port data isn't recognized. */

pure_expr *lv2pure_get(lv2plugin_t *p, int k)
{
  if (!p || k<0 || k>=p->n || !p->data[k]) return 0;
  // FIXME: We should preallocate some static Pure vectors here, to avoid
  // dynamic allocations as much as possible.
  switch (p->ty[k]) {
  case 1:
    return pure_double(*(float*)p->data[k]);
  case 2: case 3:
    return matrix_from_float_array(1, p->nsamples, p->data[k]);
  case 4:
    if (p->ty[k] & 4) {
      LV2_Atom_Sequence *seq = (LV2_Atom_Sequence*)p->data[k];
      LV2_Atom_Event* i;
      size_t n = 0;
      pure_expr **xv;
      LV2_ATOM_SEQUENCE_FOREACH(seq, ev) {
	if (ev->body.type == p->midi_event) n++;
      }
      if (n == 0) return pure_listv(0, 0);
      xv = alloca(n*sizeof(pure_expr*));
      n = 0;
      LV2_ATOM_SEQUENCE_FOREACH(seq, ev) {
	if (ev->body.type == p->midi_event) {
	  uint8_t *data = (uint8_t*)(ev+1);
	  uint32_t size = ev->body.size;
	  int *v = (int*)alloca(size*sizeof(int));
	  for (uint32_t k = 0; k < size; k++)
	    v[k] = data[k];
	  xv[n++] = matrix_from_int_array(1, size, v);
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
  if (!p || k<0 || k>=p->n || !p->data[k]) return 0;
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
    if (p->flags[k] & 4) {
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
	  void *data; uint8_t *v;
	  if (!pure_is_int_matrix(xv[i], &data) || matrix_size(xv[i]) == 0 ||
	      !(v = matrix_to_byte_array(NULL, xv[i]))) {
#if 1 // you might want to enable this for debugging purposes
	    char *s = str(xv[i]);
	    fprintf(stderr, "%s: dropped invalid MIDI event '%s'\n", p->uri, s);
	    free(s);
#endif
	    continue;
	  }
	  uint32_t k = matrix_size(xv[i]);
	  ret = forge_midi(p, 0, v, k);
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
