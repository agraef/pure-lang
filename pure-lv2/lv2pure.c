
/* Generic Pure plugin for LV2. 2014-02-28 Copyright (c) 2014 by Albert Gräf
   <aggraef@gmail.com>. Distributed under the new BSD license, please see the
   accompanying COPYING file for details. */

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

#include <lv2/lv2plug.in/ns/ext/atom/atom.h>
#include <lv2/lv2plug.in/ns/ext/atom/forge.h>
#include <lv2/lv2plug.in/ns/ext/atom/util.h>
#include <lv2/lv2plug.in/ns/ext/midi/midi.h>
#include <lv2/lv2plug.in/ns/ext/urid/urid.h>
#include <lv2/lv2plug.in/ns/ext/dynmanifest/dynmanifest.h>
#include <lv2/lv2plug.in/ns/lv2core/lv2.h>

#define MIDI_EVENT_URI "http://lv2plug.in/ns/ext/midi#MidiEvent"

// You'll have to set these if you want to compile your own plugins.
// (The pure2lv2 script automatically takes care of this.)
#ifndef URI_PREFIX
#define URI_PREFIX "http://purelang.bitbucket.org/"
#endif
#ifndef PLUGIN_NAME
#define PLUGIN_NAME "lv2pure"
#endif
#ifndef LOADER_NAME
#define LOADER_MAIN __lv2pure_main__
#endif

#define PLUGIN_URI URI_PREFIX PLUGIN_NAME

// You can also define this if the main function of your plugin is named
// something else than `plugin`. But this should rarely be necessary.
#ifndef PLUGIN_FUN
#define PLUGIN_FUN "plugin"
#endif

#include "lv2pure.h"

// This is the main entry point in the batch-compiled Pure module.
extern void LOADER_NAME(int argc, char** argv);

static void connect_port(LV2_Handle instance, uint32_t port, void* data)
{
  lv2plugin_t* plugin = (lv2plugin_t*)instance;
  if (port >= plugin->n)
    fprintf(stderr,
	    "%s: couldn't connect port %u, port index out of range\n",
	    PLUGIN_URI, port);
  else if (plugin->data[port] != data) {
    plugin->data[port] = data;
#if 0
    fprintf(stderr, "%s: port %u connected to %p\n", PLUGIN_URI, port, data);
#endif
  }
}

static bool check(unsigned i, const char *s, bool chk)
{
  if (!chk)
    fprintf(stderr, "%s: port #%u: bad %s\n", PLUGIN_URI, i, s);
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

static lv2plugin_t *create_plugin(void)
{
  pure_interp *interp = pure_current_interp();
  lv2plugin_t *plugin = calloc(1, sizeof(lv2plugin_t));
  assert(plugin);

  // Create an interpreter instance.
  LOADER_NAME(0, 0);
  plugin->interp = pure_current_interp();
  if (!plugin->interp) {
    fprintf(stderr, "%s: couldn't load Pure interpreter\n", PLUGIN_URI);
    goto fail;
  }

  // Get the plugin function.
  pure_expr *e;
  plugin->fun = pure_symbolx(pure_sym(PLUGIN_FUN), &e);
  if (!plugin->fun) {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: unhandled exception '%s'\n", PLUGIN_URI, s);
      free(s);
      pure_freenew(e);
    }
    pure_delete_interp(plugin->interp);
    goto fail;
  }
  pure_new(plugin->fun);

  // Create a pointer object representing ourselves and pass it as the first
  // argument to the plugin function.
  pure_expr *p = pure_tag(pure_pointer_tag("LV2Plugin*"), pure_pointer(plugin));
  if (!p) goto fail2;
  pure_expr *fun = pure_appx(plugin->fun, p, &e);
  if (!fun) {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: unhandled exception '%s'\n", PLUGIN_URI, s);
      free(s);
      pure_freenew(e);
    }
    goto fail2;
  }
  // Replace the plugin function with the constructed application, so that the
  // plugin pointer argument is passed on all subsequent invocations.
  pure_new(fun);
  pure_free(plugin->fun);
  plugin->fun = fun;

  // Invoke the plugin function to get the port information.
  pure_expr *info =
    pure_appx(plugin->fun, pure_symbol(pure_sym("lv2::info")), &e);
  size_t n;
  pure_expr **xv;
  bool ok = info && pure_is_listv(info, &n, &xv);
  if (ok) {
    uint32_t k_in = 0, k_out = 0, k_evin = 0, k_evout = 0;
    plugin->sym = calloc(n, sizeof(char*));
    plugin->name = calloc(n, sizeof(char*));
    plugin->ty = calloc(n, sizeof(uint8_t));
    plugin->flags = calloc(n, sizeof(uint16_t));
    plugin->mins = calloc(n, sizeof(float));
    plugin->maxs = calloc(n, sizeof(float));
    plugin->defs = calloc(n, sizeof(float));
    plugin->data = calloc(n, sizeof(void*));
    plugin->in = calloc(n, sizeof(uint32_t));
    plugin->out = calloc(n, sizeof(uint32_t));
    plugin->evin = calloc(n, sizeof(uint32_t));
    plugin->evout = calloc(n, sizeof(uint32_t));
    for (unsigned i = 0; i < n; i++) {
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
      if (m == 0 || m > 7) {
	fprintf(stderr, "%s: port #%u: bad port description\n", PLUGIN_URI, i);
	m = 0;
      }
      if (m > 0 && check(i, "symbol", pure_is_cstring_dup(yv[0], &s)))
	plugin->sym[i] = s;
      else {
	char s[10];
	sprintf(s, "port#%u", i);
	plugin->sym[i] = strdup(s);
      }
      if (m > 1 && pure_is_cstring_dup(yv[1], &s)) {
	l = 1;
	plugin->name[i] = s;
      } else
	plugin->name[i] = strdup(plugin->sym[i]);
      l++;
      if (m > l && check(i, "type", pure_is_int(yv[l], &k) && k>0 && k<5))
	plugin->ty[i] = k;
      else
	plugin->ty[i] = 1;
      l++;
      k = (plugin->ty[i] == 4)?4:0;
      // Standard LV2 hosts like jalv don't seem to like ports which are both
      // input (bit 1 of flags set) and output (bit 2) ports, so make sure
      // that it is either one or the other.
      if (m > l && check(i, "flags", pure_is_int(yv[l], &k) &&
			 (k&3) != 0 && (k&3) != 3))
	plugin->flags[i] = k;
      else
	plugin->flags[i] = 1 | (k&4);
      l++;
      if (m > l && check(i, "default value", pure_is_number(yv[l], &x)))
	plugin->defs[i] = x;
      else
	plugin->defs[i] = NAN;
      l++;
      if (m > l && check(i, "min value", pure_is_number(yv[l], &x)))
	plugin->mins[i] = x;
      else
	plugin->mins[i] = NAN;
      l++;
      if (m > l && check(i, "max value", pure_is_number(yv[l], &x)))
	plugin->maxs[i] = x;
      else
	plugin->maxs[i] = NAN;
      bool is_audio = plugin->ty[i] == 2 || plugin->ty[i] == 3;
      bool is_midi = plugin->ty[i] == 4 && (plugin->flags[i]&4);
      bool is_input = plugin->flags[i]&1;
      bool is_output = plugin->flags[i]&2;
      if (is_audio) {
	// audio or CV port
	if (is_input)
	  plugin->in[k_in++] = i;
	if (is_output)
	  plugin->out[k_out++] = i;
      }
      if (is_midi) {
	// MIDI port
	if (is_input)
	  plugin->evin[k_evin++] = i;
	if (is_output)
	  plugin->evout[k_evout++] = i;
      }
      if (yv) free(yv);
    }
    if (xv) free(xv);
    pure_freenew(info);
    if (!(plugin->in = realloc(plugin->in, k_in*sizeof(uint32_t))))
      k_in = 0;
    if (!(plugin->out = realloc(plugin->out, k_out*sizeof(uint32_t))))
      k_out = 0;
    if (!(plugin->evin = realloc(plugin->evin, k_evin*sizeof(uint32_t))))
      k_evin = 0;
    if (!(plugin->evout = realloc(plugin->evout, k_evout*sizeof(uint32_t))))
      k_evout = 0;
    plugin->n = n;
    plugin->n_in = k_in; plugin->n_out = k_out;
    plugin->n_evin = k_evin; plugin->n_evout = k_evout;
#if 0
    fprintf(stderr, "%s: successfully loaded plugin\n%u ports, %u/%u audio ports, %u/%u midi ports\n",
	    PLUGIN_URI, plugin->n, plugin->n_in, plugin->n_out,
	    plugin->n_evin, plugin->n_evout);
#endif
  } else if (info) {
    char *s = str(info);
    fprintf(stderr, "%s: bad plugin info '%s'\n", PLUGIN_URI, s);
    free(s);
    pure_freenew(info);
    goto fail2;
  } else {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: bad plugin info (unhandled exception '%s')\n",
	      PLUGIN_URI, s);
      free(s);
      pure_freenew(e);
    } else
      fprintf(stderr, "%s: bad plugin info (unknown error)\n", PLUGIN_URI);
    goto fail2;
  }

  plugin->uri = PLUGIN_URI;
  pure_switch_interp(interp);
  return plugin;

 fail2:
  pure_free(plugin->fun);
  pure_delete_interp(plugin->interp);
  
 fail:
  pure_switch_interp(interp);
  free(plugin);
  return 0;
}

static LV2_Handle
instantiate(const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               path,
            const LV2_Feature* const* features)
{
  // Scan host features for URID map.
  LV2_URID_Map* map;
  for (int i = 0; features[i]; i++) {
    if (!strcmp(features[i]->URI, LV2_URID_URI "#map"))
      map = (LV2_URID_Map*)features[i]->data;
  }
  if (!map) {
    fprintf(stderr, "%s: host doesn't support urid:map, giving up\n",
	    PLUGIN_URI);
    return 0;
  }

  // Create the plugin.
  lv2plugin_t *plugin = create_plugin();
  if (!plugin) return 0;

  // Get the URIs that we need.
  plugin->atom_chunk = map->map(map->handle, LV2_ATOM__Chunk);
  plugin->atom_sequence = map->map(map->handle, LV2_ATOM__Sequence);
  plugin->midi_event = map->map(map->handle, MIDI_EVENT_URI);
  plugin->atom_blank = map->map(map->handle, LV2_ATOM__Blank);
  plugin->atom_float = map->map(map->handle, LV2_ATOM__Float);
  plugin->atom_double = map->map(map->handle, LV2_ATOM__Double);
  plugin->atom_int = map->map(map->handle, LV2_ATOM__Int);
  plugin->atom_long = map->map(map->handle, LV2_ATOM__Long);
  plugin->time_pos = map->map(map->handle, LV2_TIME__Position);
  plugin->time_beat = map->map(map->handle, LV2_TIME__barBeat);
  plugin->time_bpm = map->map(map->handle, LV2_TIME__beatsPerMinute);
  plugin->time_speed = map->map(map->handle, LV2_TIME__speed);
  plugin->time_bar = map->map(map->handle, LV2_TIME__bar);
  plugin->time_beats_per_bar = map->map(map->handle, LV2_TIME__beatsPerBar);
  plugin->time_beat_unit = map->map(map->handle, LV2_TIME__beatUnit);

  plugin->map = map;

  // Initialize the atom forge (used to output MIDI events).
  lv2_atom_forge_init(&plugin->forge, plugin->map);

  // Record sample rate and plugin path.
  plugin->rate = rate;
  plugin->path = path?strdup(path):0;

  return (LV2_Handle)plugin;
}

static void cleanup(LV2_Handle instance)
{
  lv2plugin_t* plugin = (lv2plugin_t*)instance;
  if (plugin->interp) {
    pure_interp *interp = pure_current_interp();
    pure_switch_interp(plugin->interp);
    //pure_free(plugin->fun);
    pure_delete_interp(plugin->interp);
    pure_switch_interp(interp);
  }
  if (plugin->path) free(plugin->path);
  if (plugin->sym) {
    for (uint32_t i = 0; i < plugin->n; i++)
      free(plugin->sym[i]);
    free(plugin->sym);
  }
  if (plugin->name) {
    for (uint32_t i = 0; i < plugin->n; i++)
      free(plugin->name[i]);
    free(plugin->name);
  }
  if (plugin->ty) free(plugin->ty);
  if (plugin->flags) free(plugin->flags);
  if (plugin->mins) free(plugin->mins);
  if (plugin->maxs) free(plugin->maxs);
  if (plugin->defs) free(plugin->defs);
  if (plugin->data) free(plugin->data);
  if (plugin->in) free(plugin->in);
  if (plugin->out) free(plugin->out);
  if (plugin->evin) free(plugin->evin);
  if (plugin->evout) free(plugin->evout);
  free(plugin);
}

static void activate(LV2_Handle instance)
{
  lv2plugin_t* plugin = (lv2plugin_t*)instance;
  plugin->active = true;
}

static void deactivate(LV2_Handle instance)
{
  lv2plugin_t* plugin = (lv2plugin_t*)instance;
  plugin->active = false;
}

static void run(LV2_Handle instance, uint32_t sample_count)
{
  lv2plugin_t* plugin = (lv2plugin_t*)instance;
  pure_interp *interp = pure_current_interp();
  pure_switch_interp(plugin->interp);
  plugin->nsamples = sample_count;
  pure_expr *e, *ret = pure_appx(plugin->fun, pure_tuplel(0, 0), &e);
  if (!ret) {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: unhandled exception '%s'\n", PLUGIN_URI, s);
      free(s);
      pure_freenew(e);
    }
  } else
    pure_freenew(ret);
  pure_switch_interp(interp);
}

const void* extension_data(const char* uri)
{
  return NULL;
}

static const LV2_Descriptor descriptor = {
  PLUGIN_URI,
  instantiate,
  connect_port,
  activate,
  run,
  deactivate,
  cleanup,
  extension_data
};

extern LV2_SYMBOL_EXPORT const LV2_Descriptor*
lv2_descriptor(uint32_t index)
{
  switch (index) {
  case 0:
    return &descriptor;
  default:
    return NULL;
  }
}

// Dynamic manifest. This requires that your LV2 host has dynamic manifests
// enabled.

extern LV2_SYMBOL_EXPORT
int lv2_dyn_manifest_open(LV2_Dyn_Manifest_Handle *handle,
			  const LV2_Feature *const *features)
{
  // Create a dummy instance of the plugin so that we can inspect the port
  // information.
  lv2plugin_t *plugin = create_plugin();
  *handle = (LV2_Dyn_Manifest_Handle)plugin;
  if (plugin) {
    // We can get rid of the interpreter now, only the port data is needed.
    pure_interp *interp = pure_current_interp();
    pure_switch_interp(plugin->interp);
    pure_delete_interp(plugin->interp);
    pure_switch_interp(interp);
    plugin->interp = 0;
    return 0;
  } else
    return -1;
}

extern LV2_SYMBOL_EXPORT
int lv2_dyn_manifest_get_subjects(LV2_Dyn_Manifest_Handle handle,
				  FILE *fp)
{
  fprintf(fp, "@prefix lv2:  <http://lv2plug.in/ns/lv2core#> .\n\
<%s> a lv2:Plugin .\n", PLUGIN_URI);
  return 0;
}

extern LV2_SYMBOL_EXPORT
int lv2_dyn_manifest_get_data(LV2_Dyn_Manifest_Handle handle,
			      FILE *fp,
			      const char *uri)
{
  lv2plugin_t *plugin = (lv2plugin_t*)handle;
  fprintf(fp, "@prefix doap:  <http://usefulinc.com/ns/doap#> .\n\
@prefix foaf:  <http://xmlns.com/foaf/0.1/> .\n\
@prefix lv2:   <http://lv2plug.in/ns/lv2core#> .\n\
@prefix epp:   <http://lv2plug.in/ns/ext/port-props#> .\n\
@prefix atom:  <http://lv2plug.in/ns/ext/atom#> .\n\
@prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\
@prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> .\n\
@prefix units: <http://lv2plug.in/ns/extensions/units#> .\n\
@prefix time:  <http://lv2plug.in/ns/ext/time#> .\n\
<%s>\n\
       a lv2:Plugin ;\n\
       doap:name \"%s\" ;\n\
       lv2:binary <%s.so> ;\n\
       lv2:optionalFeature epp:supportsStrictBounds ;\n\
       lv2:optionalFeature lv2:hardRtCapable ;\n",
	  PLUGIN_URI, PLUGIN_NAME, PLUGIN_NAME);
  for (unsigned i = 0; i < plugin->n; i++) {
    fprintf(fp, "%s [\n", i==0?"    lv2:port":" ,");
    switch (plugin->ty[i]) {
    case 1: // control port
      fprintf(fp, "\
	a lv2:ControlPort ;\n\
	a lv2:%sPort ;\n\
	lv2:index %d ;\n\
	lv2:symbol \"%s\" ;\n\
	lv2:name \"%s\" ;\n",
	      ((plugin->flags[i]&3)==1)?"Input":
	      ((plugin->flags[i]&3)==2)?"Output":"",
	      i, plugin->sym[i], plugin->name[i]);
      if (plugin->flags[i]&16)
	fprintf(fp, "\
	lv2:portProperty lv2:toggled ;\n");
      if (plugin->flags[i]&32)
	fprintf(fp, "\
	lv2:portProperty lv2:integer ;\n");
      // special port designations for time/transport information (Qtractor)
      uint8_t bit = 5;
      if (plugin->flags[i]&(1<<++bit))
	fprintf(fp, "\
	lv2:designation time:position ;\n");
      if (plugin->flags[i]&(1<<++bit))
	fprintf(fp, "\
	lv2:designation time:bar ;\n");
      if (plugin->flags[i]&(1<<++bit))
	fprintf(fp, "\
	lv2:designation time:beat ;\n");
      if (plugin->flags[i]&(1<<++bit))
	fprintf(fp, "\
	lv2:designation time:beatUnit ;\n");
      if (plugin->flags[i]&(1<<++bit))
	fprintf(fp, "\
	lv2:designation time:beatsPerBar ;\n");
      if (plugin->flags[i]&(1<<++bit))
	fprintf(fp, "\
	lv2:designation time:beatsPerMinute ;\n");
      if (plugin->flags[i]&(1<<++bit))
	fprintf(fp, "\
	lv2:designation time:frame ;\n");
      if (plugin->flags[i]&(1<<++bit))
	fprintf(fp, "\
	lv2:designation time:framesPerSecond ;\n");
      if (plugin->flags[i]&(1<<++bit))
	fprintf(fp, "\
	lv2:designation time:speed ;\n");
      if (!isnan(plugin->mins[i]) || !isnan(plugin->maxs[i]))
	fprintf(fp, "\
        lv2:portProperty epp:hasStrictBounds ;\n");
      if (!isnan(plugin->mins[i]))
	fprintf(fp, "\
	lv2:minimum %g ;\n", plugin->mins[i]);
      if (!isnan(plugin->maxs[i]))
	fprintf(fp, "\
	lv2:maximum %g ;\n", plugin->maxs[i]);
      if (!isnan(plugin->defs[i]))
	fprintf(fp, "\
	lv2:default %g ;\n", plugin->defs[i]);
      break;
    case 2: // audio port
      fprintf(fp, "\
	a lv2:AudioPort ;\n\
	a lv2:%sPort ;\n\
	lv2:index %d ;\n\
	lv2:symbol \"%s\" ;\n\
	lv2:name \"%s\" ;\n",
	      ((plugin->flags[i]&3)==1)?"Input":
	      ((plugin->flags[i]&3)==2)?"Output":"",
	      i, plugin->sym[i], plugin->name[i]);
      break;
    case 3: // CV port
      fprintf(fp, "\
	a lv2:CVPort ;\n\
	a lv2:%sPort ;\n\
	lv2:index %d ;\n\
	lv2:symbol \"%s\" ;\n\
	lv2:name \"%s\" ;\n",
	      ((plugin->flags[i]&3)==1)?"Input":
	      ((plugin->flags[i]&3)==2)?"Output":"",
	      i, plugin->sym[i], plugin->name[i]);
      if (!isnan(plugin->mins[i]) || !isnan(plugin->maxs[i]))
	fprintf(fp, "\
        lv2:portProperty epp:hasStrictBounds ;\n");
      if (!isnan(plugin->mins[i]))
	fprintf(fp, "\
	lv2:minimum %g ;\n", plugin->mins[i]);
      if (!isnan(plugin->maxs[i]))
	fprintf(fp, "\
	lv2:maximum %g ;\n", plugin->maxs[i]);
      if (!isnan(plugin->mins[i]))
	fprintf(fp, "\
	lv2:default %g ;\n", plugin->defs[i]);
      break;
    case 4: // atom (MIDI) port
      fprintf(fp, "\
	a atom:AtomPort ;\n\
	a lv2:%sPort ;\n\
	lv2:index %d ;\n\
	lv2:symbol \"%s\" ;\n\
	lv2:name \"%s\" ;\n\
	atom:bufferType atom:Sequence ;\n",
	      ((plugin->flags[i]&3)==1)?"Input":
	      ((plugin->flags[i]&3)==2)?"Output":"",
	      i, plugin->sym[i], plugin->name[i]);
      if (plugin->flags[i]&4)
	fprintf(fp, "\
	atom:supports <http://lv2plug.in/ns/ext/midi#MidiEvent> ;\n");
      // time/transport messages (Ardour)
      if (plugin->flags[i]&8)
	fprintf(fp, "\
	atom:supports time:Position ;\n");
      break;
    }
    fprintf(fp, "    ]");
  }
  fprintf(fp, "\n.\n");
  return 0;
}

extern LV2_SYMBOL_EXPORT
void lv2_dyn_manifest_close(LV2_Dyn_Manifest_Handle handle)
{
  lv2plugin_t *plugin = (lv2plugin_t*)handle;
  if (plugin) cleanup(plugin);
}
