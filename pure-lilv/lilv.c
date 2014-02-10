
/* A Pure Lilv module. This was written for Pure by aggraef@gmail.com, but
   most of the basic code for interfacing to the Lilv API was gleaned from
   various programs in drobilla's repository, in particular lv2info.c and
   jalv.c, see http://drobilla.net/software/lilv/. */

#define _POSIX_C_SOURCE 200809L  /* for strdup */

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pure/runtime.h>

#include <lv2/lv2plug.in/ns/ext/atom/atom.h>
#include <lv2/lv2plug.in/ns/ext/event/event.h>
#include <lv2/lv2plug.in/ns/ext/presets/presets.h>
#include <lv2/lv2plug.in/ns/ext/port-groups/port-groups.h>

#include <lilv/lilv.h>

// These seem to be missing in the lilv headers right now.
#define MY_URI_ATOM_PORT "http://lv2plug.in/ns/ext/atom#AtomPort"
#define MY_URI_CV_PORT "http://lv2plug.in/ns/lv2core/#CVPort"

pure_expr *lilv_plugins(LilvWorld* world)
{
  const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
  size_t k = 0, l = lilv_plugins_size(plugins);
  pure_expr **xv = calloc(l, sizeof(pure_expr*));
  LILV_FOREACH (plugins, i, plugins) {
    const LilvPlugin* p = lilv_plugins_get(plugins, i);
    LilvNode* n = lilv_plugin_get_name(p);
    assert(n);
    assert(k<l);
    xv[k++] = pure_tuplel
      (2, pure_cstring_dup(lilv_node_as_string(n)),
       pure_cstring_dup(lilv_node_as_uri(lilv_plugin_get_uri(p))));
    lilv_node_free(n);
  }
  return pure_listv(k, xv);
}

pure_expr *lilv_plugin_info(LilvWorld* world, const char* plugin_uri)
{
  // Collect information about a specific plugin.
  LilvNode* uri = lilv_new_uri(world, plugin_uri);
  if (!uri) return 0;
  const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
  const LilvPlugin* p = lilv_plugins_get_by_uri(plugins, uri);
  if (!p) return 0;
  // Cached URIs.
  LilvNode* input_class = lilv_new_uri(world, LILV_URI_INPUT_PORT);
  LilvNode* output_class = lilv_new_uri(world, LILV_URI_OUTPUT_PORT);
  LilvNode* audio_class = lilv_new_uri(world, LILV_URI_AUDIO_PORT);
  LilvNode* control_class = lilv_new_uri(world, LILV_URI_CONTROL_PORT);
  LilvNode* cv_class = lilv_new_uri(world, MY_URI_CV_PORT);
  LilvNode* event_class = lilv_new_uri(world, LILV_URI_EVENT_PORT);
  LilvNode* atom_class = lilv_new_uri(world, MY_URI_ATOM_PORT);
  LilvNode* preset_class = lilv_new_uri(world, LV2_PRESETS__Preset);
  LilvNode* label_pred = lilv_new_uri(world, LILV_NS_RDFS "label");
  LilvNode* group_pred = lilv_new_uri(world, LV2_PORT_GROUPS__group);
  LilvNode* designation_pred = lilv_new_uri(world, LV2_CORE__designation);
  LilvNode* midi_event = lilv_new_uri(world, LILV_URI_MIDI_EVENT);
  // Plugin name and URI.
  LilvNode* n = lilv_plugin_get_name(p);
  assert(n);
  pure_expr *p_name = pure_cstring_dup(lilv_node_as_string(n));
  pure_expr *p_uri = pure_cstring_dup(lilv_node_as_uri(lilv_plugin_get_uri(p)));
  lilv_node_free(n);
  // Class information (Instrument etc.).
  const LilvPluginClass* pclass = lilv_plugin_get_class(p);
  const LilvNode* class_label = lilv_plugin_class_get_label(pclass);
  pure_expr *p_class = pure_cstring_dup
    (class_label?lilv_node_as_string(class_label):"");
  // Author information.
  n = lilv_plugin_get_author_name(p);
  pure_expr *p_author = pure_cstring_dup(n?lilv_node_as_string(n):"");
  if (n) lilv_node_free(n);
  n = lilv_plugin_get_author_email(p);
  pure_expr *p_email = pure_cstring_dup(n?lilv_node_as_uri(n):"");
  if (n) lilv_node_free(n);
  n = lilv_plugin_get_author_homepage(p);
  pure_expr *p_homepage = pure_cstring_dup(n?lilv_node_as_uri(n):"");
  if (n) lilv_node_free(n);
  // Bundle (plugin directory) and binary (shared lib) URIs.
  pure_expr *p_bundle = pure_cstring_dup
    (lilv_node_as_uri(lilv_plugin_get_bundle_uri(p)));
  const LilvNode* binary_uri = lilv_plugin_get_library_uri(p);
  pure_expr *p_binary = pure_cstring_dup
    (binary_uri?lilv_node_as_uri(binary_uri):"");
  // Data URIs (ttl files).
  size_t k, l;
  pure_expr **xv;
  const LilvNodes* data_uris = lilv_plugin_get_data_uris(p);
  k = 0; l = lilv_nodes_size(data_uris);
  xv = calloc(l, sizeof(pure_expr*));
  if (data_uris) {
    LILV_FOREACH (nodes, i, data_uris) {
      assert(k<l);
      xv[k++] = pure_cstring_dup
	(lilv_node_as_uri(lilv_nodes_get(data_uris, i)));
    }
  }
  pure_expr *p_data_uris = pure_listv(k, xv);
  // Required and optional features, as declared by the plugin.
  LilvNodes* data = lilv_plugin_get_required_features(p);
  k = 0; l = lilv_nodes_size(data);
  xv = calloc(l, sizeof(pure_expr*));
  if (data) {
    LILV_FOREACH (nodes, i, data) {
      assert(k<l);
      xv[k++] = pure_cstring_dup
	(lilv_node_as_uri(lilv_nodes_get(data, i)));
    }
    lilv_nodes_free(data);
  }
  pure_expr *p_required = pure_listv(k, xv);
  data = lilv_plugin_get_optional_features(p);
  k = 0; l = lilv_nodes_size(data);
  xv = calloc(l, sizeof(pure_expr*));
  if (data) {
    LILV_FOREACH (nodes, i, data) {
      assert(k<l);
      xv[k++] = pure_cstring_dup
	(lilv_node_as_uri(lilv_nodes_get(data, i)));
    }
    lilv_nodes_free(data);
  }
  pure_expr *p_optional = pure_listv(k, xv);
  // Extension data.
  data = lilv_plugin_get_extension_data(p);
  k = 0; l = lilv_nodes_size(data);
  xv = calloc(l, sizeof(pure_expr*));
  if (data) {
    LILV_FOREACH (nodes, i, data) {
      assert(k<l);
      xv[k++] = pure_cstring_dup
	(lilv_node_as_uri(lilv_nodes_get(data, i)));
    }
    lilv_nodes_free(data);
  }
  pure_expr *p_extension_data = pure_listv(k, xv);
  // Presets.
  data = lilv_plugin_get_related(p, preset_class);
  k = 0; l = lilv_nodes_size(data);
  xv = calloc(l, sizeof(pure_expr*));
  if (data) {
    LILV_FOREACH (nodes, i, data) {
      pure_expr *plabel = 0;
      pure_expr *puri = pure_cstring_dup
	(lilv_node_as_uri(lilv_nodes_get(data, i)));
      const LilvNode* preset = lilv_nodes_get(data, i);
      lilv_world_load_resource(world, preset);
      LilvNodes* titles =
	lilv_world_find_nodes(world, preset, label_pred, NULL);
      if (titles) {
	const LilvNode* title = lilv_nodes_get_first(titles);
	assert(k<l);
	plabel = pure_cstring_dup(lilv_node_as_string(title));
	lilv_nodes_free(titles);
      }
      if (!plabel) plabel = pure_cstring_dup("");
      assert(k<l);
      xv[k++] = pure_tuplel(2, plabel, puri);
    }
    lilv_nodes_free(data);
  }
  pure_expr *p_presets = pure_listv(k, xv);
  // Ports.
  const uint32_t num_ports = lilv_plugin_get_num_ports(p);
  float* mins = (float*)calloc(num_ports, sizeof(float));
  float* maxs = (float*)calloc(num_ports, sizeof(float));
  float* defs = (float*)calloc(num_ports, sizeof(float));
  lilv_plugin_get_port_ranges_float(p, mins, maxs, defs);
  xv = calloc(num_ports, sizeof(pure_expr*));
  for (uint32_t i = 0; i < num_ports; ++i) {
    const LilvPort* port = lilv_plugin_get_port_by_index(p, i);
    if (!port) {
      // Just the port number signifies an invalid port.
      xv[i] = pure_int(i);
      assert(xv[i]);
      continue;
    }
    // Port symbol and verbose name.
    const LilvNode* sym = lilv_port_get_symbol(p, port);
    LilvNode* name = lilv_port_get_name(p, port);
    pure_expr *p_psym = pure_cstring_dup(lilv_node_as_string(sym)),
      *p_pname = pure_cstring_dup(lilv_node_as_string(name));
    lilv_node_free(name);
    // Check for input/output ports.
    bool in = lilv_port_is_a(p, port, input_class);
    bool out = lilv_port_is_a(p, port, output_class);
    // Check for MIDI event/atom port (set below).
    bool midi = false;
    // Port type and attributes (set below).
    int ty = 0;
    pure_expr *attr = 0;
    bool is_control;
    if ((is_control = lilv_port_is_a(p, port, control_class)) ||
	lilv_port_is_a(p, port, cv_class)) {
      // Control or CV port. The latter is like an audio port (so data is
      // transmitted as blocks of samples), but for control data, so the
      // special attributes of a control port should also be supported.
      ty = is_control?1:3;
      // List of scale points.
      LilvScalePoints* points = lilv_port_get_scale_points(p, port);
      size_t k = 0, l = lilv_scale_points_size(points);
      pure_expr **yv = calloc(l, sizeof(pure_expr*));
      pure_expr *mapsto = pure_symbol(pure_sym("=>"));
      LILV_FOREACH (scale_points, i, points) {
	const LilvScalePoint* point = lilv_scale_points_get(points, i);
	assert(k<l);
	yv[k++] = pure_appl
	  (mapsto, 2,
	   pure_cstring_dup
	   (lilv_node_as_string(lilv_scale_point_get_label(point))),
	   pure_double
	   (lilv_node_as_float(lilv_scale_point_get_value(point))));
      }
      pure_freenew(mapsto);
      lilv_scale_points_free(points);
      pure_expr *p_points = pure_listv(k, yv);
      // Note that some of the min/max/default values can be nan, which means
      // that they are undefined.
      attr = pure_tuplel
	(4, pure_double(mins[i]), pure_double(maxs[i]), pure_double(defs[i]),
	 p_points);
    } else if (lilv_port_is_a(p, port, audio_class)) {
      // Audio port (blocks of samples).
      ty = 2;
    } else if (lilv_port_is_a(p, port, atom_class)) {
      // New-style atom port, check for MIDI capability.
      ty = 4;
      midi = lilv_port_supports_event(p, port, midi_event);
    } else if (lilv_port_is_a(p, port, event_class)) {
      // Old-style event port, check for MIDI capability.
      ty = 5;
      midi = lilv_port_supports_event(p, port, midi_event);
    }
    // Port groups.
    LilvNodes* groups = lilv_port_get_value(p, port, group_pred);
    size_t k = 0, l = lilv_nodes_size(groups);
    pure_expr **yv = calloc(l, sizeof(pure_expr*));
    LILV_FOREACH (nodes, i, groups) {
      assert(k<l);
      yv[k++] = pure_cstring_dup
	(lilv_node_as_string(lilv_nodes_get(groups, i)));
    }
    lilv_nodes_free(groups);
    pure_expr *p_groups = pure_listv(k, yv);
    // Port designations.
    LilvNodes* designations = lilv_port_get_value(p, port, designation_pred);
    k = 0; l = lilv_nodes_size(designations);
    yv = calloc(l, sizeof(pure_expr*));
    LILV_FOREACH (nodes, i, designations) {
      assert(k<l);
      yv[k++] = pure_cstring_dup
	(lilv_node_as_string(lilv_nodes_get(designations, i)));
    }
    lilv_nodes_free(designations);
    pure_expr *p_designations = pure_listv(k, yv);
    // Port properties.
    LilvNodes* properties = lilv_port_get_properties(p, port);
    k = 0; l = lilv_nodes_size(properties);
    yv = calloc(l, sizeof(pure_expr*));
    LILV_FOREACH (nodes, i, properties) {
      assert(k<l);
      yv[k++] = pure_cstring_dup
	(lilv_node_as_uri(lilv_nodes_get(properties, i)));
    }
    lilv_nodes_free(properties);
    pure_expr *p_properties = pure_listv(k, yv);
    // Assemble the port info.
    if (attr)
      xv[i] = pure_tuplel(9, pure_int(i), p_psym, p_pname, pure_int(ty),
			  pure_int(in | (out<<1) | (midi<<2)),
			  p_groups, p_designations, p_properties, attr);
    else
      xv[i] = pure_tuplel(8, pure_int(i), p_psym, p_pname, pure_int(ty),
			  pure_int(in | (out<<1) | (midi<<2)),
			  p_groups, p_designations, p_properties);
    assert(xv[i]);
  }
  free(mins); free(maxs); free(defs);
  pure_expr *p_ports = pure_listv(num_ports, xv);
  lilv_node_free(input_class);
  lilv_node_free(output_class);
  lilv_node_free(audio_class);
  lilv_node_free(control_class);
  lilv_node_free(event_class);
  lilv_node_free(atom_class);
  lilv_node_free(preset_class);
  lilv_node_free(label_pred);
  lilv_node_free(group_pred);
  lilv_node_free(designation_pred);
  lilv_node_free(midi_event);
  return pure_tuplel(14, p_name, p_uri, p_class, p_author, p_email, p_homepage,
		     p_bundle, p_binary, p_data_uris, p_required, p_optional,
		     p_extension_data, p_presets, p_ports);
}

// Plugin host features that we support. This is only a minimal set right now,
// which will hopefully be enough to run most plugins.

// Note that the uri-map extension is deprecated, but since older plugins may
// still be using it instead of the newer urid extension, we still support it
// for now.

#include <lv2/lv2plug.in/ns/ext/uri-map/uri-map.h>
#include <lv2/lv2plug.in/ns/ext/urid/urid.h>

#define NS_EXT "http://lv2plug.in/ns/ext/"

static uint32_t uri_to_id
(LV2_URI_Map_Callback_Data callback_data, const char* map, const char* uri);
static LV2_URID map_uri(LV2_URID_Map_Handle handle, const char* uri);
static const char* unmap_uri(LV2_URID_Unmap_Handle handle, LV2_URID urid);

static LV2_URI_Map_Feature uri_map   = { NULL, &uri_to_id };
static LV2_URID_Map map              = { NULL, map_uri };
static LV2_URID_Unmap unmap          = { NULL, unmap_uri };

static LV2_Feature uri_map_feature   = { NS_EXT "uri-map", &uri_map };
static LV2_Feature map_feature       = { LV2_URID__map, &map };
static LV2_Feature unmap_feature     = { LV2_URID__unmap, &unmap };

const LV2_Feature* features[] = {
	&uri_map_feature, &map_feature, &unmap_feature,	NULL
};

/* Instantiate a plugin given by its URI. This also takes care of setting up
   the required data structures and binding the ports. You need to specify the
   sample rate and a maximum block size (the actual number of samples used in
   a run of the plugin can be adjusted as needed, but may not exceed this
   number). */

typedef struct {
  // Lilv plugin instance and associated data.
  LilvInstance *instance;
  double sample_rate;
  uint32_t block_size;
  // Total number of ports.
  uint32_t n;
  // Port names and symbols.
  char **sym, **name;
  // Port types and flags (index range: 0..n-1).
  uint8_t *ty, *flags;
  // Ranges and default values of control ports (0..n-1).
  float *mins, *maxs, *defs;
  // Control port data, sample buffers for audio/CV ports (0..n-1).
  float *data, **buffer;
  // Number and port indices of audio/CV input/output ports (index range of
  // in: 0..n_in-1, out: 0..n_out-1).
  uint32_t n_in, n_out, *in, *out;
} PluginInstance;

PluginInstance *lilv_plugin_new(LilvWorld* world, const char* plugin_uri,
				double sample_rate, uint32_t block_size)
{
  LilvNode* uri = lilv_new_uri(world, plugin_uri);
  if (!uri) return 0;
  const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
  const LilvPlugin* p = lilv_plugins_get_by_uri(plugins, uri);
  if (!p) return 0;
  PluginInstance *ret = (PluginInstance*)malloc(sizeof(PluginInstance));
  if (!ret) return 0;
  ret->instance = lilv_plugin_instantiate(p, sample_rate, features);
  if (!ret->instance) {
    free(ret);
    return 0;
  }
  ret->sample_rate = sample_rate;
  ret->block_size = block_size;
  // Cached URIs.
  LilvNode* input_class = lilv_new_uri(world, LILV_URI_INPUT_PORT);
  LilvNode* output_class = lilv_new_uri(world, LILV_URI_OUTPUT_PORT);
  LilvNode* audio_class = lilv_new_uri(world, LILV_URI_AUDIO_PORT);
  LilvNode* control_class = lilv_new_uri(world, LILV_URI_CONTROL_PORT);
  LilvNode* cv_class = lilv_new_uri(world, MY_URI_CV_PORT);
  LilvNode* event_class = lilv_new_uri(world, LILV_URI_EVENT_PORT);
  LilvNode* atom_class = lilv_new_uri(world, MY_URI_ATOM_PORT);
  LilvNode* midi_event = lilv_new_uri(world, LILV_URI_MIDI_EVENT);
  // Make a first pass through the port list to fill in the basic port data
  // and determine the number of audio/CV input/output ports. We also connect
  // all ports to their corresponding buffers here.
  ret->n = lilv_plugin_get_num_ports(p);
  ret->mins = (float*)calloc(ret->n, sizeof(float));
  ret->maxs = (float*)calloc(ret->n, sizeof(float));
  ret->defs = (float*)calloc(ret->n, sizeof(float));
  lilv_plugin_get_port_ranges_float(p, ret->mins, ret->maxs, ret->defs);
  ret->sym = (char**)calloc(ret->n, sizeof(char*));
  ret->name = (char**)calloc(ret->n, sizeof(char*));
  ret->ty = (uint8_t*)calloc(ret->n, sizeof(uint8_t));
  ret->flags = (uint8_t*)calloc(ret->n, sizeof(uint8_t));
  ret->data = (float*)calloc(ret->n, sizeof(float));
  ret->buffer = (float**)calloc(ret->n, sizeof(float*));
  ret->n_in = ret->n_out = 0;
  for (uint32_t i = 0; i < ret->n; ++i) {
    const LilvPort* port = lilv_plugin_get_port_by_index(p, i);
    if (!port) continue;
    // Port symbol and verbose name.
    const LilvNode* sym = lilv_port_get_symbol(p, port);
    LilvNode* name = lilv_port_get_name(p, port);
    ret->sym[i] = strdup(lilv_node_as_string(sym));
    ret->name[i] = strdup(lilv_node_as_string(name));
    lilv_node_free(name);
    // Check for input/output ports.
    bool in = lilv_port_is_a(p, port, input_class);
    bool out = lilv_port_is_a(p, port, output_class);
    // Check for MIDI event/atom port (set below).
    bool midi = false;
    if (lilv_port_is_a(p, port, control_class)) {
      // Control port.
      ret->ty[i] = 1;
      ret->buffer[i] = &ret->data[i];
      ret->data[i] = ret->defs[i];
      lilv_instance_connect_port(ret->instance, i, ret->buffer[i]);
    } else if (lilv_port_is_a(p, port, audio_class)) {
      // Audio port (blocks of samples).
      ret->ty[i] = 2;
      ret->buffer[i] = (float*)calloc(ret->block_size, sizeof(float));
      lilv_instance_connect_port(ret->instance, i, ret->buffer[i]);
      if (in) ret->n_in++; if (out) ret->n_out++;
    } else if (lilv_port_is_a(p, port, cv_class)) {
      // CV port (like an audio port, but for control data).
      ret->ty[i] = 3;
      ret->buffer[i] = (float*)calloc(ret->block_size, sizeof(float));
      lilv_instance_connect_port(ret->instance, i, ret->buffer[i]);
      if (in) ret->n_in++; if (out) ret->n_out++;
    } else if (lilv_port_is_a(p, port, atom_class)) {
      // New-style atom port, check for MIDI capability.
      ret->ty[i] = 4;
      midi = lilv_port_supports_event(p, port, midi_event);
      // TODO: set up MIDI event buffer
    } else if (lilv_port_is_a(p, port, event_class)) {
      // Old-style event port, check for MIDI capability.
      ret->ty[i] = 5;
      midi = lilv_port_supports_event(p, port, midi_event);
      // TODO: set up MIDI event buffer
    }
    ret->flags[i] = in | (out<<1) | (midi<<2);
  }
  // Second pass to fill in the audio/CV port indices.
  ret->in = (uint32_t*)calloc(ret->n_in, sizeof(uint32_t));
  ret->out = (uint32_t*)calloc(ret->n_out, sizeof(uint32_t));
  uint32_t k = 0, l = 0;
  for (uint32_t i = 0; i < ret->n; ++i) {
    const LilvPort* port = lilv_plugin_get_port_by_index(p, i);
    if (!port || (ret->ty[i] != 2 && ret->ty[i] != 3)) continue;
    if (ret->flags[i]&1)
      ret->in[k++] = i;
    if (ret->flags[i]&2)
      ret->out[l++] = i;
  }
  lilv_node_free(input_class);
  lilv_node_free(output_class);
  lilv_node_free(audio_class);
  lilv_node_free(control_class);
  lilv_node_free(event_class);
  lilv_node_free(atom_class);
  lilv_node_free(midi_event);
  return ret;
}

void lilv_plugin_free(PluginInstance *p)
{
  if (!p) return;
  lilv_instance_free(p->instance);
  if (p->sym) {
    for (uint32_t i = 0; i < p->n; i++)
      free(p->sym[i]);
    free(p->sym);
  }
  if (p->name) {
    for (uint32_t i = 0; i < p->n; i++)
      free(p->name[i]);
    free(p->name);
  }
  if (p->ty) free(p->ty);
  if (p->ty) free(p->ty);
  if (p->flags) free(p->flags);
  if (p->mins) free(p->mins);
  if (p->maxs) free(p->maxs);
  if (p->defs) free(p->defs);
  if (p->data) free(p->data);
  // Get rid of the audio/CV buffers.
  for (uint32_t i = 0; i < p->n_in; i++) {
    const uint32_t k = p->in[i];
    if (p->buffer[k]) {
      free(p->buffer[k]);
      p->buffer[k] = NULL;
    }
  }
  for (uint32_t i = 0; i < p->n_out; i++) {
    const uint32_t k = p->out[i];
    if (p->buffer[k]) {
      free(p->buffer[k]);
      p->buffer[k] = NULL;
    }
  }
  if (p->buffer) free(p->buffer);
  if (p->in) free(p->in);
  if (p->out) free(p->out);
  free(p);
}

// Activate and deactivate the plugin.

void lilv_plugin_activate(PluginInstance *p)
{
  if (!p) return;
  lilv_instance_activate(p->instance);
}

void lilv_plugin_deactivate(PluginInstance *p)
{
  if (!p) return;
  lilv_instance_deactivate(p->instance);
}

/* GSL-compatible matrix structs, cf. gsl_structs.h in the Pure interpreter
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

/* Run the plugin on a given block of samples.

   The input samples are given as a k x n Pure double matrix. The k rows of
   the input matrix specify the sample blocks for the audio/CV input ports of
   the plugin (where k >= lilv_plugin_num_inputs(p), see below).

   The output samples produced by the plugin are written to a second l x m
   double matrix, whose l rows correspond to the audio/CV output ports of the
   plugin (where l >= lilv_plugin_num_outputs(p), see below). The output
   matrix is modified in-place and also returned as the result of the
   operation.

   The number of rows in the input and output matrices may exceed the actual
   number of audio/CV input/output ports, in which case only the first
   lilv_plugin_num_inputs(p) and lilv_plugin_num_outputs(p) rows are used,
   respectively (the rest of the output matrix remains unchanged). If the
   input or output matrix doesn't have enough rows then the operation fails.

   The actual block size (number of samples to be processed) is given by the
   minimum row size min(n,m) of the input and output matrices. Moreover, the
   number of samples may not exceed the maximum block size set when
   instantiating the plugin (or with a call to lilv_plugin_set_block_size(),
   see below); any extra columns of the input and output matrices will be
   silently ignored. */

pure_expr *lilv_plugin_run(PluginInstance *p, pure_expr *in, pure_expr *out)
{
  if (!p) return 0;
  uint32_t n = p->n_in, m = p->n_out;
  double *in_data, *out_data;
  size_t in_nrows, in_ncols, in_tda, out_nrows, out_ncols, out_tda;
  void *data;
  if (pure_is_double_matrix(in, &data)) {
    gsl_matrix *mat = (gsl_matrix*)data;
    in_data = mat->data;
    in_nrows = mat->size1; in_ncols = mat->size2; in_tda = mat->tda;
  } else if (pure_is_symbolic_matrix(in, &data)) {
    gsl_matrix_symbolic *mat = (gsl_matrix_symbolic*)data;
    in_data = NULL;
    in_nrows = mat->size1; in_ncols = mat->size2; in_tda = mat->tda;
    if (in_ncols > 0) return NULL;
  } else
    return NULL;
  if (in_nrows < n) return NULL;
  if (pure_is_double_matrix(out, &data)) {
    gsl_matrix *mat = (gsl_matrix*)data;
    out_data = mat->data;
    out_nrows = mat->size1; out_ncols = mat->size2; out_tda = mat->tda;
  } else if (pure_is_symbolic_matrix(out, &data)) {
    gsl_matrix_symbolic *mat = (gsl_matrix_symbolic*)data;
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
  if (count > p->block_size) count = p->block_size;
  if (count == 0) return out; // nothing to do
  /* Copy the samples from the input matrix to the audio/CV input buffers. It
     would be much preferable to just pass pointers to the matrix rows, but
     since Pure uses double and LV2 single precision, we have to do the
     conversion anyway. */
  for (uint32_t i = 0; i < n; i++) {
    const uint32_t k = p->in[i];
    float *x = p->buffer[k];
    double *y = in_data+i*in_tda;
    for (uint32_t j = 0; j < count; j++)
      x[j] = y[j];
  }
  // Run the plugin.
  lilv_instance_run(p->instance, p->block_size);
  // Copy the audio/CV output buffers back to the output matrix.
  for (size_t i = 0; i < m; i++) {
    const uint32_t k = p->out[i];
    float *x = p->buffer[k];
    double *y = out_data+i*out_tda;
    for (uint32_t j = 0; j < count; j++)
      y[j] = x[j];
  }
  return out;
}

/* Retrieve and manipulate the plugin data. You can retrieve the sample rate,
   maximum block size, total number of ports, and the number of (audio/CV)
   input and output ports. You can also adjust the maximum block size, but
   note that this is a fairly expensive operation involving the reallocation
   of the input/output buffers, so this shouldn't be done during realtime
   processing. */

double lilv_plugin_sample_rate(PluginInstance *p)
{
  if (!p) return NAN;
  return p->sample_rate;
}

uint32_t lilv_plugin_block_size(PluginInstance *p)
{
  if (!p) return 0;
  return p->block_size;
}

uint32_t lilv_plugin_num_ports(PluginInstance *p)
{
  if (!p) return 0;
  return p->n;
}

uint32_t lilv_plugin_num_inputs(PluginInstance *p)
{
  if (!p) return 0;
  return p->n_in;
}

uint32_t lilv_plugin_num_outputs(PluginInstance *p)
{
  if (!p) return 0;
  return p->n_out;
}

void lilv_plugin_set_block_size(PluginInstance *p, uint32_t block_size)
{
  if (!p) return;
  p->block_size = block_size;
  for (uint32_t i = 0; i < p->n_in; i++) {
    const uint32_t k = p->in[i];
    p->buffer[k] = realloc(p->buffer[k], block_size*sizeof(float));
  }
  for (uint32_t i = 0; i < p->n_out; i++) {
    const uint32_t k = p->out[i];
    p->buffer[k] = realloc(p->buffer[k], block_size*sizeof(float));
  }
}

/* Retrieve port information. For a given port number, this yields a tuple
   with the port symbol and name, type, flags and, in the case of a control or
   CV port, the minimum, maximum and default value of the port. The meaning of
   the fields is the same as with the lilv_plugin_info() operation. */

pure_expr *lilv_plugin_port_info(PluginInstance *p, uint32_t k)
{
  if (!p || k >= p->n) return 0;
  if (p->ty[k] == 1 || p->ty[k] == 3)
    return pure_tuplel(7, pure_cstring_dup(p->sym[k]),
		       pure_cstring_dup(p->name[k]),
		       pure_int(p->ty[k]), pure_int(p->flags[k]),
		       pure_double(p->mins[k]), pure_double(p->maxs[k]),
		       pure_double(p->defs[k]));
  else
    return pure_tuplel(4, pure_cstring_dup(p->sym[k]),
		       pure_cstring_dup(p->name[k]),
		       pure_int(p->ty[k]), pure_int(p->flags[k]));
}

/* Get and set the control port values. When setting a value, it is always
   clamped to the prescribed range, if available. */

double lilv_plugin_get_control(PluginInstance *p, uint32_t k)
{
  if (!p || k >= p->n) return NAN;
  return p->data[k];
}

void lilv_plugin_set_control(PluginInstance *p, uint32_t k, double x)
{
  if (!p || k >= p->n) return;
  if (x > p->maxs[k]) x = p->maxs[k];
  if (x < p->mins[k]) x = p->mins[k];
  p->data[k] = x;
}

/* symap implementation pilfered from Jalv. This is needed to implement the
   URI mapping stuff below. */

/*****************************************************************************/

/*
  Copyright 2011-2012 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

struct SymapImpl;

typedef struct SymapImpl Symap;

struct SymapImpl {
	/**
	   Unsorted array of strings, such that the symbol for ID i is found
	   at symbols[i - 1].
	*/
	char** symbols;

	/**
	   Array of IDs, sorted by corresponding string in @ref symbols.
	*/
	uint32_t* index;

	/**
	   Number of symbols (number of items in @ref symbols and @ref index).
	*/
	uint32_t size;
};

static Symap*
symap_new(void)
{
	Symap* map = (Symap*)malloc(sizeof(Symap));
	map->symbols = NULL;
	map->index   = NULL;
	map->size    = 0;
	return map;
}

static void
symap_free(Symap* map)
{
	for (uint32_t i = 0; i < map->size; ++i) {
		free(map->symbols[i]);
	}

	free(map->symbols);
	free(map->index);
	free(map);
}

static char*
symap_strdup(const char* str)
{
	const size_t len  = strlen(str);
	char*        copy = (char*)malloc(len + 1);
	memcpy(copy, str, len + 1);
	return copy;
}

/**
   Return the index into map->index (not the ID) corresponding to @c sym,
   or the index where a new entry for @c sym should be inserted.
*/
static uint32_t
symap_search(const Symap* map, const char* sym, bool* exact)
{
	*exact = false;
	if (map->size == 0) {
		return 0;  // Empty map, insert at 0
	} else if (strcmp(map->symbols[map->index[map->size - 1] - 1], sym) < 0) {
		return map->size;  // Greater than last element, append
	}

	uint32_t lower = 0;
	uint32_t upper = map->size - 1;
	uint32_t i     = upper;
	int      cmp;

	while (upper >= lower) {
		i   = lower + ((upper - lower) / 2);
		cmp = strcmp(map->symbols[map->index[i] - 1], sym);

		if (cmp == 0) {
			*exact = true;
			return i;
		} else if (cmp > 0) {
			if (i == 0) {
				break;  // Avoid underflow
			}
			upper = i - 1;
		} else {
			lower = ++i;
		}
	}

	assert(!*exact || strcmp(map->symbols[map->index[i] - 1], sym) > 0);
	return i;
}

static uint32_t
symap_try_map(Symap* map, const char* sym)
{
	bool           exact;
	const uint32_t index = symap_search(map, sym, &exact);
	if (exact) {
		assert(!strcmp(map->symbols[map->index[index]], sym));
		return map->index[index];
	}

	return 0;
}

static uint32_t
symap_map(Symap* map, const char* sym)
{
	bool           exact;
	const uint32_t index = symap_search(map, sym, &exact);
	if (exact) {
		assert(!strcmp(map->symbols[map->index[index] - 1], sym));
		return map->index[index];
	}

	const uint32_t id  = ++map->size;
	char* const    str = symap_strdup(sym);

	/* Append new symbol to symbols array */
	map->symbols = (char**)realloc(map->symbols, map->size * sizeof(str));
	map->symbols[id - 1] = str;

	/* Insert new index element into sorted index */
	map->index = (uint32_t*)realloc(map->index, map->size * sizeof(uint32_t));
	if (index < map->size - 1) {
		memmove(map->index + index + 1,
		        map->index + index,
		        (map->size - index - 1) * sizeof(uint32_t));
	}

	map->index[index] = id;

	return id;
}

static const char*
symap_unmap(Symap* map, uint32_t id)
{
	if (id == 0) {
		return NULL;
	} else if (id <= map->size) {
		return map->symbols[id - 1];
	}
	return NULL;
}

/*****************************************************************************/

/* uri-map and urid support. Note that we only keep one global URI table right
   now, so all plugin instances use the same set of URI ids and implementing
   several independent LV2 hosts in the same Pure program isn't really
   supported right now. */

static Symap *symap;

static inline void symap_init()
{
  if (!symap) {
    symap = symap_new();
    assert(symap);
  }
}

static uint32_t
uri_to_id(LV2_URI_Map_Callback_Data callback_data,
          const char*               map,
          const char*               uri)
{
  symap_init();
  const LV2_URID id = symap_map(symap, uri);
  return id;
}

static LV2_URID
map_uri(LV2_URID_Map_Handle handle,
        const char*         uri)
{
  symap_init();
  const LV2_URID id = symap_map(symap, uri);
  return id;
}

static const char*
unmap_uri(LV2_URID_Unmap_Handle handle,
          LV2_URID              urid)
{
  symap_init();
  const char* uri = symap_unmap(symap, urid);
  return uri;
}
