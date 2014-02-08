
/* A Pure Lilv module. This was written for Pure by aggraef@gmail.com, but
   most of the basic code for interfacing to the Lilv API was gleaned from
   various programs in drobilla's repository, in particular lv2info.c and
   jalv.c, see http://drobilla.net/software/lilv/. */

#include <assert.h>
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
      const LilvNode* preset = lilv_nodes_get(data, i);
      lilv_world_load_resource(world, preset);
      LilvNodes* titles =
	lilv_world_find_nodes(world, preset, label_pred, NULL);
      if (titles) {
	const LilvNode* title = lilv_nodes_get_first(titles);
	assert(k<l);
	xv[k++] = pure_cstring_dup(lilv_node_as_string(title));
	lilv_nodes_free(titles);
      }
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
