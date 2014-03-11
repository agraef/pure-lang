
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

// This is to be set *only* if the plugin is to be loaded from the source
// script rather than being linked into the plugin binary (useful for
// debugging purposes). The plugin script is searched for in the bundle
// directory.
//#define PLUGIN_SCRIPT "lv2pure.pure"

// This search path is used to discover the local path of the plugin when
// generating the dynamic manifest, if the plugin is loaded from a source
// script.
#ifdef PLUGIN_SCRIPT
#ifndef DEFAULT_LV2_PATH
#define DEFAULT_LV2_PATH "~/.lv2:/usr/lib/lv2:/usr/local/lib/lv2"
#endif
#endif

#include "lv2pure.h"

// Global interpreter instance.
static pure_interp *interp = 0;

#ifndef PLUGIN_SCRIPT
// This is the main entry point in the batch-compiled Pure module.
extern void LOADER_NAME(int argc, char** argv);
#endif

#ifdef PLUGIN_SCRIPT

// Some helper functions to discover the bundle path which holds the plugin
// script, when the script is loaded from source.

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

static const char *dirstr = "/\\", *volstr = "";
#define PATHDELIM ':'
#define MAXSTRLEN 1024

static const char *lv2_path(void)
{
  static char *path = NULL;
  if (!path && !(path = getenv("LV2_PATH")))
    path = DEFAULT_LV2_PATH;
  return path;
}

static char *home(void)
{
  static char *homedir = NULL;
  if (!homedir && !(homedir = getenv("HOME"))) {
    homedir = strdup("/");
    *homedir = *dirstr;
  }
  return homedir;
}

#define tilde(s) (s[0] == '~' && (!s[1] || strchr(dirstr, s[1]) && !strchr(volstr, s[1])))

static int absolute(char *s)
{
  char *t = s;
  if (!s || !*s)
    return 0;
  else if (tilde(s))
    return 1;
  else {
    while (*s && !strchr(dirstr, *s)) ++s;
    return *s && (s == t || strchr(volstr, *s));
  }
}

static int dirprefix(char *s, char *prefix)
{
  int l = strlen(prefix);
  return s && *s && strncmp(s, prefix, l) == 0 &&
    (!s[l] || strchr(dirstr, s[l]) && !strchr(volstr, s[l]));
}

static char *dirname(char *t, char *s)
{
  char *s1, *s2 = NULL;
  for (s1 = s; *s1; s1++)
    if (strchr(dirstr, *s1))
      s2 = s1+1;
  if (s2) {
    strncpy(t, s, s2-s);
    t[s2-s] = 0;
  } else
    *t = 0;
  return t;
}

static char *basename(char *t, char *s, char c)
{
  char *s1, *s2;
  for (s1 = s2 = s; *s1; s1++)
    if (strchr(dirstr, *s1))
      s2 = s1+1;
  if ((s1 = strchr(strcpy(t, s2), c)))
    *s1 = 0;
  return t;
}

static char *absname(char *t, char *s)
{
  if (absolute(s))
    strcpy(t, s);
  else {
    if (!getcwd(t, MAXSTRLEN))
      strcpy(t, s);
    else {
      int l = strlen(t);
      if (l <= 1 || !strchr(dirstr, t[l-1]))
	t[l++] = *dirstr;
      strcpy(t+l, s);
    }
  }
  return t;
}

static char *expand(char *s1, char *s2)
{
  if (tilde(s2)) {
    char *h = home();
    int l = strlen(h);
    strcpy(s1, h);
    if (l > 0 && strchr(dirstr, h[l-1]))
      strcpy(s1+l, s2+2);
    else
      strcpy(s1+l, s2+1);
  } else
    strcpy(s1, s2);
  return s1;
}

static int chkfile(char *s)
{
  struct stat st;
  return !stat(s, &st) && S_ISREG(st.st_mode);
}

static char *lv2path = 0;

static char *searchlib(char *s1, char *s2)
{
  const char *s, *t;
  if (tilde(s2))
    return expand(s1, s2);
  else if (absolute(s2) || dirprefix(s2, ".") || dirprefix(s2, ".."))
    return strcpy(s1, s2);
  for (s = lv2_path(); *s; s = t) {
    int l;
    char p[MAXSTRLEN];
    if (!(t = strchr(s, PATHDELIM)))
      t = strchr(s, 0);
    if (s == t) goto next;
    if (s[0] == '.')
      if (t == s+1)
	s = t;
      else if (strchr(dirstr, s[1]) &&
	       !strchr(volstr, s[1]))
	s += 2;
    l = t-s;
    strncpy(p, s, l);
    p[l] = 0;
    expand(s1, p);
    l = strlen(s1);
    if (l > 0 && (!strchr(dirstr, s1[l-1]) ||
		  strchr(volstr, s1[l-1])))
      s1[l] = *dirstr, l++;
    strcpy(s1+l, s2);
    if (chkfile(s1))
      return s1;
  next:
    if (*t) t++;
  }
  return strcpy(s1, s2);
}

#endif

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
  pure_interp *s_interp = pure_current_interp();
  lv2plugin_t *plugin = calloc(1, sizeof(lv2plugin_t));
  assert(plugin);

  if (interp)
    // We already created the interpreter, switch to it.
    pure_switch_interp(interp);
  else {
    // Create a new interpreter instance.
#ifdef PLUGIN_SCRIPT
    // Locate the script file in the bundle, searching the LV2 plugin path.
    char path[MAXSTRLEN], script[MAXSTRLEN];
    snprintf(path, MAXSTRLEN, "%s.lv2%c%s",
             PLUGIN_NAME, *dirstr, PLUGIN_SCRIPT);
    searchlib(script, path);
    if (!chkfile(script)) {
      fprintf(stderr, "%s: couldn't find script '%s'\n",
              PLUGIN_URI, PLUGIN_SCRIPT);
      goto fail;
    }
    printf("** %s: loading %s\n", PLUGIN_URI, script);
    char *argv[] = {script, script, NULL};
    interp = pure_create_interp(2, argv);
    pure_switch_interp(interp);
#else
    LOADER_NAME(0, 0);
    interp = pure_current_interp();
#endif
  }
  if (!interp) {
    fprintf(stderr, "%s: couldn't load Pure interpreter\n", PLUGIN_URI);
    goto fail;
  }

  // Get the plugin function.
  pure_expr *e;
#ifdef PLUGIN_SCRIPT
  // Make sure that the plugin function is compiled eagerly right now, since
  // we don't want the JIT to kick in later when we're running in realtime.
  pure_interp_compile(interp, pure_sym(PLUGIN_FUN));
#endif
  plugin->fun = pure_symbolx(pure_sym(PLUGIN_FUN), &e);
  if (!plugin->fun) {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: unhandled exception '%s'\n", PLUGIN_URI, s);
      free(s);
      pure_freenew(e);
    }
    goto fail;
  }
  pure_new(plugin->fun);

  // Create a pointer object representing ourselves and pass it as the first
  // argument to the plugin function. This should yield a closure (or partial
  // application) which is then used on all subsequent invocations.
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
  pure_new(fun);
  pure_free(plugin->fun);
  plugin->fun = fun;

  // Invoke the manifest function to get the port information.
  pure_expr *info = pure_symbolx(pure_sym("manifest"), &e);
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
    fprintf(stderr, "%s: bad manifest '%s'\n", PLUGIN_URI, s);
    free(s);
    pure_freenew(info);
    goto fail2;
  } else {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: bad manifest (unhandled exception '%s')\n",
	      PLUGIN_URI, s);
      free(s);
      pure_freenew(e);
    } else
      fprintf(stderr, "%s: bad manifest (unknown error)\n", PLUGIN_URI);
    goto fail2;
  }

  plugin->uri = PLUGIN_URI;
  pure_switch_interp(s_interp);
  return plugin;

 fail2:
  pure_free(plugin->fun);
  
 fail:
  pure_switch_interp(s_interp);
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
  if (plugin->fun) {
    pure_interp *s_interp = pure_lock_interp(interp);
    pure_free(plugin->fun);
    pure_unlock_interp(s_interp);
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

static void activation_cb(lv2plugin_t* plugin, bool active)
{
  plugin->active = active;
  pure_interp *s_interp = pure_lock_interp(interp);
  pure_expr *e, *ret = pure_appx(plugin->fun, pure_int(active), &e);
  if (!ret) {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: unhandled exception '%s'\n", PLUGIN_URI, s);
      free(s);
      pure_freenew(e);
    }
  } else
    pure_freenew(ret);
  pure_unlock_interp(s_interp);
}

static void activate(LV2_Handle instance)
{
  lv2plugin_t* plugin = (lv2plugin_t*)instance;
  if (plugin) activation_cb(plugin, true);
}

static void deactivate(LV2_Handle instance)
{
  lv2plugin_t* plugin = (lv2plugin_t*)instance;
  if (plugin) activation_cb(plugin, false);
}

static void run(LV2_Handle instance, uint32_t sample_count)
{
  lv2plugin_t* plugin = (lv2plugin_t*)instance;
  if (!plugin) return;
  // XXXFIXME: As some LV2 hosts such as Ardour are heavily multi-threaded and
  // the Pure runtime isn't thread-safe yet, we need to obtain the global
  // interpreter lock here so that invocations of the plugin functions are
  // effectively serialized. Until the Pure runtime becomes fully thread-safe,
  // this may be a major bottleneck if you run many Pure plugin instances in
  // the same process.
  pure_interp *s_interp = pure_lock_interp(interp);
  plugin->nsamples = sample_count;
  plugin->running = true;
  pure_expr *e, *ret = pure_appx(plugin->fun, pure_tuplel(0, 0), &e);
  plugin->running = false;
  if (!ret) {
    if (e) {
      char *s = str(e);
      fprintf(stderr, "%s: unhandled exception '%s'\n", PLUGIN_URI, s);
      free(s);
      pure_freenew(e);
    }
  } else
    pure_freenew(ret);
  pure_unlock_interp(s_interp);
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
  if (plugin)
    return 0;
  else
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
