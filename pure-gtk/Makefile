
# This needs GNU make. Really.

# Package name and version number:
dist = pure-gtk-$(version)
version = 0.12

# Try to guess the installation prefix:
prefix = $(patsubst %/bin/pure$(EXE),%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

# Try to guess the host system type.
host = $(shell ./config.guess)

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

PURE_COPTS=-I.. -L..

# Take care of some common systems.
ifneq "$(findstring -linux,$(host))" ""
# Some newer Linux systems make --as-needed the default, we need the
# old behaviour here. NOTE: This seems to be broken with recent gcc
# versions, so you may have to link batch-compiled pure-gtk scripts
# against the required GTK+ libraries anyway. :(
EXTRA_LD_FLAGS = -Wl,--no-as-needed
endif
ifneq "$(findstring -mingw,$(host))" ""
# Windows
EXE = .exe
DLL = .dll
atk_missing = -x '^(atk_value_get_minimum_increment|atk_streamable_content_get_uri|atk_relation_remove_target|atk_hyperlink_impl_get_hyperlink|atk_hyperlink_impl_get_type|atk_get_version|atk_(plug|socket).*)$$'
dlldir = $(patsubst %/include,%/bin,$(includedir))
PURE_COPTS += -L$(dlldir)
# additional magic needed to locate gtk stuff on Windows
windows = true
dllopts = $(foreach dllname, $(foreach dll,$(shell pkg-config --libs-only-l $(1)),$(basename $(notdir $(wildcard $(patsubst -l%,$(dlldir)/lib%*.dll,$(dll)))))), -l$(dllname))
else
atk_missing = -x '^(linux|unix)$$'
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX (untested)
DLL = .dylib
shared = -dynamiclib
endif
ifneq "$(findstring x86_64-,$(host))" ""
# 64 bit, needs -fPIC flag
PIC = -fPIC
endif

# Default CFLAGS are -g -O2, CPPFLAGS is empty by default. These can be set
# from the command line as usual. Use CFLAGS and CPPFLAGS for compiler (-O
# etc.) and preprocessor (-I etc.) options, respectively. NOTE: LDFLAGS and
# LIBS aren't supported here, as the necessary linker options and libraries to
# be linked are determined automatically using pkg-config. If you really need
# to change these, you'll have to edit the compilation commands for each
# supported module below.

CFLAGS = -g -O2

# Extra pure-gen flags.
#PGFLAGS = -v

# Stuff to build.

# These are the core APIs related to gtk+.
core = gtk.pure glib.pure

# Uncomment this to build various auxiliary interfaces. You might also wish to
# add glade.pure to get the libglade interface which isn't built by default.
addons = atk.pure cairo.pure pango.pure

modules = $(core) $(addons)
c-modules = $(modules:.pure=.c)
dlls = $(c-modules:.c=$(DLL))

examples = examples/hello examples/uiexample examples/life

# No need to edit below this line, usually.

FLAGS = $(CPPFLAGS) $(CFLAGS) $(PIC)

DISTFILES = COPYING COPYING.LESSER Makefile README config.guess debian/* \
examples/*.pure examples/*.glade $(modules) $(c-modules)
SEDFILES = README

.PHONY: all clean realclean generate examples install uninstall dist distcheck

all: $(dlls)

clean:
	rm -f *$(DLL) *~ *.a *.o $(examples)

realclean: clean
	rm -f $(modules) $(c-modules)

generate:
	rm -f $(dlls) *.o $(modules) $(c-modules)
	$(MAKE) all

# Compile the examples.

# NOTE: Recent gcc versions require that we link in the GTK+ libs when
# batch-compiling these programs.
EXTRA_LD_LIBS = $(shell pkg-config --libs gtk+-2.0)

examples: $(examples)

examples/%: examples/%.pure
	(cd examples && pure $(PURE_COPTS) -c $(notdir $<) -o $(notdir $@) $(EXTRA_LD_LIBS))

# Install targets.

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp $(modules) $(dlls) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f $(addprefix "$(DESTDIR)$(libdir)/pure/", $(modules) $(dlls))

# Roll a distribution tarball.

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(SEDFILES); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)

# Debian packaging

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@

#############################################################################
# Generator stuff. You only need this if you want to regenerate the wrappers.
# You need pure-gen and the GTK headers to do this.
#############################################################################

# Path to the installed GTK, Glib, Cairo and Pango headers. The headers are
# assumed to live in the gtk-2.0, glib-2.0 etc. subdirectories under this
# directory. If your system uses a different layout then you can also adjust
# the individual include directories below.

# NOTE: The paths need to be valid at generation time only. The generated C
# source uses relative paths which should work on most systems which have GTK+
# installed.

# NOTE: Most wrapper modules are just empty shells which link in the required
# GTK libraries for each module. (Hence you should never try to add the
# --as-needed linker option supported by some GNU ld versions; the modules
# will then stop working.) This trick seems to work on most systems, at least
# when using GNU ld. But if you get lots of undefined extern symbol errors
# when loading the modules then you might have to modify the .pure module
# sources so that they pull in the required GTK libraries with the appropriate
# 'using' clauses instead.

includedir = /usr/include

gtkdir   = $(includedir)/gtk-2.0
glibdir  = $(includedir)/glib-2.0
atkdir   = $(includedir)/atk-1.0
cairodir = $(includedir)
pangodir = $(includedir)/pango-1.0
gladedir = $(includedir)/libglade-2.0

# gtk and gdk

GTK_CFLAGS = $(shell pkg-config --cflags gtk+-2.0)
GTK_INCLUDES = $(shell pkg-config --cflags-only-I gtk+-2.0)
GTK_LIBS = $(EXTRA_LD_FLAGS) $(shell pkg-config --libs gtk+-2.0)
ifdef windows
GTK_DLLS = $(call dllopts, gtk+-2.0) -lgtk
endif

gtk$(DLL): gtk.c
	$(CC) $(shared) $(FLAGS) $(GTK_CFLAGS) -o $@ $< $(GTK_LIBS)

gtk.pure gtk.c:
	pure-gen $(PGFLAGS) $(GTK_INCLUDES) -s '$(gtkdir)/gdk*/*.h;' -x '^(linux|unix)$$' -p gdk -m gdk -o gtk.pure -c gtk.c -fc-auto $(gtkdir)/gdk/gdk.h $(GTK_DLLS)
	pure-gen $(PGFLAGS) -N $(GTK_INCLUDES) -s '$(gtkdir)/gtk/*.h;' -x '^(linux|unix|ubuntu_.*)$$' -p gtk -m gtk -fc-auto $(gtkdir)/gtk/gtk.h
	sed -e 's|#include \"$(gtkdir)/\(\([A-Za-z-]\+/\)\?[A-Za-z-]\+\.h\)\"|#include <\1>|g' < gtk.c > gtk.c.new && rm gtk.c && mv gtk.c.new gtk.c

# glib et al

ifdef windows
GLIB_DLLS = $(call dllopts, gtk+-2.0) -lglib
endif

# This isn't available in all glib versions.
HAVE_GIO = $(shell pkg-config --exists gio-2.0 && echo yes || echo no)

glib$(DLL): glib.c
	$(CC) $(shared) $(FLAGS) $(GTK_CFLAGS) -o $@ $< $(GTK_LIBS)

# We should maybe strip this down so that it only contains the stuff needed
# for Pure-GTK applications. Also note that the glib headers declare some junk
# which isn't actually available in the libraries, we get rid of this with -x.
glib.pure glib.c:
	pure-gen $(PGFLAGS) $(GTK_INCLUDES) -s '$(glibdir)/glib/*.h;' -x '^(linux|unix|G_GNUC_[A-Z_]*|atexit|glib_dummy_decl|g_thread_init(_with_errorcheck_mutexes)?)$$' -p g -m glib -fc-auto $(glibdir)/glib.h $(GLIB_DLLS)
	pure-gen $(PGFLAGS) -N $(GTK_INCLUDES) -s '$(glibdir)/gobject/*.h;' -x '^(linux|unix|g_value_c_init|g_value_types_init|g_enum_types_init|g_param_type_init|g_boxed_type_init|g_object_type_init|g_param_spec_types_init|g_value_transforms_init|g_signal_init)$$' -p g -m glib -o glib.pure -c glib.c -fc-auto $(glibdir)/glib-object.h
ifeq ($(HAVE_GIO),yes)
ifdef windows
	pure-gen $(PGFLAGS) -N $(GTK_INCLUDES) -s '$(glibdir)/gio/*.h;' -x '^(g_io_module_(un)?load|g_io_module_query|g_dbus.*)$$' -p g -m glib -o glib.pure -c glib.c -fc-auto $(glibdir)/gio/gio.h
else
	pure-gen $(PGFLAGS) -N $(GTK_INCLUDES) -s '$(glibdir)/gio/*.h;' -x '^(unix|linux|g_io_module_(un)?load|g_io_module_query)$$' -p g -m glib -o glib.pure -c glib.c -fc-auto $(glibdir)/gio/gio.h
endif
endif
	pure-gen $(PGFLAGS) -N $(GTK_INCLUDES) -s '$(glibdir)/gmodule.h;' -x '^(linux|unix)$$' -p g -m glib -o glib.pure -c glib.c -fc-auto $(glibdir)/gmodule.h
	sed -e 's|#include \"$(glibdir)/\(\([A-Za-z-]\+/\)\?[A-Za-z-]\+\.h\)\"|#include <\1>|g' < glib.c > glib.c.new && rm glib.c && mv glib.c.new glib.c

# atk

ATK_CFLAGS = $(shell pkg-config --cflags atk)
ATK_INCLUDES = $(shell pkg-config --cflags-only-I atk)
ATK_LIBS = $(EXTRA_LD_FLAGS) $(shell pkg-config --libs atk)
ifdef windows
ATK_DLLS = $(call dllopts, atk) -latk
endif

atk$(DLL): atk.c
	$(CC) $(shared) $(FLAGS) $(ATK_CFLAGS) -o $@ $< $(ATK_LIBS)

atk.pure atk.c:
	pure-gen $(PGFLAGS) $(ATK_INCLUDES) -s '$(atkdir)/atk/*.h;' $(atk_missing) -p atk -m atk -fc-auto $(atkdir)/atk/atk.h $(ATK_DLLS)
	sed -e 's|#include \"$(atkdir)/\(\([A-Za-z-]\+/\)\?[A-Za-z-]\+\.h\)\"|#include <\1>|g' < atk.c > atk.c.new && rm atk.c && mv atk.c.new atk.c

# cairo

CAIRO_CFLAGS = $(shell pkg-config --cflags cairo)
CAIRO_INCLUDES = $(shell pkg-config --cflags-only-I cairo)
CAIRO_LIBS = $(EXTRA_LD_FLAGS) $(shell pkg-config --libs cairo)
ifdef windows
CAIRO_DLLS = $(call dllopts, cairo) -lcairo
endif

cairo$(DLL): cairo.c
	$(CC) $(shared) $(FLAGS) $(CAIRO_CFLAGS) -o $@ $< $(CAIRO_LIBS)

cairo.pure cairo.c:
	pure-gen $(PGFLAGS) $(CAIRO_INCLUDES) -s '$(cairodir)/cairo/*.h;' -x '^(linux|unix)$$' -p cairo -m cairo -fc-auto $(cairodir)/cairo/cairo.h $(CAIRO_DLLS)
	sed -e 's|#include \"$(cairodir)/\(\([A-Za-z-]\+/\)\?[A-Za-z-]\+\.h\)\"|#include <\1>|g' < cairo.c > cairo.c.new && rm cairo.c && mv cairo.c.new cairo.c

# pango

PANGO_CFLAGS = $(shell pkg-config --cflags pango)
PANGO_INCLUDES = $(shell pkg-config --cflags-only-I pango)
PANGO_LIBS = $(EXTRA_LD_FLAGS) $(shell pkg-config --libs pango)
ifdef windows
PANGO_DLLS = $(call dllopts, pango) -lpango
endif

pango$(DLL): pango.c
	$(CC) $(shared) $(FLAGS) $(PANGO_CFLAGS) -o $@ $< $(PANGO_LIBS)

pango.pure pango.c:
	pure-gen $(PGFLAGS) $(PANGO_INCLUDES) -s '$(pangodir)/pango/*.h;' -x '^(linux|unix)$$' -p pango -m pango -fc-auto $(pangodir)/pango/pango.h $(PANGO_DLLS)
	sed -e 's|#include \"$(pangodir)/\(\([A-Za-z-]\+/\)\?[A-Za-z-]\+\.h\)\"|#include <\1>|g' < pango.c > pango.c.new && rm pango.c && mv pango.c.new pango.c

# libglade

GLADE_CFLAGS = $(shell pkg-config --cflags libglade-2.0)
GLADE_INCLUDES = $(shell pkg-config --cflags-only-I libglade-2.0)
GLADE_LIBS = $(EXTRA_LD_FLAGS) $(shell pkg-config --libs libglade-2.0)
ifdef windows
GLADE_DLLS = $(call dllopts, libglade-2.0) -lglade
endif

glade$(DLL): glade.c
	$(CC) $(shared) $(FLAGS) $(GLADE_CFLAGS) -o $@ $< $(GLADE_LIBS)

glade.pure glade.c:
	pure-gen $(PGFLAGS) $(GLADE_INCLUDES) -s '$(gladedir)/glade/*.h;' -p glade -m glade -fc-auto $(gladedir)/glade/glade.h $(GLADE_DLLS)
	sed -e 's|#include \"$(gladedir)/\(\([A-Za-z-]\+/\)\?[A-Za-z-]\+\.h\)\"|#include <\1>|g' < glade.c > glade.c.new && rm glade.c && mv glade.c.new glade.c
