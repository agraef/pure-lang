
# This needs GNU make. Really.

# Package name and version number:
dist = pure-gtk-$(version)
version = 0.1

# Try to guess the installation prefix:
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
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

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows
DLL = .dll
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

# Default CFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
# These can be set from the command line as usual. Use CFLAGS, CPPFLAGS and
# LDFLAGS for compiler (-O etc.), preprocessor (-I etc.) and linker (-L etc.) 
# options, respectively. LIBS is to be used for additional libraries to be
# linked (-l etc.).

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
dlls = $(modules:.pure=$(DLL))

# No need to edit below this line, usually.

FLAGS = $(CPPFLAGS) $(CFLAGS) $(PIC) $(LDFLAGS)

DISTFILES = COPYING Makefile README config.guess examples/*.pure \
examples/*.glade $(modules) $(c-modules)

.PHONY: all clean realclean generate install uninstall dist distcheck

all: $(dlls)

clean:
	rm -f *$(DLL) *~ *.a *.o

realclean: clean
	rm -f $(modules) $(c-modules)

generate:
	rm -f $(modules) $(c-modules)
	$(MAKE) all

# Install targets.

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp $(modules) $(dlls) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f $(addprefix "$(DESTDIR)$(libdir)/pure/", $(modules) $(dlls))

# Roll a distribution tarball.

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)

#############################################################################
# Generator stuff. You only need this if you want to regenerate the wrappers.
# You need pure-gen and the GTK headers to do this.
#############################################################################

# Path to the installed GTK, Glib, Cairo and Pango headers. The headers are
# assumed to live in the gtk-2.0, glib-2.0 etc. subdirectories under this
# directory. If your system uses a different layout then you can also adjust
# the individual include directories below.

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
GTK_LIBS = $(shell pkg-config --libs gtk+-2.0)

gtk$(DLL): gtk.c
	$(CC) $(shared) $(FLAGS) $(GTK_CFLAGS) -o $@ $< $(GTK_LIBS)

gtk.pure gtk.c:
	pure-gen $(PGFLAGS) $(GTK_INCLUDES) -s '$(gtkdir)/gdk*/*.h::' -p gdk -m gdk -o gtk.pure -c gtk.c -fc $(gtkdir)/gdk/gdk.h
	pure-gen $(PGFLAGS) -N $(GTK_INCLUDES) -s '$(gtkdir)/gtk/*.h::' -p gtk -m gtk -fc $(gtkdir)/gtk/gtk.h

# glib et al

# This isn't available in all glib versions.
HAVE_GIO = $(shell pkg-config --exists gio-2.0 && echo yes || echo no)

glib$(DLL): glib.c
	$(CC) $(shared) $(FLAGS) $(GTK_CFLAGS) -o $@ $< $(GTK_LIBS)

# We should maybe strip this down so that it only contains the stuff needed
# for Pure-GTK applications. Also note that the glib headers declare some junk
# which isn't actually available in the libraries, we get rid of this with -x.
glib.pure glib.c:
	pure-gen $(PGFLAGS) $(GTK_INCLUDES) -s '$(glibdir)/glib/*.h::' -x '^(G_GNUC_[A-Z_]*|glib_dummy_decl|g_thread_init(_with_errorcheck_mutexes)?)$$' -p g -m glib -fc $(glibdir)/glib.h
	pure-gen $(PGFLAGS) -N $(GTK_INCLUDES) -s '$(glibdir)/gobject/*.h::' -x '^(g_value_c_init|g_value_types_init|g_enum_types_init|g_param_type_init|g_boxed_type_init|g_object_type_init|g_param_spec_types_init|g_value_transforms_init|g_signal_init)$$' -p g -m glib -o glib.pure -c glib.c -fc $(glibdir)/glib-object.h
ifeq ($(HAVE_GIO),yes)
	pure-gen $(PGFLAGS) -N $(GTK_INCLUDES) -s '$(glibdir)/gio/*.h::' -x '^g_io_module_(un)?load$$' -p g -m glib -o glib.pure -c glib.c -fc $(glibdir)/gio/gio.h
endif
	pure-gen $(PGFLAGS) -N $(GTK_INCLUDES) -s '$(glibdir)/gmodule.h::' -p g -m glib -o glib.pure -c glib.c -fc $(glibdir)/gmodule.h

# atk

ATK_CFLAGS = $(shell pkg-config --cflags atk)
ATK_INCLUDES = $(shell pkg-config --cflags-only-I atk)
ATK_LIBS = $(shell pkg-config --libs atk)

atk$(DLL): atk.c
	$(CC) $(shared) $(FLAGS) $(ATK_CFLAGS) -o $@ $< $(ATK_LIBS)

atk.pure atk.c:
	pure-gen $(PGFLAGS) $(ATK_INCLUDES) -s '$(atkdir)/atk/*.h::' -p atk -m atk -fc $(atkdir)/atk/atk.h

# cairo

CAIRO_CFLAGS = $(shell pkg-config --cflags cairo)
CAIRO_INCLUDES = $(shell pkg-config --cflags-only-I cairo)
CAIRO_LIBS = $(shell pkg-config --libs cairo)

cairo$(DLL): cairo.c
	$(CC) $(shared) $(FLAGS) $(CAIRO_CFLAGS) -o $@ $< $(CAIRO_LIBS)

cairo.pure cairo.c:
	pure-gen $(PGFLAGS) $(CAIRO_INCLUDES) -s '$(cairodir)/cairo/*.h::' -p cairo -m cairo -fc $(cairodir)/cairo/cairo.h

# pango

PANGO_CFLAGS = $(shell pkg-config --cflags pango)
PANGO_INCLUDES = $(shell pkg-config --cflags-only-I pango)
PANGO_LIBS = $(shell pkg-config --libs pango)

pango$(DLL): pango.c
	$(CC) $(shared) $(FLAGS) $(PANGO_CFLAGS) -o $@ $< $(PANGO_LIBS)

pango.pure pango.c:
	pure-gen $(PGFLAGS) $(PANGO_INCLUDES) -s '$(pangodir)/pango/*.h::' -p pango -m pango -fc $(pangodir)/pango/pango.h

# libglade

GLADE_CFLAGS = $(shell pkg-config --cflags libglade-2.0)
GLADE_INCLUDES = $(shell pkg-config --cflags-only-I libglade-2.0)
GLADE_LIBS = $(shell pkg-config --libs libglade-2.0)

glade$(DLL): glade.c
	$(CC) $(shared) $(FLAGS) $(GLADE_CFLAGS) -o $@ $< $(GLADE_LIBS)

glade.pure glade.c:
	pure-gen $(PGFLAGS) $(GLADE_INCLUDES) -s '$(gladedir)/glade/*.h::' -p glade -m glade -fc $(gladedir)/glade/glade.h
