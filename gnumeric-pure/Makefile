
# This Makefile requires GNU make.

# Package name and version number:
dist = gnumeric-pure-$(version)
version = 0.13

# NOTE: This version of the module is set up to build with Gnumeric 1.12. It
# will *not* compile with Gnumeric 1.10 and earlier any more because of
# changes in the plugin API. To compile with Gnumeric <= 1.10, please use
# version 0.12 of the module.

# Build and installation instructions:

# 1.  Make sure that you have gnumeric and the corresponding development
#     packages installed.

#     NOTE: Older Gnumeric versions (<= 1.9.12) lack the gnumeric-features.h
#     file or have it installed in the wrong directory. If all else fails then
#     you'll have to grab the file from the Gnumeric sources and place it into
#     the directory with the gnumeric-pure sources, or specify the include
#     path needed to locate this file in step 2 below. (Note that this file is
#     created when configuring the Gnumeric sources. Also note that very old
#     Gnumeric versions don't have this file, in this case you can just copy
#     gnumeric-config.h and symlink it to gnumeric-features.h instead.)

#     The Makefile tries to guess the installation path and version number of
#     Gnumeric on its own. If it guesses wrong, you'll have to adjust the
#     values of the prefix and gnmversion variables (see below) accordingly.
#     Moreover, you may want to adjust the value of the gnmlocalplugindir
#     variable below if you'd like the install-local target to go into some
#     other directory than ~/.gnumeric/$(gnmversion)/plugins.

# 2.  Adjust the following as needed for your GTK+/GOffice installation:

GODEPS=libspreadsheet-1.12

INCLUDES = `pkg-config --cflags $(GODEPS)`
GTKLIBS = `pkg-config --libs $(GODEPS)`

#     Adjust the following as needed to build with OpenGL support. NOTE: This
#     requires a GtkGLExt version which is compatible with GTK3, see
#     https://github.com/tdz/gtkglext. Also, there are still some issues with
#     the OpenGL support under GTK3 right now, so this is disabled by
#     default. Uncomment the following line if you want to give it a try.

#GLDEPS=gtkglext-3.0 gtkglext-x11-3.0

# 3.  Run 'make' to build the plugin.

# 4a. Run 'sudo make install' for a global installation in the gnumeric plugin
#     directory (usually something like /usr/local/gnumeric/1.9.x/plugins.

# - OR -

# 4b. Run 'make install-local' for a local installation in your home directory 
#     (usually something like ~/.gnumeric/1.9.x/plugins).

# Finally, make sure that the installation directory is in Gnumeric's plugin
# path. To use the plugin, you must enable 'Pure plugin loader' and 'Pure
# Functions' in the plugin list under 'Tools | Plug-ins'. A few sample
# functions can be found in .../plugins/pure-func/pure_func.pure. Edit that
# script as needed and edit the plugin.xml file in that directory to add the
# functions that you want. See the README file for more information.

# Try to guess the Gnumeric version number:
#gnmversion=$(shell gnumeric --version | sed -n -e "s/^gnumeric version '\([0-9.]*\)'.*/\1/" -e "/^[0-9.]*$$/p")
gnmversion=$(shell pkg-config --modversion libspreadsheet-1.12)

# Try to guess the Gnumeric installation prefix:
prefix = $(patsubst %/bin/gnumeric,%,$(shell which gnumeric 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/gnumeric, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

# Gnumeric library path.
gnmlibdir = $(libdir)/gnumeric

# Install dir for the plugin and accompanying stuff.
gnmplugindir = $(gnmlibdir)/$(gnmversion)/plugins
ifneq "$(HOME)" ""
gnmlocalplugindir = $(HOME)/.gnumeric/$(gnmversion)/plugins
else
gnmlocalplugindir = $(gnmplugindir)
endif

pureloaderdir = pure-loader
purefuncdir = pure-func
pureglfuncdir = pure-glfunc

# Try to guess the host system type.
host = $(shell ./config.guess)

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows/mingw
EXE = .exe
DLL = .dll
LDFLAGS = -Wl,--enable-auto-import
LIBS = -lgsl -lgmp
GLDEPS = gtkglext-1.0 gtkglext-win32-1.0
export EXE
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

# GtkGLExt support, if requested.
# IMPORTANT: DON'T REMOVE THE -lstdc++ linker option, it prevents issues with
# some proprietary graphics card drivers on Linux. See the following URL:
# http://wiki.fifengine.de/Segfault_in_cxa_allocate_exception#Workaround
ifneq "$(GLDEPS)" ""
INCLUDES += `pkg-config --cflags $(GLDEPS)` -DUSE_GL
GTKLIBS += -lstdc++ `pkg-config --libs $(GLDEPS)`
endif

# Default CFLAGS and CPPFLAGS, LDFLAGS and LIBS are empty by default.
CFLAGS = -g -O2
CPPFLAGS = -Wall

DISTFILES = ChangeLog COPYING Makefile README config.guess doc/*.png \
pure-loader/boot.c pure-loader/pure-gnumeric.c pure-loader/pure-gnumeric.h \
pure-loader/pure-loader.c pure-loader/pure-loader.h \
pure-loader/plugin.xml pure-loader/pure-ui.xml \
pure-func/*.pure pure-func/plugin.xml \
pure-glfunc/*.pure pure-glfunc/plugin.xml \
pure-gnm/*.pure pure-gnm/plugin.xml pure-gnm/Makefile \
debian/* examples/*.gnumeric \
examples/*/*.pure examples/*/*.dsp examples/*/*.gnumeric \
examples/*/Makefile examples/*/config.guess
SEDFILES = README

all: pure-loader/pure_loader$(DLL) pure-func/plugin.xml pure-glfunc/plugin.xml
	$(MAKE) -C pure-gnm

pure-loader/pure_loader$(DLL): pure-loader/boot.o pure-loader/pure-gnumeric.o pure-loader/pure-loader.o
	$(CC) $(shared) -o $@ $(LDFLAGS) $^ $(GTKLIBS) -lpure $(LIBS)

%.o: %.c
	$(CC) $(INCLUDES) -I. -I.. $(CPPFLAGS) -DGNM_VERSION='"$(gnmversion)"' -DPLUGIN_VERSION='"$(version)"' -DPURE_INCLUDES='"-I$(gnmplugindir)/$(purefuncdir)","-I$(gnmlocalplugindir)/$(purefuncdir)"' $(CFLAGS) $(PIC) -o $@ -c $<

pure-func/plugin.xml: pure-func/pure_func.pure
	(cd pure-func && pure -x ../pure-gnm/pure-gnm.pure pure_func.pure > plugin.xml)

pure-glfunc/plugin.xml: pure-glfunc/pure_glfunc.pure
	(cd pure-glfunc && pure -x ../pure-gnm/pure-gnm.pure pure_glfunc.pure > plugin.xml)

clean:
	(cd pure-loader/ && rm -f pure_loader$(DLL) *~ *.a *.o)
	rm -f *~
	$(MAKE) -C pure-gnm clean

install:
	test -d "$(DESTDIR)$(gnmplugindir)/$(purefuncdir)" || mkdir -p "$(DESTDIR)$(gnmplugindir)/$(purefuncdir)"
	test -d "$(DESTDIR)$(gnmplugindir)/$(pureglfuncdir)" || mkdir -p "$(DESTDIR)$(gnmplugindir)/$(pureglfuncdir)"
	test -d "$(DESTDIR)$(gnmplugindir)/$(pureloaderdir)" || mkdir -p "$(DESTDIR)$(gnmplugindir)/$(pureloaderdir)"
	cp COPYING README pure-func/*.pure pure-func/plugin.xml "$(DESTDIR)$(gnmplugindir)/$(purefuncdir)"
	cp pure-glfunc/*.pure pure-glfunc/plugin.xml "$(DESTDIR)$(gnmplugindir)/$(pureglfuncdir)"
	cp pure-loader/pure_loader$(DLL) pure-loader/plugin.xml pure-loader/pure-ui.xml "$(DESTDIR)$(gnmplugindir)/$(pureloaderdir)"
	$(MAKE) -C pure-gnm install

install-local:
	test -d "$(DESTDIR)$(gnmlocalplugindir)/$(purefuncdir)" || mkdir -p "$(DESTDIR)$(gnmlocalplugindir)/$(purefuncdir)"
	test -d "$(DESTDIR)$(gnmlocalplugindir)/$(pureglfuncdir)" || mkdir -p "$(DESTDIR)$(gnmlocalplugindir)/$(pureglfuncdir)"
	test -d "$(DESTDIR)$(gnmlocalplugindir)/$(pureloaderdir)" || mkdir -p "$(DESTDIR)$(gnmlocalplugindir)/$(pureloaderdir)"
	cp COPYING README pure-func/*.pure pure-func/plugin.xml "$(DESTDIR)$(gnmlocalplugindir)/$(purefuncdir)"
	cp pure-glfunc/*.pure pure-glfunc/plugin.xml "$(DESTDIR)$(gnmlocalplugindir)/$(pureglfuncdir)"
	cp pure-loader/pure_loader$(DLL) pure-loader/plugin.xml pure-loader/pure-ui.xml "$(DESTDIR)$(gnmlocalplugindir)/$(pureloaderdir)"
	$(MAKE) -C pure-gnm install

uninstall:
	rm -rf "$(DESTDIR)$(gnmplugindir)/$(purefuncdir)" "$(DESTDIR)$(gnmplugindir)/$(pureglfuncdir)" "$(DESTDIR)$(gnmplugindir)/$(pureloaderdir)"
	make -C pure-gnm uninstall

uninstall-local:
	rm -rf "$(DESTDIR)$(gnmlocalplugindir)/$(purefuncdir)" "$(DESTDIR)$(gnmlocalplugindir)/$(pureglfuncdir)" "$(DESTDIR)$(gnmlocalplugindir)/$(pureloaderdir)"
	make -C pure-gnm uninstall

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

distsubdirs = doc debian examples examples/faust examples/myplugin examples/octave pure-func pure-glfunc pure-gnm pure-loader

dist: pure-func/plugin.xml pure-glfunc/plugin.xml
	rm -rf $(dist)
	mkdir $(dist)
	for x in $(distsubdirs); do mkdir $(dist)/$$x; done
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	for x in $(SEDFILES); do rm -f $(dist)/$$x; $(call datesubst,$$PWD/$$x,$(dist)/$$x); done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@
