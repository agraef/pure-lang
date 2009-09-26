
# This Makefile requires GNU make.

# Package name and version number:
dist = gnumeric-pure-$(version)
version = 0.3

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

GODEPS=libgoffice-0.8

INCLUDES = `pkg-config --cflags $(GODEPS)` \
-I$(prefix)/include/libspreadsheet-1.10/spreadsheet
LIBS = `pkg-config --libs $(GODEPS)`

#     Adjust the following as needed to build with OpenGL support (requires
#     GtkGLExt). Or comment out the following line if you don't want this.

GLDEPS=gtkglext-1.0 gtkglext-x11-1.0

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
# functions that you want. See the README file or gnumeric-pure.html for more
# information.

# Try to guess the Gnumeric version number:
gnmversion=$(shell gnumeric --version | sed -n -e "s/^gnumeric version '\([0-9.]*\)'.*/\1/" -e "/^[0-9.]*$$/p")

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
# Windows/mingw (untested)
DLL = .dll
LDFLAGS = -Wl,--enable-auto-import
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
LIBS += -lstdc++ `pkg-config --libs $(GLDEPS)`
endif

# Default CFLAGS and CPPFLAGS, LDFLAGS and LIBS are empty by default.
CFLAGS = -g -O2
CPPFLAGS = -Wall

DISTFILES = ChangeLog COPYING Makefile README \
gnumeric-pure.html preamble.tex config.guess \
pure-loader/boot.c pure-loader/pure-gnumeric.c pure-loader/pure-gnumeric.h \
pure-loader/pure-loader.c pure-loader/pure-loader.h \
pure-loader/plugin.xml pure-loader/pure-ui.xml \
pure-func/*.pure pure-func/plugin.xml \
pure-glfunc/*.pure pure-glfunc/plugin.xml examples/*.gnumeric

all: pure-loader/pure_loader$(DLL)

pure-loader/pure_loader$(DLL): pure-loader/boot.o pure-loader/pure-gnumeric.o pure-loader/pure-loader.o
	$(CC) $(shared) -o $@ $(LDFLAGS) $^ $(LIBS) -lpure

%.o: %.c
	$(CC) $(INCLUDES) -I. -I.. $(CPPFLAGS) -DGNM_VERSION='"$(gnmversion)"' -DPLUGIN_VERSION='"$(version)"' -DPURE_INCLUDES='"-I$(gnmplugindir)/$(purefuncdir)","-I$(gnmlocalplugindir)/$(purefuncdir)"' $(CFLAGS) $(PIC) -o $@ -c $<

clean:
	(cd pure-loader/ && rm -f pure_loader$(DLL) *~ *.a *.o)
	rm -f *~ gnumeric-pure.tex *.dvi *.pdf *.aux *.log *.out *.toc

# documentation (this needs docutils)

html: gnumeric-pure.html
latex: gnumeric-pure.tex
pdf: gnumeric-pure.pdf

gnumeric-pure.html: README
	rst2html.py $< $@

# Adjust these as needed.
LATEXOPT = --use-latex-toc --use-latex-docinfo --stylesheet=preamble.tex --documentoptions="11pt" --no-section-numbering

gnumeric-pure.tex: README
	rst2latex.py $(LATEXOPT) $< $@

%.pdf: %.tex preamble.tex
	pdflatex $<
	pdflatex $<
	rm -f *.aux *.log *.out *.toc

install:
	test -d "$(DESTDIR)$(gnmplugindir)/$(purefuncdir)" || mkdir -p "$(DESTDIR)$(gnmplugindir)/$(purefuncdir)"
	test -d "$(DESTDIR)$(gnmplugindir)/$(pureglfuncdir)" || mkdir -p "$(DESTDIR)$(gnmplugindir)/$(pureglfuncdir)"
	test -d "$(DESTDIR)$(gnmplugindir)/$(pureloaderdir)" || mkdir -p "$(DESTDIR)$(gnmplugindir)/$(pureloaderdir)"
	cp COPYING README gnumeric-pure.html pure-func/*.pure pure-func/plugin.xml "$(DESTDIR)$(gnmplugindir)/$(purefuncdir)"
	cp pure-glfunc/*.pure pure-glfunc/plugin.xml "$(DESTDIR)$(gnmplugindir)/$(pureglfuncdir)"
	cp pure-loader/pure_loader$(DLL) pure-loader/plugin.xml pure-loader/pure-ui.xml "$(DESTDIR)$(gnmplugindir)/$(pureloaderdir)"

install-local:
	test -d "$(DESTDIR)$(gnmlocalplugindir)/$(purefuncdir)" || mkdir -p "$(DESTDIR)$(gnmlocalplugindir)/$(purefuncdir)"
	test -d "$(DESTDIR)$(gnmlocalplugindir)/$(pureglfuncdir)" || mkdir -p "$(DESTDIR)$(gnmlocalplugindir)/$(pureglfuncdir)"
	test -d "$(DESTDIR)$(gnmlocalplugindir)/$(pureloaderdir)" || mkdir -p "$(DESTDIR)$(gnmlocalplugindir)/$(pureloaderdir)"
	cp COPYING README gnumeric-pure.html pure-func/*.pure pure-func/plugin.xml "$(DESTDIR)$(gnmlocalplugindir)/$(purefuncdir)"
	cp pure-glfunc/*.pure pure-glfunc/plugin.xml "$(DESTDIR)$(gnmlocalplugindir)/$(pureglfuncdir)"
	cp pure-loader/pure_loader$(DLL) pure-loader/plugin.xml pure-loader/pure-ui.xml "$(DESTDIR)$(gnmlocalplugindir)/$(pureloaderdir)"

uninstall:
	rm -rf "$(DESTDIR)$(gnmplugindir)/$(purefuncdir)" "$(DESTDIR)$(gnmplugindir)/$(pureglfuncdir)" "$(DESTDIR)$(gnmplugindir)/$(pureloaderdir)"

uninstall-local:
	rm -rf "$(DESTDIR)$(gnmlocalplugindir)/$(purefuncdir)" "$(DESTDIR)$(gnmlocalplugindir)/$(pureglfuncdir)" "$(DESTDIR)$(gnmlocalplugindir)/$(pureloaderdir)"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples && mkdir $(dist)/pure-func $(dist)/pure-glfunc && mkdir $(dist)/pure-loader
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
