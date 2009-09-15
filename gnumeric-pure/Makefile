
# This Makefile requires GNU make.

# Package name and version number:
dist = gnumeric-pure-$(version)
version = 0.2

# Build and installation instructions:

# 1.  Make sure that you have gnumeric and the corresponding development
#     packages installed. You'll also need the gnumeric-features.h file from
#     the Gnumeric sources, place it into the directory with the gnumeric-pure
#     sources, or specify the include path needed to locate these files in
#     step 2 below.

# 2.  Adjust the following as needed for your GTK+/GOffice installation:

INCLUDES = $(shell pkg-config --cflags libgoffice-0.8) \
-I$(prefix)/include/libspreadsheet-1.10/spreadsheet
LIBS = $(shell pkg-config --libs libgoffice-0.8)

#     The Makefile tries to guess the installation path and version number of
#     Gnumeric on its own. If it guesses wrong, you'll also have to adjust the
#     values of the prefix and gnmversion variables (see below) accordingly.

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
gnmlocalplugindir = $(HOME)/.gnumeric/$(gnmversion)/plugins

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

# Default CFLAGS and CPPFLAGS, LDFLAGS and LIBS are empty by default.
CFLAGS = -g -O2
CPPFLAGS = -Wall

DISTFILES = COPYING Makefile README gnumeric-pure.html pure-examples.gnumeric \
config.guess \
pure-loader/boot.c pure-loader/pure-gnumeric.c pure-loader/pure-gnumeric.h \
pure-loader/pure-loader.c pure-loader/pure-loader.h \
pure-loader/plugin.xml pure-loader/pure-ui.xml \
pure-func/*.pure pure-func/plugin.xml

all: pure-loader/pure_loader$(DLL)

pure-loader/pure_loader$(DLL): pure-loader/boot.o pure-loader/pure-gnumeric.o pure-loader/pure-loader.o
	$(CC) $(shared) -o $@ $(LDFLAGS) $^ $(LIBS) -lpure

%.o: %.c
	$(CC) $(INCLUDES) -I. -I.. $(CPPFLAGS) -DPLUGIN_VERSION='"$(version)"' $(CFLAGS) $(PIC) -o $@ -c $<

clean:
	(cd pure-loader/ && rm -f pure_loader$(DLL) *~ *.a *.o)
	rm -f *~ *.tex *.dvi *.pdf *.aux *.log *.out

# documentation (this needs docutils)

html: gnumeric-pure.html
latex: gnumeric-pure.tex
pdf: gnumeric-pure.pdf

gnumeric-pure.html: README
	rst2html.py $< $@

gnumeric-pure.tex: README
	rst2latex.py $< $@

gnumeric-pure.pdf: gnumeric-pure.tex
	pdflatex $<
	pdflatex $<
	rm -f *.aux *.log *.out

install:
	test -d "$(DESTDIR)$(gnmplugindir)/pure-func" || mkdir -p "$(DESTDIR)$(gnmplugindir)/pure-func"
	test -d "$(DESTDIR)$(gnmplugindir)/pure-loader" || mkdir -p "$(DESTDIR)$(gnmplugindir)/pure-loader"
	cp COPYING README gnumeric-pure.html pure-func/*.pure pure-func/plugin.xml "$(DESTDIR)$(gnmplugindir)/pure-func"
	cp pure-loader/pure_loader$(DLL) pure-loader/plugin.xml pure-loader/pure-ui.xml "$(DESTDIR)$(gnmplugindir)/pure-loader"

install-local:
	test -d "$(DESTDIR)$(gnmlocalplugindir)/pure-func" || mkdir -p "$(DESTDIR)$(gnmlocalplugindir)/pure-func"
	test -d "$(DESTDIR)$(gnmlocalplugindir)/pure-loader" || mkdir -p "$(DESTDIR)$(gnmlocalplugindir)/pure-loader"
	cp COPYING README gnumeric-pure.html pure-func/*.pure pure-func/plugin.xml "$(DESTDIR)$(gnmlocalplugindir)/pure-func"
	cp pure-loader/pure_loader$(DLL) pure-loader/plugin.xml pure-loader/pure-ui.xml "$(DESTDIR)$(gnmlocalplugindir)/pure-loader"

uninstall:
	rm -rf "$(DESTDIR)$(gnmplugindir)/pure-func" "$(DESTDIR)$(gnmplugindir)/pure-loader"

uninstall-local:
	rm -rf "$(DESTDIR)$(gnmlocalplugindir)/pure-func" "$(DESTDIR)$(gnmlocalplugindir)/pure-loader"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/pure-func && mkdir $(dist)/pure-loader
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
