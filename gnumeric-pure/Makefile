
# See README for detailed installation instructions. Briefly:

# 1.  Make sure that you have gnumeric and the corresponding development
#     packages installed. You also need to have the gnumeric sources unpacked
#     and configured in ..

# 2.  Run 'make' to build the plugin.

# 3a. Run 'sudo make install' for a global installation in the gnumeric plugin
#     directory (usually something like /usr/local/gnumeric/1.9.x/plugins.

# - OR -

# 3b. Run 'make install-local' for a local installation in your home directory 
#     (usually something like ~/.gnumeric/1.9.x/plugins).

# Finally, make sure that the installation directory is in Gnumeric's plugin
# path. To use the plugin, you must enable 'Pure plugin loader' and 'Pure
# Functions' in the plugin list under 'Tools | Plug-ins'. A few sample
# functions can be found in .../plugins/pure-func/pure_func.pure. Edit that
# script as needed and edit the plugin.xml directory of that file to add the
# functions that you want.

# Package name and version number:
dist = gnumeric-pure-$(version)
version = 0.1

# Try to guess the Gnumeric version number (this needs GNU make):
gnmversion=$(shell gnumeric --version | sed -n -e "s/^gnumeric version '\([0-9.]*\)'.*/\1/" -e "/^[0-9.]*$$/p")

# You might have to adjust this for your GTK+ installation.
INCLUDES = $(shell pkg-config --cflags libgoffice-0.8)
LIBS = $(shell pkg-config --libs libgoffice-0.8)

# NOTE: To compile this plugin outside the gnumeric source tree, you need to
# have the (properly configured) gnumeric sources somewhere, since the plugin
# requires some of Gnumeric's internal headers which aren't usually installed.
# By default, the Makefile assumes that they are in ../gnumeric-$(gnmversion);
# if you have them elsewhere, you'll need to adjust this variable accordingly.
gnmsrc=../gnumeric-$(gnmversion)

# Try to guess the Gnumeric installation prefix (needed to install the plugin):
prefix = $(patsubst %/bin/gnumeric,%,$(shell which gnumeric 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Try to guess the Pure installation prefix (needed to install the docs):
pureprefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(pureprefix)),)
# Fall back to /usr/local.
pureprefix = /usr/local
endif

# Installation goes into $(libdir)/gnumeric, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

# Gnumeric library path.
gnmlibdir = $(libdir)/gnumeric

# Install dir for the plugin and accompanying stuff.
gnmplugindir = $(gnmlibdir)/$(gnmversion)/plugins
gnmlocalplugindir = $(HOME)/.gnumeric/$(gnmversion)/plugins

# Install dir for the documentation.
purelibdir = $(pureprefix)/lib
puredatadir = $(purelibdir)/pure

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

# Default CFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
CFLAGS = -g -O2

DISTFILES = COPYING Makefile README gnumeric-pure.html config.guess \
pure-loader/boot.c pure-loader/pure-gnumeric.c pure-loader/pure-gnumeric.h \
pure-loader/pure-loader.c pure-loader/pure-loader.h pure-loader/plugin.xml \
pure-func/*.pure pure-func/plugin.xml

all: pure-loader/pure_loader$(DLL)

pure-loader/pure_loader$(DLL): pure-loader/boot.o pure-loader/pure-gnumeric.o pure-loader/pure-loader.o
	$(CC) $(shared) -o $@ $(PIC) $(LDFLAGS) $^ $(LIBS) -lpure

%.o: %.c
	$(CC) $(INCLUDES) -I$(gnmsrc) -I$(gnmsrc)/src $(CPPFLAGS) $(CFLAGS) $(PIC) -o $@ -c $<

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
	cp COPYING README pure-func/*.pure pure-func/plugin.xml "$(DESTDIR)$(gnmplugindir)/pure-func"
	cp pure-loader/pure_loader$(DLL) pure-loader/plugin.xml "$(DESTDIR)$(gnmplugindir)/pure-loader"
	test -d "$(DESTDIR)$(puredatadir)" || mkdir -p "$(DESTDIR)$(puredatadir)"
	cp gnumeric-pure.html "$(DESTDIR)$(puredatadir)"

install-local:
	test -d "$(DESTDIR)$(gnmlocalplugindir)/pure-func" || mkdir -p "$(DESTDIR)$(gnmlocalplugindir)/pure-func"
	test -d "$(DESTDIR)$(gnmlocalplugindir)/pure-loader" || mkdir -p "$(DESTDIR)$(gnmlocalplugindir)/pure-loader"
	cp COPYING README gnumeric-pure.html pure-func/*.pure pure-func/plugin.xml "$(DESTDIR)$(gnmlocalplugindir)/pure-func"
	cp pure-loader/pure_loader$(DLL) pure-loader/plugin.xml "$(DESTDIR)$(gnmlocalplugindir)/pure-loader"

uninstall:
	rm -rf "$(DESTDIR)$(gnmplugindir)/pure-func" "$(DESTDIR)$(gnmplugindir)/pure-loader" "$(DESTDIR)$(puredatadir)"/gnumeric-pure.html

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
	cd $(dist) && make gnmsrc=../$(gnmsrc) && make install DESTDIR=./BUILD
	rm -rf $(dist)
