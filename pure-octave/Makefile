
# This uses mkoctfile to build the module. Octave and Pure must be installed.

# Package name and version number:
dist = pure-octave-$(version)
version = 0.5

# Try to guess the Octave version number. We're only interested in the
# major/minor version here, to cope with the C/C++ API breakage in Octave 3.8.
octversion=$(shell mkoctfile --version 2>&1 | sed -e 's/^mkoctfile, version \([0-9.]*\).*/\1/' | sed -e 's/\([0-9]*\)[.]\([0-9]*\).*/-DOCTAVE_\1_\2/')

# Try to guess the installation prefix (this needs GNU make):
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

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows (untested)
EXE = .exe
DLL = .dll
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX (untested)
DLL = .dylib
endif

# Add the -rpath flag so that the dynamic linker finds liboctave.so etc. when
# Pure loads the module.
RLD_FLAG=`mkoctfile -p RLD_FLAG`

DISTFILES = COPYING Makefile README config.guess embed.cc embed.h \
gsl_structs.h gnuplot.pure octave.pure debian/* examples/*.pure
SEDFILES = README

all: octave_embed$(DLL)

octave_embed$(DLL): embed.cc embed.h
	rm -f $@
	mkoctfile -v $(octversion) -o $@ $< -lpure $(RLD_FLAG)
	if test -f $@.oct; then mv $@.oct $@; fi

clean:
	rm -f embed.o octave_embed$(DLL) *~ core octave-core

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp gnuplot.pure octave.pure octave_embed$(DLL) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/gnuplot.pure" "$(DESTDIR)$(libdir)/pure/octave.pure" "$(DESTDIR)$(libdir)/pure/octave_embed$(DLL)"

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

debsrc = $(shell echo $(dist) | sed -e 's/-$(version)/_$(version)/').orig.tar.gz

deb: $(debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(debsrc):
	wget -nv https://bitbucket.org/purelang/pure-lang/downloads/$(dist).tar.gz -O $@
