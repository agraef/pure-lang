
# This uses mkoctfile to build the module. Octave and Pure must be installed.

# Package name and version number:
dist = pure-octave-$(version)
version = 0.1

# Try to guess the installation prefix (this needs GNU make):
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

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows (untested)
DLL = .dll
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX (untested)
DLL = .dylib
endif

# Add the -rpath flag so that the dynamic linker finds liboctave.so etc. when
# Pure loads the module.
RLD_FLAG=`mkoctfile -p RLD_FLAG`

DISTFILES = COPYING Makefile README config.guess \
embed.cc embed.h gsl_structs.h octave.pure examples/*.pure

all: octave_embed$(DLL)

octave_embed$(DLL): embed.cc embed.h
	mkoctfile -v -o $@ $< -lpure $(RLD_FLAG)

clean:
	rm -f embed.o octave_embed$(DLL) *~ core octave-core

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp octave.pure octave_embed$(DLL) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/octave.pure" "$(DESTDIR)$(libdir)/pure/octave_embed$(DLL)"

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
