
# Package name and version number:
dist = pd-pure-$(version)
version = 0.18

# Here are some variables you can specify when invoking 'make', besides the
# usual CXXFLAGS, CPPFLAGS etc.:

# PD=pd: This sets the flavour of Pd for which pd-pure is to be installed. By
# default this is just 'pd', which will work with Miller Puckette's "vanilla"
# distribution of Pd. You can set this, e.g., to 'pd-extended' or 'pd-l2ork'
# if you're running these alternative Pd distributions instead.

# VERBOSE=-DVERBOSE: This enables special code to make pd-pure give some
# feedback on stdout when it's compiling scripts. This is disabled by default.

# EAGER=-DEAGER: This enables special code to enforce eager compilation at
# script load time. This is enabled automatically for Pure versions which
# support this (0.34 and later), thus you shouldn't have to set this flag
# manually. However, you can forcibly disable this option (not recommended) by
# passing 'EAGER='.

PD=pd

# Pd executable name variants.
PDEXE=$(subst pd-extended,pdextended,$(PD))

# Try to guess the Pd installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/$(PDEXE)$(EXE),%,$(shell which $(PDEXE) 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Try to guess the Pure minor version number, and set up the EAGER option
# accordingly:
pureminor = $(shell pure --version 2>/dev/null | sed -n -e 's/^Pure 0\.\([0-9]*\).*/\1/' -e '/^[0-9]*$$/p' 2>/dev/null)
EAGER = $(shell (test -z "$(pureminor)" || test "$(pureminor)" -ge 34) && echo '-DEAGER')

# Pure include and library dir.
pureinc = $(shell pkg-config pure --cflags)
purelib = $(shell pkg-config pure --libs)

# Installation goes into $(libdir)/$(PD), you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib
includedir = $(prefix)/include

# Pd library path.
pdlibdir = $(libdir)/$(PD)

# Install dir for the external and accompanying stuff.
pdextradir = $(pdlibdir)/extra/pure

# Try to guess the host system type.
host = $(shell ./config.guess)

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows
EXE = .exe
DLL = .dll
LIBS = -lpd
LDFLAGS = -Wl,--enable-auto-import
endif
ifneq "$(findstring -linux,$(host))" ""
# Linux
DLL = .pd_linux
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX
DLL = .pd_darwin
shared = -dynamiclib -Wl,-undefined -Wl,dynamic_lookup
endif
ifneq "$(findstring x86_64-,$(host))" ""
# 64 bit, needs -fPIC flag
PIC = -fPIC
endif

# Default CFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
# If Pure 0.34 or later is used, -DEAGER is added to CPPFLAGS. These variables
# can be set from the command line as usual. Use CFLAGS, CPPFLAGS and LDFLAGS
# for additional compiler (-O etc.), preprocessor (-I etc.) and linker (-L
# etc.) options, respectively. LIBS is to be used for additional libraries to
# be linked (-l etc.).

CFLAGS = -g -O2
ifneq "$(EAGER)" ""
override CPPFLAGS += $(EAGER)
endif
ifneq "$(VERBOSE)" ""
override CPPFLAGS += $(VERBOSE)
endif

override CPPFLAGS += -Ipd $(pureinc)

DISTFILES = COPYING Makefile README config.guess pure.c pd/*.h \
debian/* examples/*.pure examples/*.pd examples/*.el \
examples/lib/Makefile examples/lib/*.pure examples/lib/*.c examples/lib/*.pd
SEDFILES = README

all: pure$(DLL)

pure$(DLL): pure.o
	$(CC) $(shared) -o $@ $(PIC) $(LDFLAGS) $< $(purelib) $(LIBS)

pure.o: pure.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(PIC) -o $@ -c $< -DPD="\"$(PD)\"" -DVERSION="\"$(version)\"" -DLIBDIR="\"$(pdlibdir)\""

clean:
	rm -f pure$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(pdextradir)" || mkdir -p "$(DESTDIR)$(pdextradir)"
	cp COPYING README Makefile pure.c pure$(DLL) examples/*.pure examples/*.pd examples/*.el "$(DESTDIR)$(pdextradir)"
	test -d "$(DESTDIR)$(pdextradir)/lib" || mkdir -p "$(DESTDIR)$(pdextradir)/lib"
	cp examples/lib/Makefile examples/lib/*.pure examples/lib/*.c examples/lib/*.pd "$(DESTDIR)$(pdextradir)/lib"

uninstall:
	rm -rf "$(DESTDIR)$(pdextradir)"

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/pd && mkdir $(dist)/examples && mkdir $(dist)/examples/lib
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
