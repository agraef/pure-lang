
# Package name and version number:
dist = pd-pure-$(version)
version = 0.26

# Here are some variables you can specify when invoking 'make', besides the
# usual CXXFLAGS, CPPFLAGS etc.:

# PD=pd: This sets the flavor of Pd for which pd-pure is to be installed. By
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

# Specific setup for the various Pd flavors.
PD_INC=/$(PD)
ifeq ($(PD),pd-extended)
PD_INC=/pdextended
endif

# Pd include path. This is searched for m_pd.h if we have it. Otherwise the
# generic header file pd/m_pd.h in the sources is used. This enables us to
# compile pd-pure on systems where Pd isn't installed in the usual places
# (most importantly, Mac and Windows).
pdincdir = $(includedir)$(PD_INC)

# Pd library path.
pdlibdir = $(libdir)/$(PD)

# Install dir for the external and accompanying stuff.
pdextradir = $(pdlibdir)/extra

# pd-pure external goes into the pure subdir.
pdpuredir = $(pdextradir)/pure

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
ifeq "$(PD)" "pd"
ifneq "$(wildcard /c/Program*/Pd/src/m_pd.h)" ""
# Special case: Vanilla Pd on Windows. This assumes an existing Pd
# installation in the "Program Files" folder. Note that installing directly
# into $(pdlibdir) will fail (permission problem), thus you'll have to install
# into a staging directgory instead (using DESTDIR) and then manually copy
# files over.
pdlibdir = $(wildcard /c/Program*/Pd)
pdincdir = "$(pdlibdir)/src"
LIBS = "$(pdlibdir)/bin/pd.dll"
endif
endif
ifneq "$(PD)" "pd"
ifneq "$(wildcard /c/Program*/Purr*Data/include/m_pd.h)" ""
# Special case: Purr Data flavor of Pd on Windows. This will be tried anyway
# if $(PD) is set to anything else but "pd".
pdlibdir = $(wildcard /c/Program*/Purr*Data)
pdincdir = "$(pdlibdir)/include"
LIBS = -L"$(pdlibdir)/bin" -lpd
endif
endif
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

override CPPFLAGS += -I$(pdincdir) -Ipd $(pureinc)

DISTFILES = COPYING Makefile README config.guess pure.c pd/*.h \
debian/* lib/*.pure examples/*.pure examples/*.pd examples/*.el \
examples/libexample/Makefile examples/libexample/*.pure examples/libexample/*.c examples/libexample/*.pd
SEDFILES = README

all: pure$(DLL)

pure$(DLL): pure.o
	$(CC) $(shared) -o $@ $(PIC) $(LDFLAGS) $< $(purelib) $(LIBS)

pure.o: pure.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(PIC) -o $@ -c $< -DPD="\"$(PD)\"" -DVERSION="\"$(version)\"" -DLIBDIR="\"$(pdlibdir)\"" $(PD_MENU_FLAGS)

clean:
	rm -f pure$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(pdpuredir)" || mkdir -p "$(DESTDIR)$(pdpuredir)"
	cp COPYING README Makefile pure.c pure$(DLL) examples/*.pure examples/*.pd examples/*.el "$(DESTDIR)$(pdpuredir)"
	test -d "$(DESTDIR)$(pdextradir)" || mkdir -p "$(DESTDIR)$(pdextradir)"
	cp examples/pure-remote.pd "$(DESTDIR)$(pdextradir)"
	test -d "$(DESTDIR)$(pdpuredir)/lib" || mkdir -p "$(DESTDIR)$(pdpuredir)/lib"
	cp lib/*.pure "$(DESTDIR)$(pdpuredir)/lib"
	test -d "$(DESTDIR)$(pdpuredir)/libexample" || mkdir -p "$(DESTDIR)$(pdpuredir)/libexample"
	cp examples/libexample/Makefile examples/libexample/*.pure examples/libexample/*.c examples/libexample/*.pd "$(DESTDIR)$(pdpuredir)/libexample"

uninstall:
	rm -rf "$(DESTDIR)$(pdpuredir)" "$(DESTDIR)$(pdextradir)/pure-remote.pd"

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/lib && mkdir $(dist)/pd && mkdir $(dist)/examples && mkdir $(dist)/examples/libexample
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
	wget -nv https://github.com/agraef/pure-lang/releases/download/$(dist)/$(dist).tar.gz -O $@

# Special rules to build the Pd-L2Ork and Purr-Data versions.

l2ork-debsrc = $(shell echo $(dist) | sed -e 's/pd-pure/pdl2ork-pure/' -e 's/-$(version)/_$(version)/').orig.tar.gz

l2ork-deb: $(l2ork-debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist)/debian && mkdir l2ork && (sed -e 's/pd-pure/pdl2ork-pure/g' changelog > l2ork/changelog) && (sed -e 's/pd-pure/pdl2ork-pure/g' -e 's/puredata/pd-l2ork/g' control > l2ork/control) && (sed -e 's/--/-- PD=pd-l2ork/g' rules > l2ork/rules) && chmod a+x l2ork/rules && mv -f l2ork/* . && rmdir l2ork
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(l2ork-debsrc):
	wget -nv https://github.com/agraef/pure-lang/releases/download/$(dist)/$(dist).tar.gz -O $@

purrdata-debsrc = $(shell echo $(dist) | sed -e 's/pd-pure/purrdata-pure/' -e 's/-$(version)/_$(version)/').orig.tar.gz

purrdata-deb: $(purrdata-debsrc) dist
	tar xfz $(dist).tar.gz
	cd $(dist)/debian && mkdir purrdata && (sed -e 's/pd-pure/purrdata-pure/g' changelog > purrdata/changelog) && (sed -e 's/pd-pure/purrdata-pure/g' -e 's/puredata/purr-data/g' control > purrdata/control) && (sed -e 's/--/-- PD=purr-data/g' rules > purrdata/rules) && chmod a+x purrdata/rules && mv -f purrdata/* . && rmdir purrdata
	cd $(dist) && debuild $(DEBUILD_FLAGS)
	rm -rf $(dist)

$(purrdata-debsrc):
	wget -nv https://github.com/agraef/pure-lang/releases/download/$(dist)/$(dist).tar.gz -O $@
