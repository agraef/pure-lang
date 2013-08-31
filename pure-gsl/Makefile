
# Package name and version number:
dist = pure-gsl-$(version)
version = 0.11

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
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows
EXE = .exe
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
# These can be set from the command line as usual. Use CFLAGS, CPPFLAGS and
# LDFLAGS for compiler (-O etc.), preprocessor (-I etc.) and linker (-L etc.) 
# options, respectively. LIBS is to be used for additional libraries to be
# linked (-l etc.).

CFLAGS = -g -O2

FLAGS = $(CPPFLAGS) $(CFLAGS) $(PIC) $(LDFLAGS)

DISTFILES = COPYING Makefile README config.guess debian/* examples/*.pure gsl/*.pure gsl.c gsl.pure

all: gsl$(DLL)

gsl$(DLL): gsl.c
	$(CC) $(shared) -o $@ $< $(FLAGS) -lgsl -lgslcblas -lm -lpure $(LIBS)

clean:
	rm -f *$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	mkdir -p "$(DESTDIR)$(libdir)/pure/gsl"
	cp gsl.pure gsl$(DLL) "$(DESTDIR)$(libdir)/pure"
	cp gsl/*.pure "$(DESTDIR)$(libdir)/pure/gsl/"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/gsl.pure" "$(DESTDIR)$(libdir)/pure/gsl$(DLL)"
	rm -R -f "$(DESTDIR)$(libdir)/pure/gsl"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/examples && mkdir $(dist)/gsl
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
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

# Generate the documentation from the library sources.
# Keep the files in order of the GSL manual Chapter order.

sources = gsl.pure gsl/poly.pure gsl/sf.pure gsl/matrix.pure gsl/fit.pure gsl/stats.pure gsl/randist.pure gsl/sort.pure
target = pure-gsl

$(target).txt: $(sources)
	pure-doc -is $(sources) > $@
