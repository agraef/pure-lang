
# Package name and version number:
dist = pure-gsl-$(version)
version = 0.12

# compiler flags -- these can be overridden from the command line if needed,
# as well as CPPFLAGS, LDFLAGS and LIBS
CFLAGS = -O3
#CFLAGS = -g -O2

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

prefix      = $(shell pkg-config pure --variable prefix)
bindir      = $(prefix)/bin
libdir      = $(shell pkg-config pure --variable libdir)

ifeq ($(DLL),.dylib)
# OSX: hardcode the installation path into the module, so that the module can
# be linked in by the batch compiler.
dllname = -install_name "$(libdir)/pure/$@"
endif

ifeq ($(DLL),.dll)
LDFLAGS = -Wl,--enable-auto-import
endif

LIBS = -lgsl -lgslcblas -lm

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS) $(LIBS)

DISTFILES = COPYING Makefile README debian/* examples/*.pure gsl/*.pure gsl.c gsl.pure

all: gsl$(DLL)

gsl$(DLL): gsl.c
	$(CC) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) $< $(MOD_LDFLAGS)

clean:
	rm -Rf *$(DLL)* *~ *.a *.o

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
