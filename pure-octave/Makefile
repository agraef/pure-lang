
# This uses mkoctfile to build the module. Octave and Pure must be installed.

# Package name and version number:
dist = pure-octave-$(version)
version = 0.13

# platform-specific setup

DLL         = $(shell pkg-config pure --variable DLL)
PIC         = $(shell pkg-config pure --variable PIC)
shared      = $(shell pkg-config pure --variable shared)

prefix      = $(shell pkg-config pure --variable prefix)
bindir      = $(prefix)/bin
libdir      = $(shell pkg-config pure --variable libdir)

ifeq ($(DLL),.dylib)
dllname = -install_name "$(libdir)/pure/$@"
# MAC-specific quirk: echo -n doesn't work in the standard shell
ECHON = /bin/echo -n
else
ECHON = echo -n
endif

# The mkoctfile to use. Adjust this as needed if it's named differently, isn't
# in the standard PATH or if you have multiple Octave installations on your
# system.
mkoctfile = mkoctfile

# Try to guess the Octave version number. We're only interested in the
# major/minor version here, to cope with the C/C++ API breakage in Octave 3.8+.
octversion = $(shell $(mkoctfile) --version 2>&1 | sed -e 's/^mkoctfile, version \([0-9.]*\).*/\1/' | sed -e 's/\([0-9]*\)[.]\([0-9]*\).*/\1 \2/')
octversionflag = -DOCTAVE_MAJOR=$(word 1,$(octversion)) -DOCTAVE_MINOR=$(word 2,$(octversion))

# Add the -rpath flag so that the dynamic linker finds liboctave.so etc. when
# Pure loads the module. NOTE: This doesn't seem to be needed any more.
RLD_FLAG=$(shell $(mkoctfile) -p RLD_FLAG)

# Octave 5.1 doesn't automatically include these linker options any more.
OCT_FLAGS=-L$(shell $(mkoctfile) -p OCTLIBDIR) $(shell $(mkoctfile) -p LIBOCTINTERP) $(shell $(mkoctfile) -p LIBOCTAVE)

DISTFILES = COPYING Makefile README embed.cc embed.h \
gsl_structs.h gnuplot.pure octave.pure debian/* examples/*.pure
SEDFILES = README

all: octave_embed$(DLL)

ifeq ($(DLL),.dylib)
# OS X quirk: mkoctfile wants to create a bundle, but we want just a
# dylib which can be linked into other modules.
embed.o: embed.cc embed.h
	rm -f $@
	$(mkoctfile) -v $(octversionflag) -o $@ -c $<
MKOCT_LDCXX = $(shell $(mkoctfile) -p LD_CXX)
MKOCT_LDFLAGS = $(shell $(mkoctfile) -p LFLAGS) $(shell $(mkoctfile) -p OCTAVE_LIBS) $(shell $(mkoctfile) -p LDFLAGS)
octave_embed$(DLL): embed.o
	rm -f $@
	$(MKOCT_LDCXX) $(shared) $(dllname) -o $@ embed.o $(MKOCT_LDFLAGS) -lpure $(RLD_FLAG)
else
ifeq ($(DLL),.dll)
# Custom build commands for msys2/mingw32 using a static build of Octave 4.0.3.
# Set this to the staging directory where your build of Octave is installed.
octave-buildroot = $(HOME)/buildroot-octave/mingw32
#octave-buildroot = c:/msys64/home/ag/buildroot-octave/mingw32
octave_embed$(DLL): embed.cc embed.h
	rm -f $@
	g++ -c -mieee-fp -g -O2 -I$(octave-buildroot)/include/octave-4.0.3/octave -I$(octave-buildroot)/include/octave-4.0.3 -DOCTAVE_MAJOR=4 -DOCTAVE_MINOR=0 $< -o embed.o
	g++ -mieee-fp -g -O2 -shared -Wl,--export-all-symbols -Wl,--enable-auto-import -Wl,--enable-runtime-pseudo-reloc  -o $@  embed.o -lpure -L$(octave-buildroot)/lib/octave/4.0.3 -loctinterp -loctave -lglpk -lportaudio -ldsound -lsetupapi -lwinmm -lole32 -luuid -lsndfile -lcurl -lopenblas -lreadline -lncurses -lpcre -lquadmath -lhdf5 -lfftw3 -lfftw3f -lfontconfig -lfreetype -lopengl32 -lglu32 -lgdi32 -lws2_32 -lgfortran -lz
	strip $@
else
octave_embed$(DLL): embed.cc embed.h
	rm -f $@
	$(mkoctfile) -v $(octversionflag) -o $@ $< -lpure $(RLD_FLAG) $(OCT_FLAGS)
	if test -f $@.oct; then mv $@.oct $@; fi
endif
endif

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
	wget -nv https://github.com/agraef/pure-lang/releases/download/$(dist)/$(dist).tar.gz -O $@
