
# Package name and version number:
dist = pure-faust-$(version)
version = 0.9

# Try to guess the Pure installation prefix:
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
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows
DLL = .dll
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

# Default CXXFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
# These can be set from the command line as usual. Use CXXFLAGS, CPPFLAGS and
# LDFLAGS for additional compiler (-O etc.), preprocessor (-I etc.) and linker
# (-L etc.) options, respectively. LIBS is to be used for additional libraries
# to be linked (-l etc.).

CXXFLAGS = -g -O2

DISTFILES = COPYING COPYING.LESSER Makefile README \
config.guess faust.cc faust.pure faust2.pure \
debian/* examples/Makefile examples/pure.c examples/pure.cpp \
examples/*.pure examples/*.dsp
SEDFILES = README

all: faust$(DLL)

faust$(DLL): faust.o
	$(CXX) $(shared) -o $@ $(PIC) $(LDFLAGS) $< -lpure -lltdl $(LIBS)

faust.o: faust.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(PIC) -o $@ -c $<

clean:
	rm -f faust$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp faust$(DLL) faust.pure faust2.pure "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure"/faust$(DLL) "$(DESTDIR)$(libdir)/pure"/faust.pure "$(DESTDIR)$(libdir)/pure"/faust2.pure

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
