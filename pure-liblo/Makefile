
# Package name and version number:
dist = pure-liblo-$(version)
version = 0.8

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
LIBS = -lgmp

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows
EXE = .exe
DLL = .dll
LIBS = -lws2_32 -lgmp
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

LO_CFLAGS = $(shell pkg-config --cflags liblo) $(shell pkg-config --atleast-version=0.26 liblo && echo '-DHAVE_TIMETAG=1')
LO_LIBS = $(shell pkg-config --libs liblo)

DISTFILES = COPYING COPYING.LESSER Makefile README config.guess \
debian/* examples/*.pure lo.c lo.pure lo-add.c lo-add.pure osc.pure
SEDFILES = README

all: lo$(DLL)

lo$(DLL): lo.c lo-add.c
	$(CC) $(shared) -o $@ $^ $(LO_CFLAGS) $(FLAGS) -lpure $(LO_LIBS) $(LIBS)

clean:
	rm -f *$(DLL) *~ *.a *.o

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp lo.pure lo$(DLL) osc.pure "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/lo.pure" "$(DESTDIR)$(libdir)/pure/lo$(DLL)" "$(DESTDIR)$(libdir)/pure/osc.pure"

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

#############################################################################
# Generator stuff. You only need this if you want to regenerate the wrappers.
# You need pure-gen and the liblo headers to do this.
#############################################################################

# Path to the installed liblo headers (the lo subdir is appended).

# NOTE: The path needs to be valid at generation time only. The generated C
# source uses a relative path which should work on most systems which have
# liblo installed.

includedir = $(shell pkg-config --variable=prefix liblo)/include

# The following need to be excluded since they are internal or need special
# treatment. In the latter case we provide Purified wrappers in lo-add.c/pure
# instead.

excludes = '^(lo_address_get_url|lo_server_get_url|lo_server_thread_get_url|lo_message_add|lo_message_add_varargs|lo_send|lo_send_timestamped|lo_send_from|lo_.+_internal|LO_MARKER_A|LO_MARKER_B)$$'

############################################################################

generate:
	rm -rf lo.pure lo.c
	$(MAKE) all

# NOTE: For compatibility with Pure 0.45 and later, we edit the final Pure
# module to substitute void* for lo_timetag* so that we can easily pass Pure
# int vectors as timestamps. (Using void* rather than int* will also work with
# older Pure versions. In the future we really want to use int* instead.)

lo.pure lo.c: lo-add.pure
	pure-gen -fc-auto -p lo -m lo -s '$(includedir)/lo/*.h;' -x $(excludes) '$(includedir)/lo/lo.h'
	sed -e 's|#include \"$(includedir)/\(\([A-Za-z-]\+/\)\?[A-Za-z-]\+\.h\)\"|#include <\1>|g' < lo.c > lo.c.new && rm lo.c && mv lo.c.new lo.c
	cat lo.pure lo-add.pure | sed -e 's|lo_timetag[*]|void/*lo_timetag*/*|g' > lo.pure.new && rm lo.pure && mv lo.pure.new lo.pure
