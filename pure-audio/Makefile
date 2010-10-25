
# Package name and version number:
dist = pure-audio-$(version)
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
shared = -shared

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows (untested)
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

# Default CFLAGS are -g -O2, CPPFLAGS, LDFLAGS and LIBS are empty by default.
# These can be set from the command line as usual. Use CFLAGS, CPPFLAGS and
# LDFLAGS for compiler (-O etc.), preprocessor (-I etc.) and linker (-L etc.) 
# options, respectively. LIBS is to be used for additional libraries to be
# linked (-l etc.).

CFLAGS = -g -O2

FLAGS = $(CPPFLAGS) $(CFLAGS) $(PIC) $(LDFLAGS)

DISTFILES = COPYING Makefile README config.guess \
examples/*.pure examples/*.pd audio.c audio.pure portaudio.pure \
sndfile/Makefile sndfile/sfinfo.c sndfile/sfinfo.h sndfile/sf.pure \
sndfile/sndfile.pure

# Makefile to build the PortAudio and libsndfile wrappers.

.PHONY: all clean realclean subdirs generate install uninstall dist distcheck

all: audio$(DLL) portaudio.pure subdirs

subdirs:
	$(MAKE) -C sndfile all

clean:
	rm -f *.o *$(DLL)
	$(MAKE) -C sndfile clean

realclean:
	rm -f *.o *$(DLL) portaudio.pure
	$(MAKE) -C sndfile realclean

audio$(DLL): audio.c
	$(CC) $(shared) -o $@ $< $(FLAGS) -lportaudio -lpthread -lpure $(LIBS)

# Type 'make generate' to force a rebuild of the wrappers. This requires
# pure-gen and that you have required header files in the $(includedir)
# directory.

includedir = /usr/include

generate:
	rm -f portaudio.pure
	make portaudio.pure
	$(MAKE) -C sndfile generate

portaudio.pure: $(includedir)/portaudio.h
	pure-gen -m Pa -p Pa -s '$(includedir)/portaudio.h::' $< -laudio -o $@

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp audio.pure portaudio.pure audio$(DLL) sndfile/sndfile.pure sndfile/sfinfo$(DLL) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/audio.pure" "$(DESTDIR)$(libdir)/pure/portaudio.pure" "$(DESTDIR)$(libdir)/pure/sndfile.pure" "$(DESTDIR)$(libdir)/pure/audio$(DLL)" "$(DESTDIR)$(libdir)/pure/sfinfo$(DLL)"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples && mkdir $(dist)/sndfile
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
