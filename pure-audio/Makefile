
# Package name and version number:
dist = pure-audio-$(version)
version = 0.5

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
realtime = realtime

# Take care of some common systems.
ifneq "$(findstring -mingw,$(host))" ""
# Windows
EXE = .exe
DLL = .dll
#realtime = 
windows = true
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
debian/* examples/*.pure examples/*.pd audio.c audio.pure portaudio.pure \
fftw/Makefile fftw/fftw.c fftw/fftw.pure \
samplerate/Makefile samplerate/srcprocess.c samplerate/srcprocess.h \
samplerate/src.pure samplerate/samplerate.pure \
sndfile/Makefile sndfile/sfinfo.c sndfile/sfinfo.h sndfile/sf.pure \
sndfile/sndfile.pure \
realtime/Makefile realtime/realtime.c realtime/realtime.pure
SEDFILES = README

subdirs = fftw samplerate sndfile $(realtime)

# Makefile to build the PortAudio wrapper and the libsamplerate and libsndfile
# support libraries.

.PHONY: all clean realclean subdirs generate install uninstall dist distcheck

all: audio$(DLL) portaudio.pure subdirs

subdirs:
	for dir in $(subdirs); do $(MAKE) -C $$dir all; done

clean:
	rm -f *.o *$(DLL)
	for dir in $(subdirs); do $(MAKE) -C $$dir clean; done

realclean:
	rm -f *.o *$(DLL) portaudio.pure
	for dir in $(subdirs); do $(MAKE) -C $$dir realclean; done

audio$(DLL): audio.c
	$(CC) $(shared) -o $@ $< $(FLAGS) -lportaudio -lpthread -lpure $(LIBS)

# Type 'make generate' to force a rebuild of the wrappers. This requires
# pure-gen and that you have required header files in the $(includedir)
# directory.

includedir = /usr/include

generate:
	rm -f portaudio.pure
	$(MAKE) portaudio.pure
	$(MAKE) -C samplerate generate
	$(MAKE) -C sndfile generate

portaudio.pure:
ifdef windows
	echo 'using "lib:libportaudio-2.dll";' > $@
	pure-gen -C -ansi -N -m Pa -p Pa -s '$(includedir)/portaudio.h;' $(includedir)/portaudio.h -laudio -o $@
	mv $@ $@.tmp
	tr -d '\r' < $@.tmp > $@
	rm -f $@.tmp
else
	pure-gen -m Pa -p Pa -s '$(includedir)/portaudio.h;' -x '^(linux|unix)$$' $(includedir)/portaudio.h -laudio -o $@
endif

# Type 'make diffs' to generate the wrappers for this platform and compare
# them against the distributed sources.

diffs:
	@$(MAKE) dist > /dev/null
	@rm -rf xxx yyy
	@tar xfz $(dist).tar.gz && mv $(dist) xxx
	@tar xfz $(dist).tar.gz && mv $(dist) yyy
	@rm -f $(dist).tar.gz
	@cd yyy && $(MAKE) generate > /dev/null
	@diff -ru xxx yyy || true
	@rm -rf xxx yyy

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp audio.pure portaudio.pure audio$(DLL) fftw/fftw.pure fftw/fftw$(DLL) samplerate/samplerate.pure samplerate/srcprocess$(DLL) sndfile/sndfile.pure sndfile/sfinfo$(DLL) "$(DESTDIR)$(libdir)/pure"
ifneq ($(strip $(realtime)),)
	cp realtime/realtime.pure realtime/realtime$(DLL) "$(DESTDIR)$(libdir)/pure"
endif

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/audio.pure" "$(DESTDIR)$(libdir)/pure/portaudio.pure" "$(DESTDIR)$(libdir)/pure/fftw.pure" "$(DESTDIR)$(libdir)/pure/samplerate.pure" "$(DESTDIR)$(libdir)/pure/sndfile.pure" "$(DESTDIR)$(libdir)/pure/audio$(DLL)" "$(DESTDIR)$(libdir)/pure/fftw$(DLL)" "$(DESTDIR)$(libdir)/pure/srcprocess$(DLL)" "$(DESTDIR)$(libdir)/pure/sfinfo$(DLL)"
ifneq ($(strip $(realtime)),)
	rm -f "$(DESTDIR)$(libdir)/pure/realtime.pure" "$(DESTDIR)$(libdir)/pure/realtime$(DLL)"
endif

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/examples && mkdir $(dist)/fftw && mkdir $(dist)/samplerate && mkdir $(dist)/sndfile && mkdir $(dist)/realtime
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
