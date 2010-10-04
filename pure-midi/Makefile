
# Package name and version number:
dist = pure-midi-$(version)
version = 0.1

# Platform-specific defaults, edit this as needed.
#PIC = -fPIC # uncomment for x86-64 compilation
DLL = .so

# Take care of some common systems.
host = $(shell ./config.guess)
ifneq "$(findstring -mingw,$(host))" ""
# Windows
DLL = .dll
endif
ifneq "$(findstring -darwin,$(host))" ""
# OSX
DLL = .dylib
endif

# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

# Installation goes into $(libdir)/pure, you can also set this directly
# instead of $(prefix).
libdir = $(prefix)/lib

DISTFILES = COPYING Makefile README config.guess \
examples/*.pure examples/*.mid \
midi.pure portmidi.pure portmidi.h porttime.h \
midifile/Makefile midifile/midifile.c midifile/midifile.h midifile/mf.c midifile/mf.h midifile/midifile.pure \
realtime/Makefile realtime/realtime.c realtime/realtime.pure

# Makefile to build the PortMidi wrapper as well as the midifiles and realtime
# support libraries.

.PHONY: all clean realclean subdirs generate install uninstall dist distcheck

all: portmidi.pure subdirs

subdirs:
	$(MAKE) -C midifile all
	$(MAKE) -C realtime all

clean:
	$(MAKE) -C midifile clean
	$(MAKE) -C realtime clean

realclean: clean
	rm -f portmidi.pure

# Type 'make generate' to force a rebuild of the PortMidi wrapper. This
# requires pure-gen and that you have portmidi.h and porttime.h in the current
# directory.

generate:
	rm -f portmidi.pure
	make portmidi.pure

# NOTE: Unfortunately, portmidi.h declares Pm_WriteSysEx with an unsigned
# char* in the last argument, which we really want to be a void* in Pure. We
# fix this on the fly.

portmidi.pure: portmidi.h porttime.h
	pure-gen -m Pm -p Pm -sportmidi.h:: portmidi.h -llibportmidi -o portmidi.orig
	pure-gen -N -m Pt -p Pt -sporttime.h:: porttime.h -o portmidi.orig
# Fix up the botched Pm_WriteSysEx prototype.
	sed -e 's/Pm_WriteSysEx(void\*, int, char\*)/Pm_WriteSysEx(void\*, int, void\*)/' < portmidi.orig > $@
	rm -f portmidi.orig

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp midi.pure portmidi.pure midifile/midifile.pure midifile/midifile$(DLL) realtime/realtime.pure realtime/realtime$(DLL) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/midi.pure" "$(DESTDIR)$(libdir)/pure/portmidi.pure" "$(DESTDIR)$(libdir)/pure/midifile.pure" "$(DESTDIR)$(libdir)/pure/midifile$(DLL)" "$(DESTDIR)$(libdir)/pure/realtime.pure" "$(DESTDIR)$(libdir)/pure/realtime$(DLL)"

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/examples && mkdir $(dist)/midifile && mkdir $(dist)/realtime
	for x in $(DISTFILES); do ln -sf $$PWD/$$x $(dist)/$$x; done
	rm -f $(dist).tar.gz
	tar cfzh $(dist).tar.gz $(dist)
	rm -rf $(dist)

distcheck: dist
	tar xfz $(dist).tar.gz
	cd $(dist) && make && make install DESTDIR=./BUILD
	rm -rf $(dist)
