
# Package name and version number:
dist = pure-midi-$(version)
version = 0.5

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
dllname = -install_name "$(libdir)/pure/$@"
endif

MOD_CFLAGS  = $(PIC) $(shell pkg-config pure --cflags) $(CFLAGS) $(CPPFLAGS)
MOD_LDFLAGS = $(shell pkg-config pure --libs) $(LDFLAGS) $(LIBS)

DISTFILES = COPYING Makefile README \
debian/* examples/*.pure examples/*.mid pmdev.h pmdev.c \
midi.pure portmidi.pure portmidi.h porttime.h \
midifile/Makefile midifile/midifile.c midifile/midifile.h \
midifile/mf.c midifile/mf.h midifile/midifile.pure
SEDFILES = README

# Makefile to build the PortMidi wrapper and the midifile support library.

.PHONY: all clean realclean subdirs generate install uninstall dist distcheck

all: pmlib$(DLL) portmidi.pure subdirs

subdirs:
	$(MAKE) -C midifile all

clean:
	rm -f *.o *$(DLL)
	$(MAKE) -C midifile clean

realclean: clean
	rm -f portmidi.pure

pmlib$(DLL): pmdev.c
	$(CC) $(shared) $(dllname) -o $@ $(MOD_CFLAGS) $< $(MOD_LDFLAGS) -lportmidi

# Type 'make generate' to force a rebuild of the PortMidi wrapper. This
# requires pure-gen and that you have portmidi.h and porttime.h in the current
# directory.

generate:
	rm -f portmidi.pure
	make portmidi.pure

portmidi.pure: portmidi.h porttime.h pmdev.h
ifeq ($(DLL),.dll)
	echo 'using "lib:portmidi";' > $@
	pure-gen -N -m Pm -p Pm -sportmidi.h:: portmidi.h -lpmlib -o $@
else
	pure-gen -m Pm -p Pm -sportmidi.h:: portmidi.h -lpmlib -o $@
endif
	pure-gen -N -m Pt -p Pt -sporttime.h:: porttime.h -o $@
	pure-gen -N -m midi -p pm -spmdev.h:: pmdev.h -o $@
ifeq ($(DLL),.dll)
	mv $@ $@.tmp
	sed -e "s?extern int Pm_Synchronize?//extern int Pm_Synchronize?" < $@.tmp > $@
	rm -f $@.tmp
endif

install:
	test -d "$(DESTDIR)$(libdir)/pure" || mkdir -p "$(DESTDIR)$(libdir)/pure"
	cp midi.pure portmidi.pure pmlib$(DLL) midifile/midifile.pure midifile/midifile$(DLL) "$(DESTDIR)$(libdir)/pure"

uninstall:
	rm -f "$(DESTDIR)$(libdir)/pure/midi.pure" "$(DESTDIR)$(libdir)/pure/portmidi.pure" "$(DESTDIR)$(libdir)/pure/midifile.pure" "$(DESTDIR)$(libdir)/pure/pmlib$(DLL)" "$(DESTDIR)$(libdir)/pure/midifile$(DLL)"

date = $(shell date "+%B %-d, %Y")
datesubst = sed -e "s?@version@?$(version)?g" -e "s?|today|?$(date)?g" < $(1) > $(2)

dist:
	rm -rf $(dist)
	mkdir $(dist) && mkdir $(dist)/debian && mkdir $(dist)/examples && mkdir $(dist)/midifile
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
